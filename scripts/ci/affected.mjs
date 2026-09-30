#!/usr/bin/env node
/**
 * @file Change-impact planning and shard assignment for the workspace package matrix.
 *
 * Pure functions (exported, unit-tested in affected.test.mjs) plus a small CLI:
 *
 *   node scripts/ci/affected.mjs --from=HEAD^1            # print the affected package set as JSON
 *   node scripts/ci/affected.mjs --from=HEAD^1 --shard=1/2
 *
 * Correctness rules:
 *  - A changed file belongs to the package whose directory is its longest path prefix.
 *  - Packages that (transitively) depend on a changed package are affected too.
 *  - Toolchain-level files (lockfile, root manifest, shared configs, CI machinery itself) affect everything.
 *  - Anything unreadable or ambiguous falls back to "everything": a wrong guess may cost time, never coverage.
 */
import { execFileSync } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

/** Files whose change can alter the behaviour of every package. */
export const GLOBAL_PATTERNS = [
  /^pnpm-lock\.yaml$/,
  /^pnpm-workspace\.yaml$/,
  /^package\.json$/,
  /^\.npmrc$/,
  /^\.tool-versions$/,
  /^\.nvmrc$/,
  /^\.github\/actions\//,
  /^\.github\/workflows\/ci\.yml$/,
  /^scripts\/(verify-workspace-packages|ci\/)/,
  /^(vitest|vite|eslint|prettier|tsconfig|jsconfig|babel)[^/]*$/,
  /^\.(eslintrc|prettierrc)[^/]*$/,
];

const DEP_FIELDS = ['dependencies', 'devDependencies', 'peerDependencies', 'optionalDependencies'];

/**
 * @param {string} file - repo-relative POSIX path
 * @returns {boolean} true when the file can affect every package
 */
export function isGlobalChange(file) {
  return GLOBAL_PATTERNS.some(re => re.test(file));
}

/**
 * Reverse-dependency closure: every package that depends (transitively) on any seed package.
 * @param {Array<{name:string, deps:string[]}>} packages
 * @param {Iterable<string>} seeds
 * @returns {Set<string>} seeds plus all their dependents
 */
export function dependentsClosure(packages, seeds) {
  const dependents = new Map();
  for (const pkg of packages) {
    for (const dep of pkg.deps) {
      if (!dependents.has(dep)) dependents.set(dep, []);
      dependents.get(dep).push(pkg.name);
    }
  }
  const out = new Set(seeds);
  const queue = [...out];
  while (queue.length) {
    for (const next of dependents.get(queue.pop()) ?? []) {
      if (!out.has(next)) {
        out.add(next);
        queue.push(next);
      }
    }
  }
  return out;
}

/**
 * @param {{packages: Array<{name:string, path:string, deps:string[]}>, changedFiles: string[]}} input
 * @returns {{all: boolean, reason: string, changed: string[], selected: string[]}}
 */
export function selectAffected({ packages, changedFiles }) {
  const everything = reason => ({
    all: true,
    reason,
    changed: [],
    selected: packages.map(p => p.name).sort(),
  });
  if (!changedFiles) return everything('change list unavailable');
  const byLength = [...packages].sort((a, b) => b.path.length - a.path.length);
  const changed = new Set();
  for (const file of changedFiles) {
    if (isGlobalChange(file)) return everything(`global file changed: ${file}`);
    const owner = byLength.find(p => file === p.path || file.startsWith(`${p.path}/`));
    if (owner) changed.add(owner.name);
  }
  const selected = dependentsClosure(packages, changed);
  return {
    all: false,
    reason: changed.size ? 'package closure' : 'no package files changed',
    changed: [...changed].sort(),
    selected: [...selected].sort(),
  };
}

/**
 * Stable longest-processing-time-first shard assignment. Independent of which packages are selected,
 * so a package always lands on the same shard (good for caches and for reading results).
 * @param {string[]} names
 * @param {Record<string, number>} weights - seconds per package (default 10)
 * @param {number} count
 * @returns {string[][]} shards[i] = package names
 */
export function assignShards(names, weights, count) {
  const shards = Array.from({ length: count }, () => ({ load: 0, names: [] }));
  const sorted = [...names].sort(
    (a, b) => (weights[b] ?? 10) - (weights[a] ?? 10) || a.localeCompare(b)
  );
  for (const name of sorted) {
    const target = shards.reduce((min, s) => (s.load < min.load ? s : min));
    target.names.push(name);
    target.load += weights[name] ?? 10;
  }
  return shards.map(s => s.names.sort());
}

/**
 * Read workspace packages from package.json manifests (no package-manager invocation).
 * @param {string} root - repo root
 * @param {Array<{name:string, path:string}>} listed - packages as reported by `pnpm list -r --json`
 * @returns {Array<{name:string, path:string, deps:string[]}>}
 */
export function withDependencies(root, listed) {
  const names = new Set(listed.map(p => p.name));
  return listed.map(p => {
    const manifest = JSON.parse(readFileSync(path.join(root, p.path, 'package.json'), 'utf8'));
    const deps = new Set();
    for (const field of DEP_FIELDS) {
      for (const dep of Object.keys(manifest[field] ?? {})) if (names.has(dep)) deps.add(dep);
    }
    return { name: p.name, path: p.path, deps: [...deps] };
  });
}

/**
 * Files changed between `from` and HEAD, or null when git cannot answer (caller falls back to "all").
 * @param {string} from - git ref, e.g. HEAD^1 on a pull-request merge checkout
 * @param {string} cwd
 * @returns {string[]|null}
 */
export function gitChangedFiles(from, cwd = process.cwd()) {
  try {
    const out = execFileSync('git', ['diff', '--name-only', '--no-renames', from, 'HEAD'], {
      cwd,
      encoding: 'utf8',
      maxBuffer: 64 * 1024 * 1024,
    });
    return out.split('\n').filter(Boolean);
  } catch {
    return null;
  }
}

/**
 * @param {string} spec - "i/n" (1-based)
 * @returns {{index:number, count:number}}
 */
export function parseShard(spec) {
  const m = /^(\d+)\/(\d+)$/.exec(spec ?? '');
  if (!m || +m[1] < 1 || +m[1] > +m[2])
    throw new Error(`invalid --shard=${spec} (expected i/n, 1<=i<=n)`);
  return { index: +m[1], count: +m[2] };
}

export function loadWeights(file) {
  try {
    return existsSync(file) ? (JSON.parse(readFileSync(file, 'utf8')).weights ?? {}) : {};
  } catch {
    return {};
  }
}

const isMain = process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) {
  const arg = name =>
    process.argv
      .find(a => a.startsWith(`--${name}=`))
      ?.split('=')
      .slice(1)
      .join('=');
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
  const listed = JSON.parse(
    execFileSync('pnpm', ['list', '-r', '--depth', '-1', '--json'], {
      cwd: root,
      encoding: 'utf8',
      maxBuffer: 64 * 1024 * 1024,
    })
  )
    .map(p => ({ name: p.name, path: path.relative(root, p.path).split(path.sep).join('/') }))
    .filter(p => p.path);
  const packages = withDependencies(root, listed);
  const from = arg('from');
  const result = from
    ? selectAffected({ packages, changedFiles: gitChangedFiles(from, root) })
    : selectAffected({ packages, changedFiles: null });
  const shard = arg('shard');
  if (shard) {
    const { index, count } = parseShard(shard);
    const mine = assignShards(
      packages.map(p => p.name),
      loadWeights(path.join(root, 'scripts/ci/package-weights.json')),
      count
    )[index - 1];
    result.shard = {
      index,
      count,
      packages: mine,
      selected: mine.filter(n => result.selected.includes(n)),
    };
  }
  console.log(JSON.stringify(result, null, 2));
}
