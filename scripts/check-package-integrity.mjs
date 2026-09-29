#!/usr/bin/env node
/**
 * @file Package integrity sweep for the monorepo.
 *
 * Checks, for every packages/<pkg>/package.json:
 *  - main/module/types/typings/bin/exports (all conditions + subpaths)/files targets exist
 *  - script commands: file arguments (node/tsx/vitest/... *.mjs|js|cjs|ts|sh) exist and
 *    `pnpm --filter <name>` names are workspace packages
 * and for every packages/<pkg>/src/**\/*.{mjs,js}:
 *  - relative import specifiers resolve to an existing file
 *  - `@unrdf/<pkg>[/<subpath>]` imports are covered by that package's exports map
 *
 * Usage: node scripts/check-package-integrity.mjs [--json]   (exit 1 on findings)
 */
import { readFileSync, readdirSync, existsSync, statSync } from 'node:fs';
import { dirname, resolve, join, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const SRC_EXT = /\.(mjs|js)$/;
const SCRIPT_FILE = /(^|\/)[^/.][^/]*\.(mjs|cjs|js|ts|mts|sh)$/;

function isFile(p) {
  try {
    return statSync(p).isFile();
  } catch {
    return false;
  }
}
function isDir(p) {
  try {
    return statSync(p).isDirectory();
  } catch {
    return false;
  }
}

/** Convert a simple glob (with *, ** and {a,b}) to a RegExp. */
function globToRegExp(glob, starCrossesDirs = false) {
  let re = '';
  for (let i = 0; i < glob.length; i++) {
    const c = glob[i];
    if (c === '*') {
      if (glob[i + 1] === '*') {
        i++;
        if (glob[i + 1] === '/') i++;
        re += '(?:.*/)?';
      } else re += starCrossesDirs ? '.*' : '[^/]*';
    } else if (c === '?') re += '[^/]';
    else if (c === '{') {
      const end = glob.indexOf('}', i);
      re += `(?:${glob
        .slice(i + 1, end)
        .split(',')
        .join('|')})`;
      i = end;
    } else re += c.replace(/[.+^$()|[\]\\]/g, '\\$&');
  }
  return new RegExp(`^${re}$`);
}

function walk(dir, filter, out = []) {
  let entries;
  try {
    entries = readdirSync(dir, { withFileTypes: true });
  } catch {
    return out;
  }
  for (const e of entries) {
    if (e.name === 'node_modules' || e.name === '.git') continue;
    const p = join(dir, e.name);
    if (e.isDirectory()) walk(p, filter, out);
    else if (filter(p)) out.push(p);
  }
  return out;
}

/** True if a (possibly globbed) relative path matches at least one file/dir under base. */
function targetExists(base, target, starCrossesDirs = false) {
  const clean = target.replace(/^\.\//, '');
  if (!/[*?{]/.test(clean)) return existsSync(resolve(base, clean));
  const segs = clean.split('/');
  const firstGlob = segs.findIndex(s => /[*?{]/.test(s));
  const root = resolve(base, ...segs.slice(0, firstGlob));
  if (!isDir(root)) return false;
  const re = globToRegExp(clean, starCrossesDirs);
  return walk(root, () => true).some(f => re.test(relative(base, f).split('\\').join('/')));
}

export function loadWorkspace(repoRoot = REPO_ROOT) {
  const pkgsDir = join(repoRoot, 'packages');
  const pkgs = new Map();
  if (!isDir(pkgsDir)) return pkgs;
  for (const d of readdirSync(pkgsDir)) {
    const dir = join(pkgsDir, d);
    const pjPath = join(dir, 'package.json');
    if (!isFile(pjPath)) continue;
    try {
      const pj = JSON.parse(readFileSync(pjPath, 'utf8'));
      pkgs.set(pj.name || d, { name: pj.name || d, dir, dirName: d, pj, pjPath });
    } catch (err) {
      pkgs.set(d, { name: d, dir, dirName: d, pj: null, pjPath, parseError: err.message });
    }
  }
  return pkgs;
}

/** Collect every string target in an exports value (all conditions). */
function exportTargets(val, out = []) {
  if (typeof val === 'string') out.push(val);
  else if (Array.isArray(val)) val.forEach(v => exportTargets(v, out));
  else if (val && typeof val === 'object') Object.values(val).forEach(v => exportTargets(v, out));
  return out;
}

function exportsMap(ex) {
  return typeof ex === 'object' &&
    ex !== null &&
    !Array.isArray(ex) &&
    Object.keys(ex).some(k => k.startsWith('.'))
    ? ex
    : { '.': ex };
}

/** Split a shell-ish command into simple commands, each a token list. */
function commandsOf(script) {
  return script
    .split(/&&|\|\||;|\||\n/)
    .map(c => c.trim())
    .filter(Boolean)
    .map(c => (c.match(/"[^"]*"|'[^']*'|\S+/g) || []).map(t => t.replace(/^["']|["']$/g, '')));
}

function checkManifest(pkg, workspace, findings, repoRoot) {
  const { pj, dir } = pkg;
  const add = (kind, message, extra = {}) =>
    findings.push({
      kind,
      severity: 'error',
      package: pkg.name,
      file: relative(repoRoot, pkg.pjPath),
      message,
      ...extra,
    });
  const addTarget = (message, extra) => {
    // Build outputs (dist/) and files[] entries are advisory: they only exist after build/publish.
    const t = extra.target.replace(/^\.\//, '');
    const warn = extra.field === 'files' || /^dist(\/|$)/.test(t);
    add(warn ? 'missing-build-output' : 'missing-target', message, warn ? { ...extra, severity: 'warn' } : extra);
  };
  if (!pj) return add('invalid-json', `package.json does not parse: ${pkg.parseError}`);

  for (const f of ['main', 'module', 'types', 'typings', 'browser']) {
    if (typeof pj[f] === 'string' && !targetExists(dir, pj[f]))
      addTarget(`${f} -> ${pj[f]} does not exist`, { field: f, target: pj[f] });
  }
  if (pj.bin) {
    const bins = typeof pj.bin === 'string' ? { [pj.name]: pj.bin } : pj.bin;
    for (const [n, t] of Object.entries(bins))
      if (!targetExists(dir, t))
        addTarget(`bin.${n} -> ${t} does not exist`, { field: 'bin', target: t });
  }
  if (pj.exports !== undefined) {
    for (const [sub, val] of Object.entries(exportsMap(pj.exports)))
      for (const t of exportTargets(val))
        if (t.startsWith('.') && !targetExists(dir, t, true))
          addTarget(`exports["${sub}"] -> ${t} does not exist`, {
            field: 'exports',
            target: t,
          });
  }
  if (Array.isArray(pj.files)) {
    for (const f of pj.files) {
      if (f.startsWith('!')) continue;
      if (!targetExists(dir, f))
        addTarget(`files entry "${f}" does not exist`, { field: 'files', target: f });
    }
  }
  for (const [sname, cmd] of Object.entries(pj.scripts || {})) {
    if (typeof cmd !== 'string') continue;
    for (const toks of commandsOf(cmd)) {
      let cwd = dir;
      for (let i = 0; i < toks.length; i++) {
        const t = toks[i];
        if (t === 'cd' && toks[i + 1]) {
          cwd = resolve(cwd, toks[i + 1]);
          continue;
        }
        if ((t === '--filter' || t === '-F') && toks[i + 1]) {
          const n = toks[i + 1];
          if (!/[*.{}!\[]/.test(n) && !workspace.has(n))
            add('unknown-filter', `script "${sname}": pnpm --filter ${n} is not a workspace package`, {
              script: sname,
              target: n,
            });
          i++;
          continue;
        }
        const v = t.startsWith('-') ? (t.includes('=') ? t.slice(t.indexOf('=') + 1) : '') : t;
        if (!v || /[$`]|^https?:|^@|:/.test(v)) continue;
        if (i === 0) continue; // the command itself
        if (/^(\.\/)?node_modules\//.test(v)) continue; // installed dependency binary
        if (!SCRIPT_FILE.test(v)) continue;
        if (!targetExists(cwd, v))
          add('missing-script-target', `script "${sname}": ${v} does not exist`, {
            script: sname,
            target: v,
          });
      }
    }
  }
}

/** Resolve `subpath` (`.` or `./x`) through an exports map: undefined = no map, null = uncovered. */
function resolveExportSubpath(pj, subpath) {
  if (pj.exports === undefined) return undefined;
  const map = exportsMap(pj.exports);
  if (subpath in map) return exportTargets(map[subpath]);
  for (const [k, v] of Object.entries(map)) {
    const star = k.indexOf('*');
    if (star === -1) {
      if (k.endsWith('/') && subpath.startsWith(k)) return exportTargets(v);
      continue;
    }
    const pre = k.slice(0, star);
    const post = k.slice(star + 1);
    if (subpath.startsWith(pre) && subpath.endsWith(post) && subpath.length >= k.length - 1) {
      const mid = subpath.slice(pre.length, subpath.length - post.length);
      return exportTargets(v).map(t => t.replace(/\*/g, mid));
    }
  }
  return null;
}

const IMPORT_RES = [
  /\b(?:import|export)\s+(?:[^'"`;]*?\s+from\s+)?['"]([^'"\n]+)['"]/g,
  /\bimport\s*\(\s*['"]([^'"\n]+)['"]\s*\)/g,
  /\brequire\s*\(\s*['"]([^'"\n]+)['"]\s*\)/g,
];

function stripComments(src) {
  return src
    .replace(/\/\*[\s\S]*?\*\//g, m => m.replace(/[^\n]/g, ' '))
    .replace(/(^|[^:'"`\\])\/\/[^\n]*/g, '$1');
}

function checkSources(pkg, workspace, findings, repoRoot) {
  for (const file of walk(join(pkg.dir, 'src'), p => SRC_EXT.test(p))) {
    let text;
    try {
      text = stripComments(readFileSync(file, 'utf8'));
    } catch {
      continue;
    }
    const rel = relative(repoRoot, file);
    const seen = new Set();
    for (const re of IMPORT_RES) {
      for (const m of text.matchAll(re)) {
        const spec = m[1];
        if (seen.has(spec)) continue;
        seen.add(spec);
        const line = text.slice(0, m.index).split('\n').length;
        // A dynamic import with an explicit .catch() fallback is an optional dependency.
        const guarded =
          m[0].startsWith('import(') && text.slice(m.index + m[0].length).startsWith('.catch');
        const report = (kind, message) =>
          findings.push({
            kind,
            severity: guarded ? 'warn' : 'error',
            package: pkg.name,
            file: rel,
            line,
            message,
            target: spec,
          });
        if (spec.startsWith('./') || spec.startsWith('../')) {
          if (/[${}*]/.test(spec)) continue;
          if (!isFile(resolve(dirname(file), spec)))
            report('unresolved-relative-import', `import "${spec}" does not resolve to a file`);
        } else if (spec.startsWith('@unrdf/')) {
          const parts = spec.split('/');
          const name = `${parts[0]}/${parts[1]}`;
          const sub = parts.length > 2 ? `./${parts.slice(2).join('/')}` : '.';
          const dep = workspace.get(name);
          if (!dep) {
            report('unknown-workspace-package', `import "${spec}": ${name} is not a workspace package`);
            continue;
          }
          if (!dep.pj) continue;
          const targets = resolveExportSubpath(dep.pj, sub);
          if (targets === undefined) {
            const base = resolve(dep.dir, sub);
            if (sub !== '.' && !isFile(base) && !isFile(`${base}.js`) && !isFile(`${base}.mjs`))
              report(
                'unresolved-package-subpath',
                `import "${spec}": ${name} has no exports map and ${sub} is not a file`
              );
          } else if (targets === null) {
            report('unexported-subpath', `import "${spec}": ${sub} not covered by ${name} exports map`);
          } else if (
            targets.length &&
            targets.every(t => t.startsWith('.') && !targetExists(dep.dir, t, true))
          ) {
            report(
              'export-target-missing',
              `import "${spec}": exports target ${targets.join(', ')} missing in ${name}`
            );
          }
        }
      }
    }
  }
}

export function runChecks(repoRoot = REPO_ROOT) {
  const workspace = loadWorkspace(repoRoot);
  const findings = [];
  for (const pkg of workspace.values()) {
    checkManifest(pkg, workspace, findings, repoRoot);
    if (pkg.pj) checkSources(pkg, workspace, findings, repoRoot);
  }
  return { packages: workspace.size, findings };
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const result = runChecks();
  if (process.argv.includes('--json')) {
    console.log(JSON.stringify(result, null, 2));
  } else {
    for (const f of result.findings)
      console.log(`[${f.kind}] ${f.file}${f.line ? `:${f.line}` : ''} (${f.package}) ${f.message}`);
    console.log(`\n${result.packages} packages, ${result.findings.filter(f => f.severity === 'error').length} errors, ${result.findings.filter(f => f.severity === 'warn').length} warnings`);
  }
  process.exit(result.findings.some(f => f.severity === 'error') ? 1 : 0);
}
