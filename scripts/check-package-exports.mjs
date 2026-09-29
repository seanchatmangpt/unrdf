#!/usr/bin/env node
/**
 * @file Verify that every package entry imports and every named re-export resolves.
 *
 * For each package: (1) dynamically import the package entry, (2) statically scan the
 * entry (and files it re-exports from) for `export { a, b as c } from './x'` and check
 * each imported name exists in the target module's namespace. Unlike a plain import,
 * this reports ALL unresolved names, not only the first link error.
 *
 * Usage: node scripts/check-package-exports.mjs [pkg ...]
 */
import { readFileSync, existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { pathToFileURL, fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const DEFAULT = ['knowledge-engine', 'kgc-swarm', 'daemon', 'core', 'project-engine', 'cli'];
const pkgs = process.argv.slice(2).length ? process.argv.slice(2) : DEFAULT;

function entryOf(pkgDir) {
  const pj = JSON.parse(readFileSync(resolve(pkgDir, 'package.json'), 'utf8'));
  const dot = pj.exports?.['.'];
  const e =
    (typeof dot === 'string' ? dot : dot?.import || dot?.default) || pj.main || 'src/index.mjs';
  return resolve(pkgDir, e);
}

const RE = /export\s*\{([^}]*)\}\s*from\s*['"]([^'"]+)['"]/g;
const problems = [];

async function checkFile(file, seen) {
  if (seen.has(file)) return;
  seen.add(file);
  const src = readFileSync(file, 'utf8').replace(/\/\*[\s\S]*?\*\//g, '').replace(/^\s*\/\/.*$/gm, '');
  for (const m of src.matchAll(RE)) {
    const names = m[1]
      .split(',')
      .map(s => s.trim())
      .filter(Boolean)
      .map(s => s.split(/\s+as\s+/)[0].trim());
    const spec = m[2];
    let target;
    if (spec.startsWith('.')) target = pathToFileURL(resolve(dirname(file), spec)).href;
    else target = spec;
    let ns;
    try {
      ns = await import(target);
    } catch (e) {
      problems.push(`${file}: cannot import '${spec}': ${e.message.split('\n')[0]}`);
      continue;
    }
    for (const n of names) {
      if (n === 'default' ? !('default' in ns) : !(n in ns)) {
        problems.push(`${file}: '${n}' not exported by '${spec}'`);
      }
    }
    if (spec.startsWith('.')) {
      const p = resolve(dirname(file), spec);
      if (existsSync(p)) await checkFile(p, seen);
    }
  }
}

for (const p of pkgs) {
  const dir = resolve(root, 'packages', p);
  const entry = entryOf(dir);
  try {
    await import(pathToFileURL(entry).href);
    console.log(`OK    import ${p} (${entry.replace(root + '/', '')})`);
  } catch (e) {
    problems.push(`${p}: entry import failed: ${e.message.split('\n')[0]}`);
  }
  await checkFile(entry, new Set());
}

if (problems.length) {
  console.log(`\n${problems.length} unresolved:`);
  for (const p of problems) console.log('  - ' + p.replace(root + '/', ''));
  process.exit(1);
}
console.log('\nAll entries and named re-exports resolve.');
