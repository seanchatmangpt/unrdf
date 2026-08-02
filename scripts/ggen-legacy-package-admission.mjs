#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, lstatSync, readdirSync, readFileSync } from 'node:fs';
import { mkdir, writeFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const out = path.join(root, '.artifacts/ggen-legacy-package-admission');
const authority = {
  repository: 'seanchatmangpt/ggen-legacy',
  commit: '297e44a78aae35fee11b5a5efa262155a153940e',
  ticket: 'planning/v26.8.1/tickets/UNRDF-001-package-reconstitution.json',
};
const sourceHead = process.env.GITHUB_SHA || process.env.GITHUB_HEAD_SHA || null;
const digest = value => createHash('sha256').update(value).digest('hex');
const relative = value => path.relative(root, value).split(path.sep).join('/');

function walk(directory, files = []) {
  if (!existsSync(directory)) return files;
  for (const name of readdirSync(directory).sort()) {
    if (['.git', '.artifacts', 'coverage', 'dist', 'node_modules'].includes(name)) continue;
    const candidate = path.join(directory, name);
    let stat;
    try { stat = lstatSync(candidate); } catch { continue; }
    if (stat.isSymbolicLink()) continue;
    if (stat.isDirectory()) walk(candidate, files);
    else files.push(candidate);
  }
  return files;
}

function declaredEntries(directory, manifest) {
  const values = [manifest.main, manifest.module];
  const exported = manifest.exports?.['.'] ?? manifest.exports;
  if (typeof exported === 'string') values.push(exported);
  else if (exported && typeof exported === 'object') values.push(exported.import, exported.node, exported.default);
  return values.filter(value => typeof value === 'string').map(value => path.resolve(directory, value)).filter(existsSync);
}

const manifestPaths = [];
for (const surface of ['packages', 'apps', 'examples', 'benchmarks']) {
  manifestPaths.push(...walk(path.join(root, surface)).filter(file => path.basename(file) === 'package.json'));
}
manifestPaths.sort();

const packages = [];
for (const manifestPath of manifestPaths) {
  const directory = path.dirname(manifestPath);
  const checks = [];
  let manifest;
  try {
    const bytes = readFileSync(manifestPath);
    manifest = JSON.parse(bytes);
    checks.push({ id: 'manifest-json', subject: relative(manifestPath), command: ['JSON.parse'], exitCode: 0, state: 'ALIVE', digest: digest(bytes) });
  } catch (error) {
    packages.push({ name: null, path: relative(directory), standing: 'BUILD_BROKEN', checks: [{ id: 'manifest-json', exitCode: 1, state: 'BUILD_BROKEN', error: error.message }] });
    continue;
  }

  const executable = [...new Set([
    ...declaredEntries(directory, manifest),
    ...walk(directory).filter(file => /\.(mjs|cjs|js)$/.test(file)),
  ])].slice(0, 16);

  for (const file of executable) {
    const result = spawnSync(process.execPath, ['--check', file], { cwd: root, encoding: 'utf8', timeout: 30000 });
    checks.push({
      id: 'syntax',
      subject: relative(file),
      command: [process.execPath, '--check', relative(file)],
      exitCode: result.status ?? 1,
      signal: result.signal,
      state: result.status === 0 ? 'ALIVE' : 'BUILD_BROKEN',
      stderr: (result.stderr || '').slice(-2000),
      digest: digest(readFileSync(file)),
    });
    if (result.status === 0) break;
  }

  if (!executable.length) {
    const content = walk(directory).find(file => /\.(json|md|ttl|toml|yaml|yml)$/.test(file));
    if (content) checks.push({ id: 'content-witness', subject: relative(content), command: ['read', relative(content)], exitCode: 0, state: 'ALIVE', digest: digest(readFileSync(content)) });
  }

  const manifestAlive = checks.some(check => check.id === 'manifest-json' && check.state === 'ALIVE');
  const subjectAlive = checks.some(check => check.id !== 'manifest-json' && check.state === 'ALIVE');
  packages.push({
    name: manifest.name || relative(directory),
    path: relative(directory),
    version: manifest.version ?? null,
    private: manifest.private === true,
    standing: manifestAlive && subjectAlive ? 'PARTIAL_ALIVE' : 'BUILD_BROKEN',
    checks,
    exclusions: ['full package crown remains governed by build, test, integration, receipt, and replay'],
  });
}

const summary = packages.reduce((result, item) => {
  result[item.standing] = (result[item.standing] || 0) + 1;
  return result;
}, {});
const receipt = {
  schema: 'ggen.legacy.unrdf-package-admission-receipt/1',
  authority,
  source: { repository: 'seanchatmangpt/unrdf', commit: sourceHead },
  generatedBy: 'scripts/ggen-legacy-package-admission.mjs',
  environment: { node: process.version, platform: process.platform, arch: process.arch },
  packageCount: packages.length,
  summary,
  standing: packages.length > 0 && packages.every(item => item.standing === 'PARTIAL_ALIVE') ? 'PARTIAL_ALIVE' : 'BUILD_BROKEN',
  packages,
};

await mkdir(out, { recursive: true });
await writeFile(path.join(out, 'receipt.json'), `${JSON.stringify(receipt, null, 2)}\n`);
await writeFile(path.join(out, 'package-standing.tsv'), ['package\tpath\tstanding', ...packages.map(item => `${item.name}\t${item.path}\t${item.standing}`)].join('\n') + '\n');
console.log(`GGEN_LEGACY_PACKAGE_ADMISSION ${JSON.stringify({ standing: receipt.standing, packageCount: receipt.packageCount, summary, receipt: relative(path.join(out, 'receipt.json')) })}`);
process.exitCode = receipt.standing === 'PARTIAL_ALIVE' ? 0 : 1;
