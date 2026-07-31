#!/usr/bin/env node
/** Execute every applicable workspace lint, build, and test script with receipts. */
import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath, pathToFileURL } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const outDir = path.join(root, '.artifacts', 'package-matrix');
const concurrency = process.argv.find((a) => a.startsWith('--concurrency='))?.split('=')[1] ?? '4';
const phases = ['lint', 'build', 'test'];

async function exec(command, args, timeoutMs = 7_200_000) {
  const startedAt = new Date().toISOString();
  const start = process.hrtime.bigint();
  const stdout = [];
  const stderr = [];
  const child = spawn(command, args, { cwd: root, stdio: ['ignore', 'pipe', 'pipe'] });
  child.stdout.on('data', (chunk) => {
    stdout.push(chunk);
    process.stdout.write(chunk);
  });
  child.stderr.on('data', (chunk) => {
    stderr.push(chunk);
    process.stderr.write(chunk);
  });
  let timedOut = false;
  const timer = setTimeout(() => {
    timedOut = true;
    child.kill('SIGTERM');
  }, timeoutMs);
  const exitCode = await new Promise((resolve, reject) => {
    child.once('error', reject);
    child.once('close', (code) => resolve(code ?? 1));
  });
  clearTimeout(timer);
  return {
    command: [command, ...args],
    startedAt,
    completedAt: new Date().toISOString(),
    durationMs: Math.round(Number(process.hrtime.bigint() - start) / 1_000_000),
    exitCode,
    timedOut,
    stdout: Buffer.concat(stdout).toString('utf8'),
    stderr: Buffer.concat(stderr).toString('utf8'),
  };
}

function entries(manifest) {
  const values = [manifest.module, manifest.main];
  const exported = manifest.exports?.['.'] ?? manifest.exports;
  if (typeof exported === 'string') values.push(exported);
  if (exported && typeof exported === 'object') values.push(exported.import, exported.default);
  return [...new Set(values.filter((value) => typeof value === 'string' && !value.includes('*')))];
}

await mkdir(outDir, { recursive: true });
const receipt = {
  schemaVersion: 1,
  base: process.env.GITHUB_BASE_SHA ?? null,
  head: process.env.GITHUB_HEAD_SHA ?? null,
  node: process.version,
  startedAt: new Date().toISOString(),
  state: 'UNKNOWN',
  packages: [],
  phases: [],
  smoke: [],
};

const discovery = await exec('pnpm', ['list', '-r', '--depth', '-1', '--json']);
if (discovery.exitCode !== 0) throw new Error('WORKSPACE_DISCOVERY_FAILED');
const listed = JSON.parse(discovery.stdout);
const names = new Map();
for (const item of listed) {
  const packagePath = path.resolve(item.path);
  if (packagePath === root) continue;
  const relativePath = path.relative(root, packagePath);
  const manifestPath = path.join(packagePath, 'package.json');
  if (!existsSync(manifestPath)) throw new Error(`WORKSPACE_MANIFEST_MISSING:${relativePath}`);
  const manifest = JSON.parse(await readFile(manifestPath, 'utf8'));
  if (!manifest.name) throw new Error(`PACKAGE_NAME_MISSING:${relativePath}`);
  if (names.has(manifest.name)) throw new Error(`PACKAGE_NAME_DUPLICATE:${manifest.name}`);
  names.set(manifest.name, relativePath);
  receipt.packages.push({ name: manifest.name, path: relativePath, manifest });
}
receipt.packages.sort((a, b) => a.path.localeCompare(b.path));
if (receipt.packages.length === 0) throw new Error('WORKSPACE_DISCOVERY_EMPTY');

for (const phase of phases) {
  const targets = receipt.packages.filter((pkg) => typeof pkg.manifest.scripts?.[phase] === 'string');
  if (targets.length === 0) continue;
  const result = await exec('pnpm', [
    '-r',
    '--no-bail',
    '--stream',
    `--workspace-concurrency=${concurrency}`,
    '--if-present',
    'run',
    phase,
  ]);
  await writeFile(path.join(outDir, `${phase}.log`), `${result.stdout}${result.stderr}`);
  receipt.phases.push({
    phase,
    targets: targets.map(({ name, path: packagePath }) => ({ name, path: packagePath })),
    targetCount: targets.length,
    exitCode: result.exitCode,
    timedOut: result.timedOut,
    durationMs: result.durationMs,
    state: result.exitCode === 0 && !result.timedOut ? 'ALIVE' : 'BUILD_BROKEN',
  });
}

for (const pkg of receipt.packages.filter((item) => !item.manifest.scripts?.test)) {
  const candidate = entries(pkg.manifest).find((entry) => existsSync(path.resolve(root, pkg.path, entry)));
  if (!candidate) {
    receipt.smoke.push({
      name: pkg.name,
      path: pkg.path,
      state: pkg.manifest.private ? 'NOT_APPLICABLE' : 'UNSUPPORTED',
      reason: 'no test script or existing import target',
    });
    continue;
  }
  const target = path.resolve(root, pkg.path, candidate);
  const result = await exec(process.execPath, [
    '--input-type=module',
    '--eval',
    `await import(${JSON.stringify(pathToFileURL(target).href)})`,
  ], 300_000);
  receipt.smoke.push({
    name: pkg.name,
    path: pkg.path,
    target: candidate,
    exitCode: result.exitCode,
    state: result.exitCode === 0 && !result.timedOut ? 'ALIVE' : 'BUILD_BROKEN',
  });
}

receipt.completedAt = new Date().toISOString();
receipt.state = [
  ...receipt.phases.map((phase) => phase.state),
  ...receipt.smoke.map((smoke) => smoke.state),
].every((state) => ['ALIVE', 'NOT_APPLICABLE'].includes(state))
  ? 'ALIVE'
  : 'BUILD_BROKEN';
for (const pkg of receipt.packages) delete pkg.manifest;
await writeFile(path.join(outDir, 'receipt.json'), `${JSON.stringify(receipt, null, 2)}\n`);
console.log(`PACKAGE_MATRIX_RECEIPT ${JSON.stringify({
  state: receipt.state,
  packageCount: receipt.packages.length,
  phases: receipt.phases.map(({ phase, targetCount, state }) => ({ phase, targetCount, state })),
  smoke: receipt.smoke,
})}`);
process.exitCode = receipt.state === 'ALIVE' ? 0 : 1;
