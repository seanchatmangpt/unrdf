#!/usr/bin/env node
import { spawn } from 'node:child_process';
import { createWriteStream, existsSync } from 'node:fs';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath, pathToFileURL } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const out = path.join(root, '.artifacts/package-matrix');
const pnpm = process.platform === 'win32' ? 'pnpm.cmd' : 'pnpm';
const limit = +(process.argv.find(x => x.startsWith('--concurrency='))?.split('=')[1] || 4);
const timeout = +(process.argv.find(x => x.startsWith('--timeout-ms='))?.split('=')[1] || 300000);
const phases = ['lint', 'build', 'test'];
const noop = /\b(echo|printf)\b.*\b(no|skipped)\b.*\b(test|build|lint)/i;
const masked = /\|\|\s*true\b|;\s*exit\s+0\b/;
const receipt = { schemaVersion: 2, base: process.env.GITHUB_BASE_SHA || null, head: process.env.GITHUB_HEAD_SHA || null, node: process.version, startedAt: new Date().toISOString(), state: 'UNKNOWN', packages: [], executions: [], imports: [] };

const slug = s => s.replace(/^@/, '').replace(/[^a-zA-Z0-9._-]+/g, '-');
const tail = (a, b, n = 16384) => (a + b).slice(-n);

async function run(cmd, args, cwd, logPath, ms = timeout) {
  await mkdir(path.dirname(logPath), { recursive: true });
  const log = createWriteStream(logPath);
  const start = process.hrtime.bigint();
  let stdout = '', stderr = '', timedOut = false, spawnError = null;
  const child = spawn(cmd, args, { cwd, env: { ...process.env, CI: process.env.CI || '1' }, stdio: ['ignore', 'pipe', 'pipe'] });
  child.stdout.on('data', x => { const s = x.toString(); stdout = tail(stdout, s, 1048576); log.write(s); });
  child.stderr.on('data', x => { const s = x.toString(); stderr = tail(stderr, s); log.write(s); });
  child.once('error', e => { spawnError = e.message; });
  const timer = setTimeout(() => { timedOut = true; child.kill('SIGTERM'); }, ms);
  const exitCode = await new Promise(resolve => child.once('close', code => resolve(code ?? 1)));
  clearTimeout(timer); await new Promise(resolve => log.end(resolve));
  return { command: [cmd, ...args], cwd: path.relative(root, cwd) || '.', log: path.relative(root, logPath), exitCode, timedOut, spawnError, durationMs: Math.round(Number(process.hrtime.bigint() - start) / 1e6), stdoutTail: stdout, stderrTail: stderr };
}

async function pool(items, worker) {
  const results = new Array(items.length); let cursor = 0;
  await Promise.all(Array.from({ length: Math.min(limit, items.length) }, async () => {
    while (cursor < items.length) { const i = cursor++; results[i] = await worker(items[i]); }
  }));
  return results;
}

function entries(manifest) {
  const values = [manifest.module, manifest.main];
  const rootExport = manifest.exports?.['.'] ?? manifest.exports;
  if (typeof rootExport === 'string') values.push(rootExport);
  else if (rootExport && typeof rootExport === 'object') values.push(rootExport.import, rootExport.node, rootExport.default);
  return [...new Set(values.filter(x => typeof x === 'string' && !x.includes('*')))];
}

await mkdir(out, { recursive: true });
try {
  const discovery = await run(pnpm, ['list', '-r', '--depth', '-1', '--json'], root, path.join(out, 'discovery.log'), 120000);
  if (discovery.exitCode) throw new Error('WORKSPACE_DISCOVERY_FAILED');
  const seen = new Set();
  for (const item of JSON.parse(discovery.stdoutTail)) {
    const abs = path.resolve(item.path); if (abs === root) continue;
    const rel = path.relative(root, abs); const manifest = JSON.parse(await readFile(path.join(abs, 'package.json'), 'utf8'));
    if (!manifest.name) throw new Error(`PACKAGE_NAME_MISSING:${rel}`);
    if (seen.has(manifest.name)) throw new Error(`PACKAGE_NAME_DUPLICATE:${manifest.name}`);
    seen.add(manifest.name); receipt.packages.push({ name: manifest.name, path: rel, private: manifest.private === true, scripts: manifest.scripts || {}, entries: entries(manifest) });
  }
  receipt.packages.sort((a, b) => a.path.localeCompare(b.path));
  if (!receipt.packages.length) throw new Error('WORKSPACE_DISCOVERY_EMPTY');

  for (const phase of phases) {
    const targets = receipt.packages.filter(p => typeof p.scripts[phase] === 'string');
    receipt.executions.push(...await pool(targets, async p => {
      console.log(`[${phase}] ${p.name}`);
      const result = await run(pnpm, ['--dir', path.join(root, p.path), 'run', phase], root, path.join(out, phase, `${slug(p.name)}.log`));
      const script = p.scripts[phase];
      const state = result.exitCode || result.timedOut || result.spawnError ? 'BUILD_BROKEN' : masked.test(script) ? 'UNSUPPORTED' : noop.test(script) ? 'NOT_APPLICABLE' : 'ALIVE';
      console.log(`[${phase}] ${p.name}: ${state}`);
      return { package: p.name, path: p.path, phase, script, state, ...result };
    }));
    for (const p of receipt.packages.filter(p => !p.scripts[phase])) receipt.executions.push({ package: p.name, path: p.path, phase, script: null, state: 'NOT_APPLICABLE', reason: `no ${phase} script` });
  }

  receipt.imports.push(...await pool(receipt.packages, async p => {
    const target = p.entries.find(x => existsSync(path.resolve(root, p.path, x)));
    if (!target) return { package: p.name, path: p.path, state: p.private ? 'NOT_APPLICABLE' : 'BUILD_BROKEN', reason: p.entries.length ? 'declared root export is missing' : 'no root export declared', candidates: p.entries };
    const result = await run(process.execPath, ['--input-type=module', '--eval', `await import(${JSON.stringify(pathToFileURL(path.resolve(root, p.path, target)).href)})`], root, path.join(out, 'import', `${slug(p.name)}.log`), 120000);
    return { package: p.name, path: p.path, target, state: result.exitCode || result.timedOut ? 'BUILD_BROKEN' : 'ALIVE', ...result };
  }));

  const states = [...receipt.executions, ...receipt.imports].map(x => x.state);
  receipt.summary = states.reduce((m, s) => (m[s] = (m[s] || 0) + 1, m), {});
  receipt.state = states.every(s => s === 'ALIVE' || s === 'NOT_APPLICABLE') ? 'ALIVE' : 'BUILD_BROKEN';
} catch (error) {
  receipt.state = 'BUILD_BROKEN'; receipt.error = { name: error.name, message: error.message, stack: error.stack };
} finally {
  receipt.completedAt = new Date().toISOString();
  await writeFile(path.join(out, 'receipt.json'), `${JSON.stringify(receipt, null, 2)}\n`);
  console.log(`PACKAGE_MATRIX_RECEIPT ${JSON.stringify({ state: receipt.state, packageCount: receipt.packages.length, summary: receipt.summary || {}, receipt: '.artifacts/package-matrix/receipt.json' })}`);
  process.exitCode = receipt.state === 'ALIVE' ? 0 : 1;
}
