#!/usr/bin/env node
/**
 * Production package verifier.
 *
 * Separates observation, execution, release closure and standing. A package is
 * ALIVE only when its admitted public surface imports, every applicable local
 * gate executes without masking, and every runtime dependency is itself ALIVE.
 */
import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { createWriteStream, existsSync } from 'node:fs';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { ALL_PACKAGES } from '../src/generated/package-exports.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const out = path.join(root, '.artifacts/package-matrix');
const pnpm = process.platform === 'win32' ? 'pnpm.cmd' : 'pnpm';
const concurrency = +(process.argv.find(x => x.startsWith('--concurrency='))?.split('=')[1] || 6);
const timeoutMs = +(process.argv.find(x => x.startsWith('--timeout-ms='))?.split('=')[1] || 120000);
const masked = /\|\|\s*true\b|;\s*exit\s+0\b/;
const noop = /\b(echo|printf)\b.*\b(no|skip|not applicable)\b.*\b(test|build|lint)/i;
const sha256 = value => createHash('sha256').update(value).digest('hex');
const relative = value => path.relative(root, value).split(path.sep).join('/');
const tail = (current, addition, max = 16384) => (current + addition).slice(-max);

function sourceIdentity() {
  return process.env.GITHUB_HEAD_SHA || process.env.GITHUB_SHA || null;
}

async function killTree(child) {
  if (!child.pid) return;
  try {
    if (process.platform !== 'win32') process.kill(-child.pid, 'SIGTERM');
    else child.kill('SIGTERM');
  } catch {}
  await new Promise(resolve => setTimeout(resolve, 750));
  try {
    if (process.platform !== 'win32') process.kill(-child.pid, 'SIGKILL');
    else child.kill('SIGKILL');
  } catch {}
}

async function run(cmd, args, cwd, logPath, ms = timeoutMs) {
  await mkdir(path.dirname(logPath), { recursive: true });
  const log = createWriteStream(logPath);
  const start = process.hrtime.bigint();
  let stdout = '', stderr = '', spawnError = null, timedOut = false;
  const child = spawn(cmd, args, {
    cwd,
    env: { ...process.env, CI: process.env.CI || '1' },
    stdio: ['ignore', 'pipe', 'pipe'],
    detached: process.platform !== 'win32',
  });
  child.stdout.on('data', chunk => { const text = chunk.toString(); stdout = tail(stdout, text, 1024 * 1024); log.write(text); });
  child.stderr.on('data', chunk => { const text = chunk.toString(); stderr = tail(stderr, text); log.write(text); });
  child.once('error', error => { spawnError = error.message; });

  const timer = setTimeout(async () => {
    timedOut = true;
    await killTree(child);
  }, ms);
  const exitCode = await new Promise(resolve => child.once('close', code => resolve(code ?? 1)));
  clearTimeout(timer);
  await new Promise(resolve => log.end(resolve));
  return {
    command: [cmd, ...args], cwd: relative(cwd), log: relative(logPath), exitCode,
    timedOut, spawnError, durationMs: Math.round(Number(process.hrtime.bigint() - start) / 1e6),
    stdoutTail: stdout, stderrTail: stderr,
  };
}

async function pool(items, worker) {
  const results = new Array(items.length);
  let cursor = 0;
  await Promise.all(Array.from({ length: Math.min(concurrency, Math.max(1, items.length)) }, async () => {
    while (cursor < items.length) {
      const index = cursor++;
      results[index] = await worker(items[index], index);
    }
  }));
  return results;
}

function scriptStanding(script, result) {
  if (result.spawnError) return 'BLOCKED';
  if (result.timedOut || result.exitCode !== 0) return 'BUILD_BROKEN';
  if (masked.test(script)) return 'UNSUPPORTED';
  if (noop.test(script)) return 'NOT_APPLICABLE';
  return 'ALIVE';
}

function aggregateStanding(states, { publicSurface = true } = {}) {
  if (states.includes('BUILD_BROKEN')) return 'BUILD_BROKEN';
  if (states.includes('BLOCKED')) return 'BLOCKED';
  if (states.includes('UNSUPPORTED')) return 'UNSUPPORTED';
  if (publicSurface && !states.includes('ALIVE')) return 'UNKNOWN';
  return states.every(state => ['ALIVE', 'NOT_APPLICABLE'].includes(state)) ? 'ALIVE' : 'PARTIAL_ALIVE';
}

async function discoverWorkspace() {
  const result = await run(pnpm, ['list', '-r', '--depth', '-1', '--json'], root, path.join(out, 'discovery.log'), 120000);
  if (result.exitCode || result.timedOut || result.spawnError) throw new Error('WORKSPACE_DISCOVERY_FAILED');
  const packages = [];
  for (const item of JSON.parse(result.stdoutTail)) {
    const abs = path.resolve(item.path);
    if (abs === root || !abs.startsWith(path.join(root, 'packages') + path.sep)) continue;
    const manifest = JSON.parse(await readFile(path.join(abs, 'package.json'), 'utf8'));
    packages.push({ name: manifest.name, path: relative(abs), private: manifest.private === true, scripts: manifest.scripts || {} });
  }
  return packages.sort((a, b) => a.name.localeCompare(b.name));
}

function projectionParity(workspace) {
  const observed = workspace.map(pkg => `${pkg.name}\t${pkg.path}`).sort();
  const projected = ALL_PACKAGES.map(pkg => `${pkg.name}\t${pkg.path}`).sort();
  const missingFromProjection = observed.filter(item => !projected.includes(item));
  const phantomProjection = projected.filter(item => !observed.includes(item));
  return {
    state: missingFromProjection.length || phantomProjection.length ? 'BUILD_BROKEN' : 'ALIVE',
    observedCount: observed.length,
    projectedCount: projected.length,
    missingFromProjection,
    phantomProjection,
    digest: sha256(JSON.stringify(projected)),
  };
}

async function importSurface(pkg) {
  if (pkg.private && !pkg.entry) return { package: pkg.name, phase: 'import', state: 'NOT_APPLICABLE', reason: 'private package without declared root entry' };
  if (!pkg.entry) return { package: pkg.name, phase: 'import', state: 'BUILD_BROKEN', reason: 'declared public root entry missing from projection' };
  const target = path.resolve(root, pkg.path, pkg.entry);
  if (!existsSync(target)) return { package: pkg.name, phase: 'import', state: 'BUILD_BROKEN', reason: `projected entry missing: ${relative(target)}` };
  const result = await run(process.execPath, ['--input-type=module', '--eval', `await import(${JSON.stringify(pathToFileURL(target).href)})`], root, path.join(out, 'import', `${pkg.name.replace(/^@/, '').replace(/[^a-zA-Z0-9._-]+/g, '-')}.log`), 60000);
  return { package: pkg.name, phase: 'import', target: relative(target), state: result.exitCode || result.timedOut ? 'BUILD_BROKEN' : result.spawnError ? 'BLOCKED' : 'ALIVE', ...result };
}

async function main() {
  await mkdir(out, { recursive: true });
  const receipt = {
    schema: 'urn:unrdf:workspace-package-readiness-receipt:v3',
    source: { repository: 'seanchatmangpt/unrdf', commit: sourceIdentity() },
    startedAt: new Date().toISOString(),
    environment: { node: process.version, platform: process.platform, arch: process.arch },
    state: 'UNKNOWN', parity: null, packages: [], executions: [], dependencyClosure: [],
  };

  try {
    const workspace = await discoverWorkspace();
    receipt.parity = projectionParity(workspace);
    const workspaceByName = new Map(workspace.map(pkg => [pkg.name, pkg]));

    // Cheap, high-information public-surface proof first. Slow package tests can no longer
    // prevent the verifier from learning whether a package can even be imported.
    const imports = await pool(ALL_PACKAGES, importSurface);
    receipt.executions.push(...imports);

    for (const phase of ['lint', 'build', 'test']) {
      const targets = ALL_PACKAGES.filter(pkg => typeof workspaceByName.get(pkg.name)?.scripts?.[phase] === 'string');
      receipt.executions.push(...await pool(targets, async pkg => {
        const script = workspaceByName.get(pkg.name).scripts[phase];
        const result = await run(pnpm, ['--dir', path.join(root, pkg.path), 'run', phase], root, path.join(out, phase, `${pkg.name.replace(/^@/, '').replace(/[^a-zA-Z0-9._-]+/g, '-')}.log`));
        return { package: pkg.name, phase, script, state: scriptStanding(script, result), ...result };
      }));
      for (const pkg of ALL_PACKAGES.filter(pkg => typeof workspaceByName.get(pkg.name)?.scripts?.[phase] !== 'string')) {
        receipt.executions.push({ package: pkg.name, phase, script: null, state: 'NOT_APPLICABLE', reason: `no ${phase} script` });
      }
    }

    const executionsByPackage = new Map();
    for (const execution of receipt.executions) {
      const list = executionsByPackage.get(execution.package) || [];
      list.push(execution);
      executionsByPackage.set(execution.package, list);
    }

    for (const projection of ALL_PACKAGES) {
      const executions = executionsByPackage.get(projection.name) || [];
      const ownStanding = aggregateStanding(executions.map(item => item.state), { publicSurface: !projection.private });
      receipt.packages.push({
        ...projection,
        ownStanding,
        executed: executions.map(({ phase, state, exitCode, timedOut, durationMs, log, reason }) => ({ phase, state, exitCode, timedOut, durationMs, log, reason })),
      });
    }

    const standingByName = new Map(receipt.packages.map(pkg => [pkg.name, pkg.ownStanding]));
    let changed = true;
    const releaseStanding = new Map(standingByName);
    // Monotone fixed point: dependency failure can only reduce release standing, never raise it.
    while (changed) {
      changed = false;
      for (const pkg of receipt.packages) {
        const own = standingByName.get(pkg.name);
        const blockedDeps = pkg.dependencies.filter(dep => releaseStanding.get(dep) !== 'ALIVE');
        const next = own === 'ALIVE' && blockedDeps.length === 0 ? 'ALIVE' : own === 'BUILD_BROKEN' ? 'BUILD_BROKEN' : blockedDeps.length ? 'PARTIAL_ALIVE' : own;
        if (releaseStanding.get(pkg.name) !== next) { releaseStanding.set(pkg.name, next); changed = true; }
      }
    }

    for (const pkg of receipt.packages) {
      pkg.releaseStanding = releaseStanding.get(pkg.name);
      pkg.blockedBy = pkg.dependencies.filter(dep => releaseStanding.get(dep) !== 'ALIVE');
    }
    receipt.dependencyClosure = receipt.packages.filter(pkg => pkg.cyclic).map(pkg => ({ package: pkg.name, sccId: pkg.sccId, sccSize: pkg.sccSize, dependencies: pkg.dependencies }));
    receipt.summary = receipt.packages.reduce((summary, pkg) => {
      summary[pkg.releaseStanding] = (summary[pkg.releaseStanding] || 0) + 1;
      return summary;
    }, {});
    receipt.state = receipt.parity.state === 'ALIVE' && receipt.packages.every(pkg => pkg.private || pkg.releaseStanding === 'ALIVE') ? 'ALIVE' : 'BUILD_BROKEN';
  } catch (error) {
    receipt.state = 'BUILD_BROKEN';
    receipt.error = { name: error.name, message: error.message, stack: error.stack };
  } finally {
    receipt.completedAt = new Date().toISOString();
    await writeFile(path.join(out, 'receipt.json'), `${JSON.stringify(receipt, null, 2)}\n`);
    await writeFile(path.join(out, 'package-standing.tsv'), ['package\town\trelease\tblocked_by', ...receipt.packages.map(pkg => `${pkg.name}\t${pkg.ownStanding}\t${pkg.releaseStanding}\t${pkg.blockedBy.join(',')}`)].join('\n') + '\n');
    console.log(`PACKAGE_READINESS_RECEIPT ${JSON.stringify({ state: receipt.state, parity: receipt.parity?.state, packageCount: receipt.packages.length, summary: receipt.summary || {}, receipt: '.artifacts/package-matrix/receipt.json' })}`);
    process.exitCode = receipt.state === 'ALIVE' ? 0 : 1;
  }
}

main();
