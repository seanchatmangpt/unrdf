#!/usr/bin/env node
/**
 * Production package verifier.
 *
 * Separates observation, execution, package standing, dependency-closed release
 * standing, and aggregate standing. ALIVE is manufactured only from executed
 * evidence against the admitted package graph.
 */
import { spawn, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { createWriteStream, existsSync } from 'node:fs';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { ALL_PACKAGES } from '../src/generated/package-exports.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const out = path.join(root, '.artifacts/package-matrix');
const observationReceiptPath = path.join(root, '.artifacts/package-observation/receipt.json');
const observationTurtlePath = path.join(root, '.artifacts/package-observation/package-topology.ttl');
const pnpm = process.platform === 'win32' ? 'pnpm.cmd' : 'pnpm';
const concurrency = +(process.argv.find(x => x.startsWith('--concurrency='))?.split('=')[1] || 6);
const timeoutMs = +(process.argv.find(x => x.startsWith('--timeout-ms='))?.split('=')[1] || 120000);
const masked = /\|\|\s*true\b|;\s*exit\s+0\b/;
const noop = /\b(echo|printf)\b.*\b(no|skip|not applicable)\b.*\b(test|build|lint)/i;
const fatalObservationCodes = new Set([
  'PACKAGE_MANIFEST_INVALID_JSON',
  'PACKAGE_NAME_MISSING',
  'PACKAGE_NAME_DUPLICATE',
  'INTERNAL_DEPENDENCY_MISSING',
]);
const standingSeverity = new Map([
  ['ALIVE', 0],
  ['NOT_APPLICABLE', 0],
  ['PARTIAL_ALIVE', 1],
  ['UNKNOWN', 2],
  ['UNSUPPORTED', 3],
  ['BLOCKED', 4],
  ['BUILD_BROKEN', 5],
]);
const sha256 = value => createHash('sha256').update(value).digest('hex');
const relative = value => path.relative(root, value).split(path.sep).join('/');
const tail = (current, addition, max = 16384) => (current + addition).slice(-max);

function sourceIdentity() {
  const fromEnvironment = process.env.GITHUB_HEAD_SHA || process.env.GITHUB_SHA;
  if (fromEnvironment) return fromEnvironment;
  const result = spawnSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8', timeout: 2000 });
  return result.status === 0 ? result.stdout.trim() : null;
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
  child.stdout.on('data', chunk => {
    const text = chunk.toString();
    stdout = tail(stdout, text, 1024 * 1024);
    log.write(text);
  });
  child.stderr.on('data', chunk => {
    const text = chunk.toString();
    stderr = tail(stderr, text);
    log.write(text);
  });
  child.once('error', error => { spawnError = error.message; });

  const timer = setTimeout(async () => {
    timedOut = true;
    await killTree(child);
  }, ms);
  const exitCode = await new Promise(resolve => child.once('close', code => resolve(code ?? 1)));
  clearTimeout(timer);
  await new Promise(resolve => log.end(resolve));
  return {
    command: [cmd, ...args],
    cwd: relative(cwd),
    log: relative(logPath),
    exitCode,
    timedOut,
    spawnError,
    durationMs: Math.round(Number(process.hrtime.bigint() - start) / 1e6),
    stdoutTail: stdout,
    stderrTail: stderr,
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
  if (states.includes('UNKNOWN')) return 'UNKNOWN';
  if (publicSurface && !states.includes('ALIVE')) return 'UNKNOWN';
  return states.every(state => ['ALIVE', 'NOT_APPLICABLE'].includes(state)) ? 'ALIVE' : 'PARTIAL_ALIVE';
}

function dependencyClosedStanding(own, dependencyStates) {
  if (own !== 'ALIVE') return own;
  const worst = dependencyStates.reduce((state, next) => {
    return (standingSeverity.get(next) || 0) > (standingSeverity.get(state) || 0) ? next : state;
  }, 'ALIVE');
  return worst === 'NOT_APPLICABLE' ? 'ALIVE' : worst;
}

async function ensureObservation() {
  if (!existsSync(observationReceiptPath) || !existsSync(observationTurtlePath)) {
    const execution = await run(
      process.execPath,
      ['scripts/unrdf-package-discovery.mjs'],
      root,
      path.join(out, 'observation.log'),
      30000,
    );
    if (execution.spawnError || execution.timedOut || !existsSync(observationReceiptPath) || !existsSync(observationTurtlePath)) {
      throw new Error(`PACKAGE_OBSERVATION_BLOCKED:${execution.spawnError || `exit=${execution.exitCode}`}`);
    }
  }
  const observation = JSON.parse(await readFile(observationReceiptPath, 'utf8'));
  if (!observation?.graphDigest || !Array.isArray(observation.packages) || !Array.isArray(observation.anomalies)) {
    throw new Error('PACKAGE_OBSERVATION_RECEIPT_INVALID');
  }
  return observation;
}

async function discoverWorkspace() {
  const result = await run(pnpm, ['list', '-r', '--depth', '-1', '--json'], root, path.join(out, 'discovery.log'), 120000);
  if (result.exitCode || result.timedOut || result.spawnError) throw new Error('WORKSPACE_DISCOVERY_FAILED');
  const packages = [];
  for (const item of JSON.parse(result.stdoutTail)) {
    const abs = path.resolve(item.path);
    if (abs === root) continue;
    const rel = relative(abs);
    const segments = rel.split('/');
    // The admitted release graph is packages/* only. Nested examples are separate
    // workspace fixtures, not independently released @unrdf package surfaces.
    if (segments.length !== 2 || segments[0] !== 'packages') continue;
    const manifest = JSON.parse(await readFile(path.join(abs, 'package.json'), 'utf8'));
    packages.push({ name: manifest.name, path: rel, private: manifest.private === true, scripts: manifest.scripts || {} });
  }
  return packages.sort((a, b) => a.name.localeCompare(b.name));
}

function projectionParity(workspace, observation) {
  const observed = workspace.map(pkg => `${pkg.name}\t${pkg.path}`).sort();
  const projected = ALL_PACKAGES.map(pkg => `${pkg.name}\t${pkg.path}`).sort();
  const admitted = observation.packages.map(pkg => `${pkg.name}\t${pkg.path}`).sort();
  const missingFromProjection = admitted.filter(item => !projected.includes(item));
  const phantomProjection = projected.filter(item => !admitted.includes(item));
  const workspaceVsObservation = {
    missingFromObservation: observed.filter(item => !admitted.includes(item)),
    phantomObservation: admitted.filter(item => !observed.includes(item)),
  };
  return {
    state: missingFromProjection.length || phantomProjection.length || workspaceVsObservation.missingFromObservation.length || workspaceVsObservation.phantomObservation.length
      ? 'BUILD_BROKEN'
      : 'ALIVE',
    workspaceCount: observed.length,
    observationCount: admitted.length,
    projectedCount: projected.length,
    missingFromProjection,
    phantomProjection,
    ...workspaceVsObservation,
    digest: sha256(JSON.stringify({ observed, admitted, projected })),
  };
}

function observationDefectsFor(pkg, observation) {
  return observation.anomalies.filter(anomaly => {
    if (anomaly.package === pkg.name) return true;
    if (typeof anomaly.path === 'string' && (anomaly.path === pkg.path || anomaly.path.startsWith(`${pkg.path}/`))) return true;
    return false;
  });
}

async function importSurface(pkg) {
  if (pkg.private && !pkg.entry) {
    return { package: pkg.name, phase: 'import', state: 'NOT_APPLICABLE', reason: 'private package without declared root entry' };
  }
  if (!pkg.entry) {
    return { package: pkg.name, phase: 'import', state: 'BUILD_BROKEN', reason: 'declared public root entry missing from projection' };
  }
  const target = path.resolve(root, pkg.path, pkg.entry);
  if (!existsSync(target)) {
    return { package: pkg.name, phase: 'import', state: 'BUILD_BROKEN', reason: `projected entry missing: ${relative(target)}` };
  }
  const result = await run(
    process.execPath,
    ['--input-type=module', '--eval', `await import(${JSON.stringify(pathToFileURL(target).href)})`],
    root,
    path.join(out, 'import', `${pkg.name.replace(/^@/, '').replace(/[^a-zA-Z0-9._-]+/g, '-')}.log`),
    60000,
  );
  return {
    package: pkg.name,
    phase: 'import',
    target: relative(target),
    state: result.exitCode || result.timedOut ? 'BUILD_BROKEN' : result.spawnError ? 'BLOCKED' : 'ALIVE',
    ...result,
  };
}

async function main() {
  await mkdir(out, { recursive: true });
  const receipt = {
    schema: 'urn:unrdf:workspace-package-readiness-receipt:v3',
    source: { repository: 'seanchatmangpt/unrdf', commit: sourceIdentity() },
    startedAt: new Date().toISOString(),
    environment: { node: process.version, platform: process.platform, arch: process.arch },
    state: 'UNKNOWN',
    observation: null,
    parity: null,
    refusal: null,
    packages: [],
    executions: [],
    dependencyClosure: [],
  };

  try {
    const observation = await ensureObservation();
    receipt.observation = {
      state: observation.state,
      graphDigest: observation.graphDigest,
      anomalyCount: observation.anomalies.length,
      stronglyConnectedComponents: observation.stronglyConnectedComponents,
      anomalies: observation.anomalies,
    };

    const workspace = await discoverWorkspace();
    receipt.parity = projectionParity(workspace, observation);

    // Zero unreceipted actuation: never execute package code against a stale or
    // phantom projection. ggen must first project the admitted graph and commit
    // that result. The next verifier run can then execute the exact subject.
    if (receipt.parity.state !== 'ALIVE') {
      receipt.state = 'BUILD_BROKEN';
      receipt.refusal = {
        code: 'PROJECTION_DRIFT',
        standing: 'BUILD_BROKEN',
        message: 'Committed generated package projection does not equal the admitted package observation graph',
        observationDigest: receipt.observation.graphDigest,
        parityDigest: receipt.parity.digest,
      };
      return;
    }

    const workspaceByName = new Map(workspace.map(pkg => [pkg.name, pkg]));

    // Cheap, high-information public-surface proof first. Slow package tests can no longer
    // prevent the verifier from learning whether a package can even be imported.
    const imports = await pool(ALL_PACKAGES, importSurface);
    receipt.executions.push(...imports);

    for (const phase of ['lint', 'build', 'test']) {
      const targets = ALL_PACKAGES.filter(pkg => typeof workspaceByName.get(pkg.name)?.scripts?.[phase] === 'string');
      receipt.executions.push(...await pool(targets, async pkg => {
        const script = workspaceByName.get(pkg.name).scripts[phase];
        const result = await run(
          pnpm,
          ['--dir', path.join(root, pkg.path), 'run', phase],
          root,
          path.join(out, phase, `${pkg.name.replace(/^@/, '').replace(/[^a-zA-Z0-9._-]+/g, '-')}.log`),
        );
        return { package: pkg.name, phase, script, state: scriptStanding(script, result), ...result };
      }));
      for (const pkg of ALL_PACKAGES.filter(pkg => typeof workspaceByName.get(pkg.name)?.scripts?.[phase] !== 'string')) {
        const requiredTestMissing = phase === 'test' && !pkg.private;
        receipt.executions.push({
          package: pkg.name,
          phase,
          script: null,
          state: requiredTestMissing ? 'UNSUPPORTED' : 'NOT_APPLICABLE',
          reason: requiredTestMissing ? 'public package has no executable test script' : `no ${phase} script`,
        });
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
      const observationDefects = observationDefectsFor(projection, observation);
      const fatalDefects = observationDefects.filter(defect => fatalObservationCodes.has(defect.code));
      const executedStanding = aggregateStanding(executions.map(item => item.state), { publicSurface: !projection.private });
      const ownStanding = fatalDefects.length ? 'BUILD_BROKEN' : executedStanding;
      receipt.packages.push({
        ...projection,
        observationDefects,
        executedStanding,
        ownStanding,
        executed: executions.map(({ phase, state, exitCode, timedOut, durationMs, log, reason }) => ({
          phase,
          state,
          exitCode,
          timedOut,
          durationMs,
          log,
          reason,
        })),
      });
    }

    const ownStandingByName = new Map(receipt.packages.map(pkg => [pkg.name, pkg.ownStanding]));
    const releaseStanding = new Map(ownStandingByName);
    let changed = true;
    // Monotone fixed point over the admitted dependency graph. One broken edge is topology;
    // its consequence propagates only through packages that actually depend on it.
    while (changed) {
      changed = false;
      for (const pkg of receipt.packages) {
        const dependencyStates = pkg.dependencies.map(dep => releaseStanding.get(dep) || 'UNKNOWN');
        const next = dependencyClosedStanding(ownStandingByName.get(pkg.name), dependencyStates);
        if ((standingSeverity.get(next) || 0) > (standingSeverity.get(releaseStanding.get(pkg.name)) || 0)) {
          releaseStanding.set(pkg.name, next);
          changed = true;
        }
      }
    }

    for (const pkg of receipt.packages) {
      pkg.releaseStanding = releaseStanding.get(pkg.name);
      pkg.blockedBy = pkg.dependencies
        .map(dep => ({ package: dep, standing: releaseStanding.get(dep) || 'UNKNOWN' }))
        .filter(dep => dep.standing !== 'ALIVE');
    }

    receipt.dependencyClosure = receipt.packages
      .filter(pkg => pkg.cyclic)
      .map(pkg => ({ package: pkg.name, sccId: pkg.sccId, sccSize: pkg.sccSize, dependencies: pkg.dependencies }));
    receipt.summary = receipt.packages.reduce((summary, pkg) => {
      summary[pkg.releaseStanding] = (summary[pkg.releaseStanding] || 0) + 1;
      return summary;
    }, {});

    const publicStates = receipt.packages.filter(pkg => !pkg.private).map(pkg => pkg.releaseStanding);
    if (receipt.observation.state === 'BUILD_BROKEN' || receipt.parity.state === 'BUILD_BROKEN' || publicStates.includes('BUILD_BROKEN')) {
      receipt.state = 'BUILD_BROKEN';
    } else if (publicStates.includes('BLOCKED')) {
      receipt.state = 'BLOCKED';
    } else if (publicStates.includes('UNSUPPORTED')) {
      receipt.state = 'UNSUPPORTED';
    } else if (receipt.observation.state === 'PARTIAL_ALIVE' || publicStates.some(state => state !== 'ALIVE')) {
      receipt.state = 'PARTIAL_ALIVE';
    } else {
      receipt.state = 'ALIVE';
    }
  } catch (error) {
    receipt.state = 'BUILD_BROKEN';
    receipt.error = { name: error.name, message: error.message, stack: error.stack };
  } finally {
    receipt.completedAt = new Date().toISOString();
    await writeFile(path.join(out, 'receipt.json'), `${JSON.stringify(receipt, null, 2)}\n`);
    await writeFile(path.join(out, 'package-standing.tsv'), [
      'package\texecuted\town\trelease\tblocked_by',
      ...receipt.packages.map(pkg => `${pkg.name}\t${pkg.executedStanding}\t${pkg.ownStanding}\t${pkg.releaseStanding}\t${pkg.blockedBy.map(dep => `${dep.package}:${dep.standing}`).join(',')}`),
    ].join('\n') + '\n');
    console.log(`PACKAGE_READINESS_RECEIPT ${JSON.stringify({
      state: receipt.state,
      observation: receipt.observation?.state,
      parity: receipt.parity?.state,
      refusal: receipt.refusal?.code || null,
      packageCount: receipt.packages.length,
      summary: receipt.summary || {},
      receipt: '.artifacts/package-matrix/receipt.json',
    })}`);
    process.exitCode = receipt.state === 'ALIVE' ? 0 : 1;
  }
}

main();
