#!/usr/bin/env node

import { createHash } from 'node:crypto';
import { mkdirSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';
import process from 'node:process';

const testDirectory = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(testDirectory, '..');
const repositoryRoot = resolve(packageRoot, '../..');
const outputDirectory = resolve(
  process.env.VERIFIER_OUT_DIR || join(packageRoot, '.artifacts', 'verification')
);

function digest(value) {
  return createHash('sha256').update(value).digest('hex');
}

function runCommand(name, command, args, cwd, timeoutMs) {
  const startedAt = Date.now();
  return new Promise(resolveRun => {
    const child = spawn(command, args, {
      cwd,
      env: { ...process.env, CI: '1' },
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    let stdout = '';
    let stderr = '';
    let timedOut = false;
    const timer = setTimeout(() => {
      timedOut = true;
      child.kill('SIGTERM');
      setTimeout(() => child.kill('SIGKILL'), 2_000).unref();
    }, timeoutMs);

    child.stdout.on('data', chunk => { stdout += chunk; });
    child.stderr.on('data', chunk => { stderr += chunk; });
    child.on('error', error => {
      clearTimeout(timer);
      resolveRun({
        name,
        command: [command, ...args],
        cwd,
        exitCode: null,
        signal: null,
        timedOut,
        durationMs: Date.now() - startedAt,
        stdoutDigest: digest(stdout),
        stderrDigest: digest(`${stderr}${error.stack || error.message}`),
        stdoutTail: stdout.slice(-4_000),
        stderrTail: `${stderr}${error.stack || error.message}`.slice(-4_000),
      });
    });
    child.on('close', (exitCode, signal) => {
      clearTimeout(timer);
      resolveRun({
        name,
        command: [command, ...args],
        cwd,
        exitCode,
        signal,
        timedOut,
        durationMs: Date.now() - startedAt,
        stdoutDigest: digest(stdout),
        stderrDigest: digest(stderr),
        stdoutTail: stdout.slice(-4_000),
        stderrTail: stderr.slice(-4_000),
      });
    });
  });
}

function stable(value) {
  if (Array.isArray(value)) return value.map(stable);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map(key => [key, stable(value[key])]));
  }
  return value;
}

function markdown(receipt) {
  const rows = receipt.commands.map(command =>
    `| ${command.name} | \`${command.command.join(' ')}\` | ${command.exitCode ?? 'spawn-error'} | ${command.durationMs} | ${command.timedOut ? 'yes' : 'no'} |`
  ).join('\n');
  return `# Daemon Integration Verification\n\n` +
    `**Standing:** ${receipt.standing}\n\n` +
    `**Receipt:** \`${receipt.digest}\`\n\n` +
    `| Gate | Command | Exit | Duration (ms) | Timed out |\n` +
    `|---|---|---:|---:|---|\n${rows}\n\n` +
    `This report contains only observations from the commands above. It makes no repository-wide production-readiness or performance claim.\n`;
}

async function main() {
  const commands = await Promise.all([
    runCommand('daemon-tests', 'pnpm', ['test'], packageRoot, 180_000),
    runCommand('daemon-lint', 'pnpm', ['lint'], packageRoot, 120_000),
    runCommand(
      'daemon-wip-audit',
      process.execPath,
      [join(repositoryRoot, 'scripts', 'audit-wip.mjs'), '--scope', 'packages/daemon/src', '--json'],
      repositoryRoot,
      30_000
    ),
  ]);

  const commandFailure = commands.some(command => command.exitCode !== 0 || command.timedOut);
  const receiptWithoutDigest = {
    schema: 'urn:unrdf:daemon:integration-verification:v1',
    subject: {
      package: '@unrdf/daemon',
      repositoryRoot,
      packageRoot,
    },
    standing: commandFailure ? 'BUILD_BROKEN' : 'ALIVE',
    commands,
    exclusions: [
      'No performance standing is inferred without benchmark execution.',
      'No repository-wide standing is inferred from package-local gates.',
    ],
  };
  const receipt = {
    ...receiptWithoutDigest,
    digest: digest(JSON.stringify(stable(receiptWithoutDigest))),
  };

  mkdirSync(outputDirectory, { recursive: true });
  writeFileSync(join(outputDirectory, 'daemon-integration-verification.json'), `${JSON.stringify(receipt, null, 2)}\n`);
  writeFileSync(join(outputDirectory, 'daemon-integration-verification.md'), markdown(receipt));
  process.stdout.write(`${JSON.stringify(receipt, null, 2)}\n`);
  process.exitCode = commandFailure ? 1 : 0;
}

main().catch(error => {
  process.stderr.write(`${error.stack || error.message}\n`);
  process.exitCode = 1;
});
