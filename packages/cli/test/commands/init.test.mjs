/**
 * @file End-to-end test for `unrdf init` (real process, real filesystem, no mocks)
 *
 * The command is executed in a child process through citty's runMain, exactly as the CLI
 * entry point does, so process.exit codes and console output are the real ones.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { execFile } from 'child_process';
import { promisify } from 'util';
import { promises as fs } from 'fs';
import os from 'os';
import path from 'path';
import { fileURLToPath } from 'url';
import { initCommand } from '../../src/cli/commands/init.mjs';

const run = promisify(execFile);
const here = path.dirname(fileURLToPath(import.meta.url));
const runner = path.resolve(here, '../fixtures/run-init.mjs');

/** Run `unrdf init <args>`; resolves with { code, stdout, stderr } even on non-zero exit. */
async function runInit(args) {
  try {
    const { stdout, stderr } = await run(
      process.execPath,
      [runner, ...args],
      { cwd: path.resolve(here, '../..'), timeout: 60_000 }
    );
    return { code: 0, stdout, stderr };
  } catch (error) {
    return { code: error.code, stdout: error.stdout ?? '', stderr: error.stderr ?? '' };
  }
}

async function write(root, rel, content = 'x\n') {
  const file = path.join(root, rel);
  await fs.mkdir(path.dirname(file), { recursive: true });
  await fs.writeFile(file, content, 'utf8');
}

describe('unrdf init', () => {
  let root;

  beforeEach(async () => {
    root = await fs.mkdtemp(path.join(os.tmpdir(), 'unrdf-cli-init-'));
    await write(root, 'package.json', '{"name":"demo"}\n');
    await write(root, 'vitest.config.mjs', 'export default {};\n');
    await write(root, 'src/users/user.service.mjs', 'export const a = 1;\n');
    await write(root, 'src/orders/order.mjs', 'export const b = 2;\n');
    await write(root, 'test/users.test.mjs', 'export {};\n');
  });

  afterEach(async () => {
    await fs.rm(root, { recursive: true, force: true });
  });

  it('is a citty command named init with the documented flags', () => {
    expect(initCommand.meta.name).toBe('init');
    expect(Object.keys(initCommand.args)).toEqual(
      expect.arrayContaining(['root', 'dry-run', 'verbose', 'skip-snapshot', 'skip-hooks'])
    );
  });

  it('initialises a project end to end and writes .unrdf output files', async () => {
    const { code, stdout, stderr } = await runInit(['--root', root]);

    expect(stderr).toBe('');
    expect(code).toBe(0);
    expect(stdout).toContain('PROJECT INITIALIZATION REPORT');
    expect(stdout).toContain('Tech Stack: vitest');
    expect(stdout).toContain('Features: 2');
    expect(stdout).toContain('Domain Model: 2 entities');
    expect(stdout).toContain('Files: 5');

    const names = (await fs.readdir(path.join(root, '.unrdf'))).sort();
    expect(names).toEqual(['domain.nt', 'hooks.json', 'init-report.json', 'snapshot.json', 'templates.json']);

    const report = JSON.parse(await fs.readFile(path.join(root, '.unrdf/init-report.json'), 'utf8'));
    expect(report.stats.totalFiles).toBe(5);
    expect(report.domainEntities.map(e => e.name)).toEqual(['Order', 'User']);
  });

  it('--dry-run writes nothing', async () => {
    const { code, stdout } = await runInit(['--root', root, '--dry-run']);
    expect(code).toBe(0);
    expect(stdout).toContain('PROJECT INITIALIZATION REPORT');
    await expect(fs.stat(path.join(root, '.unrdf'))).rejects.toThrow(/ENOENT/);
  });

  it('exits non-zero with an error when the root does not exist', async () => {
    const { code, stderr } = await runInit(['--root', path.join(root, 'missing')]);
    expect(code).toBe(1);
    expect(stderr).toContain('Initialization failed');
    expect(stderr).toMatch(/scan:/);
  });
});
