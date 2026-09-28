/**
 * The CLI entry point must load and run. Unit tests import command modules
 * directly, so a syntax/link error in main.mjs (e.g. a duplicated import) went
 * unnoticed until someone ran the binary. This runs the real file.
 */
import { describe, it, expect } from 'vitest';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const MAIN = fileURLToPath(new URL('../src/cli/main.mjs', import.meta.url));
// The CLI's logger (consola) goes silent when NODE_ENV=test or TEST is set, which vitest
// sets; run the child the way a user would.
const userEnv = () => {
  const { TEST: _test, ...env } = process.env;
  return { ...env, NODE_ENV: 'production' };
};
const run = (...args) =>
  spawnSync(process.execPath, [MAIN, ...args], { encoding: 'utf8', timeout: 30_000, env: userEnv() });

describe('unrdf main.mjs entry point', () => {
  it('loads and prints usage listing every registered command', () => {
    const result = run('--help');
    expect(result.status, result.stderr).toBe(0);
    for (const command of ['graph', 'query', 'hooks', 'daemon', 'atomvm', 'init', 'doctor']) {
      expect(result.stdout, `stderr: ${result.stderr}`).toContain(command);
    }
  });

  it('reports the package.json version, never a template placeholder', () => {
    const result = run('--version');
    expect(result.status, result.stderr).toBe(0);
    expect(result.stdout).not.toMatch(/[[\]{}]/);
    expect(result.stdout).toMatch(/\d+\.\d+\.\d+/);
  });
});
