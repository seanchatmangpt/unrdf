/**
 * @file CLI Integration Tests for Sync Command
 * @module cli/test/sync
 * @description Tests for the sync command definition and argument structure
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { mkdirSync, rmSync, existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { syncCommand } from '../../src/cli/commands/sync.mjs';

describe('Sync Command', () => {
  it('should have correct command structure, meta, args, and run function', () => {
    // Command object
    expect(syncCommand).toBeDefined();
    expect(syncCommand.meta.name).toBe('sync');
    expect(syncCommand.meta.description).toContain('code');
    expect(typeof syncCommand.run).toBe('function');
    expect(syncCommand.run.constructor.name).toBe('AsyncFunction');

    // Required args exist
    const expectedArgs = ['config', 'dry-run', 'verbose', 'force', 'rule', 'output'];
    for (const arg of expectedArgs) {
      expect(syncCommand.args[arg]).toBeDefined();
      expect(syncCommand.args[arg].description.length).toBeGreaterThan(5);
    }

    // Consistent kebab-case naming
    const argNames = Object.keys(syncCommand.args);
    for (const name of argNames) {
      expect(name).not.toMatch(/[A-Z]/);
    }
  });

  it('should have correct arg types, defaults, and aliases', () => {
    // config: string, default 'unrdf.toml'
    expect(syncCommand.args.config.type).toBe('string');
    expect(syncCommand.args.config.default).toBe('unrdf.toml');
    expect(syncCommand.args.config.description.toLowerCase()).toContain('unrdf.toml');

    // dry-run: boolean, default false
    expect(syncCommand.args['dry-run'].type).toBe('boolean');
    expect(syncCommand.args['dry-run'].default).toBe(false);
    expect(syncCommand.args['dry-run'].description).toMatch(/preview|without writing/i);

    // verbose: boolean, alias -v, default false
    expect(syncCommand.args.verbose.type).toBe('boolean');
    expect(syncCommand.args.verbose.alias).toBe('v');
    expect(syncCommand.args.verbose.default).toBe(false);

    // force: boolean, alias -f, default false
    expect(syncCommand.args.force.type).toBe('boolean');
    expect(syncCommand.args.force.alias).toBe('f');

    // rule: string, no default (optional)
    expect(syncCommand.args.rule.type).toBe('string');
    expect(syncCommand.args.rule.default).toBeUndefined();

    // output: string, default 'text'
    expect(syncCommand.args.output.type).toBe('string');
    expect(syncCommand.args.output.default).toBe('text');
  });

  it('should error when unrdf.toml not found, show helpful message, and accept custom config path', async () => {
    let testDir, originalCwd, consoleErrorSpy, processExitSpy;

    testDir = join(tmpdir(), 'unrdf-sync-test-' + Date.now());
    mkdirSync(testDir, { recursive: true });
    originalCwd = process.cwd();
    process.chdir(testDir);
    consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
    processExitSpy = vi.spyOn(process, 'exit').mockImplementation(() => { throw new Error('process.exit called'); });

    try {
      // Error when no config
      await expect(
        syncCommand.run({ args: { config: 'unrdf.toml', 'dry-run': false, verbose: false, force: false, output: 'text' } })
      ).rejects.toThrow('process.exit called');
      expect(consoleErrorSpy).toHaveBeenCalledWith(expect.stringContaining('Configuration file not found'));
      expect(processExitSpy).toHaveBeenCalledWith(1);

      // Shows example config
      const allCalls = consoleErrorSpy.mock.calls.flat().join('\n');
      expect(allCalls).toContain('[project]');
      expect(allCalls).toContain('[ontology]');
      expect(allCalls).toContain('[generation]');

      // Custom config path
      const customConfig = join(testDir, 'custom-config.toml');
      await expect(
        syncCommand.run({ args: { config: customConfig, 'dry-run': false, verbose: false, force: false, output: 'text' } })
      ).rejects.toThrow('process.exit called');
      expect(consoleErrorSpy).toHaveBeenCalledWith(expect.stringContaining(customConfig));
    } finally {
      process.chdir(originalCwd);
      if (existsSync(testDir)) rmSync(testDir, { recursive: true, force: true });
      consoleErrorSpy.mockRestore();
      processExitSpy.mockRestore();
    }
  });
});
