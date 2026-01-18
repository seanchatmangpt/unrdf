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
  describe('Command Registration', () => {
    it('should export syncCommand as default and named export', () => {
      expect(syncCommand).toBeDefined();
      expect(typeof syncCommand).toBe('object');
    });

    it('should have correct meta.name', () => {
      expect(syncCommand.meta).toBeDefined();
      expect(syncCommand.meta.name).toBe('sync');
    });

    it('should have description in meta', () => {
      expect(syncCommand.meta.description).toBeDefined();
      expect(syncCommand.meta.description).toContain('code');
      expect(syncCommand.meta.description).toContain('RDF');
    });

    it('should have a run function', () => {
      expect(syncCommand.run).toBeDefined();
      expect(typeof syncCommand.run).toBe('function');
    });

    it('should define args object with all required flags', () => {
      expect(syncCommand.args).toBeDefined();
      expect(typeof syncCommand.args).toBe('object');

      // All expected args should exist
      const expectedArgs = ['config', 'dry-run', 'verbose', 'force', 'rule', 'output'];
      for (const arg of expectedArgs) {
        expect(syncCommand.args[arg]).toBeDefined();
      }
    });
  });

  describe('--help (args definition)', () => {
    it('should define config arg with string type', () => {
      const configArg = syncCommand.args.config;
      expect(configArg).toBeDefined();
      expect(configArg.type).toBe('string');
    });

    it('should define config arg with description', () => {
      const configArg = syncCommand.args.config;
      expect(configArg.description).toBeDefined();
      expect(configArg.description.toLowerCase()).toContain('ggen.toml');
    });

    it('should define dry-run arg with boolean type', () => {
      const dryRunArg = syncCommand.args['dry-run'];
      expect(dryRunArg).toBeDefined();
      expect(dryRunArg.type).toBe('boolean');
    });

    it('should define verbose arg with boolean type and alias', () => {
      const verboseArg = syncCommand.args.verbose;
      expect(verboseArg).toBeDefined();
      expect(verboseArg.type).toBe('boolean');
      expect(verboseArg.alias).toBe('v');
    });

    it('should define force arg with boolean type and alias', () => {
      const forceArg = syncCommand.args.force;
      expect(forceArg).toBeDefined();
      expect(forceArg.type).toBe('boolean');
      expect(forceArg.alias).toBe('f');
    });

    it('should define rule arg with string type', () => {
      const ruleArg = syncCommand.args.rule;
      expect(ruleArg).toBeDefined();
      expect(ruleArg.type).toBe('string');
    });

    it('should define output arg with string type and default', () => {
      const outputArg = syncCommand.args.output;
      expect(outputArg).toBeDefined();
      expect(outputArg.type).toBe('string');
      expect(outputArg.default).toBe('text');
    });

    it('should have descriptions for all args', () => {
      for (const [_name, arg] of Object.entries(syncCommand.args)) {
        expect(arg.description).toBeDefined();
        expect(arg.description.length).toBeGreaterThan(5);
      }
    });
  });

  describe('--config flag', () => {
    it('should have default value of ggen.toml', () => {
      const configArg = syncCommand.args.config;
      expect(configArg.default).toBe('ggen.toml');
    });

    it('should accept string type for path', () => {
      const configArg = syncCommand.args.config;
      expect(configArg.type).toBe('string');
    });

    it('should describe configuration file purpose', () => {
      const configArg = syncCommand.args.config;
      expect(configArg.description).toMatch(/path|config/i);
    });
  });

  describe('--dry-run flag', () => {
    it('should have default value of false', () => {
      const dryRunArg = syncCommand.args['dry-run'];
      expect(dryRunArg.default).toBe(false);
    });

    it('should be boolean type', () => {
      const dryRunArg = syncCommand.args['dry-run'];
      expect(dryRunArg.type).toBe('boolean');
    });

    it('should describe preview behavior', () => {
      const dryRunArg = syncCommand.args['dry-run'];
      expect(dryRunArg.description).toMatch(/preview|without writing/i);
    });
  });

  describe('--verbose flag', () => {
    it('should have default value of false', () => {
      const verboseArg = syncCommand.args.verbose;
      expect(verboseArg.default).toBe(false);
    });

    it('should have alias -v', () => {
      const verboseArg = syncCommand.args.verbose;
      expect(verboseArg.alias).toBe('v');
    });

    it('should be boolean type', () => {
      const verboseArg = syncCommand.args.verbose;
      expect(verboseArg.type).toBe('boolean');
    });

    it('should describe verbose output', () => {
      const verboseArg = syncCommand.args.verbose;
      expect(verboseArg.description).toMatch(/verbose|output/i);
    });
  });

  describe('--rule flag', () => {
    it('should accept string type for rule name', () => {
      const ruleArg = syncCommand.args.rule;
      expect(ruleArg.type).toBe('string');
    });

    it('should not have default value (optional)', () => {
      const ruleArg = syncCommand.args.rule;
      expect(ruleArg.default).toBeUndefined();
    });

    it('should describe rule filtering', () => {
      const ruleArg = syncCommand.args.rule;
      expect(ruleArg.description).toMatch(/rule|specified/i);
    });
  });

  describe('Default config file detection (ggen.toml)', () => {
    let testDir;
    let originalCwd;
    let consoleErrorSpy;
    let processExitSpy;

    beforeEach(() => {
      testDir = join(tmpdir(), 'unrdf-sync-test-' + Date.now());
      mkdirSync(testDir, { recursive: true });
      originalCwd = process.cwd();
      process.chdir(testDir);

      // Mock console.error and process.exit
      consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
      processExitSpy = vi.spyOn(process, 'exit').mockImplementation(() => {
        throw new Error('process.exit called');
      });
    });

    afterEach(() => {
      process.chdir(originalCwd);
      if (existsSync(testDir)) {
        rmSync(testDir, { recursive: true, force: true });
      }
      consoleErrorSpy.mockRestore();
      processExitSpy.mockRestore();
    });

    it('should error when ggen.toml does not exist', async () => {
      // No config file created
      await expect(
        syncCommand.run({
          args: {
            config: 'ggen.toml',
            'dry-run': false,
            verbose: false,
            force: false,
            output: 'text',
          },
        })
      ).rejects.toThrow('process.exit called');

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining('Configuration file not found')
      );
      expect(processExitSpy).toHaveBeenCalledWith(1);
    });

    it('should show helpful message when config not found', async () => {
      await expect(
        syncCommand.run({
          args: {
            config: 'ggen.toml',
            'dry-run': false,
            verbose: false,
            force: false,
            output: 'text',
          },
        })
      ).rejects.toThrow('process.exit called');

      // Should show example config
      const allCalls = consoleErrorSpy.mock.calls.flat().join('\n');
      expect(allCalls).toContain('[project]');
      expect(allCalls).toContain('[ontology]');
      expect(allCalls).toContain('[generation]');
    });

    it('should use default config path when not specified', () => {
      const configArg = syncCommand.args.config;
      expect(configArg.default).toBe('ggen.toml');
    });

    it('should accept custom config path', async () => {
      const customConfig = join(testDir, 'custom-config.toml');

      await expect(
        syncCommand.run({
          args: {
            config: customConfig,
            'dry-run': false,
            verbose: false,
            force: false,
            output: 'text',
          },
        })
      ).rejects.toThrow('process.exit called');

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining(customConfig)
      );
    });
  });

  describe('Arg Validation', () => {
    it('should have no required positional args', () => {
      // All args should have defaults or be optional
      for (const [_argName, arg] of Object.entries(syncCommand.args)) {
        // Either has default or is optional (no required: true for positional)
        const hasDefault = arg.default !== undefined;
        const isOptionalString = arg.type === 'string' && arg.default === undefined;
        expect(hasDefault || isOptionalString).toBe(true);
      }
    });

    it('should have correct types for all boolean flags', () => {
      const booleanFlags = ['dry-run', 'verbose', 'force'];
      for (const flag of booleanFlags) {
        expect(syncCommand.args[flag].type).toBe('boolean');
        expect(syncCommand.args[flag].default).toBe(false);
      }
    });

    it('should have correct types for all string args', () => {
      const stringArgs = ['config', 'rule', 'output'];
      for (const arg of stringArgs) {
        expect(syncCommand.args[arg].type).toBe('string');
      }
    });
  });

  describe('Command Structure Completeness', () => {
    it('should match citty command interface', () => {
      // Citty commands have: meta, args, run
      expect(syncCommand).toHaveProperty('meta');
      expect(syncCommand).toHaveProperty('args');
      expect(syncCommand).toHaveProperty('run');
    });

    it('should have consistent arg naming (kebab-case for multi-word)', () => {
      const argNames = Object.keys(syncCommand.args);
      for (const name of argNames) {
        // Should not have camelCase for multi-word args
        expect(name).not.toMatch(/[A-Z]/);
      }
    });

    it('should document all args with descriptions', () => {
      const argNames = Object.keys(syncCommand.args);
      expect(argNames.length).toBeGreaterThan(0);

      for (const argName of argNames) {
        const arg = syncCommand.args[argName];
        expect(arg.description).toBeDefined();
        expect(typeof arg.description).toBe('string');
        expect(arg.description.length).toBeGreaterThan(10);
      }
    });

    it('should have async run function', () => {
      // Check if run returns a promise (async function)
      expect(syncCommand.run.constructor.name).toBe('AsyncFunction');
    });
  });
});
