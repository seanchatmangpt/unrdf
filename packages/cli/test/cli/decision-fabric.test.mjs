/**
 * @file Decision Fabric CLI Tests
 * @module cli/test/decision-fabric
 * @description Test suite for decision fabric CLI command structure and exports
 *
 * Tests command structure for:
 * - decision command (strategic decision processing)
 * - pareto command (feature analysis)
 * - socratic command (assumption extraction)
 */

import { describe, it, expect } from 'vitest';
import { decisionCommand } from '../../src/cli/commands/decision.mjs';
import { paretoCommand } from '../../src/cli/commands/pareto.mjs';
import { socraticCommand } from '../../src/cli/commands/socratic.mjs';

describe('Decision Fabric Commands', () => {
  describe('Decision Command', () => {
    describe('command structure', () => {
      it('should export decisionCommand', () => {
        expect(decisionCommand).toBeDefined();
      });

      it('should have proper meta information', () => {
        expect(decisionCommand.meta).toBeDefined();
        expect(decisionCommand.meta.name).toBe('decision');
        expect(decisionCommand.meta.description).toContain('decision');
      });

      it('should have required arguments', () => {
        expect(decisionCommand.args).toBeDefined();
        expect(decisionCommand.args.statement).toBeDefined();
        expect(decisionCommand.args.statement.required).toBe(true);
      });

      it('should have optional features argument', () => {
        expect(decisionCommand.args.features).toBeDefined();
        expect(decisionCommand.args.features.type).toBe('string');
        expect(decisionCommand.args.features.alias).toBe('f');
      });

      it('should have optional output argument', () => {
        expect(decisionCommand.args.output).toBeDefined();
        expect(decisionCommand.args.output.type).toBe('string');
        expect(decisionCommand.args.output.default).toBe('text');
        expect(decisionCommand.args.output.alias).toBe('o');
      });

      it('should have run function', () => {
        expect(decisionCommand.run).toBeDefined();
        expect(typeof decisionCommand.run).toBe('function');
      });
    });
  });

  describe('Pareto Command', () => {
    describe('command structure', () => {
      it('should export paretoCommand', () => {
        expect(paretoCommand).toBeDefined();
      });

      it('should have proper meta information', () => {
        expect(paretoCommand.meta).toBeDefined();
        expect(paretoCommand.meta.name).toBe('pareto');
        expect(paretoCommand.meta.description).toContain('Pareto');
      });

      it('should have required file argument', () => {
        expect(paretoCommand.args).toBeDefined();
        expect(paretoCommand.args.file).toBeDefined();
        expect(paretoCommand.args.file.required).toBe(true);
        expect(paretoCommand.args.file.type).toBe('positional');
      });

      it('should have optional output argument', () => {
        expect(paretoCommand.args.output).toBeDefined();
        expect(paretoCommand.args.output.type).toBe('string');
        expect(paretoCommand.args.output.default).toBe('text');
        expect(paretoCommand.args.output.alias).toBe('o');
      });

      it('should support multiple output formats', () => {
        expect(paretoCommand.args.output.description).toContain('text');
        expect(paretoCommand.args.output.description).toContain('json');
        expect(paretoCommand.args.output.description).toContain('chart');
      });

      it('should have run function', () => {
        expect(paretoCommand.run).toBeDefined();
        expect(typeof paretoCommand.run).toBe('function');
      });
    });
  });

  describe('Socratic Command', () => {
    describe('command structure', () => {
      it('should export socraticCommand', () => {
        expect(socraticCommand).toBeDefined();
      });

      it('should have proper meta information', () => {
        expect(socraticCommand.meta).toBeDefined();
        expect(socraticCommand.meta.name).toBe('socratic');
        expect(socraticCommand.meta.description).toContain('Socratic');
      });

      it('should have required statement argument', () => {
        expect(socraticCommand.args).toBeDefined();
        expect(socraticCommand.args.statement).toBeDefined();
        expect(socraticCommand.args.statement.required).toBe(true);
        expect(socraticCommand.args.statement.type).toBe('positional');
      });

      it('should have optional output argument', () => {
        expect(socraticCommand.args.output).toBeDefined();
        expect(socraticCommand.args.output.type).toBe('string');
        expect(socraticCommand.args.output.default).toBe('text');
        expect(socraticCommand.args.output.alias).toBe('o');
      });

      it('should have run function', () => {
        expect(socraticCommand.run).toBeDefined();
        expect(typeof socraticCommand.run).toBe('function');
      });
    });
  });

  describe('Command Integration', () => {
    it('all decision fabric commands should be properly defined', () => {
      const commands = [decisionCommand, paretoCommand, socraticCommand];

      for (const cmd of commands) {
        // Meta validation
        expect(cmd.meta).toBeDefined();
        expect(cmd.meta.name).toBeDefined();
        expect(cmd.meta.description).toBeDefined();

        // Args validation
        expect(cmd.args).toBeDefined();

        // Run function validation
        expect(cmd.run).toBeDefined();
        expect(typeof cmd.run).toBe('function');
      }
    });

    it('all commands should have output format option', () => {
      const commands = [decisionCommand, paretoCommand, socraticCommand];

      for (const cmd of commands) {
        expect(cmd.args.output).toBeDefined();
        expect(cmd.args.output.alias).toBe('o');
        expect(cmd.args.output.default).toBe('text');
      }
    });

    it('commands should have unique names', () => {
      const names = [
        decisionCommand.meta.name,
        paretoCommand.meta.name,
        socraticCommand.meta.name,
      ];

      const uniqueNames = new Set(names);
      expect(uniqueNames.size).toBe(names.length);
    });
  });

  describe('Command Availability', () => {
    it('decision command should support features integration', () => {
      expect(decisionCommand.args.features).toBeDefined();
      expect(decisionCommand.args.features.description).toContain('features');
    });

    it('pareto command should describe feature analysis', () => {
      expect(paretoCommand.meta.description).toMatch(/feature|Pareto/i);
    });

    it('socratic command should describe assumption analysis', () => {
      expect(socraticCommand.meta.description).toMatch(/assumption|Socratic/i);
    });
  });
});
