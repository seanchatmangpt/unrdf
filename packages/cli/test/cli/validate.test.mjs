/**
 * @file CLI Integration Tests for Validate Command
 * @module cli/test/validate
 * @description Tests for the validate command definition and argument structure
 */

import { describe, it, expect } from 'vitest';
import { validateCommand } from '../../src/cli/commands/validate.mjs';

describe('Validate Command', () => {
  it('should have correct command structure, meta, args, and run function', () => {
    // Command object
    expect(validateCommand).toBeDefined();
    expect(validateCommand.meta.name).toBe('validate');
    expect(validateCommand.meta.description).toContain('Validate');
    expect(typeof validateCommand.run).toBe('function');
    expect(validateCommand.run.constructor.name).toBe('AsyncFunction');

    // Required args exist
    const expectedArgs = ['file', 'format', 'summary', 'json'];
    for (const arg of expectedArgs) {
      expect(validateCommand.args[arg]).toBeDefined();
      expect(validateCommand.args[arg].description.length).toBeGreaterThan(5);
    }

    // Consistent kebab-case naming
    const argNames = Object.keys(validateCommand.args);
    for (const name of argNames) {
      expect(name).not.toMatch(/[A-Z]/);
    }
  });

  it('should have correct arg types, defaults, and required flags', () => {
    // file: string, required
    expect(validateCommand.args.file.type).toBe('string');
    expect(validateCommand.args.file.required).toBe(true);
    expect(validateCommand.args.file.description.toLowerCase()).toContain('rdf file');

    // format: string, default 'ntriples'
    expect(validateCommand.args.format.type).toBe('string');
    expect(validateCommand.args.format.default).toBe('ntriples');
    expect(validateCommand.args.format.description).toContain('RDF format');

    // summary: boolean, default false
    expect(validateCommand.args.summary.type).toBe('boolean');
    expect(validateCommand.args.summary.default).toBe(false);
    expect(validateCommand.args.summary.description).toContain('summary');

    // json: boolean, default false
    expect(validateCommand.args.json.type).toBe('boolean');
    expect(validateCommand.args.json.default).toBe(false);
    expect(validateCommand.args.json.description).toContain('JSON');
  });

  it('should support all RDF formats', () => {
    const formatArg = validateCommand.args.format;
    expect(formatArg.description).toContain('ntriples');
    expect(formatArg.description).toContain('turtle');
    expect(formatArg.description).toContain('rdfxml');
    expect(formatArg.description).toContain('jsonld');
  });
});
