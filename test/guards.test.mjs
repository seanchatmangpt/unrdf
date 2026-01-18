/**
 * @file Fast guard validation tests (minimal)
 * @description 3 essential tests for input guards
 */

import { describe, it, expect } from 'vitest';

const createSparqlValidator = () => {
  const validKeywords = ['SELECT', 'ASK', 'CONSTRUCT', 'DESCRIBE'];
  return {
    validate: (query) => {
      const keyword = query.trim().split(/\s+/)[0];
      return { valid: validKeywords.includes(keyword) };
    },
  };
};

const createFormatValidator = () => {
  const validFormats = ['json', 'table', 'yaml', 'csv'];
  return {
    validate: (format) => {
      if (!format) return { valid: true, format: 'table' };
      return { valid: validFormats.includes(format), format };
    },
  };
};

const createREPLValidator = () => ({
  validate: (input) => {
    const hasInfiniteLoop = /while\s*\(\s*true\s*\)/.test(input) || /for\s*\(\s*;;\s*\)/.test(input);
    return { valid: !hasInfiniteLoop, issues: hasInfiniteLoop ? ['Infinite loop'] : [] };
  },
});

describe('Guard Validation', () => {
  const sparql = createSparqlValidator();
  const format = createFormatValidator();
  const repl = createREPLValidator();

  it('SPARQL guard accepts valid keywords', () => {
    expect(sparql.validate('SELECT ?s WHERE { ?s ?p ?o }').valid).toBe(true);
    expect(sparql.validate('INVALID ?s').valid).toBe(false);
  });

  it('Format guard validates output types', () => {
    expect(format.validate('json').valid).toBe(true);
    expect(format.validate('invalid').valid).toBe(false);
  });

  it('REPL guard detects infinite loops', () => {
    expect(repl.validate('while (true) {}').valid).toBe(false);
    expect(repl.validate('for (;;) {}').valid).toBe(false);
    expect(repl.validate('x = 1').valid).toBe(true);
  });
});
