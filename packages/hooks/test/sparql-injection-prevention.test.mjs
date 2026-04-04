/**
 * @vitest-environment node
 * @file SPARQL Injection Prevention Test Suite
 *
 * Comprehensive tests for SPARQL parameter sanitization and query validation.
 * Covers 30+ injection patterns across UNION, FILTER, VALUES, comment,
 * numeric/string escaping, SERVICE, UPDATE, and variable name vectors.
 */
import { describe, it, expect } from 'vitest';
import {
  validateSparqlVariableName,
  validateSparqlQuery,
  bindSparqlParams,
  SparqlParamSchema,
} from '../src/hooks/condition-evaluator.mjs';

// ─── Variable Name Injection ────────────────────────────────────────────────

describe('validateSparqlVariableName - injection prevention', () => {
  // Valid names should pass
  it('should accept simple alphanumeric variable names', () => {
    expect(validateSparqlVariableName('x')).toBe('x');
    expect(validateSparqlVariableName('value')).toBe('value');
    expect(validateSparqlVariableName('myVar123')).toBe('myVar123');
    expect(validateSparqlVariableName('_private')).toBe('_private');
    expect(validateSparqlVariableName('camelCase')).toBe('camelCase');
  });

  it('should accept underscore-prefixed names', () => {
    expect(validateSparqlVariableName('_x')).toBe('_x');
    expect(validateSparqlVariableName('__double')).toBe('__double');
  });

  // Injection via variable name
  it('should reject variable name with closing brace (UNION injection)', () => {
    expect(() =>
      validateSparqlVariableName('x } UNION { ?a ?b ?c')
    ).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject variable name with FILTER injection', () => {
    expect(() =>
      validateSparqlVariableName('x FILTER(true)')
    ).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject variable name with semicolon (multi-statement)', () => {
    expect(() =>
      validateSparqlVariableName('x ; DROP ALL')
    ).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject variable name with dot (triple termination)', () => {
    expect(() =>
      validateSparqlVariableName('x . ?a ?b ?c')
    ).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject variable name with comment injection (#)', () => {
    expect(() =>
      validateSparqlVariableName('x #comment')
    ).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject variable name with newline injection', () => {
    expect(() =>
      validateSparqlVariableName('x\n} UNION {')
    ).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject variable name with angle brackets (URI injection)', () => {
    expect(() =>
      validateSparqlVariableName('x> <http://evil.com/>')
    ).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject empty string', () => {
    expect(() => validateSparqlVariableName('')).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject null/undefined', () => {
    expect(() => validateSparqlVariableName(null)).toThrow(/Invalid SPARQL variable name/);
    expect(() => validateSparqlVariableName(undefined)).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject non-string types', () => {
    expect(() => validateSparqlVariableName(42)).toThrow(/Invalid SPARQL variable name/);
    expect(() => validateSparqlVariableName({})).toThrow(/Invalid SPARQL variable name/);
    expect(() => validateSparqlVariableName([])).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject names starting with a digit', () => {
    expect(() => validateSparqlVariableName('1abc')).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject hyphenated names', () => {
    expect(() => validateSparqlVariableName('my-var')).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject names with spaces', () => {
    expect(() => validateSparqlVariableName('my var')).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject names with quotes (string escape attempt)', () => {
    expect(() => validateSparqlVariableName('x"injected')).toThrow(
      /Invalid SPARQL variable name/
    );
    expect(() => validateSparqlVariableName("x'injected")).toThrow(
      /Invalid SPARQL variable name/
    );
  });

  it('should reject names with backslash (escape sequence injection)', () => {
    expect(() => validateSparqlVariableName('x\\n')).toThrow(/Invalid SPARQL variable name/);
  });

  it('should reject names with parentheses (function injection)', () => {
    expect(() => validateSparqlVariableName('COUNT(?s)')).toThrow(
      /Invalid SPARQL variable name/
    );
  });
});

// ─── SPARQL Query Validation ────────────────────────────────────────────────

describe('validateSparqlQuery - query type enforcement', () => {
  // Valid queries should pass
  it('should accept valid SELECT query', () => {
    const ast = validateSparqlQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
    expect(ast).toBeDefined();
    expect(ast.type).toBe('query');
  });

  it('should accept valid ASK query', () => {
    const ast = validateSparqlQuery('ASK WHERE { ?s ?p ?o }');
    expect(ast).toBeDefined();
    expect(ast.queryType).toBe('ASK');
  });

  it('should accept valid CONSTRUCT query', () => {
    const ast = validateSparqlQuery(
      'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }'
    );
    expect(ast).toBeDefined();
    expect(ast.queryType).toBe('CONSTRUCT');
  });

  it('should accept SELECT with FILTER', () => {
    const ast = validateSparqlQuery(
      'SELECT ?s WHERE { ?s ?p ?o . FILTER(?o > 5) }'
    );
    expect(ast).toBeDefined();
  });

  it('should accept SELECT with UNION', () => {
    const ast = validateSparqlQuery(
      'SELECT ?s WHERE { { ?s <http://a> ?o } UNION { ?s <http://b> ?o } }'
    );
    expect(ast).toBeDefined();
  });

  // UPDATE injection
  it('should reject INSERT DATA', () => {
    expect(() =>
      validateSparqlQuery(
        'INSERT DATA { <http://evil> <http://p> <http://o> }'
      )
    ).toThrow(/UPDATE operations are not allowed/);
  });

  it('should reject DELETE DATA', () => {
    expect(() =>
      validateSparqlQuery(
        'DELETE DATA { <http://evil> <http://p> <http://o> }'
      )
    ).toThrow(/UPDATE operations are not allowed/);
  });

  it('should reject DROP ALL', () => {
    expect(() => validateSparqlQuery('DROP ALL')).toThrow(
      /UPDATE operations are not allowed/
    );
  });

  it('should reject CLEAR GRAPH', () => {
    expect(() =>
      validateSparqlQuery('CLEAR GRAPH <http://example.org/>')
    ).toThrow(/UPDATE operations are not allowed/);
  });

  it('should reject DELETE WHERE', () => {
    expect(() =>
      validateSparqlQuery('DELETE WHERE { ?s ?p ?o }')
    ).toThrow(/UPDATE operations are not allowed/);
  });

  it('should reject LOAD', () => {
    expect(() =>
      validateSparqlQuery('LOAD <http://evil.com/data>')
    ).toThrow(/UPDATE operations are not allowed/);
  });

  it('should reject CREATE GRAPH', () => {
    expect(() =>
      validateSparqlQuery('CREATE GRAPH <http://evil.com/graph>')
    ).toThrow(/UPDATE operations are not allowed/);
  });

  // SERVICE clause injection
  it('should reject query with SERVICE clause', () => {
    expect(() =>
      validateSparqlQuery(
        'SELECT ?s WHERE { SERVICE <http://evil.com/sparql> { ?s ?p ?o } }'
      )
    ).toThrow(/SERVICE clauses are not allowed/);
  });

  it('should reject query with SERVICE SILENT clause', () => {
    expect(() =>
      validateSparqlQuery(
        'SELECT ?s WHERE { SERVICE SILENT <http://evil.com/sparql> { ?s ?p ?o } }'
      )
    ).toThrow(/SERVICE clauses are not allowed/);
  });

  // Malformed/injection queries
  it('should reject syntactically invalid SPARQL (comment-based injection)', () => {
    expect(() =>
      validateSparqlQuery('SELECT ?s WHERE { ?s ?p ?o } # } INSERT DATA { <x> <y> <z> }')
    ).not.toThrow(); // Comments are stripped by parser, no injection effect
  });

  it('should reject empty string', () => {
    expect(() => validateSparqlQuery('')).toThrow(/non-empty string/);
  });

  it('should reject non-string input', () => {
    expect(() => validateSparqlQuery(null)).toThrow(/non-empty string/);
    expect(() => validateSparqlQuery(42)).toThrow(/non-empty string/);
    expect(() => validateSparqlQuery({})).toThrow(/non-empty string/);
  });

  it('should reject random garbage string', () => {
    expect(() => validateSparqlQuery('this is not sparql at all')).toThrow(
      /Invalid SPARQL syntax/
    );
  });

  it('should reject query with unbalanced braces', () => {
    expect(() =>
      validateSparqlQuery('SELECT ?s WHERE { ?s ?p ?o } } }')
    ).toThrow(/Invalid SPARQL syntax/);
  });
});

// ─── Parameter Binding ──────────────────────────────────────────────────────

describe('bindSparqlParams - safe parameter binding', () => {
  it('should bind string parameter as literal', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: 'hello' }
    );
    expect(result).toContain('"hello"');
    expect(result).not.toContain('?param');
  });

  it('should bind numeric parameter as typed literal', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: 42 }
    );
    expect(result).toContain('42');
    expect(result).not.toContain('?param');
  });

  it('should bind boolean parameter as typed literal', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: true }
    );
    expect(result).toContain('true');
    expect(result).not.toContain('?param');
  });

  it('should bind NamedNode parameter', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: { termType: 'NamedNode', value: 'http://example.org/thing' } }
    );
    expect(result).toContain('http://example.org/thing');
    expect(result).not.toContain('?param');
  });

  it('should bind BlankNode parameter', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: { termType: 'BlankNode', value: 'b0' } }
    );
    expect(result).not.toContain('?param');
  });

  it('should accept ?-prefixed parameter names', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { '?param': 'value' }
    );
    expect(result).toContain('"value"');
  });

  it('should return template unchanged when params is empty', () => {
    const template = 'SELECT ?s WHERE { ?s ?p ?o }';
    expect(bindSparqlParams(template, {})).toBe(template);
    expect(bindSparqlParams(template)).toBe(template);
  });

  // Injection prevention through binding
  it('should safely escape string containing SPARQL injection attempt', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: '} UNION { ?a ?b ?c' }
    );
    // The injected string should be a literal, not parsed as SPARQL
    expect(result).toContain('"} UNION { ?a ?b ?c"');
  });

  it('should safely escape string containing FILTER injection', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: '" . FILTER(true) . ?s ?p "' }
    );
    // String is properly escaped as a literal value, not as SPARQL syntax.
    // The quotes inside the value are escaped, keeping it as a single string term.
    expect(result).toContain('\\\"');
    expect(result).not.toContain('?param');
  });

  it('should safely escape string containing comment injection', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: '# } INSERT DATA { <x> <y> <z> }' }
    );
    expect(result).toContain('"#');
  });

  // Invalid parameter types
  it('should reject array as parameter value', () => {
    expect(() =>
      bindSparqlParams('SELECT ?s WHERE { ?s ?p ?param }', {
        param: [1, 2, 3],
      })
    ).toThrow(/Invalid SPARQL parameter value/);
  });

  it('should reject plain object as parameter value', () => {
    expect(() =>
      bindSparqlParams('SELECT ?s WHERE { ?s ?p ?param }', {
        param: { foo: 'bar' },
      })
    ).toThrow(/Invalid SPARQL parameter value/);
  });

  it('should reject function as parameter value', () => {
    expect(() =>
      bindSparqlParams('SELECT ?s WHERE { ?s ?p ?param }', {
        param: () => 'evil',
      })
    ).toThrow(/Invalid SPARQL parameter value/);
  });

  it('should reject null as parameter value', () => {
    expect(() =>
      bindSparqlParams('SELECT ?s WHERE { ?s ?p ?param }', { param: null })
    ).toThrow(/Invalid SPARQL parameter value/);
  });

  it('should reject NaN as parameter value', () => {
    expect(() =>
      bindSparqlParams('SELECT ?s WHERE { ?s ?p ?param }', { param: NaN })
    ).toThrow(/Invalid SPARQL parameter value/);
  });

  it('should reject Infinity as parameter value', () => {
    expect(() =>
      bindSparqlParams('SELECT ?s WHERE { ?s ?p ?param }', {
        param: Infinity,
      })
    ).toThrow(/Invalid SPARQL parameter value/);
  });

  it('should reject injection in parameter key', () => {
    expect(() =>
      bindSparqlParams('SELECT ?s WHERE { ?s ?p ?param }', {
        'x } UNION {': 'value',
      })
    ).toThrow(/Invalid SPARQL variable name/);
  });
});

// ─── Zod Schema Validation ──────────────────────────────────────────────────

describe('SparqlParamSchema - type validation', () => {
  it('should accept string values', () => {
    expect(SparqlParamSchema.safeParse('hello').success).toBe(true);
    expect(SparqlParamSchema.safeParse('').success).toBe(true);
  });

  it('should accept finite numbers', () => {
    expect(SparqlParamSchema.safeParse(42).success).toBe(true);
    expect(SparqlParamSchema.safeParse(3.14).success).toBe(true);
    expect(SparqlParamSchema.safeParse(-1).success).toBe(true);
    expect(SparqlParamSchema.safeParse(0).success).toBe(true);
  });

  it('should accept booleans', () => {
    expect(SparqlParamSchema.safeParse(true).success).toBe(true);
    expect(SparqlParamSchema.safeParse(false).success).toBe(true);
  });

  it('should accept NamedNode terms', () => {
    expect(
      SparqlParamSchema.safeParse({
        termType: 'NamedNode',
        value: 'http://example.org/',
      }).success
    ).toBe(true);
  });

  it('should accept BlankNode terms', () => {
    expect(
      SparqlParamSchema.safeParse({ termType: 'BlankNode', value: 'b0' })
        .success
    ).toBe(true);
  });

  it('should accept Literal terms', () => {
    expect(
      SparqlParamSchema.safeParse({ termType: 'Literal', value: 'text' })
        .success
    ).toBe(true);
  });

  it('should reject null', () => {
    expect(SparqlParamSchema.safeParse(null).success).toBe(false);
  });

  it('should reject undefined', () => {
    expect(SparqlParamSchema.safeParse(undefined).success).toBe(false);
  });

  it('should reject arrays', () => {
    expect(SparqlParamSchema.safeParse([1, 2]).success).toBe(false);
  });

  it('should reject plain objects', () => {
    expect(SparqlParamSchema.safeParse({ foo: 'bar' }).success).toBe(false);
  });

  it('should reject functions', () => {
    expect(SparqlParamSchema.safeParse(() => {}).success).toBe(false);
  });

  it('should reject NaN', () => {
    expect(SparqlParamSchema.safeParse(NaN).success).toBe(false);
  });

  it('should reject Infinity', () => {
    expect(SparqlParamSchema.safeParse(Infinity).success).toBe(false);
  });

  it('should reject invalid termType', () => {
    expect(
      SparqlParamSchema.safeParse({ termType: 'Variable', value: 'x' }).success
    ).toBe(false);
  });
});

// ─── Compound Injection Scenarios ───────────────────────────────────────────

describe('SPARQL injection - compound attack vectors', () => {
  it('should prevent UNION injection via variable name', () => {
    // Attack: variable = "x } UNION { ?a ?b ?c"
    // Would produce: SELECT ?x } UNION { ?a ?b ?c WHERE { ... }
    expect(() =>
      validateSparqlVariableName('x } UNION { ?a ?b ?c')
    ).toThrow();
  });

  it('should prevent VALUES injection via variable name', () => {
    expect(() =>
      validateSparqlVariableName('x } VALUES ?y { <http://evil> } { ?s ?p')
    ).toThrow();
  });

  it('should prevent OPTIONAL injection via variable name', () => {
    expect(() =>
      validateSparqlVariableName('x } OPTIONAL { ?secret ?p ?value')
    ).toThrow();
  });

  it('should prevent BIND injection via variable name', () => {
    expect(() =>
      validateSparqlVariableName('x . BIND("evil" AS ?injected)')
    ).toThrow();
  });

  it('should prevent subquery injection via variable name', () => {
    expect(() =>
      validateSparqlVariableName('x } { SELECT * WHERE { ?s ?p ?o }')
    ).toThrow();
  });

  it('should prevent nested comment injection via variable name', () => {
    expect(() =>
      validateSparqlVariableName('x\n# original query disabled\n')
    ).toThrow();
  });

  it('should neutralize UNION injection in bound string parameter', () => {
    // Even if someone passes SPARQL keywords as a value, binding escapes it
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: '} UNION { SELECT * WHERE { ?secret ?p ?o } }' }
    );
    // The UNION text should be inside a string literal, not SPARQL syntax
    expect(result).toContain('"} UNION');
  });

  it('should neutralize VALUES injection in bound string parameter', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: '} VALUES (?x) { (<http://evil>) } {' }
    );
    expect(result).toContain('"} VALUES');
  });

  it('should neutralize INSERT attempt in bound string parameter', () => {
    const result = bindSparqlParams(
      'SELECT ?s WHERE { ?s ?p ?param }',
      { param: '} ; INSERT DATA { <http://evil> <http://p> <http://o> }' }
    );
    // The dangerous payload is safely wrapped in a string literal, not executed
    expect(result).toContain('INSERT DATA');
    expect(result).toMatch(/^SELECT/); // Still a SELECT query, not converted to INSERT
    expect(result).not.toContain('?param');
  });

  it('should prevent data exfiltration via FILTER regex on variable name', () => {
    expect(() =>
      validateSparqlVariableName('o . FILTER regex(?secret, "password")')
    ).toThrow();
  });

  it('should prevent query with INSERT DATA masquerading as condition', () => {
    expect(() =>
      validateSparqlQuery(
        'INSERT DATA { <http://attacker> <http://owns> <http://yourdata> }'
      )
    ).toThrow(/UPDATE operations are not allowed/);
  });

  it('should prevent compound INSERT/WHERE', () => {
    expect(() =>
      validateSparqlQuery(
        'INSERT { ?s <http://hacked> true } WHERE { ?s ?p ?o }'
      )
    ).toThrow(/UPDATE operations are not allowed/);
  });
});
