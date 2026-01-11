import { describe, test, expect } from 'vitest';

import { parseTemporalQuery } from '../../src/temporal-query-parser.mjs'

describe('Doctests: temporal-query-parser.mjs', () => {
  test('result example 1 (line 1)', async () => {
    const result = parseTemporalQuery(`
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
AT TIMESTAMP '2026-01-01T00:00:00Z'
`);
// result.mode: 'point-in-time'
// result.timestamp: '2026-01-01T00:00:00Z'
// result.baseSparql: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
  });

  test('parseTemporalQuery example 2 (line 25)', async () => {
    const result = parseTemporalQuery(`
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
AT TIMESTAMP '2026-01-01T00:00:00Z'
`);
  });

  test('extractBaseSparql example 3 (line 111)', async () => {
    const base = extractBaseSparql(`
SELECT ?s ?p ?o WHERE { ?s ?p ?o }
AT TIMESTAMP '2026-01-01T00:00:00Z'
`);
// base: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
  });

  test('hasTemporalClauses example 4 (line 129)', async () => {
    const hasTemporal = hasTemporalClauses(`SELECT ?s ?p ?o AT TIMESTAMP '...'`);
// hasTemporal: true
  });

  test('validateTemporalQuery example 5 (line 144)', async () => {
    const validation = validateTemporalQuery(`SELECT ?s ?p ?o AT TIMESTAMP 'invalid'`);
// validation.valid: false
  });
});
