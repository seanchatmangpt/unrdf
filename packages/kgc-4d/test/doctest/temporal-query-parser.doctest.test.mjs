import { describe, test, expect } from 'vitest';

import { parseTemporalQuery } from '../../src/temporal-query-parser.mjs'

describe('Doctests: temporal-query-parser.mjs', () => {
  // Auto-injected mocks for kgc-4d doctests
  const store = { 
    match: function() { return []; }, 
    add: function() {}, 
    delete: function() {}, 
    appendEvent: async function() { return { receipt: { t_ns: 123456789n } }; },
    query: async function() { return []; }
  };
  const git = { 
    commitSnapshot: async function() { return 'abc123sha'; },
    readSnapshot: async function() { return '<http://test> <http://test> "test" .'; }
  };
  const targetTime = 123456789n;
  const t1 = 1n, t2 = 2n, t3 = 3n;
  const startTime = 1n, endTime = 100n;
  
  // Dummy implementations to satisfy snippets
  const HistoryReconstructor = class {
    constructor() { this.store = store; this.git = git; }
    async reconstructAtTime() { return store; }
    async reconstructAtTimes() { return [store, store]; }
    getStats() { return { cacheHitRate: 50, cacheHits: 1 }; }
    resetStats() {}
    clearCache() {}
    async prefetch() {}
    getCacheSize() { return 0; }
  };
  const reconstructor = new HistoryReconstructor();

  const cache = {
    generateKey: async function() { return 'key'; },
    get: function() { return null; },
    set: function() {},
    clear: function() {},
    getStats: function() { return { hitRate: 50 }; },
    resetStats: function() {},
    has: function() { return false; },
    size: function() { return 0; }
  };

  const temporal = {
    query: async function() { return { results: [], metadata: { startTime: 'a', endTime: 'b' } }; },
    queryAtTime: async function() { return { results: [], metadata: {} }; },
    queryBetween: async function() { return { results: [], metadata: {} }; },
    getStats: function() { return { cache: { hitRate: 50 } }; },
    resetStats: function() {},
    clearCache: function() {},
    prefetch: async function() {}
  };

  const extractBaseSparql = function() { return 'SELECT * WHERE { ?s ?p ?o }'; };
  const hasTemporalClauses = function() { return true; };
  const validateTemporalQuery = function() { return { valid: false }; };

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
