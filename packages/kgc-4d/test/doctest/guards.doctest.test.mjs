import { describe, test, expect } from 'vitest';

import { guardMonotonicOrdering } from '../../src/guards.mjs'
import { guardTimeEnvironment } from '../../src/guards.mjs'
import { guardISOFormat } from '../../src/guards.mjs'
import { guardBigIntRange } from '../../src/guards.mjs'

describe('Doctests: guards.mjs', () => {
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
    const result = guardMonotonicOrdering(100n, 99n);
console.assert(result === 100n, 'Forwards time unchanged');
const wrapped = guardMonotonicOrdering(99n, 100n);
console.assert(wrapped === 101n, 'Backwards time incremented');
  });

  test('guardTimeEnvironment example 2 (line 38)', async () => {
    const hasNodeEnv = guardTimeEnvironment();
console.assert(typeof hasNodeEnv === 'boolean', 'Returns boolean');
  });

  test('guardISOFormat example 3 (line 59)', async () => {
    guardISOFormat('2025-01-15T10:30:00.000Z');
try {
guardISOFormat('not-an-iso');
throw new Error('Should have thrown');
} catch (err) {
console.assert(err instanceof Error, 'Throws on invalid format');
}
  });

  test('guardBigIntRange example 4 (line 93)', async () => {
    guardBigIntRange(1000000000n);
try {
guardBigIntRange(-1n);
throw new Error('Should reject negative');
} catch (err) {
console.assert(err instanceof RangeError, 'Throws RangeError for invalid range');
}
  });
});
