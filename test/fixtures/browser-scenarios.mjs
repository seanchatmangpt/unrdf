/**
 * @fileoverview Browser Test Scenarios
 * @module test/fixtures/browser-scenarios
 *
 * @description
 * Comprehensive browser compatibility test scenarios for UNRDF v3.1.0.
 * Includes feature detection, polyfill testing, and cross-browser compatibility.
 */

/**
 * Browser Feature Matrix
 * Defines which features are available in different environments
 */
export const browserFeatureMatrix = {
  Chrome: {
    version: '120+',
    features: {
      IndexedDB: true,
      WebWorker: true,
      WebAssembly: true,
      'crypto.randomUUID': true,
      'crypto.subtle': true,
      SharedArrayBuffer: true,
      BigInt: true,
      WeakRef: true,
      FinalizationRegistry: true,
      Atomics: true,
      structuredClone: true,
    },
    limitations: [],
  },
  Firefox: {
    version: '120+',
    features: {
      IndexedDB: true,
      WebWorker: true,
      WebAssembly: true,
      'crypto.randomUUID': true,
      'crypto.subtle': true,
      SharedArrayBuffer: true,
      BigInt: true,
      WeakRef: true,
      FinalizationRegistry: true,
      Atomics: true,
      structuredClone: true,
    },
    limitations: [],
  },
  Safari: {
    version: '16.4+',
    features: {
      IndexedDB: true,
      WebWorker: true,
      WebAssembly: true,
      'crypto.randomUUID': true,
      'crypto.subtle': true,
      SharedArrayBuffer: false, // Requires special headers
      BigInt: true,
      WeakRef: true,
      FinalizationRegistry: true,
      Atomics: true,
      structuredClone: true,
    },
    limitations: ['SharedArrayBuffer requires COOP/COEP headers'],
  },
  Edge: {
    version: '120+',
    features: {
      IndexedDB: true,
      WebWorker: true,
      WebAssembly: true,
      'crypto.randomUUID': true,
      'crypto.subtle': true,
      SharedArrayBuffer: true,
      BigInt: true,
      WeakRef: true,
      FinalizationRegistry: true,
      Atomics: true,
      structuredClone: true,
    },
    limitations: [],
  },
};

/**
 * IndexedDB Test Scenarios
 * Test scenarios for IndexedDB store implementation
 */
export const indexedDBScenarios = [
  {
    id: 'basic-store',
    name: 'Basic Quad Storage',
    description: 'Store and retrieve basic RDF quads',
    setup: async _db => {
      const quads = [
        {
          subject: { value: 'http://example.org/s1' },
          predicate: { value: 'http://example.org/p1' },
          object: { value: 'literal value' },
          graph: { value: 'http://example.org/g1' },
        },
      ];
      return quads;
    },
    test: async (store, quads) => {
      await store.addQuads(quads);
      const retrieved = await store.getQuads();
      return retrieved.length === quads.length;
    },
    expectedResult: true,
  },

  {
    id: 'large-dataset',
    name: 'Large Dataset Storage',
    description: 'Store 10,000+ quads efficiently',
    setup: async _db => {
      const quads = [];
      for (let i = 0; i < 10000; i++) {
        quads.push({
          subject: { value: `http://example.org/s${i}` },
          predicate: { value: 'http://example.org/predicate' },
          object: { value: `value${i}` },
          graph: { value: 'http://example.org/graph' },
        });
      }
      return quads;
    },
    test: async (store, quads) => {
      const start = performance.now();
      await store.addQuads(quads);
      const duration = performance.now() - start;
      const retrieved = await store.getQuads();
      return {
        stored: retrieved.length === 10000,
        performance: duration < 5000, // Should complete in < 5s
      };
    },
    expectedResult: { stored: true, performance: true },
  },

  {
    id: 'query-performance',
    name: 'Query Performance',
    description: 'Query large dataset efficiently',
    setup: async _db => {
      const quads = [];
      for (let i = 0; i < 10000; i++) {
        quads.push({
          subject: { value: `http://example.org/s${i % 100}` },
          predicate: { value: `http://example.org/p${i % 10}` },
          object: { value: `value${i}` },
          graph: { value: 'http://example.org/graph' },
        });
      }
      return quads;
    },
    test: async (store, quads) => {
      await store.addQuads(quads);

      const start = performance.now();
      const results = await store.getQuads({
        subject: { value: 'http://example.org/s42' },
      });
      const duration = performance.now() - start;

      return {
        found: results.length === 100,
        performance: duration < 200, // Should complete in < 200ms
      };
    },
    expectedResult: { found: true, performance: true },
  },

  {
    id: 'transaction-safety',
    name: 'Transaction Safety',
    description: 'Ensure transaction rollback on error',
    setup: async _db => {
      return [];
    },
    test: async (store, _quads) => {
      const initialCount = (await store.getQuads()).length;

      try {
        await store.beginTransaction();
        await store.addQuads([
          {
            subject: { value: 'http://example.org/s' },
            predicate: { value: 'http://example.org/p' },
            object: { value: 'test' },
            graph: { value: 'http://example.org/g' },
          },
        ]);
        throw new Error('Simulated error');
      } catch (error) {
        await store.rollback();
      }

      const finalCount = (await store.getQuads()).length;
      return finalCount === initialCount;
    },
    expectedResult: true,
  },

  {
    id: 'concurrent-access',
    name: 'Concurrent Access',
    description: 'Handle concurrent read/write operations',
    setup: async _db => {
      return [];
    },
    test: async (store, _quads) => {
      const promises = [];

      // Concurrent writes
      for (let i = 0; i < 10; i++) {
        promises.push(
          store.addQuads([
            {
              subject: { value: `http://example.org/s${i}` },
              predicate: { value: 'http://example.org/p' },
              object: { value: `value${i}` },
              graph: { value: 'http://example.org/g' },
            },
          ])
        );
      }

      await Promise.all(promises);
      const results = await store.getQuads();

      return results.length === 10;
    },
    expectedResult: true,
  },
];

/**
 * Web Worker Test Scenarios
 * Test scenarios for Web Worker implementation
 */
export const workerScenarios = [
  {
    id: 'basic-worker',
    name: 'Basic Worker Communication',
    description: 'Send and receive messages from worker',
    workerCode: `
      self.onmessage = (event) => {
        self.postMessage({ result: event.data.value * 2 });
      };
    `,
    test: async worker => {
      return new Promise(resolve => {
        worker.onmessage = event => {
          resolve(event.data.result === 84);
        };
        worker.postMessage({ value: 42 });
      });
    },
    expectedResult: true,
  },

  {
    id: 'rdf-parsing-worker',
    name: 'RDF Parsing in Worker',
    description: 'Parse RDF data in background worker',
    workerCode: `
      // Simplified RDF parser for worker
      self.onmessage = async (event) => {
        const { turtle } = event.data;
        // Simulate parsing
        const quads = turtle.split('\\n').filter(line => line.trim()).length;
        self.postMessage({ quads });
      };
    `,
    test: async worker => {
      const turtle = `
        @prefix ex: <http://example.org/> .
        ex:subject1 ex:predicate1 "value1" .
        ex:subject2 ex:predicate2 "value2" .
      `;

      return new Promise(resolve => {
        worker.onmessage = event => {
          resolve(event.data.quads === 2);
        };
        worker.postMessage({ turtle });
      });
    },
    expectedResult: true,
  },

  {
    id: 'worker-error-handling',
    name: 'Worker Error Handling',
    description: 'Handle errors in worker thread',
    workerCode: `
      self.onmessage = (event) => {
        if (event.data.shouldError) {
          throw new Error('Worker error');
        }
        self.postMessage({ success: true });
      };
    `,
    test: async worker => {
      return new Promise(resolve => {
        worker.onerror = _error => {
          resolve(true); // Error was caught
        };
        worker.postMessage({ shouldError: true });
      });
    },
    expectedResult: true,
  },
];

/**
 * Crypto API Test Scenarios
 * Test scenarios for Web Crypto API
 */
export const cryptoScenarios = [
  {
    id: 'random-uuid',
    name: 'Random UUID Generation',
    description: 'Generate cryptographically secure UUIDs',
    test: async () => {
      const uuid = crypto.randomUUID();
      const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
      return uuidRegex.test(uuid);
    },
    expectedResult: true,
  },

  {
    id: 'sha256-hash',
    name: 'SHA-256 Hashing',
    description: 'Generate SHA-256 hash using Web Crypto',
    test: async () => {
      const data = new TextEncoder().encode('test data');
      const hashBuffer = await crypto.subtle.digest('SHA-256', data);
      const hashArray = Array.from(new Uint8Array(hashBuffer));
      const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');

      // Expected SHA-256 hash of "test data"
      const expected = '916f0027a575074ce72a331777c3478d6513f786a591bd892da1a577bf2335f9';
      return hashHex === expected;
    },
    expectedResult: true,
  },
];

/**
 * File System Abstraction Scenarios
 * Test scenarios for browser file system abstraction
 */
export const fileSystemScenarios = [
  {
    id: 'basic-file-ops',
    name: 'Basic File Operations',
    description: 'Read and write files using browser FS abstraction',
    test: async fs => {
      await fs.writeFile('/test.txt', 'Hello World');
      const content = await fs.readFile('/test.txt', 'utf8');
      return content === 'Hello World';
    },
    expectedResult: true,
  },

  {
    id: 'directory-ops',
    name: 'Directory Operations',
    description: 'Create and list directories',
    test: async fs => {
      await fs.mkdir('/testdir');
      await fs.writeFile('/testdir/file1.txt', 'content1');
      await fs.writeFile('/testdir/file2.txt', 'content2');

      const exists = fs.existsSync('/testdir');
      const files = fs.readdirSync('/testdir');

      return exists && files.length === 2;
    },
    expectedResult: true,
  },

  {
    id: 'error-handling',
    name: 'File System Error Handling',
    description: 'Handle ENOENT and other file system errors',
    test: async fs => {
      try {
        await fs.readFile('/nonexistent.txt');
        return false;
      } catch (error) {
        return error.message.includes('ENOENT');
      }
    },
    expectedResult: true,
  },
];

/**
 * Performance Benchmarks
 * Browser-specific performance benchmarks
 */
export const performanceBenchmarks = [
  {
    id: 'parse-1k-triples',
    name: 'Parse 1K Triples',
    description: 'Parse 1,000 RDF triples in browser',
    threshold: 1000, // ms
    category: 'parsing',
  },
  {
    id: 'query-10k-store',
    name: 'Query 10K Quad Store',
    description: 'Query store with 10,000 quads',
    threshold: 500,
    category: 'querying',
  },
  {
    id: 'indexeddb-write-1k',
    name: 'IndexedDB Write 1K Quads',
    description: 'Write 1,000 quads to IndexedDB',
    threshold: 2000,
    category: 'storage',
  },
];

/**
 * Get scenario by ID
 * @param {string} scenarioType - Type of scenario (indexedDB, worker, crypto, etc.)
 * @param {string} id - Scenario ID
 * @returns {Object|null} Scenario or null
 */
export function getScenario(scenarioType, id) {
  const scenarios = {
    indexedDB: indexedDBScenarios,
    worker: workerScenarios,
    crypto: cryptoScenarios,
    fileSystem: fileSystemScenarios,
  };

  const scenarioList = scenarios[scenarioType];
  return scenarioList ? scenarioList.find(s => s.id === id) : null;
}

/**
 * Get all scenarios for a type
 * @param {string} scenarioType - Type of scenario
 * @returns {Array} Array of scenarios
 */
export function getScenariosByType(scenarioType) {
  const scenarios = {
    indexedDB: indexedDBScenarios,
    worker: workerScenarios,
    crypto: cryptoScenarios,
    fileSystem: fileSystemScenarios,
  };

  return scenarios[scenarioType] || [];
}

/**
 * Check browser feature support
 * @param {string} browser - Browser name
 * @param {string} feature - Feature name
 * @returns {boolean} True if feature is supported
 */
export function isBrowserFeatureSupported(browser, feature) {
  const matrix = browserFeatureMatrix[browser];
  return matrix ? matrix.features[feature] || false : false;
}

export default {
  browserFeatureMatrix,
  indexedDBScenarios,
  workerScenarios,
  cryptoScenarios,
  fileSystemScenarios,
  performanceBenchmarks,
  getScenario,
  getScenariosByType,
  isBrowserFeatureSupported,
};
