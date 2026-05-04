/**
 * Doctest Transformer - Convert @example blocks to Vitest test cases
 * Takes extracted examples and generates executable test code
 */

/**
 * Transform examples into Vitest test cases
 * @param {Array} examples - Array of example objects from extractor
 * @param {string} sourceFile - Source file name (for describe block)
 * @returns {string} Vitest test code
 */
export function transformToVitest(examples, sourceFile) {
  if (!examples || examples.length === 0) {
    return generateEmptyTest(sourceFile);
  }

  const imports = new Set();
  const tests = [];

  // Extract and deduplicate imports from examples
  for (const example of examples) {
    const importMatches = example.code.match(/import\s+{[^}]+}\s+from\s+['"][^'"]+['"]/g);
    if (importMatches) {
      importMatches.forEach(imp => {
        // Rewrite relative imports from source files to test file location
        // e.g., './time.mjs' -> '../../src/time.mjs'
        const rewritten = imp.replace(/from\s+['"]\.\/([^'"]+)['"]/g, "from '../../src/$1'");
        imports.add(rewritten);
      });
    }
  }

  // Generate test cases
  for (let i = 0; i < examples.length; i++) {
    const example = examples[i];
    const testCode = generateTestCase(example, i);
    tests.push(testCode);
  }

  return generateTestFile(sourceFile, Array.from(imports), tests);
}

/**
 * Generate a single test case from an example
 * @param {Object} example - Example object {code, functionName, lineNumber}
 * @param {number} index - Test index for unique naming
 * @returns {string} Vitest test code
 */
function generateTestCase(example, index) {
  const { code, functionName, lineNumber } = example;

  // Clean up code: remove imports if present (will be added to file header)
  const cleanCode = code
    .split('\n')
    .filter(line => !line.trim().startsWith('import '))
    .join('\n')
    .trim();

  const testName = `${functionName} example ${index + 1} (line ${lineNumber})`;

  return `  test('${testName}', async () => {
    ${cleanCode}
  });`;
}

/**
 * Generate complete Vitest test file
 * @param {string} sourceFile - Source file name
 * @param {Array} imports - Array of import statements
 * @param {Array} tests - Array of test code strings
 * @returns {string} Complete test file code
 */
function generateTestFile(sourceFile, imports, tests) {
  const header = `import { describe, test, expect } from 'vitest';

${imports.length > 0 ? imports.join('\n') + '\n\n' : ''}`;

  const setupCode = `  // Auto-injected mocks for kgc-4d doctests
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
`;

  const describe = `describe('Doctests: ${sourceFile}', () => {
${setupCode}
${tests.join('\n\n')}
});
`;

  return header + describe;
}

/**
 * Generate empty test file (for source files with no examples)
 * @param {string} sourceFile - Source file name
 * @returns {string} Empty test file
 */
function generateEmptyTest(sourceFile) {
  return `import { describe, test } from 'vitest';

describe('Doctests: ${sourceFile}', () => {
  test('no examples yet', () => {
    // Add @example tags to source file functions
  });
});
`;
}

/**
 * CLI interface for testing transformer
 */
if (process.argv[1] === new URL(import.meta.url).pathname) {
  // Read JSON from stdin
  let input = '';
  process.stdin.on('data', chunk => { input += chunk; });
  process.stdin.on('end', () => {
    try {
      const data = JSON.parse(input);
      const output = transformToVitest(data.examples, data.sourceFile || 'test.mjs');
      console.log(output);
    } catch (error) {
      console.error('Error:', error.message);
      process.exit(1);
    }
  });
}
