/**
 * @fileoverview Test utilities for CLI v2 testing
 * Implements citty-test-utils patterns for comprehensive CLI testing
 */

import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { mkdtemp, writeFile, readFile, rm, mkdir } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { spawn } from 'node:child_process';
import { Store } from 'n3';

const __dirname = dirname(fileURLToPath(import.meta.url));
const CLI_V2_PATH = join(__dirname, '../../src/cli-v2/index.mjs');

/**
 * Execute CLI v2 command
 * @param {string} command - Full command string (e.g., "hook eval test.json")
 * @param {Object} options - Execution options
 * @returns {Promise<{stdout: string, stderr: string, exitCode: number, duration: number}>}
 */
export async function runCLI(command, options = {}) {
  const args = command.split(' ');
  const startTime = Date.now();

  return new Promise((resolve, reject) => {
    const child = spawn('node', [CLI_V2_PATH, ...args], {
      cwd: options.cwd || process.cwd(),
      env: { ...process.env, ...options.env, NODE_ENV: 'test' },
      stdio: 'pipe'
    });

    let stdout = '';
    let stderr = '';

    child.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    child.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    child.on('close', (exitCode) => {
      const duration = Date.now() - startTime;
      resolve({ stdout, stderr, exitCode, duration });
    });

    child.on('error', (error) => {
      reject(error);
    });

    // Timeout handling
    if (options.timeout) {
      setTimeout(() => {
        child.kill();
        reject(new Error(`Command timeout after ${options.timeout}ms: ${command}`));
      }, options.timeout);
    }
  });
}

/**
 * Create test context with mocked dependencies
 * @param {Object} overrides - Context overrides
 * @returns {Object} Test context
 */
export function createTestContext(overrides = {}) {
  return {
    store: overrides.store || new Store(),
    baseIRI: overrides.baseIRI || 'http://test.example.org/',
    prefixes: overrides.prefixes || {
      'ex': 'http://test.example.org/',
      'foaf': 'http://xmlns.com/foaf/0.1/',
      'schema': 'https://schema.org/'
    },
    verbose: overrides.verbose || false,
    quiet: overrides.quiet || true,
    format: overrides.format || 'json',
    ...overrides
  };
}

/**
 * Create temporary test directory
 * @param {string} prefix - Directory prefix
 * @returns {Promise<{dir: string, cleanup: Function}>}
 */
export async function createTempDir(prefix = 'cli-v2-test-') {
  const dir = await mkdtemp(join(tmpdir(), prefix));

  return {
    dir,
    cleanup: async () => {
      try {
        await rm(dir, { recursive: true, force: true });
      } catch (err) {
        // Ignore cleanup errors
      }
    }
  };
}

/**
 * Create test project structure
 * @param {string} baseDir - Base directory
 * @returns {Promise<Object>} Project paths
 */
export async function createTestProject(baseDir) {
  const paths = {
    hooks: join(baseDir, 'hooks'),
    data: join(baseDir, 'data'),
    policies: join(baseDir, 'policies'),
    config: join(baseDir, 'unrdf.config.mjs'),
    receipts: join(baseDir, '.unrdf', 'receipts'),
    baselines: join(baseDir, '.unrdf', 'baselines')
  };

  // Create directories
  await mkdir(paths.hooks, { recursive: true });
  await mkdir(paths.data, { recursive: true });
  await mkdir(paths.policies, { recursive: true });
  await mkdir(paths.receipts, { recursive: true });
  await mkdir(paths.baselines, { recursive: true });

  // Create config
  const config = `export default {
  baseIRI: 'http://test.example.org/',
  prefixes: {
    'ex': 'http://test.example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'schema': 'https://schema.org/'
  },
  validation: {
    strict: true,
    validateOnLoad: true
  }
};`;

  await writeFile(paths.config, config);

  return paths;
}

/**
 * Assertion helpers
 */
export const assert = {
  /**
   * Assert command succeeded
   */
  success(result) {
    if (result.exitCode !== 0) {
      throw new Error(
        `Expected success but got exit code ${result.exitCode}\n` +
        `Stderr: ${result.stderr}\n` +
        `Stdout: ${result.stdout}`
      );
    }
  },

  /**
   * Assert command failed
   */
  failure(result) {
    if (result.exitCode === 0) {
      throw new Error(
        `Expected failure but command succeeded\n` +
        `Stdout: ${result.stdout}`
      );
    }
  },

  /**
   * Assert output contains text
   */
  outputContains(result, text) {
    const output = result.stdout + result.stderr;
    if (!output.includes(text)) {
      throw new Error(
        `Expected output to contain "${text}"\n` +
        `Got: ${output}`
      );
    }
  },

  /**
   * Assert output matches regex
   */
  outputMatches(result, regex) {
    const output = result.stdout + result.stderr;
    if (!regex.test(output)) {
      throw new Error(
        `Expected output to match ${regex}\n` +
        `Got: ${output}`
      );
    }
  },

  /**
   * Assert performance target met
   */
  performanceTarget(result, targetMs, operation) {
    if (result.duration > targetMs) {
      throw new Error(
        `Performance target missed for ${operation}\n` +
        `Expected: < ${targetMs}ms\n` +
        `Got: ${result.duration}ms`
      );
    }
  },

  /**
   * Assert JSON output
   */
  jsonOutput(result, validator) {
    try {
      const data = JSON.parse(result.stdout);
      if (validator) {
        validator(data);
      }
      return data;
    } catch (err) {
      throw new Error(
        `Expected valid JSON output\n` +
        `Error: ${err.message}\n` +
        `Output: ${result.stdout}`
      );
    }
  }
};

/**
 * Test data generators
 */
export const generators = {
  /**
   * Generate RDF triples
   */
  rdfTriples(count, prefix = 'ex:entity') {
    let turtle = `@prefix ex: <http://test.example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix schema: <https://schema.org/> .

`;
    for (let i = 0; i < count; i++) {
      turtle += `${prefix}${i} a foaf:Person ;
  foaf:name "Person ${i}" ;
  foaf:age ${20 + (i % 50)} ;
  schema:email "person${i}@test.example.org" .

`;
    }
    return turtle;
  },

  /**
   * Generate SPARQL query
   */
  sparqlQuery(type = 'select', filter = '') {
    const queries = {
      select: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?person ?name ?age WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age .
  ${filter}
}`,
      ask: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
ASK { ?s a foaf:Person }`,
      construct: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
CONSTRUCT { ?person foaf:name ?name }
WHERE { ?person a foaf:Person ; foaf:name ?name }`,
      describe: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
DESCRIBE ?person
WHERE { ?person a foaf:Person }`
    };
    return queries[type] || queries.select;
  },

  /**
   * Generate hook definition
   */
  hookDefinition(type = 'sparql-ask', name = 'test-hook') {
    const hooks = {
      'sparql-ask': {
        meta: {
          name,
          description: `Test ${type} hook`,
          version: '1.0.0'
        },
        when: {
          kind: 'sparql-ask',
          ref: {
            inline: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
ASK { ?s a foaf:Person }`
          }
        },
        run: {
          action: 'log',
          message: 'Hook fired'
        }
      },
      'shacl': {
        meta: {
          name,
          description: `Test ${type} hook`,
          version: '1.0.0'
        },
        when: {
          kind: 'shacl',
          ref: {
            inline: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string
  ] .`
          }
        },
        run: {
          action: 'log',
          message: 'Validation hook fired'
        }
      },
      'delta': {
        meta: {
          name,
          description: `Test ${type} hook`,
          version: '1.0.0'
        },
        when: {
          kind: 'delta',
          threshold: 0.1
        },
        run: {
          action: 'log',
          message: 'Delta detected'
        }
      }
    };
    return hooks[type] || hooks['sparql-ask'];
  },

  /**
   * Generate SHACL shapes
   */
  shaclShapes(targetClass = 'foaf:Person') {
    return `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://test.example.org/> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ${targetClass} ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string
  ] ;
  sh:property [
    sh:path foaf:age ;
    sh:minCount 1 ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150
  ] .`;
  }
};

/**
 * Mock sidecar server for testing
 */
export class MockSidecar {
  constructor(port = 8080) {
    this.port = port;
    this.running = false;
    this.requests = [];
  }

  async start() {
    // Mock implementation - in real tests, start actual server
    this.running = true;
    return this;
  }

  async stop() {
    this.running = false;
    this.requests = [];
  }

  getRequests() {
    return this.requests;
  }

  reset() {
    this.requests = [];
  }
}

/**
 * Performance measurement utilities
 */
export const perf = {
  /**
   * Measure operation performance
   */
  async measure(fn, iterations = 1) {
    const durations = [];

    for (let i = 0; i < iterations; i++) {
      const start = Date.now();
      await fn();
      durations.push(Date.now() - start);
    }

    return {
      mean: durations.reduce((a, b) => a + b, 0) / durations.length,
      min: Math.min(...durations),
      max: Math.max(...durations),
      p50: durations.sort((a, b) => a - b)[Math.floor(durations.length * 0.5)],
      p95: durations.sort((a, b) => a - b)[Math.floor(durations.length * 0.95)],
      p99: durations.sort((a, b) => a - b)[Math.floor(durations.length * 0.99)]
    };
  },

  /**
   * Assert performance SLA
   */
  assertSLA(metrics, sla) {
    const violations = [];

    if (sla.mean && metrics.mean > sla.mean) {
      violations.push(`Mean ${metrics.mean}ms > ${sla.mean}ms`);
    }
    if (sla.p50 && metrics.p50 > sla.p50) {
      violations.push(`P50 ${metrics.p50}ms > ${sla.p50}ms`);
    }
    if (sla.p95 && metrics.p95 > sla.p95) {
      violations.push(`P95 ${metrics.p95}ms > ${sla.p95}ms`);
    }
    if (sla.p99 && metrics.p99 > sla.p99) {
      violations.push(`P99 ${metrics.p99}ms > ${sla.p99}ms`);
    }

    if (violations.length > 0) {
      throw new Error(`Performance SLA violations:\n${violations.join('\n')}`);
    }
  }
};

/**
 * Test scenario builder
 */
export class TestScenario {
  constructor(name) {
    this.name = name;
    this.steps = [];
    this.context = {};
  }

  step(name, fn) {
    this.steps.push({ name, fn });
    return this;
  }

  async run() {
    console.log(`\n🧪 Running scenario: ${this.name}`);

    for (const step of this.steps) {
      console.log(`  → ${step.name}`);
      await step.fn(this.context);
    }

    console.log(`✅ Scenario completed: ${this.name}\n`);
    return this.context;
  }
}

/**
 * Create test scenario
 */
export function scenario(name) {
  return new TestScenario(name);
}
