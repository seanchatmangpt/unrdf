/**
 * @fileoverview CLI Test Helpers - Utilities for testing UNRDF CLI commands
 * Follows citty-test-utils patterns for command testing
 */

import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { mkdtemp, writeFile, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { spawn } from 'node:child_process';

const __dirname = dirname(fileURLToPath(import.meta.url));
const CLI_PATH = join(__dirname, '../../src/cli.mjs');

/**
 * Execute CLI command with arguments
 * @param {string[]} args - Command arguments
 * @param {Object} options - Execution options
 * @returns {Promise<{stdout: string, stderr: string, exitCode: number}>}
 */
export async function execCLI(args, options = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn('node', [CLI_PATH, ...args], {
      cwd: options.cwd || process.cwd(),
      env: { ...process.env, ...options.env },
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
      resolve({ stdout, stderr, exitCode });
    });

    child.on('error', (error) => {
      reject(error);
    });

    // Handle timeout
    if (options.timeout) {
      setTimeout(() => {
        child.kill();
        reject(new Error('Command timeout'));
      }, options.timeout);
    }
  });
}

/**
 * Create a temporary test directory with cleanup
 * @returns {Promise<{dir: string, cleanup: Function}>}
 */
export async function createTempDir() {
  const dir = await mkdtemp(join(tmpdir(), 'unrdf-test-'));

  return {
    dir,
    cleanup: async () => {
      try {
        await rm(dir, { recursive: true, force: true });
      } catch (err) {
        // Ignore cleanup errors in tests
      }
    }
  };
}

/**
 * Create a temporary RDF file with sample data
 * @param {string} content - Turtle content
 * @param {string} filename - Optional filename
 * @returns {Promise<{path: string, cleanup: Function}>}
 */
export async function createTempRDFFile(content, filename = 'test.ttl') {
  const { dir, cleanup: cleanupDir } = await createTempDir();
  const path = join(dir, filename);
  await writeFile(path, content);

  return {
    path,
    cleanup: cleanupDir
  };
}

/**
 * Sample RDF data templates
 */
export const RDF_SAMPLES = {
  simple: `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 .`,

  extended: `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix schema: <https://schema.org/> .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 ;
  schema:email "alice@example.org" ;
  foaf:knows ex:person2 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 ;
  schema:email "bob@example.org" .

ex:person3 a foaf:Person ;
  foaf:name "Charlie" ;
  foaf:age 35 ;
  schema:email "charlie@example.org" ;
  foaf:knows ex:person1, ex:person2 .`,

  empty: `@prefix ex: <http://example.org/> .`,

  shaclShape: `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string
  ] ;
  sh:property [
    sh:path foaf:age ;
    sh:minCount 1 ;
    sh:datatype xsd:integer
  ] .`
};

/**
 * Sample SPARQL queries
 */
export const SPARQL_QUERIES = {
  selectAll: `SELECT ?s ?p ?o WHERE { ?s ?p ?o }`,

  selectPeople: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?person ?name WHERE {
  ?person a foaf:Person ;
          foaf:name ?name .
}`,

  selectByAge: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?person ?name ?age WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age .
  FILTER (?age > 25)
}`,

  askPerson: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
ASK { ?s a foaf:Person }`,

  countPeople: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT (COUNT(?person) as ?count) WHERE {
  ?person a foaf:Person .
}`
};

/**
 * Assert CLI output contains expected text
 * @param {string} output - CLI output
 * @param {string} expected - Expected text
 */
export function assertOutputContains(output, expected) {
  if (!output.includes(expected)) {
    throw new Error(`Expected output to contain "${expected}", but got: ${output}`);
  }
}

/**
 * Assert CLI command succeeded
 * @param {Object} result - CLI execution result
 */
export function assertSuccess(result) {
  if (result.exitCode !== 0) {
    throw new Error(`Expected exit code 0, got ${result.exitCode}.\nStderr: ${result.stderr}`);
  }
}

/**
 * Assert CLI command failed
 * @param {Object} result - CLI execution result
 */
export function assertFailure(result) {
  if (result.exitCode === 0) {
    throw new Error('Expected command to fail, but it succeeded');
  }
}

/**
 * Parse JSON output from CLI
 * @param {string} output - CLI output
 * @returns {Object} Parsed JSON
 */
export function parseJSONOutput(output) {
  try {
    return JSON.parse(output);
  } catch (err) {
    throw new Error(`Failed to parse JSON output: ${output}`);
  }
}

/**
 * Create a test project structure
 * @param {string} baseDir - Base directory
 * @returns {Promise<Object>} Project structure paths
 */
export async function createTestProject(baseDir) {
  const paths = {
    data: join(baseDir, 'data.ttl'),
    shape: join(baseDir, 'shape.ttl'),
    query: join(baseDir, 'query.rq'),
    config: join(baseDir, 'unrdf.config.mjs')
  };

  await writeFile(paths.data, RDF_SAMPLES.simple);
  await writeFile(paths.shape, RDF_SAMPLES.shaclShape);
  await writeFile(paths.query, SPARQL_QUERIES.selectPeople);

  const config = `export default {
  baseIRI: 'http://example.org/',
  prefixes: {
    'ex': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'schema': 'https://schema.org/'
  }
};`;

  await writeFile(paths.config, config);

  return paths;
}

/**
 * Measure command execution time
 * @param {Function} fn - Function to measure
 * @returns {Promise<{result: any, duration: number}>}
 */
export async function measureTime(fn) {
  const start = Date.now();
  const result = await fn();
  const duration = Date.now() - start;
  return { result, duration };
}

/**
 * Wait for a condition to be true
 * @param {Function} condition - Condition function
 * @param {number} timeout - Timeout in ms
 * @param {number} interval - Check interval in ms
 */
export async function waitFor(condition, timeout = 5000, interval = 100) {
  const start = Date.now();
  while (Date.now() - start < timeout) {
    if (await condition()) {
      return true;
    }
    await new Promise(resolve => setTimeout(resolve, interval));
  }
  throw new Error('Timeout waiting for condition');
}

/**
 * Create a CLI test context with setup and teardown
 * @returns {Object} Test context
 */
export function createCLITestContext() {
  const cleanupFns = [];

  return {
    /**
     * Add cleanup function
     * @param {Function} fn - Cleanup function
     */
    onCleanup(fn) {
      cleanupFns.push(fn);
    },

    /**
     * Execute all cleanup functions
     */
    async cleanup() {
      for (const fn of cleanupFns.reverse()) {
        try {
          await fn();
        } catch (err) {
          // Ignore cleanup errors
        }
      }
    },

    /**
     * Create temp directory and register cleanup
     */
    async createTempDir() {
      const { dir, cleanup } = await createTempDir();
      this.onCleanup(cleanup);
      return dir;
    },

    /**
     * Create temp RDF file and register cleanup
     */
    async createTempRDFFile(content, filename) {
      const { path, cleanup } = await createTempRDFFile(content, filename);
      this.onCleanup(cleanup);
      return path;
    }
  };
}

/**
 * Mock environment variables for testing
 * @param {Object} vars - Environment variables
 * @returns {Function} Restore function
 */
export function mockEnv(vars) {
  const original = {};

  for (const [key, value] of Object.entries(vars)) {
    original[key] = process.env[key];
    process.env[key] = value;
  }

  return () => {
    for (const [key, value] of Object.entries(original)) {
      if (value === undefined) {
        delete process.env[key];
      } else {
        process.env[key] = value;
      }
    }
  };
}
