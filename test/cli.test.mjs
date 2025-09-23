#!/usr/bin/env node

/**
 * UNRDF CLI Tests using citty-test-utils
 * 
 * Comprehensive test suite for the UNRDF CLI using the citty-test-utils framework
 * for robust testing of command-line interfaces.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { 
  runLocalCitty, 
  setupCleanroom, 
  runCitty, 
  teardownCleanroom,
  scenario,
  scenarios,
  testUtils
} from 'citty-test-utils';
import { mkdir, writeFile, access, rm } from 'node:fs/promises';
import { join, resolve } from 'node:path';

const testDataDir = resolve(process.cwd(), 'test-cli-data');

describe('UNRDF CLI', () => {
  beforeAll(async () => {
    await setupTestData();
  });

  afterAll(async () => {
    await cleanupTestData();
  });

  describe('Basic CLI Operations', () => {
    it('should show help information', async () => {
      const result = await runLocalCitty(['--help'], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('UNRDF - Opinionated composable framework')
        .expectOutput('Commands:')
        .expectOutput('parse')
        .expectOutput('query')
        .expectOutput('validate')
        .expectOutput('convert')
        .expectOutput('init')
        .expectOutput('id')
        .expectOutput('store')
        .expectOutput('prefix')
        .expectOutput('metrics')
        .expectOutput('delta');
    });

    it('should show version information', async () => {
      const result = await runLocalCitty(['--version'], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('1.0.0');
    });

    it('should handle invalid commands gracefully', async () => {
      const result = await runLocalCitty(['invalid-command'], {
        cwd: process.cwd()
      });
      
      result
        .expectFailure()
        .expectStderr(/Unknown command/);
    });
  });

  describe('Parse Command', () => {
    it('should parse Turtle data successfully', async () => {
      const result = await runLocalCitty([
        'parse', 
        join(testDataDir, 'sample.ttl')
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Parsed 6 triples successfully')
        .expectOutput('Parse Summary:')
        .expectOutput('Triples: 6')
        .expectOutput('Subjects: 2')
        .expectOutput('Predicates: 3');
    });

    it('should parse with detailed statistics', async () => {
      const result = await runLocalCitty([
        'parse', 
        join(testDataDir, 'sample.ttl'),
        '--stats'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Parsed 6 triples successfully')
        .expectOutput('Parse Summary:')
        .expectOutput('Triples: 6')
        .expectOutput('Subjects: 2')
        .expectOutput('Predicates: 3')
        .expectOutput('Objects: 6')
        .expectOutput('Literals: 4')
        .expectOutput('IRIs: 2')
        .expectOutput('Blank nodes: 0');
    });

    it('should output to file when specified', async () => {
      const outputFile = join(testDataDir, 'parsed-output.ttl');
      
      const result = await runLocalCitty([
        'parse', 
        join(testDataDir, 'sample.ttl'),
        '--output', outputFile
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Output written to');
      
      // Verify file was created
      await expect(access(outputFile)).resolves.not.toThrow();
    });

    it('should handle different input formats', async () => {
      // Test JSON-LD parsing
      const result = await runLocalCitty([
        'parse', 
        join(testDataDir, 'sample.jsonld'),
        '--format', 'json-ld'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Parsed');
    });

    it('should handle invalid input gracefully', async () => {
      const result = await runLocalCitty([
        'parse', 
        'nonexistent.ttl'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectFailure()
        .expectStderr(/File not found/);
    });
  });

  describe('Query Command', () => {
    it('should execute SPARQL queries successfully', async () => {
      const result = await runLocalCitty([
        'query',
        join(testDataDir, 'sample.ttl'),
        '--query', 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Executing SPARQL query');
    });

    it('should support JSON output format', async () => {
      const result = await runLocalCitty([
        'query',
        join(testDataDir, 'sample.ttl'),
        '--query', 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name } LIMIT 1',
        '--format', 'json'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Executing SPARQL query');
    });

    it('should support CSV output format', async () => {
      const result = await runLocalCitty([
        'query',
        join(testDataDir, 'sample.ttl'),
        '--query', 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name } LIMIT 1',
        '--format', 'csv'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Executing SPARQL query');
    });

    it('should support query files', async () => {
      const result = await runLocalCitty([
        'query',
        join(testDataDir, 'sample.ttl'),
        '--query-file', join(testDataDir, 'query.sparql')
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Executing SPARQL query');
    });
  });

  describe('Validate Command', () => {
    it('should validate RDF data against SHACL shapes', async () => {
      const result = await runLocalCitty([
        'validate',
        join(testDataDir, 'sample.ttl'),
        '--shape', join(testDataDir, 'person-shape.ttl')
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Validating RDF data');
    });

    it('should output validation report to file', async () => {
      const outputFile = join(testDataDir, 'validation-report.json');
      
      const result = await runLocalCitty([
        'validate',
        join(testDataDir, 'sample.ttl'),
        '--shape', join(testDataDir, 'person-shape.ttl'),
        '--output', outputFile
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Validating RDF data');
      
      // Verify file was created
      await expect(access(outputFile)).resolves.not.toThrow();
    });
  });

  describe('Convert Command', () => {
    it('should convert between RDF formats', async () => {
      const result = await runLocalCitty([
        'convert',
        join(testDataDir, 'sample.ttl'),
        '--from', 'turtle',
        '--to', 'json-ld'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Converting data');
    });

    it('should save converted data to file', async () => {
      const outputFile = join(testDataDir, 'converted.jsonld');
      
      const result = await runLocalCitty([
        'convert',
        join(testDataDir, 'sample.ttl'),
        '--from', 'turtle',
        '--to', 'json-ld',
        '--output', outputFile
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Converting data')
        .expectOutput('Converted data written to');
      
      // Verify file was created
      await expect(access(outputFile)).resolves.not.toThrow();
    });
  });

  describe('Store Command', () => {
    it('should show store statistics', async () => {
      const result = await runLocalCitty([
        'store', 'stats',
        join(testDataDir, 'sample.ttl')
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Store Statistics:')
        .expectOutput('Total triples: 6')
        .expectOutput('Unique subjects: 2')
        .expectOutput('Unique predicates: 3')
        .expectOutput('Unique objects: 6');
    });

    it('should clear store', async () => {
      const result = await runLocalCitty([
        'store', 'clear'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Store cleared');
    });
  });

  describe('Prefix Command', () => {
    it('should list known prefixes', async () => {
      const result = await runLocalCitty([
        'prefix', 'list'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Known prefixes:')
        .expectOutput('ex: http://example.org/')
        .expectOutput('foaf: http://xmlns.com/foaf/0.1/')
        .expectOutput('schema: https://schema.org/');
    });

    it('should expand CURIEs', async () => {
      const result = await runLocalCitty([
        'prefix', 'expand', 'foaf:Person'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Expanded:')
        .expectOutput('http://xmlns.com/foaf/0.1/Person');
    });

    it('should shrink IRIs', async () => {
      const result = await runLocalCitty([
        'prefix', 'shrink', 'http://xmlns.com/foaf/0.1/Person'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Shrunk:')
        .expectOutput('foaf:Person');
    });
  });

  describe('ID Command', () => {
    it('should generate UUIDs', async () => {
      const result = await runLocalCitty([
        'id', 'uuid'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/);
    });

    it('should generate multiple UUIDs', async () => {
      const result = await runLocalCitty([
        'id', 'uuid', '--count', '3'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess();
      
      const lines = result.result.stdout.trim().split('\n');
      expect(lines).toHaveLength(3);
      for (const line of lines) {
        expect(line).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/);
      }
    });

    it('should generate hash-based IDs', async () => {
      const result = await runLocalCitty([
        'id', 'hash', 'test input'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput(/test/);
    });

    it('should generate generic IDs', async () => {
      const result = await runLocalCitty([
        'id', 'generate', '--prefix', 'test'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput(/test/);
    });
  });

  describe('Metrics Command', () => {
    it('should analyze RDF metrics', async () => {
      const result = await runLocalCitty([
        'metrics',
        join(testDataDir, 'sample.ttl')
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('RDF Metrics:')
        .expectOutput('Total triples: 6')
        .expectOutput('Unique subjects: 2')
        .expectOutput('Unique predicates: 3')
        .expectOutput('Unique objects: 6')
        .expectOutput('Literals: 4')
        .expectOutput('IRIs: 2')
        .expectOutput('Blank nodes: 0');
    });

    it('should show detailed metrics', async () => {
      const result = await runLocalCitty([
        'metrics',
        join(testDataDir, 'sample.ttl'),
        '--detailed'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('RDF Metrics:')
        .expectOutput('Detailed Analysis:');
    });
  });

  describe('Delta Command', () => {
    it('should compare RDF datasets', async () => {
      const result = await runLocalCitty([
        'delta',
        join(testDataDir, 'sample.ttl'),
        join(testDataDir, 'sample-modified.ttl')
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Dataset Comparison:')
        .expectOutput('Added triples:')
        .expectOutput('Removed triples:')
        .expectOutput('Unchanged triples:');
    });

    it('should save delta report to file', async () => {
      const outputFile = join(testDataDir, 'delta-report.json');
      
      const result = await runLocalCitty([
        'delta',
        join(testDataDir, 'sample.ttl'),
        join(testDataDir, 'sample-modified.ttl'),
        '--output', outputFile
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectSuccess()
        .expectOutput('Dataset Comparison:')
        .expectOutput('Delta report written to');
      
      // Verify file was created
      await expect(access(outputFile)).resolves.not.toThrow();
    });
  });

  describe('Init Command', () => {
    it('should initialize a new project', async () => {
      const projectDir = join(testDataDir, 'test-project');
      
      const result = await runLocalCitty([
        'init', 'test-project'
      ], {
        cwd: testDataDir
      });
      
      result
        .expectSuccess()
        .expectOutput('Initializing UNRDF project: test-project')
        .expectOutput('Project initialized in')
        .expectOutput('Created files:')
        .expectOutput('package.json')
        .expectOutput('unrdf.config.mjs')
        .expectOutput('data.ttl');
      
      // Verify files were created
      await expect(access(join(projectDir, 'package.json'))).resolves.not.toThrow();
      await expect(access(join(projectDir, 'unrdf.config.mjs'))).resolves.not.toThrow();
      await expect(access(join(projectDir, 'data.ttl'))).resolves.not.toThrow();
    });
  });

  describe('Configuration', () => {
    it('should load configuration from unrdf.config.mjs', async () => {
      const result = await runLocalCitty([
        'prefix', 'list'
      ], {
        cwd: join(testDataDir, 'test-project')
      });
      
      result
        .expectSuccess()
        .expectOutput('Known prefixes:');
    });

    it('should use environment variables', async () => {
      const result = await runLocalCitty([
        'prefix', 'list'
      ], {
        cwd: process.cwd(),
        env: {
          UNRDF_BASE_IRI: 'http://env.example.org/',
          UNRDF_PREFIXES: JSON.stringify({
            'env': 'http://env.example.org/',
            'test': 'http://test.example.org/'
          })
        }
      });
      
      result
        .expectSuccess()
        .expectOutput('Known prefixes:');
    });
  });

  describe('Error Handling', () => {
    it('should handle missing required arguments', async () => {
      const result = await runLocalCitty([
        'validate'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectFailure();
    });

    it('should handle file not found errors', async () => {
      const result = await runLocalCitty([
        'parse', 'nonexistent.ttl'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectFailure()
        .expectStderr(/File not found/);
    });

    it('should provide helpful error messages', async () => {
      const result = await runLocalCitty([
        'convert', 'test.ttl', '--to', 'invalid-format'
      ], {
        cwd: process.cwd()
      });
      
      result
        .expectFailure()
        .expectStderr(/Unsupported target format/);
    });
  });

  describe('Performance', () => {
    it('should complete operations within reasonable time', async () => {
      const startTime = Date.now();
      
      const result = await runLocalCitty([
        'parse',
        join(testDataDir, 'sample.ttl'),
        '--stats'
      ], {
        cwd: process.cwd()
      });
      
      const endTime = Date.now();
      const duration = endTime - startTime;
      
      result.expectSuccess();
      expect(duration).toBeLessThan(5000); // Should complete within 5 seconds
    });
  });

  describe('Scenario Testing', () => {
    it('should handle complete workflow', async () => {
      const result = await scenario('Complete workflow')
        .step('Parse data')
        .run('parse', join(testDataDir, 'sample.ttl'))
        .expectSuccess()
        .expectOutput('Parsed 6 triples successfully')
        .step('Query data')
        .run('query', join(testDataDir, 'sample.ttl'), '--query', 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }')
        .expectSuccess()
        .expectOutput('Executing SPARQL query')
        .step('Generate ID')
        .run('id', 'uuid')
        .expectSuccess()
        .expectOutput(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/)
        .execute('local', { cwd: process.cwd() });
      
      expect(result.success).toBe(true);
    });

    it('should use pre-built scenarios', async () => {
      const helpResult = await scenarios.help('local', { cwd: process.cwd() }).execute();
      const versionResult = await scenarios.version('local', { cwd: process.cwd() }).execute();
      
      expect(helpResult.success).toBe(true);
      expect(versionResult.success).toBe(true);
    });
  });

  describe('Cleanroom Testing', () => {
    beforeAll(async () => {
      await setupCleanroom({ rootDir: process.cwd() });
    });

    afterAll(async () => {
      await teardownCleanroom();
    });

    it('should work in cleanroom environment', async () => {
      const result = await runCitty(['--help'], {
        env: { DEBUG: 'true' }
      });
      
      result
        .expectSuccess()
        .expectOutput('UNRDF - Opinionated composable framework')
        .expectOutput('Commands:');
    });

    it('should handle complex workflow in cleanroom', async () => {
      const result = await scenario('Cleanroom workflow')
        .step('Get help')
        .run('--help')
        .expectSuccess()
        .expectOutput('UNRDF - Opinionated composable framework')
        .step('Get version')
        .run('--version')
        .expectSuccess()
        .expectOutput('1.0.0')
        .step('Test invalid command')
        .run('invalid-command')
        .expectFailure()
        .expectStderr(/Unknown command/)
        .execute('cleanroom');
      
      expect(result.success).toBe(true);
    });
  });

  describe('Retry Testing', () => {
    it('should handle flaky operations with retry', async () => {
      await testUtils.retry(async () => {
        const result = await runLocalCitty(['--help'], {
          cwd: process.cwd()
        });
        result.expectSuccess();
      }, 3, 1000);
    });
  });
});

/**
 * Setup test data files
 */
async function setupTestData() {
  await mkdir(testDataDir, { recursive: true });
  
  // Sample Turtle data
  const sampleTurtle = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 ;
  foaf:knows ex:person2 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 ;
  foaf:knows ex:person1 .`;
  
  await writeFile(join(testDataDir, 'sample.ttl'), sampleTurtle);
  
  // Modified sample data for delta testing
  const modifiedTurtle = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 31 ;
  foaf:knows ex:person2 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 ;
  foaf:knows ex:person1 .

ex:person3 a foaf:Person ;
  foaf:name "Charlie" ;
  foaf:age 35 .`;
  
  await writeFile(join(testDataDir, 'sample-modified.ttl'), modifiedTurtle);
  
  // Sample JSON-LD data
  const sampleJsonLd = {
    "@context": {
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    },
    "@graph": [
      {
        "@id": "ex:person1",
        "@type": "foaf:Person",
        "foaf:name": "Alice",
        "foaf:age": 30
      },
      {
        "@id": "ex:person2",
        "@type": "foaf:Person",
        "foaf:name": "Bob",
        "foaf:age": 25
      }
    ]
  };
  
  await writeFile(join(testDataDir, 'sample.jsonld'), JSON.stringify(sampleJsonLd, null, 2));
  
  // Sample SPARQL query
  const sampleQuery = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name WHERE {
  ?person a foaf:Person .
  ?person foaf:name ?name .
}`;
  
  await writeFile(join(testDataDir, 'query.sparql'), sampleQuery);
  
  // Sample SHACL shape
  const sampleShape = `@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1
  ] ;
  sh:property [
    sh:path foaf:age ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150
  ] .`;
  
  await writeFile(join(testDataDir, 'person-shape.ttl'), sampleShape);
}

/**
 * Cleanup test data files
 */
async function cleanupTestData() {
  try {
    await rm(testDataDir, { recursive: true, force: true });
  } catch (error) {
    // Ignore cleanup errors
  }
}