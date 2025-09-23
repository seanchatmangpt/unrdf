/**
 * CLI Tests using citty-test-utils
 * 
 * Comprehensive test suite for the UNRDF CLI using citty-test-utils
 */

import { describe, it, beforeAll, afterAll, expect } from 'vitest';
import { 
  runLocalCitty, 
  setupCleanroom, 
  runCitty, 
  teardownCleanroom,
  scenario,
  scenarios,
  testUtils
} from 'citty-test-utils';
import { readFile, writeFile, mkdir } from 'fs/promises';

describe('UNRDF CLI Tests', () => {
  beforeAll(async () => {
    await setupTestData();
  });

  afterAll(async () => {
    await cleanupTestData();
  });

  describe('Basic CLI Operations', () => {
    it('should show help information', async () => {
      const result = await runLocalCitty(['--help'], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput('UNRDF Command Line Interface')
        .expectOutput('Available commands:')
        .expectNoStderr();
    });

    it('should show version information', async () => {
      const result = await runLocalCitty(['--version'], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput(/\d+\.\d+\.\d+/)
        .expectNoStderr();
    });

    it('should handle invalid commands gracefully', async () => {
      const result = await runLocalCitty(['invalid-command'], {
        cwd: './playground',
        env: { DEBUG: 'true' }
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
        './test-data/sample.ttl',
        '--format', 'turtle'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput('Parsed 4 triples successfully')
        .expectOutput('Triples: 4')
        .expectNoStderr();
    });

    it('should handle invalid input gracefully', async () => {
      const result = await runLocalCitty([
        'parse',
        './nonexistent.ttl'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectFailure()
        .expectStderr(/Parse error/);
    });

    it('should output to file when specified', async () => {
      const result = await runLocalCitty([
        'parse',
        './test-data/sample.ttl',
        '--output', './test-data/output.ttl'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput('Output written to ./test-data/output.ttl');

      // Verify file was created
      const outputContent = await readFile('./test-data/output.ttl', 'utf-8');
      expect(outputContent).toContain('John Doe');
    });
  });

  describe('Query Command', () => {
    it('should execute SPARQL queries successfully', async () => {
      const result = await runLocalCitty([
        'query',
        './test-data/sample.ttl',
        '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }',
        '--format', 'table'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput('John Doe')
        .expectOutput('Jane Smith')
        .expectNoStderr();
    });

    it('should support JSON output format', async () => {
      const result = await runLocalCitty([
        'query',
        './test-data/sample.ttl',
        '--query', 'SELECT ?name WHERE { ?person foaf:name ?name } LIMIT 1',
        '--format', 'json'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput(/\[.*"name".*\]/);
    });

    it('should handle invalid SPARQL queries', async () => {
      const result = await runLocalCitty([
        'query',
        './test-data/sample.ttl',
        '--query', 'INVALID SPARQL QUERY'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectFailure()
        .expectStderr(/Query error/);
    });
  });

  describe('Validation Command', () => {
    it('should validate RDF data against SHACL shapes', async () => {
      const result = await runLocalCitty([
        'validate',
        './test-data/sample.ttl',
        './test-data/person-shape.ttl'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput('Conforms: true')
        .expectOutput('Violations: 0')
        .expectNoStderr();
    });

    it('should handle missing shape file', async () => {
      const result = await runLocalCitty([
        'validate',
        './test-data/sample.ttl',
        './nonexistent-shape.ttl'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectFailure()
        .expectStderr(/Validation error/);
    });
  });

  describe('Scenario Testing', () => {
    it('should execute basic workflow scenario', async () => {
      const result = await scenario('Basic Workflow')
        .step('Parse data')
        .run('parse', './test-data/sample.ttl')
        .expectSuccess()
        .expectOutput('Parsed 4 triples successfully')
        .step('Query data')
        .run('query', './test-data/sample.ttl', '--query', 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }')
        .expectSuccess()
        .expectOutput('4')
        .execute('local', { cwd: './playground' });

      expect(result.success).toBe(true);
    });

    it('should use pre-built scenarios', async () => {
      const helpResult = await scenarios.help('local').execute({ cwd: './playground' });
      const versionResult = await scenarios.version('local').execute({ cwd: './playground' });

      expect(helpResult.success).toBe(true);
      expect(versionResult.success).toBe(true);
    });
  });

  describe('Environment Testing', () => {
    it('should work in cleanroom environment', async () => {
      await setupCleanroom({ 
        rootDir: './playground',
        nodeImage: 'node:18-alpine'
      });

      const result = await runCitty(['--help'], {
        env: { DEBUG: 'true' }
      });

      result
        .expectSuccess()
        .expectOutput('UNRDF Command Line Interface')
        .expectNoStderr();

      await teardownCleanroom();
    });
  });

  describe('Error Handling', () => {
    it('should handle missing required arguments', async () => {
      const result = await runLocalCitty([
        'validate',
        './test-data/sample.ttl'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectFailure()
        .expectStderr(/Validation error/);
    });

    it('should handle file not found errors', async () => {
      const result = await runLocalCitty([
        'parse',
        './nonexistent.ttl'
      ], {
        cwd: './playground',
        env: { DEBUG: 'true' }
      });

      result
        .expectFailure()
        .expectStderr(/Parse error/);
    });
  });

  describe('Performance Testing', () => {
    it('should complete operations within reasonable time', async () => {
      const startTime = Date.now();
      
      const result = await runLocalCitty([
        'parse',
        './test-data/sample.ttl'
      ], {
        cwd: './playground',
        env: { DEBUG: 'false' }
      });

      const endTime = Date.now();
      const duration = endTime - startTime;

      result.expectSuccess();
      expect(duration).toBeLessThan(5000); // Should complete within 5 seconds
    });
  });

  describe('Retry and Resilience', () => {
    it('should handle flaky operations with retry', async () => {
      await testUtils.retry(async () => {
        const result = await runLocalCitty(['--help'], {
          cwd: './playground',
          env: { DEBUG: 'true' }
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
  await mkdir('./test-data', { recursive: true });
  
  // Sample Turtle data
  const sampleTurtle = `
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix ex: <http://example.org/> .
    
    ex:john a foaf:Person ;
      foaf:name "John Doe" ;
      foaf:age 30 .
    
    ex:jane a foaf:Person ;
      foaf:name "Jane Smith" ;
      foaf:age 28 .
  `;
  
  await writeFile('./test-data/sample.ttl', sampleTurtle);
  
  // SHACL shape for validation
  const personShape = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix ex: <http://example.org/> .
    
    ex:PersonShape a sh:NodeShape ;
      sh:targetClass foaf:Person ;
      sh:property [
        sh:path foaf:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
      ] ;
      sh:property [
        sh:path foaf:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
      ] .
  `;
  
  await writeFile('./test-data/person-shape.ttl', personShape);
}

/**
 * Cleanup test data files
 */
async function cleanupTestData() {
  try {
    const { rm } = await import('fs/promises');
    await rm('./test-data', { recursive: true, force: true });
  } catch (error) {
    console.log('⚠️  Could not clean up test data:', error.message);
  }
}
