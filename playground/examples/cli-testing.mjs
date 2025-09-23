#!/usr/bin/env node

/**
 * CLI Testing Example using citty-test-utils
 * 
 * This example demonstrates how to test the UNRDF CLI using citty-test-utils:
 * - Basic CLI command testing
 * - Output validation
 * - Error handling
 * - Environment setup
 */

import { runLocalCitty, setupCleanroom, runCitty, teardownCleanroom } from 'citty-test-utils';
import { readFile, writeFile, mkdir } from 'node:fs/promises';

console.log('🧪 UNRDF CLI Testing Example\n');

async function main() {
  try {
    // Create test data files
    await setupTestData();
    
    console.log('📁 Test data files created');
    
    // Test 1: Basic CLI help command
    console.log('\n🔍 Test 1: Basic Help Command');
    const helpResult = await runLocalCitty(['--help'], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    helpResult
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .expectOutput('Available commands:')
      .expectNoStderr();
    
    console.log('✅ Help command test passed');
    
    // Test 2: Parse command with valid Turtle data
    console.log('\n🔍 Test 2: Parse Command');
    const parseResult = await runLocalCitty([
      'parse', 
      './test-data/sample.ttl',
      '--format', 'turtle'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    parseResult
      .expectSuccess()
      .expectOutput('Parsed 4 triples successfully')
      .expectOutput('Triples: 4')
      .expectNoStderr();
    
    console.log('✅ Parse command test passed');
    
    // Test 3: Query command with SPARQL
    console.log('\n🔍 Test 3: Query Command');
    const queryResult = await runLocalCitty([
      'query',
      './test-data/sample.ttl',
      '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }',
      '--format', 'table'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    queryResult
      .expectSuccess()
      .expectOutput('John Doe')
      .expectOutput('Jane Smith')
      .expectNoStderr();
    
    console.log('✅ Query command test passed');
    
    // Test 4: Validation command
    console.log('\n🔍 Test 4: Validation Command');
    const validateResult = await runLocalCitty([
      'validate',
      './test-data/sample.ttl',
      './test-data/person-shape.ttl'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    validateResult
      .expectSuccess()
      .expectOutput('Conforms: true')
      .expectOutput('Violations: 0')
      .expectNoStderr();
    
    console.log('✅ Validation command test passed');
    
    // Test 5: Error handling - invalid command
    console.log('\n🔍 Test 5: Error Handling');
    const errorResult = await runLocalCitty([
      'invalid-command'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    errorResult
      .expectFailure()
      .expectStderr(/Unknown command/);
    
    console.log('✅ Error handling test passed');
    
    // Test 6: Cleanroom environment testing
    console.log('\n🔍 Test 6: Cleanroom Environment');
    
    await setupCleanroom({ 
      rootDir: './playground',
      nodeImage: 'node:18-alpine'
    });
    
    const cleanroomResult = await runCitty(['--help'], {
      env: { DEBUG: 'true' }
    });
    
    cleanroomResult
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .expectNoStderr();
    
    console.log('✅ Cleanroom environment test passed');
    
    await teardownCleanroom();
    
    // Test 7: Complex workflow test
    console.log('\n🔍 Test 7: Complex Workflow');
    
    // Step 1: Parse data
    const step1 = await runLocalCitty([
      'parse',
      './test-data/sample.ttl',
      '--output', './test-data/parsed.ttl'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    step1.expectSuccess();
    console.log('  ✅ Step 1: Parse completed');
    
    // Step 2: Query the parsed data
    const step2 = await runLocalCitty([
      'query',
      './test-data/parsed.ttl',
      '--query', 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }',
      '--format', 'table'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    step2
      .expectSuccess()
      .expectOutput('4'); // Should find 4 triples
    
    console.log('  ✅ Step 2: Query completed');
    
    // Step 3: Validate the data
    const step3 = await runLocalCitty([
      'validate',
      './test-data/parsed.ttl',
      './test-data/person-shape.ttl'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    step3
      .expectSuccess()
      .expectOutput('Conforms: true');
    
    console.log('  ✅ Step 3: Validation completed');
    
    console.log('✅ Complex workflow test passed');
    
    // Test 8: Performance testing
    console.log('\n🔍 Test 8: Performance Testing');
    
    const startTime = Date.now();
    const perfResult = await runLocalCitty([
      'parse',
      './test-data/sample.ttl'
    ], {
      cwd: './playground',
      env: { DEBUG: 'false' } // Disable debug for cleaner timing
    });
    
    const endTime = Date.now();
    const duration = endTime - startTime;
    
    perfResult.expectSuccess();
    console.log(`  ✅ Parse completed in ${duration}ms`);
    
    if (duration > 5000) {
      console.log('  ⚠️  Performance warning: Parse took longer than expected');
    } else {
      console.log('  ✅ Performance test passed');
    }
    
    // Test 9: Output format testing
    console.log('\n🔍 Test 9: Output Format Testing');
    
    // Test JSON output
    const jsonResult = await runLocalCitty([
      'query',
      './test-data/sample.ttl',
      '--query', 'SELECT ?name WHERE { ?person foaf:name ?name } LIMIT 1',
      '--format', 'json'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    jsonResult
      .expectSuccess()
      .expectOutput(/\[.*"name".*\]/); // Should contain JSON array
    
    console.log('  ✅ JSON output format test passed');
    
    // Test table output
    const tableResult = await runLocalCitty([
      'query',
      './test-data/sample.ttl',
      '--query', 'SELECT ?name WHERE { ?person foaf:name ?name } LIMIT 1',
      '--format', 'table'
    ], {
      cwd: './playground',
      env: { DEBUG: 'true' }
    });
    
    tableResult
      .expectSuccess()
      .expectOutput('name'); // Should contain table header
    
    console.log('  ✅ Table output format test passed');
    
    console.log('\n🎉 All CLI tests completed successfully!');
    
    // Cleanup
    await cleanupTestData();
    
  } catch (error) {
    console.error('❌ Test failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

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
    const { rm } = await import('node:fs/promises');
    await rm('./test-data', { recursive: true, force: true });
    console.log('🧹 Test data cleaned up');
  } catch (error) {
    console.log('⚠️  Could not clean up test data:', error.message);
  }
}

main();
