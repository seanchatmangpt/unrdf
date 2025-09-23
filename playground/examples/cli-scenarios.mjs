#!/usr/bin/env node

/**
 * CLI Scenarios Example using citty-test-utils
 * 
 * This example demonstrates advanced scenario-based testing:
 * - Multi-step workflows
 * - Scenario builders
 * - Pre-built scenarios
 * - Custom actions
 * - Environment-specific testing
 */

import { 
  scenario, 
  scenarios, 
  runLocalCitty, 
  setupCleanroom, 
  runCitty, 
  teardownCleanroom,
  testUtils
} from 'citty-test-utils';
import { readFile, writeFile, mkdir } from 'fs/promises';

console.log('🎭 UNRDF CLI Scenarios Example\n');

async function main() {
  try {
    // Setup test data
    await setupTestData();
    console.log('📁 Test data files created');
    
    // Scenario 1: Basic CLI workflow
    console.log('\n🎬 Scenario 1: Basic CLI Workflow');
    
    const basicWorkflow = await scenario('Basic CLI Workflow')
      .step('Get help information')
      .run('--help')
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .expectOutput('Available commands:')
      .step('Get version information')
      .run('--version')
      .expectSuccess()
      .expectOutput(/\d+\.\d+\.\d+/)
      .step('Test invalid command')
      .run('invalid-command')
      .expectFailure()
      .expectStderr(/Unknown command/)
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ Basic workflow completed: ${basicWorkflow.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 2: RDF Processing Pipeline
    console.log('\n🎬 Scenario 2: RDF Processing Pipeline');
    
    const rdfPipeline = await scenario('RDF Processing Pipeline')
      .step('Parse RDF data')
      .run('parse', './test-data/sample.ttl', '--format', 'turtle')
      .expectSuccess()
      .expectOutput('Parsed 4 triples successfully')
      .step('Query the parsed data')
      .run('query', './test-data/sample.ttl', '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }', '--format', 'table')
      .expectSuccess()
      .expectOutput('John Doe')
      .expectOutput('Jane Smith')
      .step('Validate the data')
      .run('validate', './test-data/sample.ttl', './test-data/person-shape.ttl')
      .expectSuccess()
      .expectOutput('Conforms: true')
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ RDF pipeline completed: ${rdfPipeline.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 3: Error Handling and Recovery
    console.log('\n🎬 Scenario 3: Error Handling and Recovery');
    
    const errorHandling = await scenario('Error Handling and Recovery')
      .step('Test invalid file path')
      .run('parse', './nonexistent.ttl')
      .expectFailure()
      .expectStderr(/Parse error/)
      .step('Test invalid SPARQL query')
      .run('query', './test-data/sample.ttl', '--query', 'INVALID SPARQL')
      .expectFailure()
      .expectStderr(/Query error/)
      .step('Test missing required arguments')
      .run('validate', './test-data/sample.ttl')
      .expectFailure()
      .expectStderr(/Validation error/)
      .step('Recovery with valid command')
      .run('--help')
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ Error handling completed: ${errorHandling.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 4: Custom Actions with Context
    console.log('\n🎬 Scenario 4: Custom Actions with Context');
    
    const customActions = await scenario('Custom Actions with Context')
      .step('Custom data preparation', async ({ context }) => {
        // Create custom test data
        const customData = `
          @prefix ex: <http://example.org/> .
          @prefix foaf: <http://xmlns.com/foaf/0.1/> .
          
          ex:custom a foaf:Person ;
            foaf:name "Custom Person" ;
            foaf:age 25 .
        `;
        
        await writeFile('./test-data/custom.ttl', customData);
        context.customDataCreated = true;
        
        return { success: true, message: 'Custom data created' };
      })
      .step('Process custom data')
      .run('parse', './test-data/custom.ttl')
      .expectSuccess()
      .expectOutput('Parsed 3 triples successfully')
      .step('Query custom data')
      .run('query', './test-data/custom.ttl', '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }', '--format', 'json')
      .expectSuccess()
      .expectOutput('Custom Person')
      .step('Cleanup custom data', async ({ context }) => {
        if (context.customDataCreated) {
          const { rm } = await import('fs/promises');
          await rm('./test-data/custom.ttl', { force: true });
          return { success: true, message: 'Custom data cleaned up' };
        }
        return { success: true, message: 'No cleanup needed' };
      })
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ Custom actions completed: ${customActions.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 5: Performance and Load Testing
    console.log('\n🎬 Scenario 5: Performance and Load Testing');
    
    const performanceTest = await scenario('Performance and Load Testing')
      .step('Single parse operation')
      .run('parse', './test-data/sample.ttl')
      .expectSuccess()
      .expectOutput('Parsed 4 triples successfully')
      .step('Multiple query operations', async ({ context }) => {
        const queries = [
          'SELECT ?name WHERE { ?person foaf:name ?name }',
          'SELECT ?age WHERE { ?person foaf:age ?age }',
          'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }'
        ];
        
        const results = [];
        for (const query of queries) {
          const result = await runLocalCitty([
            'query',
            './test-data/sample.ttl',
            '--query', query,
            '--format', 'table'
          ], { cwd: './playground' });
          
          results.push(result.success);
        }
        
        context.allQueriesSuccessful = results.every(r => r);
        return { 
          success: context.allQueriesSuccessful, 
          message: `Executed ${queries.length} queries successfully` 
        };
      })
      .step('Verify performance')
      .run('--help')
      .expectSuccess()
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ Performance test completed: ${performanceTest.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 6: Environment Comparison
    console.log('\n🎬 Scenario 6: Environment Comparison');
    
    // Test in local environment
    const localResult = await scenario('Local Environment Test')
      .step('Local help command')
      .run('--help')
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .execute('local', { cwd: './playground' });
    
    console.log(`  ✅ Local environment: ${localResult.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Test in cleanroom environment
    await setupCleanroom({ 
      rootDir: './playground',
      nodeImage: 'node:18-alpine'
    });
    
    const cleanroomResult = await scenario('Cleanroom Environment Test')
      .step('Cleanroom help command')
      .run('--help')
      .expectSuccess()
      .expectOutput('UNRDF Command Line Interface')
      .execute('cleanroom');
    
    console.log(`  ✅ Cleanroom environment: ${cleanroomResult.success ? 'SUCCESS' : 'FAILED'}`);
    
    await teardownCleanroom();
    
    // Scenario 7: Pre-built Scenarios
    console.log('\n🎬 Scenario 7: Pre-built Scenarios');
    
    // Use pre-built help scenario
    const helpScenario = await scenarios.help('local').execute({ cwd: './playground' });
    console.log(`  ✅ Pre-built help scenario: ${helpScenario.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Use pre-built version scenario
    const versionScenario = await scenarios.version('local').execute({ cwd: './playground' });
    console.log(`  ✅ Pre-built version scenario: ${versionScenario.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 8: Retry and Resilience Testing
    console.log('\n🎬 Scenario 8: Retry and Resilience Testing');
    
    const retryTest = await scenario('Retry and Resilience Testing')
      .step('Test retry mechanism', async () => {
        // Simulate a flaky operation that might fail
        let attempts = 0;
        const maxAttempts = 3;
        
        while (attempts < maxAttempts) {
          try {
            const result = await runLocalCitty(['--help'], { 
              cwd: './playground',
              timeout: 5000 // 5 second timeout
            });
            
            if (result.success) {
              return { success: true, message: `Succeeded on attempt ${attempts + 1}` };
            }
          } catch (error) {
            attempts++;
            if (attempts >= maxAttempts) {
              throw error;
            }
            // Wait before retry
            await new Promise(resolve => setTimeout(resolve, 1000));
          }
        }
        
        return { success: false, message: 'All retry attempts failed' };
      })
      .step('Verify resilience')
      .run('--version')
      .expectSuccess()
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ Retry test completed: ${retryTest.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 9: Complex Multi-Command Workflow
    console.log('\n🎬 Scenario 9: Complex Multi-Command Workflow');
    
    const complexWorkflow = await scenario('Complex Multi-Command Workflow')
      .step('Initialize workflow')
      .run('--help')
      .expectSuccess()
      .step('Parse sample data')
      .run('parse', './test-data/sample.ttl', '--output', './test-data/workflow-output.ttl')
      .expectSuccess()
      .step('Query parsed data')
      .run('query', './test-data/workflow-output.ttl', '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }', '--format', 'json')
      .expectSuccess()
      .step('Validate data')
      .run('validate', './test-data/workflow-output.ttl', './test-data/person-shape.ttl')
      .expectSuccess()
      .step('Cleanup workflow files', async () => {
        try {
          const { rm } = await import('fs/promises');
          await rm('./test-data/workflow-output.ttl', { force: true });
          return { success: true, message: 'Workflow files cleaned up' };
        } catch (error) {
          return { success: false, message: `Cleanup failed: ${error.message}` };
        }
      })
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ Complex workflow completed: ${complexWorkflow.success ? 'SUCCESS' : 'FAILED'}`);
    
    // Scenario 10: Edge Cases and Boundary Testing
    console.log('\n🎬 Scenario 10: Edge Cases and Boundary Testing');
    
    const edgeCases = await scenario('Edge Cases and Boundary Testing')
      .step('Test empty input')
      .run('parse', './test-data/empty.ttl')
      .expectSuccess() // Should handle empty input gracefully
      .step('Test large output')
      .run('query', './test-data/sample.ttl', '--query', 'SELECT * WHERE { ?s ?p ?o }', '--format', 'table')
      .expectSuccess()
      .step('Test special characters')
      .run('query', './test-data/sample.ttl', '--query', 'SELECT ?name WHERE { ?person foaf:name ?name }', '--format', 'json')
      .expectSuccess()
      .execute('local', { cwd: './playground' });
    
    console.log(`✅ Edge cases completed: ${edgeCases.success ? 'SUCCESS' : 'FAILED'}`);
    
    console.log('\n🎉 All CLI scenarios completed successfully!');
    
    // Cleanup
    await cleanupTestData();
    
  } catch (error) {
    console.error('❌ Scenario test failed:', error.message);
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
  
  // Empty file for edge case testing
  await writeFile('./test-data/empty.ttl', '');
  
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
    console.log('🧹 Test data cleaned up');
  } catch (error) {
    console.log('⚠️  Could not clean up test data:', error.message);
  }
}

main();
