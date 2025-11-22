/**
 * @file SPARQL Query Validator
 * @description Validates README SPARQL query claims (lines 176-206)
 */

import { createDarkMatterCore } from '../../src/knowledge-engine/dark-matter-core.mjs';
import { _query, select, ask, construct } from '../../src/knowledge-engine/query.mjs';
import { Store, DataFactory } from 'n3';

const { namedNode, _literal } = DataFactory;

/**
 * Validate SPARQL query functionality
 */
async function validateSparqlQueries() {
  console.log('\n🔍 SPARQL Query Validator Agent\n');
  console.log('='.repeat(60));

  const results = {
    status: 'PASS',
    tests: {
      systemQueryExists: false,
      selectQueryWorks: false,
      askQueryWorks: false,
      constructQueryWorks: false,
      returnTypesCorrect: false,
    },
    performance: {
      selectLatency: 0,
      askLatency: 0,
      constructLatency: 0,
    },
    issues: [],
    recommendations: [],
  };

  try {
    // Step 1: Create and initialize system (README pattern)
    console.log('\n📦 Step 1: Initialize Dark Matter Core');
    const system = createDarkMatterCore();
    await system.initialize();
    console.log('✅ System initialized');

    // Step 2: Add test data
    console.log('\n📝 Step 2: Add test data');
    const store = new Store();
    const foafKnows = namedNode('http://xmlns.com/foaf/0.1/knows');
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const charlie = namedNode('http://example.org/charlie');

    store.addQuad(alice, foafKnows, bob);
    store.addQuad(alice, foafKnows, charlie);
    store.addQuad(bob, foafKnows, charlie);

    console.log(`✅ Added ${store.size} test quads`);

    // Step 3: Test if system has query method (README claims system.query())
    console.log('\n🔍 Step 3: Check system.query() method');
    if (typeof system.query === 'function') {
      results.tests.systemQueryExists = true;
      console.log('✅ system.query() method exists');
    } else {
      results.tests.systemQueryExists = false;
      results.issues.push('❌ system.query() method does NOT exist on DarkMatterCore');
      results.recommendations.push(
        'README claims system.query() but DarkMatterCore does not expose this method'
      );
      console.log('❌ system.query() method NOT FOUND');
    }

    // Step 4: Test SELECT query (using direct query function)
    console.log('\n🔍 Step 4: Test SELECT query');
    const selectStart = Date.now();

    try {
      const selectResults = await select(
        store,
        `
        SELECT ?person ?friend
        WHERE {
          ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
        }
      `
      );

      results.performance.selectLatency = Date.now() - selectStart;

      if (Array.isArray(selectResults) && selectResults.length > 0) {
        results.tests.selectQueryWorks = true;
        console.log(
          `✅ SELECT query works (${selectResults.length} results in ${results.performance.selectLatency}ms)`
        );
      } else {
        results.tests.selectQueryWorks = false;
        results.issues.push('❌ SELECT query returned empty or invalid results');
      }
    } catch (error) {
      results.tests.selectQueryWorks = false;
      results.issues.push(`❌ SELECT query failed: ${error.message}`);
      results.performance.selectLatency = Date.now() - selectStart;
      console.log(`❌ SELECT query failed: ${error.message}`);
    }

    // Step 5: Test ASK query
    console.log('\n🔍 Step 5: Test ASK query');
    const askStart = Date.now();

    try {
      const askResult = await ask(store, 'ASK { ?s ?p ?o }');
      results.performance.askLatency = Date.now() - askStart;

      if (typeof askResult === 'boolean') {
        results.tests.askQueryWorks = true;
        console.log(
          `✅ ASK query works (result: ${askResult} in ${results.performance.askLatency}ms)`
        );
      } else {
        results.tests.askQueryWorks = false;
        results.issues.push(`❌ ASK query returned non-boolean: ${typeof askResult}`);
      }
    } catch (error) {
      results.tests.askQueryWorks = false;
      results.issues.push(`❌ ASK query failed: ${error.message}`);
      results.performance.askLatency = Date.now() - askStart;
      console.log(`❌ ASK query failed: ${error.message}`);
    }

    // Step 6: Test CONSTRUCT query
    console.log('\n🔍 Step 6: Test CONSTRUCT query');
    const constructStart = Date.now();

    try {
      const constructResult = await construct(
        store,
        `
        CONSTRUCT { ?s ?p ?o }
        WHERE { ?s ?p ?o }
      `
      );

      results.performance.constructLatency = Date.now() - constructStart;

      if (constructResult && typeof constructResult.getQuads === 'function') {
        results.tests.constructQueryWorks = true;
        console.log(
          `✅ CONSTRUCT query works (${constructResult.size} quads in ${results.performance.constructLatency}ms)`
        );
      } else {
        results.tests.constructQueryWorks = false;
        results.issues.push('❌ CONSTRUCT query did not return a valid Store');
      }
    } catch (error) {
      results.tests.constructQueryWorks = false;
      results.issues.push(`❌ CONSTRUCT query failed: ${error.message}`);
      results.performance.constructLatency = Date.now() - constructStart;
      console.log(`❌ CONSTRUCT query failed: ${error.message}`);
    }

    // Step 7: Verify return types
    console.log('\n🔍 Step 7: Verify return types');
    const typeChecks = [
      results.tests.selectQueryWorks && 'SELECT returns Array',
      results.tests.askQueryWorks && 'ASK returns boolean',
      results.tests.constructQueryWorks && 'CONSTRUCT returns Store',
    ].filter(Boolean);

    if (typeChecks.length === 3) {
      results.tests.returnTypesCorrect = true;
      console.log('✅ All return types correct');
    } else {
      results.tests.returnTypesCorrect = false;
      results.issues.push('❌ Some return types are incorrect');
    }

    // Cleanup
    await system.cleanup();
  } catch (error) {
    results.status = 'FAIL';
    results.issues.push(`❌ CRITICAL ERROR: ${error.message}`);
    console.error(`\n❌ CRITICAL ERROR: ${error.message}`);
  }

  // Determine final status
  const passedTests = Object.values(results.tests).filter(Boolean).length;
  const totalTests = Object.keys(results.tests).length;

  if (!results.tests.systemQueryExists) {
    results.status = 'FAIL';
    results.recommendations.push(
      'CRITICAL: README documentation is incorrect - system.query() does not exist'
    );
    results.recommendations.push(
      'Update README to show: import { query } from "unrdf" instead of system.query()'
    );
  }

  if (passedTests < totalTests) {
    results.status = 'FAIL';
  }

  // Print final report
  console.log('\n' + '='.repeat(60));
  console.log('\n📊 FINAL VALIDATION REPORT\n');
  console.log(`AGENT: SPARQL Query Validator`);
  console.log(`STATUS: ${results.status === 'PASS' ? '✅ PASS' : '❌ FAIL'}`);
  console.log(`\nTEST RESULTS (${passedTests}/${totalTests} passed):`);
  console.log(`${results.tests.systemQueryExists ? '✅' : '❌'} system.query() exists`);
  console.log(`${results.tests.selectQueryWorks ? '✅' : '❌'} SELECT query works`);
  console.log(`${results.tests.askQueryWorks ? '✅' : '❌'} ASK query works`);
  console.log(`${results.tests.constructQueryWorks ? '✅' : '❌'} CONSTRUCT query works`);
  console.log(`${results.tests.returnTypesCorrect ? '✅' : '❌'} Return types correct`);

  console.log(`\nQUERY PERFORMANCE:`);
  console.log(`- SELECT latency: ${results.performance.selectLatency}ms`);
  console.log(`- ASK latency: ${results.performance.askLatency}ms`);
  console.log(`- CONSTRUCT latency: ${results.performance.constructLatency}ms`);

  if (results.issues.length > 0) {
    console.log(`\nCRITICAL ISSUES:`);
    results.issues.forEach(issue => console.log(`  ${issue}`));
  }

  if (results.recommendations.length > 0) {
    console.log(`\nRECOMMENDATIONS:`);
    results.recommendations.forEach(rec => console.log(`  ${rec}`));
  }

  console.log('\n' + '='.repeat(60));

  // Exit with appropriate code
  process.exit(results.status === 'PASS' ? 0 : 1);
}

// Run validation
validateSparqlQueries().catch(error => {
  console.error('\n💥 VALIDATION CRASHED:', error);
  process.exit(1);
});
