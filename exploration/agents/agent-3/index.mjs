#!/usr/bin/env node

/**
 * UNRDF SPARQL Query & CONSTRUCT Derivation Explorer - Agent 3
 * Explores SPARQL SELECT queries and CONSTRUCT derivation capabilities
 *
 * Usage: node /home/user/unrdf/exploration/agents/agent-3/index.mjs
 * Output: Query results, derived triple counts, SPARQL feature support
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { createStore, dataFactory } from '../../../packages/oxigraph/src/index.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/**
 * Main SPARQL Explorer
 */
async function exploreSparqlCapabilities() {
  console.log('🔍 UNRDF SPARQL Query & CONSTRUCT Derivation Explorer\n');
  console.log('─'.repeat(60));

  // Create store and load test data
  const store = createStore();
  console.log('\n📦 Loading test dataset (Alice/Bob/Carol network)...');

  loadTestData(store);
  console.log(
    `✓ Loaded ${store.size} triples into store\n`
  );

  // Result collector
  const results = {
    timestamp: new Date().toISOString(),
    testDataLoaded: store.size,
    selectQueries: [],
    constructQueries: [],
    errors: [],
    sparqlFeaturesSupported: [],
    sparqlFeaturesUnsupported: [],
  };

  // Test 1: Basic SELECT query
  console.log('─'.repeat(60));
  console.log('\n✓ Test 1: Basic SELECT - Find all people\n');
  try {
    const selectResult = store.query(
      `SELECT ?person ?name WHERE {
        ?person a <http://xmlns.com/foaf/0.1/Person> ;
                <http://xmlns.com/foaf/0.1/name> ?name .
      }`
    );
    console.log(`  Query: SELECT ?person ?name WHERE { ... }`);
    console.log(`  Result count: ${selectResult.length} rows`);
    selectResult.forEach((row, idx) => {
      console.log(`    Row ${idx + 1}: ${JSON.stringify(row)}`);
    });

    results.selectQueries.push({
      name: 'Find all people',
      resultCount: selectResult.length,
      success: true,
      description: 'SELECT with rdf:type and literal matching',
    });
    results.sparqlFeaturesSupported.push('SELECT queries');
    results.sparqlFeaturesSupported.push('rdf:type (a) matching');
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.selectQueries.push({
      name: 'Find all people',
      error: error.message,
      success: false,
    });
    results.errors.push({
      test: 'SELECT query 1',
      error: error.message,
    });
  }

  // Test 2: SELECT with aggregate (COUNT)
  console.log('\n─'.repeat(60));
  console.log('\n✓ Test 2: SELECT with aggregate - Count people\n');
  try {
    const countResult = store.query(
      `SELECT (COUNT(?person) as ?count) WHERE {
        ?person a <http://xmlns.com/foaf/0.1/Person> .
      }`
    );
    console.log(`  Query: SELECT (COUNT(?person) as ?count) WHERE { ... }`);
    console.log(`  Result: ${JSON.stringify(countResult)}`);

    results.selectQueries.push({
      name: 'Count people',
      resultCount: countResult.length,
      rawResult: countResult,
      success: true,
      description: 'SELECT with COUNT aggregate function',
    });
    results.sparqlFeaturesSupported.push('COUNT aggregate');
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.selectQueries.push({
      name: 'Count people',
      error: error.message,
      success: false,
    });
  }

  // Test 3: SELECT with FILTER
  console.log('\n─'.repeat(60));
  console.log('\n✓ Test 3: SELECT with FILTER - Find people over 30\n');
  try {
    const filterResult = store.query(
      `SELECT ?name ?age WHERE {
        ?person <http://xmlns.com/foaf/0.1/name> ?name ;
                <http://xmlns.com/foaf/0.1/age> ?age .
        FILTER (?age > 30)
      }`
    );
    console.log(`  Query: SELECT ?name ?age WHERE { ... FILTER (?age > 30) }`);
    console.log(`  Result count: ${filterResult.length} rows`);
    filterResult.forEach((row, idx) => {
      console.log(`    Row ${idx + 1}: ${JSON.stringify(row)}`);
    });

    results.selectQueries.push({
      name: 'Find people over 30',
      resultCount: filterResult.length,
      success: true,
      description: 'SELECT with FILTER on numeric literal',
    });
    results.sparqlFeaturesSupported.push('FILTER clause');
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.selectQueries.push({
      name: 'Find people over 30',
      error: error.message,
      success: false,
    });
  }

  // Test 4: CONSTRUCT - Derive friend relationships
  console.log('\n─'.repeat(60));
  console.log('\n✓ Test 4: CONSTRUCT - Derive Friend Type\n');
  try {
    const constructResult = store.query(
      `CONSTRUCT {
        ?person <http://example.org/hasFriend> ?friend .
        ?person <http://example.org/isFriendOf> ?friend .
      } WHERE {
        ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
      }`
    );

    console.log(
      `  Query: CONSTRUCT { ... } WHERE { ?person foaf:knows ?friend }`
    );
    console.log(`  Result type: ${typeof constructResult}`);
    console.log(`  Result is array: ${Array.isArray(constructResult)}`);
    console.log(`  Result count: ${constructResult.length} derived triples`);

    constructResult.forEach((triple, idx) => {
      console.log(`    Triple ${idx + 1}: ${JSON.stringify(triple)}`);
    });

    results.constructQueries.push({
      name: 'Derive friend relationships',
      derivedTripleCount: constructResult.length,
      success: true,
      description: 'CONSTRUCT creates new triples from foaf:knows pattern',
    });
    results.sparqlFeaturesSupported.push('CONSTRUCT queries');
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.constructQueries.push({
      name: 'Derive friend relationships',
      error: error.message,
      success: false,
    });
    results.errors.push({
      test: 'CONSTRUCT query 1',
      error: error.message,
    });
  }

  // Test 5: CONSTRUCT with pattern derivation
  console.log('\n─'.repeat(60));
  console.log('\n✓ Test 5: CONSTRUCT - Derive Type-based Triples\n');
  try {
    const typeConstructResult = store.query(
      `CONSTRUCT {
        ?person <http://example.org/role> <http://example.org/ContactPerson> .
      } WHERE {
        ?person a <http://xmlns.com/foaf/0.1/Person> ;
                <http://xmlns.com/foaf/0.1/age> ?age .
        FILTER (?age >= 25)
      }`
    );

    console.log(
      `  Query: CONSTRUCT { ... role role:ContactPerson } WHERE { ... }`
    );
    console.log(`  Result count: ${typeConstructResult.length} derived triples`);
    typeConstructResult.forEach((triple, idx) => {
      console.log(`    Triple ${idx + 1}: ${JSON.stringify(triple)}`);
    });

    results.constructQueries.push({
      name: 'Derive person roles',
      derivedTripleCount: typeConstructResult.length,
      success: true,
      description: 'CONSTRUCT with FILTER to derive role assignments',
    });
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.constructQueries.push({
      name: 'Derive person roles',
      error: error.message,
      success: false,
    });
  }

  // Test 6: ASK query
  console.log('\n─'.repeat(60));
  console.log('\n✓ Test 6: ASK - Check if Alice knows anyone\n');
  try {
    const askResult = store.query(
      `ASK {
        <http://example.org/person1> <http://xmlns.com/foaf/0.1/knows> ?friend .
      }`
    );
    console.log(
      `  Query: ASK { <alice> foaf:knows ?friend }`
    );
    console.log(`  Result type: ${typeof askResult}`);
    console.log(`  Result value: ${askResult}`);

    results.sparqlFeaturesSupported.push('ASK queries');
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.errors.push({
      test: 'ASK query',
      error: error.message,
    });
  }

  // Test 7: CONSTRUCT with multiple patterns
  console.log('\n─'.repeat(60));
  console.log('\n✓ Test 7: CONSTRUCT - Complex derivation\n');
  try {
    const complexConstruct = store.query(
      `CONSTRUCT {
        ?person1 <http://example.org/connectedTo> ?person2 .
      } WHERE {
        ?person1 <http://xmlns.com/foaf/0.1/knows> ?person2 .
        ?person2 <http://xmlns.com/foaf/0.1/knows> ?person1 .
      }`
    );

    console.log(
      `  Query: CONSTRUCT mutual connections (knows both ways)`
    );
    console.log(`  Result count: ${complexConstruct.length} mutual connections`);
    complexConstruct.forEach((triple, idx) => {
      console.log(`    Triple ${idx + 1}: ${JSON.stringify(triple)}`);
    });

    results.constructQueries.push({
      name: 'Derive mutual connections',
      derivedTripleCount: complexConstruct.length,
      success: true,
      description: 'CONSTRUCT with bidirectional pattern matching',
    });
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.constructQueries.push({
      name: 'Derive mutual connections',
      error: error.message,
      success: false,
    });
  }

  // Test 8: DESCRIBE query (if supported)
  console.log('\n─'.repeat(60));
  console.log('\n✓ Test 8: DESCRIBE - Get all triples for a resource\n');
  try {
    const describeResult = store.query(
      `DESCRIBE <http://example.org/person1>`
    );
    console.log(
      `  Query: DESCRIBE <alice>`
    );
    console.log(`  Result type: ${typeof describeResult}`);
    console.log(`  Result is array: ${Array.isArray(describeResult)}`);
    if (Array.isArray(describeResult)) {
      console.log(`  Result count: ${describeResult.length} triples about Alice`);
      describeResult.forEach((triple, idx) => {
        console.log(`    Triple ${idx + 1}: ${JSON.stringify(triple)}`);
      });
    }

    results.sparqlFeaturesSupported.push('DESCRIBE queries');
  } catch (error) {
    console.error(`✗ Query failed: ${error.message}`);
    results.sparqlFeaturesUnsupported.push('DESCRIBE queries');
  }

  // Summary
  console.log('\n' + '─'.repeat(60));
  console.log('\n📊 SUMMARY\n');

  console.log(`✓ SELECT queries successful: ${results.selectQueries.filter(q => q.success).length}/${results.selectQueries.length}`);
  console.log(`✓ CONSTRUCT queries successful: ${results.constructQueries.filter(q => q.success).length}/${results.constructQueries.length}`);
  console.log(`✓ SPARQL features supported: ${results.sparqlFeaturesSupported.length}`);
  console.log(`  - ${results.sparqlFeaturesSupported.join('\n  - ')}`);

  if (results.sparqlFeaturesUnsupported.length > 0) {
    console.log(`⚠ SPARQL features unsupported:`);
    console.log(`  - ${results.sparqlFeaturesUnsupported.join('\n  - ')}`);
  }

  if (results.errors.length > 0) {
    console.log(`\n⚠ Errors encountered: ${results.errors.length}`);
    results.errors.forEach(e => {
      console.log(`  - ${e.test}: ${e.error}`);
    });
  }

  // Write results to file
  const resultsPath = path.join(__dirname, 'sparql-explorer-results.json');
  fs.writeFileSync(resultsPath, JSON.stringify(results, null, 2));
  console.log(`\n📁 Results saved to: ${resultsPath}`);

  console.log('\n✅ SPARQL exploration complete!\n');
  return results;
}

/**
 * Load test data into store
 * @param {OxigraphStore} store - The RDF store
 */
function loadTestData(store) {
  // Alice Johnson
  const alice = dataFactory.namedNode('http://example.org/person1');
  const aliceName = dataFactory.literal('Alice Johnson');
  const aliceAge = dataFactory.literal('30');

  store.addQuad(
    dataFactory.quad(
      alice,
      dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/Person')
    )
  );
  store.addQuad(
    dataFactory.quad(
      alice,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
      aliceName
    )
  );
  store.addQuad(
    dataFactory.quad(
      alice,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/age'),
      aliceAge
    )
  );

  // Bob Smith
  const bob = dataFactory.namedNode('http://example.org/person2');
  const bobName = dataFactory.literal('Bob Smith');
  const bobAge = dataFactory.literal('25');

  store.addQuad(
    dataFactory.quad(
      bob,
      dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/Person')
    )
  );
  store.addQuad(
    dataFactory.quad(
      bob,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
      bobName
    )
  );
  store.addQuad(
    dataFactory.quad(
      bob,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/age'),
      bobAge
    )
  );

  // Carol Davis
  const carol = dataFactory.namedNode('http://example.org/person3');
  const carolName = dataFactory.literal('Carol Davis');
  const carolAge = dataFactory.literal('35');

  store.addQuad(
    dataFactory.quad(
      carol,
      dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/Person')
    )
  );
  store.addQuad(
    dataFactory.quad(
      carol,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
      carolName
    )
  );
  store.addQuad(
    dataFactory.quad(
      carol,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/age'),
      carolAge
    )
  );

  // Relationships (foaf:knows)
  // Alice knows Bob and Carol
  store.addQuad(
    dataFactory.quad(
      alice,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows'),
      bob
    )
  );
  store.addQuad(
    dataFactory.quad(
      alice,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows'),
      carol
    )
  );

  // Bob knows Alice
  store.addQuad(
    dataFactory.quad(
      bob,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows'),
      alice
    )
  );

  // Carol knows Alice
  store.addQuad(
    dataFactory.quad(
      carol,
      dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows'),
      alice
    )
  );
}

// Run explorer
exploreSparqlCapabilities().catch(console.error);
