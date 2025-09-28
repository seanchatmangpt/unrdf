#!/usr/bin/env node

/**
 * Test suite for sparql-utils.mjs
 * 
 * Tests SPARQL query building and analysis utilities
 */

import { 
  SPARQLBuilder, createSPARQLBuilder, buildSelectQuery, buildConstructQuery,
  buildAskQuery, COMMON_PATTERNS, analyzeSPARQLQuery, validateSPARQLQuery,
  extractVariables, extractIRIs
} from '../../src/utils/sparql-utils.mjs';

console.log('🧪 Testing sparql-utils.mjs\n');

let testsPassed = 0;
let testsTotal = 0;

function test(name, fn) {
  testsTotal++;
  try {
    fn();
    console.log(`✅ ${name}`);
    testsPassed++;
  } catch (error) {
    console.log(`❌ ${name}: ${error.message}`);
  }
}

// === SPARQLBuilder tests ===
test('SPARQLBuilder creates builder', () => {
  const builder = new SPARQLBuilder();
  if (!builder) throw new Error('Should create builder');
});

test('SPARQLBuilder adds prefix', () => {
  const builder = new SPARQLBuilder();
  builder.addPrefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
  if (!builder.prefixes.has('rdf')) throw new Error('Should add prefix');
});

test('SPARQLBuilder adds multiple prefixes', () => {
  const builder = new SPARQLBuilder();
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', ex: 'http://example.org/' };
  builder.addPrefixes(prefixes);
  if (!builder.prefixes.has('rdf')) throw new Error('Should add rdf prefix');
  if (!builder.prefixes.has('ex')) throw new Error('Should add ex prefix');
});

test('SPARQLBuilder sets query type', () => {
  const builder = new SPARQLBuilder();
  builder.setType('ASK');
  if (builder.queryType !== 'ASK') throw new Error('Should set query type');
});

test('SPARQLBuilder adds SELECT variables', () => {
  const builder = new SPARQLBuilder();
  builder.select('?s', '?p', '?o');
  if (builder.selectVars.length !== 3) throw new Error('Should add 3 variables');
  if (!builder.selectVars.includes('?s')) throw new Error('Should include ?s');
});

test('SPARQLBuilder adds WHERE clause', () => {
  const builder = new SPARQLBuilder();
  builder.where('?s', '?p', '?o');
  if (builder.whereClauses.length !== 1) throw new Error('Should add WHERE clause');
  if (!builder.whereClauses[0].includes('?s ?p ?o')) throw new Error('Should include triple pattern');
});

test('SPARQLBuilder adds OPTIONAL clause', () => {
  const builder = new SPARQLBuilder();
  builder.optional('?s', '?p', '?o');
  if (builder.optionalClauses.length !== 1) throw new Error('Should add OPTIONAL clause');
  if (!builder.optionalClauses[0].includes('OPTIONAL')) throw new Error('Should include OPTIONAL');
});

test('SPARQLBuilder adds FILTER clause', () => {
  const builder = new SPARQLBuilder();
  builder.filter('?age > 18');
  if (builder.filters.length !== 1) throw new Error('Should add FILTER clause');
  if (!builder.filters[0].includes('FILTER')) throw new Error('Should include FILTER');
});

test('SPARQLBuilder adds GROUP BY', () => {
  const builder = new SPARQLBuilder();
  builder.groupBy('?s', '?p');
  if (builder.groupByVars.length !== 2) throw new Error('Should add GROUP BY variables');
});

test('SPARQLBuilder adds ORDER BY', () => {
  const builder = new SPARQLBuilder();
  builder.orderBy('?s', 'ASC');
  if (builder.orderByVars.length !== 1) throw new Error('Should add ORDER BY');
  if (!builder.orderByVars[0].includes('ASC')) throw new Error('Should include direction');
});

test('SPARQLBuilder sets LIMIT', () => {
  const builder = new SPARQLBuilder();
  builder.setLimit(10);
  if (builder.limit !== 10) throw new Error('Should set limit');
});

test('SPARQLBuilder sets OFFSET', () => {
  const builder = new SPARQLBuilder();
  builder.setOffset(5);
  if (builder.offset !== 5) throw new Error('Should set offset');
});

test('SPARQLBuilder sets DISTINCT', () => {
  const builder = new SPARQLBuilder();
  builder.setDistinct(true);
  if (!builder.distinct) throw new Error('Should set distinct');
});

test('SPARQLBuilder builds SELECT query', () => {
  const builder = new SPARQLBuilder();
  builder.addPrefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
  builder.select('?s', '?p', '?o');
  builder.where('?s', '?p', '?o');
  
  const query = builder.build();
  if (!query.includes('PREFIX rdf:')) throw new Error('Should include prefix');
  if (!query.includes('SELECT ?s ?p ?o')) throw new Error('Should include SELECT');
  if (!query.includes('WHERE {')) throw new Error('Should include WHERE');
  if (!query.includes('?s ?p ?o')) throw new Error('Should include triple pattern');
});

test('SPARQLBuilder builds ASK query', () => {
  const builder = new SPARQLBuilder();
  builder.setType('ASK');
  builder.where('?s', '?p', '?o');
  
  const query = builder.build();
  if (!query.includes('ASK')) throw new Error('Should include ASK');
  if (!query.includes('WHERE {')) throw new Error('Should include WHERE');
});

test('SPARQLBuilder resets builder', () => {
  const builder = new SPARQLBuilder();
  builder.addPrefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
  builder.select('?s');
  builder.where('?s', '?p', '?o');
  builder.reset();
  
  if (builder.prefixes.size !== 0) throw new Error('Should clear prefixes');
  if (builder.selectVars.length !== 0) throw new Error('Should clear select vars');
  if (builder.whereClauses.length !== 0) throw new Error('Should clear where clauses');
});

// === createSPARQLBuilder tests ===
test('createSPARQLBuilder creates new builder', () => {
  const builder = createSPARQLBuilder();
  if (!(builder instanceof SPARQLBuilder)) throw new Error('Should be SPARQLBuilder instance');
});

// === buildSelectQuery tests ===
test('buildSelectQuery builds simple SELECT', () => {
  const variables = ['?s', '?p', '?o'];
  const patterns = {
    '?s': {
      '?p': '?o'
    }
  };
  
  const query = buildSelectQuery(variables, patterns);
  if (!query.includes('SELECT ?s ?p ?o')) throw new Error('Should include SELECT');
  if (!query.includes('?s ?p ?o')) throw new Error('Should include pattern');
});

test('buildSelectQuery with options', () => {
  const variables = ['?s'];
  const patterns = {
    '?s': {
      'rdf:type': 'ex:Person'
    }
  };
  const options = {
    prefixes: { ex: 'http://example.org/' },
    limit: 10,
    orderBy: { expression: '?s', direction: 'ASC' }
  };
  
  const query = buildSelectQuery(variables, patterns, options);
  if (!query.includes('PREFIX ex:')) throw new Error('Should include prefix');
  if (!query.includes('LIMIT 10')) throw new Error('Should include limit');
  if (!query.includes('ORDER BY ?s ASC')) throw new Error('Should include order by');
});

// === buildConstructQuery tests ===
test('buildConstructQuery builds CONSTRUCT', () => {
  const constructTemplate = {};
  const wherePatterns = {
    '?s': {
      '?p': '?o'
    }
  };
  
  const query = buildConstructQuery(constructTemplate, wherePatterns);
  if (!query.includes('CONSTRUCT')) throw new Error('Should include CONSTRUCT');
  if (!query.includes('WHERE {')) throw new Error('Should include WHERE');
});

// === buildAskQuery tests ===
test('buildAskQuery builds ASK', () => {
  const patterns = {
    '?s': {
      'rdf:type': 'ex:Person'
    }
  };
  
  const query = buildAskQuery(patterns);
  if (!query.includes('ASK')) throw new Error('Should include ASK');
  if (!query.includes('WHERE {')) throw new Error('Should include WHERE');
});

// === COMMON_PATTERNS tests ===
test('COMMON_PATTERNS.getTypes creates type pattern', () => {
  const pattern = COMMON_PATTERNS.getTypes('?s');
  if (!pattern['?s']['rdf:type']) throw new Error('Should include rdf:type pattern');
});

test('COMMON_PATTERNS.getProperties creates property pattern', () => {
  const pattern = COMMON_PATTERNS.getProperties('?s');
  if (!pattern['?s']['?property']) throw new Error('Should include property pattern');
});

test('COMMON_PATTERNS.findSubjectsOfType creates subject pattern', () => {
  const pattern = COMMON_PATTERNS.findSubjectsOfType('ex:Person');
  if (!pattern['?subject']['rdf:type']) throw new Error('Should include type pattern');
});

// === analyzeSPARQLQuery tests ===
test('analyzeSPARQLQuery analyzes SELECT query', () => {
  const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
  const analysis = analyzeSPARQLQuery(query);
  
  if (analysis.type !== 'SELECT') throw new Error('Should identify SELECT type');
  if (!analysis.variables.includes('s')) throw new Error('Should extract variables');
});

test('analyzeSPARQLQuery analyzes ASK query', () => {
  const query = 'ASK WHERE { ?s ?p ?o }';
  const analysis = analyzeSPARQLQuery(query);
  
  if (analysis.type !== 'ASK') throw new Error('Should identify ASK type');
});

test('analyzeSPARQLQuery analyzes CONSTRUCT query', () => {
  const query = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }';
  const analysis = analyzeSPARQLQuery(query);
  
  if (analysis.type !== 'CONSTRUCT') throw new Error('Should identify CONSTRUCT type');
});

test('analyzeSPARQLQuery extracts prefixes', () => {
  const query = 'PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> SELECT ?s WHERE { ?s rdf:type ?o }';
  const analysis = analyzeSPARQLQuery(query);
  
  if (analysis.prefixes.length === 0) throw new Error('Should extract prefixes');
  if (analysis.prefixes[0].prefix !== 'rdf') throw new Error('Should extract prefix name');
});

// === validateSPARQLQuery tests ===
test('validateSPARQLQuery validates correct query', () => {
  const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
  const result = validateSPARQLQuery(query);
  
  if (!result.valid) throw new Error('Should be valid');
  if (result.errorCount > 0) throw new Error('Should have no errors');
});

test('validateSPARQLQuery rejects empty query', () => {
  const query = '';
  const result = validateSPARQLQuery(query);
  
  if (result.valid) throw new Error('Should be invalid');
  if (result.errorCount === 0) throw new Error('Should have errors');
});

test('validateSPARQLQuery rejects query without type', () => {
  const query = 'WHERE { ?s ?p ?o }';
  const result = validateSPARQLQuery(query);
  
  if (result.valid) throw new Error('Should be invalid');
  if (result.errorCount === 0) throw new Error('Should have errors');
});

// === extractVariables tests ===
test('extractVariables extracts variables', () => {
  const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o . ?s ?p2 ?o2 }';
  const variables = extractVariables(query);
  
  if (variables.length !== 5) throw new Error('Should extract 5 variables');
  if (!variables.includes('s')) throw new Error('Should include s');
  if (!variables.includes('p')) throw new Error('Should include p');
  if (!variables.includes('o')) throw new Error('Should include o');
  if (!variables.includes('p2')) throw new Error('Should include p2');
  if (!variables.includes('o2')) throw new Error('Should include o2');
});

// === extractIRIs tests ===
test('extractIRIs extracts IRIs', () => {
  const query = 'SELECT ?s WHERE { ?s <http://example.org/p> <http://example.org/o> }';
  const iris = extractIRIs(query);
  
  if (iris.length !== 2) throw new Error('Should extract 2 IRIs');
  if (!iris.includes('http://example.org/p')) throw new Error('Should include predicate IRI');
  if (!iris.includes('http://example.org/o')) throw new Error('Should include object IRI');
});

// === Summary ===
console.log(`\n📊 Results: ${testsPassed}/${testsTotal} tests passed`);

if (testsPassed === testsTotal) {
  console.log('🎉 All sparql-utils tests passed!');
  process.exit(0);
} else {
  console.log('❌ Some sparql-utils tests failed');
  process.exit(1);
}
