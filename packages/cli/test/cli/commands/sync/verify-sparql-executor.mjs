/**
 * @file SPARQL Executor Verification Script
 * @description Standalone verification that SPARQL executor works correctly
 */

import {
  executeSparqlQuery,
  executeParameterizedQuery,
  buildPrefixDeclarations,
  transformResults,
  substituteParameters,
  validateSparqlQuery,
  SparqlExecutionError
} from '../../../../src/cli/commands/sync/sparql-executor.mjs';

const tests = [];
const assert = (condition, message) => {
  if (!condition) throw new Error('FAILED: ' + message);
  tests.push(message);
};

console.log('Running SPARQL Executor Tests...\n');

// Test buildPrefixDeclarations
const prefixes = buildPrefixDeclarations({ ex: 'http://example.org/' });
assert(prefixes.includes('PREFIX ex: <http://example.org/>'), 'buildPrefixDeclarations: custom prefix');
assert(prefixes.includes('PREFIX rdf:'), 'buildPrefixDeclarations: includes common prefixes');
assert(prefixes.includes('PREFIX owl:'), 'buildPrefixDeclarations: includes owl prefix');

// Test substituteParameters - simple string
const sub1 = substituteParameters('SELECT * WHERE { $subject a $type }', {
  subject: 'http://example.org/item',
  type: 'owl:Class'
});
assert(sub1.includes('<http://example.org/item>'), 'substituteParameters: URI substitution');
assert(sub1.includes('owl:Class'), 'substituteParameters: prefixed name preserved');

// Test substituteParameters - typed parameters
const sub2 = substituteParameters('SELECT * WHERE { $entity a owl:Class }', {
  entity: { value: 'http://example.org/Person', type: 'uri' }
});
assert(sub2.includes('<http://example.org/Person>'), 'substituteParameters: typed URI');

const sub3 = substituteParameters('SELECT * WHERE { ?s rdfs:label $label }', {
  label: { value: 'Hello', type: 'literal', language: 'en' }
});
assert(sub3.includes('"Hello"@en'), 'substituteParameters: language-tagged literal');

const sub4 = substituteParameters('SELECT * WHERE { ?s ex:count $count }', {
  count: { value: '42', type: 'typed-literal', datatype: 'http://www.w3.org/2001/XMLSchema#integer' }
});
assert(sub4.includes('"42"^^<http://www.w3.org/2001/XMLSchema#integer>'), 'substituteParameters: typed literal');

// Test substituteParameters - numeric and boolean
const sub5 = substituteParameters('SELECT * WHERE { ?s ex:age $age }', { age: 25 });
assert(sub5.includes('25'), 'substituteParameters: numeric value');

const sub6 = substituteParameters('SELECT * WHERE { ?s ex:active $active }', { active: true });
assert(sub6.includes('true'), 'substituteParameters: boolean value');

// Test validateSparqlQuery - valid queries
const v1 = validateSparqlQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
assert(v1.valid === true, 'validateSparqlQuery: valid SELECT');
assert(v1.type === 'SELECT', 'validateSparqlQuery: detects SELECT type');
assert(v1.variables.includes('s'), 'validateSparqlQuery: extracts variables');

const v2 = validateSparqlQuery('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');
assert(v2.valid === true, 'validateSparqlQuery: valid CONSTRUCT');
assert(v2.type === 'CONSTRUCT', 'validateSparqlQuery: detects CONSTRUCT type');

const v3 = validateSparqlQuery('ASK { ?s a owl:Class }');
assert(v3.valid === true, 'validateSparqlQuery: valid ASK');

const v4 = validateSparqlQuery('DESCRIBE ?s WHERE { ?s a owl:Class }');
assert(v4.valid === true, 'validateSparqlQuery: valid DESCRIBE');

// Test validateSparqlQuery - invalid queries
const v5 = validateSparqlQuery('');
assert(v5.valid === false, 'validateSparqlQuery: empty query is invalid');

const v6 = validateSparqlQuery('{ ?s ?p ?o }');
assert(v6.valid === false, 'validateSparqlQuery: missing query type');

const v7 = validateSparqlQuery('SELECT ?s WHERE { ?s ?p ?o');
assert(v7.valid === false, 'validateSparqlQuery: unbalanced braces');

// Test transformResults - Map bindings
const map = new Map();
map.set('name', { termType: 'Literal', value: 'Alice' });
map.set('age', { termType: 'Literal', value: '30' });
const t1 = transformResults([map]);
assert(t1.length === 1, 'transformResults: correct length');
assert(t1[0].name === 'Alice', 'transformResults: extracts value');
assert(t1[0]._index === 0, 'transformResults: adds index');
assert(t1[0]._meta !== undefined, 'transformResults: adds metadata');

// Test transformResults - boolean (ASK result)
const t2 = transformResults(true);
assert(t2.length === 1, 'transformResults: boolean result length');
assert(t2[0].result === true, 'transformResults: boolean result value');
assert(t2[0]._isBoolean === true, 'transformResults: marks boolean');

// Test transformResults - empty results
const t3 = transformResults([]);
assert(t3.length === 0, 'transformResults: empty array');
const t4 = transformResults(null);
assert(t4.length === 0, 'transformResults: null');

// Test transformResults - quads (CONSTRUCT/DESCRIBE)
const quads = [{
  subject: { termType: 'NamedNode', value: 'http://example.org/s' },
  predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
  object: { termType: 'Literal', value: 'value' },
  graph: null
}];
const t5 = transformResults(quads);
assert(t5[0]._isQuad === true, 'transformResults: marks quad');
assert(t5[0].subject === 'http://example.org/s', 'transformResults: quad subject');
assert(t5[0].predicate === 'http://example.org/p', 'transformResults: quad predicate');
assert(t5[0].object === 'value', 'transformResults: quad object');

// Test transformResults - URI with local name extraction
const map2 = new Map();
map2.set('class', { termType: 'NamedNode', value: 'http://example.org/ontology#Person' });
const t6 = transformResults([map2]);
assert(t6[0]._meta.class.localName === 'Person', 'transformResults: extracts local name');
assert(t6[0]._meta.class.namespace === 'http://example.org/ontology#', 'transformResults: extracts namespace');

// Test SparqlExecutionError
const err = new SparqlExecutionError('Test error', {
  query: 'SELECT ?s WHERE { ?s ?p ?o }',
  phase: 'execute'
});
assert(err.name === 'SparqlExecutionError', 'SparqlExecutionError: correct name');
assert(err.query !== undefined, 'SparqlExecutionError: has query');
assert(err.phase === 'execute', 'SparqlExecutionError: has phase');

// Test executeSparqlQuery - error cases
try {
  await executeSparqlQuery(null, 'SELECT ?s WHERE { ?s ?p ?o }');
  assert(false, 'executeSparqlQuery: should throw on null store');
} catch (e) {
  assert(e instanceof SparqlExecutionError, 'executeSparqlQuery: throws SparqlExecutionError');
}

try {
  await executeSparqlQuery({}, 'SELECT ?s WHERE { ?s ?p ?o }');
  assert(false, 'executeSparqlQuery: should throw on invalid store');
} catch (e) {
  assert(e.message.includes('query()'), 'executeSparqlQuery: helpful error for missing query method');
}

try {
  await executeSparqlQuery({ query: () => {} }, '');
  assert(false, 'executeSparqlQuery: should throw on empty query');
} catch (e) {
  assert(e instanceof SparqlExecutionError, 'executeSparqlQuery: throws on empty query');
}

// Test executeSparqlQuery with mock store
const mockStore = {
  query: (q) => {
    const results = new Map();
    results.set('class', { termType: 'NamedNode', value: 'http://example.org/Person' });
    return [results];
  }
};
const r1 = await executeSparqlQuery(mockStore, 'SELECT ?class WHERE { ?class a owl:Class }');
assert(r1.length === 1, 'executeSparqlQuery: returns results');
assert(r1[0].class === 'http://example.org/Person', 'executeSparqlQuery: transforms results');

// Test executeParameterizedQuery
const r2 = await executeParameterizedQuery(
  mockStore,
  'SELECT ?prop WHERE { $class ?prop ?value }',
  { class: { value: 'http://example.org/Person', type: 'uri' } }
);
assert(r2.length === 1, 'executeParameterizedQuery: works with parameters');

console.log('\n' + '='.repeat(50));
console.log('All ' + tests.length + ' tests passed!');
console.log('='.repeat(50));

for (let i = 0; i < tests.length; i++) {
  console.log('  ' + (i + 1) + '. ' + tests[i]);
}
