/**
 * @file SPARQL Executor Integration Test
 * @description Tests SPARQL executor with real OxigraphStore
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  executeSparqlQuery,
  executeParameterizedQuery,
  buildPrefixDeclarations,
  validateSparqlQuery
} from '../../../../src/cli/commands/sync/sparql-executor.mjs';

const { namedNode, literal, quad } = dataFactory;

console.log('Running SPARQL Executor Integration Tests...\n');

const tests = [];
const assert = (condition, message) => {
  if (!condition) throw new Error('FAILED: ' + message);
  tests.push(message);
};

// Create a real store with test data
const store = createStore();

// Add some test triples
const ex = 'http://example.org/';
const rdf = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const rdfs = 'http://www.w3.org/2000/01/rdf-schema#';
const owl = 'http://www.w3.org/2002/07/owl#';

// Add classes
store.add(quad(
  namedNode(ex + 'Person'),
  namedNode(rdf + 'type'),
  namedNode(owl + 'Class')
));

store.add(quad(
  namedNode(ex + 'Person'),
  namedNode(rdfs + 'label'),
  literal('Person', 'en')
));

store.add(quad(
  namedNode(ex + 'Organization'),
  namedNode(rdf + 'type'),
  namedNode(owl + 'Class')
));

store.add(quad(
  namedNode(ex + 'Organization'),
  namedNode(rdfs + 'label'),
  literal('Organization', 'en')
));

// Add properties
store.add(quad(
  namedNode(ex + 'name'),
  namedNode(rdf + 'type'),
  namedNode(owl + 'DatatypeProperty')
));

store.add(quad(
  namedNode(ex + 'name'),
  namedNode(rdfs + 'domain'),
  namedNode(ex + 'Person')
));

store.add(quad(
  namedNode(ex + 'worksFor'),
  namedNode(rdf + 'type'),
  namedNode(owl + 'ObjectProperty')
));

store.add(quad(
  namedNode(ex + 'worksFor'),
  namedNode(rdfs + 'domain'),
  namedNode(ex + 'Person')
));

store.add(quad(
  namedNode(ex + 'worksFor'),
  namedNode(rdfs + 'range'),
  namedNode(ex + 'Organization')
));

console.log('Store size:', store.size, 'quads\n');

// Test 1: Simple SELECT query
console.log('Test 1: Simple SELECT query for classes');
const classQuery = `
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  SELECT ?class WHERE { ?class rdf:type owl:Class }
`;
const classes = await executeSparqlQuery(store, classQuery, {});
console.log('  Found classes:', classes.map(r => r.class));
assert(classes.length === 2, 'Found 2 classes');
assert(classes.some(r => r.class === ex + 'Person'), 'Found Person class');
assert(classes.some(r => r.class === ex + 'Organization'), 'Found Organization class');

// Test 2: SELECT with labels
console.log('\nTest 2: SELECT classes with labels');
const labelQuery = `
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  SELECT ?class ?label WHERE {
    ?class rdf:type owl:Class .
    ?class rdfs:label ?label
  }
`;
const classesWithLabels = await executeSparqlQuery(store, labelQuery, {});
console.log('  Classes with labels:', classesWithLabels.map(r => ({ class: r.class, label: r.label })));
assert(classesWithLabels.length === 2, 'Found 2 classes with labels');
assert(classesWithLabels.some(r => r.label === 'Person'), 'Found Person label');

// Test 3: Query using custom prefixes
console.log('\nTest 3: Query with custom prefixes');
const customPrefixQuery = `
  SELECT ?prop WHERE {
    ?prop a owl:DatatypeProperty
  }
`;
const datatypeProps = await executeSparqlQuery(store, customPrefixQuery, {
  ex: ex
});
console.log('  Datatype properties:', datatypeProps.map(r => r.prop));
assert(datatypeProps.length === 1, 'Found 1 datatype property');
assert(datatypeProps[0].prop === ex + 'name', 'Found name property');

// Test 4: Parameterized query
console.log('\nTest 4: Parameterized query');
const paramQuery = 'SELECT ?prop WHERE { ?prop rdfs:domain $class }';
const personProps = await executeParameterizedQuery(
  store,
  paramQuery,
  { class: { value: ex + 'Person', type: 'uri' } },
  { ex: ex }
);
console.log('  Person properties:', personProps.map(r => r.prop));
assert(personProps.length === 2, 'Found 2 properties with Person domain');

// Test 5: Query with domain and range
console.log('\nTest 5: Query for object properties with domain and range');
const objectPropQuery = `
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  SELECT ?prop ?domain ?range WHERE {
    ?prop rdf:type owl:ObjectProperty .
    ?prop rdfs:domain ?domain .
    ?prop rdfs:range ?range
  }
`;
const objectProps = await executeSparqlQuery(store, objectPropQuery, {});
console.log('  Object properties:', objectProps.map(r => ({
  prop: r.prop,
  domain: r.domain,
  range: r.range
})));
assert(objectProps.length === 1, 'Found 1 object property');
assert(objectProps[0].prop === ex + 'worksFor', 'Found worksFor property');

// Test 6: Verify template-friendly format
console.log('\nTest 6: Verify template-friendly format');
const result = classesWithLabels[0];
assert(typeof result.class === 'string', 'class is a string');
assert(typeof result.label === 'string', 'label is a string');
assert(result._meta !== undefined, 'Has _meta property');
assert(result._meta.class.termType === 'NamedNode', 'class meta has termType');
assert(result._index !== undefined, 'Has _index property');

// Test 7: ASK query
console.log('\nTest 7: ASK query');
const askQuery = `
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  ASK { <http://example.org/Person> rdf:type owl:Class }
`;
const askResult = await executeSparqlQuery(store, askQuery, {});
console.log('  ASK result:', askResult);
assert(askResult[0].result === true, 'ASK returns true for existing triple');

// Test 8: Query validation
console.log('\nTest 8: Query validation');
const validQuery = 'SELECT ?s WHERE { ?s ?p ?o }';
const validResult = validateSparqlQuery(validQuery);
assert(validResult.valid === true, 'Valid query passes validation');
assert(validResult.type === 'SELECT', 'Detects SELECT type');

const invalidQuery = 'SELECT ?s WHERE { ?s ?p ?o';
const invalidResult = validateSparqlQuery(invalidQuery);
assert(invalidResult.valid === false, 'Invalid query fails validation');

console.log('\n' + '='.repeat(50));
console.log('All ' + tests.length + ' integration tests passed!');
console.log('='.repeat(50));
