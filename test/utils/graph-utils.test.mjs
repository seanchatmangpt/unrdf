#!/usr/bin/env node

/**
 * Test suite for graph-utils.mjs
 * 
 * Tests store operations and query helpers
 */

import { 
  getObjects, getSubjects, getPredicates, isA, getTypes,
  pluck, indexByPredicate, getProperties, hasSubject,
  getAllSubjects, getAllPredicates, getAllObjects,
  findByProperty, getFirstObject, countQuadsForSubject, getQuadsForSubject
} from '../../src/utils/graph-utils.mjs';
import { Store } from 'n3';

console.log('🧪 Testing graph-utils.mjs\n');

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

// Create test store
const store = new Store();
store.addQuad('http://example.org/person1', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/Person');
store.addQuad('http://example.org/person1', 'http://example.org/name', 'John Doe');
store.addQuad('http://example.org/person1', 'http://example.org/age', '30');
store.addQuad('http://example.org/person2', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://example.org/Person');
store.addQuad('http://example.org/person2', 'http://example.org/name', 'Jane Smith');
store.addQuad('http://example.org/person2', 'http://example.org/age', '25');

// === getObjects tests ===
test('getObjects returns correct objects', () => {
  const result = getObjects(store, 'http://example.org/person1', 'http://example.org/name');
  if (result.length !== 1) throw new Error('Should have 1 object');
  if (result[0].value !== 'John Doe') throw new Error('Wrong object value');
});

test('getObjects with string subject', () => {
  const result = getObjects(store, 'http://example.org/person1', 'http://example.org/age');
  if (result.length !== 1) throw new Error('Should have 1 object');
  if (result[0].value !== '30') throw new Error('Wrong object value');
});

// === getSubjects tests ===
test('getSubjects returns correct subjects', () => {
  const result = getSubjects(store, 'http://example.org/name', 'John Doe');
  if (result.length !== 1) throw new Error('Should have 1 subject');
  if (result[0].value !== 'http://example.org/person1') throw new Error('Wrong subject value');
});

// === getPredicates tests ===
test('getPredicates returns correct predicates', () => {
  const result = getPredicates(store, 'http://example.org/person1', 'John Doe');
  if (result.length !== 1) throw new Error('Should have 1 predicate');
  if (result[0].value !== 'http://example.org/name') throw new Error('Wrong predicate value');
});

// === isA tests ===
test('isA returns true for correct type', () => {
  const result = isA(store, 'http://example.org/person1', 'http://example.org/Person');
  if (!result) throw new Error('Should be true');
});

test('isA returns false for wrong type', () => {
  const result = isA(store, 'http://example.org/person1', 'http://example.org/Animal');
  if (result) throw new Error('Should be false');
});

// === getTypes tests ===
test('getTypes returns correct types', () => {
  const result = getTypes(store, 'http://example.org/person1');
  if (result.length !== 1) throw new Error('Should have 1 type');
  if (result[0] !== 'http://example.org/Person') throw new Error('Wrong type');
});

// === pluck tests ===
test('pluck returns quads with predicate', () => {
  const result = pluck(store, 'http://example.org/name');
  if (result.length !== 2) throw new Error('Should have 2 quads');
  if (result[0].predicate.value !== 'http://example.org/name') throw new Error('Wrong predicate');
});

// === indexByPredicate tests ===
test('indexByPredicate creates correct index', () => {
  const result = indexByPredicate(store, 'http://example.org/name');
  if (result.size !== 2) throw new Error('Should have 2 entries');
  if (!result.has('http://example.org/person1')) throw new Error('Missing person1');
  if (!result.has('http://example.org/person2')) throw new Error('Missing person2');
  if (result.get('http://example.org/person1')[0] !== 'John Doe') throw new Error('Wrong value for person1');
});

// === getProperties tests ===
test('getProperties returns all properties', () => {
  const result = getProperties(store, 'http://example.org/person1');
  if (result.size !== 3) throw new Error('Should have 3 properties');
  if (!result.has('http://www.w3.org/1999/02/22-rdf-syntax-ns#type')) throw new Error('Missing type property');
  if (!result.has('http://example.org/name')) throw new Error('Missing name property');
  if (!result.has('http://example.org/age')) throw new Error('Missing age property');
});

// === hasSubject tests ===
test('hasSubject returns true for existing subject', () => {
  const result = hasSubject(store, 'http://example.org/person1');
  if (!result) throw new Error('Should be true');
});

test('hasSubject returns false for non-existent subject', () => {
  const result = hasSubject(store, 'http://example.org/nonexistent');
  if (result) throw new Error('Should be false');
});

// === getAllSubjects tests ===
test('getAllSubjects returns all subjects', () => {
  const result = getAllSubjects(store);
  if (result.length !== 2) throw new Error('Should have 2 subjects');
  if (!result.includes('http://example.org/person1')) throw new Error('Missing person1');
  if (!result.includes('http://example.org/person2')) throw new Error('Missing person2');
});

// === getAllPredicates tests ===
test('getAllPredicates returns all predicates', () => {
  const result = getAllPredicates(store);
  if (result.length !== 3) throw new Error('Should have 3 predicates');
  if (!result.includes('http://www.w3.org/1999/02/22-rdf-syntax-ns#type')) throw new Error('Missing type predicate');
  if (!result.includes('http://example.org/name')) throw new Error('Missing name predicate');
  if (!result.includes('http://example.org/age')) throw new Error('Missing age predicate');
});

// === getAllObjects tests ===
test('getAllObjects returns all objects', () => {
  const result = getAllObjects(store);
  if (result.length !== 5) throw new Error('Should have 5 objects');
  if (!result.includes('http://example.org/Person')) throw new Error('Missing Person type');
  if (!result.includes('John Doe')) throw new Error('Missing John Doe');
  if (!result.includes('Jane Smith')) throw new Error('Missing Jane Smith');
});

// === findByProperty tests ===
test('findByProperty finds subjects with property', () => {
  const result = findByProperty(store, 'http://example.org/name', 'John Doe');
  if (result.length !== 1) throw new Error('Should have 1 subject');
  if (result[0] !== 'http://example.org/person1') throw new Error('Wrong subject');
});

// === getFirstObject tests ===
test('getFirstObject returns first object', () => {
  const result = getFirstObject(store, 'http://example.org/person1', 'http://example.org/name');
  if (result !== 'John Doe') throw new Error('Wrong first object');
});

test('getFirstObject returns null for non-existent', () => {
  const result = getFirstObject(store, 'http://example.org/person1', 'http://example.org/nonexistent');
  if (result !== null) throw new Error('Should be null');
});

// === countQuadsForSubject tests ===
test('countQuadsForSubject returns correct count', () => {
  const result = countQuadsForSubject(store, 'http://example.org/person1');
  if (result !== 3) throw new Error('Should have 3 quads');
});

// === getQuadsForSubject tests ===
test('getQuadsForSubject returns all quads', () => {
  const result = getQuadsForSubject(store, 'http://example.org/person1');
  if (result.length !== 3) throw new Error('Should have 3 quads');
  if (result[0].subject.value !== 'http://example.org/person1') throw new Error('Wrong subject');
});

// === Summary ===
console.log(`\n📊 Results: ${testsPassed}/${testsTotal} tests passed`);

if (testsPassed === testsTotal) {
  console.log('🎉 All graph-utils tests passed!');
  process.exit(0);
} else {
  console.log('❌ Some graph-utils tests failed');
  process.exit(1);
}






