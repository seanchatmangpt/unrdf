#!/usr/bin/env node

/**
 * Test suite for quad-utils.mjs
 * 
 * Tests quad/JSON transformations and filtering utilities
 */

import { 
  quadToJSON, jsonToQuad, quadsToJSON, jsonToQuads,
  extractSubjects, extractPredicates, extractObjects,
  filterBySubject, filterByPredicate, filterByObject,
  groupBySubject, groupByPredicate
} from '../../src/utils/quad-utils.mjs';
import { DataFactory } from 'n3';

const { quad, namedNode, literal, blankNode } = DataFactory;

console.log('🧪 Testing quad-utils.mjs\n');

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

// Create test quads
const testQuad1 = quad(
  namedNode('http://example.org/subject1'),
  namedNode('http://example.org/predicate1'),
  literal('object1')
);

const testQuad2 = quad(
  namedNode('http://example.org/subject1'),
  namedNode('http://example.org/predicate2'),
  literal('object2')
);

const testQuad3 = quad(
  namedNode('http://example.org/subject2'),
  namedNode('http://example.org/predicate1'),
  literal('object3')
);

const testQuads = [testQuad1, testQuad2, testQuad3];

// === quadToJSON tests ===
test('quadToJSON converts quad to JSON', () => {
  const result = quadToJSON(testQuad1);
  if (result.subject !== 'http://example.org/subject1') throw new Error('Wrong subject');
  if (result.predicate !== 'http://example.org/predicate1') throw new Error('Wrong predicate');
  if (result.object !== 'object1') throw new Error('Wrong object');
  if (result.subjectType !== 'NamedNode') throw new Error('Wrong subjectType');
  if (result.objectType !== 'Literal') throw new Error('Wrong objectType');
});

test('quadToJSON handles default graph', () => {
  const result = quadToJSON(testQuad1);
  if (result.graph !== null) throw new Error('Default graph should be null');
  if (result.graphType !== 'DefaultGraph') throw new Error('Wrong graphType');
});

// === jsonToQuad tests ===
test('jsonToQuad converts JSON to quad', () => {
  const json = {
    subject: 'http://example.org/subject1',
    predicate: 'http://example.org/predicate1',
    object: 'object1'
  };
  const result = jsonToQuad(json);
  if (result.subject.value !== 'http://example.org/subject1') throw new Error('Wrong subject');
  if (result.predicate.value !== 'http://example.org/predicate1') throw new Error('Wrong predicate');
  if (result.object.value !== 'object1') throw new Error('Wrong object');
});

test('jsonToQuad handles IRI objects', () => {
  const json = {
    subject: 'http://example.org/subject1',
    predicate: 'http://example.org/predicate1',
    object: 'http://example.org/object1'
  };
  const result = jsonToQuad(json);
  if (result.object.termType !== 'NamedNode') throw new Error('Object should be NamedNode');
});

test('jsonToQuad handles graph', () => {
  const json = {
    subject: 'http://example.org/subject1',
    predicate: 'http://example.org/predicate1',
    object: 'object1',
    graph: 'http://example.org/graph1'
  };
  const result = jsonToQuad(json);
  if (result.graph.value !== 'http://example.org/graph1') throw new Error('Wrong graph');
});

// === quadsToJSON tests ===
test('quadsToJSON converts array of quads', () => {
  const result = quadsToJSON(testQuads);
  if (result.length !== 3) throw new Error('Wrong length');
  if (result[0].subject !== 'http://example.org/subject1') throw new Error('Wrong first subject');
});

// === jsonToQuads tests ===
test('jsonToQuads converts array of JSON', () => {
  const jsonArray = [
    {
      subject: 'http://example.org/subject1',
      predicate: 'http://example.org/predicate1',
      object: 'object1'
    },
    {
      subject: 'http://example.org/subject2',
      predicate: 'http://example.org/predicate2',
      object: 'object2'
    }
  ];
  const result = jsonToQuads(jsonArray);
  if (result.length !== 2) throw new Error('Wrong length');
  if (result[0].subject.value !== 'http://example.org/subject1') throw new Error('Wrong first subject');
});

// === extractSubjects tests ===
test('extractSubjects gets unique subjects', () => {
  const result = extractSubjects(testQuads);
  if (result.length !== 2) throw new Error('Should have 2 unique subjects');
  if (!result.includes('http://example.org/subject1')) throw new Error('Missing subject1');
  if (!result.includes('http://example.org/subject2')) throw new Error('Missing subject2');
});

// === extractPredicates tests ===
test('extractPredicates gets unique predicates', () => {
  const result = extractPredicates(testQuads);
  if (result.length !== 2) throw new Error('Should have 2 unique predicates');
  if (!result.includes('http://example.org/predicate1')) throw new Error('Missing predicate1');
  if (!result.includes('http://example.org/predicate2')) throw new Error('Missing predicate2');
});

// === extractObjects tests ===
test('extractObjects gets unique objects', () => {
  const result = extractObjects(testQuads);
  if (result.length !== 3) throw new Error('Should have 3 unique objects');
  if (!result.includes('object1')) throw new Error('Missing object1');
  if (!result.includes('object2')) throw new Error('Missing object2');
  if (!result.includes('object3')) throw new Error('Missing object3');
});

// === filterBySubject tests ===
test('filterBySubject filters correctly', () => {
  const result = filterBySubject(testQuads, 'http://example.org/subject1');
  if (result.length !== 2) throw new Error('Should have 2 quads for subject1');
  if (result[0].subject.value !== 'http://example.org/subject1') throw new Error('Wrong subject in result');
});

test('filterBySubject returns empty for non-existent subject', () => {
  const result = filterBySubject(testQuads, 'http://example.org/nonexistent');
  if (result.length !== 0) throw new Error('Should be empty');
});

// === filterByPredicate tests ===
test('filterByPredicate filters correctly', () => {
  const result = filterByPredicate(testQuads, 'http://example.org/predicate1');
  if (result.length !== 2) throw new Error('Should have 2 quads for predicate1');
  if (result[0].predicate.value !== 'http://example.org/predicate1') throw new Error('Wrong predicate in result');
});

// === filterByObject tests ===
test('filterByObject filters correctly', () => {
  const result = filterByObject(testQuads, 'object1');
  if (result.length !== 1) throw new Error('Should have 1 quad for object1');
  if (result[0].object.value !== 'object1') throw new Error('Wrong object in result');
});

// === groupBySubject tests ===
test('groupBySubject groups correctly', () => {
  const result = groupBySubject(testQuads);
  if (result.size !== 2) throw new Error('Should have 2 groups');
  if (!result.has('http://example.org/subject1')) throw new Error('Missing subject1 group');
  if (!result.has('http://example.org/subject2')) throw new Error('Missing subject2 group');
  if (result.get('http://example.org/subject1').length !== 2) throw new Error('Subject1 should have 2 quads');
  if (result.get('http://example.org/subject2').length !== 1) throw new Error('Subject2 should have 1 quad');
});

// === groupByPredicate tests ===
test('groupByPredicate groups correctly', () => {
  const result = groupByPredicate(testQuads);
  if (result.size !== 2) throw new Error('Should have 2 groups');
  if (!result.has('http://example.org/predicate1')) throw new Error('Missing predicate1 group');
  if (!result.has('http://example.org/predicate2')) throw new Error('Missing predicate2 group');
  if (result.get('http://example.org/predicate1').length !== 2) throw new Error('Predicate1 should have 2 quads');
  if (result.get('http://example.org/predicate2').length !== 1) throw new Error('Predicate2 should have 1 quad');
});

// === Summary ===
console.log(`\n📊 Results: ${testsPassed}/${testsTotal} tests passed`);

if (testsPassed === testsTotal) {
  console.log('🎉 All quad-utils tests passed!');
  process.exit(0);
} else {
  console.log('❌ Some quad-utils tests failed');
  process.exit(1);
}






