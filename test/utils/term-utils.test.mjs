#!/usr/bin/env node

/**
 * Test suite for term-utils.mjs
 * 
 * Tests RDF term manipulation and conversion utilities
 */

import { 
  asNamedNode, asLiteral, asBlankNode, asString,
  isNamedNode, isLiteral, isBlankNode, getIRI, smartLiteral
} from '../../src/utils/term-utils.mjs';
import { DataFactory } from 'n3';

const { namedNode, literal, blankNode } = DataFactory;

console.log('🧪 Testing term-utils.mjs\n');

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

// === asNamedNode tests ===
test('asNamedNode with string', () => {
  const result = asNamedNode('http://example.org/test');
  if (result.termType !== 'NamedNode') throw new Error('Expected NamedNode');
  if (result.value !== 'http://example.org/test') throw new Error('Wrong value');
});

test('asNamedNode with existing NamedNode', () => {
  const existing = namedNode('http://example.org/existing');
  const result = asNamedNode(existing);
  if (result !== existing) throw new Error('Should return same instance');
});

test('asNamedNode with null throws error', () => {
  try {
    asNamedNode(null);
    throw new Error('Should have thrown error');
  } catch (error) {
    if (!error.message.includes('IRI cannot be null')) throw error;
  }
});

test('asNamedNode with undefined throws error', () => {
  try {
    asNamedNode(undefined);
    throw new Error('Should have thrown error');
  } catch (error) {
    if (!error.message.includes('IRI cannot be null')) throw error;
  }
});

// === asLiteral tests ===
test('asLiteral with string', () => {
  const result = asLiteral('hello');
  if (result.termType !== 'Literal') throw new Error('Expected Literal');
  if (result.value !== 'hello') throw new Error('Wrong value');
  if (result.datatype.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') throw new Error('Wrong datatype');
});

test('asLiteral with custom datatype', () => {
  const result = asLiteral('42', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString');
  if (result.value !== '42') throw new Error('Wrong value');
  if (result.datatype.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') throw new Error('Wrong datatype');
});

test('asLiteral with null throws error', () => {
  try {
    asLiteral(null);
    throw new Error('Should have thrown error');
  } catch (error) {
    if (!error.message.includes('value cannot be null')) throw error;
  }
});

// === asBlankNode tests ===
test('asBlankNode without ID', () => {
  const result = asBlankNode();
  if (result.termType !== 'BlankNode') throw new Error('Expected BlankNode');
});

test('asBlankNode with ID', () => {
  const result = asBlankNode('test123');
  if (result.termType !== 'BlankNode') throw new Error('Expected BlankNode');
  if (result.value !== 'test123') throw new Error('Wrong value');
});

// === asString tests ===
test('asString with Literal', () => {
  const literal = asLiteral('hello');
  const result = asString(literal);
  if (result !== 'hello') throw new Error('Wrong string value');
});

test('asString with NamedNode', () => {
  const namedNode = asNamedNode('http://example.org/test');
  const result = asString(namedNode);
  if (result !== 'http://example.org/test') throw new Error('Wrong string value');
});

test('asString with string', () => {
  const result = asString('hello');
  if (result !== 'hello') throw new Error('Wrong string value');
});

// === type checking tests ===
test('isNamedNode with NamedNode', () => {
  const namedNode = asNamedNode('http://example.org/test');
  if (!isNamedNode(namedNode)) throw new Error('Should be NamedNode');
});

test('isNamedNode with Literal', () => {
  const literal = asLiteral('hello');
  if (isNamedNode(literal)) throw new Error('Should not be NamedNode');
});

test('isLiteral with Literal', () => {
  const literal = asLiteral('hello');
  if (!isLiteral(literal)) throw new Error('Should be Literal');
});

test('isLiteral with NamedNode', () => {
  const namedNode = asNamedNode('http://example.org/test');
  if (isLiteral(namedNode)) throw new Error('Should not be Literal');
});

test('isBlankNode with BlankNode', () => {
  const blankNode = asBlankNode('test123');
  if (!isBlankNode(blankNode)) throw new Error('Should be BlankNode');
});

test('isBlankNode with NamedNode', () => {
  const namedNode = asNamedNode('http://example.org/test');
  if (isBlankNode(namedNode)) throw new Error('Should not be BlankNode');
});

// === getIRI tests ===
test('getIRI with NamedNode', () => {
  const namedNode = asNamedNode('http://example.org/test');
  const result = getIRI(namedNode);
  if (result !== 'http://example.org/test') throw new Error('Wrong IRI');
});

test('getIRI with string', () => {
  const result = getIRI('http://example.org/test');
  if (result !== 'http://example.org/test') throw new Error('Wrong IRI');
});

// === smartLiteral tests ===
test('smartLiteral with boolean', () => {
  const result = smartLiteral(true);
  if (result.value !== 'true') throw new Error('Wrong value');
  if (result.datatype.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') throw new Error('Wrong datatype');
});

test('smartLiteral with number', () => {
  const result = smartLiteral(42);
  if (result.value !== '42') throw new Error('Wrong value');
  if (result.datatype.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') throw new Error('Wrong datatype');
});

test('smartLiteral with Date', () => {
  const date = new Date('2023-01-01T00:00:00Z');
  const result = smartLiteral(date);
  if (result.value !== date.toISOString()) throw new Error('Wrong value');
  if (result.datatype.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') throw new Error('Wrong datatype');
});

test('smartLiteral with object', () => {
  const obj = { name: 'test' };
  const result = smartLiteral(obj);
  if (result.value !== JSON.stringify(obj)) throw new Error('Wrong value');
  if (result.datatype.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') throw new Error('Wrong datatype');
});

test('smartLiteral with string', () => {
  const result = smartLiteral('hello');
  if (result.value !== 'hello') throw new Error('Wrong value');
  if (result.datatype.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString') throw new Error('Wrong datatype');
});

// === Summary ===
console.log(`\n📊 Results: ${testsPassed}/${testsTotal} tests passed`);

if (testsPassed === testsTotal) {
  console.log('🎉 All term-utils tests passed!');
  process.exit(0);
} else {
  console.log('❌ Some term-utils tests failed');
  process.exit(1);
}
