#!/usr/bin/env node

/**
 * Test suite for id-utils.mjs
 * 
 * Tests ID generation and management utilities
 */

import { 
  makeBNodeGenerator, skolemize, generateRandomBNodeId, generateDeterministicBNodeId,
  createRandomBlankNode, createDeterministicBlankNode, generateUUID, generateId,
  generateHashId, generateShortUUID, createUUIDNamedNode, createShortUUIDNamedNode,
  generateTimestampId, makeCounterIdGenerator, createHashIRI, createHashNamedNode,
  createNamespaceId, createNamespaceNamedNode, extractLocalName, extractNamespace,
  isBlankNodeIRI, iriToBlankNodeId, blankNodeIdToIRI, generateStableId, createStableNamedNode
} from '../../src/utils/id-utils.mjs';
import { DataFactory } from 'n3';

const { namedNode, blankNode } = DataFactory;

console.log('🧪 Testing id-utils.mjs\n');

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

// === makeBNodeGenerator tests ===
test('makeBNodeGenerator creates sequential blank nodes', () => {
  const generator = makeBNodeGenerator('test');
  const bnode1 = generator();
  const bnode2 = generator();
  
  if (bnode1.termType !== 'BlankNode') throw new Error('Should be BlankNode');
  if (bnode2.termType !== 'BlankNode') throw new Error('Should be BlankNode');
  if (bnode1.value === bnode2.value) throw new Error('Should be different values');
  if (!bnode1.value.startsWith('test')) throw new Error('Should start with prefix');
});

// === skolemize tests ===
test('skolemize creates skolemized IRI', () => {
  const result = skolemize('bnode123');
  if (result !== 'http://example.org/.well-known/genid/bnode123') throw new Error('Wrong skolemized IRI');
});

test('skolemize with custom base IRI', () => {
  const result = skolemize('bnode123', 'http://custom.org/genid/');
  if (result !== 'http://custom.org/genid/bnode123') throw new Error('Wrong skolemized IRI');
});

// === generateRandomBNodeId tests ===
test('generateRandomBNodeId generates random ID', () => {
  const id1 = generateRandomBNodeId(8);
  const id2 = generateRandomBNodeId(8);
  
  if (id1.length !== 8) throw new Error('Wrong length');
  if (id2.length !== 8) throw new Error('Wrong length');
  if (id1 === id2) throw new Error('Should be different');
});

// === generateDeterministicBNodeId tests ===
test('generateDeterministicBNodeId generates deterministic ID', () => {
  const id1 = generateDeterministicBNodeId('test content');
  const id2 = generateDeterministicBNodeId('test content');
  
  if (id1.length !== 16) throw new Error('Wrong length');
  if (id1 !== id2) throw new Error('Should be identical');
});

// === createRandomBlankNode tests ===
test('createRandomBlankNode creates blank node', () => {
  const bnode = createRandomBlankNode(8);
  if (bnode.termType !== 'BlankNode') throw new Error('Should be BlankNode');
  if (bnode.value.length !== 8) throw new Error('Wrong ID length');
});

// === createDeterministicBlankNode tests ===
test('createDeterministicBlankNode creates deterministic blank node', () => {
  const bnode1 = createDeterministicBlankNode('test content');
  const bnode2 = createDeterministicBlankNode('test content');
  
  if (bnode1.termType !== 'BlankNode') throw new Error('Should be BlankNode');
  if (bnode1.value !== bnode2.value) throw new Error('Should be identical');
});

// === generateUUID tests ===
test('generateUUID generates valid UUID', () => {
  const uuid = generateUUID();
  const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
  if (!uuidRegex.test(uuid)) throw new Error('Invalid UUID format');
});

// === generateId tests ===
test('generateId generates ID with prefix', () => {
  const id = generateId('test');
  if (!id.startsWith('test-')) throw new Error('Should start with prefix');
  if (id.length <= 5) throw new Error('Should be longer than prefix');
});

// === generateHashId tests ===
test('generateHashId generates hash-based ID', () => {
  const id1 = generateHashId('test content');
  const id2 = generateHashId('test content');
  
  if (id1.length !== 64) throw new Error('Should be 64 characters (SHA-256)');
  if (id1 !== id2) throw new Error('Should be identical');
});

// === generateShortUUID tests ===
test('generateShortUUID generates short UUID', () => {
  const shortUuid = generateShortUUID();
  if (shortUuid.length < 10) throw new Error('Should be reasonably long');
});

// === createUUIDNamedNode tests ===
test('createUUIDNamedNode creates named node with UUID', () => {
  const namedNode = createUUIDNamedNode();
  if (namedNode.termType !== 'NamedNode') throw new Error('Should be NamedNode');
  if (!namedNode.value.includes('http://example.org/id/')) throw new Error('Should use default base IRI');
});

// === createShortUUIDNamedNode tests ===
test('createShortUUIDNamedNode creates named node with short UUID', () => {
  const namedNode = createShortUUIDNamedNode();
  if (namedNode.termType !== 'NamedNode') throw new Error('Should be NamedNode');
  if (!namedNode.value.includes('http://example.org/id/')) throw new Error('Should use default base IRI');
});

// === generateTimestampId tests ===
test('generateTimestampId generates timestamp-based ID', () => {
  const id = generateTimestampId('test');
  if (!id.startsWith('test')) throw new Error('Should start with prefix');
  if (isNaN(parseInt(id.slice(4)))) throw new Error('Should contain timestamp');
});

// === makeCounterIdGenerator tests ===
test('makeCounterIdGenerator creates sequential IDs', () => {
  const generator = makeCounterIdGenerator('id');
  const id1 = generator();
  const id2 = generator();
  
  if (id1 !== 'id0') throw new Error('First ID should be id0');
  if (id2 !== 'id1') throw new Error('Second ID should be id1');
});

// === createHashIRI tests ===
test('createHashIRI creates hash-based IRI', () => {
  const iri = createHashIRI('test content');
  if (!iri.startsWith('http://example.org/hash/')) throw new Error('Should use default base IRI');
  if (!iri.includes('sha256/')) throw new Error('Should include algorithm');
});

// === createHashNamedNode tests ===
test('createHashNamedNode creates named node with hash IRI', () => {
  const namedNode = createHashNamedNode('test content');
  if (namedNode.termType !== 'NamedNode') throw new Error('Should be NamedNode');
  if (!namedNode.value.includes('sha256/')) throw new Error('Should include hash');
});

// === createNamespaceId tests ===
test('createNamespaceId creates namespace ID', () => {
  const id = createNamespaceId('http://example.org/', 'test');
  if (id !== 'http://example.org/test') throw new Error('Wrong namespace ID');
});

test('createNamespaceId adds hash to namespace', () => {
  const id = createNamespaceId('http://example.org', 'test');
  if (id !== 'http://example.org#test') throw new Error('Should add hash');
});

// === createNamespaceNamedNode tests ===
test('createNamespaceNamedNode creates named node with namespace', () => {
  const namedNode = createNamespaceNamedNode('http://example.org/', 'test');
  if (namedNode.termType !== 'NamedNode') throw new Error('Should be NamedNode');
  if (namedNode.value !== 'http://example.org/test') throw new Error('Wrong value');
});

// === extractLocalName tests ===
test('extractLocalName extracts from hash namespace', () => {
  const localName = extractLocalName('http://example.org#test');
  if (localName !== 'test') throw new Error('Wrong local name');
});

test('extractLocalName extracts from slash namespace', () => {
  const localName = extractLocalName('http://example.org/path/test');
  if (localName !== 'test') throw new Error('Wrong local name');
});

// === extractNamespace tests ===
test('extractNamespace extracts hash namespace', () => {
  const namespace = extractNamespace('http://example.org#test');
  if (namespace !== 'http://example.org#') throw new Error('Wrong namespace');
});

test('extractNamespace extracts slash namespace', () => {
  const namespace = extractNamespace('http://example.org/path/test');
  if (namespace !== 'http://example.org/path/') throw new Error('Wrong namespace');
});

// === isBlankNodeIRI tests ===
test('isBlankNodeIRI detects blank node IRI', () => {
  if (!isBlankNodeIRI('_:bnode123')) throw new Error('Should detect _: prefix');
  if (!isBlankNodeIRI('http://example.org/.well-known/genid/bnode123')) throw new Error('Should detect genid');
  if (isBlankNodeIRI('http://example.org/regular')) throw new Error('Should not detect regular IRI');
});

// === iriToBlankNodeId tests ===
test('iriToBlankNodeId converts _: prefix', () => {
  const id = iriToBlankNodeId('_:bnode123');
  if (id !== 'bnode123') throw new Error('Wrong ID');
});

test('iriToBlankNodeId converts genid IRI', () => {
  const id = iriToBlankNodeId('http://example.org/.well-known/genid/bnode123');
  if (id !== 'bnode123') throw new Error('Wrong ID');
});

// === blankNodeIdToIRI tests ===
test('blankNodeIdToIRI converts to genid IRI', () => {
  const iri = blankNodeIdToIRI('bnode123');
  if (iri !== 'http://example.org/.well-known/genid/bnode123') throw new Error('Wrong IRI');
});

// === generateStableId tests ===
test('generateStableId generates stable ID from values', () => {
  const id1 = generateStableId('value1', 'value2');
  const id2 = generateStableId('value1', 'value2');
  
  if (id1 !== id2) throw new Error('Should be identical');
  if (id1.length !== 16) throw new Error('Should be 16 characters');
});

// === createStableNamedNode tests ===
test('createStableNamedNode creates stable named node', () => {
  const namedNode1 = createStableNamedNode('http://example.org/', 'value1', 'value2');
  const namedNode2 = createStableNamedNode('http://example.org/', 'value1', 'value2');
  
  if (namedNode1.termType !== 'NamedNode') throw new Error('Should be NamedNode');
  if (namedNode1.value !== namedNode2.value) throw new Error('Should be identical');
  if (!namedNode1.value.startsWith('http://example.org/')) throw new Error('Should use base IRI');
});

// === Summary ===
console.log(`\n📊 Results: ${testsPassed}/${testsTotal} tests passed`);

if (testsPassed === testsTotal) {
  console.log('🎉 All id-utils tests passed!');
  process.exit(0);
} else {
  console.log('❌ Some id-utils tests failed');
  process.exit(1);
}
