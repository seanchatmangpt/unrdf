#!/usr/bin/env node

/**
 * Test suite for namespace-utils.mjs
 * 
 * Tests namespace management and vocabulary utilities
 */

import { 
  COMMON_VOCABULARIES, COMMON_PREFIXES, NamespaceManager, createNamespaceManager,
  getVocabularyTerm, getVocabularyForIRI, getVocabularyStats, validateNamespaces,
  generateTurtlePrefixes, generateSPARQLPrefixes, createNamespace, expandCurie, shrinkIri
} from '../../src/utils/namespace-utils.mjs';
import { Store } from 'n3';

console.log('🧪 Testing namespace-utils.mjs\n');

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

// === COMMON_VOCABULARIES tests ===
test('COMMON_VOCABULARIES contains expected vocabularies', () => {
  if (!COMMON_VOCABULARIES.RDF) throw new Error('Missing RDF vocabulary');
  if (!COMMON_VOCABULARIES.RDFS) throw new Error('Missing RDFS vocabulary');
  if (!COMMON_VOCABULARIES.OWL) throw new Error('Missing OWL vocabulary');
  if (!COMMON_VOCABULARIES.FOAF) throw new Error('Missing FOAF vocabulary');
});

// === COMMON_PREFIXES tests ===
test('COMMON_PREFIXES contains expected prefixes', () => {
  if (!COMMON_PREFIXES.rdf) throw new Error('Missing rdf prefix');
  if (!COMMON_PREFIXES.rdfs) throw new Error('Missing rdfs prefix');
  if (!COMMON_PREFIXES.owl) throw new Error('Missing owl prefix');
  if (!COMMON_PREFIXES.foaf) throw new Error('Missing foaf prefix');
});

// === NamespaceManager tests ===
test('NamespaceManager initializes with common vocabularies', () => {
  const manager = new NamespaceManager();
  if (manager.getNamespace('rdf') !== COMMON_VOCABULARIES.RDF) throw new Error('Wrong RDF namespace');
  if (manager.getNamespace('rdfs') !== COMMON_VOCABULARIES.RDFS) throw new Error('Wrong RDFS namespace');
});

test('NamespaceManager adds custom namespace', () => {
  const manager = new NamespaceManager();
  manager.addNamespace('ex', 'http://example.org/');
  if (manager.getNamespace('ex') !== 'http://example.org/') throw new Error('Wrong custom namespace');
});

test('NamespaceManager creates named node with prefix', () => {
  const manager = new NamespaceManager();
  const namedNode = manager.createNamedNode('rdf', 'type');
  if (namedNode.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') throw new Error('Wrong named node');
});

test('NamespaceManager expands prefixed IRI', () => {
  const manager = new NamespaceManager();
  const expanded = manager.expandIRI('rdf:type');
  if (expanded !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') throw new Error('Wrong expansion');
});

test('NamespaceManager contracts full IRI', () => {
  const manager = new NamespaceManager();
  const contracted = manager.contractIRI('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  if (contracted !== 'rdf:type') throw new Error('Wrong contraction');
});

test('NamespaceManager exports prefixes', () => {
  const manager = new NamespaceManager();
  const prefixes = manager.exportPrefixes();
  if (!prefixes.rdf) throw new Error('Missing rdf prefix in export');
  if (!prefixes.rdfs) throw new Error('Missing rdfs prefix in export');
});

test('NamespaceManager imports prefixes', () => {
  const manager = new NamespaceManager();
  const customPrefixes = { ex: 'http://example.org/' };
  manager.importPrefixes(customPrefixes);
  if (manager.getNamespace('ex') !== 'http://example.org/') throw new Error('Wrong imported namespace');
});

test('NamespaceManager removes namespace', () => {
  const manager = new NamespaceManager();
  manager.addNamespace('ex', 'http://example.org/');
  manager.removeNamespace('ex');
  if (manager.getNamespace('ex') !== null) throw new Error('Namespace should be removed');
});

test('NamespaceManager clears all namespaces', () => {
  const manager = new NamespaceManager();
  manager.clear();
  if (manager.getPrefixes().length > 0) throw new Error('Should have no prefixes after clear');
});

// === createNamespaceManager tests ===
test('createNamespaceManager creates new manager', () => {
  const manager = createNamespaceManager();
  if (!(manager instanceof NamespaceManager)) throw new Error('Should be NamespaceManager instance');
});

// === getVocabularyTerm tests ===
test('getVocabularyTerm gets RDF term', () => {
  const term = getVocabularyTerm('RDF', 'type');
  if (term.value !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') throw new Error('Wrong RDF term');
});

test('getVocabularyTerm gets FOAF term', () => {
  const term = getVocabularyTerm('FOAF', 'Person');
  if (term.value !== 'http://xmlns.com/foaf/0.1/Person') throw new Error('Wrong FOAF term');
});

test('getVocabularyTerm throws for unknown vocabulary', () => {
  try {
    getVocabularyTerm('UNKNOWN', 'term');
    throw new Error('Should have thrown error');
  } catch (error) {
    if (!error.message.includes('Unknown vocabulary')) throw error;
  }
});

// === getVocabularyForIRI tests ===
test('getVocabularyForIRI identifies RDF vocabulary', () => {
  const vocab = getVocabularyForIRI('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  if (vocab !== 'RDF') throw new Error('Should identify RDF vocabulary');
});

test('getVocabularyForIRI identifies FOAF vocabulary', () => {
  const vocab = getVocabularyForIRI('http://xmlns.com/foaf/0.1/Person');
  if (vocab !== 'FOAF') throw new Error('Should identify FOAF vocabulary');
});

test('getVocabularyForIRI returns null for unknown vocabulary', () => {
  const vocab = getVocabularyForIRI('http://example.org/unknown');
  if (vocab !== null) throw new Error('Should return null for unknown vocabulary');
});

// === getVocabularyStats tests ===
test('getVocabularyStats analyzes store', () => {
  const store = new Store();
  store.addQuad('http://example.org/person1', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://xmlns.com/foaf/0.1/Person');
  store.addQuad('http://example.org/person1', 'http://xmlns.com/foaf/0.1/name', 'John Doe');
  
  const stats = getVocabularyStats(store);
  if (!stats.vocabularies.includes('RDF')) throw new Error('Should include RDF vocabulary');
  if (!stats.vocabularies.includes('FOAF')) throw new Error('Should include FOAF vocabulary');
  if (stats.totalVocabularies !== 2) throw new Error('Should have 2 vocabularies');
});

// === validateNamespaces tests ===
test('validateNamespaces validates store', () => {
  const store = new Store();
  store.addQuad('http://example.org/person1', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://xmlns.com/foaf/0.1/Person');
  store.addQuad('http://example.org/person1', 'http://example.org/unknown', 'value');
  
  const result = validateNamespaces(store);
  if (result.valid) throw new Error('Should not be valid due to unknown namespace');
  if (!result.usedVocabularies.includes('RDF')) throw new Error('Should include RDF vocabulary');
  if (!result.unknownNamespaces.includes('http://example.org/')) throw new Error('Should include unknown namespace');
});

// === generateTurtlePrefixes tests ===
test('generateTurtlePrefixes generates Turtle format', () => {
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', ex: 'http://example.org/' };
  const result = generateTurtlePrefixes(prefixes);
  if (!result.includes('@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .')) throw new Error('Missing RDF prefix');
  if (!result.includes('@prefix ex: <http://example.org/> .')) throw new Error('Missing ex prefix');
});

// === generateSPARQLPrefixes tests ===
test('generateSPARQLPrefixes generates SPARQL format', () => {
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', ex: 'http://example.org/' };
  const result = generateSPARQLPrefixes(prefixes);
  if (!result.includes('PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>')) throw new Error('Missing RDF prefix');
  if (!result.includes('PREFIX ex: <http://example.org/>')) throw new Error('Missing ex prefix');
});

// === createNamespace tests ===
test('createNamespace creates namespace function', () => {
  const ns = createNamespace('http://example.org/');
  const iri = ns('test');
  if (iri !== 'http://example.org/test') throw new Error('Wrong namespace IRI');
});

// === expandCurie tests ===
test('expandCurie expands CURIE', () => {
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', ex: 'http://example.org/' };
  const expanded = expandCurie('rdf:type', prefixes);
  if (expanded !== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') throw new Error('Wrong expansion');
});

test('expandCurie returns original for non-CURIE', () => {
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#' };
  const expanded = expandCurie('http://example.org/test', prefixes);
  if (expanded !== 'http://example.org/test') throw new Error('Should return original');
});

test('expandCurie returns original for unknown prefix', () => {
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#' };
  const expanded = expandCurie('unknown:type', prefixes);
  if (expanded !== 'unknown:type') throw new Error('Should return original');
});

// === shrinkIri tests ===
test('shrinkIri shrinks IRI to CURIE', () => {
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', ex: 'http://example.org/' };
  const shrunk = shrinkIri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', prefixes);
  if (shrunk !== 'rdf:type') throw new Error('Wrong shrinkage');
});

test('shrinkIri returns original for no match', () => {
  const prefixes = { rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#' };
  const shrunk = shrinkIri('http://example.org/unknown', prefixes);
  if (shrunk !== 'http://example.org/unknown') throw new Error('Should return original');
});

// === Summary ===
console.log(`\n📊 Results: ${testsPassed}/${testsTotal} tests passed`);

if (testsPassed === testsTotal) {
  console.log('🎉 All namespace-utils tests passed!');
  process.exit(0);
} else {
  console.log('❌ Some namespace-utils tests failed');
  process.exit(1);
}

