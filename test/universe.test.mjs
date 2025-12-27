/**
 * @file Universe Tests
 * @description Tests for Universe, Partition, and OntologyRelease
 */

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import { Universe, Partition, OntologyRelease } from '../src/admission/universe.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const fixturesDir = join(__dirname, 'fixtures');

describe('Universe Tests', () => {
  test('[TEST] Universe - Load provided TTL ontology and verify 6 partitions created', async () => {
    console.log('[START] Loading TTL ontology');
    const ttlPath = join(fixturesDir, 'test-universe.ttl');
    const ttlContent = readFileSync(ttlPath, 'utf-8');

    const universe = new Universe();
    await universe.loadFromTTL(ttlContent);

    console.log('[ASSERT] Universe loaded');
    const partitions = universe.getPartitions();
    console.log(`[ASSERT] Partition count: ${partitions.length}`);
    assert.equal(partitions.length, 6, 'Should have 6 partitions');
    console.log('[RESULT] pass');
  });

  test('[TEST] Universe - Verify IndustrialSubstrate is read-only', async () => {
    console.log('[START] Checking IndustrialSubstrate read-only status');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const substrate = universe.getPartition('IndustrialSubstrate');
    console.log('[ASSERT] IndustrialSubstrate partition exists');
    assert.ok(substrate, 'IndustrialSubstrate partition should exist');

    console.log('[ASSERT] Checking read-only flag');
    assert.equal(substrate.isReadOnlyPartition(), true, 'IndustrialSubstrate should be read-only');
    console.log('[RESULT] pass');
  });

  test('[TEST] Universe - Verify protected namespaces in SystemPolicyPartition (≥7 namespaces)', async () => {
    console.log('[START] Checking protected namespaces');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const policyPartition = universe.getPartition('SystemPolicyPartition');
    console.log('[ASSERT] SystemPolicyPartition exists');
    assert.ok(policyPartition, 'SystemPolicyPartition should exist');

    const protectedNS = policyPartition.getProtectedNamespaces();
    console.log(`[ASSERT] Protected namespace count: ${protectedNS.size}`);
    assert.ok(protectedNS.size >= 7, 'Should have at least 7 protected namespaces');

    console.log('[ASSERT] Verifying standard namespaces present');
    assert.ok(protectedNS.has('http://www.w3.org/1999/02/22-rdf-syntax-ns#'), 'Should have RDF namespace');
    assert.ok(protectedNS.has('http://www.w3.org/2000/01/rdf-schema#'), 'Should have RDFS namespace');
    assert.ok(protectedNS.has('http://www.w3.org/2002/07/owl#'), 'Should have OWL namespace');
    console.log('[RESULT] pass');
  });

  test('[TEST] Universe - Verify 7 allowed ontologies registered with correct IRIs', async () => {
    console.log('[START] Checking allowed ontologies');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const policyPartition = universe.getPartition('SystemPolicyPartition');
    const ontologies = policyPartition.getOntologies();

    console.log(`[ASSERT] Ontology count: ${ontologies.length}`);
    assert.equal(ontologies.length, 7, 'Should have exactly 7 allowed ontologies');

    console.log('[ASSERT] Verifying ontology IRIs');
    const iris = ontologies.map(o => o.getIRI());
    assert.ok(iris.includes('http://www.w3.org/1999/02/22-rdf-syntax-ns#'), 'Should include RDF');
    assert.ok(iris.includes('http://www.w3.org/2000/01/rdf-schema#'), 'Should include RDFS');
    assert.ok(iris.includes('http://www.w3.org/2002/07/owl#'), 'Should include OWL');
    assert.ok(iris.includes('http://www.w3.org/2001/XMLSchema#'), 'Should include XSD');
    assert.ok(iris.includes('http://www.w3.org/ns/shacl#'), 'Should include SHACL');
    assert.ok(iris.includes('http://purl.org/dc/terms/'), 'Should include DC Terms');
    assert.ok(iris.includes('http://xmlns.com/foaf/0.1/'), 'Should include FOAF');
    console.log('[RESULT] pass');
  });

  test('[TEST] Universe - Verify content hash computation is deterministic', async () => {
    console.log('[START] Testing hash determinism');
    const ttlPath = join(fixturesDir, 'test-universe.ttl');
    const ttlContent = readFileSync(ttlPath, 'utf-8');

    const universe1 = new Universe();
    await universe1.loadFromTTL(ttlContent);
    const hash1 = universe1.getContentHash();

    const universe2 = new Universe();
    await universe2.loadFromTTL(ttlContent);
    const hash2 = universe2.getContentHash();

    console.log(`[ASSERT] Hash 1: ${hash1?.slice(0, 16)}...`);
    console.log(`[ASSERT] Hash 2: ${hash2?.slice(0, 16)}...`);
    assert.equal(hash1, hash2, 'Content hashes should be deterministic');
    console.log('[RESULT] pass');
  });

  test('[TEST] Universe - Verify partition IRIs are distinct and valid', async () => {
    console.log('[START] Verifying partition IRI distinctness');
    const universe = new Universe();
    await universe.loadFromTTL('');

    console.log('[ASSERT] Checking distinct IRIs');
    const isDistinct = universe.verifyDistinctPartitionIRIs();
    assert.equal(isDistinct, true, 'All partition IRIs should be distinct');

    console.log('[ASSERT] Verifying IRI format');
    const partitions = universe.getPartitions();
    for (const partition of partitions) {
      const iri = partition.getIRI();
      assert.ok(iri.startsWith('https://'), `IRI should be HTTPS: ${iri}`);
      assert.ok(iri.includes('unrdf.org'), `IRI should include unrdf.org: ${iri}`);
    }
    console.log('[RESULT] pass');
  });

  test('[TEST] OntologyRelease - Create and validate ontology release', () => {
    console.log('[START] Creating ontology release');
    const ontology = new OntologyRelease({
      iri: 'http://example.org/ontology#',
      version: '1.0.0',
      namespace: 'http://example.org/ontology#',
      isProtected: true,
    });

    console.log('[ASSERT] Ontology created');
    assert.equal(ontology.getIRI(), 'http://example.org/ontology#');
    assert.equal(ontology.getNamespace(), 'http://example.org/ontology#');
    assert.equal(ontology.isProtectedNamespace(), true);
    console.log('[RESULT] pass');
  });

  test('[TEST] Partition - Add ontology and verify protected namespaces', () => {
    console.log('[START] Creating partition');
    const partition = new Partition({
      name: 'TestPartition',
      type: 'ApplicationOverlay',
      iri: 'https://test.org/partition',
      isReadOnly: false,
    });

    console.log('[ASSERT] Adding protected ontology');
    const ontology = new OntologyRelease({
      iri: 'http://test.org/ontology#',
      version: '1.0.0',
      namespace: 'http://test.org/ontology#',
      isProtected: true,
    });

    partition.addOntology(ontology);

    const protectedNS = partition.getProtectedNamespaces();
    console.log('[ASSERT] Protected namespace added');
    assert.equal(protectedNS.size, 1);
    assert.ok(protectedNS.has('http://test.org/ontology#'));
    console.log('[RESULT] pass');
  });

  test('[TEST] Universe - Verify all partition types are distinct', async () => {
    console.log('[START] Verifying partition type uniqueness');
    const universe = new Universe();
    await universe.loadFromTTL('');

    const partitions = universe.getPartitions();
    const types = new Set(partitions.map(p => p.type));

    console.log(`[ASSERT] Partition types: ${types.size}`);
    assert.equal(types.size, partitions.length, 'All partition types should be unique');

    const expectedTypes = [
      'IndustrialSubstrate',
      'SystemPolicyPartition',
      'StudiosOverlay',
      'ApplicationOverlay',
      'TemporalOverlay',
      'ProjectionOverlay',
    ];

    for (const expectedType of expectedTypes) {
      assert.ok(types.has(expectedType), `Should have ${expectedType}`);
    }
    console.log('[RESULT] pass');
  });

  test('[TEST] Universe - Load empty content successfully', async () => {
    console.log('[START] Loading empty content');
    const universe = new Universe();
    await universe.loadFromTTL('');

    console.log('[ASSERT] Universe created with default partitions');
    const partitions = universe.getPartitions();
    assert.equal(partitions.length, 6);
    console.log('[RESULT] pass');
  });
});

console.log('\n=== Universe Test Suite Complete ===');
