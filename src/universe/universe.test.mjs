/**
 * @file universe.test.mjs
 * @description Test suite for RDF Universe implementation
 */

import { strict as assert } from 'assert';
import {
  validateIri,
  loadTurtle,
  parseQuads,
  computeContentHash,
  extractNamespaces,
  canonicalizeQuads,
} from './rdf-utils.mjs';
import { OntologyRelease, AllowedOntology } from './ontology-release.mjs';
import { OntologyRegistry } from './registry.mjs';
import {
  Partition,
  IndustrialSubstrate,
  CorporateCanon,
  BusinessUnitOverlay,
  RegionalOverlay,
  ExecutionLedger,
  SystemPolicyPartition,
} from './partition.mjs';
import { Universe } from './universe.mjs';

/**
 * Test RDF Utils
 */
async function testRdfUtils() {
  console.log('Testing RDF Utils...');

  // Test IRI validation
  assert.strictEqual(validateIri('http://example.org/'), true, 'Valid HTTP IRI');
  assert.strictEqual(validateIri('https://example.org/'), true, 'Valid HTTPS IRI');
  assert.strictEqual(validateIri('urn:uuid:123'), true, 'Valid URN IRI');
  assert.strictEqual(validateIri('not-a-valid-iri'), false, 'Invalid IRI');

  // Test Turtle loading
  const turtleContent = `
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix ex: <http://example.org/> .

    ex:alice foaf:name "Alice" .
    ex:alice foaf:age 30 .
    ex:bob foaf:name "Bob" .
  `;

  const store = loadTurtle(turtleContent);
  assert.strictEqual(store.size, 3, 'Loaded 3 triples');

  // Test quad parsing
  const quads = parseQuads(turtleContent);
  assert.strictEqual(quads.length, 3, 'Parsed 3 quads');

  // Test canonicalization
  const sorted = canonicalizeQuads(quads);
  assert.strictEqual(sorted.length, 3, 'Canonicalized 3 quads');

  // Test content hashing
  const hash1 = await computeContentHash(quads);
  assert.strictEqual(typeof hash1, 'string', 'Hash is a string');
  assert.strictEqual(hash1.length, 64, 'SHA256 hash is 64 hex chars');

  // Test deterministic hashing
  const hash2 = await computeContentHash(quads);
  assert.strictEqual(hash1, hash2, 'Hashing is deterministic');

  // Test namespace extraction
  const namespaces = extractNamespaces(quads);
  assert.ok(namespaces.has('http://xmlns.com/foaf/0.1/'), 'Extracted FOAF namespace');
  assert.ok(namespaces.has('http://example.org/'), 'Extracted example namespace');

  console.log('✅ RDF Utils tests passed');
}

/**
 * Test Ontology Release and AllowedOntology
 */
async function testOntologyClasses() {
  console.log('Testing Ontology Classes...');

  // Test OntologyRelease
  const release = new OntologyRelease({
    namespaceIri: 'http://www.w3.org/ns/prov#',
    version: '2013-04-30',
    contentHash: '1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
  });

  assert.strictEqual(release.namespaceIri, 'http://www.w3.org/ns/prov#');
  assert.strictEqual(release.version, '2013-04-30');
  assert.strictEqual(
    release.getReleaseId(),
    'http://www.w3.org/ns/prov#@2013-04-30'
  );

  // Test AllowedOntology
  const ontology = new AllowedOntology({
    namespaceIri: 'http://www.w3.org/ns/prov#',
    name: 'PROV-O',
    description: 'W3C Provenance Ontology',
    releases: [release],
  });

  assert.strictEqual(ontology.name, 'PROV-O');
  assert.strictEqual(ontology.releases.length, 1);
  assert.strictEqual(ontology.getLatestRelease(), release);
  assert.strictEqual(ontology.getRelease('2013-04-30'), release);

  // Test JSON serialization
  const json = ontology.toJSON();
  const restored = AllowedOntology.fromJSON(json);
  assert.strictEqual(restored.name, ontology.name);
  assert.strictEqual(restored.releases.length, ontology.releases.length);

  console.log('✅ Ontology Classes tests passed');
}

/**
 * Test OntologyRegistry
 */
async function testOntologyRegistry() {
  console.log('Testing OntologyRegistry...');

  const registry = new OntologyRegistry();

  const provRelease = new OntologyRelease({
    namespaceIri: 'http://www.w3.org/ns/prov#',
    version: '2013-04-30',
    contentHash: '1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
  });

  const provOntology = new AllowedOntology({
    namespaceIri: 'http://www.w3.org/ns/prov#',
    name: 'PROV-O',
    releases: [provRelease],
  });

  // Test registration
  registry.register(provOntology);
  assert.strictEqual(registry.size, 1, 'Registry has 1 ontology');

  // Test lookup by namespace
  const found = registry.getByNamespace('http://www.w3.org/ns/prov#');
  assert.strictEqual(found?.name, 'PROV-O');

  // Test lookup by hash
  const foundByHash = registry.getByHash(
    '1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef'
  );
  assert.strictEqual(foundByHash?.namespaceIri, 'http://www.w3.org/ns/prov#');

  // Test allow-listing
  assert.strictEqual(
    registry.isNamespaceAllowed('http://www.w3.org/ns/prov#'),
    true
  );
  assert.strictEqual(
    registry.isNamespaceAllowed('http://not-allowed.org/'),
    false
  );

  // Test standard registry
  const standardRegistry = OntologyRegistry.createStandardRegistry();
  assert.strictEqual(standardRegistry.size, 7, 'Standard registry has 7 ontologies');

  const namespaces = standardRegistry.getAllNamespaces();
  assert.ok(namespaces.includes('http://www.w3.org/ns/prov#'), 'PROV-O included');
  assert.ok(namespaces.includes('http://www.w3.org/ns/odrl/2/'), 'ODRL included');
  assert.ok(
    namespaces.includes('http://www.w3.org/2004/02/skos/core#'),
    'SKOS included'
  );
  assert.ok(
    namespaces.includes('http://www.w3.org/2006/time#'),
    'OWL-Time included'
  );
  assert.ok(namespaces.includes('http://www.w3.org/ns/dcat#'), 'DCAT included');
  assert.ok(namespaces.includes('http://www.w3.org/ns/org#'), 'ORG included');
  assert.ok(namespaces.includes('http://www.w3.org/ns/oa#'), 'OA included');

  console.log('✅ OntologyRegistry tests passed');
}

/**
 * Test Partition classes
 */
async function testPartitions() {
  console.log('Testing Partitions...');

  // Test abstract Partition class
  try {
    new Partition({ name: 'Test' });
    assert.fail('Should not be able to instantiate abstract Partition');
  } catch (err) {
    assert.ok(err.message.includes('abstract class'));
  }

  // Test IndustrialSubstrate
  const industrial = new IndustrialSubstrate();
  assert.strictEqual(industrial.name, 'IndustrialSubstrate');
  assert.strictEqual(industrial.readOnly, true);
  assert.strictEqual(industrial.namespaceIris.length, 7);
  assert.strictEqual(industrial.protectedNamespaces.length, 7);

  // Test CorporateCanon
  const corporate = new CorporateCanon();
  assert.strictEqual(corporate.name, 'CorporateCanon');
  assert.strictEqual(corporate.readOnly, true);

  // Test BusinessUnitOverlay
  const businessUnit = new BusinessUnitOverlay();
  assert.strictEqual(businessUnit.name, 'BusinessUnitOverlay');
  assert.strictEqual(businessUnit.readOnly, false);

  // Test RegionalOverlay
  const regional = new RegionalOverlay();
  assert.strictEqual(regional.name, 'RegionalOverlay');
  assert.strictEqual(regional.readOnly, false);

  // Test ExecutionLedger
  const execution = new ExecutionLedger();
  assert.strictEqual(execution.name, 'ExecutionLedger');
  assert.strictEqual(execution.readOnly, false);

  // Test SystemPolicyPartition
  const policy = new SystemPolicyPartition();
  assert.strictEqual(policy.name, 'SystemPolicyPartition');
  assert.strictEqual(policy.readOnly, true);

  console.log('✅ Partition tests passed');
}

/**
 * Test Universe
 */
async function testUniverse() {
  console.log('Testing Universe...');

  const universe = Universe.createStandard();

  // Test partition access
  assert.strictEqual(universe.getAllPartitions().length, 6, 'Universe has 6 partitions');

  const industrial = universe.getPartition('IndustrialSubstrate');
  assert.ok(industrial, 'Found IndustrialSubstrate');
  assert.strictEqual(industrial.readOnly, true);

  // Test partition names
  const names = universe.getPartitionNames();
  assert.deepStrictEqual(
    names,
    [
      'IndustrialSubstrate',
      'CorporateCanon',
      'BusinessUnitOverlay',
      'RegionalOverlay',
      'ExecutionLedger',
      'SystemPolicyPartition',
    ],
    'Correct partition order'
  );

  // Test registry
  assert.strictEqual(universe.registry.size, 7, 'Registry has 7 ontologies');

  // Test validation
  const validation = await universe.validateIndustrialSubstrate();
  assert.strictEqual(validation.valid, true, 'IndustrialSubstrate is valid');
  assert.strictEqual(validation.namespaceCount, 7, 'Has 7 namespaces');

  // Test partition sizes
  const sizes = universe.getPartitionSizes();
  assert.strictEqual(typeof sizes.IndustrialSubstrate, 'number');
  assert.strictEqual(typeof sizes.CorporateCanon, 'number');

  // Test merge (with empty partitions)
  const merged = universe.merge();
  assert.ok(merged, 'Merged store created');

  console.log('✅ Universe tests passed');
}

/**
 * Test integration scenarios
 */
async function testIntegration() {
  console.log('Testing Integration Scenarios...');

  const universe = Universe.createStandard();

  // Test loading sample content into BusinessUnitOverlay
  const businessUnit = universe.getPartition('BusinessUnitOverlay');
  assert.ok(businessUnit, 'Found BusinessUnitOverlay');

  const sampleTurtle = `
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .

    ex:department1 a ex:Department ;
      ex:name "Engineering" .
  `;

  businessUnit.loadTurtle(sampleTurtle);
  assert.strictEqual(businessUnit.size, 2, 'Loaded 2 triples into BusinessUnitOverlay');

  // Test content hash computation
  const hash = await businessUnit.getContentHash();
  assert.strictEqual(typeof hash, 'string', 'Hash computed');
  assert.strictEqual(hash.length, 64, 'Hash is SHA256');

  // Test merge includes BusinessUnitOverlay data
  const merged = universe.merge();
  assert.ok(merged.size >= 2, 'Merged store includes BusinessUnitOverlay data');

  // Test query across partitions
  const results = merged.query(`
    PREFIX ex: <http://example.org/>
    SELECT ?dept WHERE {
      ?dept a ex:Department .
    }
  `);

  assert.ok(results, 'Query executed');

  console.log('✅ Integration tests passed');
}

/**
 * Main test runner
 */
async function runTests() {
  console.log('🚀 Starting RDF Universe Test Suite\n');

  try {
    await testRdfUtils();
    await testOntologyClasses();
    await testOntologyRegistry();
    await testPartitions();
    await testUniverse();
    await testIntegration();

    console.log('\n✅ All tests passed!');
    process.exit(0);
  } catch (error) {
    console.error('\n❌ Test failed:', error);
    process.exit(1);
  }
}

// Run tests
runTests();
