/**
 * @file Test Fixtures for Narrative State Chain Types
 * @module test/narrative-state-chain/fixtures/types
 * @description Factory functions and test data for all NSC types
 */

import crypto from 'crypto';

/**
 * Generate deterministic hash (sha256)
 * @param {string} content - Content to hash
 * @returns {string} SHA256 hash (hex)
 */
export function hashContent(content) {
  return crypto.createHash('sha256').update(content).digest('hex');
}

/**
 * Create a UniverseRecord fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} UniverseRecord
 */
export function createUniverseFixture(overrides = {}) {
  const schema = {
    requiredTypes: ['ex:Agent', 'ex:Action'],
    properties: {
      'ex:name': {
        range: 'xsd:string',
        minCardinality: 0,
        maxCardinality: 1,
        functional: true
      },
      'ex:age': {
        range: 'xsd:integer',
        minCardinality: 0,
        maxCardinality: 1,
        functional: true
      }
    },
    closedWorld: [],
    namespace: 'http://example.org/'
  };

  const metadata = {
    created: Date.now(),
    updated: Date.now(),
    parentUniverses: [],
    creator: 'did:key:test-creator',
    version: 1
  };

  const id = hashContent(JSON.stringify(schema));

  return {
    id,
    schema,
    reconcile: async (observations) => ({
      status: 'accepted',
      resultingGraph: new Map(),
      invariantChecks: [],
      sideEffectTokens: []
    }),
    invariants: {
      rules: [
        {
          id: 'inv-001',
          sparql: 'ASK { ?x a ex:Agent }',
          description: 'At least one agent must exist',
          severity: 'error'
        }
      ],
      enforcement: 'strict'
    },
    guards: new Map([
      ['guard-001', () => ({ allowed: true, guardId: 'guard-001', timestamp: Date.now() })]
    ]),
    metadata,
    ...overrides
  };
}

/**
 * Create an Observation fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} Observation
 */
export function createObservationFixture(overrides = {}) {
  return {
    quad: {
      subject: { termType: 'NamedNode', value: 'http://example.org/subject' },
      predicate: { termType: 'NamedNode', value: 'http://example.org/predicate' },
      object: { termType: 'Literal', value: 'test-value' },
      graph: { termType: 'DefaultGraph' }
    },
    source: 'agent:test',
    timestamp: Date.now(),
    confidence: 1.0,
    ...overrides
  };
}

/**
 * Create a Delta fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} Delta
 */
export function createDeltaFixture(overrides = {}) {
  const additions = [
    {
      subject: { termType: 'NamedNode', value: 'http://example.org/s1' },
      predicate: { termType: 'NamedNode', value: 'http://example.org/p1' },
      object: { termType: 'Literal', value: 'o1' },
      graph: { termType: 'DefaultGraph' }
    }
  ];

  const deletions = [];

  const canonicalContent = JSON.stringify({
    additions: sortQuads(additions),
    deletions: sortQuads(deletions)
  });

  return {
    additions,
    deletions,
    hash: hashContent(canonicalContent),
    proof: {
      algorithm: 'set-cover',
      alternativeCount: 3,
      justification: 'Minimal by graph diff'
    },
    ...overrides
  };
}

/**
 * Create a Consequence fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} Consequence
 */
export function createConsequenceFixture(overrides = {}) {
  return {
    status: 'accepted',
    resultingGraph: new Map(),
    invariantChecks: [
      {
        invariantId: 'inv-001',
        satisfied: true,
        query: 'ASK { ?x a ex:Agent }',
        bindings: {}
      }
    ],
    sideEffectTokens: [],
    ...overrides
  };
}

/**
 * Create a GuardResult fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} GuardResult
 */
export function createGuardResultFixture(overrides = {}) {
  return {
    allowed: true,
    denyReason: null,
    guardId: 'guard-001',
    timestamp: Date.now(),
    ...overrides
  };
}

/**
 * Create a Receipt fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} Receipt
 */
export function createReceiptFixture(overrides = {}) {
  const baseReceipt = {
    sceneId: hashContent(`scene-${Date.now()}`),
    timestamp: Date.now(),
    admissibilityChecks: [createGuardResultFixture()],
    minimalityProof: {
      algorithm: 'set-cover',
      alternativeCount: 2,
      justification: 'Minimal'
    },
    forkParents: [],
    signature: {
      algorithm: 'ed25519',
      publicKey: 'pk_test_' + Math.random().toString(36).substring(7),
      signature: 'sig_test_' + Math.random().toString(36).substring(7),
      signerId: 'did:key:test'
    }
  };

  const receiptHash = hashContent(JSON.stringify(baseReceipt));

  return {
    ...baseReceipt,
    receiptHash,
    ...overrides
  };
}

/**
 * Create a SceneEnvelope fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} SceneEnvelope
 */
export function createSceneEnvelopeFixture(overrides = {}) {
  const observations = [createObservationFixture()];
  const delta = createDeltaFixture();
  const universe = createUniverseFixture();

  return {
    id: hashContent(JSON.stringify({ observations, delta, universeId: universe.id })),
    universeId: universe.id,
    observations,
    delta,
    consequences: createConsequenceFixture(),
    artifacts: [],
    receipt: createReceiptFixture(),
    metadata: {
      created: Date.now(),
      author: 'did:key:test',
      sequenceNumber: 1,
      tags: []
    },
    ...overrides
  };
}

/**
 * Create a BridgeProof fixture
 * @param {Object} overrides - Field overrides
 * @returns {Object} BridgeProof
 */
export function createBridgeProofFixture(overrides = {}) {
  const sourceUniverseId = hashContent('universe-a');
  const targetUniverseId = hashContent('universe-b');

  return {
    id: hashContent(`bridge-${sourceUniverseId}-${targetUniverseId}`),
    sourceUniverseId,
    targetUniverseId,
    typeCoercion: {
      classMapping: new Map([
        ['ex:Person', 'foaf:Person'],
        ['ex:name', 'foaf:name']
      ]),
      propertyMapping: new Map([
        ['ex:age', 'foaf:age']
      ]),
      transforms: [],
      defaultNamespace: 'http://example.org/'
    },
    invariantPreservation: [],
    accessGrants: [],
    validity: {
      sourceSignature: {
        algorithm: 'ed25519',
        publicKey: 'pk_src',
        signature: 'sig_src',
        signerId: 'did:key:src'
      },
      targetSignature: {
        algorithm: 'ed25519',
        publicKey: 'pk_tgt',
        signature: 'sig_tgt',
        signerId: 'did:key:tgt'
      },
      witnessSignatures: [],
      merkleRoot: hashContent('bridge-merkle')
    },
    metadata: {
      created: Date.now(),
      creator: 'did:key:bridge-creator',
      bidirectional: false,
      usageCount: 0
    },
    ...overrides
  };
}

/**
 * Helper: Sort quads for deterministic hashing
 * @param {Array} quads - Quads to sort
 * @returns {Array} Sorted quads
 */
function sortQuads(quads) {
  return [...quads].sort((a, b) => {
    const aStr = JSON.stringify(a);
    const bStr = JSON.stringify(b);
    return aStr.localeCompare(bStr);
  });
}

/**
 * Create test RDF triples
 * @param {number} count - Number of triples to generate
 * @returns {Array} Array of RDF quads
 */
export function generateQuads(count) {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push({
      subject: { termType: 'NamedNode', value: `http://example.org/s${i}` },
      predicate: { termType: 'NamedNode', value: `http://example.org/p${i % 5}` },
      object: { termType: 'Literal', value: `value-${i}` },
      graph: { termType: 'DefaultGraph' }
    });
  }
  return quads;
}

/**
 * Create a batch of scenes for integration testing
 * @param {number} count - Number of scenes
 * @param {string} universeId - Universe ID
 * @returns {Array} Array of SceneEnvelope fixtures
 */
export function generateScenes(count, universeId) {
  const scenes = [];
  for (let i = 0; i < count; i++) {
    scenes.push(
      createSceneEnvelopeFixture({
        universeId,
        metadata: {
          created: Date.now() + i * 1000,
          author: `did:key:author${i % 3}`,
          sequenceNumber: i + 1,
          tags: ['test']
        }
      })
    );
  }
  return scenes;
}
