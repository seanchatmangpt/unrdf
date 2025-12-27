/**
 * @file Property-Based Test Generators using fast-check
 * @module test/narrative-state-chain/fixtures/generators
 * @description Generators for property-based testing with fast-check
 */

import fc from 'fast-check';

/**
 * Generate arbitrary RDF quad
 * @returns {fc.Arbitrary<Object>} Arbitrary RDF quad
 */
export const quadArbitrary = fc.record({
  subject: fc.record({
    termType: fc.constant('NamedNode'),
    value: fc.webUrl()
  }),
  predicate: fc.record({
    termType: fc.constant('NamedNode'),
    value: fc.webUrl()
  }),
  object: fc.oneof(
    fc.record({
      termType: fc.constant('Literal'),
      value: fc.string({ minLength: 1 })
    }),
    fc.record({
      termType: fc.constant('NamedNode'),
      value: fc.webUrl()
    })
  ),
  graph: fc.record({
    termType: fc.constant('DefaultGraph'),
    value: fc.constant('')
  })
});

/**
 * Generate arbitrary Observation
 * @returns {fc.Arbitrary<Object>} Arbitrary Observation
 */
export const observationArbitrary = fc.record({
  quad: quadArbitrary,
  source: fc.string({ minLength: 1, maxLength: 50 }),
  timestamp: fc.integer({ min: 0, max: Date.now() }),
  confidence: fc.float({ min: 0, max: 1 })
});

/**
 * Generate arbitrary Guard predicate result
 * @returns {fc.Arbitrary<Object>} Arbitrary GuardResult
 */
export const guardResultArbitrary = fc.record({
  allowed: fc.boolean(),
  denyReason: fc.option(fc.string({ minLength: 1 })),
  guardId: fc.string({ minLength: 1 }),
  timestamp: fc.integer({ min: 0, max: Date.now() })
});

/**
 * Generate arbitrary Delta
 * @returns {fc.Arbitrary<Object>} Arbitrary Delta
 */
export const deltaArbitrary = fc.record({
  additions: fc.array(quadArbitrary, { minLength: 0, maxLength: 100 }),
  deletions: fc.array(quadArbitrary, { minLength: 0, maxLength: 100 }),
  hash: fc.string({ minLength: 64, maxLength: 64 }),
  proof: fc.record({
    algorithm: fc.constantFrom('set-cover', 'graph-diff', 'custom'),
    alternativeCount: fc.integer({ min: 0, max: 100 }),
    justification: fc.string()
  })
});

/**
 * Generate arbitrary Receipt
 * @returns {fc.Arbitrary<Object>} Arbitrary Receipt
 */
export const receiptArbitrary = fc.record({
  sceneId: fc.string({ minLength: 64, maxLength: 64 }),
  timestamp: fc.integer({ min: 0, max: Date.now() }),
  admissibilityChecks: fc.array(guardResultArbitrary, { minLength: 1, maxLength: 10 }),
  minimalityProof: fc.record({
    algorithm: fc.constantFrom('set-cover', 'graph-diff', 'custom'),
    alternativeCount: fc.integer({ min: 0, max: 50 }),
    justification: fc.string()
  }),
  forkParents: fc.array(fc.string({ minLength: 64, maxLength: 64 }), { maxLength: 5 }),
  signature: fc.record({
    algorithm: fc.constantFrom('ed25519', 'secp256k1', 'bls12-381'),
    publicKey: fc.string({ minLength: 20 }),
    signature: fc.string({ minLength: 40 }),
    signerId: fc.string({ minLength: 1 })
  }),
  receiptHash: fc.string({ minLength: 64, maxLength: 64 })
});

/**
 * Generate arbitrary SceneEnvelope
 * @returns {fc.Arbitrary<Object>} Arbitrary SceneEnvelope
 */
export const sceneEnvelopeArbitrary = fc.record({
  id: fc.string({ minLength: 64, maxLength: 64 }),
  universeId: fc.string({ minLength: 64, maxLength: 64 }),
  observations: fc.array(observationArbitrary, { minLength: 1, maxLength: 100 }),
  delta: deltaArbitrary,
  consequences: fc.record({
    status: fc.constantFrom('accepted', 'rejected', 'deferred'),
    resultingGraph: fc.constant(new Map()),
    invariantChecks: fc.array(fc.record({
      invariantId: fc.string({ minLength: 1 }),
      satisfied: fc.boolean(),
      query: fc.string(),
      bindings: fc.record({})
    }), { maxLength: 10 }),
    sideEffectTokens: fc.array(fc.string(), { maxLength: 10 })
  }),
  artifacts: fc.array(fc.record({
    id: fc.string({ minLength: 64, maxLength: 64 }),
    type: fc.string({ minLength: 1 }),
    data: fc.string(),
    generatedBy: fc.string(),
    dependencies: fc.array(fc.string(), { maxLength: 5 })
  }), { maxLength: 10 }),
  receipt: receiptArbitrary,
  metadata: fc.record({
    created: fc.integer({ min: 0, max: Date.now() }),
    author: fc.string({ minLength: 1 }),
    sequenceNumber: fc.integer({ min: 1, max: 10000 }),
    tags: fc.array(fc.string(), { maxLength: 5 })
  })
});

/**
 * Generate arbitrary BridgeProof
 * @returns {fc.Arbitrary<Object>} Arbitrary BridgeProof
 */
export const bridgeProofArbitrary = fc.record({
  id: fc.string({ minLength: 64, maxLength: 64 }),
  sourceUniverseId: fc.string({ minLength: 64, maxLength: 64 }),
  targetUniverseId: fc.string({ minLength: 64, maxLength: 64 }),
  typeCoercion: fc.record({
    classMapping: fc.constant(new Map()),
    propertyMapping: fc.constant(new Map()),
    transforms: fc.array(fc.anything(), { maxLength: 5 }),
    defaultNamespace: fc.webUrl()
  }),
  invariantPreservation: fc.array(fc.record({
    sourceInvariantId: fc.string({ minLength: 1 }),
    targetInvariantId: fc.string({ minLength: 1 }),
    proofType: fc.constantFrom('logical-implication', 'constraint-relaxation', 'manual-verification'),
    proof: fc.string()
  }), { maxLength: 5 }),
  accessGrants: fc.array(fc.record({
    guardId: fc.string({ minLength: 1 }),
    reason: fc.string(),
    grantor: fc.string({ minLength: 1 }),
    expiresAt: fc.option(fc.integer({ min: Date.now(), max: Date.now() + 86400000 }))
  }), { maxLength: 5 }),
  validity: fc.record({
    sourceSignature: fc.record({
      algorithm: fc.constantFrom('ed25519', 'secp256k1', 'bls12-381'),
      publicKey: fc.string({ minLength: 20 }),
      signature: fc.string({ minLength: 40 }),
      signerId: fc.string({ minLength: 1 })
    }),
    targetSignature: fc.record({
      algorithm: fc.constantFrom('ed25519', 'secp256k1', 'bls12-381'),
      publicKey: fc.string({ minLength: 20 }),
      signature: fc.string({ minLength: 40 }),
      signerId: fc.string({ minLength: 1 })
    }),
    witnessSignatures: fc.array(fc.record({
      algorithm: fc.constantFrom('ed25519', 'secp256k1', 'bls12-381'),
      publicKey: fc.string({ minLength: 20 }),
      signature: fc.string({ minLength: 40 }),
      signerId: fc.string({ minLength: 1 })
    }), { maxLength: 3 }),
    merkleRoot: fc.string({ minLength: 64, maxLength: 64 })
  }),
  metadata: fc.record({
    created: fc.integer({ min: 0, max: Date.now() }),
    creator: fc.string({ minLength: 1 }),
    bidirectional: fc.boolean(),
    usageCount: fc.integer({ min: 0, max: 1000 })
  })
});

/**
 * Constraint: Two observations are "commutative" if their order doesn't matter
 * Generator for testing commutativity properties
 * @returns {fc.Arbitrary<Array>} Pair of observations
 */
export const commutativeObservationPairArbitrary = fc.tuple(
  observationArbitrary,
  observationArbitrary
).filter(([obs1, obs2]) => {
  // Observations are commutative if they affect different subjects
  return obs1.quad.subject.value !== obs2.quad.subject.value;
});

/**
 * Generate deterministic RDF content for reproducibility testing
 * @param {number} seed - Seed value
 * @returns {Object} Deterministic test data
 */
export function generateDeterministicData(seed) {
  const rng = fc.Random.createRandom(Buffer.alloc(4, seed));
  const quads = fc.sample(quadArbitrary, 10, { randomModule: rng });
  const observations = fc.sample(observationArbitrary, 5, { randomModule: rng });

  return {
    quads,
    observations,
    seed
  };
}

/**
 * Generate a sequence of scenes that form a chain
 * @param {number} length - Number of scenes in chain
 * @returns {fc.Arbitrary<Array>} Scene chain
 */
export const sceneChainArbitrary = fc.array(
  sceneEnvelopeArbitrary,
  { minLength: 1, maxLength: 20 }
).map(scenes => {
  // Link scenes by setting forkParents
  return scenes.map((scene, i) => ({
    ...scene,
    metadata: {
      ...scene.metadata,
      sequenceNumber: i + 1
    },
    receipt: {
      ...scene.receipt,
      forkParents: i > 0 ? [scenes[i - 1].receipt.sceneId] : []
    }
  }));
});
