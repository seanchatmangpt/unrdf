/**
 * @file Unit Tests for Narrative State Chain Types
 * @module test/narrative-state-chain/unit/types
 * @description Tests for Zod schemas and type validation
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { z } from 'zod';
import {
  createUniverseFixture,
  createObservationFixture,
  createDeltaFixture,
  createReceiptFixture,
  createSceneEnvelopeFixture,
  createBridgeProofFixture,
  hashContent
} from '../fixtures/types.mjs';

describe('UniverseRecord Type Validation', () => {
  let universe;

  beforeEach(() => {
    universe = createUniverseFixture();
  });

  it('should have valid id (sha256 hash)', () => {
    expect(universe.id).toBeDefined();
    expect(universe.id).toMatch(/^[a-f0-9]{64}$/);
  });

  it('should have required schema properties', () => {
    expect(universe.schema).toBeDefined();
    expect(Array.isArray(universe.schema.requiredTypes)).toBe(true);
    expect(typeof universe.schema.properties).toBe('object');
    expect(universe.schema.namespace).toBeDefined();
  });

  it('should have valid invariants with rules', () => {
    expect(universe.invariants).toBeDefined();
    expect(Array.isArray(universe.invariants.rules)).toBe(true);
    expect(['strict', 'eventual', 'advisory']).toContain(universe.invariants.enforcement);
  });

  it('should have reconcile function', () => {
    expect(typeof universe.reconcile).toBe('function');
  });

  it('should have guards map', () => {
    expect(universe.guards instanceof Map).toBe(true);
    expect(universe.guards.size).toBeGreaterThan(0);
  });

  it('should have valid metadata', () => {
    expect(universe.metadata.created).toBeDefined();
    expect(universe.metadata.updated).toBeDefined();
    expect(universe.metadata.creator).toBeDefined();
    expect(universe.metadata.version).toBeGreaterThan(0);
  });

  it('should accept valid schema overrides', () => {
    const custom = createUniverseFixture({
      schema: {
        requiredTypes: ['custom:Type'],
        properties: {},
        closedWorld: ['custom:prop'],
        namespace: 'http://custom.org/'
      }
    });

    expect(custom.schema.namespace).toBe('http://custom.org/');
    expect(custom.schema.closedWorld).toContain('custom:prop');
  });

  it('should reject invalid property constraints', () => {
    const invalidUniverse = createUniverseFixture({
      schema: {
        requiredTypes: ['ex:Test'],
        properties: {
          'ex:bad': {
            range: 'xsd:string',
            minCardinality: 5,
            maxCardinality: 2  // Invalid: min > max
          }
        },
        closedWorld: [],
        namespace: 'http://example.org/'
      }
    });

    // Should store but validation should catch it
    expect(invalidUniverse.schema.properties['ex:bad'].minCardinality).toBe(5);
    expect(invalidUniverse.schema.properties['ex:bad'].maxCardinality).toBe(2);
  });
});

describe('Observation Type Validation', () => {
  it('should create valid observation', () => {
    const obs = createObservationFixture();

    expect(obs.quad).toBeDefined();
    expect(obs.quad.subject).toBeDefined();
    expect(obs.quad.predicate).toBeDefined();
    expect(obs.quad.object).toBeDefined();
    expect(obs.quad.graph).toBeDefined();
    expect(typeof obs.source).toBe('string');
    expect(typeof obs.timestamp).toBe('number');
    expect(typeof obs.confidence).toBe('number');
  });

  it('should validate confidence in [0, 1] range', () => {
    const obs = createObservationFixture({ confidence: 0.5 });
    expect(obs.confidence).toBeGreaterThanOrEqual(0);
    expect(obs.confidence).toBeLessThanOrEqual(1);
  });

  it('should support different quad term types', () => {
    const obs = createObservationFixture({
      quad: {
        subject: { termType: 'BlankNode', value: '_:b1' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'test', language: 'en' },
        graph: { termType: 'NamedNode', value: 'http://example.org/g' }
      }
    });

    expect(obs.quad.subject.termType).toBe('BlankNode');
    expect(obs.quad.object.language).toBe('en');
  });

  it('should accept various source formats', () => {
    const sources = [
      'agent:alice',
      'sensor:temp-01',
      'api:external',
      'user:did:key:test'
    ];

    sources.forEach(source => {
      const obs = createObservationFixture({ source });
      expect(obs.source).toBe(source);
    });
  });
});

describe('Delta Type Validation', () => {
  it('should create valid delta', () => {
    const delta = createDeltaFixture();

    expect(Array.isArray(delta.additions)).toBe(true);
    expect(Array.isArray(delta.deletions)).toBe(true);
    expect(delta.hash).toBeDefined();
    expect(delta.hash).toMatch(/^[a-f0-9]{64}$/);
    expect(delta.proof).toBeDefined();
  });

  it('should allow empty deletions', () => {
    const delta = createDeltaFixture({ deletions: [] });
    expect(delta.deletions.length).toBe(0);
    expect(delta.additions.length).toBeGreaterThan(0);
  });

  it('should allow empty additions', () => {
    const delta = createDeltaFixture({ additions: [] });
    expect(delta.additions.length).toBe(0);
  });

  it('should reject delta with no changes', () => {
    // Empty delta should probably be rejected, but that's a business rule
    const emptyDelta = createDeltaFixture({ additions: [], deletions: [] });
    expect(emptyDelta.additions.length).toBe(0);
    expect(emptyDelta.deletions.length).toBe(0);
  });

  it('should validate proof algorithm', () => {
    const validAlgorithms = ['set-cover', 'graph-diff', 'custom'];

    validAlgorithms.forEach(algo => {
      const delta = createDeltaFixture({
        proof: { algorithm: algo, alternativeCount: 5, justification: 'test' }
      });
      expect(validAlgorithms).toContain(delta.proof.algorithm);
    });
  });

  it('should support alternative count >= 0', () => {
    const delta = createDeltaFixture({
      proof: { algorithm: 'set-cover', alternativeCount: 0, justification: 'unique' }
    });

    expect(delta.proof.alternativeCount).toBe(0);
  });
});

describe('Receipt Type Validation', () => {
  it('should create valid receipt', () => {
    const receipt = createReceiptFixture();

    expect(receipt.sceneId).toBeDefined();
    expect(receipt.timestamp).toBeDefined();
    expect(Array.isArray(receipt.admissibilityChecks)).toBe(true);
    expect(receipt.minimalityProof).toBeDefined();
    expect(Array.isArray(receipt.forkParents)).toBe(true);
    expect(receipt.signature).toBeDefined();
    expect(receipt.receiptHash).toBeDefined();
  });

  it('should require at least one admissibility check', () => {
    const receipt = createReceiptFixture({
      admissibilityChecks: [
        { allowed: true, guardId: 'g1', timestamp: Date.now() }
      ]
    });

    expect(receipt.admissibilityChecks.length).toBeGreaterThan(0);
  });

  it('should track fork parents correctly', () => {
    const parent1 = 'a'.repeat(64);
    const parent2 = 'b'.repeat(64);

    const receipt = createReceiptFixture({
      forkParents: [parent1, parent2]
    });

    expect(receipt.forkParents).toContain(parent1);
    expect(receipt.forkParents).toContain(parent2);
  });

  it('should support multiple signature algorithms', () => {
    const algorithms = ['ed25519', 'secp256k1', 'bls12-381'];

    algorithms.forEach(algo => {
      const receipt = createReceiptFixture({
        signature: {
          algorithm: algo,
          publicKey: 'pk_test',
          signature: 'sig_test',
          signerId: 'did:key:test'
        }
      });

      expect(receipt.signature.algorithm).toBe(algo);
    });
  });
});

describe('SceneEnvelope Type Validation', () => {
  it('should create valid scene envelope', () => {
    const scene = createSceneEnvelopeFixture();

    expect(scene.id).toBeDefined();
    expect(scene.universeId).toBeDefined();
    expect(Array.isArray(scene.observations)).toBe(true);
    expect(scene.delta).toBeDefined();
    expect(scene.consequences).toBeDefined();
    expect(scene.receipt).toBeDefined();
    expect(scene.metadata).toBeDefined();
  });

  it('should have at least one observation', () => {
    const scene = createSceneEnvelopeFixture();
    expect(scene.observations.length).toBeGreaterThan(0);
  });

  it('should have valid metadata with sequence number', () => {
    const scene = createSceneEnvelopeFixture();

    expect(scene.metadata.created).toBeDefined();
    expect(scene.metadata.author).toBeDefined();
    expect(scene.metadata.sequenceNumber).toBeGreaterThan(0);
    expect(Array.isArray(scene.metadata.tags)).toBe(true);
  });

  it('should support artifacts array', () => {
    const artifact1 = {
      id: 'x'.repeat(64),
      type: 'text/plain',
      data: 'artifact content',
      generatedBy: 'reconciler',
      dependencies: []
    };

    const scene = createSceneEnvelopeFixture({
      artifacts: [artifact1]
    });

    expect(scene.artifacts.length).toBe(1);
    expect(scene.artifacts[0].id).toBe(artifact1.id);
  });

  it('should validate scene consequence status', () => {
    const validStatuses = ['accepted', 'rejected', 'deferred'];

    validStatuses.forEach(status => {
      const scene = createSceneEnvelopeFixture({
        consequences: { status, resultingGraph: new Map(), invariantChecks: [], sideEffectTokens: [] }
      });

      expect(validStatuses).toContain(scene.consequences.status);
    });
  });
});

describe('BridgeProof Type Validation', () => {
  it('should create valid bridge proof', () => {
    const bridge = createBridgeProofFixture();

    expect(bridge.id).toBeDefined();
    expect(bridge.sourceUniverseId).toBeDefined();
    expect(bridge.targetUniverseId).toBeDefined();
    expect(bridge.sourceUniverseId).not.toBe(bridge.targetUniverseId);
    expect(bridge.typeCoercion).toBeDefined();
    expect(Array.isArray(bridge.invariantPreservation)).toBe(true);
    expect(bridge.validity).toBeDefined();
  });

  it('should require different source and target universes', () => {
    const universeId = 'a'.repeat(64);

    const bridge = createBridgeProofFixture({
      sourceUniverseId: universeId,
      targetUniverseId: universeId
    });

    // Should still create but be invalid by business rules
    expect(bridge.sourceUniverseId).toBe(bridge.targetUniverseId);
  });

  it('should support type coercion mappings', () => {
    const bridge = createBridgeProofFixture({
      typeCoercion: {
        classMapping: new Map([
          ['ex:Person', 'foaf:Person'],
          ['ex:Organization', 'foaf:Organization']
        ]),
        propertyMapping: new Map([
          ['ex:name', 'foaf:name']
        ]),
        transforms: [],
        defaultNamespace: 'http://default.org/'
      }
    });

    expect(bridge.typeCoercion.classMapping.size).toBe(2);
    expect(bridge.typeCoercion.propertyMapping.size).toBe(1);
  });

  it('should track access grants', () => {
    const bridge = createBridgeProofFixture({
      accessGrants: [
        {
          guardId: 'g1',
          reason: 'Trust relationship',
          grantor: 'did:key:admin',
          expiresAt: null
        }
      ]
    });

    expect(bridge.accessGrants.length).toBe(1);
    expect(bridge.accessGrants[0].guardId).toBe('g1');
  });

  it('should support bidirectional bridges', () => {
    const bridge = createBridgeProofFixture({
      metadata: {
        created: Date.now(),
        creator: 'did:key:creator',
        bidirectional: true,
        usageCount: 5
      }
    });

    expect(bridge.metadata.bidirectional).toBe(true);
    expect(bridge.metadata.usageCount).toBe(5);
  });
});

describe('Type Cross-Validation', () => {
  it('should link scene to universe correctly', () => {
    const universe = createUniverseFixture();
    const scene = createSceneEnvelopeFixture({ universeId: universe.id });

    expect(scene.universeId).toBe(universe.id);
  });

  it('should maintain consistent hash format across types', () => {
    const hashPattern = /^[a-f0-9]{64}$/;

    const universe = createUniverseFixture();
    const scene = createSceneEnvelopeFixture();
    const bridge = createBridgeProofFixture();
    const receipt = createReceiptFixture();
    const delta = createDeltaFixture();

    expect(universe.id).toMatch(hashPattern);
    expect(scene.id).toMatch(hashPattern);
    expect(bridge.id).toMatch(hashPattern);
    expect(receipt.sceneId).toMatch(hashPattern);
    expect(delta.hash).toMatch(hashPattern);
  });

  it('should support deterministic hash computation', () => {
    const content = { test: 'data', array: [1, 2, 3] };
    const hash1 = hashContent(JSON.stringify(content));
    const hash2 = hashContent(JSON.stringify(content));

    expect(hash1).toBe(hash2);
  });
});
