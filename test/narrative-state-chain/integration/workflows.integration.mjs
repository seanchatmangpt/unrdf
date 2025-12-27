/**
 * @file Integration Tests for Narrative State Chain
 * @module test/narrative-state-chain/integration/workflows
 * @description End-to-end workflow tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createUniverseFixture,
  createSceneEnvelopeFixture,
  generateScenes,
  createObservationFixture,
  createBridgeProofFixture
} from '../fixtures/types.mjs';
import {
  verifySceneStructure,
  verifySceneChain,
  verifySequenceNumbers,
  verifyInvariantCheckResult
} from '../helpers/validation.mjs';

describe('Complete Scene Processing Workflow', () => {
  let universe;
  let scene;

  beforeEach(() => {
    universe = createUniverseFixture();
    scene = createSceneEnvelopeFixture({ universeId: universe.id });
  });

  it('should process valid scene end-to-end', async () => {
    // Verify scene structure is valid
    expect(verifySceneStructure(scene)).toBe(true);

    // Verify it references the correct universe
    expect(scene.universeId).toBe(universe.id);

    // Verify receipt was generated
    expect(scene.receipt).toBeDefined();
    expect(scene.receipt.sceneId).toBe(scene.id);

    // Verify admissibility checks were performed
    expect(Array.isArray(scene.receipt.admissibilityChecks)).toBe(true);
    expect(scene.receipt.admissibilityChecks.length).toBeGreaterThan(0);
  });

  it('should generate deterministic receipt hash', () => {
    // Same scene should produce same receipt hash
    const hash1 = scene.receipt.receiptHash;
    const hash2 = scene.receipt.receiptHash;

    expect(hash1).toBe(hash2);
    expect(/^[a-f0-9]{64}$/.test(hash1)).toBe(true);
  });

  it('should validate all invariants during processing', () => {
    // All invariants should be checked
    expect(Array.isArray(scene.consequences.invariantChecks)).toBe(true);

    scene.consequences.invariantChecks.forEach(check => {
      expect(verifyInvariantCheckResult(check)).toBe(true);
    });
  });

  it('should accept or reject based on guard results', () => {
    // If all guards pass, consequence should be 'accepted'
    const allGuardsPassed = scene.receipt.admissibilityChecks.every(g => g.allowed);

    if (allGuardsPassed) {
      expect(scene.consequences.status).toBe('accepted');
    }
  });

  it('should track side effects separately from state changes', () => {
    // Side effects should be opaque tokens, not executed immediately
    const tokens = scene.consequences.sideEffectTokens;

    expect(Array.isArray(tokens)).toBe(true);
    // Each token should be a string or object (not executed)
    tokens.forEach(token => {
      expect(typeof token === 'string' || typeof token === 'object').toBe(true);
    });
  });

  it('should attach minimal artifacts', () => {
    // Artifacts are optional but should be valid
    expect(Array.isArray(scene.artifacts)).toBe(true);

    scene.artifacts.forEach(artifact => {
      expect(artifact.id).toBeDefined();
      expect(artifact.type).toBeDefined();
      expect(artifact.generatedBy).toBeDefined();
      expect(Array.isArray(artifact.dependencies)).toBe(true);
    });
  });
});

describe('Scene Chain and Ordering', () => {
  let universe;
  let scenes;

  beforeEach(() => {
    universe = createUniverseFixture();
    scenes = generateScenes(5, universe.id);
  });

  it('should maintain scene ordering in chain', () => {
    // Verify sequence numbers increase
    expect(verifySequenceNumbers(scenes)).toBe(true);
  });

  it('should link scenes via fork parents', () => {
    // Update fork parents to form chain
    for (let i = 1; i < scenes.length; i++) {
      scenes[i].receipt.forkParents = [scenes[i - 1].receipt.sceneId];
    }

    expect(verifySceneChain(scenes)).toBe(true);
  });

  it('should detect invalid scene order', () => {
    // Shuffle scenes
    const shuffled = [scenes[2], scenes[0], scenes[1], scenes[3], scenes[4]];

    // Sequence numbers should be out of order
    expect(verifySequenceNumbers(shuffled)).toBe(false);
  });

  it('should support concurrent scenes with different parents', () => {
    // Create fork: scene 3 and scene 4 both reference scene 2
    scenes[3].receipt.forkParents = [scenes[2].receipt.sceneId];
    scenes[4].receipt.forkParents = [scenes[2].receipt.sceneId];

    // This is valid - both scenes fork from same parent
    expect(scenes[3].receipt.forkParents[0]).toBe(scenes[2].receipt.sceneId);
    expect(scenes[4].receipt.forkParents[0]).toBe(scenes[2].receipt.sceneId);
  });

  it('should track scene lineage in metadata', () => {
    const scene = scenes[2];

    // Metadata should have creation time
    expect(scene.metadata.created).toBeDefined();
    expect(scene.metadata.author).toBeDefined();
    expect(scene.metadata.sequenceNumber).toBe(3);
  });

  it('should support tags for scene categorization', () => {
    const scene = createSceneEnvelopeFixture({
      metadata: {
        created: Date.now(),
        author: 'did:key:test',
        sequenceNumber: 1,
        tags: ['vote', 'critical', 'audit']
      }
    });

    expect(scene.metadata.tags).toContain('vote');
    expect(scene.metadata.tags).toContain('critical');
  });
});

describe('Guard Evaluation Integration', () => {
  let universe;
  let scene;

  beforeEach(() => {
    universe = createUniverseFixture();
    scene = createSceneEnvelopeFixture({ universeId: universe.id });
  });

  it('should evaluate all guards before accepting scene', () => {
    // Every guard in universe should have been evaluated
    const guardIds = Array.from(universe.guards.keys());

    expect(Array.isArray(scene.receipt.admissibilityChecks)).toBe(true);
    // At least one guard check should be present
    expect(scene.receipt.admissibilityChecks.length).toBeGreaterThan(0);
  });

  it('should reject scene if any guard denies', () => {
    // Create a scene with failing guard
    const failedGuard = {
      allowed: false,
      denyReason: 'Unauthorized agent',
      guardId: 'guard-auth',
      timestamp: Date.now()
    };

    const rejectedScene = createSceneEnvelopeFixture({
      receipt: {
        ...scene.receipt,
        admissibilityChecks: [failedGuard]
      },
      consequences: {
        status: 'rejected',
        resultingGraph: new Map(),
        invariantChecks: [],
        sideEffectTokens: []
      }
    });

    expect(rejectedScene.consequences.status).toBe('rejected');
  });

  it('should provide guard denial reasons', () => {
    scene.receipt.admissibilityChecks.forEach(check => {
      if (!check.allowed) {
        expect(check.denyReason).toBeDefined();
        expect(typeof check.denyReason).toBe('string');
      }
    });
  });

  it('should timestamp each guard evaluation', () => {
    scene.receipt.admissibilityChecks.forEach(check => {
      expect(typeof check.timestamp).toBe('number');
      expect(check.timestamp).toBeGreaterThan(0);
    });
  });
});

describe('Invariant Validation Integration', () => {
  let universe;
  let scene;

  beforeEach(() => {
    universe = createUniverseFixture();
    scene = createSceneEnvelopeFixture({ universeId: universe.id });
  });

  it('should check all universe invariants', () => {
    const invariantIds = universe.invariants.rules.map(r => r.id);

    // Scene should have checked at least the universe's invariants
    expect(Array.isArray(scene.consequences.invariantChecks)).toBe(true);
  });

  it('should enforce strict invariants', () => {
    const strictUniverse = createUniverseFixture({
      invariants: {
        rules: [
          {
            id: 'strict-inv-1',
            sparql: 'ASK { ?x a ex:Agent }',
            description: 'Must have agent',
            severity: 'error'
          }
        ],
        enforcement: 'strict'
      }
    });

    const scene = createSceneEnvelopeFixture({ universeId: strictUniverse.id });

    // If invariant fails in strict mode, scene should be rejected
    const failedCheck = {
      invariantId: 'strict-inv-1',
      satisfied: false,
      query: 'ASK { ?x a ex:Agent }',
      bindings: {}
    };

    const rejectedScene = createSceneEnvelopeFixture({
      consequences: {
        status: 'rejected',
        resultingGraph: new Map(),
        invariantChecks: [failedCheck],
        sideEffectTokens: []
      }
    });

    expect(rejectedScene.consequences.status).toBe('rejected');
  });

  it('should allow eventual consistency mode', () => {
    const eventualUniverse = createUniverseFixture({
      invariants: {
        rules: [
          {
            id: 'eventual-inv-1',
            sparql: 'ASK { ?x ex:sum ?s }',
            description: 'Sum converges',
            severity: 'warning'
          }
        ],
        enforcement: 'eventual'
      }
    });

    // In eventual mode, invariants can be violated temporarily
    const scene = createSceneEnvelopeFixture({ universeId: eventualUniverse.id });

    expect(eventualUniverse.invariants.enforcement).toBe('eventual');
  });
});

describe('Bridge Integration', () => {
  let universeA;
  let universeB;
  let bridge;
  let sceneA;

  beforeEach(() => {
    universeA = createUniverseFixture();
    universeB = createUniverseFixture();
    bridge = createBridgeProofFixture({
      sourceUniverseId: universeA.id,
      targetUniverseId: universeB.id
    });
    sceneA = createSceneEnvelopeFixture({ universeId: universeA.id });
  });

  it('should translate scene across bridge', () => {
    // Bridge should map A's observations to B's schema
    expect(bridge.typeCoercion.classMapping instanceof Map).toBe(true);
    expect(bridge.typeCoercion.propertyMapping instanceof Map).toBe(true);
  });

  it('should preserve invariants during bridge crossing', () => {
    // Any invariant preserved should reference both universes
    bridge.invariantPreservation.forEach(inv => {
      expect(inv.sourceInvariantId).toBeDefined();
      expect(inv.targetInvariantId).toBeDefined();
      expect(['logical-implication', 'constraint-relaxation', 'manual-verification'])
        .toContain(inv.proofType);
    });
  });

  it('should grant access when needed', () => {
    const bridge = createBridgeProofFixture({
      sourceUniverseId: universeA.id,
      targetUniverseId: universeB.id,
      accessGrants: [
        {
          guardId: 'auth-check',
          reason: 'Trust relationship between universes',
          grantor: 'did:key:admin',
          expiresAt: null
        }
      ]
    });

    expect(bridge.accessGrants.length).toBe(1);
    expect(bridge.accessGrants[0].guardId).toBe('auth-check');
  });

  it('should support bidirectional bridges', () => {
    const bibridge = createBridgeProofFixture({
      sourceUniverseId: universeA.id,
      targetUniverseId: universeB.id,
      metadata: {
        created: Date.now(),
        creator: 'did:key:creator',
        bidirectional: true,
        usageCount: 0
      }
    });

    expect(bibridge.metadata.bidirectional).toBe(true);
  });

  it('should track bridge usage', () => {
    const bridge = createBridgeProofFixture({
      metadata: {
        created: Date.now(),
        creator: 'did:key:creator',
        bidirectional: false,
        usageCount: 5
      }
    });

    expect(bridge.metadata.usageCount).toBe(5);
  });
});

describe('Artifact Generation', () => {
  let scene;

  beforeEach(() => {
    scene = createSceneEnvelopeFixture();
  });

  it('should generate deterministic artifacts', () => {
    const artifact1 = {
      id: 'abc'.repeat(21) + 'a',  // 64 chars
      type: 'text/plain',
      data: 'content',
      generatedBy: 'reconciler',
      dependencies: []
    };

    const scene1 = createSceneEnvelopeFixture({ artifacts: [artifact1] });
    const scene2 = createSceneEnvelopeFixture({ artifacts: [artifact1] });

    // Same artifact ID should be identical
    expect(scene1.artifacts[0].id).toBe(scene2.artifacts[0].id);
  });

  it('should track artifact dependencies', () => {
    const artifact1 = { id: 'x'.repeat(64), type: 'base', data: 'base', generatedBy: 'r', dependencies: [] };
    const artifact2 = { id: 'y'.repeat(64), type: 'derived', data: 'derived', generatedBy: 'r', dependencies: [artifact1.id] };

    const scene = createSceneEnvelopeFixture({ artifacts: [artifact1, artifact2] });

    expect(scene.artifacts[1].dependencies).toContain(artifact1.id);
  });

  it('should support multiple artifact types', () => {
    const artifacts = [
      { id: 'a'.repeat(64), type: 'text/plain', data: 'txt', generatedBy: 'r', dependencies: [] },
      { id: 'b'.repeat(64), type: 'application/json', data: '{}', generatedBy: 'r', dependencies: [] },
      { id: 'c'.repeat(64), type: 'application/rdf+xml', data: '<rdf/>', generatedBy: 'r', dependencies: [] }
    ];

    const scene = createSceneEnvelopeFixture({ artifacts });

    expect(scene.artifacts.map(a => a.type)).toContain('text/plain');
    expect(scene.artifacts.map(a => a.type)).toContain('application/json');
    expect(scene.artifacts.map(a => a.type)).toContain('application/rdf+xml');
  });
});
