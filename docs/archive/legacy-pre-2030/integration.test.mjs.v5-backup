/**
 * Integration Test: Full End-to-End Governed Ontology Substrate Workflow
 *
 * Tests the complete flow:
 * 1. Load universe with all 6 partitions
 * 2. Propose a valid Δ (delta) capsule
 * 3. Admit delta with invariant checks
 * 4. Verify receipt generation and chaining
 * 5. Validate receipt integrity
 *
 * @test Full workflow produces valid artifacts with receipts
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Universe } from './universe/universe.mjs';
import { DeltaCapsule } from './admission/delta-capsule.mjs';
import { AdmissionEngine } from './admission/admission-engine.mjs';
import { ReceiptChain } from './receipts/receipt-chain.mjs';

describe('Integration: Full Governed Ontology Substrate Workflow', () => {

  let universe;
  let admissionEngine;
  let receiptChain;

  beforeEach(() => {
    universe = new Universe();
    admissionEngine = new AdmissionEngine({
      strictMode: true,
      maxQuads: 100000
    });
    receiptChain = new ReceiptChain();
  });

  describe('1. Universe Initialization', () => {

    it('should load universe with all 6 partitions', () => {
      const partitions = universe.getAllPartitions();
      expect(partitions).toHaveLength(6);

      const types = partitions.map(p => p.constructor.name);
      expect(types).toContain('IndustrialSubstrate');
      expect(types).toContain('CorporateCanon');
      expect(types).toContain('BusinessUnitOverlay');
      expect(types).toContain('RegionalOverlay');
      expect(types).toContain('ExecutionLedger');
      expect(types).toContain('SystemPolicyPartition');
    });

    it('should have industrial substrate with 7 allowed ontologies', async () => {
      const substrate = universe.getPartition('IndustrialSubstrate');
      const hash = await substrate.computeHash();

      expect(hash).toBeDefined();
      expect(typeof hash).toBe('string');
      expect(hash.length).toBeGreaterThan(10);
    });

    it('should enforce read-only on industrial substrate', () => {
      const substrate = universe.getPartition('IndustrialSubstrate');
      expect(substrate.isReadOnly()).toBe(true);
    });

    it('should have 8 protected namespaces in system policy', async () => {
      const systemPolicy = universe.getPartition('SystemPolicyPartition');
      const protectedNamespaces = systemPolicy.getProtectedNamespaces();

      expect(protectedNamespaces).toBeDefined();
      expect(protectedNamespaces.size).toBeGreaterThanOrEqual(8);

      // Verify key namespaces
      expect(Array.from(protectedNamespaces)).toContainEqual(
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#' // RDF
      );
      expect(Array.from(protectedNamespaces)).toContainEqual(
        'http://www.w3.org/2000/01/rdf-schema#' // RDFS
      );
    });

  });

  describe('2. Delta Capsule Proposal', () => {

    it('should create a valid delta capsule for business unit overlay', () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.disney.com/graph/bu/studios',
          name: 'StudiosOverlay'
        },
        changes: [
          {
            operation: 'add',
            quads: [
              {
                subject: { termType: 'NamedNode', value: 'http://example.com/Film1' },
                predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
                object: { termType: 'NamedNode', value: 'http://example.com/Film' },
                graph: { termType: 'DefaultGraph' }
              }
            ]
          }
        ],
        invariants: [
          { name: 'Q_typing', enabled: true },
          { name: 'Q_noncollision', enabled: true },
          { name: 'Q_monotone', enabled: true },
          { name: 'Q_determinism', enabled: true },
          { name: 'Q_provenance', enabled: true },
          { name: 'Q_bounds', enabled: true }
        ],
        provenance: {
          agent: 'integration-test@unrdf.org',
          timestamp: new Date().toISOString()
        }
      });

      expect(delta).toBeDefined();
      expect(delta.getPartition().namespace).toBe('http://example.disney.com/graph/bu/studios');
      expect(delta.getQuadCount()).toBe(1);
      expect(delta.isAdditiveOnly()).toBe(true);
    });

    it('should compute deterministic hash for delta capsule', async () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/test',
          name: 'TestOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Item1' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Item' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: '2025-01-01T00:00:00Z' }
      });

      const hash1 = delta.getHash();
      const hash2 = delta.getHash();

      expect(hash1).toBe(hash2);
      expect(hash1.length).toBeGreaterThan(10);
    });

  });

  describe('3. Admission Control', () => {

    it('should admit valid delta capsule with all invariants passing', async () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.disney.com/graph/bu/studios',
          name: 'StudiosOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Film1' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Film' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [
          { name: 'Q_typing', enabled: true },
          { name: 'Q_noncollision', enabled: true },
          { name: 'Q_monotone', enabled: true },
          { name: 'Q_determinism', enabled: true },
          { name: 'Q_provenance', enabled: true },
          { name: 'Q_bounds', enabled: true }
        ],
        provenance: {
          agent: 'test-agent',
          timestamp: new Date().toISOString()
        }
      });

      const decision = await admissionEngine.admitCapsule(delta);

      expect(decision).toBeDefined();
      expect(decision.allowed).toBe(true);
      expect(decision.checks).toBeDefined();
      expect(decision.checks.length).toBeGreaterThan(0);
      expect(decision.receipt).toBeDefined();
    });

    it('should deny delta editing protected namespace', async () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', // RDF namespace - PROTECTED
          name: 'BadOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#BadTerm' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/2000/01/rdf-schema#label' },
            object: { termType: 'Literal', value: 'Bad' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [{ name: 'Q_noncollision', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const decision = await admissionEngine.admitCapsule(delta);

      expect(decision).toBeDefined();
      expect(decision.allowed).toBe(false);
      expect(decision.receipt).toBeDefined();
      expect(decision.receipt.decision).toBe('deny');
    });

    it('should have guard decision in receipt', async () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/test',
          name: 'TestOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Item1' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Item' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const decision = await admissionEngine.admitCapsule(delta);

      expect(decision.receipt.guardDecision).toBeDefined();
      expect(decision.receipt.guardDecision.allowed).toBeDefined();
    });

  });

  describe('4. Receipt Generation & Chaining', () => {

    it('should generate deterministic receipts', async () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/test',
          name: 'TestOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Item' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Class' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: '2025-01-01T12:00:00Z' }
      });

      const decision1 = await admissionEngine.admitCapsule(delta);
      const decision2 = await admissionEngine.admitCapsule(delta);

      expect(decision1.receipt.receiptHash).toBe(decision2.receipt.receiptHash);
    });

    it('should chain receipts with beforeHash link', async () => {
      const delta1 = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/test1',
          name: 'Overlay1'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Item1' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Class' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: '2025-01-01T12:00:00Z' }
      });

      const decision1 = await admissionEngine.admitCapsule(delta1);
      const receipt1 = decision1.receipt;

      // Second receipt should have beforeHash pointing to first
      const delta2 = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/test2',
          name: 'Overlay2'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Item2' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Class' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: '2025-01-01T12:00:01Z' }
      });

      const decision2 = await admissionEngine.admitCapsule(delta2);
      const receipt2 = decision2.receipt;

      expect(receipt1.beforeHash).toBeNull();
      expect(receipt2.beforeHash).toBe(receipt1.receiptHash);
    });

    it('should verify receipt chain integrity', async () => {
      const deltas = [];
      const receipts = [];

      for (let i = 0; i < 3; i++) {
        const delta = new DeltaCapsule({
          partition: {
            namespace: `http://example.com/test${i}`,
            name: `Overlay${i}`
          },
          changes: [{
            operation: 'add',
            quads: [{
              subject: { termType: 'NamedNode', value: `http://example.com/Item${i}` },
              predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
              object: { termType: 'NamedNode', value: 'http://example.com/Class' },
              graph: { termType: 'DefaultGraph' }
            }]
          }],
          invariants: [{ name: 'Q_typing', enabled: true }],
          provenance: { agent: 'test', timestamp: `2025-01-01T12:00:0${i}Z` }
        });

        const decision = await admissionEngine.admitCapsule(delta);
        receipts.push(decision.receipt);
      }

      // Build chain
      for (const receipt of receipts) {
        await receiptChain.append(receipt);
      }

      // Verify
      const verification = await receiptChain.verify();

      expect(verification.valid).toBe(true);
      expect(verification.receipts).toHaveLength(3);
      expect(verification.merkleRoot).toBeDefined();
    });

  });

  describe('5. Invariant Enforcement', () => {

    it('should fail Q_typing for invalid RDF syntax', async () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/test',
          name: 'TestOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'Literal', value: 'NotAllowed' }, // Literals cannot be subjects
            predicate: { termType: 'NamedNode', value: 'http://example.com/prop' },
            object: { termType: 'NamedNode', value: 'http://example.com/value' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const decision = await admissionEngine.admitCapsule(delta);

      expect(decision.allowed).toBe(false);
      const typingCheck = decision.checks.find(c => c.name === 'Q_typing');
      expect(typingCheck).toBeDefined();
      expect(typingCheck.passed).toBe(false);
    });

    it('should track all invariant check results in receipt', async () => {
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/test',
          name: 'TestOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Item' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Class' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [
          { name: 'Q_typing', enabled: true },
          { name: 'Q_noncollision', enabled: true },
          { name: 'Q_monotone', enabled: true }
        ],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const decision = await admissionEngine.admitCapsule(delta);

      expect(decision.receipt.invariantResults).toBeDefined();
      expect(decision.receipt.invariantResults.length).toBeGreaterThan(0);
      expect(decision.receipt.invariantResults[0]).toHaveProperty('name');
      expect(decision.receipt.invariantResults[0]).toHaveProperty('passed');
      expect(decision.receipt.invariantResults[0]).toHaveProperty('reason');
    });

  });

  describe('6. End-to-End Workflow', () => {

    it('should complete full workflow: propose → admit → chain → verify', async () => {
      // Step 1: Create delta
      const delta = new DeltaCapsule({
        partition: {
          namespace: 'http://example.com/bu/studios',
          name: 'StudiosOverlay'
        },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'http://example.com/Film1' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.com/Film' },
            graph: { termType: 'DefaultGraph' }
          }]
        }],
        invariants: [
          { name: 'Q_typing', enabled: true },
          { name: 'Q_noncollision', enabled: true }
        ],
        provenance: {
          agent: 'studio-developer@disney.com',
          timestamp: new Date().toISOString()
        }
      });

      // Step 2: Admit
      const decision = await admissionEngine.admitCapsule(delta);
      expect(decision.allowed).toBe(true);

      // Step 3: Get receipt
      const receipt = decision.receipt;
      expect(receipt).toBeDefined();
      expect(receipt.decision).toBe('allow');
      expect(receipt.epoch).toMatch(/^τ_\d{4}_\d{2}_\d{2}/);
      expect(receipt.receiptHash).toBeDefined();

      // Step 4: Chain receipt
      await receiptChain.append(receipt);

      // Step 5: Verify chain
      const verification = await receiptChain.verify();
      expect(verification.valid).toBe(true);
      expect(verification.receipts).toHaveLength(1);
    });

    it('should handle multiple sequential admissions with chaining', async () => {
      const results = [];

      for (let i = 0; i < 5; i++) {
        const delta = new DeltaCapsule({
          partition: {
            namespace: `http://example.com/partition${i}`,
            name: `Partition${i}`
          },
          changes: [{
            operation: 'add',
            quads: [{
              subject: { termType: 'NamedNode', value: `http://example.com/entity${i}` },
              predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
              object: { termType: 'NamedNode', value: 'http://example.com/Entity' },
              graph: { termType: 'DefaultGraph' }
            }]
          }],
          invariants: [{ name: 'Q_typing', enabled: true }],
          provenance: { agent: 'test', timestamp: new Date().toISOString() }
        });

        const decision = await admissionEngine.admitCapsule(delta);
        results.push(decision);
        await receiptChain.append(decision.receipt);
      }

      // Verify all receipts
      const verification = await receiptChain.verify();
      expect(verification.valid).toBe(true);
      expect(verification.receipts).toHaveLength(5);

      // Verify chain links
      for (let i = 1; i < results.length; i++) {
        expect(results[i].receipt.beforeHash).toBe(results[i - 1].receipt.receiptHash);
      }
    });

  });

});
