/**
 * @file Zero-Knowledge SPARQL Tests
 * @module @unrdf/zkp/test/sparql-zkp
 * @description Comprehensive tests for ZK-SPARQL proof system
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  SPARQLZKProver,
  createZKProver,
  verifyZKProof,
  estimateProofPerformance,
} from '../src/sparql-zkp-prover.mjs';

describe('SPARQLZKProver', () => {
  let prover;

  beforeEach(() => {
    prover = new SPARQLZKProver({
      maxTriples: 1000,
      maxResults: 100,
    });
  });

  describe('Basic Proof Generation', () => {
    it('should generate proof for simple SELECT query', async () => {
      const triples = [
        { subject: ':Alice', predicate: 'rdf:type', object: 'Person' },
        { subject: ':Bob', predicate: 'rdf:type', object: 'Person' },
      ];

      const query = 'SELECT ?s WHERE { ?s rdf:type Person }';

      const results = [{ s: ':Alice' }, { s: ':Bob' }];

      const { proof, publicSignals } = await prover.prove(
        triples,
        query,
        results
      );

      expect(proof).toBeDefined();
      expect(proof.protocol).toBe('groth16');
      expect(proof.curve).toBe('bn128');
      expect(proof.pi_a).toHaveLength(3);
      expect(proof.pi_b).toHaveLength(3);
      expect(proof.pi_c).toHaveLength(3);

      expect(publicSignals).toBeDefined();
      expect(publicSignals.queryHash).toHaveLength(64);
      expect(publicSignals.resultHash).toHaveLength(64);
      expect(publicSignals.storeCommitment).toHaveLength(64);
      expect(publicSignals.resultCount).toBe(2);
    });

    it('should generate proof for query with FILTER', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
        { subject: ':Bob', predicate: ':age', object: '17' },
        { subject: ':Carol', predicate: ':age', object: '30' },
      ];

      const query = 'SELECT ?person WHERE { ?person :age ?age FILTER(?age > 18) }';

      const results = [{ person: ':Alice' }, { person: ':Carol' }];

      const { proof, publicSignals } = await prover.prove(
        triples,
        query,
        results
      );

      expect(proof).toBeDefined();
      expect(publicSignals.resultCount).toBe(2);
    });

    it('should handle empty results', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const query = 'SELECT ?p WHERE { ?p :age ?age FILTER(?age > 100) }';

      const results = [];

      const { proof, publicSignals } = await prover.prove(
        triples,
        query,
        results
      );

      expect(proof).toBeDefined();
      expect(publicSignals.resultCount).toBe(0);
    });

    it('should handle single triple', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':name', object: '"Alice"' },
      ];

      const query = 'SELECT ?s ?o WHERE { ?s :name ?o }';

      const results = [{ s: ':Alice', o: '"Alice"' }];

      const { proof, publicSignals } = await prover.prove(
        triples,
        query,
        results
      );

      expect(proof).toBeDefined();
      expect(publicSignals.resultCount).toBe(1);
    });
  });

  describe('Proof Verification', () => {
    it('should verify valid proof', async () => {
      const triples = [
        { subject: ':Alice', predicate: 'rdf:type', object: 'Person' },
      ];

      const query = 'SELECT ?s WHERE { ?s rdf:type Person }';

      const results = [{ s: ':Alice' }];

      const { proof, publicSignals } = await prover.prove(
        triples,
        query,
        results
      );

      const valid = await prover.verify(proof, publicSignals);

      expect(valid).toBe(true);
    });

    it('should reject invalid proof structure', async () => {
      const invalidProof = {
        pi_a: ['1', '2'],
        pi_b: [['1', '2']],
        pi_c: ['1', '2', '3'],
        protocol: 'groth16',
        curve: 'bn128',
      };

      const publicSignals = {
        queryHash: 'a'.repeat(64),
        resultHash: 'b'.repeat(64),
        storeCommitment: 'c'.repeat(64),
        resultCount: 1,
      };

      const valid = await prover.verify(invalidProof, publicSignals);

      expect(valid).toBe(false);
    });

    it('should reject proof with wrong protocol', async () => {
      const invalidProof = {
        pi_a: ['1', '2', '3'],
        pi_b: [
          ['1', '2'],
          ['3', '4'],
          ['5', '6'],
        ],
        pi_c: ['7', '8', '9'],
        protocol: 'plonk',
        curve: 'bn128',
      };

      const publicSignals = {
        queryHash: 'a'.repeat(64),
        resultHash: 'b'.repeat(64),
        storeCommitment: 'c'.repeat(64),
        resultCount: 1,
      };

      const valid = await prover.verify(invalidProof, publicSignals);

      expect(valid).toBe(false);
    });
  });

  describe('Receipt Integration', () => {
    it('should generate proof with receipt', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const results = [{ s: ':Alice' }];

      const receipt = await prover.proveWithReceipt(triples, query, results);

      expect(receipt).toBeDefined();
      expect(receipt.id).toBeDefined();
      expect(receipt.timestamp).toBeDefined();
      expect(receipt.timestamp_ns).toBeDefined();
      expect(receipt.operation).toBe('sparql_query_zkp');
      expect(receipt.zkProof).toBeDefined();
      expect(receipt.zkProof.type).toBe('groth16');
      expect(receipt.zkProof.proof).toBeDefined();
      expect(receipt.zkProof.publicSignals).toBeDefined();
      expect(receipt.receiptHash).toBeDefined();
    });

    it('should generate unique receipt IDs', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const results = [{ s: ':Alice' }];

      const receipt1 = await prover.proveWithReceipt(triples, query, results);
      const receipt2 = await prover.proveWithReceipt(triples, query, results);

      expect(receipt1.id).not.toBe(receipt2.id);
    });
  });

  describe('Batch Proving', () => {
    it('should batch prove multiple queries', async () => {
      const queries = [
        {
          triples: [{ subject: ':Alice', predicate: 'rdf:type', object: 'Person' }],
          query: 'SELECT ?s WHERE { ?s rdf:type Person }',
          results: [{ s: ':Alice' }],
        },
        {
          triples: [{ subject: ':Bob', predicate: ':age', object: '30' }],
          query: 'SELECT ?s WHERE { ?s :age ?age }',
          results: [{ s: ':Bob' }],
        },
      ];

      const proofs = await prover.batchProve(queries);

      expect(proofs).toHaveLength(2);
      expect(proofs[0].proof).toBeDefined();
      expect(proofs[1].proof).toBeDefined();
    });

    it('should handle empty batch', async () => {
      const proofs = await prover.batchProve([]);

      expect(proofs).toHaveLength(0);
    });
  });

  describe('Merkle Tree Commitment', () => {
    it('should compute consistent store commitment', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
        { subject: ':Bob', predicate: ':age', object: '30' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const results = [{ s: ':Alice' }, { s: ':Bob' }];

      const { publicSignals: ps1 } = await prover.prove(triples, query, results);
      const { publicSignals: ps2 } = await prover.prove(triples, query, results);

      expect(ps1.storeCommitment).toBe(ps2.storeCommitment);
    });

    it('should produce different commitments for different stores', async () => {
      const triples1 = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const triples2 = [
        { subject: ':Bob', predicate: ':age', object: '30' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const { publicSignals: ps1 } = await prover.prove(
        triples1,
        query,
        [{ s: ':Alice' }]
      );

      const { publicSignals: ps2 } = await prover.prove(
        triples2,
        query,
        [{ s: ':Bob' }]
      );

      expect(ps1.storeCommitment).not.toBe(ps2.storeCommitment);
    });
  });

  describe('Input Validation', () => {
    it('should reject invalid triples', async () => {
      const invalidTriples = [
        { subject: ':Alice', predicate: ':age' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      await expect(
        prover.prove(invalidTriples, query, [])
      ).rejects.toThrow();
    });

    it('should reject too many triples', async () => {
      const prover = new SPARQLZKProver({ maxTriples: 10 });

      const triples = Array.from({ length: 20 }, (_, i) => ({
        subject: `:s${i}`,
        predicate: ':p',
        object: ':o',
      }));

      const query = 'SELECT ?s WHERE { ?s :p :o }';

      await expect(
        prover.prove(triples, query, [])
      ).rejects.toThrow(/exceeds circuit capacity/);
    });

    it('should reject too many results', async () => {
      const prover = new SPARQLZKProver({ maxResults: 5 });

      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const results = Array.from({ length: 10 }, (_, i) => ({ s: `:s${i}` }));

      await expect(
        prover.prove(triples, query, results)
      ).rejects.toThrow(/exceeds circuit capacity/);
    });
  });

  describe('Performance', () => {
    it('should prove small query quickly', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const results = [{ s: ':Alice' }];

      const startTime = performance.now();

      await prover.prove(triples, query, results);

      const duration = performance.now() - startTime;

      expect(duration).toBeLessThan(100);
    });

    it('should verify quickly', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const results = [{ s: ':Alice' }];

      const { proof, publicSignals } = await prover.prove(
        triples,
        query,
        results
      );

      const startTime = performance.now();

      await prover.verify(proof, publicSignals);

      const duration = performance.now() - startTime;

      expect(duration).toBeLessThan(10);
    });
  });

  describe('Factory Functions', () => {
    it('should create prover with createZKProver', () => {
      const prover = createZKProver({ maxTriples: 500 });

      expect(prover).toBeInstanceOf(SPARQLZKProver);
      expect(prover.config.maxTriples).toBe(500);
    });

    it('should verify with verifyZKProof', async () => {
      const triples = [
        { subject: ':Alice', predicate: ':age', object: '25' },
      ];

      const query = 'SELECT ?s WHERE { ?s :age ?age }';

      const results = [{ s: ':Alice' }];

      const { proof, publicSignals } = await prover.prove(
        triples,
        query,
        results
      );

      const valid = await verifyZKProof(proof, publicSignals);

      expect(valid).toBe(true);
    });
  });

  describe('Performance Estimation', () => {
    it('should estimate proof performance', () => {
      const estimate = estimateProofPerformance(1000, 50);

      expect(estimate.tripleCount).toBe(1000);
      expect(estimate.resultCount).toBe(50);
      expect(estimate.constraintCount).toBeGreaterThan(0);
      expect(estimate.provingTimeMs).toBeGreaterThan(0);
      expect(estimate.verificationTimeMs).toBe(2);
      expect(estimate.proofSizeBytes).toBe(192);
    });

    it('should scale linearly with triple count', () => {
      const est1 = estimateProofPerformance(1000, 10);
      const est2 = estimateProofPerformance(2000, 10);

      const ratio = est2.constraintCount / est1.constraintCount;

      expect(ratio).toBeCloseTo(2, 0);
    });
  });
});
