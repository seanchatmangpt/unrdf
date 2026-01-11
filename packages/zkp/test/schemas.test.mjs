/**
 * @file Schema Tests
 * @module @unrdf/zkp/test/schemas
 * @description Tests for Zod validation schemas
 */

import { describe, it, expect } from 'vitest';
import {
  SPARQLQuerySchema,
  TripleSchema,
  TriplePatternSchema,
  FilterConstraintSchema,
  R1CSConstraintSchema,
  ArithmeticCircuitSchema,
  Groth16ProofSchema,
  PublicSignalsSchema,
  VerificationKeySchema,
  ZKProofWithMetadataSchema,
  PerformanceEstimateSchema,
  ZKReceiptSchema,
} from '../src/schemas.mjs';

describe('Schema Validation', () => {
  describe('SPARQLQuerySchema', () => {
    it('should validate SELECT query', () => {
      const query = {
        type: 'SELECT',
        pattern: '?s ?p ?o',
      };

      const result = SPARQLQuerySchema.safeParse(query);

      expect(result.success).toBe(true);
    });

    it('should validate query with filters', () => {
      const query = {
        type: 'SELECT',
        pattern: '?s :age ?age',
        filters: ['?age > 18'],
      };

      const result = SPARQLQuerySchema.safeParse(query);

      expect(result.success).toBe(true);
    });

    it('should reject invalid query type', () => {
      const query = {
        type: 'INVALID',
        pattern: '?s ?p ?o',
      };

      const result = SPARQLQuerySchema.safeParse(query);

      expect(result.success).toBe(false);
    });
  });

  describe('TripleSchema', () => {
    it('should validate triple', () => {
      const triple = {
        subject: ':Alice',
        predicate: ':age',
        object: '25',
      };

      const result = TripleSchema.safeParse(triple);

      expect(result.success).toBe(true);
    });

    it('should reject incomplete triple', () => {
      const triple = {
        subject: ':Alice',
        predicate: ':age',
      };

      const result = TripleSchema.safeParse(triple);

      expect(result.success).toBe(false);
    });
  });

  describe('FilterConstraintSchema', () => {
    it('should validate gt filter', () => {
      const filter = {
        type: 'gt',
        variable: 'age',
        value: 18,
      };

      const result = FilterConstraintSchema.safeParse(filter);

      expect(result.success).toBe(true);
    });

    it('should validate eq filter with string value', () => {
      const filter = {
        type: 'eq',
        variable: 'name',
        value: 'Alice',
      };

      const result = FilterConstraintSchema.safeParse(filter);

      expect(result.success).toBe(true);
    });

    it('should reject invalid filter type', () => {
      const filter = {
        type: 'invalid',
        variable: 'age',
        value: 18,
      };

      const result = FilterConstraintSchema.safeParse(filter);

      expect(result.success).toBe(false);
    });
  });

  describe('Groth16ProofSchema', () => {
    it('should validate proof', () => {
      const proof = {
        pi_a: ['1', '2', '3'],
        pi_b: [
          ['1', '2'],
          ['3', '4'],
          ['5', '6'],
        ],
        pi_c: ['7', '8', '9'],
        protocol: 'groth16',
        curve: 'bn128',
      };

      const result = Groth16ProofSchema.safeParse(proof);

      expect(result.success).toBe(true);
    });

    it('should reject proof with wrong protocol', () => {
      const proof = {
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

      const result = Groth16ProofSchema.safeParse(proof);

      expect(result.success).toBe(false);
    });

    it('should reject proof with wrong pi_a length', () => {
      const proof = {
        pi_a: ['1', '2'],
        pi_b: [
          ['1', '2'],
          ['3', '4'],
          ['5', '6'],
        ],
        pi_c: ['7', '8', '9'],
        protocol: 'groth16',
        curve: 'bn128',
      };

      const result = Groth16ProofSchema.safeParse(proof);

      expect(result.success).toBe(false);
    });
  });

  describe('PublicSignalsSchema', () => {
    it('should validate public signals', () => {
      const signals = {
        queryHash: 'a'.repeat(64),
        resultHash: 'b'.repeat(64),
        storeCommitment: 'c'.repeat(64),
        resultCount: 10,
      };

      const result = PublicSignalsSchema.safeParse(signals);

      expect(result.success).toBe(true);
    });

    it('should reject hash with wrong length', () => {
      const signals = {
        queryHash: 'abc',
        resultHash: 'b'.repeat(64),
        storeCommitment: 'c'.repeat(64),
        resultCount: 10,
      };

      const result = PublicSignalsSchema.safeParse(signals);

      expect(result.success).toBe(false);
    });

    it('should reject negative result count', () => {
      const signals = {
        queryHash: 'a'.repeat(64),
        resultHash: 'b'.repeat(64),
        storeCommitment: 'c'.repeat(64),
        resultCount: -1,
      };

      const result = PublicSignalsSchema.safeParse(signals);

      expect(result.success).toBe(false);
    });
  });

  describe('ArithmeticCircuitSchema', () => {
    it('should validate circuit', () => {
      const circuit = {
        constraints: [
          {
            a: { x: '1' },
            b: { y: '1' },
            c: { z: '1' },
          },
        ],
        publicInputs: ['queryHash', 'resultCount'],
        privateInputs: ['triples'],
        nConstraints: 1,
        nVars: 3,
      };

      const result = ArithmeticCircuitSchema.safeParse(circuit);

      expect(result.success).toBe(true);
    });
  });

  describe('ZKReceiptSchema', () => {
    it('should validate receipt', () => {
      const receipt = {
        id: '123e4567-e89b-12d3-a456-426614174000',
        timestamp: '2024-01-01T00:00:00.000Z',
        timestamp_ns: 1000000000000000n,
        operation: 'sparql_query_zkp',
        query: {
          type: 'SPARQL',
          pattern: '?s ?p ?o',
        },
        zkProof: {
          type: 'groth16',
          proof: {
            pi_a: ['1', '2', '3'],
            pi_b: [
              ['1', '2'],
              ['3', '4'],
              ['5', '6'],
            ],
            pi_c: ['7', '8', '9'],
            protocol: 'groth16',
            curve: 'bn128',
          },
          publicSignals: {
            queryHash: 'a'.repeat(64),
            resultHash: 'b'.repeat(64),
            storeCommitment: 'c'.repeat(64),
            resultCount: 1,
          },
          verificationStatus: 'verified',
        },
        receiptHash: 'abc123',
      };

      const result = ZKReceiptSchema.safeParse(receipt);

      expect(result.success).toBe(true);
    });
  });

  describe('PerformanceEstimateSchema', () => {
    it('should validate performance estimate', () => {
      const estimate = {
        tripleCount: 1000,
        resultCount: 50,
        constraintCount: 10250,
        provingTimeMs: 5000,
        verificationTimeMs: 2,
        proofSizeBytes: 192,
        setupTimeMs: 10000,
        circuitSizeKB: 1025,
      };

      const result = PerformanceEstimateSchema.safeParse(estimate);

      expect(result.success).toBe(true);
    });
  });
});
