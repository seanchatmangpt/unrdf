/**
 * @file Zero-Knowledge Proof Schemas
 * @module @unrdf/zkp/schemas
 * @description Zod validation schemas for ZK-SPARQL proof system
 */

import { z } from 'zod';

/**
 * SPARQL query schema
 */
export const SPARQLQuerySchema = z.object({
  type: z.enum(['SELECT', 'ASK', 'CONSTRUCT', 'DESCRIBE']),
  pattern: z.string().min(1),
  filters: z.array(z.string()).optional(),
  limit: z.number().int().positive().optional(),
});

/**
 * Triple pattern schema
 */
export const TriplePatternSchema = z.object({
  subject: z.string(),
  predicate: z.string(),
  object: z.string(),
  isVariable: z
    .object({
      subject: z.boolean().optional(),
      predicate: z.boolean().optional(),
      object: z.boolean().optional(),
    })
    .optional(),
});

/**
 * RDF triple schema
 */
export const TripleSchema = z.object({
  subject: z.string(),
  predicate: z.string(),
  object: z.string(),
});

/**
 * FILTER constraint schema
 */
export const FilterConstraintSchema = z.object({
  type: z.enum(['gt', 'lt', 'eq', 'neq', 'gte', 'lte']),
  variable: z.string(),
  value: z.union([z.string(), z.number()]),
});

/**
 * R1CS constraint schema
 */
export const R1CSConstraintSchema = z.object({
  a: z.record(z.string(), z.string()),
  b: z.record(z.string(), z.string()),
  c: z.record(z.string(), z.string()),
});

/**
 * Arithmetic circuit schema
 */
export const ArithmeticCircuitSchema = z.object({
  constraints: z.array(R1CSConstraintSchema),
  publicInputs: z.array(z.string()),
  privateInputs: z.array(z.string()),
  nConstraints: z.number().int().nonnegative(),
  nVars: z.number().int().positive(),
});

/**
 * Groth16 proof schema
 */
export const Groth16ProofSchema = z.object({
  pi_a: z.array(z.string()).length(3),
  pi_b: z.array(z.array(z.string()).length(2)).length(3),
  pi_c: z.array(z.string()).length(3),
  protocol: z.literal('groth16'),
  curve: z.literal('bn128'),
});

/**
 * Public signals schema
 */
export const PublicSignalsSchema = z.object({
  queryHash: z.string().length(64),
  resultHash: z.string().length(64),
  storeCommitment: z.string().length(64),
  resultCount: z.number().int().nonnegative(),
});

/**
 * Verification key schema
 */
export const VerificationKeySchema = z.object({
  protocol: z.literal('groth16'),
  curve: z.literal('bn128'),
  nPublic: z.number().int().positive(),
  vk_alpha_1: z.array(z.string()),
  vk_beta_2: z.array(z.array(z.string())),
  vk_gamma_2: z.array(z.array(z.string())),
  vk_delta_2: z.array(z.array(z.string())),
  vk_alphabeta_12: z.array(z.array(z.string())),
  IC: z.array(z.array(z.string())),
});

/**
 * Proving key schema
 */
export const ProvingKeySchema = z.object({
  protocol: z.literal('groth16'),
  curve: z.literal('bn128'),
  nVars: z.number().int().positive(),
  nPublic: z.number().int().nonnegative(),
  domainSize: z.number().int().positive(),
  vk_alpha_1: z.array(z.string()),
  vk_beta_1: z.array(z.string()),
  vk_delta_1: z.array(z.string()),
  A: z.array(z.array(z.string())),
  B1: z.array(z.array(z.string())),
  B2: z.array(z.array(z.string())),
  C: z.array(z.array(z.string())),
});

/**
 * ZK proof with metadata schema
 */
export const ZKProofWithMetadataSchema = z.object({
  proof: Groth16ProofSchema,
  publicSignals: PublicSignalsSchema,
  metadata: z
    .object({
      provingTimeMs: z.number().nonnegative(),
      circuitSize: z.number().int().positive(),
      timestamp: z.string(),
      version: z.string(),
    })
    .optional(),
});

/**
 * Circuit compilation result schema
 */
export const CircuitCompilationSchema = z.object({
  circuit: ArithmeticCircuitSchema,
  r1cs: z.object({
    constraints: z.array(R1CSConstraintSchema),
    nVars: z.number().int().positive(),
    nPublic: z.number().int().nonnegative(),
    nOutputs: z.number().int().nonnegative(),
  }),
  witness: z.record(z.string(), z.string()),
});

/**
 * Performance estimate schema
 */
export const PerformanceEstimateSchema = z.object({
  tripleCount: z.number().int().nonnegative(),
  resultCount: z.number().int().nonnegative(),
  constraintCount: z.number().int().nonnegative(),
  provingTimeMs: z.number().nonnegative(),
  verificationTimeMs: z.number().nonnegative(),
  proofSizeBytes: z.number().int().positive(),
  setupTimeMs: z.number().nonnegative(),
  circuitSizeKB: z.number().nonnegative(),
});

/**
 * ZK receipt schema
 */
export const ZKReceiptSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string(),
  timestamp_ns: z.bigint(),
  operation: z.literal('sparql_query_zkp'),
  query: z.object({
    type: z.string(),
    pattern: z.string(),
  }),
  zkProof: z.object({
    type: z.literal('groth16'),
    proof: Groth16ProofSchema,
    publicSignals: PublicSignalsSchema,
    verificationStatus: z.enum(['pending', 'verified', 'failed']),
  }),
  receiptHash: z.string(),
});
