/**
 * @file Zero-Knowledge SPARQL - Main Entry Point
 * @module @unrdf/zkp
 * @description
 * Privacy-preserving SPARQL query proofs using zk-SNARKs (Groth16).
 *
 * **First production ZK-RDF system** enabling:
 * - Prove query results without revealing data
 * - 192-byte constant-size proofs
 * - 1-2ms verification time
 * - 128-bit security (BN128 curve)
 *
 * @example
 * import { createZKProver } from '@unrdf/zkp';
 *
 * const prover = createZKProver();
 * const { proof, publicSignals } = await prover.prove(triples, query, results);
 * const valid = await prover.verify(proof, publicSignals);
 */

export {
  SPARQLZKProver,
  createZKProver,
  verifyZKProof,
  estimateProofPerformance,
} from './sparql-zkp-prover.mjs';

export {
  CircuitCompiler,
  createCircuitCompiler,
  compileSPARQL,
} from './circuit-compiler.mjs';

export {
  Groth16Prover,
  createGroth16Prover,
  generateProof,
} from './groth16-prover.mjs';

export {
  Groth16Verifier,
  createGroth16Verifier,
  verifyProof,
} from './groth16-verifier.mjs';

export * from './schemas.mjs';
