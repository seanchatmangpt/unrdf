/**
 * @file Groth16 zk-SNARK Prover
 * @module @unrdf/zkp/groth16-prover
 * @description
 * Production Groth16 prover using snarkjs.
 * Generates 192-byte proofs with ~1-10s proving time.
 *
 * **Security**: 128-bit security level (BN128 curve)
 * **Performance**: Constant-size proofs, linear proving time
 */

import { groth16 } from 'snarkjs';
import { trace } from '@opentelemetry/api';
import { Groth16ProofSchema } from './schemas.mjs';

const tracer = trace.getTracer('@unrdf/zkp/groth16-prover');

/**
 * Groth16 Prover
 *
 * Generates zk-SNARK proofs using Groth16 protocol.
 * Proofs are constant size (192 bytes) with linear proving time.
 *
 * @class
 * @example
 * const prover = new Groth16Prover({ zkeyPath: './circuit.zkey' });
 * const { proof, publicSignals } = await prover.prove(witness);
 */
export class Groth16Prover {
  /**
   * @param {Object} config - Prover configuration
   * @param {string} [config.wasmPath] - Path to circuit WASM
   * @param {string} [config.zkeyPath] - Path to proving key
   */
  constructor(config = {}) {
    this.config = {
      wasmPath: config.wasmPath || null,
      zkeyPath: config.zkeyPath || null,
    };
  }

  /**
   * Generate Groth16 proof
   *
   * @param {Object} witness - Circuit witness (private + public inputs)
   * @param {string} [wasmPath] - Override WASM path
   * @param {string} [zkeyPath] - Override zkey path
   * @returns {Promise<{proof: Object, publicSignals: Array<string>}>}
   *
   * @example
   * const { proof, publicSignals } = await prover.prove({
   *   queryHash: '0x123...',
   *   triples: [...],
   *   resultCount: 10
   * });
   */
  async prove(witness, wasmPath = null, zkeyPath = null) {
    return tracer.startActiveSpan('groth16.prove', async (span) => {
      try {
        const startTime = performance.now();

        const wasm = wasmPath || this.config.wasmPath;
        const zkey = zkeyPath || this.config.zkeyPath;

        if (!wasm || !zkey) {
          throw new Error(
            'WASM and zkey paths required. Either pass to constructor or prove()'
          );
        }

        span.setAttribute('circuit.wasm', wasm);
        span.setAttribute('circuit.zkey', zkey);

        const { proof, publicSignals } = await groth16.fullProve(
          witness,
          wasm,
          zkey
        );

        const provingTime = performance.now() - startTime;

        span.setAttribute('proving.time_ms', provingTime);
        span.setAttribute('proof.public_signals', publicSignals.length);
        span.setStatus({ code: 1 });

        const validatedProof = Groth16ProofSchema.parse(proof);

        return {
          proof: validatedProof,
          publicSignals,
          metadata: {
            provingTimeMs: provingTime,
            timestamp: new Date().toISOString(),
          },
        };
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        span.recordException(error);
        throw new Error(`Groth16 proving failed: ${error.message}`);
      } finally {
        span.end();
      }
    });
  }

  /**
   * Export verification key from proving key
   *
   * @param {string} [zkeyPath] - Path to proving key
   * @returns {Promise<Object>} Verification key
   *
   * @example
   * const vkey = await prover.exportVerificationKey();
   */
  async exportVerificationKey(zkeyPath = null) {
    return tracer.startActiveSpan(
      'groth16.export-vkey',
      async (span) => {
        try {
          const zkey = zkeyPath || this.config.zkeyPath;

          if (!zkey) {
            throw new Error('zkey path required');
          }

          const vKey = await groth16.exportVerificationKey(zkey);

          span.setAttribute('vkey.nPublic', vKey.nPublic);
          span.setStatus({ code: 1 });

          return vKey;
        } catch (error) {
          span.setStatus({ code: 2, message: error.message });
          span.recordException(error);
          throw new Error(`Verification key export failed: ${error.message}`);
        } finally {
          span.end();
        }
      }
    );
  }

  /**
   * Estimate proof size
   *
   * Groth16 proofs are constant size (192 bytes)
   * regardless of circuit complexity.
   *
   * @returns {number} Proof size in bytes (always 192)
   *
   * @example
   * const size = prover.estimateProofSize();
   * console.log(`Proof size: ${size} bytes`);
   */
  estimateProofSize() {
    return 192;
  }
}

/**
 * Create Groth16 prover instance
 *
 * @param {Object} config - Prover configuration
 * @returns {Groth16Prover} Prover instance
 *
 * @example
 * const prover = createGroth16Prover({
 *   wasmPath: './circuit.wasm',
 *   zkeyPath: './circuit.zkey'
 * });
 */
export function createGroth16Prover(config = {}) {
  return new Groth16Prover(config);
}

/**
 * Generate proof (convenience function)
 *
 * @param {Object} witness - Circuit witness
 * @param {string} wasmPath - Path to circuit WASM
 * @param {string} zkeyPath - Path to proving key
 * @returns {Promise<{proof: Object, publicSignals: Array<string>}>}
 *
 * @example
 * const { proof, publicSignals } = await generateProof(
 *   witness,
 *   './circuit.wasm',
 *   './circuit.zkey'
 * );
 */
export async function generateProof(witness, wasmPath, zkeyPath) {
  const prover = new Groth16Prover({ wasmPath, zkeyPath });
  return await prover.prove(witness);
}
