/**
 * @file Groth16 zk-SNARK Verifier
 * @module @unrdf/zkp/groth16-verifier
 * @description
 * Production Groth16 verifier using snarkjs.
 * Constant-time verification (<2ms) with 128-bit security.
 *
 * **Performance**: 1-2ms verification regardless of circuit size
 * **Security**: Pairing-based cryptography on BN128 curve
 */

import { groth16 } from 'snarkjs';
import { trace } from '@opentelemetry/api';
import { Groth16ProofSchema, VerificationKeySchema } from './schemas.mjs';

const tracer = trace.getTracer('@unrdf/zkp/groth16-verifier');

/**
 * Groth16 Verifier
 *
 * Verifies zk-SNARK proofs in constant time (<2ms).
 * Uses pairing-based cryptography for security.
 *
 * @class
 * @example
 * const verifier = new Groth16Verifier({ vkeyPath: './vkey.json' });
 * const valid = await verifier.verify(proof, publicSignals);
 */
export class Groth16Verifier {
  /**
   * @param {Object} [config] - Verifier configuration
   * @param {string} [config.vkeyPath] - Path to verification key
   * @param {Object} [config.vkey] - Verification key object
   */
  constructor(config = {}) {
    this.config = {
      vkeyPath: config.vkeyPath || null,
      vkey: config.vkey || null,
    };
    this.cachedVKey = null;
  }

  /**
   * Verify Groth16 proof
   *
   * @param {Object} proof - zk-SNARK proof
   * @param {Array<string>} publicSignals - Public signals
   * @param {Object} [vkey] - Override verification key
   * @returns {Promise<boolean>} True if proof is valid
   *
   * @example
   * const valid = await verifier.verify(proof, ['0x123...', '0xabc...']);
   * if (valid) console.log('Proof verified!');
   */
  async verify(proof, publicSignals, vkey = null) {
    return tracer.startActiveSpan('groth16.verify', async (span) => {
      try {
        const startTime = performance.now();

        Groth16ProofSchema.parse(proof);

        const verificationKey = vkey || this.cachedVKey || (await this._loadVKey());

        span.setAttribute('vkey.nPublic', verificationKey.nPublic);
        span.setAttribute('signals.count', publicSignals.length);

        const valid = await groth16.verify(verificationKey, publicSignals, proof);

        const verifyTime = performance.now() - startTime;

        span.setAttribute('verification.time_ms', verifyTime);
        span.setAttribute('verification.result', valid);
        span.setStatus({ code: 1 });

        return valid;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        span.recordException(error);
        return false;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Batch verify multiple proofs
   *
   * @param {Array<{proof: Object, publicSignals: Array<string>}>} proofs
   * @returns {Promise<Array<boolean>>} Verification results
   *
   * @example
   * const results = await verifier.batchVerify([
   *   { proof: proof1, publicSignals: signals1 },
   *   { proof: proof2, publicSignals: signals2 }
   * ]);
   */
  async batchVerify(proofs) {
    return tracer.startActiveSpan('groth16.batch-verify', async (span) => {
      try {
        span.setAttribute('batch.size', proofs.length);

        const results = await Promise.all(
          proofs.map(({ proof, publicSignals }) =>
            this.verify(proof, publicSignals)
          )
        );

        const successCount = results.filter((r) => r).length;

        span.setAttribute('batch.success', successCount);
        span.setAttribute('batch.failed', proofs.length - successCount);
        span.setStatus({ code: 1 });

        return results;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Load verification key
   * @private
   */
  async _loadVKey() {
    if (this.cachedVKey) {
      return this.cachedVKey;
    }

    if (this.config.vkey) {
      this.cachedVKey = VerificationKeySchema.parse(this.config.vkey);
      return this.cachedVKey;
    }

    if (this.config.vkeyPath) {
      const fs = await import('node:fs/promises');
      const data = await fs.readFile(this.config.vkeyPath, 'utf8');
      this.cachedVKey = VerificationKeySchema.parse(JSON.parse(data));
      return this.cachedVKey;
    }

    throw new Error('No verification key provided');
  }

  /**
   * Estimate verification time
   *
   * Groth16 verification is constant time (~1-2ms)
   * regardless of circuit complexity.
   *
   * @returns {number} Estimated verification time in ms
   *
   * @example
   * const timeMs = verifier.estimateVerificationTime();
   */
  estimateVerificationTime() {
    return 2;
  }
}

/**
 * Create Groth16 verifier instance
 *
 * @param {Object} [config] - Verifier configuration
 * @returns {Groth16Verifier} Verifier instance
 *
 * @example
 * const verifier = createGroth16Verifier({ vkeyPath: './vkey.json' });
 */
export function createGroth16Verifier(config = {}) {
  return new Groth16Verifier(config);
}

/**
 * Verify proof (convenience function)
 *
 * @param {Object} proof - zk-SNARK proof
 * @param {Array<string>} publicSignals - Public signals
 * @param {Object} vkey - Verification key
 * @returns {Promise<boolean>} True if proof is valid
 *
 * @example
 * const valid = await verifyProof(proof, publicSignals, vkey);
 */
export async function verifyProof(proof, publicSignals, vkey) {
  const verifier = new Groth16Verifier({ vkey });
  return await verifier.verify(proof, publicSignals);
}
