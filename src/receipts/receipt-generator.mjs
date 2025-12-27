/**
 * @fileoverview Receipt Generator - Emit receipts for operations
 *
 * **Purpose**: Centralized receipt generation for:
 * - Admissibility decisions (allow/deny)
 * - Validation runs
 * - Projection operations
 * - Any operation requiring audit trail
 *
 * **Features**:
 * - Auto-collect toolchain versions (node, packages)
 * - Auto-link to previous receipt (beforeHash)
 * - Auto-compute output hashes
 * - Maintains receipt chain
 *
 * @module receipts/receipt-generator
 */

import { Receipt } from './receipt.mjs';
import { ReceiptChain } from './receipt-chain.mjs';
import { blake3 } from 'hash-wasm';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

/**
 * Receipt Generator - Stateful generator maintaining receipt chain
 *
 * @example
 * const generator = new ReceiptGenerator();
 * const receipt = await generator.emitAdmissibilityReceipt({
 *   ontologyReleases: ['hash1', 'hash2'],
 *   deltaCapsule: 'hash3',
 *   decision: 'allow',
 *   universeState: someObject
 * });
 */
export class ReceiptGenerator {
  /**
   * Create a new receipt generator
   *
   * @param {object} [options] - Generator options
   * @param {string} [options.packageJsonPath] - Path to package.json (for toolchain versions)
   */
  constructor(options = {}) {
    this.chain = new ReceiptChain();
    this.toolchainVersion = null;
    this.packageJsonPath = options.packageJsonPath;
  }

  /**
   * Get current toolchain version
   *
   * **Includes**:
   * - Node.js version
   * - Key package versions (from package.json)
   *
   * @returns {object} {node: string, packages: {[key]: string}}
   */
  getToolchainVersion() {
    if (this.toolchainVersion) {
      return this.toolchainVersion;
    }

    const nodeVersion = process.version;
    const packages = {};

    // Try to read package.json for dependency versions
    if (this.packageJsonPath) {
      try {
        const packageJson = JSON.parse(readFileSync(this.packageJsonPath, 'utf-8'));
        const deps = {
          ...packageJson.dependencies,
          ...packageJson.devDependencies,
        };

        // Include key packages
        const keyPackages = [
          '@unrdf/core',
          '@unrdf/oxigraph',
          'hash-wasm',
          'zod',
          'n3',
        ];

        for (const pkg of keyPackages) {
          if (deps[pkg]) {
            packages[pkg] = deps[pkg];
          }
        }
      } catch {
        // If package.json can't be read, just use node version
      }
    }

    this.toolchainVersion = { node: nodeVersion, packages };
    return this.toolchainVersion;
  }

  /**
   * Compute output hash from universe state
   *
   * @param {any} universeState - Universe state object
   * @returns {Promise<string>} BLAKE3 hash
   */
  async computeOutputHash(universeState) {
    // Canonical JSON serialization
    const canonical = JSON.stringify(universeState, Object.keys(universeState).sort());
    return blake3(canonical);
  }

  /**
   * Emit receipt for admissibility decision
   *
   * @param {object} options - Receipt options
   * @param {string[]} options.ontologyReleases - Ontology release hashes
   * @param {string} options.deltaCapsule - Δ capsule hash
   * @param {'allow'|'deny'} options.decision - Admissibility decision
   * @param {any} options.universeState - Resulting universe state
   * @param {Date} [options.timestamp] - Generation timestamp
   * @returns {Promise<Receipt>} Emitted receipt
   */
  async emitAdmissibilityReceipt(options) {
    const outputHash = await this.computeOutputHash(options.universeState);
    const beforeHash = this.chain.getLast()?.receiptHash || null;

    const receipt = await Receipt.create({
      inputHashes: {
        ontologyReleases: options.ontologyReleases,
        deltaCapsule: options.deltaCapsule,
      },
      decision: options.decision,
      outputHash,
      toolchainVersion: this.getToolchainVersion(),
      beforeHash,
      timestamp: options.timestamp,
    });

    await this.chain.append(receipt);
    return receipt;
  }

  /**
   * Emit receipt for validation run
   *
   * @param {object} options - Receipt options
   * @param {string[]} options.ontologyReleases - Ontology release hashes
   * @param {string} options.validationReport - Validation report hash
   * @param {'allow'|'deny'} options.decision - Validation result (allow = pass, deny = fail)
   * @param {any} options.validationState - Validation state
   * @param {Date} [options.timestamp] - Generation timestamp
   * @returns {Promise<Receipt>} Emitted receipt
   */
  async emitValidationReceipt(options) {
    const outputHash = await this.computeOutputHash(options.validationState);
    const beforeHash = this.chain.getLast()?.receiptHash || null;

    const receipt = await Receipt.create({
      inputHashes: {
        ontologyReleases: options.ontologyReleases,
        deltaCapsule: options.validationReport,
      },
      decision: options.decision,
      outputHash,
      toolchainVersion: this.getToolchainVersion(),
      beforeHash,
      timestamp: options.timestamp,
    });

    await this.chain.append(receipt);
    return receipt;
  }

  /**
   * Emit receipt for projection operation
   *
   * @param {object} options - Receipt options
   * @param {string[]} options.ontologyReleases - Ontology release hashes
   * @param {string} options.projectionInput - Projection input hash
   * @param {'allow'|'deny'} options.decision - Projection result
   * @param {any} options.projectionOutput - Projection output state
   * @param {Date} [options.timestamp] - Generation timestamp
   * @returns {Promise<Receipt>} Emitted receipt
   */
  async emitProjectionReceipt(options) {
    const outputHash = await this.computeOutputHash(options.projectionOutput);
    const beforeHash = this.chain.getLast()?.receiptHash || null;

    const receipt = await Receipt.create({
      inputHashes: {
        ontologyReleases: options.ontologyReleases,
        deltaCapsule: options.projectionInput,
      },
      decision: options.decision,
      outputHash,
      toolchainVersion: this.getToolchainVersion(),
      beforeHash,
      timestamp: options.timestamp,
    });

    await this.chain.append(receipt);
    return receipt;
  }

  /**
   * Get receipt chain
   *
   * @returns {ReceiptChain}
   */
  getChain() {
    return this.chain;
  }

  /**
   * Verify entire receipt chain
   *
   * @returns {Promise<{valid: boolean, errors: string[]}>}
   */
  async verifyChain() {
    return this.chain.verify();
  }

  /**
   * Export all receipts to JSON-LD
   *
   * @returns {object}
   */
  toJSONLD() {
    return this.chain.toJSONLD();
  }
}
