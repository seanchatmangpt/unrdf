/**
 * @fileoverview Monorepo Admission Engine - Atomic admission control for package changes
 *
 * The Monorepo Admission Engine provides atomic admission decisions:
 * - Either ALL packages in a delta are admitted, or ALL are denied
 * - No partial admissions - maintains monorepo consistency
 * - Generates cryptographic receipts for audit trails
 *
 * Admission Flow:
 * 1. Receive PackageDelta with proposed changes to N packages
 * 2. For each package: validate local constraints
 * 3. For the monorepo: validate cross-package constraints
 * 4. Emit atomic receipt: ALL admitted or ALL denied
 *
 * @module monorepo-admission/monorepo-admission-engine
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';
import { blake3 } from 'hash-wasm';

import {
  PackagePartition,
  PackageDelta,
  PackageChange
} from './package-partition.mjs';

import { checkAllCrossPackageInvariants } from './cross-package-invariants.mjs';
import { checkAllCrossPackageGuards } from './cross-package-guards.mjs';
import { MonorepoUniverse } from './monorepo-universe.mjs';

/**
 * Admission decision schema
 */
export const AdmissionDecisionSchema = z.object({
  admissionId: z.string().uuid(),
  timestamp: z.string().datetime(),
  admitted: z.boolean(),
  decision: z.enum(['ALLOW', 'DENY']),
  phase: z.enum(['GUARD_CHECK', 'INVARIANT_CHECK', 'PACKAGE_LOCAL', 'COMPLETE']),
  reason: z.string(),
  atomicGuarantee: z.boolean().default(true)
});

/**
 * Receipt schema for audit trail
 */
export const AdmissionReceiptSchema = z.object({
  receiptId: z.string().uuid(),
  admissionId: z.string().uuid(),
  timestamp: z.string().datetime(),
  decision: z.enum(['ALLOW', 'DENY']),
  inputHashes: z.object({
    deltaHash: z.string(),
    universeHashBefore: z.string(),
    partitionHashes: z.array(z.object({
      name: z.string(),
      hash: z.string()
    }))
  }),
  outputHashes: z.object({
    universeHashAfter: z.string().nullable(),
    receiptHash: z.string()
  }),
  checks: z.object({
    guardsRun: z.number(),
    guardsPassed: z.number(),
    invariantsRun: z.number(),
    invariantsPassed: z.number()
  }),
  affectedPackages: z.array(z.string()),
  epoch: z.string()
});

/**
 * Engine configuration schema
 */
export const EngineConfigSchema = z.object({
  strictMode: z.boolean().default(true),
  atomicAdmission: z.boolean().default(true),
  generateReceipts: z.boolean().default(true),
  enableLocalChecks: z.boolean().default(true),
  auditLog: z.boolean().default(true),
  maxBatchSize: z.number().default(50)
});

/**
 * MonorepoAdmissionEngine - Atomic admission control for monorepo changes
 *
 * @class MonorepoAdmissionEngine
 * @example
 * const engine = new MonorepoAdmissionEngine(universe, { strictMode: true });
 *
 * const delta = new PackageDelta({
 *   agent: 'ci-pipeline',
 *   changes: [
 *     { packageName: '@unrdf/core', changeType: 'version_bump', details: { ... } },
 *     { packageName: '@unrdf/hooks', changeType: 'dependency_update', details: { ... } }
 *   ]
 * });
 *
 * const result = await engine.admit(delta);
 * if (result.admitted) {
 *   console.log('Receipt:', result.receipt.receiptId);
 * } else {
 *   console.error('Denied:', result.reason);
 * }
 */
export class MonorepoAdmissionEngine {
  /**
   * Create a new MonorepoAdmissionEngine
   * @param {MonorepoUniverse} universe - The monorepo universe
   * @param {Object} config - Engine configuration
   */
  constructor(universe, config = {}) {
    this.universe = universe;
    this.config = EngineConfigSchema.parse(config);

    /** @type {Array<Object>} Audit log entries */
    this.auditLog = [];

    /** @type {Array<Object>} Generated receipts */
    this.receipts = [];

    /** @type {Object} Statistics */
    this.stats = {
      totalProcessed: 0,
      allowed: 0,
      denied: 0,
      deniedByGuards: 0,
      deniedByInvariants: 0,
      deniedByLocal: 0
    };
  }

  /**
   * Admit a delta (atomic admission)
   * @param {PackageDelta|Object} delta - Delta to admit
   * @param {Object} options - Admission options
   * @returns {Promise<Object>} Admission result with receipt
   */
  async admit(delta, options = {}) {
    // Ensure delta is PackageDelta instance
    if (!(delta instanceof PackageDelta)) {
      try {
        delta = new PackageDelta(delta);
      } catch (error) {
        return this._createDenialResult(
          'INVALID_DELTA',
          `Invalid delta structure: ${error.message}`,
          null
        );
      }
    }

    const admissionId = crypto.randomUUID();
    const timestamp = new Date().toISOString();

    this.stats.totalProcessed++;

    // Validate batch size
    if (delta.changes.length > this.config.maxBatchSize) {
      return this._createDenialResult(
        'BATCH_TOO_LARGE',
        `Delta contains ${delta.changes.length} changes, max is ${this.config.maxBatchSize}`,
        delta
      );
    }

    // Capture universe state before
    const universeHashBefore = this.universe._contentHash;

    // Step 1: Run local package checks (if enabled)
    if (this.config.enableLocalChecks) {
      const localResult = await this._runLocalChecks(delta);

      if (!localResult.passed) {
        this.stats.denied++;
        this.stats.deniedByLocal++;

        const result = this._buildResult({
          admissionId,
          timestamp,
          admitted: false,
          decision: 'DENY',
          phase: 'PACKAGE_LOCAL',
          reason: localResult.reason,
          delta,
          universeHashBefore,
          localResult
        });

        this._logAdmission(result);
        return result;
      }
    }

    // Step 2: Run cross-package guards
    const guardResults = checkAllCrossPackageGuards(
      this.universe.partitions,
      delta,
      options
    );

    if (!guardResults.allowed) {
      this.stats.denied++;
      this.stats.deniedByGuards++;

      const result = this._buildResult({
        admissionId,
        timestamp,
        admitted: false,
        decision: 'DENY',
        phase: 'GUARD_CHECK',
        reason: guardResults.summary,
        delta,
        universeHashBefore,
        guardResults
      });

      this._logAdmission(result);
      return result;
    }

    // Step 3: Run cross-package invariants
    const invariantResults = checkAllCrossPackageInvariants(
      this.universe.partitions,
      delta,
      options
    );

    if (!invariantResults.passed) {
      this.stats.denied++;
      this.stats.deniedByInvariants++;

      const result = this._buildResult({
        admissionId,
        timestamp,
        admitted: false,
        decision: 'DENY',
        phase: 'INVARIANT_CHECK',
        reason: invariantResults.summary,
        delta,
        universeHashBefore,
        guardResults,
        invariantResults
      });

      this._logAdmission(result);
      return result;
    }

    // Step 4: All checks passed - ALLOW
    this.stats.allowed++;

    // Compute hypothetical new universe hash (if changes applied)
    const universeHashAfter = this._computeHypotheticalHash(delta);

    const result = this._buildResult({
      admissionId,
      timestamp,
      admitted: true,
      decision: 'ALLOW',
      phase: 'COMPLETE',
      reason: `All checks passed: ${guardResults.guards.length} guards, ${invariantResults.results.length} invariants`,
      delta,
      universeHashBefore,
      universeHashAfter,
      guardResults,
      invariantResults
    });

    // Generate receipt
    if (this.config.generateReceipts) {
      result.receipt = await this._generateReceipt(result);
      this.receipts.push(result.receipt);
    }

    this._logAdmission(result);
    return result;
  }

  /**
   * Run local package checks
   * @param {PackageDelta} delta - Delta to check
   * @returns {Object} Local check results
   * @private
   */
  async _runLocalChecks(delta) {
    const failures = [];

    for (const change of delta.changes) {
      const partition = this.universe.getPartition(change.packageName);

      if (!partition) {
        failures.push({
          package: change.packageName,
          reason: 'Package not found in universe'
        });
        continue;
      }

      // Check if trying to modify protected package without proper auth
      if (partition.isProtectedPartition()) {
        const changeObj = new PackageChange(change);

        if (changeObj.isBreaking()) {
          // Breaking changes to protected packages need special handling
          // This would typically check for approval in change.details
          if (!change.details?.approvedBy) {
            failures.push({
              package: change.packageName,
              changeType: change.changeType,
              reason: 'Breaking change to protected package requires approval'
            });
          }
        }
      }

      // Validate version bumps are valid
      if (change.changeType === 'version_bump') {
        const newVersion = change.details?.newVersion;
        if (newVersion) {
          try {
            // Validate semantic version format
            const match = newVersion.match(/^(\d+)\.(\d+)\.(\d+)/);
            if (!match) {
              failures.push({
                package: change.packageName,
                newVersion,
                reason: 'Invalid semantic version format'
              });
            }
          } catch {
            failures.push({
              package: change.packageName,
              reason: 'Failed to validate version'
            });
          }
        }
      }
    }

    return {
      passed: failures.length === 0,
      failures,
      reason: failures.length > 0
        ? `Local check failed: ${failures.length} package(s) have issues`
        : 'All local checks passed'
    };
  }

  /**
   * Build admission result object
   * @param {Object} params - Result parameters
   * @returns {Object} Complete result
   * @private
   */
  _buildResult(params) {
    const {
      admissionId,
      timestamp,
      admitted,
      decision,
      phase,
      reason,
      delta,
      universeHashBefore,
      universeHashAfter,
      localResult,
      guardResults,
      invariantResults
    } = params;

    return {
      admissionId,
      timestamp,
      admitted,
      decision,
      phase,
      reason,
      atomicGuarantee: this.config.atomicAdmission,
      deltaHash: delta?.getHash?.() || null,
      universeHashBefore,
      universeHashAfter: admitted ? universeHashAfter : null,
      affectedPackages: delta ? Array.from(delta.getAffectedPackages()) : [],
      checks: {
        local: localResult || null,
        guards: guardResults || null,
        invariants: invariantResults || null
      },
      stats: {
        changesProposed: delta?.changes?.length || 0,
        packagesAffected: delta?.getAffectedPackages?.()?.size || 0,
        hasBreakingChanges: delta?.hasBreakingChanges?.() || false
      }
    };
  }

  /**
   * Create a denial result
   * @param {string} code - Denial code
   * @param {string} reason - Denial reason
   * @param {PackageDelta} delta - Related delta
   * @returns {Object} Denial result
   * @private
   */
  _createDenialResult(code, reason, delta) {
    this.stats.denied++;

    return {
      admissionId: crypto.randomUUID(),
      timestamp: new Date().toISOString(),
      admitted: false,
      decision: 'DENY',
      phase: 'VALIDATION',
      reason,
      code,
      atomicGuarantee: true,
      deltaHash: delta?.getHash?.() || null,
      affectedPackages: delta ? Array.from(delta.getAffectedPackages()) : []
    };
  }

  /**
   * Compute hypothetical universe hash after applying delta
   * @param {PackageDelta} delta - Delta to apply
   * @returns {string} Hypothetical hash
   * @private
   */
  _computeHypotheticalHash(delta) {
    // Create a snapshot of current hashes
    const partitionHashes = [];

    for (const [name, partition] of this.universe.partitions) {
      // If package is in delta, compute hypothetical new hash
      if (delta.getAffectedPackages().has(name)) {
        // Simplified: just include delta hash in computation
        const combinedHash = createHash('sha256')
          .update(partition.getHash())
          .update(delta.getHash())
          .digest('hex');

        partitionHashes.push({ name, hash: combinedHash });
      } else {
        partitionHashes.push({ name, hash: partition.getHash() });
      }
    }

    // Sort and hash
    partitionHashes.sort((a, b) => a.name.localeCompare(b.name));

    return createHash('sha256')
      .update(JSON.stringify(partitionHashes))
      .digest('hex');
  }

  /**
   * Generate admission receipt
   * @param {Object} result - Admission result
   * @returns {Promise<Object>} Receipt
   * @private
   */
  async _generateReceipt(result) {
    const receiptId = crypto.randomUUID();
    const epoch = this._generateEpoch();

    // Collect partition hashes for affected packages
    const partitionHashes = result.affectedPackages.map(name => ({
      name,
      hash: this.universe.getPartition(name)?.getHash() || 'unknown'
    }));

    const receiptData = {
      receiptId,
      admissionId: result.admissionId,
      timestamp: result.timestamp,
      decision: result.decision,
      inputHashes: {
        deltaHash: result.deltaHash,
        universeHashBefore: result.universeHashBefore,
        partitionHashes
      },
      outputHashes: {
        universeHashAfter: result.universeHashAfter
      },
      checks: {
        guardsRun: result.checks.guards?.guards?.length || 0,
        guardsPassed: result.checks.guards?.guards?.filter(g => g.allowed)?.length || 0,
        invariantsRun: result.checks.invariants?.results?.length || 0,
        invariantsPassed: result.checks.invariants?.results?.filter(i => i.passed)?.length || 0
      },
      affectedPackages: result.affectedPackages,
      epoch
    };

    // Compute receipt hash
    const receiptHash = await this._computeReceiptHash(receiptData);
    receiptData.outputHashes.receiptHash = receiptHash;

    return AdmissionReceiptSchema.parse(receiptData);
  }

  /**
   * Compute receipt hash using BLAKE3
   * @param {Object} receiptData - Receipt data
   * @returns {Promise<string>} BLAKE3 hash
   * @private
   */
  async _computeReceiptHash(receiptData) {
    const canonical = JSON.stringify({
      admissionId: receiptData.admissionId,
      decision: receiptData.decision,
      inputHashes: receiptData.inputHashes,
      epoch: receiptData.epoch
    });

    return blake3(canonical);
  }

  /**
   * Generate epoch string
   * @returns {string} Epoch string
   * @private
   */
  _generateEpoch() {
    const now = new Date();
    const year = now.getUTCFullYear();
    const month = String(now.getUTCMonth() + 1).padStart(2, '0');
    const day = String(now.getUTCDate()).padStart(2, '0');
    const hour = String(now.getUTCHours()).padStart(2, '0');
    const minute = String(now.getUTCMinutes()).padStart(2, '0');
    const ms = String(now.getUTCMilliseconds()).padStart(3, '0');

    return `tau_${year}_${month}_${day}_${hour}${minute}_${ms}`;
  }

  /**
   * Log admission to audit log
   * @param {Object} result - Admission result
   * @private
   */
  _logAdmission(result) {
    if (!this.config.auditLog) return;

    this.auditLog.push({
      timestamp: result.timestamp,
      admissionId: result.admissionId,
      decision: result.decision,
      phase: result.phase,
      reason: result.reason,
      affectedPackages: result.affectedPackages
    });

    // Keep bounded
    if (this.auditLog.length > 1000) {
      this.auditLog.shift();
    }
  }

  /**
   * Admit multiple deltas (batch admission)
   * All must pass for any to be admitted (super-atomic)
   * @param {Array<PackageDelta>} deltas - Deltas to admit
   * @param {Object} options - Admission options
   * @returns {Promise<Object>} Batch admission result
   */
  async admitBatch(deltas, options = {}) {
    const batchId = crypto.randomUUID();
    const results = [];
    const allPassed = [];

    // First pass: check all deltas
    for (const delta of deltas) {
      const result = await this.admit(delta, { ...options, dryRun: true });
      results.push(result);
      allPassed.push(result.admitted);
    }

    // Super-atomic: all must pass
    const batchAdmitted = allPassed.every(p => p === true);

    if (batchAdmitted) {
      return {
        batchId,
        timestamp: new Date().toISOString(),
        admitted: true,
        decision: 'ALLOW',
        reason: `All ${deltas.length} deltas admitted`,
        results,
        atomicGuarantee: true
      };
    } else {
      // Find first failure for reason
      const firstFailure = results.find(r => !r.admitted);

      return {
        batchId,
        timestamp: new Date().toISOString(),
        admitted: false,
        decision: 'DENY',
        reason: `Batch denied: ${firstFailure.reason}`,
        results,
        atomicGuarantee: true,
        failedAt: results.findIndex(r => !r.admitted)
      };
    }
  }

  /**
   * Validate a delta without committing (dry run)
   * @param {PackageDelta} delta - Delta to validate
   * @param {Object} options - Validation options
   * @returns {Promise<Object>} Validation result
   */
  async validate(delta, options = {}) {
    // Save stats
    const savedStats = { ...this.stats };

    const result = await this.admit(delta, options);

    // Restore stats (validation doesn't count)
    this.stats = savedStats;

    // Remove from audit log
    if (this.config.auditLog && this.auditLog.length > 0) {
      this.auditLog.pop();
    }

    // Remove receipt
    if (result.receipt && this.receipts.length > 0) {
      this.receipts.pop();
    }

    return {
      valid: result.admitted,
      ...result,
      isDryRun: true
    };
  }

  /**
   * Get engine statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      allowRate: this.stats.totalProcessed > 0
        ? `${((this.stats.allowed / this.stats.totalProcessed) * 100).toFixed(2)}%`
        : 'N/A',
      receiptCount: this.receipts.length
    };
  }

  /**
   * Get recent audit log entries
   * @param {number} limit - Number of entries
   * @returns {Array<Object>} Audit log entries
   */
  getAuditLog(limit = 100) {
    return this.auditLog.slice(-limit);
  }

  /**
   * Get generated receipts
   * @param {number} limit - Number of receipts
   * @returns {Array<Object>} Receipts
   */
  getReceipts(limit = 100) {
    return this.receipts.slice(-limit);
  }

  /**
   * Verify a receipt
   * @param {Object} receipt - Receipt to verify
   * @returns {Promise<Object>} Verification result
   */
  async verifyReceipt(receipt) {
    try {
      // Recompute hash
      const recomputedHash = await this._computeReceiptHash({
        admissionId: receipt.admissionId,
        decision: receipt.decision,
        inputHashes: receipt.inputHashes,
        epoch: receipt.epoch
      });

      const hashValid = recomputedHash === receipt.outputHashes.receiptHash;

      return {
        valid: hashValid,
        receiptId: receipt.receiptId,
        hashValid,
        reason: hashValid ? 'Receipt hash verified' : 'Receipt hash mismatch'
      };
    } catch (error) {
      return {
        valid: false,
        receiptId: receipt.receiptId,
        reason: `Verification failed: ${error.message}`
      };
    }
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.stats = {
      totalProcessed: 0,
      allowed: 0,
      denied: 0,
      deniedByGuards: 0,
      deniedByInvariants: 0,
      deniedByLocal: 0
    };
  }

  /**
   * Clear audit log
   */
  clearAuditLog() {
    this.auditLog = [];
  }
}

/**
 * Create a MonorepoAdmissionEngine instance
 * @param {MonorepoUniverse} universe - Universe instance
 * @param {Object} config - Engine configuration
 * @returns {MonorepoAdmissionEngine} New engine instance
 */
export function createMonorepoAdmissionEngine(universe, config = {}) {
  return new MonorepoAdmissionEngine(universe, config);
}
