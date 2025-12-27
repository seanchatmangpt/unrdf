/**
 * @fileoverview Admission Engine - Gates all changes via Δ capsules and invariants
 *
 * The Admission Engine is the central gatekeeper for all RDF store modifications.
 * It validates proposed changes against:
 * 1. Forbidden operations (H) - Hard blocks
 * 2. Invariants (Q) - Checkable properties
 * 3. Partition policies
 *
 * **Decision Flow**:
 * 1. Receive Δ capsule
 * 2. Run forbidden operation guards → DENY if blocked
 * 3. Run enabled invariant checks → DENY if critical invariants fail
 * 4. Return decision (ALLOW/DENY) with complete reasoning
 *
 * @module admission/admission-engine
 */

import { z } from 'zod';
import { DeltaCapsule } from './delta-capsule.mjs';
import { checkForbiddenOperations, isProtectedPartition } from './forbidden-operations.mjs';
import { checkAllInvariants } from './invariants.mjs';

/**
 * Decision result schema
 */
export const DecisionResultSchema = z.object({
  allowed: z.boolean(),
  decision: z.enum(['ALLOW', 'DENY']),
  reason: z.string(),
  capsuleId: z.string(),
  timestamp: z.string().datetime(),
  checks: z.object({
    forbiddenOperations: z.object({
      passed: z.boolean(),
      blockedBy: z.array(z.string()).optional()
    }),
    invariants: z.object({
      passed: z.boolean(),
      failedInvariants: z.array(z.string()).optional()
    })
  }),
  details: z.record(z.any()).optional()
});

/**
 * Admission Engine Configuration Schema
 */
export const AdmissionConfigSchema = z.object({
  strictMode: z.boolean().default(true),
  allowWarnings: z.boolean().default(true),
  customBounds: z.record(z.number()).optional(),
  customInvariants: z.array(z.function()).optional(),
  auditLog: z.boolean().default(true)
});

/**
 * Admission Engine - Validates and gates all RDF changes
 *
 * @class AdmissionEngine
 * @example
 * const engine = new AdmissionEngine({ strictMode: true });
 * const decision = await engine.admitCapsule(deltaCapsule);
 * if (decision.allowed) {
 *   // Apply changes
 * } else {
 *   console.error('Denied:', decision.reason);
 * }
 */
export class AdmissionEngine {
  /**
   * Create a new Admission Engine
   * @param {Object} [config] - Engine configuration
   * @param {boolean} [config.strictMode=true] - Fail on any invariant violation
   * @param {boolean} [config.allowWarnings=true] - Allow warnings but not errors
   * @param {Object} [config.customBounds] - Custom complexity bounds
   * @param {Array} [config.customInvariants] - Additional invariant checks
   * @param {boolean} [config.auditLog=true] - Enable audit logging
   */
  constructor(config = {}) {
    this.config = AdmissionConfigSchema.parse(config);
    this.auditLog = [];
    this.stats = {
      totalProcessed: 0,
      allowed: 0,
      denied: 0,
      deniedByGuards: 0,
      deniedByInvariants: 0
    };
  }

  /**
   * Process a Delta Capsule and make admission decision
   *
   * @param {DeltaCapsule|Object} capsule - Capsule to process
   * @param {Object} [options] - Processing options
   * @returns {Object} Decision result
   */
  async admitCapsule(capsule, options = {}) {
    // Ensure capsule is DeltaCapsule instance
    if (!(capsule instanceof DeltaCapsule)) {
      try {
        capsule = new DeltaCapsule(capsule);
      } catch (error) {
        return this._createDenialDecision(
          null,
          'INVALID_CAPSULE',
          `Invalid capsule structure: ${error.message}`,
          { error: error.message }
        );
      }
    }

    this.stats.totalProcessed++;

    const timestamp = new Date().toISOString();
    const capsuleId = capsule.id;

    // Step 1: Run forbidden operation guards
    const guardResults = checkForbiddenOperations(capsule);

    if (!guardResults.allowed) {
      this.stats.denied++;
      this.stats.deniedByGuards++;

      const decision = {
        allowed: false,
        decision: 'DENY',
        reason: guardResults.summary,
        capsuleId,
        timestamp,
        checks: {
          forbiddenOperations: {
            passed: false,
            blockedBy: guardResults.blockedBy
          },
          invariants: {
            passed: null, // Not checked
            failedInvariants: []
          }
        },
        details: {
          guards: guardResults.guards,
          phase: 'GUARD_CHECK'
        }
      };

      this._logDecision(decision, capsule);
      return decision;
    }

    // Step 2: Run invariant checks
    const invariantOptions = {
      bounds: this.config.customBounds,
      ...options
    };

    const invariantResults = checkAllInvariants(capsule, invariantOptions);

    // Determine if invariant failures should block
    let invariantsPassed = invariantResults.passed;

    if (!this.config.strictMode && this.config.allowWarnings) {
      // In lenient mode, only block on errors not warnings
      invariantsPassed = invariantResults.errorCount === 0;
    }

    if (!invariantsPassed) {
      this.stats.denied++;
      this.stats.deniedByInvariants++;

      const failedInvariants = invariantResults.failures
        .filter(f => this.config.strictMode || f.strictness === 'error')
        .map(f => f.invariant);

      const decision = {
        allowed: false,
        decision: 'DENY',
        reason: invariantResults.summary,
        capsuleId,
        timestamp,
        checks: {
          forbiddenOperations: {
            passed: true,
            blockedBy: []
          },
          invariants: {
            passed: false,
            failedInvariants
          }
        },
        details: {
          invariants: invariantResults.results,
          failures: invariantResults.failures,
          phase: 'INVARIANT_CHECK'
        }
      };

      this._logDecision(decision, capsule);
      return decision;
    }

    // Step 3: Run custom invariants (if any)
    if (this.config.customInvariants && this.config.customInvariants.length > 0) {
      const customResults = [];

      for (const customInvariant of this.config.customInvariants) {
        try {
          const result = await customInvariant(capsule, options);
          customResults.push(result);

          if (!result.passed && this.config.strictMode) {
            this.stats.denied++;
            this.stats.deniedByInvariants++;

            const decision = {
              allowed: false,
              decision: 'DENY',
              reason: `Custom invariant failed: ${result.reason}`,
              capsuleId,
              timestamp,
              checks: {
                forbiddenOperations: { passed: true, blockedBy: [] },
                invariants: { passed: false, failedInvariants: [result.invariantName] }
              },
              details: {
                customInvariant: result,
                phase: 'CUSTOM_INVARIANT_CHECK'
              }
            };

            this._logDecision(decision, capsule);
            return decision;
          }
        } catch (error) {
          // Custom invariant threw error
          if (this.config.strictMode) {
            this.stats.denied++;

            const decision = {
              allowed: false,
              decision: 'DENY',
              reason: `Custom invariant error: ${error.message}`,
              capsuleId,
              timestamp,
              checks: {
                forbiddenOperations: { passed: true, blockedBy: [] },
                invariants: { passed: false, failedInvariants: ['custom'] }
              },
              details: {
                error: error.message,
                phase: 'CUSTOM_INVARIANT_ERROR'
              }
            };

            this._logDecision(decision, capsule);
            return decision;
          }
        }
      }
    }

    // All checks passed - ALLOW
    this.stats.allowed++;

    const decision = {
      allowed: true,
      decision: 'ALLOW',
      reason: `All checks passed: ${guardResults.guards.length} guard(s), ${invariantResults.results.length} invariant(s)`,
      capsuleId,
      timestamp,
      checks: {
        forbiddenOperations: {
          passed: true,
          blockedBy: []
        },
        invariants: {
          passed: true,
          failedInvariants: []
        }
      },
      details: {
        guards: guardResults.guards,
        invariants: invariantResults.results,
        capsuleStats: {
          quadCount: capsule.getQuadCount(),
          isAdditiveOnly: capsule.isAdditiveOnly(),
          namespaces: Array.from(capsule.getNamespaces())
        }
      }
    };

    this._logDecision(decision, capsule);
    return decision;
  }

  /**
   * Create a denial decision
   * @param {string} capsuleId - Capsule ID
   * @param {string} code - Denial code
   * @param {string} reason - Denial reason
   * @param {Object} details - Additional details
   * @returns {Object} Decision result
   * @private
   */
  _createDenialDecision(capsuleId, code, reason, details = {}) {
    this.stats.denied++;

    return {
      allowed: false,
      decision: 'DENY',
      reason,
      capsuleId: capsuleId || 'UNKNOWN',
      timestamp: new Date().toISOString(),
      checks: {
        forbiddenOperations: { passed: null, blockedBy: [] },
        invariants: { passed: null, failedInvariants: [] }
      },
      details: {
        code,
        ...details
      }
    };
  }

  /**
   * Log decision to audit log
   * @param {Object} decision - Decision result
   * @param {DeltaCapsule} capsule - Related capsule
   * @private
   */
  _logDecision(decision, capsule) {
    if (!this.config.auditLog) return;

    this.auditLog.push({
      timestamp: decision.timestamp,
      capsuleId: decision.capsuleId,
      decision: decision.decision,
      reason: decision.reason,
      partition: capsule ? capsule.partition.name : null,
      agent: capsule ? capsule.provenance.agent : null
    });

    // Keep audit log bounded (last 1000 entries)
    if (this.auditLog.length > 1000) {
      this.auditLog.shift();
    }
  }

  /**
   * Batch process multiple capsules
   * @param {Array<DeltaCapsule>} capsules - Capsules to process
   * @param {Object} [options] - Processing options
   * @returns {Array<Object>} Decision results
   */
  async admitBatch(capsules, options = {}) {
    const results = [];

    for (const capsule of capsules) {
      const decision = await this.admitCapsule(capsule, options);
      results.push(decision);

      // In strict mode, stop on first denial
      if (this.config.strictMode && !decision.allowed) {
        break;
      }
    }

    return results;
  }

  /**
   * Get engine statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      allowRate: this.stats.totalProcessed > 0
        ? (this.stats.allowed / this.stats.totalProcessed * 100).toFixed(2) + '%'
        : 'N/A',
      denyRate: this.stats.totalProcessed > 0
        ? (this.stats.denied / this.stats.totalProcessed * 100).toFixed(2) + '%'
        : 'N/A'
    };
  }

  /**
   * Get recent audit log entries
   * @param {number} [limit=100] - Number of entries to return
   * @returns {Array} Audit log entries
   */
  getAuditLog(limit = 100) {
    return this.auditLog.slice(-limit);
  }

  /**
   * Clear audit log
   */
  clearAuditLog() {
    this.auditLog = [];
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
      deniedByInvariants: 0
    };
  }

  /**
   * Validate capsule without making decision (dry-run)
   * @param {DeltaCapsule|Object} capsule - Capsule to validate
   * @returns {Object} Validation result
   */
  async validateCapsule(capsule) {
    // Temporarily disable stats tracking
    const originalTotalProcessed = this.stats.totalProcessed;

    const decision = await this.admitCapsule(capsule);

    // Restore stats (validation doesn't count)
    this.stats.totalProcessed = originalTotalProcessed;
    if (decision.allowed) {
      this.stats.allowed--;
    } else {
      this.stats.denied--;
      if (decision.details.phase === 'GUARD_CHECK') {
        this.stats.deniedByGuards--;
      } else {
        this.stats.deniedByInvariants--;
      }
    }

    // Remove from audit log
    if (this.config.auditLog && this.auditLog.length > 0) {
      this.auditLog.pop();
    }

    return {
      valid: decision.allowed,
      decision,
      isDryRun: true
    };
  }
}

/**
 * Create a simple admission engine instance
 * @param {Object} [config] - Engine configuration
 * @returns {AdmissionEngine} New engine instance
 */
export function createAdmissionEngine(config = {}) {
  return new AdmissionEngine(config);
}

/**
 * Convenience function to check if a capsule would be admitted
 * @param {DeltaCapsule|Object} capsule - Capsule to check
 * @param {Object} [config] - Engine configuration
 * @returns {Promise<boolean>} True if would be admitted
 */
export async function wouldAdmit(capsule, config = {}) {
  const engine = new AdmissionEngine(config);
  const decision = await engine.admitCapsule(capsule);
  return decision.allowed;
}
