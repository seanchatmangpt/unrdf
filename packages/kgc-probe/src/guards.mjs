/**
 * @fileoverview KGC Probe - Guard Registry and Implementations
 *
 * Guards validate observations against quality thresholds.
 * They run after agent execution to detect problematic scan results.
 *
 * Guard Registry Pattern:
 * - Quality checks (confidence, coverage)
 * - Severity limits (max critical observations)
 * - Completeness verification
 *
 * @module @unrdf/kgc-probe/guards
 */

import { GuardConfigSchema } from './types.mjs';

/**
 * Guard interface definition
 * @typedef {Object} Guard
 * @property {string} id - Guard identifier
 * @property {Function} validate - Validation function
 */

/**
 * GuardRegistry - Manages guard registration and execution
 *
 * @class GuardRegistry
 */
export class GuardRegistry {
  /**
   * Create guard registry with default guards
   * @param {Object} [config] - Guard configuration
   */
  constructor(config = {}) {
    this.guards = new Map();
    this.config = config;

    // Register default guards
    this.registerDefault();
  }

  /**
   * Register default guards
   * @private
   */
  registerDefault() {
    // Guard 1: Quality check (confidence and observation count)
    this.register('quality_check', {
      id: 'quality_check',
      validate: (observations) => this.validateQuality(observations)
    });

    // Guard 2: Completeness verification
    this.register('completeness_check', {
      id: 'completeness_check',
      validate: (observations) => this.validateCompleteness(observations)
    });

    // Guard 3: Severity limits
    this.register('severity_limit', {
      id: 'severity_limit',
      validate: (observations) => this.validateSeverity(observations)
    });

    // Guard 4: Artifact integrity
    this.register('integrity_check', {
      id: 'integrity_check',
      validate: (observations) => this.validateIntegrity(observations)
    });

    // Guard 5: Agent coverage
    this.register('agent_coverage', {
      id: 'agent_coverage',
      validate: (observations) => this.validateAgentCoverage(observations)
    });
  }

  /**
   * Register custom guard
   * @param {string} id - Guard identifier
   * @param {Guard} guard - Guard implementation
   */
  register(id, guard) {
    this.guards.set(id, guard);
  }

  /**
   * Get guard by ID
   * @param {string} id - Guard identifier
   * @returns {Guard | undefined}
   */
  get(id) {
    return this.guards.get(id);
  }

  /**
   * List all guard IDs
   * @returns {string[]}
   */
  list() {
    return Array.from(this.guards.keys());
  }

  /**
   * Run single guard
   * @param {string} guardId - Guard identifier
   * @param {Array} observations - Observations to validate
   * @returns {Object[]} Violations found
   */
  validate(guardId, observations) {
    const guard = this.guards.get(guardId);
    if (!guard) {
      throw new Error(`Guard not found: ${guardId}`);
    }

    return guard.validate(observations);
  }

  /**
   * Run all guards
   * @param {Array} observations - Observations to validate
   * @returns {Object[]} All violations from all guards
   */
  validateAll(observations) {
    const allViolations = [];

    for (const [guardId, guard] of this.guards) {
      try {
        const violations = guard.validate(observations);
        allViolations.push(...violations);
      } catch (err) {
        console.error(`Guard ${guardId} error:`, err);
      }
    }

    return allViolations;
  }

  // =========================================================================
  // GUARD IMPLEMENTATIONS
  // =========================================================================

  /**
   * Validate observation quality
   * Checks: low confidence count, average metrics
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateQuality(observations) {
    const violations = [];
    const thresholds = {
      critical_observations: 50,
      confidence_min: 0.6
    };

    // Count low-confidence observations
    const lowConfidence = observations.filter(
      o => o.metrics?.confidence < thresholds.confidence_min
    );

    if (lowConfidence.length > thresholds.critical_observations) {
      violations.push({
        guard_id: 'quality_check',
        severity: 'warning',
        details: {
          message: 'High count of low-confidence observations',
          count: lowConfidence.length,
          threshold: thresholds.critical_observations,
          confidence_min: thresholds.confidence_min
        }
      });
    }

    // Calculate average confidence
    const avgConfidence = observations.length > 0
      ? observations.reduce((sum, o) => sum + (o.metrics?.confidence || 0), 0) / observations.length
      : 1.0;

    if (avgConfidence < 0.7) {
      violations.push({
        guard_id: 'quality_check',
        severity: 'warning',
        details: {
          message: 'Average confidence below 70%',
          actual: avgConfidence,
          threshold: 0.7
        }
      });
    }

    return violations;
  }

  /**
   * Validate completeness observations
   * Checks: coverage metrics from completeness agent
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateCompleteness(observations) {
    const violations = [];
    const thresholds = {
      coverage_min: 0.7
    };

    const completenessObs = observations.filter(
      o => o.kind === 'completeness' || o.kind === 'completeness_level'
    );

    if (completenessObs.length === 0) {
      return []; // No completeness observations, pass
    }

    const avgCoverage = completenessObs.reduce(
      (sum, o) => sum + (o.metrics?.coverage || 0),
      0
    ) / completenessObs.length;

    if (avgCoverage < thresholds.coverage_min) {
      violations.push({
        guard_id: 'completeness_check',
        severity: 'warning',
        details: {
          message: 'Data coverage below threshold',
          coverage: avgCoverage,
          threshold: thresholds.coverage_min,
          observations_checked: completenessObs.length
        }
      });
    }

    return violations;
  }

  /**
   * Validate severity limits
   * Checks: count of critical observations
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateSeverity(observations) {
    const violations = [];
    const thresholds = {
      critical_limit: 10
    };

    const criticalCount = observations.filter(o => o.severity === 'critical').length;

    if (criticalCount > thresholds.critical_limit) {
      violations.push({
        guard_id: 'severity_limit',
        severity: 'critical',
        details: {
          message: 'Critical violations exceed limit',
          count: criticalCount,
          limit: thresholds.critical_limit
        }
      });
    }

    return violations;
  }

  /**
   * Validate artifact integrity
   * Checks: observation structure and required fields
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateIntegrity(observations) {
    const violations = [];
    let malformed = 0;

    for (const obs of observations) {
      // Check required fields
      if (!obs.id || !obs.agent || !obs.timestamp || !obs.kind) {
        malformed++;
      }
      // Check metrics structure
      if (!obs.metrics || typeof obs.metrics.confidence !== 'number') {
        malformed++;
      }
    }

    if (malformed > 0) {
      violations.push({
        guard_id: 'integrity_check',
        severity: 'critical',
        details: {
          message: 'Malformed observations detected',
          count: malformed,
          total: observations.length
        }
      });
    }

    return violations;
  }

  /**
   * Validate agent coverage
   * Checks: at least one observation per expected agent
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateAgentCoverage(observations) {
    const violations = [];
    const expectedAgents = 10; // 10 probe agents

    const uniqueAgents = new Set(
      observations
        .filter(o => !o.agent.startsWith('guard:'))
        .map(o => o.agent)
    );

    // Warn if fewer than 70% of agents produced observations
    const coverageRatio = uniqueAgents.size / expectedAgents;
    if (coverageRatio < 0.7) {
      violations.push({
        guard_id: 'agent_coverage',
        severity: 'warning',
        details: {
          message: 'Agent coverage below 70%',
          agents_active: uniqueAgents.size,
          agents_expected: expectedAgents,
          coverage: coverageRatio
        }
      });
    }

    return violations;
  }
}

/**
 * Create GuardRegistry instance
 * @param {Object} [config] - Guard configuration
 * @returns {GuardRegistry} New guard registry
 */
export function createGuardRegistry(config) {
  return new GuardRegistry(config);
}
