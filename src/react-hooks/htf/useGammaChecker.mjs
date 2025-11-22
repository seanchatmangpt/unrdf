/**
 * @fileoverview useGammaChecker - React hook for Γ-globalization validation
 * Ensures all Δ-shards obey Q-invariants and tracks drift toward μ-fixed point
 */

import { useState, useCallback, useEffect, useMemo } from 'react';
import { computeGammaGlobalization, StandardInvariants, evolveTowardMuFixed } from './htf-core.mjs';

/**
 * useGammaChecker hook - Validate thesis convergence to μ-fixed point
 *
 * @param {DeltaShard[]} shards - Shards to validate
 * @param {QueryInvariant[]} customInvariants - Additional Q-invariants to check
 * @param {Object} options - Validation options
 * @param {boolean} options.autoCheck - Auto-validate on shard changes
 * @param {Function} options.onViolation - Callback on invariant violation
 * @param {Function} options.onConvergence - Callback when θ → μ (converged)
 *
 * @returns {Object} Validation state and methods
 * @returns {ValidationReport} report - Current validation report
 * @returns {Violation[]} violations - All current violations
 * @returns {number} drift - Distance from μ-fixed point (0 = converged)
 * @returns {boolean} isConverged - Whether thesis has converged
 * @returns {TauEvolution} evolution - Temporal evolution state
 * @returns {Function} validateShards - Manually validate all shards
 * @returns {Function} fixViolations - Auto-fix violations
 * @returns {Function} addCustomInvariant - Add new Q to check
 * @returns {Function} removeCustomInvariant - Remove custom Q
 * @returns {Function} generateValidationReport - Export validation as report
 */
export function useGammaChecker(shards = [], customInvariants = [], options = {}) {
  const { autoCheck = true, onViolation = () => {}, onConvergence = () => {} } = options;

  const [validationState, setValidationState] = useState({
    timestamp: new Date(),
    violations: [],
    drift: 1,
    isConverged: false,
  });

  const [evolution, setEvolution] = useState({
    epoch: 0,
    state: {},
    distance: 1,
    isConverged: false,
  });
  const [customChecks, setCustomChecks] = useState(customInvariants);
  const [fixHistory, setFixHistory] = useState([]);

  // All invariants (standard + custom)
  const allInvariants = useMemo(() => {
    return [...StandardInvariants, ...customChecks];
  }, [customChecks]);

  // ====== VALIDATION ======

  /**
   * Validate all shards against Q-invariants
   */
  const validateShards = useCallback(() => {
    const gamma = computeGammaGlobalization(shards, allInvariants);

    // Check standard invariants
    const violations = [];

    for (const invariant of allInvariants) {
      for (const shard of shards) {
        if (invariant.appliesTo.includes(shard.family)) {
          try {
            const result = invariant.predicate(shard);
            if (!result) {
              violations.push({
                id: `${invariant.id}:${shard.id}`,
                invariantId: invariant.id,
                invariantDescription: invariant.description,
                shardId: shard.id,
                shardLabel: shard.label,
                severity: categorizeViolation(invariant.id),
                fixSuggestion: suggestFix(invariant.id, shard),
                timestamp: new Date(),
              });
            }
          } catch (e) {
            violations.push({
              id: `${invariant.id}:${shard.id}`,
              invariantId: invariant.id,
              invariantDescription: invariant.description,
              shardId: shard.id,
              shardLabel: shard.label,
              severity: 'error',
              errorMessage: e.message,
              timestamp: new Date(),
            });
          }
        }
      }
    }

    // Update validation state
    const isConverged = violations.length === 0 && gamma.drift < 0.05;
    const newState = {
      timestamp: new Date(),
      violations,
      drift: gamma.drift,
      isConverged,
      report: gamma,
    };

    setValidationState(newState);

    // Notify on violations
    if (violations.length > 0) {
      onViolation(violations);
    }

    // Notify on convergence
    if (isConverged && !validationState.isConverged) {
      onConvergence({ epoch: evolution.epoch, drift: gamma.drift });
    }

    return newState;
  }, [shards, allInvariants, validationState, evolution, onViolation, onConvergence]);

  // Auto-validate when shards change
  useEffect(() => {
    if (autoCheck) {
      validateShards();
    }
  }, [shards, autoCheck, validateShards]);

  // Track evolution
  useEffect(() => {
    const newEvolution = evolveTowardMuFixed(shards, evolution);
    setEvolution(newEvolution);
  }, [shards, evolution]);

  // ====== INVARIANT MANAGEMENT ======

  /**
   * Add a custom Q-invariant
   */
  const addCustomInvariant = useCallback(
    invariant => {
      setCustomChecks(prev => [...prev, invariant]);
      validateShards();
    },
    [validateShards]
  );

  /**
   * Remove custom Q-invariant
   */
  const removeCustomInvariant = useCallback(
    invariantId => {
      setCustomChecks(prev => prev.filter(inv => inv.id !== invariantId));
      validateShards();
    },
    [validateShards]
  );

  /**
   * Get all active invariants
   */
  const getActiveInvariants = useCallback(() => {
    return allInvariants.map(inv => ({
      id: inv.id,
      description: inv.description,
      appliesTo: inv.appliesTo,
      isCustom: !StandardInvariants.find(s => s.id === inv.id),
    }));
  }, [allInvariants]);

  // ====== FIXING VIOLATIONS ======

  /**
   * Suggest automatic fixes for violations
   */
  const suggestFixesForViolation = useCallback(
    violationId => {
      const violation = validationState.violations.find(v => v.id === violationId);
      if (!violation) return [];

      const shard = shards.find(s => s.id === violation.shardId);
      if (!shard) return [];

      const fixes = [];

      // Common fix patterns
      if (violation.invariantId === 'Q-coherence') {
        fixes.push({
          type: 'expand-content',
          description: 'Expand shard content to minimum 100 characters',
          action: () => ({ content: shard.content + ' [NEEDS EXPANSION]' }),
        });
      }

      if (violation.invariantId === 'Q-positioning') {
        fixes.push({
          type: 'validate-deps',
          description: 'Ensure all dependencies are valid shard IDs',
          action: () => ({
            dependencies: (shard.dependencies || []).filter(id => shards.some(s => s.id === id)),
          }),
        });
      }

      if (violation.invariantId === 'Q-contribution') {
        fixes.push({
          type: 'set-weight',
          description: 'Set shard weight > 0 to indicate contribution',
          action: () => ({ weight: Math.max(shard.weight || 0, 0.5) }),
        });
      }

      return fixes;
    },
    [validationState, shards]
  );

  /**
   * Auto-apply fixes to violations
   */
  const fixViolations = useCallback(
    (violationIds = null) => {
      const targetViolations = violationIds
        ? validationState.violations.filter(v => violationIds.includes(v.id))
        : validationState.violations;

      const fixes = [];
      const updatedShards = shards.map(shard => {
        let updated = { ...shard };

        for (const violation of targetViolations) {
          if (violation.shardId === shard.id) {
            const suggestedFix = suggestFixesForViolation(violation.id);
            if (suggestedFix.length > 0) {
              const fix = suggestedFix[0];
              const fixAction = fix.action();
              updated = { ...updated, ...fixAction };
              fixes.push({
                shardId: shard.id,
                violationId: violation.id,
                fix: fix.type,
                before: shard,
                after: updated,
              });
            }
          }
        }

        return updated;
      });

      // Record in history and update shards
      setFixHistory(prev => [...prev, { timestamp: new Date(), fixes, shards: updatedShards }]);

      // Re-validate
      validateShards();

      return { fixes, updatedShards };
    },
    [validationState, shards, suggestFixesForViolation, validateShards]
  );

  /**
   * Rollback to previous validation state
   */
  const rollbackToEpoch = useCallback(
    epochIndex => {
      if (fixHistory[epochIndex]) {
        const { shards: previousShards } = fixHistory[epochIndex];
        // Note: Would need to propagate back to parent component
        // This is a signal; actual update happens via parent state
        return previousShards;
      }
      return shards;
    },
    [fixHistory, shards]
  );

  // ====== REPORTING ======

  /**
   * Generate comprehensive validation report
   */
  const generateValidationReport = useCallback(
    (format = 'json') => {
      const report = {
        timestamp: validationState.timestamp,
        epoch: evolution.epoch,
        shards: shards.length,
        violations: validationState.violations.length,
        drift: validationState.drift,
        isConverged: validationState.isConverged,
        invariantsChecked: allInvariants.length,
        violations: validationState.violations,
        evolution,
        activeInvariants: getActiveInvariants(),
      };

      if (format === 'json') {
        return JSON.stringify(report, null, 2);
      } else if (format === 'markdown') {
        return generateMarkdownReport(report);
      } else if (format === 'html') {
        return generateHTMLReport(report);
      }

      return report;
    },
    [validationState, evolution, allInvariants, shards, getActiveInvariants]
  );

  /**
   * Get detailed violation analysis
   */
  const getViolationAnalysis = useCallback(() => {
    const byInvariant = {};
    const bySeverity = {};
    const byFamily = {};

    for (const violation of validationState.violations) {
      // By invariant
      if (!byInvariant[violation.invariantId]) {
        byInvariant[violation.invariantId] = [];
      }
      byInvariant[violation.invariantId].push(violation);

      // By severity
      if (!bySeverity[violation.severity]) {
        bySeverity[violation.severity] = [];
      }
      bySeverity[violation.severity].push(violation);

      // By family
      const shard = shards.find(s => s.id === violation.shardId);
      if (shard) {
        if (!byFamily[shard.family]) {
          byFamily[shard.family] = [];
        }
        byFamily[shard.family].push(violation);
      }
    }

    return { byInvariant, bySeverity, byFamily };
  }, [validationState, shards]);

  /**
   * Check if drift is decreasing (convergence in progress)
   */
  const isDriftDecreasing = useMemo(() => {
    if (evolution.epoch < 2) return false;
    return validationState.drift < evolution.distance;
  }, [validationState, evolution]);

  return {
    // Validation state
    state: {
      violations: validationState.violations,
      drift: validationState.drift,
      isConverged: validationState.isConverged,
      timestamp: validationState.timestamp,
    },

    // Evolution tracking
    evolution,
    isDriftDecreasing,

    // Validation operations
    validateShards,
    addCustomInvariant,
    removeCustomInvariant,
    getActiveInvariants,

    // Fixing violations
    suggestFixesForViolation,
    fixViolations,
    rollbackToEpoch,

    // Reporting
    generateValidationReport,
    getViolationAnalysis,
    getStats: () => ({
      totalViolations: validationState.violations.length,
      totalShards: shards.length,
      drift: validationState.drift,
      isConverged: validationState.isConverged,
      epoch: evolution.epoch,
      invariantsChecked: allInvariants.length,
    }),
  };
}

// ====== HELPERS ======

/**
 * Categorize violation severity
 */
function categorizeViolation(invariantId) {
  const severityMap = {
    'Q-coherence': 'warning',
    'Q-positioning': 'error',
    'Q-contribution': 'warning',
    'Q-imrad-closure': 'info',
    'Q-argument-chain': 'info',
  };
  return severityMap[invariantId] || 'info';
}

/**
 * Suggest fix for a violation
 */
function suggestFix(invariantId, shard) {
  const suggestions = {
    'Q-coherence': `Expand shard content (currently ${shard.content?.length || 0} chars, need ≥100)`,
    'Q-positioning': 'Verify all dependency IDs are valid',
    'Q-contribution': 'Set weight > 0 (currently ' + (shard.weight || 0) + ')',
    'Q-imrad-closure': `Ensure shard ID is in [intro, method, result, discuss]`,
    'Q-argument-chain': `Ensure shard ID is in [claim, ground, proof, objection, reply]`,
  };
  return suggestions[invariantId] || 'See invariant description for requirements';
}

function generateMarkdownReport(report) {
  let md = `# Γ-Globalization Validation Report\n\n`;

  md += `**Date:** ${report.timestamp.toISOString()}\n`;
  md += `**Epoch:** ${report.epoch}\n`;
  md += `**Thesis Status:** ${report.isConverged ? '✅ CONVERGED' : '⚠️ NOT CONVERGED'}\n\n`;

  md += `## Convergence Metrics\n\n`;
  md += `- **Drift:** ${(report.drift * 100).toFixed(1)}% (toward μ-fixed point)\n`;
  md += `- **Violations:** ${report.violations}/${report.shards} shards\n`;
  md += `- **Invariants Checked:** ${report.invariantsChecked}\n\n`;

  md += `## Evolution Timeline\n\n`;
  md += `- **Current Distance:** ${(report.evolution.distance * 100).toFixed(1)}%\n`;
  md += `- **Improvement (Last Epoch):** ${(report.evolution.improvement * 100).toFixed(2)}%\n`;
  md += `- **Epochs to Convergence (Est.):** ${Math.ceil(report.evolution.distance * 10)}\n\n`;

  md += `## Violations by Severity\n\n`;
  const bySeverity = {};
  for (const v of report.violations) {
    bySeverity[v.severity] = (bySeverity[v.severity] || 0) + 1;
  }
  for (const [severity, count] of Object.entries(bySeverity)) {
    md += `- **${severity}:** ${count} violations\n`;
  }
  md += '\n';

  md += `## Top Violations\n\n`;
  for (const violation of report.violations.slice(0, 10)) {
    md += `- **${violation.invariantDescription}** (${violation.shardLabel})\n`;
    if (violation.fixSuggestion) {
      md += `  - Fix: ${violation.fixSuggestion}\n`;
    }
  }

  return md;
}

function generateHTMLReport(report) {
  return `
    <div class="gamma-validation-report">
      <h2>Γ-Globalization Validation</h2>
      <div class="metrics">
        <p><strong>Status:</strong> ${report.isConverged ? '✅ Converged' : '⚠️ Not Converged'}</p>
        <p><strong>Drift:</strong> ${(report.drift * 100).toFixed(1)}%</p>
        <p><strong>Violations:</strong> ${report.violations}</p>
      </div>
    </div>
  `;
}

export default useGammaChecker;
