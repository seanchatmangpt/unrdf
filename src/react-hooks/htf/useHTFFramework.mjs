/**
 * @fileoverview useHTFFramework - Master hook combining all HTF operations
 * Provides complete thesis management with Λ-scheduling, Π-merging, Γ-validation
 */

import { useState, useCallback, useMemo, useEffect } from 'react';
import { useLambdaScheduling } from './useLambdaScheduling.mjs';
import { usePiProfile } from './usePiProfile.mjs';
import { useGammaChecker } from './useGammaChecker.mjs';

/**
 * useHTFFramework - Complete thesis framework
 *
 * @param {Object} config - Framework configuration
 * @param {DeltaShard[]} config.shards - Initial shards
 * @param {Date} config.deadline - Thesis deadline
 * @param {number} config.weeksAvailable - Weeks until deadline
 * @param {Object} config.callbacks - Callbacks
 * @param {Function} config.callbacks.onScheduleChange - When schedule changes
 * @param {Function} config.callbacks.onMergeChange - When merge changes
 * @param {Function} config.callbacks.onViolation - When violations found
 * @param {Function} config.callbacks.onConvergence - When thesis converges
 *
 * @returns {Object} Unified framework interface
 */
export function useHTFFramework(config = {}) {
  const {
    shards: initialShards = [],
    deadline = new Date(Date.now() + 26 * 7 * 24 * 60 * 60 * 1000),
    weeksAvailable = null,
    callbacks = {}
  } = config;

  const {
    onScheduleChange = () => {},
    onMergeChange = () => {},
    onViolation = () => {},
    onConvergence = () => {}
  } = callbacks;

  // Master state
  const [shards, setShards] = useState(initialShards);
  const [mode, setMode] = useState('dashboard'); // 'dashboard', 'schedule', 'merge', 'validate'

  // Λ-Scheduling subsystem
  const scheduling = useLambdaScheduling(shards, {
    deadline,
    weeksAvailable,
    onScheduleChange
  });

  // Π-Merge subsystem
  const profile = usePiProfile(shards, {
    onMergeChange,
    showMergePoints: true
  });

  // Γ-Validation subsystem
  const validation = useGammaChecker(shards, [], {
    autoCheck: true,
    onViolation,
    onConvergence
  });

  // ====== MASTER SHARD OPERATIONS ======

  /**
   * Add shard and auto-sync across subsystems
   */
  const addShard = useCallback((shard) => {
    setShards(prev => [...prev, shard]);
    scheduling.addShard(shard);
  }, [scheduling]);

  /**
   * Update shard and auto-sync
   */
  const updateShard = useCallback((shardId, updates) => {
    setShards(prev => prev.map(s =>
      s.id === shardId ? { ...s, ...updates } : s
    ));
    scheduling.updateShard(shardId, updates);
  }, [scheduling]);

  /**
   * Remove shard and auto-sync
   */
  const removeShard = useCallback((shardId) => {
    setShards(prev => prev.filter(s => s.id !== shardId));
    scheduling.removeShard(shardId);
  }, [scheduling]);

  // ====== UNIFIED OPERATIONS ======

  /**
   * Run complete thesis validation and optimization
   */
  const optimizeThesis = useCallback(async () => {
    // 1. Fix violations
    if (validation.state.violations.length > 0) {
      validation.fixViolations();
    }

    // 2. Reweight for coherence
    const suggestions = profile.suggestReweighting();
    Object.entries(suggestions).forEach(([shardId, weight]) => {
      profile.setShardWeight(shardId, weight);
    });

    // 3. Auto-schedule
    scheduling.adjustTimings(0.1);

    // 4. Re-validate
    validation.validateShards();

    return {
      success: validation.state.isConverged,
      drift: validation.state.drift,
      coherence: profile.analysis.coherence.overall,
      violations: validation.state.violations.length
    };
  }, [validation, profile, scheduling]);

  /**
   * Recommend next steps to convergence
   */
  const recommendNextSteps = useCallback(() => {
    const steps = [];

    // Step 1: Fix violations
    if (validation.state.violations.length > 0) {
      steps.push({
        priority: 1,
        action: 'fix-violations',
        label: `Fix ${validation.state.violations.length} validation violations`,
        detail: `${validation.state.violations.length} shards violate invariants`,
        auto: true
      });
    }

    // Step 2: Improve coherence
    if (profile.analysis.coherence.overall < 0.8) {
      steps.push({
        priority: 2,
        action: 'improve-coherence',
        label: 'Rebalance family weights',
        detail: `Coherence is ${(profile.analysis.coherence.overall * 100).toFixed(1)}%, target 80%+`,
        auto: true
      });
    }

    // Step 3: Add missing shards
    const recommendations = profile.recommendMissingShards();
    if (recommendations.length > 0) {
      steps.push({
        priority: 3,
        action: 'add-missing-shards',
        label: `Add ${recommendations.length} missing shard families`,
        detail: recommendations.map(r => r.family).join(', '),
        auto: false
      });
    }

    // Step 4: Optimize schedule
    const slack = scheduling.scheduling.slack;
    if (slack < 2) {
      steps.push({
        priority: 4,
        action: 'optimize-schedule',
        label: 'Compress schedule (low slack time)',
        detail: `Only ${Math.ceil(slack)} weeks slack before deadline`,
        auto: false
      });
    }

    return steps.sort((a, b) => a.priority - b.priority);
  }, [validation, profile, scheduling]);

  /**
   * Generate complete thesis report
   */
  const generateThesisReport = useCallback((format = 'markdown') => {
    const sections = [];

    // Header
    sections.push('# Complete Thesis Report\n');
    sections.push(`**Generated:** ${new Date().toISOString()}\n`);
    sections.push(`**Status:** ${validation.state.isConverged ? '✅ CONVERGED' : '⚠️ IN PROGRESS'}\n\n`);

    // Summary
    sections.push('## Executive Summary\n\n');
    sections.push(`- **Total Shards:** ${shards.length}\n`);
    sections.push(`- **Families Represented:** ${Object.keys(profile.analysis.profileByFamily).filter(f => profile.analysis.profileByFamily[f].count > 0).length}/7\n`);
    sections.push(`- **Overall Coherence:** ${(profile.analysis.coherence.overall * 100).toFixed(1)}%\n`);
    sections.push(`- **Validation Violations:** ${validation.state.violations.length}\n`);
    sections.push(`- **Convergence Drift:** ${(validation.state.drift * 100).toFixed(1)}%\n`);
    sections.push(`- **Time to Deadline:** ${scheduling.scheduling.slack.toFixed(1)} weeks\n\n`);

    // Schedule section
    sections.push('## Schedule Summary\n\n');
    sections.push(`- **Critical Path:** ${scheduling.scheduling.criticalPath.path.length} shards\n`);
    sections.push(`- **Duration:** ${scheduling.scheduling.criticalPath.duration} weeks\n`);
    sections.push(`- **Available Slack:** ${scheduling.scheduling.slack.toFixed(1)} weeks\n\n`);

    // Merge/Profile section
    sections.push('## Coherence Analysis\n\n');
    sections.push(`- **Family Balance:** ${(profile.analysis.coherence.familyBalance * 100).toFixed(1)}%\n`);
    sections.push(`- **Family Coverage:** ${(profile.analysis.coherence.familyCoverage * 100).toFixed(1)}%\n`);
    sections.push(`- **Integration Strength:** ${(profile.analysis.coherence.integrationStrength * 100).toFixed(1)}%\n`);
    sections.push(`- **Completeness:** ${(profile.analysis.coherence.completeness * 100).toFixed(1)}%\n\n`);

    // Violations section
    if (validation.state.violations.length > 0) {
      sections.push('## Violations\n\n');
      const analysis = validation.getViolationAnalysis();
      for (const [severity, violations] of Object.entries(analysis.bySeverity)) {
        sections.push(`### ${severity.toUpperCase()} (${violations.length})\n\n`);
        violations.slice(0, 5).forEach(v => {
          sections.push(`- **${v.shardLabel}:** ${v.invariantDescription}\n`);
        });
        if (violations.length > 5) {
          sections.push(`- ... and ${violations.length - 5} more\n`);
        }
        sections.push('\n');
      }
    }

    // Recommendations section
    const nextSteps = recommendNextSteps();
    if (nextSteps.length > 0) {
      sections.push('## Next Steps\n\n');
      nextSteps.forEach(step => {
        const autoLabel = step.auto ? '[AUTO]' : '[MANUAL]';
        sections.push(`${step.priority}. ${autoLabel} **${step.label}**\n`);
        sections.push(`   ${step.detail}\n\n`);
      });
    }

    return sections.join('');
  }, [shards, validation, profile, scheduling, recommendNextSteps]);

  /**
   * Get comprehensive thesis statistics
   */
  const getThesisStats = useCallback(() => {
    return {
      shards: {
        total: shards.length,
        byFamily: Object.fromEntries(
          Object.entries(profile.analysis.profileByFamily).map(([family, prof]) => [family, prof.count])
        )
      },
      schedule: {
        totalWeeks: scheduling.scheduling.startDate && scheduling.scheduling.deadline
          ? Math.ceil((scheduling.scheduling.deadline - scheduling.scheduling.startDate) / (7 * 24 * 60 * 60 * 1000))
          : 0,
        slack: scheduling.scheduling.slack,
        criticalPathLength: scheduling.scheduling.criticalPath.path.length
      },
      quality: {
        coherence: profile.analysis.coherence.overall,
        violations: validation.state.violations.length,
        drift: validation.state.drift,
        isConverged: validation.state.isConverged
      },
      progress: {
        completed: shards.filter(s => scheduling.progress[s.id] === 'completed').length,
        inProgress: shards.filter(s => scheduling.progress[s.id] === 'in_progress').length,
        pending: shards.filter(s => !scheduling.progress[s.id] || scheduling.progress[s.id] === 'scheduled').length
      }
    };
  }, [shards, validation, profile, scheduling]);

  // Sync shards across all subsystems
  useEffect(() => {
    // All subsystems use the same shards from parent useState
    // No additional sync needed
  }, [shards]);

  return {
    // ===== STATE =====
    state: {
      shards,
      mode,
      setMode
    },

    // ===== SUBSYSTEMS =====
    scheduling,
    profile,
    validation,

    // ===== MASTER OPERATIONS =====
    addShard,
    updateShard,
    removeShard,

    // ===== UNIFIED OPERATIONS =====
    optimizeThesis,
    recommendNextSteps,
    generateThesisReport,
    getThesisStats,

    // ===== CONVENIENCE HELPERS =====
    isConverged: () => validation.state.isConverged,
    getDrift: () => validation.state.drift,
    getCoherence: () => profile.analysis.coherence.overall,
    getProgress: () => {
      const completed = shards.filter(s => scheduling.progress[s.id] === 'completed').length;
      return completed / Math.max(shards.length, 1);
    }
  };
}

export default useHTFFramework;
