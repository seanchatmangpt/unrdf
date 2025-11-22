/**
 * @file How-To 02: Execute MAPEK Cycle
 * @module agents/how-to/execute-mapek-cycle
 * 
 * @description
 * Task-focused guide for executing MAPEK cycles and interpreting results.
 * This guide solves the problem: "How do I run MAPEK and handle the results?"
 * 
 * @targetAudience AutonomicAgent, HyperintelligentSystem
 * @format machine-executable
 */

import { runMapekIteration, runContinuousMapekLoop } from 'unrdf/project-engine';
import { buildProjectModelFromFs, inferDomainModel } from 'unrdf/project-engine';

/**
 * Problem: Need to run a single MAPEK cycle and get machine-readable results
 * 
 * @param {string} projectRoot - Project root directory
 * @param {Object} [options] - Additional options
 * @returns {Promise<Object>} MAPEK result in machine-readable format
 */
export async function executeSingleMapekCycle(projectRoot, options = {}) {
  // Build project model
  const projectStore = await buildProjectModelFromFs(projectRoot);
  const domainStore = await inferDomainModel(projectStore);
  
  // Run MAPEK iteration
  const result = await runMapekIteration({
    projectStore,
    domainStore,
    projectRoot,
    stackProfile: options.stackProfile,
    baselineSnapshot: options.baselineSnapshot,
    knowledge: options.knowledge
  });
  
  // Return machine-readable format
  return {
    health: {
      overall: result.overallHealth,
      gapScore: result.metrics.gapScore,
      typeScore: result.metrics.typeScore,
      hotspotScore: result.metrics.hotspotScore,
      driftSeverity: result.metrics.driftSeverity
    },
    findings: {
      gaps: {
        count: result.findings.gaps?.gaps?.length || 0,
        critical: result.findings.gaps?.gaps?.filter(g => g.score > 80).length || 0,
        items: result.findings.gaps?.gaps?.map(g => ({
          entity: g.entity,
          missingRoles: g.missingRoles,
          score: g.score
        })) || []
      },
      typeIssues: {
        count: result.findings.typeIssues?.mismatches?.length || 0,
        highSeverity: result.findings.typeIssues?.mismatches?.filter(m => m.severity === 'high').length || 0,
        items: result.findings.typeIssues?.mismatches?.map(m => ({
          entity: m.entity,
          severity: m.severity,
          issues: m.issues
        })) || []
      },
      hotspots: {
        count: result.findings.hotspots?.hotspots?.length || 0,
        highRisk: result.findings.hotspots?.hotspots?.filter(h => h.risk === 'HIGH').length || 0,
        items: result.findings.hotspots?.hotspots?.map(h => ({
          feature: h.feature,
          risk: h.risk,
          reason: h.reason
        })) || []
      },
      drift: {
        severity: result.findings.drift?.driftSeverity || 'none',
        details: result.findings.drift || null
      }
    },
    decisions: result.decisions.map(d => ({
      issue: d.issue,
      severity: d.severity,
      action: d.action,
      autoFixable: d.autoFixable,
      description: d.description,
      targets: d.targets || []
    })),
    actions: result.actions.map(a => ({
      type: a.type,
      status: a.status,
      entity: a.entity,
      roles: a.roles,
      timestamp: a.timestamp
    })),
    learnings: result.learnings,
    shouldRepeat: result.shouldRepeat,
    phase: result.phase
  };
}

/**
 * Problem: Need to run continuous MAPEK loop until convergence
 * 
 * @param {string} projectRoot - Project root directory
 * @param {Object} [options] - Loop options
 * @returns {Promise<Object>} Convergence result
 */
export async function executeContinuousMapekLoop(projectRoot, options = {}) {
  const {
    intervalMs = 5000,
    maxIterations = 10,
    healthThreshold = 80,
    applyActions = async () => {}
  } = options;
  
  const result = await runContinuousMapekLoop({
    getState: async () => {
      const projectStore = await buildProjectModelFromFs(projectRoot);
      const domainStore = await inferDomainModel(projectStore);
      return { projectStore, domainStore, projectRoot };
    },
    applyActions: async (actions) => {
      await applyActions(actions);
    },
    intervalMs,
    maxIterations
  });
  
  return {
    converged: result.converged,
    iterations: result.iterations,
    finalHealth: result.finalHealth,
    finalState: result.finalState ? {
      health: result.finalState.overallHealth,
      decisions: result.finalState.decisions?.length || 0,
      actions: result.finalState.actions?.length || 0
    } : null
  };
}

/**
 * Problem: Need to interpret MAPEK results and decide on actions
 * 
 * @param {Object} mapekResult - Result from executeSingleMapekCycle
 * @returns {Object} Action recommendations
 */
export function interpretMapekResults(mapekResult) {
  const recommendations = {
    immediate: [],
    scheduled: [],
    manual: []
  };
  
  // Immediate actions: Critical issues that are auto-fixable
  const criticalAutoFixable = mapekResult.decisions.filter(
    d => d.severity === 'critical' && d.autoFixable
  );
  recommendations.immediate = criticalAutoFixable.map(d => ({
    type: 'auto-fix',
    issue: d.issue,
    action: d.action,
    description: d.description
  }));
  
  // Scheduled actions: High/medium severity auto-fixable
  const scheduledAutoFixable = mapekResult.decisions.filter(
    d => (d.severity === 'high' || d.severity === 'medium') && d.autoFixable
  );
  recommendations.scheduled = scheduledAutoFixable.map(d => ({
    type: 'auto-fix',
    issue: d.issue,
    action: d.action,
    description: d.description
  }));
  
  // Manual actions: Non-auto-fixable or low severity
  const manualActions = mapekResult.decisions.filter(
    d => !d.autoFixable || d.severity === 'low'
  );
  recommendations.manual = manualActions.map(d => ({
    type: 'manual-review',
    issue: d.issue,
    severity: d.severity,
    description: d.description,
    recommendation: d.recommendation
  }));
  
  return {
    recommendations,
    summary: {
      totalIssues: mapekResult.decisions.length,
      autoFixable: mapekResult.decisions.filter(d => d.autoFixable).length,
      manualReview: mapekResult.decisions.filter(d => !d.autoFixable).length,
      healthStatus: mapekResult.health.overall >= 80 ? 'healthy' : 
                   mapekResult.health.overall >= 60 ? 'degraded' : 'unhealthy'
    }
  };
}

// Export for machine execution
export default {
  executeSingleMapekCycle,
  executeContinuousMapekLoop,
  interpretMapekResults
};

