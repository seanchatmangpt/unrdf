/**
 * @file Tutorial 02: MAPEK Integration
 * @module agents/tutorials/mapek-integration
 * 
 * @description
 * Machine-executable tutorial for integrating with MAPEK autonomic loop.
 * Demonstrates how agents can run MAPEK cycles, interpret results, and
 * create Knowledge Hooks from findings.
 * 
 * @targetAudience AutonomicAgent, HyperintelligentSystem
 * @format machine-executable
 */

import { runMapekIteration, createAutonomicHooks, reportMapekStatus } from 'unrdf/project-engine';
import { buildProjectModelFromFs, inferDomainModel } from 'unrdf/project-engine';
import { createDarkMatterCore, defineHook } from 'unrdf';
import { z } from 'zod';

/**
 * Step 1: Initialize project model from filesystem
 * 
 * @param {string} projectRoot - Root directory of project
 * @returns {Promise<Object>} Project and domain stores
 */
export async function initializeProjectModel(projectRoot) {
  const projectStore = await buildProjectModelFromFs(projectRoot);
  const domainStore = await inferDomainModel(projectStore);
  
  return {
    projectStore,
    domainStore,
    projectRoot
  };
}

/**
 * Step 2: Run single MAPEK iteration
 * 
 * @param {Object} state - Project state from Step 1
 * @param {Object} [options] - Additional options
 * @returns {Promise<Object>} MAPEK result
 */
export async function runMapekCycle(state, options = {}) {
  const result = await runMapekIteration({
    projectStore: state.projectStore,
    domainStore: state.domainStore,
    projectRoot: state.projectRoot,
    stackProfile: options.stackProfile,
    baselineSnapshot: options.baselineSnapshot,
    knowledge: options.knowledge
  });
  
  // Machine-readable format
  return {
    health: result.overallHealth,
    phase: result.phase,
    metrics: result.metrics,
    findings: {
      gaps: result.findings.gaps?.gaps?.length || 0,
      typeIssues: result.findings.typeIssues?.mismatches?.length || 0,
      hotspots: result.findings.hotspots?.hotspots?.length || 0,
      drift: result.findings.drift?.driftSeverity || 'none'
    },
    decisions: result.decisions.map(d => ({
      issue: d.issue,
      severity: d.severity,
      action: d.action,
      autoFixable: d.autoFixable,
      description: d.description
    })),
    actions: result.actions,
    learnings: result.learnings,
    shouldRepeat: result.shouldRepeat
  };
}

/**
 * Step 3: Create Knowledge Hooks from MAPEK findings
 * 
 * @param {Object} mapekResult - Result from Step 2
 * @param {Object} state - Project state
 * @returns {Promise<Array>} Array of hook definitions
 */
export async function createHooksFromMapek(mapekResult, state) {
  const hooks = createAutonomicHooks(mapekResult, state.projectStore);
  
  return hooks.map(hook => ({
    name: hook.meta.name,
    description: hook.meta.description,
    condition: hook.when.kind,
    source: hook.meta.source
  }));
}

/**
 * Step 4: Register hooks and execute
 * 
 * @param {Array} hooks - Hook definitions from Step 3
 * @returns {Promise<Object>} Registration results
 */
export async function registerAndExecuteHooks(hooks) {
  const system = await createDarkMatterCore();
  
  const results = [];
  for (const hookDef of hooks) {
    try {
      // Reconstruct hook from definition
      const hook = defineHook({
        meta: {
          name: hookDef.name,
          description: hookDef.description,
          source: hookDef.source || 'mapek:execute'
        },
        when: hookDef.when || {
          kind: 'sparql-ask',
          query: 'ASK { ?s ?p ?o }'
        },
        run: async (event) => {
          return {
            result: {
              action: 'autonomic-fix',
              hook: hookDef.name,
              triggered: true
            }
          };
        }
      });
      
      await system.registerHook(hook);
      results.push({
        hook: hookDef.name,
        status: 'registered',
        error: null
      });
    } catch (error) {
      results.push({
        hook: hookDef.name,
        status: 'failed',
        error: error.message
      });
    }
  }
  
  return {
    registered: results.filter(r => r.status === 'registered').length,
    failed: results.filter(r => r.status === 'failed').length,
    results
  };
}

/**
 * Complete tutorial execution
 * 
 * @param {string} projectRoot - Project root directory
 * @returns {Promise<Object>} Complete tutorial results
 */
export async function runTutorial(projectRoot = process.cwd()) {
  const results = {
    step1: null,
    step2: null,
    step3: null,
    step4: null
  };
  
  try {
    // Step 1: Initialize
    results.step1 = await initializeProjectModel(projectRoot);
    
    // Step 2: Run MAPEK
    results.step2 = await runMapekCycle(results.step1);
    
    // Step 3: Create hooks
    results.step3 = await createHooksFromMapek(results.step2, results.step1);
    
    // Step 4: Register hooks
    results.step4 = await registerAndExecuteHooks(results.step3);
    
  } catch (error) {
    results.error = {
      message: error.message,
      stack: error.stack
    };
  }
  
  return results;
}

// Export for machine execution
export default {
  initializeProjectModel,
  runMapekCycle,
  createHooksFromMapek,
  registerAndExecuteHooks,
  runTutorial
};

