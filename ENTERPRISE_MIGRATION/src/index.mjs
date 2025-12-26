/**
 * ENTERPRISE_MIGRATION - Main Entry Point
 *
 * Unified API for safe, verifiable enterprise migration from legacy systems
 * to UNRDF substrate platform.
 *
 * @module enterprise-migration
 * @version 1.0.0
 */

// Core orchestration
export {
  runMigration,
  verifyAll,
  getHealth,
  getState,
  getPhases,
  reset,
} from '../agent-1/control-plane.mjs';

// Impact analysis (Agent 6)
export {
  computeImpactSet,
  hasOverlap,
  mergeImpactSets,
  serializeImpactSet,
  deserializeImpactSet,
} from '../agent-6/impact-set.mjs';

// Routing modes (Agent 7)
export {
  ROUTING_MODES,
  setMode,
  getMode,
  listModes,
  resetModes,
  getModeStats,
  exportConfig as exportRoutingConfig,
  importConfig as importRoutingConfig,
} from '../agent-7/routing-modes.mjs';

/**
 * Package metadata
 * @type {Object}
 */
export const metadata = {
  name: 'enterprise-migration',
  version: '1.0.0',
  description: 'Enterprise migration system for UNRDF substrate platform',
  agents: {
    'agent-1': 'Control Plane - Orchestration',
    'agent-2': 'Discovery - Legacy system scanning',
    'agent-3': 'Schema Mapping - RDF ontology mapping',
    'agent-4': 'Validation - Data integrity validation',
    'agent-5': 'Migration - Data migration execution',
    'agent-6': 'Impact Analysis - Operation impact computation',
    'agent-7': 'Routing Modes - Shadow routing configuration',
    'agent-8': 'Shadow Testing - Dual system testing',
    'agent-9': 'Benchmarking - Performance comparison',
    'agent-10': 'Rollback - Rollback preparation and execution',
  },
  license: 'MIT',
  repository: 'https://github.com/unrdf/unrdf',
};

/**
 * Quick start - Run complete migration with defaults
 *
 * @param {Object} [options] - Migration options
 * @returns {Promise<Object>} Migration results
 *
 * @example
 * import { quickStart } from './src/index.mjs';
 * const results = await quickStart({ dryRun: true });
 * console.log(`Migration ${results.success ? 'succeeded' : 'failed'}`);
 */
export async function quickStart(options = {}) {
  const { dryRun = true } = options;

  console.log('ENTERPRISE_MIGRATION Quick Start');
  console.log('=================================\n');
  console.log(`Mode: ${dryRun ? 'DRY RUN' : 'PRODUCTION'}\n`);

  // Step 1: Verify all components
  console.log('Step 1: Verifying components...');
  const { verifyAll } = await import('../agent-1/control-plane.mjs');
  const verification = await verifyAll();

  if (!verification.allHealthy) {
    console.error('Component verification failed:');
    console.error(verification.failures);
    return {
      success: false,
      step: 'verification',
      error: 'Component verification failed',
      failures: verification.failures,
    };
  }
  console.log('✓ All components verified\n');

  // Step 2: Run migration
  console.log('Step 2: Running migration...');
  const { runMigration } = await import('../agent-1/control-plane.mjs');
  const results = await runMigration({ dryRun });

  if (!results.success) {
    console.error('Migration failed:');
    console.error(results.errors);
    return results;
  }
  console.log(`✓ Migration completed in ${results.duration}ms\n`);

  // Step 3: Health check
  console.log('Step 3: Final health check...');
  const { getHealth } = await import('../agent-1/control-plane.mjs');
  const health = await getHealth();

  if (!health.healthy) {
    console.error('Health check failed:');
    console.error(health);
    return {
      success: false,
      step: 'health-check',
      error: 'Health check failed',
      health,
    };
  }
  console.log('✓ System healthy\n');

  console.log('=================================');
  console.log('Migration complete!');
  console.log(`Phases completed: ${results.phasesCompleted}/10`);
  console.log(`Duration: ${results.duration}ms`);

  return {
    success: true,
    ...results,
    health,
  };
}

/**
 * Get package version
 * @returns {string} Version string
 */
export function getVersion() {
  return metadata.version;
}

/**
 * List all available agents
 * @returns {Object} Agent descriptions
 */
export function listAgents() {
  return metadata.agents;
}
