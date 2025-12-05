/**
 * @file Dependency analysis for deletion operations
 * @module cli/utils/dependency-analyzer
 *
 * Analyzes cascade impact before deletion
 * FM-CLI-013: Show dependent resources before delete
 */

/**
 * Analyze dependencies for a resource
 *
 * @param {string} resourceType - Type of resource (hook, policy, context, graph)
 * @param {string} resourceName - Name of the resource
 * @returns {Promise<Object>} Dependency analysis results
 *
 * @example
 * const deps = await analyzeDependencies('hook', 'my-hook');
 * console.log(`This hook is referenced by ${deps.dependentCount} policies`);
 */
export async function analyzeDependencies(resourceType, resourceName) {
  const dependents = {
    policies: [],
    hooks: [],
    contexts: [],
    graphs: []
  };

  // TODO: Implement actual dependency querying
  // For now, return structure for testing

  return {
    resourceType,
    resourceName,
    dependents,
    dependentCount: Object.values(dependents).flat().length,
    cascadeRisk: calculateCascadeRisk(dependents)
  };
}

/**
 * Calculate cascade risk level
 */
function calculateCascadeRisk(dependents) {
  const total = Object.values(dependents).flat().length;

  if (total === 0) return 'LOW';
  if (total <= 3) return 'MEDIUM';
  if (total <= 10) return 'HIGH';
  return 'CRITICAL';
}

/**
 * Format dependency analysis for user display
 */
export function formatDependencyAnalysis(analysis) {
  const lines = [];

  lines.push('');
  lines.push('📊 Dependency Analysis:');
  lines.push(`   Resource: ${analysis.resourceType} "${analysis.resourceName}"`);
  lines.push(`   Dependent resources: ${analysis.dependentCount}`);
  lines.push(`   Cascade risk: ${analysis.cascadeRisk}`);

  if (analysis.dependentCount > 0) {
    lines.push('');
    lines.push('   Dependent resources:');

    if (analysis.dependents.policies.length > 0) {
      lines.push(`   • Policies: ${analysis.dependents.policies.join(', ')}`);
    }
    if (analysis.dependents.hooks.length > 0) {
      lines.push(`   • Hooks: ${analysis.dependents.hooks.join(', ')}`);
    }
    if (analysis.dependents.contexts.length > 0) {
      lines.push(`   • Contexts: ${analysis.dependents.contexts.join(', ')}`);
    }
    if (analysis.dependents.graphs.length > 0) {
      lines.push(`   • Graphs: ${analysis.dependents.graphs.join(', ')}`);
    }

    if (analysis.cascadeRisk === 'CRITICAL') {
      lines.push('');
      lines.push('   ⚠️  WARNING: Deleting this resource will break dependent resources');
      lines.push('   Consider updating dependent resources first');
    }
  }

  lines.push('');

  return lines.join('\n');
}

export default {
  analyzeDependencies,
  formatDependencyAnalysis
};
