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
    graphs: [],
    warnings: []
  };

  // POKA-YOKE: Warn user about beta status
  dependents.warnings.push('Dependency analysis is in beta - some dependencies may not be detected');

  try {
    // Implement actual dependency checking based on resource type
    switch (resourceType) {
      case 'hook':
        await analyzeHookDependencies(resourceName, dependents);
        break;
      case 'graph':
        await analyzeGraphDependencies(resourceName, dependents);
        break;
      case 'policy':
        await analyzePolicyDependencies(resourceName, dependents);
        break;
      case 'context':
        await analyzeContextDependencies(resourceName, dependents);
        break;
      default:
        dependents.warnings.push(`Unknown resource type: ${resourceType}`);
    }
  } catch (error) {
    // POKA-YOKE: Don't fail deletion if dependency analysis fails
    dependents.warnings.push(`Dependency analysis failed: ${error.message}`);
    dependents.warnings.push('Please verify manually that no resources depend on this');
  }

  return {
    resourceType,
    resourceName,
    dependents,
    dependentCount: Object.values(dependents)
      .filter(v => Array.isArray(v))
      .flat().length,
    cascadeRisk: calculateCascadeRisk(dependents),
    warnings: dependents.warnings
  };
}

/**
 * Analyze dependencies for a hook
 */
async function analyzeHookDependencies(hookName, dependents) {
  const { readdir, readFile } = await import('node:fs/promises');
  const { join } = await import('node:path');
  const { homedir } = await import('node:os');

  // Check policy files for references to this hook
  const policyDir = join(homedir(), '.unrdf', 'policies');

  try {
    const files = await readdir(policyDir);
    const policyFiles = files.filter(f => f.endsWith('.json'));

    for (const file of policyFiles) {
      try {
        const content = await readFile(join(policyDir, file), 'utf-8');
        const policy = JSON.parse(content);

        // Check if policy references this hook
        if (policy.hooks && Array.isArray(policy.hooks)) {
          if (policy.hooks.includes(hookName)) {
            dependents.policies.push(policy.meta?.name || file);
          }
        }

        // Check in hook definitions within policy
        if (policy.when?.hooks) {
          const hookRefs = JSON.stringify(policy.when.hooks);
          if (hookRefs.includes(hookName)) {
            dependents.policies.push(policy.meta?.name || file);
          }
        }
      } catch (error) {
        // Skip malformed policy files
        continue;
      }
    }
  } catch (error) {
    if (error.code !== 'ENOENT') {
      throw error;
    }
    // Policy directory doesn't exist - no dependencies
  }
}

/**
 * Analyze dependencies for a graph
 */
async function analyzeGraphDependencies(graphName, dependents) {
  const { readdir, readFile } = await import('node:fs/promises');
  const { join } = await import('node:path');
  const { homedir } = await import('node:os');

  // Check hooks for SPARQL queries referencing this graph
  const hooksDir = join(homedir(), '.unrdf', 'hooks');

  try {
    const files = await readdir(hooksDir);
    const hookFiles = files.filter(f => f.endsWith('.json'));

    for (const file of hookFiles) {
      try {
        const content = await readFile(join(hooksDir, file), 'utf-8');
        const hook = JSON.parse(content);

        // Check if hook SPARQL queries reference this graph
        if (hook.when?.ref?.content) {
          if (hook.when.ref.content.includes(`GRAPH <${graphName}>`)) {
            dependents.hooks.push(hook.meta?.name || file);
          }
        }

        // Check default graph references
        if (hook.when?.defaultGraph === graphName) {
          dependents.hooks.push(hook.meta?.name || file);
        }
      } catch (error) {
        // Skip malformed hook files
        continue;
      }
    }
  } catch (error) {
    if (error.code !== 'ENOENT') {
      throw error;
    }
  }
}

/**
 * Analyze dependencies for a policy
 */
async function analyzePolicyDependencies(policyName, dependents) {
  // Policies are currently top-level, don't have dependents
  // Future: Check if other policies inherit from this one
  dependents.warnings.push('Policy dependency analysis not yet implemented');
}

/**
 * Analyze dependencies for a context
 */
async function analyzeContextDependencies(contextName, dependents) {
  // Contexts don't currently have dependencies
  // Future: Check if any scripts/configs reference this context
  dependents.warnings.push('Context dependency analysis not yet implemented');
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

  // Show warnings if any
  if (analysis.warnings && analysis.warnings.length > 0) {
    lines.push('');
    analysis.warnings.forEach(warning => {
      lines.push(`   ℹ️  ${warning}`);
    });
  }

  lines.push('');

  return lines.join('\n');
}

export default {
  analyzeDependencies,
  formatDependencyAnalysis
};
