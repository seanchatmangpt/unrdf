/**
 * @file CLI confirmation prompts for destructive operations
 * @module cli/utils/confirmation
 *
 * Implements poka-yoke guards that prevent accidental data loss
 * by requiring user confirmation for delete/apply operations
 */

/**
 * Prompt user for confirmation on destructive operations
 *
 * @param {Object} options - Confirmation options
 * @param {string} options.action - Action type (delete, apply, etc)
 * @param {string} options.resource - Resource type (graph, hook, policy, etc)
 * @param {string} options.name - Resource name
 * @param {string} [options.summary] - Detailed summary of impact
 * @param {boolean} [options.requiresForce] - Require --force flag
 * @returns {Promise<boolean>} True if confirmed, false if cancelled
 *
 * @example
 * const confirmed = await promptConfirmation({
 *   action: 'delete',
 *   resource: 'graph',
 *   name: 'production-data',
 *   summary: 'This will permanently delete 1.2M triples',
 *   requiresForce: true
 * });
 */
export async function promptConfirmation(options) {
  const {
    action = 'delete',
    resource = 'resource',
    name,
    summary = '',
    requiresForce = true
  } = options;

  // Build confirmation message
  const messages = [];
  messages.push('');
  messages.push(`⚠️  WARNING: ${action.toUpperCase()} OPERATION`);
  messages.push('');
  messages.push(`Action: ${action} ${resource}`);
  messages.push(`Resource: ${name}`);

  if (summary) {
    messages.push(`Impact: ${summary}`);
  }

  messages.push('');
  messages.push(`This action CANNOT be undone.`);
  messages.push('');

  if (requiresForce) {
    messages.push(`To ${action} without confirmation, use: --force`);
    messages.push('');
  }

  const message = messages.join('\n');
  console.log(message);

  // For non-interactive environments, deny confirmation
  if (!process.stdin.isTTY) {
    console.error(`❌ Cannot prompt for confirmation in non-interactive mode`);
    console.error(`Use --force to skip confirmation (not recommended for destructive operations)`);
    return false;
  }

  // Prompt for confirmation
  const rl = await createReadlineInterface();

  return new Promise((resolve) => {
    rl.question(
      'Type "yes" to confirm, or press Enter to cancel: ',
      (answer) => {
        rl.close();

        if (answer.trim().toLowerCase() === 'yes') {
          console.log(`✅ ${action.charAt(0).toUpperCase() + action.slice(1)} confirmed`);
          resolve(true);
        } else {
          console.log(`⛔ ${action.charAt(0).toUpperCase() + action.slice(1)} cancelled`);
          resolve(false);
        }
      }
    );

    // Handle Ctrl+C during prompt
    rl.on('SIGINT', () => {
      console.log('\n⛔ Operation cancelled');
      resolve(false);
    });
  });
}

/**
 * Create readline interface for prompting
 */
async function createReadlineInterface() {
  const readline = await import('node:readline');
  return readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
}

/**
 * Format a summary of operation impact
 */
export function formatImpactSummary(stats) {
  if (!stats) return '';

  const parts = [];

  if (stats.triples) {
    parts.push(`${stats.triples.toLocaleString()} triples`);
  }

  if (stats.graphs) {
    parts.push(`${stats.graphs} graph(s)`);
  }

  if (stats.hooks) {
    parts.push(`${stats.hooks} hook(s)`);
  }

  if (stats.policies) {
    parts.push(`${stats.policies} policy(ies)`);
  }

  if (stats.dependencies) {
    parts.push(`${stats.dependencies} dependent resource(s)`);
  }

  return parts.length > 0 ? parts.join(', ') : '';
}

/**
 * Check if force flag is set
 */
export function shouldSkipConfirmation(ctx) {
  return ctx?.args?.force || ctx?.flags?.force || process.env.UNRDF_FORCE === 'true';
}

/**
 * Execute destructive operation with confirmation
 *
 * @param {Object} ctx - CLI context
 * @param {Object} options - Confirmation options
 * @param {Function} executeFunc - Function to execute if confirmed
 * @returns {Promise<any>} Result of executeFunc or null if cancelled
 *
 * @example
 * await executeWithConfirmation(ctx, {
 *   action: 'delete',
 *   resource: 'graph',
 *   name: 'production-graph',
 *   summary: '1.2M triples will be deleted'
 * }, async () => {
 *   return await deleteGraph('production-graph');
 * });
 */
export async function executeWithConfirmation(ctx, options, executeFunc) {
  // Skip confirmation if force flag set
  if (shouldSkipConfirmation(ctx)) {
    console.log(`🔓 Skipping confirmation (--force flag set)`);
    return executeFunc();
  }

  // Prompt for confirmation
  const confirmed = await promptConfirmation(options);

  if (!confirmed) {
    return null;
  }

  // Execute operation
  return executeFunc();
}

export default {
  promptConfirmation,
  formatImpactSummary,
  shouldSkipConfirmation,
  executeWithConfirmation
};
