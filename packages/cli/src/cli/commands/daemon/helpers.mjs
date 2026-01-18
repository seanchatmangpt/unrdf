/**
 * @file Daemon Command Helpers
 * @module cli/commands/daemon/helpers
 * @description Shared helpers and state for daemon commands
 */

/**
 * In-memory operation registry and event log for CLI
 */
export const operationRegistry = new Map();
export const eventLog = [];

/**
 * Initialize the operation registry
 */
export function initializeRegistry() {
  if (operationRegistry.size === 0) {
    operationRegistry.set('backup-graphs', {
      id: 'backup-graphs',
      name: 'Backup RDF Graphs',
      status: 'scheduled',
      createdAt: new Date(),
      metadata: { category: 'maintenance', priority: 'high' },
    });

    operationRegistry.set('cleanup-temp', {
      id: 'cleanup-temp',
      name: 'Cleanup Temporary Files',
      status: 'scheduled',
      createdAt: new Date(),
      metadata: { category: 'maintenance', priority: 'medium' },
    });

    operationRegistry.set('sync-federation', {
      id: 'sync-federation',
      name: 'Synchronize Federation Nodes',
      status: 'scheduled',
      createdAt: new Date(),
      metadata: { category: 'distribution', priority: 'high' },
    });

    operationRegistry.set('compact-storage', {
      id: 'compact-storage',
      name: 'Compact Storage Engine',
      status: 'scheduled',
      createdAt: new Date(),
      metadata: { category: 'optimization', priority: 'low' },
    });

    operationRegistry.set('validate-integrity', {
      id: 'validate-integrity',
      name: 'Validate Data Integrity',
      status: 'scheduled',
      createdAt: new Date(),
      metadata: { category: 'validation', priority: 'high' },
    });
  }
}

/**
 * Format operation object for display
 */
export function formatOperation(op) {
  return {
    id: op.id,
    name: op.name || op.id,
    status: op.status,
    createdAt: op.createdAt.toISOString(),
    metadata: op.metadata || {},
  };
}

/**
 * Format milliseconds to human-readable duration
 */
export function formatDuration(ms) {
  if (ms < 1000) return `${ms}ms`;
  if (ms < 60000) return `${(ms / 1000).toFixed(2)}s`;
  return `${(ms / 60000).toFixed(2)}m`;
}
