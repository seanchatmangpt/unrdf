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
 * Default operations loaded on initialization
 * @type {Array<{id: string, name: string, category: string, priority: string}>}
 */
const DEFAULT_OPERATIONS = [
  { id: 'test-op', name: 'Test Operation', category: 'testing', priority: 'low' },
  { id: 'backup-graphs', name: 'Backup RDF Graphs', category: 'maintenance', priority: 'high' },
  { id: 'cleanup-temp', name: 'Cleanup Temporary Files', category: 'maintenance', priority: 'medium' },
  { id: 'sync-federation', name: 'Synchronize Federation Nodes', category: 'distribution', priority: 'high' },
  { id: 'compact-storage', name: 'Compact Storage Engine', category: 'optimization', priority: 'low' },
  { id: 'validate-integrity', name: 'Validate Data Integrity', category: 'validation', priority: 'high' },
];

/**
 * Initialize the operation registry
 */
export function initializeRegistry() {
  if (operationRegistry.size === 0) {
    DEFAULT_OPERATIONS.forEach(op => {
      operationRegistry.set(op.id, {
        id: op.id,
        name: op.name,
        status: 'scheduled',
        createdAt: new Date(),
        metadata: { category: op.category, priority: op.priority },
      });
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
