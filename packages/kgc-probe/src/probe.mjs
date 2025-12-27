/**
 * @fileoverview KGC Probe - Convenience Function
 *
 * High-level runProbe() for executing a complete scan with one call.
 *
 * @module @unrdf/kgc-probe/probe
 */

import { createProbeOrchestrator } from './orchestrator.mjs';
import { createMemoryStorage } from './storage/index.mjs';

/**
 * Run full probe scan with sensible defaults
 *
 * Convenience function that:
 * 1. Creates orchestrator with default storage
 * 2. Runs all agents and guards
 * 3. Returns artifact directly
 *
 * @param {Object} config - Probe configuration
 * @param {string} config.universe_id - Universe to scan
 * @param {string} [config.snapshot_id] - Optional snapshot
 * @param {Object} [config.storage] - Custom storage backend
 * @returns {Promise<Artifact>} Generated artifact
 *
 * @example
 * const artifact = await runProbe({
 *   universe_id: 'my-universe',
 *   snapshot_id: 'snap_123'
 * });
 *
 * console.log(artifact.summary);
 */
export async function runProbe(config) {
  if (!config || !config.universe_id) {
    throw new Error('runProbe requires universe_id in config');
  }

  // Create orchestrator with provided or default storage
  const storage = config.storage || createMemoryStorage();
  const orchestrator = createProbeOrchestrator({ storage });

  // Run scan
  const result = await orchestrator.scan({
    universe_id: config.universe_id,
    snapshot_id: config.snapshot_id,
    agents: config.agents,
    guards: config.guards,
    distributed: config.distributed || false,
    persist: config.persist !== false // Default true
  });

  if (result.status === 'failed') {
    throw new Error(`Probe scan failed: ${result.errors.map(e => e.error).join(', ')}`);
  }

  return result.artifact;
}
