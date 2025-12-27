/**
 * @fileoverview KGC Probe - Orchestrator
 *
 * ProbeOrchestrator coordinates agent execution, guard validation,
 * shard merging, and artifact generation with deterministic hashing.
 *
 * Algorithm: OrchestrateScan (5-phase execution)
 *
 * @module @unrdf/kgc-probe/orchestrator
 */

import { randomUUID } from 'crypto';
import { ProbeConfigSchema, validateProbeConfig } from './types.mjs';
import { createGuardRegistry } from './guards.mjs';
import { createAgentRegistry } from './agents/index.mjs';
import { hashObservations, computeArtifactSummary } from './artifact.mjs';

/**
 * ProbeOrchestrator - Main orchestration engine
 *
 * Coordinates:
 * 1. Parallel agent execution
 * 2. Guard validation
 * 3. Shard merging
 * 4. Artifact generation
 *
 * @class ProbeOrchestrator
 */
export class ProbeOrchestrator {
  /**
   * Create orchestrator with storage backend
   * @param {Object} options - Configuration
   * @param {Storage} options.storage - Storage backend (memory/file/db)
   * @param {GuardRegistry} [options.guards] - Custom guard registry
   * @param {AgentRegistry} [options.agents] - Custom agent registry
   */
  constructor(options = {}) {
    if (!options.storage) {
      throw new Error('ProbeOrchestrator requires storage backend');
    }

    this.storage = options.storage;
    this.guards = options.guards || createGuardRegistry();
    this.agents = options.agents || createAgentRegistry();

    /** @type {Set<string>} - Event listeners */
    this.listeners = new Map();
  }

  /**
   * Register event listener
   * @param {string} event - Event name ('agent_complete', 'guard_violation', etc.)
   * @param {Function} callback - Callback function
   */
  on(event, callback) {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, []);
    }
    this.listeners.get(event).push(callback);
  }

  /**
   * Emit event to registered listeners
   * @param {string} event - Event name
   * @param {unknown} data - Event data
   * @private
   */
  emit(event, data) {
    const callbacks = this.listeners.get(event) || [];
    for (const cb of callbacks) {
      try {
        cb(data);
      } catch (err) {
        console.error(`[ProbeOrchestrator] Listener error for ${event}:`, err);
      }
    }
  }

  /**
   * Execute full integrity scan
   *
   * Phase 1: Initialization
   * Phase 2: Parallel agent execution
   * Phase 3: Guard validation
   * Phase 4: Shard merging
   * Phase 5: Artifact generation and persistence
   *
   * @param {Object} scanConfig - Scan configuration
   * @param {string} scanConfig.universe_id - Universe to scan
   * @param {string} [scanConfig.snapshot_id] - Optional snapshot reference
   * @param {string[]} [scanConfig.agents] - Agent IDs (all if omitted)
   * @param {boolean} [scanConfig.distributed] - Enable shard merging
   * @param {boolean} [scanConfig.persist] - Save to storage
   * @returns {Promise<ScanResult>} Scan result with artifact
   */
  async scan(scanConfig) {
    // Validate configuration
    let config;
    try {
      config = validateProbeConfig(scanConfig);
    } catch (err) {
      throw new Error(`Invalid probe config: ${err.message}`);
    }

    const startTime = Date.now();
    const runId = randomUUID();
    const observations = [];
    const errors = [];

    try {
      // Phase 1: Initialization
      this.emit('scan_start', { runId, config });

      // Phase 2: Parallel Agent Execution
      const agentIds = config.agents || this.agents.list();

      const agentPromises = agentIds.map(agentId =>
        this.executeAgent(agentId, config, observations, errors)
      );

      await Promise.allSettled(agentPromises);

      this.emit('agents_complete', {
        runId,
        agentCount: agentIds.length,
        observationCount: observations.length
      });

      // Phase 3: Guard Validation
      const guardIds = config.guards || this.guards.list();
      for (const guardId of guardIds) {
        try {
          const violations = this.guards.validate(guardId, observations);
          for (const violation of violations) {
            observations.push({
              id: randomUUID(),
              agent: `guard:${guardId}`,
              timestamp: new Date().toISOString(),
              kind: 'guard_violation',
              severity: violation.severity,
              subject: 'artifact:self',
              evidence: {
                query: `guard_${guardId}`,
                result: violation.details,
                witnesses: []
              },
              metrics: {
                confidence: 1.0,
                coverage: 1.0,
                latency_ms: 0
              },
              tags: ['guard', guardId]
            });
          }
          this.emit('guard_complete', { guardId, violations: violations.length });
        } catch (err) {
          errors.push({ guard: guardId, error: err.message });
          this.emit('guard_error', { guardId, error: err.message });
        }
      }

      // Phase 4: Shard Merging
      let shardHash = '';
      let shardCount = 1;

      if (config.distributed) {
        try {
          const shards = await this.storage.fetchShards?.();
          if (shards && shards.length > 0) {
            shardCount = shards.length;
            // Hash all shards together for determinism
            const shardData = shards.map(s => s.probe_run_id).sort().join('|');
            shardHash = await this.hashString(shardData);
            this.emit('shards_merged', { shardCount, shardHash });
          }
        } catch (err) {
          errors.push({ operation: 'shard_merge', error: err.message });
          this.emit('shard_merge_error', { error: err.message });
        }
      } else {
        shardHash = await this.hashString('');
      }

      // Phase 5: Artifact Generation
      const endTime = Date.now();
      const executionTime = endTime - startTime;

      const artifact = {
        version: '1.0',
        universe_id: config.universe_id,
        snapshot_id: config.snapshot_id || 'current',
        generated_at: new Date(endTime).toISOString(),
        probe_run_id: runId,
        shard_count: shardCount,
        shard_hash: shardHash,
        observations: observations,
        summary: computeArtifactSummary(observations),
        metadata: {
          agents_run: agentIds,
          guards_applied: guardIds,
          execution_time_ms: executionTime,
          storage_backend: this.storage.type,
          config: config
        },
        integrity: {
          checksum: await hashObservations(observations),
          verified_at: null
        }
      };

      // Persistence
      if (config.persist) {
        try {
          await this.storage.saveArtifact(artifact);
          this.emit('artifact_saved', { runId, artifactSize: observations.length });
        } catch (err) {
          errors.push({ operation: 'persist', error: err.message });
          this.emit('artifact_save_error', { error: err.message });
        }
      }

      this.emit('scan_complete', {
        runId,
        status: errors.length === 0 ? 'success' : 'partial',
        executionTime,
        observationCount: observations.length
      });

      return {
        artifact,
        status: errors.length === 0 ? 'success' : 'partial',
        errors
      };
    } catch (err) {
      this.emit('scan_error', { runId, error: err.message });
      throw err;
    }
  }

  /**
   * Execute single agent
   * @param {string} agentId - Agent identifier
   * @param {Object} config - Scan config
   * @param {Array} observations - Shared observations array
   * @param {Array} errors - Shared errors array
   * @private
   */
  async executeAgent(agentId, config, observations, errors) {
    const startTime = Date.now();
    try {
      const agent = this.agents.get(agentId);
      if (!agent) {
        throw new Error(`Agent not found: ${agentId}`);
      }

      // Call agent scan (would need store passed in real impl)
      const results = await agent.scan(config);

      if (Array.isArray(results)) {
        observations.push(...results);
      }

      const endTime = Date.now();
      this.emit('agent_complete', {
        agentId,
        observationCount: results.length,
        latency: endTime - startTime
      });
    } catch (err) {
      errors.push({ agent: agentId, error: err.message });
      this.emit('agent_error', { agentId, error: err.message });
    }
  }

  /**
   * Hash string using Blake3
   * @param {string} data - Data to hash
   * @returns {Promise<string>} Hex-encoded hash
   * @private
   */
  async hashString(data) {
    // In production, use hash-wasm for Blake3
    // For now, return placeholder
    const encoder = new TextEncoder();
    const buffer = encoder.encode(data);
    // Would be: blake3(buffer).then(h => h.toString('hex'))
    return 'blake3_placeholder_' + buffer.length.toString(16).padStart(64, '0');
  }

  /**
   * Load artifact from storage
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<Artifact>} Loaded artifact
   */
  async loadArtifact(artifactId) {
    return this.storage.loadArtifact(artifactId);
  }

  /**
   * List all artifacts in storage
   * @returns {Promise<Artifact[]>} Array of artifacts
   */
  async listArtifacts() {
    return this.storage.listArtifacts();
  }
}

/**
 * Create ProbeOrchestrator instance
 * @param {Object} options - Configuration
 * @returns {ProbeOrchestrator} New orchestrator
 */
export function createProbeOrchestrator(options) {
  return new ProbeOrchestrator(options);
}
