/**
 * @fileoverview Precalculated Shards Fixtures
 *
 * Contains 10 agent outputs with:
 * - Observations for each domain
 * - Receipts and hashes
 * - Expected merge output
 * - Expected conflicts
 *
 * @module @unrdf/kgc-probe/test/fixtures/precalculated-shards
 */

import { FROZEN_TIMESTAMP } from './frozen-environment.mjs';

/**
 * Generate deterministic UUID based on agent and index
 * @param {string} agent - Agent name
 * @param {number} index - Observation index
 * @returns {string} UUID
 */
function generateUUID(agent, index) {
  const hash = `${agent}_${index}`.padStart(32, '0').slice(0, 32);
  return `${hash.slice(0, 8)}-${hash.slice(8, 12)}-4${hash.slice(13, 16)}-8${hash.slice(17, 20)}-${hash.slice(20, 32)}`;
}

/**
 * Create observation for a given agent and domain
 * @param {string} agentId - Agent identifier
 * @param {string} kind - Observation kind
 * @param {number} index - Index for uniqueness
 * @param {Object} overrides - Field overrides
 * @returns {Object} Observation
 */
export function createObservation(agentId, kind, index, overrides = {}) {
  return {
    id: generateUUID(agentId, index),
    agent: agentId,
    timestamp: FROZEN_TIMESTAMP,
    kind,
    severity: overrides.severity || 'info',
    subject: overrides.subject || `example:entity${index}`,
    predicate: overrides.predicate || 'rdf:type',
    object: overrides.object || `example:Type${index}`,
    evidence: {
      query: overrides.query || `SELECT ?s WHERE { ?s a example:Type${index} }`,
      result: overrides.result || { count: index + 1 },
      witnesses: overrides.witnesses || [`example:entity${index}`]
    },
    metrics: {
      confidence: overrides.confidence ?? 0.95 - (index * 0.01),
      coverage: overrides.coverage ?? 0.90 - (index * 0.01),
      latency_ms: overrides.latency ?? 100 + (index * 10)
    },
    tags: overrides.tags || [kind, agentId]
  };
}

// ============================================================================
// 10 AGENT SHARDS
// ============================================================================

/**
 * Shard 1: Runtime Agent (Node.js runtime observations)
 */
export const SHARD_RUNTIME = {
  probe_run_id: 'runtime-shard-001',
  universe_id: 'test-universe',
  agent: 'runtime',
  observations: [
    createObservation('runtime', 'runtime_check', 0, {
      subject: 'node:version',
      predicate: 'sys:version',
      object: 'v22.12.0',
      confidence: 0.99
    }),
    createObservation('runtime', 'runtime_check', 1, {
      subject: 'node:memory',
      predicate: 'sys:heapUsed',
      object: '30MB',
      severity: 'info'
    }),
    createObservation('runtime', 'runtime_check', 2, {
      subject: 'node:features',
      predicate: 'sys:wasmSupport',
      object: 'true'
    })
  ],
  hash: 'a'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 2: Filesystem Agent
 */
export const SHARD_FS = {
  probe_run_id: 'fs-shard-002',
  universe_id: 'test-universe',
  agent: 'filesystem',
  observations: [
    createObservation('filesystem', 'fs_check', 0, {
      subject: 'file:/project/package.json',
      predicate: 'fs:exists',
      object: 'true'
    }),
    createObservation('filesystem', 'fs_check', 1, {
      subject: 'file:/project/src',
      predicate: 'fs:isDirectory',
      object: 'true'
    }),
    createObservation('filesystem', 'fs_check', 2, {
      subject: 'file:/project/node_modules',
      predicate: 'fs:size',
      object: '0',
      severity: 'warning'
    })
  ],
  hash: 'b'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 3: WASM Agent
 */
export const SHARD_WASM = {
  probe_run_id: 'wasm-shard-003',
  universe_id: 'test-universe',
  agent: 'wasm',
  observations: [
    createObservation('wasm', 'wasm_check', 0, {
      subject: 'wasm:support',
      predicate: 'sys:available',
      object: 'true'
    }),
    createObservation('wasm', 'wasm_check', 1, {
      subject: 'wasm:memory',
      predicate: 'sys:maxPages',
      object: '65536'
    })
  ],
  hash: 'c'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 4: Performance Agent
 */
export const SHARD_PERF = {
  probe_run_id: 'perf-shard-004',
  universe_id: 'test-universe',
  agent: 'performance',
  observations: [
    createObservation('performance', 'perf_check', 0, {
      subject: 'perf:eventLoop',
      predicate: 'sys:latency',
      object: '1.5ms'
    }),
    createObservation('performance', 'perf_check', 1, {
      subject: 'perf:gcPause',
      predicate: 'sys:duration',
      object: '2.3ms'
    }),
    createObservation('performance', 'perf_check', 2, {
      subject: 'perf:heapGrowth',
      predicate: 'sys:rate',
      object: '0.5MB/s',
      severity: 'warning'
    })
  ],
  hash: 'd'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 5: Network Agent
 */
export const SHARD_NET = {
  probe_run_id: 'net-shard-005',
  universe_id: 'test-universe',
  agent: 'network',
  observations: [
    createObservation('network', 'net_check', 0, {
      subject: 'net:localhost',
      predicate: 'sys:reachable',
      object: 'true'
    }),
    createObservation('network', 'net_check', 1, {
      subject: 'net:dns',
      predicate: 'sys:resolver',
      object: '127.0.0.1'
    })
  ],
  hash: 'e'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 6: Tooling Agent
 */
export const SHARD_TOOLING = {
  probe_run_id: 'tooling-shard-006',
  universe_id: 'test-universe',
  agent: 'tooling',
  observations: [
    createObservation('tooling', 'tool_check', 0, {
      subject: 'tool:pnpm',
      predicate: 'sys:version',
      object: '9.15.0'
    }),
    createObservation('tooling', 'tool_check', 1, {
      subject: 'tool:vitest',
      predicate: 'sys:version',
      object: '4.0.15'
    }),
    createObservation('tooling', 'tool_check', 2, {
      subject: 'tool:eslint',
      predicate: 'sys:configured',
      object: 'true'
    })
  ],
  hash: 'f'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 7: Storage Agent
 */
export const SHARD_STORAGE = {
  probe_run_id: 'storage-shard-007',
  universe_id: 'test-universe',
  agent: 'storage',
  observations: [
    createObservation('storage', 'storage_check', 0, {
      subject: 'storage:disk',
      predicate: 'sys:available',
      object: '100GB'
    }),
    createObservation('storage', 'storage_check', 1, {
      subject: 'storage:temp',
      predicate: 'sys:writable',
      object: 'true'
    })
  ],
  hash: '1'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 8: Concurrency Agent
 */
export const SHARD_CONCURRENCY = {
  probe_run_id: 'concurrency-shard-008',
  universe_id: 'test-universe',
  agent: 'concurrency',
  observations: [
    createObservation('concurrency', 'concurrency_check', 0, {
      subject: 'workers:pool',
      predicate: 'sys:maxWorkers',
      object: '8'
    }),
    createObservation('concurrency', 'concurrency_check', 1, {
      subject: 'workers:active',
      predicate: 'sys:count',
      object: '0'
    })
  ],
  hash: '2'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 9: Limits Agent
 */
export const SHARD_LIMITS = {
  probe_run_id: 'limits-shard-009',
  universe_id: 'test-universe',
  agent: 'limits',
  observations: [
    createObservation('limits', 'limits_check', 0, {
      subject: 'limits:openFiles',
      predicate: 'sys:max',
      object: '65536'
    }),
    createObservation('limits', 'limits_check', 1, {
      subject: 'limits:processes',
      predicate: 'sys:max',
      object: '32768'
    }),
    createObservation('limits', 'limits_check', 2, {
      subject: 'limits:memory',
      predicate: 'sys:max',
      object: '4GB'
    })
  ],
  hash: '3'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * Shard 10: System Agent
 */
export const SHARD_SYSTEM = {
  probe_run_id: 'system-shard-010',
  universe_id: 'test-universe',
  agent: 'system',
  observations: [
    createObservation('system', 'system_check', 0, {
      subject: 'sys:os',
      predicate: 'sys:platform',
      object: 'linux'
    }),
    createObservation('system', 'system_check', 1, {
      subject: 'sys:arch',
      predicate: 'sys:type',
      object: 'x64'
    }),
    createObservation('system', 'system_check', 2, {
      subject: 'sys:uptime',
      predicate: 'sys:seconds',
      object: '3600'
    })
  ],
  hash: '4'.repeat(64),
  timestamp: FROZEN_TIMESTAMP
};

/**
 * All 10 shards in array form
 */
export const ALL_SHARDS = [
  SHARD_RUNTIME,
  SHARD_FS,
  SHARD_WASM,
  SHARD_PERF,
  SHARD_NET,
  SHARD_TOOLING,
  SHARD_STORAGE,
  SHARD_CONCURRENCY,
  SHARD_LIMITS,
  SHARD_SYSTEM
];

/**
 * Expected merged output (all observations combined)
 */
export const EXPECTED_MERGED = {
  total_observations: ALL_SHARDS.reduce((sum, s) => sum + s.observations.length, 0),
  unique_agents: ALL_SHARDS.map(s => s.agent),
  shard_count: ALL_SHARDS.length,
  observations: ALL_SHARDS.flatMap(s => s.observations).sort((a, b) =>
    a.timestamp.localeCompare(b.timestamp) || a.agent.localeCompare(b.agent)
  )
};

/**
 * Expected conflicts (observations with same subject/predicate but different values)
 */
export const EXPECTED_CONFLICTS = [];

/**
 * Shard receipts with hash chains
 */
export const SHARD_RECEIPTS = ALL_SHARDS.map((shard, index) => ({
  shard_id: shard.probe_run_id,
  agent: shard.agent,
  observation_count: shard.observations.length,
  hash: shard.hash,
  prev_hash: index === 0 ? '0'.repeat(64) : ALL_SHARDS[index - 1].hash,
  merkle_root: computeMerkleRoot(shard.observations),
  timestamp: shard.timestamp
}));

/**
 * Simple merkle root computation for testing
 * @param {Array} observations - Observations to hash
 * @returns {string} Merkle root hash
 */
function computeMerkleRoot(observations) {
  if (observations.length === 0) return '0'.repeat(64);

  const hashes = observations.map(obs =>
    simpleHash(`${obs.agent}|${obs.subject}|${obs.kind}`)
  );

  while (hashes.length > 1) {
    const newHashes = [];
    for (let i = 0; i < hashes.length; i += 2) {
      const left = hashes[i];
      const right = hashes[i + 1] || left;
      newHashes.push(simpleHash(left + right));
    }
    hashes.length = 0;
    hashes.push(...newHashes);
  }

  return hashes[0];
}

/**
 * Simple deterministic hash for testing
 * @param {string} data - Data to hash
 * @returns {string} 64-char hex hash
 */
function simpleHash(data) {
  let hash = 0;
  for (let i = 0; i < data.length; i++) {
    const char = data.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash;
  }
  return Math.abs(hash).toString(16).padStart(64, '0').slice(0, 64);
}

export default {
  ALL_SHARDS,
  SHARD_RUNTIME,
  SHARD_FS,
  SHARD_WASM,
  SHARD_PERF,
  SHARD_NET,
  SHARD_TOOLING,
  SHARD_STORAGE,
  SHARD_CONCURRENCY,
  SHARD_LIMITS,
  SHARD_SYSTEM,
  EXPECTED_MERGED,
  EXPECTED_CONFLICTS,
  SHARD_RECEIPTS,
  createObservation
};
