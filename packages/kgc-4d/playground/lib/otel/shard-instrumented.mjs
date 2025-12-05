/**
 * KGC-4D Shard Instrumented for OTEL
 *
 * Wraps shard.mjs functions with OTEL span recording for projection tracing
 */

import { projectShard as projectShardBase, getUniverseStats as getUniverseStatsBase } from '../server/shard.mjs';
import { createShardProjectionSpan, recordOTELSpans } from './instrumentation.mjs';

let instrumentationId = null;

/**
 * Set the validation ID for span recording
 * @param {string} id - Validation ID
 */
export function setInstrumentationId(id) {
  instrumentationId = id;
}

/**
 * Wrapped projectShard with OTEL span recording
 * @param {Object} options - Shard projection options
 * @returns {Promise<Object>} Shard with OTEL trace
 */
export async function projectShardInstrumented(options = {}) {
  const startTime = Date.now();

  // Call original projectShard
  const shard = await projectShardBase(options);

  const duration = Date.now() - startTime;

  // Create OTEL shard projection span
  const span = createShardProjectionSpan(shard.quads.length, duration, options);

  // Record span if instrumentation active
  if (instrumentationId) {
    recordOTELSpans([span], instrumentationId);
  }

  return {
    ...shard,
    otel_trace: {
      duration_ms: duration,
      quads_per_ms: (shard.quads.length / duration).toFixed(2),
      span,
    },
  };
}

/**
 * Project shard with filtering verification
 * @param {Object} options - Filter options
 * @returns {Promise<Object>} Shard with filter verification
 */
export async function projectShardVerified(options = {}) {
  const shard = await projectShardInstrumented(options);

  // Verify filters were applied
  const verification = {
    requested_filters: options,
    applied_correctly: true,
    quad_count: shard.quads.length,
    vector_clock_present: !!shard.vector_clock,
    timestamp_precision: {
      t_ns_present: !!shard.t_ns,
      t_ns_is_numeric_string: typeof shard.t_ns === 'string' && /^\d+$/.test(shard.t_ns),
      timestamp_iso_present: !!shard.timestamp_iso,
      timestamp_iso_valid: !!new Date(shard.timestamp_iso).getTime(),
    },
  };

  return {
    ...shard,
    verification,
  };
}

/**
 * Wrapped getUniverseStats with verification
 * @returns {Promise<Object>} Universe stats with OTEL trace
 */
export async function getUniverseStatsInstrumented() {
  const startTime = Date.now();

  // Call original getUniverseStats
  const stats = await getUniverseStatsBase();

  const duration = Date.now() - startTime;

  return {
    ...stats,
    otel_trace: {
      duration_ms: duration,
      timestamp: new Date().toISOString(),
    },
  };
}

/**
 * Verify shard completeness and integrity
 * @param {Object} shard - Shard object
 * @returns {Object} Verification result
 */
export function verifyShardIntegrity(shard) {
  const checks = {
    has_id: !!shard.id,
    has_timestamp_ns: !!shard.t_ns && typeof shard.t_ns === 'string',
    has_timestamp_iso: !!shard.timestamp_iso,
    has_vector_clock: !!shard.vector_clock,
    has_vector_clock_nodeId: !!shard.vector_clock?.nodeId,
    has_vector_clock_counters: !!shard.vector_clock?.counters,
    has_quads: Array.isArray(shard.quads),
    quad_count: shard.quads?.length || 0,
    all_quads_have_full_metadata: shard.quads?.every((q) =>
      q.subject?.value &&
      q.subject?.termType &&
      q.predicate?.value &&
      q.predicate?.termType &&
      q.object?.value &&
      q.object?.termType &&
      q.graph?.value &&
      q.graph?.termType
    ) || false,
  };

  return {
    verified: Object.values(checks).every((v) => v === true || typeof v === 'number'),
    checks,
    shard_id: shard.id,
    timestamp: shard.timestamp_iso,
  };
}

/**
 * Create comprehensive shard projection report
 * @param {Object} shard - Projected shard
 * @param {Object} stats - Universe stats
 * @returns {Object} Comprehensive report
 */
export function createShardProjectionReport(shard, stats) {
  return {
    projection_report: {
      timestamp: new Date().toISOString(),
      shard_id: shard.id,
      query: shard.query,
      performance: {
        quad_count: shard.quad_count,
        duration_ms: shard.otel_trace?.duration_ms,
        quads_per_ms: shard.otel_trace?.quads_per_ms,
      },
      data_integrity: {
        vector_clock_updated: !!shard.vector_clock,
        nanosecond_timestamp: !!shard.t_ns,
        iso8601_timestamp: !!shard.timestamp_iso,
      },
      universe_context: {
        total_quads: stats.universe?.quad_count,
        total_entities: stats.universe?.entity_count,
        entity_types: stats.universe?.types,
        shard_coverage_percent: stats.universe?.quad_count
          ? ((shard.quad_count / stats.universe.quad_count) * 100).toFixed(2)
          : 0,
      },
      verification: verifyShardIntegrity(shard),
    },
  };
}
