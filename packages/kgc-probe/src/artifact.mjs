/**
 * @fileoverview KGC Probe - Artifact Operations
 *
 * Artifact management operations:
 * - Deterministic hashing (Blake3)
 * - Shard merging with deduplication
 * - Diff computation
 * - Verification
 *
 * @module @unrdf/kgc-probe/artifact
 */

import { randomUUID } from 'crypto';
import { ArtifactSchema, validateArtifact, DiffResultSchema } from './types.mjs';
import { canonicalJson, mergeObservations, sortObservations } from './orchestration-core.mjs';

/**
 * ObservationValidator - Validates observations against schema
 * @class ObservationValidator
 */
export class ObservationValidator {
  /**
   * Validate single observation
   * @param {unknown} data - Data to validate
   * @returns {Object} Validated observation
   * @throws {Error} If validation fails
   */
  validate(data) {
    // In production, use Zod
    if (!data || typeof data !== 'object') {
      throw new Error('Observation must be an object');
    }

    const required = ['id', 'agent', 'timestamp', 'kind', 'severity', 'subject'];
    for (const field of required) {
      if (!data[field]) {
        throw new Error(`Missing required field: ${field}`);
      }
    }

    return data;
  }

  /**
   * Validate batch of observations
   * @param {Array} observations - Observations to validate
   * @returns {Object[]} Validated observations
   */
  validateBatch(observations) {
    return observations.map(obs => this.validate(obs));
  }
}

/**
 * Create ObservationValidator instance
 * @returns {ObservationValidator}
 */
export function createObservationValidator() {
  return new ObservationValidator();
}

// ============================================================================
// HASHING OPERATIONS
// ============================================================================

/**
 * Hash observations deterministically
 *
 * Algorithm:
 * 1. Sort observations by (agent, timestamp, subject)
 * 2. Stringify each observation's core fields
 * 3. Compute Blake3 hash of concatenated strings
 *
 * @param {Array} observations - Observations to hash
 * @returns {Promise<string>} Hex-encoded Blake3 hash
 */
export async function hashObservations(observations) {
  const canonical = canonicalJson(sortObservations(observations || []));
  try {
    const { blake3 } = await import('hash-wasm');
    return blake3(canonical);
  } catch {
    const { createHash } = await import('node:crypto');
    return createHash('sha256').update(canonical).digest('hex');
  }
}

// ============================================================================
// SHARD OPERATIONS
// ============================================================================

/**
 * Merge multiple shards with deduplication
 *
 * Algorithm (Merge):
 * Phase 1: Collect all observations from shards
 * Phase 2: Add new observations
 * Phase 3: Dedup by content hash
 * Phase 4: Sort deterministically
 *
 * @param {Array} shards - Array of artifacts to merge
 * @param {Array} newObservations - Additional observations to merge
 * @returns {Promise<Array>} Merged and deduplicated observations
 */
export async function mergeShards(shards, newObservations = [], options = {}) {
  return mergeObservations(shards || [], newObservations || [], options).observations;
}

// ============================================================================
// DIFF OPERATIONS
// ============================================================================

/**
 * Compute diff between two artifacts
 *
 * Algorithm (Diff):
 * 1. Build sets of observations from each artifact
 * 2. Find added (in artifact2 only)
 * 3. Find removed (in artifact1 only)
 * 4. Find modified (same subject/predicate but different value)
 * 5. Calculate Jaccard similarity
 *
 * @param {Object} artifact1 - First artifact
 * @param {Object} artifact2 - Second artifact
 * @returns {Object} Diff result with added/removed/modified
 */
export function diffArtifacts(artifact1, artifact2) {
  const obs1 = artifact1.observations || [];
  const obs2 = artifact2.observations || [];

  // Create keys for matching
  const key = obs => `${obs.subject}|${obs.predicate}|${obs.object}`;
  const map1 = new Map(obs1.map(o => [key(o), o]));
  const map2 = new Map(obs2.map(o => [key(o), o]));

  const added = [];
  const removed = [];
  const modified = [];

  // Find added
  for (const [k, obs] of map2) {
    if (!map1.has(k)) {
      added.push(obs);
    }
  }

  // Find removed
  for (const [k, obs] of map1) {
    if (!map2.has(k)) {
      removed.push(obs);
    }
  }

  // Find modified (same subject/predicate, different attributes)
  const subPredKey = obs => `${obs.subject}|${obs.predicate}`;
  const map1BySubPred = new Map();
  const map2BySubPred = new Map();

  for (const obs of obs1) {
    const k = subPredKey(obs);
    if (!map1BySubPred.has(k)) {
      map1BySubPred.set(k, []);
    }
    map1BySubPred.get(k).push(obs);
  }

  for (const obs of obs2) {
    const k = subPredKey(obs);
    if (!map2BySubPred.has(k)) {
      map2BySubPred.set(k, []);
    }
    map2BySubPred.get(k).push(obs);
  }

  for (const [k, obs1List] of map1BySubPred) {
    const obs2List = map2BySubPred.get(k) || [];
    if (obs2List.length > 0 && obs1List[0].object !== obs2List[0].object) {
      modified.push({
        subject: obs1List[0].subject,
        predicate: obs1List[0].predicate,
        before: obs1List[0].object,
        after: obs2List[0].object,
        old_observation: obs1List[0],
        new_observation: obs2List[0],
      });
    }
  }

  // Calculate Jaccard similarity
  const intersection = obs1.length + obs2.length - added.length - removed.length;
  const union = obs1.length + added.length;
  const similarity = union > 0 ? intersection / union : 1.0;

  return {
    added,
    removed,
    modified,
    summary: {
      total_changes: added.length + removed.length + modified.length,
      similarity_ratio: similarity,
      artifact1_size: obs1.length,
      artifact2_size: obs2.length,
    },
  };
}

// ============================================================================
// VERIFICATION
// ============================================================================

/**
 * Verify artifact integrity
 *
 * Verification steps:
 * 1. Recompute checksum from observations
 * 2. Compare with stored checksum
 * 3. Validate schema
 *
 * @param {Object} artifact - Artifact to verify
 * @returns {Promise<{valid: boolean, errors: string[]}>} Verification result
 */
export async function verifyArtifact(artifact) {
  const errors = [];

  try {
    // Validate schema
    validateArtifact(artifact);
  } catch (err) {
    errors.push(`Schema validation failed: ${err.message}`);
  }

  // Recompute checksum
  const expectedChecksum = await hashObservations(artifact.observations);

  if (expectedChecksum !== artifact.integrity.checksum) {
    errors.push(
      `Checksum mismatch: expected ${expectedChecksum}, got ${artifact.integrity.checksum}`
    );
  }

  // Validate summary
  const computedSummary = computeArtifactSummary(artifact.observations);
  if (computedSummary.total !== artifact.summary.total) {
    errors.push(
      `Summary mismatch: expected ${computedSummary.total} observations, got ${artifact.summary.total}`
    );
  }

  return {
    valid: errors.length === 0,
    errors,
    verified_at: new Date().toISOString(),
  };
}

// ============================================================================
// SUMMARY & SERIALIZATION
// ============================================================================

/**
 * Compute artifact summary from observations
 *
 * Aggregates:
 * - Total count
 * - Count by kind
 * - Count by severity
 * - Mean confidence and coverage
 *
 * @param {Array} observations - Observations to summarize
 * @returns {Object} Summary object
 */
export function computeArtifactSummary(observations) {
  const summary = {
    total: observations.length,
    by_kind: {},
    by_severity: {
      critical: 0,
      warning: 0,
      info: 0,
    },
    confidence_mean: 0,
    coverage_mean: 0,
  };

  let confidenceSum = 0;
  let coverageSum = 0;
  let metricsCount = 0;

  for (const obs of observations) {
    // Count by kind
    summary.by_kind[obs.kind] = (summary.by_kind[obs.kind] || 0) + 1;

    // Count by severity
    if (summary.by_severity.hasOwnProperty(obs.severity)) {
      summary.by_severity[obs.severity]++;
    }

    // Aggregate metrics
    if (obs.metrics) {
      confidenceSum += obs.metrics.confidence || 0;
      coverageSum += obs.metrics.coverage || 0;
      metricsCount++;
    }
  }

  if (metricsCount > 0) {
    summary.confidence_mean = confidenceSum / metricsCount;
    summary.coverage_mean = coverageSum / metricsCount;
  }

  return summary;
}

/**
 * Serialize artifact to JSON string
 * @param {Object} artifact - Artifact to serialize
 * @returns {string} JSON string
 */
export function serializeArtifact(artifact) {
  return JSON.stringify(artifact, null, 2);
}

/**
 * Deserialize artifact from JSON string
 * @param {string} jsonStr - JSON string
 * @returns {Object} Deserialized artifact
 * @throws {Error} If parse fails
 */
export function deserializeArtifact(jsonStr) {
  try {
    const data = JSON.parse(jsonStr);
    return validateArtifact(data);
  } catch (err) {
    throw new Error(`Failed to deserialize artifact: ${err.message}`);
  }
}
