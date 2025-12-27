#!/usr/bin/env node
/**
 * @fileoverview Deterministic merge logic for KGC Probe observations
 *
 * Merges observations from multiple agents with stable, deterministic ordering.
 * Guarantees: Same input → same output hash (critical for receipt verification).
 *
 * Design principles:
 * - Deterministic: Stable sort order, canonical representation
 * - Composable: Works with any observation source
 * - Verifiable: Produces consistent hashes
 * - Fast: O(n log n) merge with minimal overhead
 */

/**
 * Severity order for stable sorting (highest priority first)
 */
const SEVERITY_ORDER = {
  fatal: 0,
  error: 1,
  warn: 2,
  info: 3,
  debug: 4,
  trace: 5
};

/**
 * Merge observations from multiple sources with deterministic ordering
 *
 * Stable sort order:
 * 1. Category (alphabetical)
 * 2. Severity (fatal > error > warn > info > debug > trace)
 * 3. File path (alphabetical)
 * 4. Line number (ascending)
 * 5. Agent ID (alphabetical)
 * 6. Timestamp (ascending)
 *
 * @param {import('./observation.mjs').Observation[]} observations - Observations to merge
 * @returns {import('./observation.mjs').Observation[]} - Sorted observations
 */
export function mergeObservations(observations) {
  // Stable sort (creates new array)
  return observations.slice().sort((a, b) => {
    // 1. Category (alphabetical)
    if (a.category !== b.category) {
      return a.category.localeCompare(b.category);
    }

    // 2. Severity (fatal > error > warn > info > debug > trace)
    if (a.severity !== b.severity) {
      return SEVERITY_ORDER[a.severity] - SEVERITY_ORDER[b.severity];
    }

    // 3. File path (alphabetical)
    const aFile = a.location?.file || '';
    const bFile = b.location?.file || '';
    if (aFile !== bFile) {
      return aFile.localeCompare(bFile);
    }

    // 4. Line number (ascending)
    const aLine = a.location?.line || 0;
    const bLine = b.location?.line || 0;
    if (aLine !== bLine) {
      return aLine - bLine;
    }

    // 5. Agent ID (alphabetical)
    if (a.metadata.agentId !== b.metadata.agentId) {
      return a.metadata.agentId.localeCompare(b.metadata.agentId);
    }

    // 6. Timestamp (ascending)
    return a.metadata.timestamp.localeCompare(b.metadata.timestamp);
  });
}

/**
 * Merge observations from multiple agent shards
 *
 * @param {Map<string, import('./observation.mjs').Observation[]>} shards - Agent ID → observations
 * @returns {import('./observation.mjs').Observation[]} - Merged and sorted observations
 */
export function mergeShards(shards) {
  const allObservations = [];

  for (const [agentId, observations] of shards.entries()) {
    allObservations.push(...observations);
  }

  return mergeObservations(allObservations);
}

/**
 * Deduplicate observations by content hash
 *
 * Useful when multiple agents might emit identical observations.
 *
 * @param {import('./observation.mjs').Observation[]} observations
 * @returns {import('./observation.mjs').Observation[]}
 */
export function deduplicateObservations(observations) {
  const seen = new Set();
  const unique = [];

  for (const obs of observations) {
    // Create deterministic content key
    const key = JSON.stringify({
      category: obs.category,
      severity: obs.severity,
      message: obs.message,
      location: obs.location,
      data: obs.data,
      tags: obs.tags.slice().sort()
    }, Object.keys(obs).sort());

    if (!seen.has(key)) {
      seen.add(key);
      unique.push(obs);
    }
  }

  return unique;
}

/**
 * Group observations by category
 *
 * @param {import('./observation.mjs').Observation[]} observations
 * @returns {Map<string, import('./observation.mjs').Observation[]>}
 */
export function groupByCategory(observations) {
  const groups = new Map();

  for (const obs of observations) {
    if (!groups.has(obs.category)) {
      groups.set(obs.category, []);
    }
    groups.get(obs.category).push(obs);
  }

  return groups;
}

/**
 * Group observations by severity
 *
 * @param {import('./observation.mjs').Observation[]} observations
 * @returns {Map<string, import('./observation.mjs').Observation[]>}
 */
export function groupBySeverity(observations) {
  const groups = new Map();

  for (const obs of observations) {
    if (!groups.has(obs.severity)) {
      groups.set(obs.severity, []);
    }
    groups.get(obs.severity).push(obs);
  }

  return groups;
}

/**
 * Filter observations by category
 *
 * @param {import('./observation.mjs').Observation[]} observations
 * @param {string[]} categories - Categories to include
 * @returns {import('./observation.mjs').Observation[]}
 */
export function filterByCategory(observations, categories) {
  const categorySet = new Set(categories);
  return observations.filter(obs => categorySet.has(obs.category));
}

/**
 * Filter observations by severity (minimum level)
 *
 * @param {import('./observation.mjs').Observation[]} observations
 * @param {string} minSeverity - Minimum severity level
 * @returns {import('./observation.mjs').Observation[]}
 */
export function filterBySeverity(observations, minSeverity) {
  const minLevel = SEVERITY_ORDER[minSeverity];
  return observations.filter(obs => SEVERITY_ORDER[obs.severity] <= minLevel);
}

export default {
  mergeObservations,
  mergeShards,
  deduplicateObservations,
  groupByCategory,
  groupBySeverity,
  filterByCategory,
  filterBySeverity
};
