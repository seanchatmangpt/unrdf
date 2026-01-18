/**
 * Impact Set Formatting and Serialization
 *
 * Provides deterministic JSON serialization and human-readable formatting
 * for impact sets. Critical for hashing, receipts, and debugging.
 *
 * @module format
 */

/**
 * @typedef {Object} ImpactSetJSON
 * @property {Array<string>} subjects - Sorted array of subject IRIs
 * @property {Array<string>} predicates - Sorted array of predicate IRIs
 * @property {Array<string>} graphs - Sorted array of graph IRIs
 * @property {Object} cardinality - Quad counts
 * @property {number} cardinality.quadsAdded - Quads added
 * @property {number} cardinality.quadsDeleted - Quads deleted
 * @property {number} cardinality.netChange - Net change
 * @property {Object} resourcesAffected - Resource metrics
 * @property {number} resourcesAffected.directAffected - Direct resources
 * @property {number} [resourcesAffected.transitive] - Transitive (optional)
 */

/**
 * Convert ImpactSet to deterministic JSON
 *
 * CRITICAL: Must be deterministic for hashing/receipts.
 * Two identical impact sets MUST produce identical JSON.
 *
 * Algorithm:
 * 1. Convert Sets to Arrays
 * 2. Sort arrays lexicographically
 * 3. Return plain object (serializable)
 *
 * Complexity: O(k log k) where k = total unique IRIs
 *
 * @param {import('./impact.mjs').ImpactSet} impact - Impact set to serialize
 * @returns {ImpactSetJSON} Deterministic JSON-serializable object
 */
export function impactSetToJSON(impact) {
  return {
    subjects: Array.from(impact.subjects).sort(),
    predicates: Array.from(impact.predicates).sort(),
    graphs: Array.from(impact.graphs).sort(),
    cardinality: {
      quadsAdded: impact.cardinality.quadsAdded,
      quadsDeleted: impact.cardinality.quadsDeleted,
      netChange: impact.cardinality.netChange,
    },
    resourcesAffected: {
      directAffected: impact.resourcesAffected.directAffected,
      ...(impact.resourcesAffected.transitive !== undefined && {
        transitive: impact.resourcesAffected.transitive,
      }),
    },
  };
}

/**
 * Restore ImpactSet from JSON
 *
 * Converts JSON representation back to ImpactSet with Set objects.
 * Inverse of impactSetToJSON.
 *
 * @param {ImpactSetJSON} json - JSON representation
 * @returns {import('./impact.mjs').ImpactSet} Restored impact set with Sets
 */
export function impactSetFromJSON(json) {
  return {
    subjects: new Set(json.subjects),
    predicates: new Set(json.predicates),
    graphs: new Set(json.graphs),
    cardinality: {
      quadsAdded: json.cardinality.quadsAdded,
      quadsDeleted: json.cardinality.quadsDeleted,
      netChange: json.cardinality.netChange,
    },
    resourcesAffected: {
      directAffected: json.resourcesAffected.directAffected,
      ...(json.resourcesAffected.transitive !== undefined && {
        transitive: json.resourcesAffected.transitive,
      }),
    },
  };
}

/**
 * Format impact set as human-readable summary
 *
 * Produces a text-based summary suitable for debugging,
 * logging, or user display.
 *
 * Output format:
 * ```
 * Impact Set Summary
 * ==================
 * Resources Affected: N subjects
 * Properties Used: M predicates
 * Graphs Modified: K named graphs
 *
 * Changes:
 *   + X quads added
 *   - Y quads deleted
 *   Δ Z net change
 *
 * Subjects:
 *   - http://example.org/subject1
 *   - http://example.org/subject2
 *   ...
 * ```
 *
 * @param {import('./impact.mjs').ImpactSet} impact - Impact set to format
 * @param {Object} [options] - Formatting options
 * @param {number} [options.maxItems=10] - Maximum items to show per section
 * @returns {string} Human-readable summary
 */
export function formatImpactSummary(impact, options = {}) {
  const { maxItems = 10 } = options;

  const lines = [];

  lines.push('Impact Set Summary');
  lines.push('==================');
  lines.push('');

  // Resource metrics
  lines.push(`Resources Affected: ${impact.resourcesAffected.directAffected} subjects`);
  lines.push(`Properties Used: ${impact.predicates.size} predicates`);
  lines.push(`Graphs Modified: ${impact.graphs.size} named graphs`);
  lines.push('');

  // Changes
  lines.push('Changes:');
  lines.push(`  + ${impact.cardinality.quadsAdded} quads added`);
  lines.push(`  - ${impact.cardinality.quadsDeleted} quads deleted`);
  lines.push(`  Δ ${impact.cardinality.netChange} net change`);
  lines.push('');

  // Subjects section
  if (impact.subjects.size > 0) {
    lines.push('Subjects:');
    const subjects = Array.from(impact.subjects).sort();
    const displaySubjects = subjects.slice(0, maxItems);

    for (const subject of displaySubjects) {
      lines.push(`  - ${subject}`);
    }

    if (subjects.length > maxItems) {
      lines.push(`  ... and ${subjects.length - maxItems} more`);
    }
    lines.push('');
  }

  // Predicates section
  if (impact.predicates.size > 0) {
    lines.push('Predicates:');
    const predicates = Array.from(impact.predicates).sort();
    const displayPredicates = predicates.slice(0, maxItems);

    for (const predicate of displayPredicates) {
      lines.push(`  - ${predicate}`);
    }

    if (predicates.length > maxItems) {
      lines.push(`  ... and ${predicates.length - maxItems} more`);
    }
    lines.push('');
  }

  // Graphs section
  if (impact.graphs.size > 0) {
    lines.push('Graphs:');
    const graphs = Array.from(impact.graphs).sort();
    const displayGraphs = graphs.slice(0, maxItems);

    for (const graph of displayGraphs) {
      lines.push(`  - ${graph}`);
    }

    if (graphs.length > maxItems) {
      lines.push(`  ... and ${graphs.length - maxItems} more`);
    }
    lines.push('');
  }

  return lines.join('\n');
}
