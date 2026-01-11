/**
 * Impact Set Extraction - Public API
 *
 * Provides tools for analyzing the impact of RDF diffs (capsules):
 * - Compute which resources, properties, and graphs are affected
 * - Merge multiple impact sets for batch analysis
 * - Find intersections for conflict detection
 * - Format impact sets for display or serialization
 *
 * @module @autonomic-innovation/impact
 */

export {
  computeImpactSet,
  mergeImpactSets,
  impactSetIntersection
} from './impact.mjs';

export {
  formatImpactSummary,
  impactSetToJSON,
  impactSetFromJSON
} from './format.mjs';

/**
 * @typedef {import('./impact.mjs').ImpactSet} ImpactSet
 * @typedef {import('./impact.mjs').Capsule} Capsule
 * @typedef {import('./impact.mjs').Quad} Quad
 * @typedef {import('./format.mjs').ImpactSetJSON} ImpactSetJSON
 */
