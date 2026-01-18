/**
 * @file index.mjs
 * @description Agent 4: Impact Set Computation
 * Public API for computing impact sets and cardinality analysis.
 */

export { computeImpactSet, summarizeImpactSet, hashImpactSet } from './impact-set.mjs'
export { groupByPredicate, groupBySubject, analyzeGraphStructure } from './cardinality.mjs'
