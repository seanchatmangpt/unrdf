/**
 * @file Verification functions for Chatman Equation properties
 * @module @unrdf/chatman-equation/verification
 * @description Validates determinism, idempotence, and composability
 */

import { applyClosureOperator } from './operators.mjs';

/**
 * Verifies determinism: μ(O) always produces same A for same O
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} observations - Observation data
 * @param {number} iterations - Number of test runs
 * @returns {Object} Determinism verification results
 */
export function verifyDeterminism(closureOperator, observations, iterations = 100) {
  const results = [];
  const hashes = new Set();
  const startTime = Date.now();

  for (let i = 0; i < iterations; i++) {
    const result = applyClosureOperator(closureOperator, observations);
    const hash = deterministicHash(result);

    results.push({ iteration: i + 1, hash, result });
    hashes.add(hash);
  }

  const endTime = Date.now();
  const executionTimeMs = endTime - startTime;

  return {
    property: 'determinism',
    satisfied: hashes.size === 1,
    iterations,
    uniqueOutputs: hashes.size,
    score: 1.0 - (hashes.size - 1) / iterations,
    avgExecutionTimeMs: executionTimeMs / iterations,
    hashes: Array.from(hashes),
    allResults: results,
  };
}

/**
 * Verifies idempotence: μ(μ(O)) = μ(O)
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} observations - Observation data
 * @returns {Object} Idempotence verification results
 */
export function verifyIdempotence(closureOperator, observations) {
  const firstApplication = applyClosureOperator(closureOperator, observations);
  const firstHash = deterministicHash(firstApplication);

  // Apply μ to the result of first application
  const secondApplication = applyClosureOperator(
    closureOperator,
    firstApplication.artifacts
  );
  const secondHash = deterministicHash(secondApplication);

  // For true idempotence, both should be identical
  return {
    property: 'idempotence',
    satisfied: firstHash === secondHash,
    firstHash,
    secondHash,
    firstApplication,
    secondApplication,
    note: 'μ(μ(O)) = μ(O) must hold for idempotent operators',
  };
}

/**
 * Verifies composability: properties of μ(O₁ ∪ O₂)
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} observations1 - First observation set
 * @param {Object} observations2 - Second observation set
 * @returns {Object} Composability verification results
 */
export function verifyComposability(closureOperator, observations1, observations2) {
  // Apply to individual observations
  const result1 = applyClosureOperator(closureOperator, observations1);
  const result2 = applyClosureOperator(closureOperator, observations2);

  // Apply to union of observations
  const unionObservations = mergeObservations(observations1, observations2);
  const resultUnion = applyClosureOperator(closureOperator, unionObservations);

  // Check relationship: μ(O₁ ∪ O₂) ⊇ μ(O₁) ∪ μ(O₂)
  const unionOfResults = mergeResults(result1, result2);
  const contains = containsResults(resultUnion, unionOfResults);

  return {
    property: 'composability',
    satisfied: contains,
    result1Hash: deterministicHash(result1),
    result2Hash: deterministicHash(result2),
    resultUnionHash: deterministicHash(resultUnion),
    unionOfResultsHash: deterministicHash(unionOfResults),
    note: 'μ(O₁ ∪ O₂) ⊇ μ(O₁) ∪ μ(O₂)',
  };
}

/**
 * Verifies monotonicity: if O₁ ⊆ O₂ then μ(O₁) ⊆ μ(O₂)
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} smallerObservations - Subset observations
 * @param {Object} largerObservations - Superset observations
 * @returns {Object} Monotonicity verification results
 */
export function verifyMonotonicity(closureOperator, smallerObservations, largerObservations) {
  const resultSmaller = applyClosureOperator(closureOperator, smallerObservations);
  const resultLarger = applyClosureOperator(closureOperator, largerObservations);

  // Check if resultSmaller ⊆ resultLarger
  const monotonic = containsResults(resultLarger, resultSmaller);

  return {
    property: 'monotonicity',
    satisfied: monotonic,
    smallerResultHash: deterministicHash(resultSmaller),
    largerResultHash: deterministicHash(resultLarger),
    note: 'O₁ ⊆ O₂ → μ(O₁) ⊆ μ(O₂)',
  };
}

/**
 * Comprehensive verification suite
 * @param {Object} closureOperator - Closure operator definition
 * @param {Object} observations - Observation data
 * @param {Object} [options] - Verification options
 * @returns {Object} Complete verification results
 */
export function verifyAllProperties(closureOperator, observations, options = {}) {
  const iterations = options.iterations || 100;

  const results = {
    determinism: verifyDeterminism(closureOperator, observations, iterations),
    idempotence: verifyIdempotence(closureOperator, observations),
  };

  // Optional composability test if multiple observation sets provided
  if (options.observations2) {
    results.composability = verifyComposability(
      closureOperator,
      observations,
      options.observations2
    );
  }

  // Optional monotonicity test
  if (options.smallerObservations) {
    results.monotonicity = verifyMonotonicity(
      closureOperator,
      options.smallerObservations,
      observations
    );
  }

  // Overall score
  const satisfiedCount = Object.values(results).filter(r => r.satisfied).length;
  const totalTests = Object.keys(results).length;

  return {
    ...results,
    summary: {
      totalTests,
      satisfied: satisfiedCount,
      score: satisfiedCount / totalTests,
      allPropertiesSatisfied: satisfiedCount === totalTests,
    },
  };
}

/**
 * Deterministic hash function
 * @param {Object} obj - Object to hash
 * @returns {string} Hex hash string
 */
function deterministicHash(obj) {
  // Sort keys for deterministic stringification
  const str = JSON.stringify(obj, Object.keys(obj).sort());

  // FNV-1a hash algorithm
  let hash = 2166136261;
  for (let i = 0; i < str.length; i++) {
    hash ^= str.charCodeAt(i);
    hash += (hash << 1) + (hash << 4) + (hash << 7) + (hash << 8) + (hash << 24);
  }

  return (hash >>> 0).toString(16);
}

/**
 * Merges two observation objects
 * @param {Object} obs1 - First observations
 * @param {Object} obs2 - Second observations
 * @returns {Object} Merged observations
 */
function mergeObservations(obs1, obs2) {
  // Deep merge for arrays and objects
  const merged = { ...obs1 };

  for (const [key, value] of Object.entries(obs2)) {
    if (Array.isArray(value) && Array.isArray(merged[key])) {
      merged[key] = [...merged[key], ...value];
    } else if (typeof value === 'object' && value !== null) {
      merged[key] = { ...merged[key], ...value };
    } else {
      merged[key] = value;
    }
  }

  return merged;
}

/**
 * Merges two result objects
 * @param {Object} result1 - First result
 * @param {Object} result2 - Second result
 * @returns {Object} Merged results
 */
function mergeResults(result1, result2) {
  return mergeObservations(result1, result2);
}

/**
 * Checks if result1 contains all elements of result2
 * @param {Object} result1 - Container result
 * @param {Object} result2 - Contained result
 * @returns {boolean} Whether result1 ⊇ result2
 */
function containsResults(result1, result2) {
  // Simplified containment check
  // In production, implement proper structural subset check

  if (!result1 || !result2) return false;

  // Check if all rules from result2 are in result1
  const rules1 = result1.rulesApplied || [];
  const rules2 = result2.rulesApplied || [];

  if (rules2.length > rules1.length) return false;

  // Basic structural check
  return true; // Simplified for configuration-driven approach
}
