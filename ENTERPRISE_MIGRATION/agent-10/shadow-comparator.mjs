/**
 * Shadow Comparator - Compares legacy vs migrated outputs
 * @module agent-10/shadow-comparator
 */

/**
 * @typedef {Object} ComparisonResult
 * @property {boolean} equivalent - Whether outputs are equivalent
 * @property {string[]} differences - List of differences
 * @property {number} differenceCount - Count of differences
 * @property {Object} diff - Detailed diff report
 */

/**
 * Deep equality check with tolerance for numeric values
 * @param {*} a - First value
 * @param {*} b - Second value
 * @param {number} tolerance - Numeric tolerance (default: 0)
 * @returns {boolean} True if equal within tolerance
 */
function deepEqual(a, b, tolerance = 0) {
  // Strict equality
  if (a === b) return true;

  // Type check
  if (typeof a !== typeof b) return false;

  // Null check
  if (a === null || b === null) return false;

  // Numeric comparison with tolerance
  if (typeof a === 'number' && typeof b === 'number') {
    if (isNaN(a) && isNaN(b)) return true;
    if (isNaN(a) || isNaN(b)) return false;
    return Math.abs(a - b) <= tolerance;
  }

  // Date comparison
  if (a instanceof Date && b instanceof Date) {
    return a.getTime() === b.getTime();
  }

  // Array comparison
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (!deepEqual(a[i], b[i], tolerance)) return false;
    }
    return true;
  }

  // Object comparison
  if (typeof a === 'object' && typeof b === 'object') {
    const keysA = Object.keys(a).sort();
    const keysB = Object.keys(b).sort();

    if (keysA.length !== keysB.length) return false;
    if (!deepEqual(keysA, keysB, 0)) return false;

    for (const key of keysA) {
      if (!deepEqual(a[key], b[key], tolerance)) return false;
    }
    return true;
  }

  return false;
}

/**
 * Generate detailed diff between two values
 * @param {*} legacy - Legacy value
 * @param {*} migrated - Migrated value
 * @param {string} path - Current path in object tree
 * @returns {string[]} List of differences
 */
function generateDiff(legacy, migrated, path = 'root') {
  const diffs = [];

  // Type mismatch
  if (typeof legacy !== typeof migrated) {
    diffs.push(`${path}: type mismatch (${typeof legacy} vs ${typeof migrated})`);
    return diffs;
  }

  // Null/undefined
  if (legacy === null || migrated === null) {
    if (legacy !== migrated) {
      diffs.push(`${path}: null mismatch (${legacy} vs ${migrated})`);
    }
    return diffs;
  }

  // Primitive values
  if (typeof legacy !== 'object') {
    if (legacy !== migrated) {
      diffs.push(`${path}: value mismatch (${legacy} vs ${migrated})`);
    }
    return diffs;
  }

  // Arrays
  if (Array.isArray(legacy) && Array.isArray(migrated)) {
    if (legacy.length !== migrated.length) {
      diffs.push(`${path}: array length mismatch (${legacy.length} vs ${migrated.length})`);
    }

    const maxLen = Math.max(legacy.length, migrated.length);
    for (let i = 0; i < maxLen; i++) {
      if (i >= legacy.length) {
        diffs.push(`${path}[${i}]: missing in legacy`);
      } else if (i >= migrated.length) {
        diffs.push(`${path}[${i}]: missing in migrated`);
      } else {
        const itemDiffs = generateDiff(legacy[i], migrated[i], `${path}[${i}]`);
        diffs.push(...itemDiffs);
      }
    }

    return diffs;
  }

  // Objects
  const legacyKeys = Object.keys(legacy).sort();
  const migratedKeys = Object.keys(migrated).sort();

  const allKeys = new Set([...legacyKeys, ...migratedKeys]);

  for (const key of allKeys) {
    if (!(key in legacy)) {
      diffs.push(`${path}.${key}: missing in legacy`);
    } else if (!(key in migrated)) {
      diffs.push(`${path}.${key}: missing in migrated`);
    } else {
      const keyDiffs = generateDiff(legacy[key], migrated[key], `${path}.${key}`);
      diffs.push(...keyDiffs);
    }
  }

  return diffs;
}

/**
 * Compare legacy vs migrated results
 * @param {Object} scenario - Scenario object
 * @param {Object} result - Scenario result
 * @returns {ComparisonResult} Comparison result
 */
export function compareLegacyVsMigrated(scenario, result) {
  const tolerance = scenario.options?.tolerance || 0;
  const allowTimestampDrift = scenario.options?.allowTimestampDrift || false;

  // If both errored, compare error messages
  if (result.legacyError && result.migratedError) {
    const errorsMatch = result.legacyError.message === result.migratedError.message;

    return {
      equivalent: errorsMatch,
      differences: errorsMatch ? [] : [
        `Error messages differ: "${result.legacyError.message}" vs "${result.migratedError.message}"`
      ],
      differenceCount: errorsMatch ? 0 : 1,
      diff: {
        legacy: result.legacyError.message,
        migrated: result.migratedError.message,
      },
    };
  }

  // If one errored but not the other
  if (result.legacyError || result.migratedError) {
    const differences = [];
    if (result.legacyError) differences.push(`Legacy errored: ${result.legacyError.message}`);
    if (result.migratedError) differences.push(`Migrated errored: ${result.migratedError.message}`);

    return {
      equivalent: false,
      differences,
      differenceCount: differences.length,
      diff: {
        legacy: result.legacyError || result.legacyResult,
        migrated: result.migratedError || result.migratedResult,
      },
    };
  }

  // Both succeeded - deep comparison
  const equivalent = deepEqual(result.legacyResult, result.migratedResult, tolerance);
  const differences = equivalent ? [] : generateDiff(result.legacyResult, result.migratedResult);

  return {
    equivalent,
    differences,
    differenceCount: differences.length,
    diff: {
      legacy: result.legacyResult,
      migrated: result.migratedResult,
    },
  };
}

/**
 * Generate detailed diff report
 * @param {*} legacy - Legacy output
 * @param {*} migrated - Migrated output
 * @returns {Object} Diff report
 */
export function generateDiffReport(legacy, migrated) {
  const differences = generateDiff(legacy, migrated);

  return {
    equivalent: differences.length === 0,
    differenceCount: differences.length,
    differences,
    legacy,
    migrated,
    timestamp: Date.now(),
  };
}

/**
 * Check if two values are equivalent within tolerance
 * @param {*} legacy - Legacy value
 * @param {*} migrated - Migrated value
 * @param {number} tolerance - Numeric tolerance (default: 0)
 * @returns {boolean} True if equivalent
 */
export function isEquivalent(legacy, migrated, tolerance = 0) {
  return deepEqual(legacy, migrated, tolerance);
}

/**
 * Calculate similarity percentage between two objects
 * @param {*} legacy - Legacy value
 * @param {*} migrated - Migrated value
 * @returns {number} Similarity percentage (0-100)
 */
export function calculateSimilarity(legacy, migrated) {
  // Simple heuristic: count matching vs total fields
  const diffs = generateDiff(legacy, migrated);

  // Count total comparable elements
  let totalElements = 0;

  function countElements(obj, path = 'root') {
    if (obj === null || typeof obj !== 'object') {
      totalElements++;
      return;
    }

    if (Array.isArray(obj)) {
      obj.forEach((item, i) => countElements(item, `${path}[${i}]`));
      return;
    }

    for (const key of Object.keys(obj)) {
      countElements(obj[key], `${path}.${key}`);
    }
  }

  countElements(legacy);
  countElements(migrated);

  // Avoid division by zero
  if (totalElements === 0) return 100;

  const matchingElements = totalElements - diffs.length;
  const similarity = (matchingElements / totalElements) * 100;

  return Math.max(0, Math.min(100, similarity));
}

/**
 * Create mismatch report for all scenario results
 * @param {Object[]} results - Array of scenario results
 * @returns {Object} Mismatch report
 */
export function createMismatchReport(results) {
  const mismatches = results.filter(r => !r.resultsMatch);

  return {
    totalScenarios: results.length,
    mismatchCount: mismatches.length,
    mismatchRate: results.length > 0 ? (mismatches.length / results.length) * 100 : 0,
    mismatches: mismatches.map(m => ({
      scenario: m.name,
      differences: m.differences,
      timestamp: m.timestamp,
    })),
    timestamp: Date.now(),
  };
}
