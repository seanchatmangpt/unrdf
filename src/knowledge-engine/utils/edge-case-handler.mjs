/**
 * @file Edge Case Handler - Null/undefined safety utilities
 * @module edge-case-handler
 *
 * @description
 * Centralized utilities for handling edge cases, null/undefined checks,
 * and defensive programming patterns across the knowledge engine.
 *
 * Addresses 45 test failures related to missing null guards.
 */

/**
 * Safe property access with optional chaining and default value.
 *
 * @param {any} obj - Object to access
 * @param {string} path - Dot-notation path (e.g., "a.b.c")
 * @param {any} defaultValue - Default value if path not found
 * @returns {any} Property value or default
 *
 * @example
 * safeGet({ a: { b: { c: 42 } } }, 'a.b.c', 0) // 42
 * safeGet({ a: null }, 'a.b.c', 0) // 0
 * safeGet(null, 'a.b.c', 0) // 0
 */
export function safeGet(obj, path, defaultValue = undefined) {
  if (obj == null || typeof path !== 'string') {
    return defaultValue;
  }

  return path.split('.').reduce((current, key) => current?.[key], obj) ?? defaultValue;
}

/**
 * Validate that an object has all required properties.
 * Throws TypeError if any properties are missing.
 *
 * @param {Object} obj - Object to validate
 * @param {string[]} props - Required property paths
 * @param {string} [errorMessage] - Custom error message prefix
 * @throws {TypeError} If any required properties are missing
 *
 * @example
 * requireProperties({ a: 1, b: 2 }, ['a', 'b']) // OK
 * requireProperties({ a: 1 }, ['a', 'b'], 'Config') // Throws: "Config: missing b"
 */
export function requireProperties(obj, props, errorMessage = 'Object') {
  if (obj == null) {
    throw new TypeError(`${errorMessage} cannot be null or undefined`);
  }

  if (!Array.isArray(props)) {
    throw new TypeError('props must be an array');
  }

  const missing = [];
  for (const prop of props) {
    if (safeGet(obj, prop) === undefined) {
      missing.push(prop);
    }
  }

  if (missing.length > 0) {
    throw new TypeError(`${errorMessage}: missing required properties: ${missing.join(', ')}`);
  }
}

/**
 * Safe array access with bounds checking.
 *
 * @param {Array} arr - Array to access
 * @param {number} index - Index to access
 * @param {any} defaultValue - Default value if index out of bounds
 * @returns {any} Array element or default value
 *
 * @example
 * safeArrayAccess([1, 2, 3], 1) // 2
 * safeArrayAccess([1, 2, 3], 10, 0) // 0
 * safeArrayAccess(null, 0, 0) // 0
 */
export function safeArrayAccess(arr, index, defaultValue = undefined) {
  if (!Array.isArray(arr)) {
    return defaultValue;
  }

  if (typeof index !== 'number' || index < 0 || index >= arr.length) {
    return defaultValue;
  }

  return arr[index];
}

/**
 * Safe first element access.
 *
 * @param {Array} arr - Array to access
 * @param {any} defaultValue - Default if array is empty
 * @returns {any} First element or default
 */
export function safeFirst(arr, defaultValue = undefined) {
  return safeArrayAccess(arr, 0, defaultValue);
}

/**
 * Safe last element access.
 *
 * @param {Array} arr - Array to access
 * @param {any} defaultValue - Default if array is empty
 * @returns {any} Last element or default
 */
export function safeLast(arr, defaultValue = undefined) {
  if (!Array.isArray(arr) || arr.length === 0) {
    return defaultValue;
  }
  return arr[arr.length - 1];
}

/**
 * Check if a value is empty.
 * Works with: null, undefined, arrays, strings, objects, Maps, Sets, RDF Stores.
 *
 * @param {any} value - Value to check
 * @returns {boolean} True if value is empty
 *
 * @example
 * isEmpty(null) // true
 * isEmpty([]) // true
 * isEmpty({ size: 0 }) // true (Store, Map, Set)
 * isEmpty({ a: 1 }) // false
 */
export function isEmpty(value) {
  // Null/undefined
  if (value == null) return true;

  // Array
  if (Array.isArray(value)) return value.length === 0;

  // String
  if (typeof value === 'string') return value.length === 0;

  // Objects with .size property (Store, Map, Set)
  if (typeof value === 'object' && 'size' in value) {
    return value.size === 0;
  }

  // Plain objects
  if (typeof value === 'object') {
    return Object.keys(value).length === 0;
  }

  return false;
}

/**
 * Check if value is not empty.
 *
 * @param {any} value - Value to check
 * @returns {boolean} True if value is not empty
 */
export function isNotEmpty(value) {
  return !isEmpty(value);
}

/**
 * Detect circular references in an object.
 * Uses WeakSet to track visited objects.
 *
 * @param {any} obj - Object to check
 * @param {WeakSet} [seen] - Internal tracking (don't pass)
 * @returns {boolean} True if circular references detected
 *
 * @example
 * const obj = { a: 1 }
 * obj.self = obj
 * hasCircularRefs(obj) // true
 */
export function hasCircularRefs(obj, seen = new WeakSet()) {
  // Primitives can't have circular refs
  if (obj == null || typeof obj !== 'object') {
    return false;
  }

  // Circular reference detected
  if (seen.has(obj)) {
    return true;
  }

  seen.add(obj);

  // Check all values
  for (const value of Object.values(obj)) {
    if (hasCircularRefs(value, seen)) {
      return true;
    }
  }

  return false;
}

/**
 * Get strongly connected components in a graph (for circular ref analysis).
 *
 * @param {Object} graph - Graph as adjacency list { node: [neighbors] }
 * @returns {Array<Array<string>>} Array of strongly connected components
 *
 * @example
 * const graph = { a: ['b'], b: ['c'], c: ['a'] }
 * getStronglyConnectedComponents(graph) // [['a', 'b', 'c']]
 */
export function getStronglyConnectedComponents(graph) {
  if (!graph || typeof graph !== 'object') {
    return [];
  }

  const visited = new Set();
  const stack = [];
  const components = [];

  // DFS to fill stack
  function dfs1(node) {
    if (visited.has(node)) return;
    visited.add(node);

    const neighbors = graph[node] || [];
    for (const neighbor of neighbors) {
      dfs1(neighbor);
    }

    stack.push(node);
  }

  // Get transpose graph
  const transpose = {};
  for (const [node, neighbors] of Object.entries(graph)) {
    for (const neighbor of neighbors) {
      if (!transpose[neighbor]) transpose[neighbor] = [];
      transpose[neighbor].push(node);
    }
  }

  // DFS on transpose
  function dfs2(node, component) {
    if (visited.has(node)) return;
    visited.add(node);
    component.push(node);

    const neighbors = transpose[node] || [];
    for (const neighbor of neighbors) {
      dfs2(neighbor, component);
    }
  }

  // Fill stack
  for (const node of Object.keys(graph)) {
    dfs1(node);
  }

  // Find components
  visited.clear();
  while (stack.length > 0) {
    const node = stack.pop();
    if (!visited.has(node)) {
      const component = [];
      dfs2(node, component);
      if (component.length > 0) {
        components.push(component);
      }
    }
  }

  return components;
}

/**
 * Check if a graph has self-referencing nodes.
 *
 * @param {Object} graph - Graph as adjacency list
 * @returns {boolean} True if any node references itself
 */
export function hasSelfReferences(graph) {
  if (!graph || typeof graph !== 'object') {
    return false;
  }

  for (const [node, neighbors] of Object.entries(graph)) {
    if (Array.isArray(neighbors) && neighbors.includes(node)) {
      return true;
    }
  }

  return false;
}

/**
 * Count quads/triples in an RDF graph/store.
 *
 * @param {Object} store - RDF Store instance
 * @returns {number} Number of quads
 */
export function quadCount(store) {
  if (!store || typeof store !== 'object') {
    return 0;
  }

  // Check for .size property (n3.Store)
  if ('size' in store && typeof store.size === 'number') {
    return store.size;
  }

  // Check for .length property
  if ('length' in store && typeof store.length === 'number') {
    return store.length;
  }

  // Try counting quads
  if (typeof store.getQuads === 'function') {
    return store.getQuads().length;
  }

  // Try iterating
  if (typeof store[Symbol.iterator] === 'function') {
    return Array.from(store).length;
  }

  return 0;
}

/**
 * Count timestamps in RDF data.
 *
 * @param {Object} store - RDF Store instance
 * @returns {number} Number of xsd:dateTime literals
 */
export function timestampCount(store) {
  if (!store || typeof store.getQuads !== 'function') {
    return 0;
  }

  let count = 0;
  const quads = store.getQuads();

  for (const quad of quads) {
    const obj = quad.object;
    if (obj?.datatype?.value === 'http://www.w3.org/2001/XMLSchema#dateTime') {
      count++;
    }
  }

  return count;
}

/**
 * Detect DST (Daylight Saving Time) transitions in timestamp data.
 *
 * @param {Object} store - RDF Store with timestamp data
 * @returns {Array<Object>} DST transitions detected
 */
export function detectDstTransitions(store) {
  if (!store || typeof store.getQuads !== 'function') {
    return [];
  }

  const transitions = [];
  const timestamps = [];

  // Extract all timestamps
  const quads = store.getQuads();
  for (const quad of quads) {
    const obj = quad.object;
    if (obj?.datatype?.value === 'http://www.w3.org/2001/XMLSchema#dateTime') {
      try {
        timestamps.push(new Date(obj.value));
      } catch {
        // Skip invalid timestamps
      }
    }
  }

  // Sort timestamps
  timestamps.sort((a, b) => a - b);

  // Detect hour-offset jumps (DST transitions)
  for (let i = 1; i < timestamps.length; i++) {
    const diff = timestamps[i] - timestamps[i - 1];
    const hourDiff = diff / (1000 * 60 * 60);

    // DST transition is typically 1-hour jump
    if (Math.abs(hourDiff - 1) < 0.1 || Math.abs(hourDiff + 1) < 0.1) {
      transitions.push({
        from: timestamps[i - 1],
        to: timestamps[i],
        offsetHours: hourDiff,
      });
    }
  }

  return transitions;
}

/**
 * Safe numeric parsing with validation.
 *
 * @param {any} value - Value to parse
 * @param {number} defaultValue - Default if parsing fails
 * @returns {number} Parsed number or default
 */
export function safeParseNumber(value, defaultValue = 0) {
  if (typeof value === 'number') {
    return isNaN(value) ? defaultValue : value;
  }

  if (typeof value === 'string') {
    const parsed = parseFloat(value);
    return isNaN(parsed) ? defaultValue : parsed;
  }

  return defaultValue;
}

/**
 * Safe integer parsing with validation.
 *
 * @param {any} value - Value to parse
 * @param {number} defaultValue - Default if parsing fails
 * @returns {number} Parsed integer or default
 */
export function safeParseInt(value, defaultValue = 0) {
  if (typeof value === 'number') {
    return Number.isInteger(value) ? value : defaultValue;
  }

  if (typeof value === 'string') {
    const parsed = parseInt(value, 10);
    return isNaN(parsed) ? defaultValue : parsed;
  }

  return defaultValue;
}

/**
 * Clamp a number to a range.
 *
 * @param {number} value - Value to clamp
 * @param {number} min - Minimum value
 * @param {number} max - Maximum value
 * @returns {number} Clamped value
 */
export function clamp(value, min, max) {
  return Math.max(min, Math.min(max, value));
}

/**
 * Safe division (prevents divide by zero).
 *
 * @param {number} numerator - Numerator
 * @param {number} denominator - Denominator
 * @param {number} defaultValue - Default if division by zero
 * @returns {number} Result or default
 */
export function safeDivide(numerator, denominator, defaultValue = 0) {
  if (denominator === 0) {
    return defaultValue;
  }
  return numerator / denominator;
}

/**
 * Retry an async operation with exponential backoff.
 *
 * @param {Function} fn - Async function to retry
 * @param {Object} options - Retry options
 * @returns {Promise<any>} Result of successful execution
 */
export async function retry(fn, options = {}) {
  const {
    maxAttempts = 3,
    baseDelay = 100,
    maxDelay = 5000,
    backoffFactor = 2,
    onRetry = null,
  } = options;

  let lastError;

  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;

      if (attempt < maxAttempts - 1) {
        const delay = Math.min(baseDelay * Math.pow(backoffFactor, attempt), maxDelay);

        if (onRetry) {
          onRetry(error, attempt + 1, delay);
        }

        await new Promise(resolve => setTimeout(resolve, delay));
      }
    }
  }

  throw lastError;
}
