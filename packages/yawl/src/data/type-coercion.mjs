/**
 * @file Type Coercion Utilities
 * @module @unrdf/yawl/data/type-coercion
 * @description Safe type transformations for YAWL data handling.
 * Returns null on failure instead of throwing, enabling graceful degradation.
 */

import { z } from 'zod';

// =============================================================================
// Numeric Coercions
// =============================================================================

/**
 * Coerce string to number
 * @param {string} value - String value
 * @returns {number|null} Number or null if coercion fails
 * @example
 * stringToNumber('42') // => 42
 * stringToNumber('3.14') // => 3.14
 * stringToNumber('invalid') // => null
 */
export function stringToNumber(value) {
  if (typeof value !== 'string') {
    return null;
  }

  const num = Number(value);
  return isNaN(num) ? null : num;
}

/**
 * Coerce number to string
 * @param {number} value - Numeric value
 * @returns {string|null} String or null if not a number
 * @example
 * numberToString(42) // => '42'
 * numberToString(3.14) // => '3.14'
 */
export function numberToString(value) {
  if (typeof value !== 'number' || isNaN(value)) {
    return null;
  }

  return String(value);
}

/**
 * Coerce value to integer
 * @param {any} value - Value to coerce
 * @returns {number|null} Integer or null if coercion fails
 * @example
 * toInteger('42') // => 42
 * toInteger(3.14) // => 3
 * toInteger('invalid') // => null
 */
export function toInteger(value) {
  const num = typeof value === 'string' ? stringToNumber(value) : value;
  if (typeof num !== 'number' || isNaN(num)) {
    return null;
  }

  return Math.floor(num);
}

/**
 * Coerce value to float
 * @param {any} value - Value to coerce
 * @returns {number|null} Float or null if coercion fails
 * @example
 * toFloat('3.14') // => 3.14
 * toFloat('42') // => 42.0
 */
export function toFloat(value) {
  const num = typeof value === 'string' ? stringToNumber(value) : value;
  if (typeof num !== 'number' || isNaN(num)) {
    return null;
  }

  return num;
}

// =============================================================================
// String Coercions
// =============================================================================

/**
 * Coerce value to string
 * @param {any} value - Value to coerce
 * @returns {string|null} String representation or null for null/undefined
 * @example
 * toString(42) // => '42'
 * toString({ a: 1 }) // => '[object Object]'
 * toString(null) // => null
 */
export function toString(value) {
  if (value === null || value === undefined) {
    return null;
  }

  return String(value);
}

/**
 * Coerce value to JSON string
 * @param {any} value - Value to serialize
 * @returns {string|null} JSON string or null if serialization fails
 * @example
 * toJSON({ a: 1, b: 2 }) // => '{"a":1,"b":2}'
 */
export function toJSON(value) {
  try {
    return JSON.stringify(value);
  } catch (error) {
    return null;
  }
}

/**
 * Parse JSON string
 * @param {string} value - JSON string
 * @returns {any|null} Parsed value or null if parsing fails
 * @example
 * fromJSON('{"a":1}') // => { a: 1 }
 * fromJSON('invalid') // => null
 */
export function fromJSON(value) {
  if (typeof value !== 'string') {
    return null;
  }

  try {
    return JSON.parse(value);
  } catch (error) {
    return null;
  }
}

// =============================================================================
// Date Coercions
// =============================================================================

/**
 * Coerce value to Date
 * @param {any} value - Value to coerce (string, number, Date)
 * @returns {Date|null} Date object or null if coercion fails
 * @example
 * toDate('2026-01-11') // => Date object
 * toDate(1736640000000) // => Date object
 * toDate('invalid') // => null
 */
export function toDate(value) {
  if (value instanceof Date) {
    return isNaN(value.getTime()) ? null : value;
  }

  const date = new Date(value);
  return isNaN(date.getTime()) ? null : date;
}

/**
 * Coerce Date to ISO string
 * @param {Date|string|number} value - Date value
 * @returns {string|null} ISO string or null if coercion fails
 * @example
 * dateToString(new Date('2026-01-11')) // => '2026-01-11T00:00:00.000Z'
 */
export function dateToString(value) {
  const date = toDate(value);
  if (!date) {
    return null;
  }

  return date.toISOString();
}

/**
 * Coerce string to Date
 * @param {string} value - Date string
 * @returns {Date|null} Date object or null if parsing fails
 * @example
 * stringToDate('2026-01-11') // => Date object
 */
export function stringToDate(value) {
  return toDate(value);
}

// =============================================================================
// Boolean Coercions
// =============================================================================

/**
 * Coerce value to boolean
 * @param {any} value - Value to coerce
 * @returns {boolean} Boolean value
 * @example
 * toBoolean('true') // => true
 * toBoolean('false') // => false
 * toBoolean(1) // => true
 * toBoolean(0) // => false
 */
export function toBoolean(value) {
  if (typeof value === 'boolean') {
    return value;
  }

  if (typeof value === 'string') {
    const lower = value.toLowerCase().trim();
    if (lower === 'true' || lower === '1' || lower === 'yes') {
      return true;
    }
    if (lower === 'false' || lower === '0' || lower === 'no') {
      return false;
    }
  }

  return Boolean(value);
}

// =============================================================================
// Array/Scalar Conversions
// =============================================================================

/**
 * Ensure value is an array
 * @param {any} value - Value to convert
 * @returns {Array} Array (wraps scalar, returns array as-is)
 * @example
 * toArray(42) // => [42]
 * toArray([1, 2, 3]) // => [1, 2, 3]
 * toArray(null) // => []
 */
export function toArray(value) {
  if (value === null || value === undefined) {
    return [];
  }

  return Array.isArray(value) ? value : [value];
}

/**
 * Extract scalar from array (first element) or return value
 * @param {any} value - Value to extract
 * @returns {any} First element if array, otherwise value itself
 * @example
 * toScalar([42]) // => 42
 * toScalar([1, 2, 3]) // => 1
 * toScalar(42) // => 42
 */
export function toScalar(value) {
  if (Array.isArray(value)) {
    return value.length > 0 ? value[0] : null;
  }

  return value;
}

// =============================================================================
// Unified Coercion
// =============================================================================

/**
 * Coerce value to target type
 * @param {any} value - Value to coerce
 * @param {string} targetType - Target type (number|string|boolean|date|array|object|json)
 * @returns {any} Coerced value or null if coercion fails
 * @example
 * coerce('42', 'number') // => 42
 * coerce(42, 'string') // => '42'
 */
export function coerce(value, targetType) {
  switch (targetType) {
    case 'number':
      return toFloat(value);
    case 'integer':
      return toInteger(value);
    case 'string':
      return toString(value);
    case 'boolean':
      return toBoolean(value);
    case 'date':
      return toDate(value);
    case 'array':
      return toArray(value);
    case 'scalar':
      return toScalar(value);
    case 'json':
      return toJSON(value);
    default:
      return value;
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  stringToNumber,
  numberToString,
  toInteger,
  toFloat,
  toString,
  toJSON,
  fromJSON,
  toDate,
  dateToString,
  stringToDate,
  toBoolean,
  toArray,
  toScalar,
  coerce,
};
