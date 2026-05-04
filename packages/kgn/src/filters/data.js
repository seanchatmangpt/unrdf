/**
 * KGEN Data Filters - P0 Essential Data Processing
 *
 * The 20% of data filters that provide 80% of value
 * Deterministic implementations with comprehensive error handling
 */

/**
 * Convert value to JSON string
 * @param {any} obj - Input value
 * @param {number} indent - Indentation spaces (default: 2)
 * @returns {string} JSON string
 */
export const json = (obj, indent = 2) => {
  try {
    return JSON.stringify(obj, null, indent);
  } catch (error) {
    // Handle circular references and non-serializable values
    try {
      return JSON.stringify(obj, (key, value) => {
        if (typeof value === 'function') return '[Function]';
        if (typeof value === 'symbol') return '[Symbol]';
        if (typeof value === 'undefined') return '[Undefined]';
        if (typeof value === 'bigint') return `[BigInt: ${value.toString()}]`;
        return value;
      }, indent);
    } catch (e) {
      return '{}';
    }
  }
};

/**
 * Parse JSON string to object
 * @param {any} str - JSON string
 * @returns {any} Parsed object or null on error
 */
export const parseJson = (str) => {
  try {
    return JSON.parse(String(str));
  } catch (error) {
    return null;
  }
};

/**
 * Provide default value if input is null/undefined/empty
 * @param {any} value - Input value
 * @param {any} defaultValue - Default value to use
 * @returns {any} Value or default
 */
export const defaultValue = (value, defaultValue = '') => {
  if (value === null || value === undefined) return defaultValue;
  if (typeof value === 'string' && value === '') return defaultValue;
  if (Array.isArray(value) && value.length === 0) return defaultValue;
  if (typeof value === 'object' && Object.keys(value).length === 0) return defaultValue;
  return value;
};

/**
 * Get type of value
 * @param {any} value - Input value
 * @returns {string} Type name
 */
export const typeOf = (value) => {
  if (value === null) return 'null';
  if (Array.isArray(value)) return 'array';
  if (value instanceof Date) return 'date';
  if (value instanceof RegExp) return 'regexp';
  return typeof value;
};

/**
 * Check if value is empty
 * @param {any} value - Input value
 * @returns {boolean} True if empty
 */
export const isEmpty = (value) => {
  if (value === null || value === undefined) return true;
  if (typeof value === 'string') return value === '';
  if (Array.isArray(value)) return value.length === 0;
  if (typeof value === 'object') return Object.keys(value).length === 0;
  if (typeof value === 'number') return isNaN(value);
  return false;
};

/**
 * Check if value is defined (not null or undefined)
 * @param {any} value - Input value
 * @returns {boolean} True if defined
 */
export const isDefined = (value) => {
  return value !== null && value !== undefined;
};

/**
 * Check if value is null
 * @param {any} value - Input value
 * @returns {boolean} True if null
 */
export const isNull = (value) => {
  return value === null;
};

/**
 * Check if value is a number
 * @param {any} value - Input value
 * @returns {boolean} True if number
 */
export const isNumber = (value) => {
  return typeof value === 'number' && !isNaN(value);
};

/**
 * Check if value is a string
 * @param {any} value - Input value
 * @returns {boolean} True if string
 */
export const isString = (value) => {
  return typeof value === 'string';
};

/**
 * Check if value is an array
 * @param {any} value - Input value
 * @returns {boolean} True if array
 */
export const isArray = (value) => {
  return Array.isArray(value);
};

/**
 * Check if value is an object (excluding arrays and null)
 * @param {any} value - Input value
 * @returns {boolean} True if object
 */
export const isObject = (value) => {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
};

/**
 * Check if value is boolean
 * @param {any} value - Input value
 * @returns {boolean} True if boolean
 */
export const isBoolean = (value) => {
  return typeof value === 'boolean';
};

/**
 * Convert value to string
 * @param {any} value - Input value
 * @returns {string} String representation
 */
export const toString = (value) => {
  if (value === null || value === undefined) return '';
  return String(value);
};

/**
 * Convert value to number
 * @param {any} value - Input value
 * @param {number} defaultValue - Default if conversion fails
 * @returns {number} Number value
 */
export const toNumber = (value, defaultValue = 0) => {
  if (value === null || value === undefined) return defaultValue;
  const num = Number(value);
  return isNaN(num) ? defaultValue : num;
};

/**
 * Convert value to integer
 * @param {any} value - Input value
 * @param {number} defaultValue - Default if conversion fails
 * @returns {number} Integer value
 */
export const toInt = (value, defaultValue = 0) => {
  if (value === null || value === undefined) return defaultValue;
  const num = parseInt(value, 10);
  return isNaN(num) ? defaultValue : num;
};

/**
 * Convert value to float
 * @param {any} value - Input value
 * @param {number} defaultValue - Default if conversion fails
 * @returns {number} Float value
 */
export const toFloat = (value, defaultValue = 0.0) => {
  if (value === null || value === undefined) return defaultValue;
  const num = parseFloat(value);
  return isNaN(num) ? defaultValue : num;
};

/**
 * Convert value to boolean
 * @param {any} value - Input value
 * @returns {boolean} Boolean value
 */
export const toBoolean = (value) => {
  if (value === null || value === undefined) return false;
  if (typeof value === 'boolean') return value;
  if (typeof value === 'string') {
    const lower = value.toLowerCase().trim();
    return lower === 'true' || lower === 'yes' || lower === '1';
  }
  if (typeof value === 'number') return value !== 0;
  if (Array.isArray(value)) return value.length > 0;
  if (typeof value === 'object') return Object.keys(value).length > 0;
  return Boolean(value);
};

/**
 * Deep clone an object (deterministic)
 * @param {any} obj - Object to clone
 * @returns {any} Cloned object
 */
export const clone = (obj) => {
  try {
    return JSON.parse(JSON.stringify(obj));
  } catch (error) {
    // Fallback for objects with circular references or non-serializable values
    if (obj === null || typeof obj !== 'object') return obj;
    if (Array.isArray(obj)) return [...obj];
    return { ...obj };
  }
};

/**
 * Get object keys
 * @param {any} obj - Input object
 * @returns {Array} Array of keys
 */
export const keys = (obj) => {
  if (obj === null || obj === undefined) return [];
  if (typeof obj !== 'object') return [];
  return Object.keys(obj);
};

/**
 * Get object values
 * @param {any} obj - Input object
 * @returns {Array} Array of values
 */
export const values = (obj) => {
  if (obj === null || obj === undefined) return [];
  if (typeof obj !== 'object') return [];
  return Object.values(obj);
};

/**
 * Get object entries as [key, value] pairs
 * @param {any} obj - Input object
 * @returns {Array} Array of [key, value] pairs
 */
export const entries = (obj) => {
  if (obj === null || obj === undefined) return [];
  if (typeof obj !== 'object') return [];
  return Object.entries(obj);
};

/**
 * Get value at object path
 * @param {any} obj - Input object
 * @param {string} path - Dot-separated path (e.g., 'user.profile.name')
 * @param {any} defaultValue - Default value if path doesn't exist
 * @returns {any} Value at path or default
 */
export const get = (obj, path, defaultValue = null) => {
  if (obj === null || obj === undefined) return defaultValue;
  if (!path) return obj;

  const keys = String(path).split('.');
  let current = obj;

  for (const key of keys) {
    if (current === null || current === undefined || !(key in current)) {
      return defaultValue;
    }
    current = current[key];
  }

  return current;
};

/**
 * Check if object has property at path
 * @param {any} obj - Input object
 * @param {string} path - Dot-separated path
 * @returns {boolean} True if path exists
 */
export const has = (obj, path) => {
  if (obj === null || obj === undefined) return false;
  if (!path) return false;

  const keys = String(path).split('.');
  let current = obj;

  for (const key of keys) {
    if (current === null || current === undefined || !(key in current)) {
      return false;
    }
    current = current[key];
  }

  return true;
};

/**
 * Merge objects (shallow merge)
 * @param {any} target - Target object
 * @param {any} source - Source object
 * @returns {Object} Merged object
 */
export const merge = (target, source) => {
  if (!isObject(target)) target = {};
  if (!isObject(source)) source = {};
  return { ...target, ...source };
};

/**
 * Round number to specified precision
 * @param {any} num - Number to round
 * @param {number} precision - Decimal places
 * @returns {number} Rounded number
 */
export const round = (num, precision = 0) => {
  if (typeof num !== 'number' || isNaN(num)) return 0;
  const factor = Math.pow(10, Math.max(0, Math.floor(precision)));
  return Math.round(num * factor) / factor;
};

/**
 * Ceiling of number
 * @param {any} num - Number
 * @returns {number} Ceiling value
 */
export const ceil = (num) => {
  if (typeof num !== 'number' || isNaN(num)) return 0;
  return Math.ceil(num);
};

/**
 * Floor of number
 * @param {any} num - Number
 * @returns {number} Floor value
 */
export const floor = (num) => {
  if (typeof num !== 'number' || isNaN(num)) return 0;
  return Math.floor(num);
};

/**
 * Absolute value of number
 * @param {any} num - Number
 * @returns {number} Absolute value
 */
export const abs = (num) => {
  if (typeof num !== 'number' || isNaN(num)) return 0;
  return Math.abs(num);
};

/**
 * Set value at object path (immutable)
 * @param {any} obj - Input object
 * @param {string} path - Dot-separated path
 * @param {any} value - Value to set
 * @returns {Object} New object with value set
 */
export const set = (obj, path, value) => {
  if (obj === null || obj === undefined) obj = {};
  if (!path) return obj;

  const keys = String(path).split('.');
  const result = clone(obj);
  let current = result;

  for (let i = 0; i < keys.length - 1; i++) {
    const key = keys[i];
    if (!(key in current) || typeof current[key] !== 'object') {
      current[key] = {};
    }
    current = current[key];
  }

  current[keys[keys.length - 1]] = value;
  return result;
};

/**
 * Omit properties from object
 * @param {any} obj - Input object
 * @param {...string} keys - Keys to omit
 * @returns {Object} Object without specified keys
 */
export const omit = (obj, ...keys) => {
  if (!isObject(obj)) return {};
  const result = { ...obj };
  for (const key of keys) {
    delete result[key];
  }
  return result;
};

/**
 * Pick properties from object
 * @param {any} obj - Input object
 * @param {...string} keys - Keys to pick
 * @returns {Object} Object with only specified keys
 */
export const pick = (obj, ...keys) => {
  if (!isObject(obj)) return {};
  const result = {};
  for (const key of keys) {
    if (key in obj) {
      result[key] = obj[key];
    }
  }
  return result;
};

/**
 * Convert object to array of [key, value] pairs for iteration
 * Enables Nunjucks iteration: {% for key, value in obj | items %}
 * @param {any} obj - Input object
 * @returns {Array} Array of [key, value] pairs
 */
export const items = (obj) => {
  if (!isObject(obj)) return [];
  return Object.entries(obj);
};

/**
 * Convert object to key-value pairs (alias for items)
 * @param {any} obj - Input object
 * @returns {Array} Array of [key, value] pairs
 */
export const dictItems = items;

// Collection of all data filters for easy import
export const dataFilters = {
  json,
  parseJson,
  defaultValue,
  default: defaultValue,
  typeOf,
  isEmpty,
  isDefined,
  isNull,
  isNumber,
  isString,
  isArray,
  isObject,
  isBoolean,
  toString,
  toNumber,
  toInt,
  toFloat,
  toBoolean,
  clone,
  keys,
  values,
  entries,
  get,
  has,
  merge,
  round,
  ceil,
  floor,
  abs,
  set,
  omit,
  pick,
  items,
  dictItems
};

export default dataFilters;