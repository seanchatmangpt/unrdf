/**
 * JSON formatters
 * @module domain/formatters/json
 */

/**
 * Format data as compact JSON
 * @param {unknown} data - Data to format
 * @returns {string} JSON string
 */
export function jsonFormatter(data) {
  return JSON.stringify(data);
}

/**
 * Format data as pretty-printed JSON
 * @param {unknown} data - Data to format
 * @param {number} [indent=2] - Indentation spaces
 * @returns {string} Formatted JSON string
 */
export function jsonPrettyFormatter(data, indent = 2) {
  return JSON.stringify(data, null, indent);
}

/**
 * Parse JSON string to object
 * @param {string} json - JSON string
 * @returns {unknown} Parsed data
 * @throws {SyntaxError} If JSON is invalid
 */
export function parseJSON(json) {
  return JSON.parse(json);
}

/**
 * Safely parse JSON string
 * @param {string} json - JSON string
 * @param {unknown} [fallback=null] - Fallback value on error
 * @returns {unknown} Parsed data or fallback
 */
export function safeParseJSON(json, fallback = null) {
  try {
    return JSON.parse(json);
  } catch {
    return fallback;
  }
}

/**
 * Check if string is valid JSON
 * @param {string} str - String to check
 * @returns {boolean}
 */
export function isValidJSON(str) {
  try {
    JSON.parse(str);
    return true;
  } catch {
    return false;
  }
}
