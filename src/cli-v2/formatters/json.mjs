/**
 * @file JSON Formatter
 * @module cli-v2/formatters/json
 */

/**
 * Format output as JSON
 * @param {*} data - Data to format
 * @param {Object} options - Formatting options
 * @returns {string} Formatted JSON
 */
export function formatJson(data, options = {}) {
  const indent = options.compact ? 0 : 2;
  return JSON.stringify(data, null, indent);
}
