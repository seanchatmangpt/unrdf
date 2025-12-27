/**
 * YAML formatter
 * @module domain/formatters/yaml
 */

import YAML from 'yaml';

/**
 * Format data as YAML
 * @param {unknown} data - Data to format
 * @param {Object} [options] - YAML options
 * @param {number} [options.indent=2] - Indentation
 * @param {boolean} [options.sortKeys=false] - Sort object keys
 * @param {number} [options.lineWidth=80] - Line width for wrapping
 * @returns {string} YAML string
 */
export function yamlFormatter(data, options = {}) {
  const { indent = 2, sortKeys = false, lineWidth = 80 } = options;
  return YAML.stringify(data, {
    indent,
    sortMapEntries: sortKeys,
    lineWidth,
  });
}

/**
 * Parse YAML string to object
 * @param {string} yaml - YAML string
 * @returns {unknown} Parsed data
 * @throws {Error} If YAML is invalid
 */
export function parseYAML(yaml) {
  return YAML.parse(yaml);
}

/**
 * Safely parse YAML string
 * @param {string} yaml - YAML string
 * @param {unknown} [fallback=null] - Fallback value on error
 * @returns {unknown} Parsed data or fallback
 */
export function safeParseYAML(yaml, fallback = null) {
  try {
    return YAML.parse(yaml);
  } catch {
    return fallback;
  }
}

/**
 * Check if string is valid YAML
 * @param {string} str - String to check
 * @returns {boolean}
 */
export function isValidYAML(str) {
  try {
    YAML.parse(str);
    return true;
  } catch {
    return false;
  }
}

/**
 * Convert JSON to YAML
 * @param {string} json - JSON string
 * @returns {string} YAML string
 */
export function jsonToYAML(json) {
  const data = JSON.parse(json);
  return yamlFormatter(data);
}

/**
 * Convert YAML to JSON
 * @param {string} yaml - YAML string
 * @param {boolean} [pretty=false] - Pretty print JSON
 * @returns {string} JSON string
 */
export function yamlToJSON(yaml, pretty = false) {
  const data = YAML.parse(yaml);
  return pretty ? JSON.stringify(data, null, 2) : JSON.stringify(data);
}
