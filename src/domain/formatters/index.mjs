/**
 * Output formatters
 * @module domain/formatters
 */

import { jsonFormatter, jsonPrettyFormatter } from './json.mjs';
import { yamlFormatter } from './yaml.mjs';
import { tableFormatter } from './table.mjs';

export { jsonFormatter, jsonPrettyFormatter, parseJSON, safeParseJSON, isValidJSON } from './json.mjs';
export { yamlFormatter, parseYAML, safeParseYAML, isValidYAML, jsonToYAML, yamlToJSON } from './yaml.mjs';
export { tableFormatter, dataToRows, formatCell, keyValueTable, listTable } from './table.mjs';

/**
 * Format output in different styles
 * @param {unknown} data - Data to format
 * @param {string} format - Format type: json, json-pretty, yaml, table
 * @param {Object} [options] - Format-specific options
 * @returns {string} Formatted output
 * @throws {Error} If format is unknown
 */
export function formatOutput(data, format, options = {}) {
  switch (format) {
    case 'json':
      return jsonFormatter(data);

    case 'json-pretty':
      return jsonPrettyFormatter(data, options.indent);

    case 'yaml':
      return yamlFormatter(data, options);

    case 'table':
      return tableFormatter(data, options);

    default:
      throw new Error(`Unknown output format: ${format}. Valid formats: json, json-pretty, yaml, table`);
  }
}

/**
 * Get formatter function by name
 * @param {string} format - Format name
 * @returns {(data: unknown, options?: Object) => string}
 * @throws {Error} If format is unknown
 */
export function getFormatter(format) {
  switch (format) {
    case 'json':
      return jsonFormatter;

    case 'json-pretty':
      return jsonPrettyFormatter;

    case 'yaml':
      return yamlFormatter;

    case 'table':
      return tableFormatter;

    default:
      throw new Error(`Unknown format: ${format}`);
  }
}

/**
 * Check if format is supported
 * @param {string} format - Format name
 * @returns {boolean}
 */
export function isValidFormat(format) {
  return ['json', 'json-pretty', 'yaml', 'table'].includes(format);
}

/**
 * Auto-detect format from string content
 * @param {string} content - Content string
 * @returns {'json'|'yaml'|'unknown'}
 */
export function detectFormat(content) {
  const trimmed = content.trim();

  // Check for JSON
  if ((trimmed.startsWith('{') && trimmed.endsWith('}')) || (trimmed.startsWith('[') && trimmed.endsWith(']'))) {
    try {
      JSON.parse(trimmed);
      return 'json';
    } catch {
      // Not valid JSON
    }
  }

  // Check for YAML indicators
  if (trimmed.includes(':') && !trimmed.startsWith('{')) {
    return 'yaml';
  }

  return 'unknown';
}
