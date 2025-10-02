/**
 * @file Output Formatters
 * @module cli-v2/formatters
 */

import { formatJson } from './json.mjs';
import { formatYaml } from './yaml.mjs';
import { formatTable } from './table.mjs';
import { formatTree } from './tree.mjs';

/**
 * Format output based on format type
 * @param {*} data - Data to format
 * @param {string} format - Format type (json, yaml, table, tree)
 * @param {Object} options - Format options
 * @returns {string} Formatted output
 */
export function formatOutput(data, format = 'json', options = {}) {
  switch (format) {
    case 'json':
      return formatJson(data, options);
    case 'yaml':
      return formatYaml(data, options);
    case 'table':
      return formatTable(data, options);
    case 'tree':
      return formatTree(data, options);
    default:
      throw new Error(`Unsupported format: ${format}`);
  }
}

export { formatJson, formatYaml, formatTable, formatTree };
