/**
 * @file YAML Formatter
 * @module cli-v2/formatters/yaml
 */

/**
 * Format output as YAML
 * @param {*} data - Data to format
 * @param {Object} options - Formatting options
 * @returns {string} Formatted YAML
 */
export function formatYaml(data, options = {}) {
  // Simple YAML formatter (for production, use a library like 'js-yaml')
  return toYaml(data, 0);
}

function toYaml(obj, indent) {
  const spaces = '  '.repeat(indent);

  if (obj === null) return 'null';
  if (typeof obj === 'undefined') return 'undefined';
  if (typeof obj === 'string') return `"${obj}"`;
  if (typeof obj === 'number' || typeof obj === 'boolean') return String(obj);

  if (Array.isArray(obj)) {
    if (obj.length === 0) return '[]';
    return obj.map(item => `${spaces}- ${toYaml(item, indent + 1)}`).join('\n');
  }

  if (typeof obj === 'object') {
    const lines = [];
    for (const [key, value] of Object.entries(obj)) {
      if (typeof value === 'object' && value !== null) {
        lines.push(`${spaces}${key}:`);
        lines.push(toYaml(value, indent + 1));
      } else {
        lines.push(`${spaces}${key}: ${toYaml(value, 0)}`);
      }
    }
    return lines.join('\n');
  }

  return String(obj);
}
