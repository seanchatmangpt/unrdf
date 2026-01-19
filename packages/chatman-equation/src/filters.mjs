/**
 * @file filters.mjs
 * @module @unrdf/chatman-equation/filters
 * @description Custom Nunjucks filters for Tera-compatible templating
 */

/**
 * Register custom filters on Nunjucks environment
 * @param {nunjucks.Environment} env - Nunjucks environment
 */
export function registerCustomFilters(env) {
  // String filters
  env.addFilter('upper', (str) => String(str).toUpperCase());
  env.addFilter('lower', (str) => String(str).toLowerCase());
  env.addFilter('title', (str) => titleCase(String(str)));
  env.addFilter('capitalize', (str) => capitalize(String(str)));
  env.addFilter('trim', (str) => String(str).trim());
  env.addFilter('truncate', (str, length = 50) => truncate(String(str), length));
  env.addFilter('slugify', (str) => slugify(String(str)));

  // Array filters
  env.addFilter('join', (arr, sep = ', ') => Array.isArray(arr) ? arr.join(sep) : arr);
  env.addFilter('sort', (arr) => Array.isArray(arr) ? [...arr].sort() : arr);
  env.addFilter('reverse', (arr) => Array.isArray(arr) ? [...arr].reverse() : arr);
  env.addFilter('unique', (arr) => Array.isArray(arr) ? [...new Set(arr)] : arr);
  env.addFilter('first', (arr) => Array.isArray(arr) ? arr[0] : arr);
  env.addFilter('last', (arr) => Array.isArray(arr) ? arr[arr.length - 1] : arr);

  // Number filters
  env.addFilter('round', (num, precision = 0) => Number(num).toFixed(precision));
  env.addFilter('abs', (num) => Math.abs(Number(num)));
  env.addFilter('floor', (num) => Math.floor(Number(num)));
  env.addFilter('ceil', (num) => Math.ceil(Number(num)));

  // Date filters
  env.addFilter('date', (dateStr, format = 'iso') => formatDate(dateStr, format));

  // Object filters
  env.addFilter('keys', (obj) => typeof obj === 'object' ? Object.keys(obj) : []);
  env.addFilter('values', (obj) => typeof obj === 'object' ? Object.values(obj) : []);

  // Markdown filters
  env.addFilter('markdown_link', (text, url) => `[${text}](${url})`);
  env.addFilter('code', (text, lang = '') => `\`\`\`${lang}\n${text}\n\`\`\``);
  env.addFilter('inline_code', (text) => `\`${text}\``);
  env.addFilter('bold', (text) => `**${text}**`);
  env.addFilter('italic', (text) => `*${text}*`);

  // Utility filters
  env.addFilter('default', (value, defaultValue) => value || defaultValue);
  env.addFilter('json', (obj, indent = 2) => JSON.stringify(obj, null, indent));
  env.addFilter('yaml', (obj) => toYAML(obj));
}

/**
 * Convert string to title case
 * @param {string} str - Input string
 * @returns {string} Title cased string
 */
function titleCase(str) {
  return str.toLowerCase().replace(/\b\w/g, (c) => c.toUpperCase());
}

/**
 * Capitalize first letter
 * @param {string} str - Input string
 * @returns {string} Capitalized string
 */
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Truncate string to specified length
 * @param {string} str - Input string
 * @param {number} length - Max length
 * @returns {string} Truncated string
 */
function truncate(str, length) {
  if (str.length <= length) return str;
  return str.substring(0, length - 3) + '...';
}

/**
 * Convert string to URL-safe slug
 * @param {string} str - Input string
 * @returns {string} Slugified string
 */
function slugify(str) {
  return str
    .toLowerCase()
    .trim()
    .replace(/[^\w\s-]/g, '')
    .replace(/[\s_-]+/g, '-')
    .replace(/^-+|-+$/g, '');
}

/**
 * Format date string
 * @param {string|Date} dateStr - Date string or object
 * @param {string} format - Output format (iso, short, long)
 * @returns {string} Formatted date
 */
function formatDate(dateStr, format) {
  const date = new Date(dateStr);

  if (isNaN(date.getTime())) {
    return String(dateStr);
  }

  switch (format) {
    case 'iso':
      return date.toISOString();
    case 'short':
      return date.toLocaleDateString();
    case 'long':
      return date.toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'long',
        day: 'numeric'
      });
    default:
      return date.toISOString();
  }
}

/**
 * Convert object to YAML-like string (simple implementation)
 * @param {Object} obj - Object to convert
 * @param {number} indent - Indentation level
 * @returns {string} YAML-like string
 */
function toYAML(obj, indent = 0) {
  const spaces = '  '.repeat(indent);
  const lines = [];

  for (const [key, value] of Object.entries(obj)) {
    if (typeof value === 'object' && !Array.isArray(value)) {
      lines.push(`${spaces}${key}:`);
      lines.push(toYAML(value, indent + 1));
    } else if (Array.isArray(value)) {
      lines.push(`${spaces}${key}:`);
      value.forEach((item) => {
        lines.push(`${spaces}  - ${item}`);
      });
    } else {
      lines.push(`${spaces}${key}: ${value}`);
    }
  }

  return lines.join('\n');
}

export default registerCustomFilters;
