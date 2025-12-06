/**
 * KGEN Text Filters - P0 Essential Text Processing
 *
 * The 20% of text filters that provide 80% of value
 * Deterministic implementations with comprehensive error handling
 */

/**
 * Convert string to uppercase
 * @param {any} str - Input value to convert
 * @returns {string} Uppercase string
 */
export const upper = (str) => {
  if (str === null || str === undefined) return '';
  return String(str).toUpperCase();
};

/**
 * Convert string to lowercase
 * @param {any} str - Input value to convert
 * @returns {string} Lowercase string
 */
export const lower = (str) => {
  if (str === null || str === undefined) return '';
  return String(str).toLowerCase();
};

/**
 * Trim whitespace from string
 * @param {any} str - Input value to trim
 * @returns {string} Trimmed string
 */
export const trim = (str) => {
  if (str === null || str === undefined) return '';
  return String(str).trim();
};

/**
 * Replace all occurrences of find with replace
 * @param {any} str - Input string
 * @param {string} find - String to find
 * @param {string} replace - Replacement string
 * @returns {string} String with replacements
 */
export const replace = (str, find = '', replace = '') => {
  if (str === null || str === undefined) return '';
  if (find === null || find === undefined) return String(str);
  return String(str).replace(new RegExp(String(find), 'g'), String(replace));
};

/**
 * Replace first occurrence of find with replace
 * @param {any} str - Input string
 * @param {string} find - String to find
 * @param {string} replace - Replacement string
 * @returns {string} String with first replacement
 */
export const replaceFirst = (str, find = '', replace = '') => {
  if (str === null || str === undefined) return '';
  if (find === null || find === undefined) return String(str);
  return String(str).replace(String(find), String(replace));
};

/**
 * Truncate string to specified length
 * @param {any} str - Input string
 * @param {number} length - Maximum length
 * @param {string} suffix - Suffix to add if truncated
 * @returns {string} Truncated string
 */
export const truncate = (str, length = 100, suffix = '...') => {
  if (str === null || str === undefined) return '';
  const text = String(str);
  if (text.length <= length) return text;
  return text.substring(0, length) + suffix;
};

/**
 * Create URL-friendly slug from string
 * @param {any} str - Input string
 * @returns {string} URL-safe slug
 */
export const slug = (str) => {
  if (str === null || str === undefined) return '';
  return String(str)
    .toLowerCase()
    .replace(/[^\w\s-]/g, '') // Remove special chars except word chars, spaces, hyphens
    .replace(/[-\s]+/g, '-')  // Replace spaces and multiple hyphens with single hyphen
    .replace(/^-+|-+$/g, ''); // Remove leading/trailing hyphens
};

/**
 * Convert string to title case
 * @param {any} str - Input string
 * @returns {string} Title case string
 */
export const title = (str) => {
  if (str === null || str === undefined) return '';
  return String(str)
    .toLowerCase()
    .replace(/\b\w/g, char => char.toUpperCase());
};

/**
 * Capitalize first letter
 * @param {any} str - Input string
 * @returns {string} Capitalized string
 */
export const capitalize = (str) => {
  if (str === null || str === undefined) return '';
  const text = String(str);
  return text.charAt(0).toUpperCase() + text.slice(1);
};

/**
 * Convert to camelCase
 * @param {any} str - Input string
 * @returns {string} camelCase string
 */
export const camelCase = (str) => {
  if (str === null || str === undefined) return '';
  return String(str)
    .replace(/[-_\s]+(.)?/g, (_, char) => char ? char.toUpperCase() : '');
};

/**
 * Convert to PascalCase
 * @param {any} str - Input string
 * @returns {string} PascalCase string
 */
export const pascalCase = (str) => {
  if (str === null || str === undefined) return '';
  const camel = camelCase(str);
  return camel.charAt(0).toUpperCase() + camel.slice(1);
};

/**
 * Convert to kebab-case
 * @param {any} str - Input string
 * @returns {string} kebab-case string
 */
export const kebabCase = (str) => {
  if (str === null || str === undefined) return '';
  return String(str)
    .replace(/([a-z])([A-Z])/g, '$1-$2')
    .replace(/[\s_]+/g, '-')
    .toLowerCase();
};

/**
 * Convert to snake_case
 * @param {any} str - Input string
 * @returns {string} snake_case string
 */
export const snakeCase = (str) => {
  if (str === null || str === undefined) return '';
  return String(str)
    .replace(/([a-z])([A-Z])/g, '$1_$2')
    .replace(/[\s-]+/g, '_')
    .toLowerCase();
};

/**
 * Convert to CONSTANT_CASE
 * @param {any} str - Input string
 * @returns {string} CONSTANT_CASE string
 */
export const constantCase = (str) => {
  return snakeCase(str).toUpperCase();
};

/**
 * Escape HTML characters
 * @param {any} str - Input string
 * @returns {string} HTML-escaped string
 */
export const escape = (str) => {
  if (str === null || str === undefined) return '';
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#x27;');
};

/**
 * Reverse string
 * @param {any} str - Input string
 * @returns {string} Reversed string
 */
export const reverse = (str) => {
  if (str === null || str === undefined) return '';
  return String(str).split('').reverse().join('');
};

/**
 * Count words in string
 * @param {any} str - Input string
 * @returns {number} Word count
 */
export const wordCount = (str) => {
  if (str === null || str === undefined) return 0;
  return String(str).trim().split(/\s+/).filter(word => word.length > 0).length;
};

/**
 * Get length of string
 * @param {any} str - Input string
 * @returns {number} String length
 */
export const length = (str) => {
  if (str === null || str === undefined) return 0;
  return String(str).length;
};

/**
 * Pad string to specified length on the left
 * @param {any} str - Input string
 * @param {number} targetLength - Target length
 * @param {string} padString - String to pad with
 * @returns {string} Left-padded string
 */
export const padStart = (str, targetLength, padString = ' ') => {
  if (str === null || str === undefined) return '';
  return String(str).padStart(targetLength, padString);
};

/**
 * Pad string to specified length on the right
 * @param {any} str - Input string
 * @param {number} targetLength - Target length
 * @param {string} padString - String to pad with
 * @returns {string} Right-padded string
 */
export const padEnd = (str, targetLength, padString = ' ') => {
  if (str === null || str === undefined) return '';
  return String(str).padEnd(targetLength, padString);
};

/**
 * Indent all lines with specified number of spaces
 * @param {any} str - Input string
 * @param {number} spaces - Number of spaces to indent
 * @returns {string} Indented string
 */
export const indent = (str, spaces = 2) => {
  if (str === null || str === undefined) return '';
  const indentation = ' '.repeat(Math.max(0, spaces));
  return String(str)
    .split('\n')
    .map(line => indentation + line)
    .join('\n');
};

/**
 * Add quotes around string with specified style
 * @param {any} str - Input string
 * @param {string} style - Quote style: 'single', 'double', 'backtick'
 * @returns {string} Quoted string
 */
export const quote = (str, style = 'double') => {
  if (str === null || str === undefined) return '';
  const text = String(str);

  switch (style) {
    case 'single':
      return `'${text.replace(/'/g, "\\'")}'`;
    case 'double':
      return `"${text.replace(/"/g, '\\"')}"`;
    case 'backtick':
      return `\`${text.replace(/`/g, '\\`')}\``;
    default:
      return `"${text.replace(/"/g, '\\"')}"`;
  }
};

/**
 * Remove surrounding quotes from string
 * @param {any} str - Input string
 * @returns {string} Unquoted string
 */
export const unquote = (str) => {
  if (str === null || str === undefined) return '';
  const text = String(str).trim();

  // Remove matching quotes
  if ((text.startsWith('"') && text.endsWith('"')) ||
      (text.startsWith("'") && text.endsWith("'")) ||
      (text.startsWith('`') && text.endsWith('`'))) {
    return text.slice(1, -1);
  }

  return text;
};

/**
 * Wrap text to specified width
 * @param {any} str - Input string
 * @param {number} width - Maximum line width
 * @param {string} breakChar - Character to break on
 * @returns {string} Wrapped text
 */
export const wrap = (str, width = 80, breakChar = ' ') => {
  if (str === null || str === undefined) return '';
  const text = String(str);

  if (width <= 0) return text;

  const words = text.split(breakChar);
  const lines = [];
  let currentLine = '';

  for (const word of words) {
    if (currentLine.length + word.length + 1 <= width) {
      currentLine += (currentLine ? breakChar : '') + word;
    } else {
      if (currentLine) lines.push(currentLine);
      currentLine = word;
    }
  }

  if (currentLine) lines.push(currentLine);
  return lines.join('\n');
};

/**
 * Extract file extension from path
 * @param {any} path - File path
 * @returns {string} File extension with dot
 */
export const extname = (path) => {
  if (path === null || path === undefined) return '';
  const pathStr = String(path);
  const filename = pathStr.split('/').pop().split('\\').pop();
  const lastDot = filename.lastIndexOf('.');
  return lastDot > 0 ? filename.substring(lastDot) : '';
};

// Collection of all text filters for easy import
export const textFilters = {
  upper,
  lower,
  trim,
  replace,
  replaceFirst,
  truncate,
  slug,
  title,
  capitalize,
  camelCase,
  pascalCase,
  kebabCase,
  snakeCase,
  constantCase,
  escape,
  reverse,
  wordCount,
  length,
  padStart,
  padEnd,
  indent,
  quote,
  unquote,
  wrap,
  extname
};

export default textFilters;