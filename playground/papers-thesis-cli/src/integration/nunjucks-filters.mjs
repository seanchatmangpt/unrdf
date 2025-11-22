/**
 * @fileoverview Custom Nunjucks filters for LaTeX template rendering
 *
 * @description
 * Provides specialized filters for Nunjucks templates that generate
 * LaTeX output. Handles special character escaping, formatting,
 * and transformation utilities for academic papers and theses.
 *
 * @module integration/nunjucks-filters
 * @version 1.0.0
 * @license MIT
 */

import { z } from 'zod';

/**
 * LaTeX special characters that require escaping
 * Maps each special character to its LaTeX escape sequence
 * @type {Record<string, string>}
 */
export const LATEX_ESCAPE_MAP = Object.freeze({
  '&': '\\&',
  '%': '\\%',
  '$': '\\$',
  '#': '\\#',
  '_': '\\_',
  '{': '\\{',
  '}': '\\}',
  '~': '\\textasciitilde{}',
  '^': '\\textasciicircum{}',
  '\\': '\\textbackslash{}'
});

/**
 * Regex pattern for matching LaTeX special characters
 * @type {RegExp}
 */
const LATEX_SPECIAL_CHARS_REGEX = /[&%$#_{}~^\\]/g;

/**
 * Escape LaTeX special characters in a string
 *
 * @param {string} str - String to escape
 * @returns {string} Escaped string safe for LaTeX
 *
 * @example
 * texescape('100% complete & ready') // '100\\% complete \\& ready'
 * texescape('$42.99') // '\\$42.99'
 * texescape('file_name') // 'file\\_name'
 */
export function texescape(str) {
  if (str === null || str === undefined) return '';
  if (typeof str !== 'string') return String(str);

  return str.replace(LATEX_SPECIAL_CHARS_REGEX, (char) => LATEX_ESCAPE_MAP[char] || char);
}

/**
 * Generate a valid BibTeX key from a title
 *
 * Algorithm:
 * 1. Extract significant words (skip articles/prepositions)
 * 2. CamelCase the first 2-3 words
 * 3. Append current year
 *
 * @param {string} title - Paper/thesis title
 * @returns {string} Valid BibTeX key (e.g., 'MachineLearning2024')
 *
 * @example
 * bibtexkey('Machine Learning for Beginners') // 'MachineLearningBeginners2024'
 * bibtexkey('A Study of the Effects') // 'StudyEffects2024'
 * bibtexkey('Deep Learning') // 'DeepLearning2024'
 */
export function bibtexkey(title) {
  if (!title || typeof title !== 'string') {
    return `unknown${new Date().getFullYear()}`;
  }

  // Words to skip (articles, prepositions, conjunctions)
  const stopWords = new Set([
    'a', 'an', 'the', 'of', 'for', 'in', 'on', 'at', 'to', 'and', 'or', 'but',
    'with', 'by', 'from', 'as', 'into', 'through', 'during', 'before', 'after',
    'above', 'below', 'between', 'under', 'again', 'further', 'then', 'once'
  ]);

  // Extract words, filter stop words, take first 3 significant words
  const words = title
    .split(/\s+/)
    .map((word) => word.replace(/[^a-zA-Z0-9]/g, ''))
    .filter((word) => word.length > 0 && !stopWords.has(word.toLowerCase()))
    .slice(0, 3);

  // CamelCase the words
  const camelCased = words
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
    .join('');

  // Append current year
  const year = new Date().getFullYear();

  return camelCased ? `${camelCased}${year}` : `unknown${year}`;
}

/**
 * Join array items for LaTeX output with optional separator
 *
 * @param {Array<string>} items - Array of items to join
 * @param {string} [sep=' '] - Separator string (default: single space)
 * @returns {string} Joined string with escaped items
 *
 * @example
 * latexjoin(['Alice', 'Bob', 'Carol'], ', ') // 'Alice, Bob, Carol'
 * latexjoin(['100%', '$50'], ' and ') // '100\\% and \\$50'
 */
export function latexjoin(items, sep = ' ') {
  if (!Array.isArray(items)) {
    return items === null || items === undefined ? '' : String(items);
  }

  return items.map((item) => texescape(String(item))).join(sep);
}

/**
 * Format options for date formatting
 * @typedef {'long' | 'short' | 'iso' | 'year' | 'month-year'} DateFormatOption
 */

/**
 * Format date for LaTeX output
 *
 * @param {string | Date} date - Date to format
 * @param {DateFormatOption} [format='long'] - Format option
 * @returns {string} Formatted date string
 *
 * @example
 * formatdate(new Date('2024-03-15'), 'long') // 'March 15, 2024'
 * formatdate('2024-03-15', 'short') // 'Mar 15, 2024'
 * formatdate('2024-03-15', 'iso') // '2024-03-15'
 * formatdate('2024-03-15', 'year') // '2024'
 * formatdate('2024-03-15', 'month-year') // 'March 2024'
 */
export function formatdate(date, format = 'long') {
  if (!date) return '';

  const d = typeof date === 'string' ? new Date(date) : date;

  if (!(d instanceof Date) || isNaN(d.getTime())) {
    return String(date);
  }

  switch (format) {
    case 'long':
      return d.toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'long',
        day: 'numeric'
      });

    case 'short':
      return d.toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric'
      });

    case 'iso':
      return d.toISOString().split('T')[0];

    case 'year':
      return String(d.getFullYear());

    case 'month-year':
      return d.toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'long'
      });

    default:
      return d.toLocaleDateString('en-US');
  }
}

/**
 * Convert string to uppercase
 *
 * @param {string} str - String to convert
 * @returns {string} Uppercase string
 *
 * @example
 * uppercase('hello world') // 'HELLO WORLD'
 */
export function uppercase(str) {
  if (str === null || str === undefined) return '';
  return String(str).toUpperCase();
}

/**
 * Convert string to lowercase
 *
 * @param {string} str - String to convert
 * @returns {string} Lowercase string
 *
 * @example
 * lowercase('HELLO WORLD') // 'hello world'
 */
export function lowercase(str) {
  if (str === null || str === undefined) return '';
  return String(str).toLowerCase();
}

/**
 * Convert string to URL-friendly slug
 *
 * @param {string} str - String to slugify
 * @returns {string} Slug format string
 *
 * @example
 * slugify('Hello World!') // 'hello-world'
 * slugify('Machine Learning & AI') // 'machine-learning-ai'
 */
export function slugify(str) {
  if (str === null || str === undefined) return '';

  return String(str)
    .toLowerCase()
    .trim()
    .replace(/[^\w\s-]/g, '') // Remove non-word chars
    .replace(/[\s_-]+/g, '-') // Replace spaces/underscores with hyphens
    .replace(/^-+|-+$/g, ''); // Remove leading/trailing hyphens
}

/**
 * Capitalize first letter of string
 *
 * @param {string} str - String to capitalize
 * @returns {string} String with first letter capitalized
 *
 * @example
 * capitalize('hello world') // 'Hello world'
 */
export function capitalize(str) {
  if (str === null || str === undefined) return '';
  const s = String(str);
  return s.charAt(0).toUpperCase() + s.slice(1);
}

/**
 * Convert string to title case
 *
 * @param {string} str - String to convert
 * @returns {string} Title case string
 *
 * @example
 * titlecase('hello world') // 'Hello World'
 */
export function titlecase(str) {
  if (str === null || str === undefined) return '';

  return String(str)
    .toLowerCase()
    .split(' ')
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ');
}

/**
 * Wrap text at specified column width
 *
 * @param {string} str - Text to wrap
 * @param {number} [width=80] - Maximum line width
 * @returns {string} Wrapped text with newlines
 *
 * @example
 * wraptext('A very long paragraph...', 40) // Multi-line output
 */
export function wraptext(str, width = 80) {
  if (str === null || str === undefined) return '';
  const text = String(str);

  const words = text.split(/\s+/);
  const lines = [];
  let currentLine = '';

  for (const word of words) {
    if (currentLine.length + word.length + 1 <= width) {
      currentLine += (currentLine ? ' ' : '') + word;
    } else {
      if (currentLine) lines.push(currentLine);
      currentLine = word;
    }
  }

  if (currentLine) lines.push(currentLine);

  return lines.join('\n');
}

/**
 * Truncate string to specified length with ellipsis
 *
 * @param {string} str - String to truncate
 * @param {number} [length=100] - Maximum length
 * @param {string} [ellipsis='...'] - Ellipsis string
 * @returns {string} Truncated string
 *
 * @example
 * truncate('A very long string...', 10) // 'A very...'
 */
export function truncate(str, length = 100, ellipsis = '...') {
  if (str === null || str === undefined) return '';
  const text = String(str);

  if (text.length <= length) return text;

  return text.substring(0, length - ellipsis.length) + ellipsis;
}

/**
 * Strip HTML tags from string
 *
 * @param {string} str - String with HTML
 * @returns {string} String without HTML tags
 *
 * @example
 * striptags('<p>Hello <b>World</b></p>') // 'Hello World'
 */
export function striptags(str) {
  if (str === null || str === undefined) return '';
  return String(str).replace(/<[^>]*>/g, '');
}

/**
 * Format number with thousands separator
 *
 * @param {number} num - Number to format
 * @param {string} [locale='en-US'] - Locale for formatting
 * @returns {string} Formatted number
 *
 * @example
 * formatnumber(1234567) // '1,234,567'
 */
export function formatnumber(num, locale = 'en-US') {
  if (num === null || num === undefined) return '';
  const n = Number(num);
  if (isNaN(n)) return String(num);
  return n.toLocaleString(locale);
}

/**
 * Convert ordinal number to ordinal string
 *
 * @param {number} num - Number to convert
 * @returns {string} Ordinal string (e.g., '1st', '2nd', '3rd')
 *
 * @example
 * ordinal(1) // '1st'
 * ordinal(22) // '22nd'
 * ordinal(33) // '33rd'
 */
export function ordinal(num) {
  if (num === null || num === undefined) return '';
  const n = Number(num);
  if (isNaN(n)) return String(num);

  const suffixes = ['th', 'st', 'nd', 'rd'];
  const v = n % 100;

  return n + (suffixes[(v - 20) % 10] || suffixes[v] || suffixes[0]);
}

/**
 * Pluralize a word based on count
 *
 * @param {number} count - Item count
 * @param {string} singular - Singular form
 * @param {string} [plural] - Plural form (defaults to singular + 's')
 * @returns {string} Correct form based on count
 *
 * @example
 * pluralize(1, 'paper') // 'paper'
 * pluralize(5, 'paper') // 'papers'
 * pluralize(0, 'thesis', 'theses') // 'theses'
 */
export function pluralize(count, singular, plural) {
  if (count === null || count === undefined) return singular;
  const n = Number(count);
  if (isNaN(n)) return singular;

  return n === 1 ? singular : plural || singular + 's';
}

/**
 * Format author name for LaTeX (Last, First M.)
 *
 * @param {string | { name: string, affiliation?: string }} author - Author info
 * @returns {string} Formatted author name
 *
 * @example
 * formatauthor('John Smith') // 'Smith, John'
 * formatauthor({ name: 'John Smith', affiliation: 'MIT' }) // 'Smith, John'
 */
export function formatauthor(author) {
  if (!author) return '';

  const name = typeof author === 'string' ? author : author.name;
  if (!name) return '';

  const parts = name.trim().split(/\s+/);
  if (parts.length === 1) return parts[0];

  const lastName = parts.pop();
  return `${lastName}, ${parts.join(' ')}`;
}

/**
 * Schema for filter registration options
 */
export const FilterRegistrationSchema = z.object({
  name: z.string().min(1),
  fn: z.function(),
  description: z.string().optional()
});

/**
 * All available Nunjucks filters as a named map
 *
 * @type {Record<string, Function>}
 */
export const ALL_FILTERS = Object.freeze({
  texescape,
  bibtexkey,
  latexjoin,
  formatdate,
  uppercase,
  lowercase,
  slugify,
  capitalize,
  titlecase,
  wraptext,
  truncate,
  striptags,
  formatnumber,
  ordinal,
  pluralize,
  formatauthor
});

/**
 * Register all filters with a Nunjucks environment
 *
 * @param {import('nunjucks').Environment} env - Nunjucks environment
 * @returns {void}
 *
 * @example
 * const env = nunjucks.configure('./templates');
 * registerAllFilters(env);
 */
export function registerAllFilters(env) {
  if (!env || typeof env.addFilter !== 'function') {
    throw new Error('Invalid Nunjucks environment: missing addFilter method');
  }

  for (const [name, fn] of Object.entries(ALL_FILTERS)) {
    env.addFilter(name, fn);
  }
}

/**
 * Filter metadata for documentation
 * @type {Array<{ name: string, description: string, signature: string }>}
 */
export const FILTER_METADATA = [
  {
    name: 'texescape',
    description: 'Escape LaTeX special characters (&, %, $, #, _, {}, ~, ^, \\)',
    signature: 'texescape(str)'
  },
  {
    name: 'bibtexkey',
    description: 'Generate bibliography key from title',
    signature: 'bibtexkey(title)'
  },
  {
    name: 'latexjoin',
    description: 'Join array items for LaTeX with optional separator',
    signature: "latexjoin(items, sep=' ')"
  },
  {
    name: 'formatdate',
    description: 'Format date for LaTeX (long, short, iso, year, month-year)',
    signature: "formatdate(date, format='long')"
  },
  {
    name: 'uppercase',
    description: 'Convert string to uppercase',
    signature: 'uppercase(str)'
  },
  {
    name: 'lowercase',
    description: 'Convert string to lowercase',
    signature: 'lowercase(str)'
  },
  {
    name: 'slugify',
    description: 'Convert to URL-friendly slug format',
    signature: 'slugify(str)'
  },
  {
    name: 'capitalize',
    description: 'Capitalize first letter of string',
    signature: 'capitalize(str)'
  },
  {
    name: 'titlecase',
    description: 'Convert string to Title Case',
    signature: 'titlecase(str)'
  },
  {
    name: 'wraptext',
    description: 'Wrap text at specified column width',
    signature: 'wraptext(str, width=80)'
  },
  {
    name: 'truncate',
    description: 'Truncate string to specified length with ellipsis',
    signature: "truncate(str, length=100, ellipsis='...')"
  },
  {
    name: 'striptags',
    description: 'Strip HTML tags from string',
    signature: 'striptags(str)'
  },
  {
    name: 'formatnumber',
    description: 'Format number with thousands separator',
    signature: "formatnumber(num, locale='en-US')"
  },
  {
    name: 'ordinal',
    description: "Convert number to ordinal string (1st, 2nd, 3rd, etc.)",
    signature: 'ordinal(num)'
  },
  {
    name: 'pluralize',
    description: 'Pluralize word based on count',
    signature: 'pluralize(count, singular, plural?)'
  },
  {
    name: 'formatauthor',
    description: 'Format author name for LaTeX (Last, First)',
    signature: 'formatauthor(author)'
  }
];
