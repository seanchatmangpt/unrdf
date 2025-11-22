/**
 * Nunjucks Custom Filters for LaTeX Template Rendering
 * =====================================================
 *
 * Custom filters for processing LaTeX templates with Nunjucks.
 * These filters handle LaTeX escaping, formatting, and specialized transformations.
 *
 * @module nunjucks-filters
 */

/**
 * LaTeX special characters that need escaping
 * @type {Map<string, string>}
 */
const LATEX_ESCAPE_MAP = new Map([
  ['&', '\\&'],
  ['%', '\\%'],
  ['$', '\\$'],
  ['#', '\\#'],
  ['_', '\\_'],
  ['{', '\\{'],
  ['}', '\\}'],
  ['~', '\\textasciitilde{}'],
  ['^', '\\textasciicircum{}'],
  ['\\', '\\textbackslash{}'],
]);

/**
 * Characters that should not be escaped in math mode
 * @type {Set<string>}
 */
const MATH_MODE_SAFE = new Set(['$', '^', '_', '\\', '{', '}']);

/**
 * Escape LaTeX special characters in text
 *
 * @param {string} text - Input text to escape
 * @param {Object} [options] - Escape options
 * @param {boolean} [options.preserveMath=true] - Preserve math mode ($...$)
 * @returns {string} LaTeX-safe escaped text
 *
 * @example
 * texescape("Price: $100 & tax")  // "Price: \$100 \& tax"
 * texescape("100% complete")      // "100\% complete"
 */
export function texescape(text, options = { preserveMath: true }) {
  if (text == null) return '';
  const str = String(text);

  if (options.preserveMath) {
    // Split by math mode delimiters and only escape non-math parts
    const parts = str.split(/(\$[^$]+\$)/g);
    return parts.map((part, i) => {
      // Odd indices are math mode content
      if (i % 2 === 1) return part;
      return escapeLatexChars(part);
    }).join('');
  }

  return escapeLatexChars(str);
}

/**
 * Internal function to escape LaTeX characters
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeLatexChars(str) {
  let result = '';
  for (const char of str) {
    result += LATEX_ESCAPE_MAP.get(char) ?? char;
  }
  return result;
}

/**
 * Format a string as a BibTeX citation key
 *
 * @param {string} text - Text to convert to BibTeX key
 * @returns {string} Valid BibTeX key
 *
 * @example
 * bibtexkey("Smith et al. 2024")  // "smith2024"
 * bibtexkey("Machine Learning")   // "machinelearning"
 */
export function bibtexkey(text) {
  if (text == null) return '';
  return String(text)
    .toLowerCase()
    .replace(/[^a-z0-9]/g, '')
    .slice(0, 50);
}

/**
 * Join array elements with LaTeX-appropriate separator
 *
 * @param {Array} arr - Array to join
 * @param {string} [separator=' \\and '] - LaTeX separator
 * @returns {string} Joined string
 *
 * @example
 * latexjoin(["Alice", "Bob"])           // "Alice \\and Bob"
 * latexjoin(["A", "B", "C"], ", ")      // "A, B, C"
 */
export function latexjoin(arr, separator = ' \\and ') {
  if (!Array.isArray(arr)) return String(arr ?? '');
  return arr.map(item => texescape(item)).join(separator);
}

/**
 * Format date for LaTeX documents
 *
 * @param {string|Date} date - Date to format
 * @param {string} [format='long'] - Format style: 'long', 'short', 'iso', 'latex'
 * @returns {string} Formatted date string
 *
 * @example
 * formatdate(new Date('2024-01-15'), 'long')   // "January 15, 2024"
 * formatdate('2024-01-15', 'short')            // "Jan 15, 2024"
 * formatdate('2024-01-15', 'latex')            // "\\today" or formatted
 */
export function formatdate(date, format = 'long') {
  if (date == null) return '\\today';

  const d = date instanceof Date ? date : new Date(date);
  if (isNaN(d.getTime())) return '\\today';

  const months = [
    'January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'
  ];

  const shortMonths = [
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  ];

  switch (format) {
    case 'long':
      return `${months[d.getMonth()]} ${d.getDate()}, ${d.getFullYear()}`;
    case 'short':
      return `${shortMonths[d.getMonth()]} ${d.getDate()}, ${d.getFullYear()}`;
    case 'iso':
      return d.toISOString().split('T')[0];
    case 'latex':
    default:
      return `${months[d.getMonth()]} ${d.getDate()}, ${d.getFullYear()}`;
  }
}

/**
 * Convert text to LaTeX small caps
 *
 * @param {string} text - Text to convert
 * @returns {string} LaTeX small caps formatted text
 *
 * @example
 * smallcaps("Introduction")  // "\\textsc{Introduction}"
 */
export function smallcaps(text) {
  if (text == null) return '';
  return `\\textsc{${texescape(text)}}`;
}

/**
 * Create LaTeX emphasis (italic)
 *
 * @param {string} text - Text to emphasize
 * @returns {string} LaTeX emphasized text
 *
 * @example
 * emph("important")  // "\\emph{important}"
 */
export function emph(text) {
  if (text == null) return '';
  return `\\emph{${texescape(text)}}`;
}

/**
 * Create LaTeX bold text
 *
 * @param {string} text - Text to bold
 * @returns {string} LaTeX bold text
 *
 * @example
 * bold("critical")  // "\\textbf{critical}"
 */
export function bold(text) {
  if (text == null) return '';
  return `\\textbf{${texescape(text)}}`;
}

/**
 * Create LaTeX typewriter/monospace text
 *
 * @param {string} text - Text to format
 * @returns {string} LaTeX typewriter text
 *
 * @example
 * mono("code")  // "\\texttt{code}"
 */
export function mono(text) {
  if (text == null) return '';
  return `\\texttt{${texescape(text)}}`;
}

/**
 * Create a LaTeX citation
 *
 * @param {string} key - Citation key
 * @param {string} [type='cite'] - Citation type: 'cite', 'citep', 'citet', 'citeauthor'
 * @returns {string} LaTeX citation command
 *
 * @example
 * cite("smith2024")           // "\\cite{smith2024}"
 * cite("smith2024", "citep")  // "\\citep{smith2024}"
 */
export function cite(key, type = 'cite') {
  if (key == null) return '';
  const safeKey = bibtexkey(key);
  return `\\${type}{${safeKey}}`;
}

/**
 * Create a LaTeX label
 *
 * @param {string} text - Label text
 * @param {string} [prefix='sec'] - Label prefix (sec, fig, tab, eq)
 * @returns {string} LaTeX label command
 *
 * @example
 * label("introduction")          // "\\label{sec:introduction}"
 * label("results", "fig")        // "\\label{fig:results}"
 */
export function label(text, prefix = 'sec') {
  if (text == null) return '';
  const safeText = String(text).toLowerCase().replace(/[^a-z0-9]/g, '-');
  return `\\label{${prefix}:${safeText}}`;
}

/**
 * Create a LaTeX reference
 *
 * @param {string} text - Reference label
 * @param {string} [prefix='sec'] - Label prefix
 * @returns {string} LaTeX ref command
 *
 * @example
 * ref("introduction")  // "\\ref{sec:introduction}"
 */
export function ref(text, prefix = 'sec') {
  if (text == null) return '';
  const safeText = String(text).toLowerCase().replace(/[^a-z0-9]/g, '-');
  return `\\ref{${prefix}:${safeText}}`;
}

/**
 * Wrap text in LaTeX environment
 *
 * @param {string} text - Content for environment
 * @param {string} env - Environment name
 * @returns {string} LaTeX environment
 *
 * @example
 * environment("Quote here", "quote")
 * // "\\begin{quote}\nQuote here\n\\end{quote}"
 */
export function environment(text, env) {
  if (text == null || env == null) return '';
  return `\\begin{${env}}\n${text}\n\\end{${env}}`;
}

/**
 * Configure Nunjucks environment with all custom filters
 *
 * @param {Object} nunjucks - Nunjucks environment instance
 * @returns {Object} Configured Nunjucks environment
 *
 * @example
 * import nunjucks from 'nunjucks';
 * import { configureNunjucks } from './nunjucks-filters.mjs';
 *
 * const env = nunjucks.configure('templates');
 * configureNunjucks(env);
 */
export function configureNunjucks(env) {
  env.addFilter('texescape', texescape);
  env.addFilter('bibtexkey', bibtexkey);
  env.addFilter('latexjoin', latexjoin);
  env.addFilter('formatdate', formatdate);
  env.addFilter('smallcaps', smallcaps);
  env.addFilter('emph', emph);
  env.addFilter('bold', bold);
  env.addFilter('mono', mono);
  env.addFilter('cite', cite);
  env.addFilter('label', label);
  env.addFilter('ref', ref);
  env.addFilter('environment', environment);

  return env;
}

/**
 * All filters as a named export object
 */
export const filters = {
  texescape,
  bibtexkey,
  latexjoin,
  formatdate,
  smallcaps,
  emph,
  bold,
  mono,
  cite,
  label,
  ref,
  environment,
};

export default {
  configureNunjucks,
  filters,
  texescape,
  bibtexkey,
  latexjoin,
  formatdate,
};
