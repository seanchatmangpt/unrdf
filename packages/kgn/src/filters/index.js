/**
 * KGEN Template Filters - Complete Filter Library
 *
 * Migrated essential filters from unjucks with 80/20 principle:
 * - Text processing (upper, lower, camelCase, etc.)
 * - Array manipulation (join, sort, unique, etc.)
 * - Data handling (json, default, type checking)
 * - RDF/SPARQL support (mocked for deterministic testing)
 *
 * All filters provide deterministic, reproducible output
 */

import crypto from 'crypto';
import { textFilters } from './text.js';
import { arrayFilters } from './array.js';
import { dataFilters } from './data.js';
import { rdfFilters } from './rdf.js';

/**
 * Create custom filters with deterministic operations
 */
export function createCustomFilters(options = {}) {
  const { deterministicMode = false, staticBuildTime = '2024-01-01T00:00:00.000Z' } = options;

  // Merge all filter collections
  const allFilters = {
    ...textFilters,
    ...arrayFilters,
    ...dataFilters,
    ...rdfFilters,

    // Smart reverse filter that works with both strings and arrays
    reverse: input => {
      if (Array.isArray(input)) {
        return [...input].reverse();
      }
      // Treat as string
      if (input === null || input === undefined) return '';
      return String(input).split('').reverse().join('');
    },

    // Enhanced deterministic filters that override or augment the base filters

    // Deterministic date/time formatting
    formatDate: (date, format = 'YYYY-MM-DD') => {
      if (deterministicMode) {
        // Use static build time for deterministic rendering
        return staticBuildTime.split('T')[0]; // Returns '2024-01-01'
      }

      const d = new Date(date);
      if (isNaN(d.getTime())) return '';

      switch (format) {
        case 'YYYY-MM-DD':
          return d.toISOString().split('T')[0];
        case 'MM/DD/YYYY':
          return `${(d.getMonth() + 1).toString().padStart(2, '0')}/${d.getDate().toString().padStart(2, '0')}/${d.getFullYear()}`;
        case 'DD/MM/YYYY':
          return `${d.getDate().toString().padStart(2, '0')}/${(d.getMonth() + 1).toString().padStart(2, '0')}/${d.getFullYear()}`;
        default:
          return d.toISOString().split('T')[0];
      }
    },

    formatTime: (date, format = 'HH:mm:ss') => {
      if (deterministicMode) {
        return '00:00:00';
      }

      const d = new Date(date);
      if (isNaN(d.getTime())) return '';

      // Use UTC time to match test expectations
      const hours = d.getUTCHours().toString().padStart(2, '0');
      const minutes = d.getUTCMinutes().toString().padStart(2, '0');
      const seconds = d.getUTCSeconds().toString().padStart(2, '0');

      switch (format) {
        case 'HH:mm:ss':
          return `${hours}:${minutes}:${seconds}`;
        case 'HH:mm':
          return `${hours}:${minutes}`;
        default:
          return `${hours}:${minutes}:${seconds}`;
      }
    },

    // Date arithmetic - add days to a date
    dateAdd: (date, amount, unit = 'day') => {
      const d = new Date(date);
      if (isNaN(d.getTime())) return '';

      switch (unit.toLowerCase()) {
        case 'day':
        case 'days':
          d.setUTCDate(d.getUTCDate() + amount);
          break;
        case 'month':
        case 'months':
          d.setUTCMonth(d.getUTCMonth() + amount);
          break;
        case 'year':
        case 'years':
          d.setUTCFullYear(d.getUTCFullYear() + amount);
          break;
        case 'hour':
        case 'hours':
          d.setUTCHours(d.getUTCHours() + amount);
          break;
        case 'minute':
        case 'minutes':
          d.setUTCMinutes(d.getUTCMinutes() + amount);
          break;
        case 'second':
        case 'seconds':
          d.setUTCSeconds(d.getUTCSeconds() + amount);
          break;
        default:
          return '';
      }

      return d.toISOString();
    },

    // Date arithmetic - subtract days from a date
    dateSub: (date, amount, unit = 'day') => {
      return allFilters.dateAdd(date, -amount, unit);
    },

    // Deterministic timestamp
    timestamp: () => {
      if (deterministicMode) {
        return staticBuildTime;
      }
      return new Date().toISOString();
    },

    // Content hashing for deterministic IDs
    hash: (content, algorithm = 'sha256') => {
      return crypto.createHash(algorithm).update(String(content)).digest('hex');
    },

    shortHash: (content, length = 8) => {
      return crypto.createHash('sha256').update(String(content)).digest('hex').substring(0, length);
    },

    // File and path utilities
    filename: filePath => {
      if (!filePath) return '';
      return filePath.split('/').pop().split('\\').pop();
    },

    basename: (filePath, ext) => {
      const name = filePath.split('/').pop().split('\\').pop();
      if (ext && name.endsWith(ext)) {
        return name.slice(0, -ext.length);
      }
      const dotIndex = name.lastIndexOf('.');
      return dotIndex > 0 ? name.slice(0, dotIndex) : name;
    },

    dirname: filePath => {
      if (!filePath) return '';
      const parts = filePath.split('/');
      if (parts.length === 1) {
        // Try Windows separators
        const winParts = filePath.split('\\');
        return winParts.length > 1 ? winParts.slice(0, -1).join('\\') : '';
      }
      return parts.slice(0, -1).join('/');
    },

    // Resolve relative path against base path
    resolve: (basePath, relativePath) => {
      if (!basePath) return relativePath;
      if (!relativePath) return basePath;

      // Split paths into parts
      const baseParts = basePath.split('/').filter(p => p);
      const relParts = relativePath.split('/').filter(p => p);

      // Process relative path components
      for (const part of relParts) {
        if (part === '..') {
          baseParts.pop();
        } else if (part !== '.') {
          baseParts.push(part);
        }
      }

      return '/' + baseParts.join('/');
    },

    // Get relative path from one path to another
    relative: (fromPath, toPath) => {
      if (!fromPath || !toPath) return toPath || '';

      const fromParts = fromPath.split('/').filter(p => p);
      const toParts = toPath.split('/').filter(p => p);

      // Find common ancestor
      let commonCount = 0;
      for (let i = 0; i < Math.min(fromParts.length, toParts.length); i++) {
        if (fromParts[i] === toParts[i]) {
          commonCount++;
        } else {
          break;
        }
      }

      // Build relative path
      const ups = fromParts.length - commonCount;
      const downs = toParts.slice(commonCount);
      const parts = Array(ups).fill('..').concat(downs);

      return parts.join('/');
    },

    // Code generation helpers
    comment: (text, style = '//') => {
      if (!text) return '';
      switch (style) {
        case '//':
          return text
            .split('\n')
            .map(line => `// ${line}`)
            .join('\n');
        case '#':
          return text
            .split('\n')
            .map(line => `# ${line}`)
            .join('\n');
        case '/*':
          return `/* ${text} */`;
        case '<!--':
          return `<!-- ${text} -->`;
        default:
          return text
            .split('\n')
            .map(line => `${style} ${line}`)
            .join('\n');
      }
    },

    // Validation and safety filters
    required: (value, message = 'Value is required') => {
      if (value === null || value === undefined || value === '') {
        throw new Error(message);
      }
      return value;
    },

    // Determinism blockers - these will throw in deterministic mode
    now: () => {
      if (deterministicMode) {
        throw new Error(
          'Filter "now" is not allowed in deterministic mode. Use "timestamp" instead.'
        );
      }
      return new Date().toISOString();
    },

    random: () => {
      if (deterministicMode) {
        throw new Error(
          'Filter "random" is not allowed in deterministic mode. Use "hash" for consistent randomness.'
        );
      }
      return Math.random();
    },

    uuid: () => {
      if (deterministicMode) {
        throw new Error(
          'Filter "uuid" is not allowed in deterministic mode. Use "hash" for consistent IDs.'
        );
      }
      return crypto.randomUUID();
    },

    // CSV conversion filters
    csv: (data, options = {}) => {
      if (!Array.isArray(data)) return '';
      if (data.length === 0) return '';

      const { delimiter = ',', quote = '"', headers = true } = options;
      const lines = [];

      // Get headers from first object
      if (headers && data.length > 0 && typeof data[0] === 'object') {
        const headerRow = Object.keys(data[0])
          .map(h => `${quote}${h}${quote}`)
          .join(delimiter);
        lines.push(headerRow);
      }

      // Add data rows
      for (const row of data) {
        if (typeof row === 'object' && row !== null) {
          const values = Object.values(row).map(v => {
            const str = String(v || '');
            return str.includes(delimiter) || str.includes(quote) || str.includes('\n')
              ? `${quote}${str.replace(new RegExp(quote, 'g'), quote + quote)}${quote}`
              : str;
          });
          lines.push(values.join(delimiter));
        } else {
          lines.push(String(row));
        }
      }

      return lines.join('\n');
    },

    // Markdown table filter
    markdown: (data, options = {}) => {
      if (!Array.isArray(data)) return '';
      if (data.length === 0) return '';

      const { align = 'left' } = options;
      const lines = [];

      if (typeof data[0] === 'object' && data[0] !== null) {
        const headers = Object.keys(data[0]);

        // Header row
        lines.push('| ' + headers.join(' | ') + ' |');

        // Separator row
        const separators = headers.map(() => {
          switch (align) {
            case 'center':
              return ':---:';
            case 'right':
              return '---:';
            default:
              return '---';
          }
        });
        lines.push('| ' + separators.join(' | ') + ' |');

        // Data rows
        for (const row of data) {
          const values = headers.map(h => String(row[h] || '').replace(/\|/g, '\\|'));
          lines.push('| ' + values.join(' | ') + ' |');
        }
      }

      return lines.join('\n');
    },
  };

  return allFilters;
}

/**
 * Register all filters with Nunjucks environment
 * @param {Object} env - Nunjucks environment
 * @param {Object} options - Filter options
 */
export function registerFilters(env, options = {}) {
  const filters = createCustomFilters(options);

  for (const [name, filter] of Object.entries(filters)) {
    env.addFilter(name, filter);
  }

  return env;
}

/**
 * Get all available filter names
 * @param {Object} options - Filter options
 * @returns {Array} Array of filter names
 */
export function getFilterNames(options = {}) {
  return Object.keys(createCustomFilters(options));
}

/**
 * Export individual filter collections
 */
export { textFilters, arrayFilters, dataFilters, rdfFilters };

/**
 * Export default function
 */
export default createCustomFilters;
