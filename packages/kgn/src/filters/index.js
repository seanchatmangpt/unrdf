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

      const hours = d.getHours().toString().padStart(2, '0');
      const minutes = d.getMinutes().toString().padStart(2, '0');
      const seconds = d.getSeconds().toString().padStart(2, '0');

      switch (format) {
        case 'HH:mm:ss':
          return `${hours}:${minutes}:${seconds}`;
        case 'HH:mm':
          return `${hours}:${minutes}`;
        default:
          return `${hours}:${minutes}:${seconds}`;
      }
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
    filename: (filePath) => {
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

    dirname: (filePath) => {
      if (!filePath) return '';
      const parts = filePath.split('/');
      if (parts.length === 1) {
        // Try Windows separators
        const winParts = filePath.split('\\');
        return winParts.length > 1 ? winParts.slice(0, -1).join('\\') : '';
      }
      return parts.slice(0, -1).join('/');
    },

    // Code generation helpers
    comment: (text, style = '//') => {
      if (!text) return '';
      switch (style) {
        case '//':
          return text.split('\n').map(line => `// ${line}`).join('\n');
        case '#':
          return text.split('\n').map(line => `# ${line}`).join('\n');
        case '/*':
          return `/* ${text} */`;
        case '<!--':
          return `<!-- ${text} -->`;
        default:
          return text.split('\n').map(line => `${style} ${line}`).join('\n');
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
        throw new Error('Filter "now" is not allowed in deterministic mode. Use "timestamp" instead.');
      }
      return new Date().toISOString();
    },

    random: () => {
      if (deterministicMode) {
        throw new Error('Filter "random" is not allowed in deterministic mode. Use "hash" for consistent randomness.');
      }
      return Math.random();
    },

    uuid: () => {
      if (deterministicMode) {
        throw new Error('Filter "uuid" is not allowed in deterministic mode. Use "hash" for consistent IDs.');
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
        const headerRow = Object.keys(data[0]).map(h => `${quote}${h}${quote}`).join(delimiter);
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
            case 'center': return ':---:';
            case 'right': return '---:';
            default: return '---';
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
    }
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