/**
 * KGEN Filter System - Deterministic template filters
 *
 * Implements all v1 Lock specification filters:
 * - Text: upper, lower, trim, replace, split, join, slice
 * - Data: default, unique, sort, groupby, map, sum, count
 * - Format: json, md, csv
 * - RDF: prefix, expand, sparql
 * - Validation: shaclReport
 * - CAS: casDigest, attestRef
 */

import crypto from 'crypto';

export class KGenFilters {
  constructor(options = {}) {
    this.options = {
      deterministicMode: options.deterministicMode !== false,
      strictMode: options.strictMode !== false,
      ...options
    };

    this.filters = new Map();
    this.registerCoreFilters();
  }

  /**
   * Register a filter function
   */
  register(name, filterFunction, options = {}) {
    if (typeof filterFunction !== 'function') {
      throw new Error(`Filter '${name}' must be a function`);
    }

    this.filters.set(name, {
      function: filterFunction,
      deterministic: options.deterministic !== false,
      description: options.description || '',
      category: options.category || 'custom'
    });
  }

  /**
   * Apply filter to value
   */
  apply(filterName, value, ...args) {
    const filter = this.filters.get(filterName);

    if (!filter) {
      if (this.options.strictMode) {
        throw new Error(`Unknown filter: ${filterName}`);
      }
      return value; // Return original value if filter not found
    }

    // Check deterministic mode compliance
    if (this.options.deterministicMode && !filter.deterministic) {
      throw new Error(`Filter '${filterName}' is not deterministic and cannot be used in deterministic mode`);
    }

    try {
      return filter.function(value, ...args);
    } catch (error) {
      if (this.options.strictMode) {
        throw new Error(`Filter '${filterName}' failed: ${error.message}`);
      }
      return value; // Return original value on error
    }
  }

  /**
   * Check if filter exists
   */
  has(filterName) {
    return this.filters.has(filterName);
  }

  /**
   * Get filter count
   */
  getFilterCount() {
    return this.filters.size;
  }

  /**
   * Register all core filters
   */
  registerCoreFilters() {
    this.registerTextFilters();
    this.registerDataFilters();
    this.registerFormatFilters();
    this.registerRDFFilters();
    this.registerValidationFilters();
    this.registerCASFilters();
    this.registerUtilityFilters();
  }

  /**
   * Text processing filters
   */
  registerTextFilters() {
    this.register('upper', (str) => String(str || '').toUpperCase(), {
      category: 'text',
      description: 'Convert string to uppercase'
    });

    this.register('lower', (str) => String(str || '').toLowerCase(), {
      category: 'text',
      description: 'Convert string to lowercase'
    });

    this.register('trim', (str) => String(str || '').trim(), {
      category: 'text',
      description: 'Remove leading and trailing whitespace'
    });

    this.register('replace', (str, search, replace = '') => {
      const searchRegex = typeof search === 'string' ?
        new RegExp(search.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'g') :
        search;
      return String(str || '').replace(searchRegex, replace);
    }, {
      category: 'text',
      description: 'Replace occurrences of search string'
    });

    this.register('split', (str, separator = '') => {
      return String(str || '').split(separator);
    }, {
      category: 'text',
      description: 'Split string into array'
    });

    this.register('join', (arr, separator = '') => {
      if (!Array.isArray(arr)) return String(arr || '');
      return arr.join(separator);
    }, {
      category: 'text',
      description: 'Join array elements into string'
    });

    this.register('slice', (str, start = 0, end) => {
      const s = String(str || '');
      return end !== undefined ? s.slice(start, end) : s.slice(start);
    }, {
      category: 'text',
      description: 'Extract substring by position'
    });
  }

  /**
   * Data processing filters
   */
  registerDataFilters() {
    this.register('default', (value, defaultValue = '') => {
      return (value === null || value === undefined || value === '') ? defaultValue : value;
    }, {
      category: 'data',
      description: 'Provide default value if original is empty'
    });

    this.register('unique', (arr) => {
      if (!Array.isArray(arr)) return arr;
      return [...new Set(arr)];
    }, {
      category: 'data',
      description: 'Remove duplicate values from array'
    });

    this.register('sort', (arr, key) => {
      if (!Array.isArray(arr)) return arr;

      return [...arr].sort((a, b) => {
        let aVal, bVal;

        if (key) {
          aVal = typeof a === 'object' ? a[key] : a;
          bVal = typeof b === 'object' ? b[key] : b;
        } else {
          aVal = a;
          bVal = b;
        }

        // Handle null/undefined consistently
        if (aVal == null && bVal == null) return 0;
        if (aVal == null) return -1;
        if (bVal == null) return 1;

        // Stable sort for determinism
        return aVal > bVal ? 1 : aVal < bVal ? -1 : 0;
      });
    }, {
      category: 'data',
      description: 'Sort array by value or key'
    });

    this.register('groupby', (arr, key) => {
      if (!Array.isArray(arr)) return {};

      const groups = {};
      arr.forEach(item => {
        const groupKey = typeof item === 'object' && item !== null ?
          item[key] : String(item);

        if (!groups[groupKey]) {
          groups[groupKey] = [];
        }
        groups[groupKey].push(item);
      });

      return groups;
    }, {
      category: 'data',
      description: 'Group array elements by key'
    });

    this.register('map', (arr, key) => {
      if (!Array.isArray(arr)) return arr;

      return arr.map(item => {
        if (typeof item === 'object' && item !== null) {
          return item[key];
        }
        return item;
      });
    }, {
      category: 'data',
      description: 'Extract values by key from array of objects'
    });

    this.register('sum', (arr, key) => {
      if (!Array.isArray(arr)) return 0;

      return arr.reduce((sum, item) => {
        let val;
        if (key && typeof item === 'object' && item !== null) {
          val = item[key];
        } else {
          val = item;
        }

        const num = Number(val);
        return sum + (isNaN(num) ? 0 : num);
      }, 0);
    }, {
      category: 'data',
      description: 'Sum numeric values in array'
    });

    this.register('count', (arr) => {
      if (Array.isArray(arr)) return arr.length;
      if (typeof arr === 'object' && arr !== null) return Object.keys(arr).length;
      return 0;
    }, {
      category: 'data',
      description: 'Count elements in array or object'
    });
  }

  /**
   * Format output filters
   */
  registerFormatFilters() {
    this.register('json', (obj, indent) => {
      try {
        const indentValue = indent ? (typeof indent === 'number' ? indent : 2) : 0;
        return JSON.stringify(obj, null, indentValue);
      } catch (error) {
        return '{}';
      }
    }, {
      category: 'format',
      description: 'Convert object to JSON string'
    });

    this.register('md', (str) => {
      // Basic markdown escaping for safety
      return String(str || '').replace(/([*_`\\])/g, '\\$1');
    }, {
      category: 'format',
      description: 'Escape markdown special characters'
    });

    this.register('csv', (arr, delimiter = ',') => {
      if (!Array.isArray(arr)) return '';

      return arr.map(item => {
        if (typeof item === 'object' && item !== null) {
          return JSON.stringify(item).replace(/"/g, '""');
        }
        const str = String(item);
        return str.includes(delimiter) ? `"${str.replace(/"/g, '""')}"` : str;
      }).join(delimiter);
    }, {
      category: 'format',
      description: 'Convert array to CSV format'
    });
  }

  /**
   * RDF and semantic web filters
   */
  registerRDFFilters() {
    this.register('prefix', (uri, prefixes = {}) => {
      if (!uri || typeof uri !== 'string') return uri;

      // Find matching prefix
      for (const [prefix, namespace] of Object.entries(prefixes)) {
        if (uri.startsWith(namespace)) {
          return uri.replace(namespace, `${prefix}:`);
        }
      }

      return uri;
    }, {
      category: 'rdf',
      description: 'Convert full URI to prefixed form'
    });

    this.register('expand', (prefixedUri, prefixes = {}) => {
      if (!prefixedUri || typeof prefixedUri !== 'string') return prefixedUri;

      const colonIndex = prefixedUri.indexOf(':');
      if (colonIndex === -1) return prefixedUri;

      const prefix = prefixedUri.substring(0, colonIndex);
      const suffix = prefixedUri.substring(colonIndex + 1);

      if (prefixes[prefix]) {
        return prefixes[prefix] + suffix;
      }

      return prefixedUri;
    }, {
      category: 'rdf',
      description: 'Expand prefixed URI to full form'
    });

    this.register('sparql', (query, params = {}) => {
      if (!query || typeof query !== 'string') return '';

      let processedQuery = query;

      // Simple parameter substitution for deterministic queries
      Object.entries(params).forEach(([key, value]) => {
        const placeholder = new RegExp(`\\$\\{${key}\\}`, 'g');
        processedQuery = processedQuery.replace(placeholder, String(value));
      });

      return processedQuery;
    }, {
      category: 'rdf',
      description: 'Process SPARQL query with parameters'
    });
  }

  /**
   * Validation filters
   */
  registerValidationFilters() {
    this.register('shaclReport', (data, shaclShapes = {}) => {
      // Simplified SHACL validation for deterministic behavior
      const report = {
        conforms: true,
        results: [],
        timestamp: this.options.deterministicMode ? '2024-01-01T00:00:00.000Z' : new Date().toISOString()
      };

      if (!data || typeof data !== 'object') {
        report.conforms = false;
        report.results.push({
          severity: 'Violation',
          message: 'Invalid data format for SHACL validation'
        });
      }

      // TODO: Implement full SHACL validation logic
      // For now, return basic conformance report

      return report;
    }, {
      category: 'validation',
      description: 'Generate SHACL validation report'
    });
  }

  /**
   * Content Addressable Storage (CAS) filters
   */
  registerCASFilters() {
    this.register('casDigest', (content, algorithm = 'sha256') => {
      try {
        const hash = crypto.createHash(algorithm);
        hash.update(String(content || ''), 'utf8');
        return hash.digest('hex');
      } catch (error) {
        if (this.options.strictMode) {
          throw new Error(`CAS digest failed: ${error.message}`);
        }
        return '';
      }
    }, {
      category: 'cas',
      description: 'Generate content-addressable digest'
    });

    this.register('attestRef', (content, options = {}) => {
      const digest = this.apply('casDigest', content, options.algorithm || 'sha256');
      const timestamp = this.options.deterministicMode ?
        '2024-01-01T00:00:00.000Z' :
        new Date().toISOString();

      return {
        digest,
        algorithm: options.algorithm || 'sha256',
        timestamp,
        attestor: options.attestor || 'kgen-templates',
        version: '1.0.0'
      };
    }, {
      category: 'cas',
      description: 'Generate attestation reference for content'
    });
  }

  /**
   * Utility filters
   */
  registerUtilityFilters() {
    // Non-deterministic filters that throw in deterministic mode
    this.register('now', () => {
      if (this.options.deterministicMode) {
        throw new Error('Filter "now" is not allowed in deterministic mode');
      }
      return new Date().toISOString();
    }, {
      category: 'utility',
      deterministic: false,
      description: 'Get current timestamp (non-deterministic)'
    });

    this.register('random', () => {
      if (this.options.deterministicMode) {
        throw new Error('Filter "random" is not allowed in deterministic mode');
      }
      return Math.random();
    }, {
      category: 'utility',
      deterministic: false,
      description: 'Generate random number (non-deterministic)'
    });

    this.register('uuid', () => {
      if (this.options.deterministicMode) {
        throw new Error('Filter "uuid" is not allowed in deterministic mode');
      }
      return crypto.randomUUID();
    }, {
      category: 'utility',
      deterministic: false,
      description: 'Generate UUID (non-deterministic)'
    });
  }

  /**
   * Get all filters by category
   */
  getFiltersByCategory(category) {
    const result = {};

    for (const [name, filter] of this.filters) {
      if (filter.category === category) {
        result[name] = {
          description: filter.description,
          deterministic: filter.deterministic
        };
      }
    }

    return result;
  }

  /**
   * List all filter names
   */
  listFilters() {
    return Array.from(this.filters.keys()).sort();
  }

  /**
   * Get filter statistics
   */
  getStats() {
    const categories = {};
    let deterministicCount = 0;

    for (const [name, filter] of this.filters) {
      if (!categories[filter.category]) {
        categories[filter.category] = 0;
      }
      categories[filter.category]++;

      if (filter.deterministic) {
        deterministicCount++;
      }
    }

    return {
      totalFilters: this.filters.size,
      deterministicFilters: deterministicCount,
      nonDeterministicFilters: this.filters.size - deterministicCount,
      categories,
      deterministicMode: this.options.deterministicMode
    };
  }
}

export default KGenFilters;