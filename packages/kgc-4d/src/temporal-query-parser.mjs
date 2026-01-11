/**
 * @file Temporal SPARQL Query Parser
 * @module @unrdf/kgc-4d/temporal-query-parser
 * @description Parse temporal SPARQL extensions for time-travel queries
 *
 * Supported syntax:
 * - AT TIMESTAMP 'iso8601' - Query at specific point in time
 * - BETWEEN 'iso8601' AND 'iso8601' - Query across time range
 *
 * @example
 * import { parseTemporalQuery } from './temporal-query-parser.mjs';
 *
 * const result = parseTemporalQuery(`
 *   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 *   AT TIMESTAMP '2026-01-01T00:00:00Z'
 * `);
 * // result.mode: 'point-in-time'
 * // result.timestamp: '2026-01-01T00:00:00Z'
 * // result.baseSparql: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
 */

import { fromISO } from './time.mjs';
import { TemporalQuerySchema } from './schemas/temporal-sparql-schema.mjs';

/**
 * Parse temporal SPARQL query
 *
 * @param {string} query - SPARQL query with temporal extension
 * @returns {Object} Parsed query with mode, timestamps, and base SPARQL
 * @throws {Error} If query syntax is invalid
 *
 * @example
 * const result = parseTemporalQuery(`
 *   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 *   AT TIMESTAMP '2026-01-01T00:00:00Z'
 * `);
 */
export function parseTemporalQuery(query) {
  if (typeof query !== 'string' || query.trim().length === 0) {
    throw new TypeError('parseTemporalQuery: query must be a non-empty string');
  }

  const trimmed = query.trim();

  // Pattern 1: AT TIMESTAMP 'iso8601'
  const atTimestampPattern = /^(.*?)\s+AT\s+TIMESTAMP\s+'([^']+)'\s*$/is;
  const atMatch = trimmed.match(atTimestampPattern);

  if (atMatch) {
    const [, baseSparql, timestamp] = atMatch;

    // Validate ISO 8601 format by parsing
    try {
      const timestampNs = fromISO(timestamp);

      const result = {
        mode: 'point-in-time',
        timestamp,
        timestampNs,
        baseSparql: baseSparql.trim(),
        originalQuery: query,
      };

      // Validate with Zod schema
      return TemporalQuerySchema.parse(result);
    } catch (error) {
      throw new Error(`Invalid timestamp format: ${timestamp}. Expected ISO 8601 format. ${error.message}`);
    }
  }

  // Pattern 2: BETWEEN 'iso8601' AND 'iso8601'
  const betweenPattern = /^(.*?)\s+BETWEEN\s+'([^']+)'\s+AND\s+'([^']+)'\s*$/is;
  const betweenMatch = trimmed.match(betweenPattern);

  if (betweenMatch) {
    const [, baseSparql, startTimestamp, endTimestamp] = betweenMatch;

    try {
      const startTimestampNs = fromISO(startTimestamp);
      const endTimestampNs = fromISO(endTimestamp);

      if (startTimestampNs >= endTimestampNs) {
        throw new Error('Start timestamp must be before end timestamp');
      }

      const result = {
        mode: 'time-range',
        startTimestamp,
        startTimestampNs,
        endTimestamp,
        endTimestampNs,
        baseSparql: baseSparql.trim(),
        originalQuery: query,
      };

      // Validate with Zod schema
      return TemporalQuerySchema.parse(result);
    } catch (error) {
      throw new Error(`Invalid timestamp format in BETWEEN clause. ${error.message}`);
    }
  }

  // Pattern 3: No temporal clause - current time query
  return TemporalQuerySchema.parse({
    mode: 'current',
    baseSparql: trimmed,
    originalQuery: query,
  });
}

/**
 * Extract base SPARQL query from temporal query
 *
 * @param {string} query - Temporal SPARQL query
 * @returns {string} Base SPARQL without temporal clauses
 *
 * @example
 * const base = extractBaseSparql(`
 *   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 *   AT TIMESTAMP '2026-01-01T00:00:00Z'
 * `);
 * // base: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
 */
export function extractBaseSparql(query) {
  const parsed = parseTemporalQuery(query);
  return parsed.baseSparql;
}

/**
 * Check if query has temporal clauses
 *
 * @param {string} query - SPARQL query
 * @returns {boolean} True if query has temporal clauses
 *
 * @example
 * const hasTemporal = hasTemporalClauses(`SELECT ?s ?p ?o AT TIMESTAMP '...'`);
 * // hasTemporal: true
 */
export function hasTemporalClauses(query) {
  const parsed = parseTemporalQuery(query);
  return parsed.mode !== 'current';
}

/**
 * Validate temporal query syntax
 *
 * @param {string} query - SPARQL query
 * @returns {{valid: boolean, error?: string}} Validation result
 *
 * @example
 * const validation = validateTemporalQuery(`SELECT ?s ?p ?o AT TIMESTAMP 'invalid'`);
 * // validation.valid: false
 */
export function validateTemporalQuery(query) {
  try {
    parseTemporalQuery(query);
    return { valid: true };
  } catch (error) {
    return {
      valid: false,
      error: error.message
    };
  }
}
