/**
 * @file SPARQL Pattern Matching Engine
 * @module sparql-pattern-matcher
 *
 * @description
 * Converts SPARQL WHERE patterns to efficient triple store queries.
 * Provides pattern matching for subjects, predicates, and objects
 * with FILTER expression support.
 *
 * This module implements the core pattern matching logic for SPARQL queries
 * without requiring a full SPARQL parser, optimized for BEAM-style pattern matching.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf.sparql-pattern-matcher');

/**
 * Variable pattern regex - matches SPARQL variables like ?x, ?subject
 * @constant {RegExp}
 */
const VARIABLE_PATTERN = /^\?(\w+)$/;

/**
 * URI pattern regex - matches full URIs like <http://example.org/foo>
 * @constant {RegExp}
 */
const URI_PATTERN = /^<([^>]+)>$/;

/**
 * Prefixed name pattern - matches prefix:localName
 * @constant {RegExp}
 */
const PREFIXED_PATTERN = /^(\w*):(\w+)$/;

/**
 * Literal pattern - matches quoted strings with optional language/datatype
 * @constant {RegExp}
 */
const LITERAL_PATTERN = /^"([^"]*)"(?:@(\w+)|\^\^<([^>]+)>)?$/;

/**
 * SPARQL SELECT query pattern
 * @constant {RegExp}
 */
const SELECT_PATTERN = /SELECT\s+((?:\?\w+\s*)+|\*)\s+WHERE\s*\{([^}]+)\}/is;

/**
 * Triple pattern regex
 * @constant {RegExp}
 */
const TRIPLE_PATTERN = /([^\s]+)\s+([^\s]+)\s+([^\s]+)\s*[.;]?/g;

/**
 * FILTER pattern
 * @constant {RegExp}
 */
const FILTER_PATTERN = /FILTER\s*\(\s*(.+?)\s*\)/gi;

/**
 * Common namespace prefixes
 * @constant {Object.<string, string>}
 */
const DEFAULT_PREFIXES = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  owl: 'http://www.w3.org/2002/07/owl#',
  foaf: 'http://xmlns.com/foaf/0.1/',
  dc: 'http://purl.org/dc/elements/1.1/',
  dcterms: 'http://purl.org/dc/terms/',
  schema: 'http://schema.org/',
  ex: 'http://example.org/',
};

/**
 * Parse a SPARQL term (variable, URI, prefixed name, or literal)
 * @param {string} term - The SPARQL term string
 * @param {Object.<string, string>} prefixes - Namespace prefixes
 * @returns {{type: string, value: string, variable?: string, lang?: string, datatype?: string}}
 */
function parseTerm(term, prefixes = DEFAULT_PREFIXES) {
  if (!term || typeof term !== 'string') {
    return { type: 'invalid', value: '' };
  }

  const trimmed = term.trim();

  // Check for variable
  const varMatch = trimmed.match(VARIABLE_PATTERN);
  if (varMatch) {
    return { type: 'variable', value: varMatch[1], variable: varMatch[1] };
  }

  // Check for full URI
  const uriMatch = trimmed.match(URI_PATTERN);
  if (uriMatch) {
    return { type: 'uri', value: uriMatch[1] };
  }

  // Check for prefixed name
  const prefixMatch = trimmed.match(PREFIXED_PATTERN);
  if (prefixMatch) {
    const prefix = prefixMatch[1] || '';
    const local = prefixMatch[2];
    const namespace = prefixes[prefix] || DEFAULT_PREFIXES[prefix];
    if (namespace) {
      return { type: 'uri', value: namespace + local };
    }
    return { type: 'prefixed', value: trimmed, prefix, local };
  }

  // Check for literal
  const literalMatch = trimmed.match(LITERAL_PATTERN);
  if (literalMatch) {
    return {
      type: 'literal',
      value: literalMatch[1],
      lang: literalMatch[2] || undefined,
      datatype: literalMatch[3] || undefined,
    };
  }

  // Check for keyword 'a' (rdf:type)
  if (trimmed === 'a') {
    return { type: 'uri', value: DEFAULT_PREFIXES.rdf + 'type' };
  }

  return { type: 'unknown', value: trimmed };
}

/**
 * Parse a FILTER expression into an evaluator function
 * @param {string} expr - FILTER expression
 * @returns {function(Object.<string, any>): boolean}
 */
function parseFilter(expr) {
  const trimmed = expr.trim();

  // Equality: ?x = "value" or ?x = <uri>
  const eqMatch = trimmed.match(/^\?(\w+)\s*=\s*(.+)$/);
  if (eqMatch) {
    const varName = eqMatch[1];
    const compareTo = parseTerm(eqMatch[2].trim());
    return bindings => {
      const val = bindings[varName];
      if (!val) return false;
      return val.value === compareTo.value;
    };
  }

  // Inequality: ?x != "value"
  const neqMatch = trimmed.match(/^\?(\w+)\s*!=\s*(.+)$/);
  if (neqMatch) {
    const varName = neqMatch[1];
    const compareTo = parseTerm(neqMatch[2].trim());
    return bindings => {
      const val = bindings[varName];
      if (!val) return true;
      return val.value !== compareTo.value;
    };
  }

  // Less than: ?x < "value"
  const ltMatch = trimmed.match(/^\?(\w+)\s*<\s*(.+)$/);
  if (ltMatch) {
    const varName = ltMatch[1];
    const compareTo = parseTerm(ltMatch[2].trim());
    return bindings => {
      const val = bindings[varName];
      if (!val) return false;
      return val.value < compareTo.value;
    };
  }

  // Greater than: ?x > "value"
  const gtMatch = trimmed.match(/^\?(\w+)\s*>\s*(.+)$/);
  if (gtMatch) {
    const varName = gtMatch[1];
    const compareTo = parseTerm(gtMatch[2].trim());
    return bindings => {
      const val = bindings[varName];
      if (!val) return false;
      return val.value > compareTo.value;
    };
  }

  // AND: expr1 && expr2
  const andMatch = trimmed.match(/^(.+?)\s*&&\s*(.+)$/);
  if (andMatch) {
    const left = parseFilter(andMatch[1]);
    const right = parseFilter(andMatch[2]);
    return bindings => left(bindings) && right(bindings);
  }

  // OR: expr1 || expr2
  const orMatch = trimmed.match(/^(.+?)\s*\|\|\s*(.+)$/);
  if (orMatch) {
    const left = parseFilter(orMatch[1]);
    const right = parseFilter(orMatch[2]);
    return bindings => left(bindings) || right(bindings);
  }

  // Default: always true (unknown filter)
  return () => true;
}

/**
 * SPARQL Pattern Matcher
 * Converts SPARQL WHERE patterns to efficient triple store queries
 */
export class SPARQLPatternMatcher {
  /**
   * Create a new SPARQL Pattern Matcher
   * @param {Object} store - OxigraphStore or compatible store with match/getQuads method
   * @param {Object.<string, string>} [prefixes] - Additional namespace prefixes
   */
  constructor(store, prefixes = {}) {
    if (!store || typeof store.match !== 'function') {
      throw new TypeError('SPARQLPatternMatcher: store must have a match() method');
    }
    this.store = store;
    this.prefixes = { ...DEFAULT_PREFIXES, ...prefixes };
    this._queryCache = new Map();
  }

  /**
   * Match a triple pattern against the store
   * @param {string|Object|null} subject - Subject pattern (variable, URI, or null for any)
   * @param {string|Object|null} predicate - Predicate pattern
   * @param {string|Object|null} object - Object pattern
   * @param {Array<function>} [filters] - Optional FILTER functions
   * @returns {Promise<Array<Object>>} Matching bindings
   */
  async matchPattern(subject, predicate, object, filters = []) {
    return tracer.startActiveSpan('sparql.matchPattern', async span => {
      const startTime = performance.now();

      try {
        // Parse patterns
        const sParsed = this._parsePatternTerm(subject);
        const pParsed = this._parsePatternTerm(predicate);
        const oParsed = this._parsePatternTerm(object);

        span.setAttributes({
          'pattern.subject.type': sParsed.type,
          'pattern.predicate.type': pParsed.type,
          'pattern.object.type': oParsed.type,
          'pattern.filters.count': filters.length,
        });

        // Build match arguments
        const sArg = sParsed.type === 'variable' ? null : this._toStoreTerm(sParsed);
        const pArg = pParsed.type === 'variable' ? null : this._toStoreTerm(pParsed);
        const oArg = oParsed.type === 'variable' ? null : this._toStoreTerm(oParsed);

        // Execute match
        const quads = this.store.match(sArg, pArg, oArg);
        const results = [];

        for (const quad of quads) {
          const binding = {};

          // Bind variables
          if (sParsed.type === 'variable') {
            binding[sParsed.variable] = quad.subject;
          }
          if (pParsed.type === 'variable') {
            binding[pParsed.variable] = quad.predicate;
          }
          if (oParsed.type === 'variable') {
            binding[oParsed.variable] = quad.object;
          }

          // Apply filters
          let passesFilters = true;
          for (const filter of filters) {
            if (!filter(binding)) {
              passesFilters = false;
              break;
            }
          }

          if (passesFilters) {
            results.push(binding);
          }
        }

        const duration = performance.now() - startTime;
        span.setAttributes({
          'pattern.result_count': results.length,
          'pattern.duration_ms': duration,
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return results;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Execute a full SPARQL SELECT query
   * @param {string} sparqlQuery - SPARQL SELECT query string
   * @returns {Promise<Array<Object>>} Query results as binding objects
   */
  async executeQuery(sparqlQuery) {
    return tracer.startActiveSpan('sparql.executeQuery', async span => {
      const startTime = performance.now();

      try {
        if (typeof sparqlQuery !== 'string' || !sparqlQuery.trim()) {
          throw new TypeError('executeQuery: query must be a non-empty string');
        }

        span.setAttributes({
          'query.length': sparqlQuery.length,
          'query.type': 'SELECT',
        });

        // Check cache
        const cacheKey = sparqlQuery.trim();
        if (this._queryCache.has(cacheKey)) {
          const cached = this._queryCache.get(cacheKey);
          if (Date.now() - cached.timestamp < 5000) { // 5s cache
            span.setAttribute('query.cache_hit', true);
            return cached.results;
          }
        }

        // Parse SELECT query
        const parsed = this._parseSelectQuery(sparqlQuery);
        if (!parsed) {
          throw new SyntaxError('Failed to parse SPARQL SELECT query');
        }

        const { variables, patterns, filters } = parsed;

        // Parse filter expressions
        const filterFns = filters.map(f => parseFilter(f));

        // Execute patterns and join results
        let results = null;
        for (const pattern of patterns) {
          const patternResults = await this.matchPattern(
            pattern.subject,
            pattern.predicate,
            pattern.object,
            filterFns
          );

          if (results === null) {
            results = patternResults;
          } else {
            // Join with existing results
            results = this._joinBindings(results, patternResults);
          }
        }

        results = results || [];

        // Project to requested variables
        if (variables[0] !== '*') {
          results = results.map(binding => {
            const projected = {};
            for (const v of variables) {
              if (binding[v] !== undefined) {
                projected[v] = binding[v];
              }
            }
            return projected;
          });
        }

        // Cache results
        this._queryCache.set(cacheKey, { results, timestamp: Date.now() });

        // Limit cache size
        if (this._queryCache.size > 100) {
          const firstKey = this._queryCache.keys().next().value;
          this._queryCache.delete(firstKey);
        }

        const duration = performance.now() - startTime;
        span.setAttributes({
          'query.result_count': results.length,
          'query.duration_ms': duration,
          'query.patterns_count': patterns.length,
        });
        span.setStatus({ code: SpanStatusCode.OK });

        return results;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Compile a triple pattern to BEAM-compatible pattern string
   * @param {string|Object|null} subject - Subject pattern
   * @param {string|Object|null} predicate - Predicate pattern
   * @param {string|Object|null} object - Object pattern
   * @returns {string} BEAM pattern representation
   */
  compileToBeamPattern(subject, predicate, object) {
    const s = this._compileTermToBeam(subject);
    const p = this._compileTermToBeam(predicate);
    const o = this._compileTermToBeam(object);

    return `{${s}, ${p}, ${o}}`;
  }

  /**
   * Parse a pattern term for matching
   * @param {string|Object|null} term - Pattern term
   * @returns {Object} Parsed term
   * @private
   */
  _parsePatternTerm(term) {
    if (term === null || term === undefined) {
      return { type: 'variable', variable: '_' };
    }

    if (typeof term === 'object' && term.termType) {
      // Already an RDF term
      return {
        type: term.termType === 'Variable' ? 'variable' : 'term',
        value: term.value,
        variable: term.termType === 'Variable' ? term.value : undefined,
        term,
      };
    }

    return parseTerm(String(term), this.prefixes);
  }

  /**
   * Convert parsed term to store-compatible term
   * @param {Object} parsed - Parsed term
   * @returns {Object|null} Store term or null
   * @private
   */
  _toStoreTerm(parsed) {
    if (parsed.type === 'variable' || parsed.type === 'invalid') {
      return null;
    }

    if (parsed.term) {
      return parsed.term;
    }

    // Create term using store's data factory if available
    if (parsed.type === 'uri') {
      const df = this.store.constructor.getDataFactory?.();
      if (df?.namedNode) {
        return df.namedNode(parsed.value);
      }
    }

    return null;
  }

  /**
   * Compile a term to BEAM pattern representation
   * @param {string|Object|null} term - Term to compile
   * @returns {string} BEAM pattern string
   * @private
   */
  _compileTermToBeam(term) {
    if (term === null || term === undefined) {
      return '_';
    }

    const parsed = this._parsePatternTerm(term);

    if (parsed.type === 'variable') {
      return `Var_${parsed.variable}`;
    }

    if (parsed.type === 'uri') {
      return `{uri, <<"${parsed.value}">>}`;
    }

    if (parsed.type === 'literal') {
      const langStr = parsed.lang ? `, <<"${parsed.lang}">>` : '';
      const dtStr = parsed.datatype ? `, <<"${parsed.datatype}">>` : '';
      return `{literal, <<"${parsed.value}">>${langStr}${dtStr}}`;
    }

    return `{unknown, <<"${parsed.value}">>}`;
  }

  /**
   * Parse a SPARQL SELECT query
   * @param {string} query - SPARQL query string
   * @returns {Object|null} Parsed query or null
   * @private
   */
  _parseSelectQuery(query) {
    const normalized = query.replace(/\s+/g, ' ').trim();
    const match = normalized.match(SELECT_PATTERN);

    if (!match) {
      return null;
    }

    // Extract variables
    const varsStr = match[1].trim();
    const variables = varsStr === '*' ? ['*'] : varsStr.match(/\?\w+/g)?.map(v => v.slice(1)) || [];

    // Extract WHERE clause content
    const whereClause = match[2].trim();

    // Extract filters
    const filters = [];
    let filterMatch;
    const filterRegex = new RegExp(FILTER_PATTERN);
    while ((filterMatch = filterRegex.exec(whereClause)) !== null) {
      filters.push(filterMatch[1]);
    }

    // Remove filters from WHERE clause for pattern parsing
    const cleanedWhere = whereClause.replace(FILTER_PATTERN, '').trim();

    // Parse triple patterns
    const patterns = [];
    const patternRegex = new RegExp(TRIPLE_PATTERN);
    let patternMatch;

    while ((patternMatch = patternRegex.exec(cleanedWhere)) !== null) {
      const [, s, p, o] = patternMatch;
      if (s && p && o) {
        patterns.push({
          subject: s.trim(),
          predicate: p.trim(),
          object: o.trim().replace(/\.$/, '').trim(),
        });
      }
    }

    return { variables, patterns, filters };
  }

  /**
   * Join two sets of bindings on common variables
   * @param {Array<Object>} left - Left bindings
   * @param {Array<Object>} right - Right bindings
   * @returns {Array<Object>} Joined bindings
   * @private
   */
  _joinBindings(left, right) {
    if (left.length === 0) return right;
    if (right.length === 0) return left;

    const results = [];

    for (const lBinding of left) {
      for (const rBinding of right) {
        // Check compatibility
        let compatible = true;
        for (const key of Object.keys(lBinding)) {
          if (rBinding[key] !== undefined) {
            const lVal = lBinding[key]?.value || lBinding[key];
            const rVal = rBinding[key]?.value || rBinding[key];
            if (lVal !== rVal) {
              compatible = false;
              break;
            }
          }
        }

        if (compatible) {
          results.push({ ...lBinding, ...rBinding });
        }
      }
    }

    return results;
  }

  /**
   * Clear the query cache
   */
  clearCache() {
    this._queryCache.clear();
  }

  /**
   * Get cache statistics
   * @returns {{size: number, maxSize: number}}
   */
  getCacheStats() {
    return {
      size: this._queryCache.size,
      maxSize: 100,
    };
  }

  /**
   * Compile a full SPARQL query to BEAM list comprehension syntax
   * @param {string} sparqlQuery - SPARQL SELECT query
   * @returns {string} BEAM list comprehension code
   */
  compileQueryToBeam(sparqlQuery) {
    const parsed = this._parseSelectQuery(sparqlQuery);
    if (!parsed) {
      throw new SyntaxError('Failed to parse SPARQL query for BEAM compilation');
    }

    const { variables, patterns, filters } = parsed;

    // Build pattern matches for list comprehension generators
    const beamPatterns = patterns.map((p, idx) => {
      const s = this._compilePatternTermToBeam(p.subject, `S${idx}`);
      const pred = this._compilePatternTermToBeam(p.predicate, `P${idx}`);
      const o = this._compilePatternTermToBeam(p.object, `O${idx}`);
      return `{${s}, ${pred}, ${o}} <- Store`;
    });

    // Extract guards (JOIN conditions on shared variables)
    const guards = [];
    const varMap = new Map();

    // Map variables to their positions
    patterns.forEach((p, idx) => {
      [
        { field: 'subject', prefix: 'S' },
        { field: 'predicate', prefix: 'P' },
        { field: 'object', prefix: 'O' }
      ].forEach(({ field, prefix }) => {
        const term = p[field];
        if (term && typeof term === 'string' && term.startsWith('?')) {
          const varName = term.slice(1);
          if (!varMap.has(varName)) {
            varMap.set(varName, []);
          }
          varMap.get(varName).push(`${prefix}${idx}`);
        }
      });
    });

    // Generate JOIN guards for shared variables (equality checks)
    for (const [varName, occurrences] of varMap.entries()) {
      if (occurrences.length > 1) {
        for (let i = 1; i < occurrences.length; i++) {
          guards.push(`${occurrences[0]} =:= ${occurrences[i]}`);
        }
      }
    }

    // Add FILTER guards
    for (const filter of filters) {
      const beamFilter = this._compileFilterToBeam(filter, varMap);
      if (beamFilter) {
        guards.push(beamFilter);
      }
    }

    // Build result expression
    const resultVars = variables[0] === '*'
      ? Array.from(varMap.keys())
      : variables;

    // Capitalize and map to BEAM variable positions
    const resultExpr = resultVars.length === 1
      ? this._getBeamVarName(resultVars[0], varMap)
      : `{${resultVars.map(v => this._getBeamVarName(v, varMap)).join(', ')}}`;

    // Combine into list comprehension
    const allConditions = [...beamPatterns, ...guards];
    const conditions = allConditions.join(',\n        ');

    return `[${resultExpr} ||\n        ${conditions}]`;
  }

  /**
   * Compile a pattern term to BEAM syntax
   * @param {string} term - Pattern term
   * @param {string} defaultVar - Default variable name
   * @returns {string} BEAM term
   * @private
   */
  _compilePatternTermToBeam(term, defaultVar) {
    if (!term) return '_';

    const trimmed = term.trim();

    // Variable: ?name -> Name
    if (trimmed.startsWith('?')) {
      return this._capitalizeVar(trimmed.slice(1));
    }

    // URI: <http://...> -> '{uri, <<"http://...">>}'
    if (trimmed.startsWith('<') && trimmed.endsWith('>')) {
      const uri = trimmed.slice(1, -1);
      return `'${uri}'`;
    }

    // Prefixed: foaf:Person -> expanded URI
    if (trimmed.includes(':') && !trimmed.startsWith('<')) {
      const [prefix, local] = trimmed.split(':');
      const namespace = this.prefixes[prefix];
      if (namespace) {
        return `'${namespace}${local}'`;
      }
    }

    // Literal: "value" -> {literal, <<"value">>}
    if (trimmed.startsWith('"')) {
      const literalValue = trimmed.match(/^"([^"]*)"/)?.[ 1] || '';
      return `{literal, <<"${literalValue}">>}`;
    }

    // Keyword 'a' -> rdf:type
    if (trimmed === 'a') {
      return `'${this.prefixes.rdf}type'`;
    }

    return defaultVar;
  }

  /**
   * Compile a FILTER expression to BEAM guard
   * @param {string} filterExpr - FILTER expression
   * @param {Map<string, Array<string>>} varMap - Variable to position mapping
   * @returns {string|null} BEAM guard or null
   * @private
   */
  _compileFilterToBeam(filterExpr, varMap) {
    const trimmed = filterExpr.trim();

    // Equality: ?x = "value"
    const eqMatch = trimmed.match(/^\?(\w+)\s*=\s*(.+)$/);
    if (eqMatch) {
      const varName = this._getBeamVarName(eqMatch[1], varMap);
      const value = this._compilePatternTermToBeam(eqMatch[2].trim(), '');
      return `${varName} =:= ${value}`;
    }

    // Inequality: ?x != "value"
    const neqMatch = trimmed.match(/^\?(\w+)\s*!=\s*(.+)$/);
    if (neqMatch) {
      const varName = this._getBeamVarName(neqMatch[1], varMap);
      const value = this._compilePatternTermToBeam(neqMatch[2].trim(), '');
      return `${varName} =/= ${value}`;
    }

    // Greater than: ?x > "value"
    const gtMatch = trimmed.match(/^\?(\w+)\s*>\s*(.+)$/);
    if (gtMatch) {
      const varName = this._getBeamVarName(gtMatch[1], varMap);
      const value = gtMatch[2].trim().replace(/^"([^"]*)"$/, '$1');
      return `is_integer(${varName}), ${varName} > ${value}`;
    }

    // Less than: ?x < "value"
    const ltMatch = trimmed.match(/^\?(\w+)\s*<\s*(.+)$/);
    if (ltMatch) {
      const varName = this._getBeamVarName(ltMatch[1], varMap);
      const value = ltMatch[2].trim().replace(/^"([^"]*)"$/, '$1');
      return `is_integer(${varName}), ${varName} < ${value}`;
    }

    return null;
  }

  /**
   * Get BEAM variable name from SPARQL variable
   * @param {string} sparqlVar - SPARQL variable name (without ?)
   * @param {Map<string, Array<string>>} varMap - Variable mapping
   * @returns {string} BEAM variable name
   * @private
   */
  _getBeamVarName(sparqlVar, varMap) {
    const positions = varMap.get(sparqlVar);
    if (positions && positions.length > 0) {
      return positions[0];
    }
    return this._capitalizeVar(sparqlVar);
  }

  /**
   * Capitalize a variable name for BEAM
   * @param {string} varName - Variable name
   * @returns {string} Capitalized name
   * @private
   */
  _capitalizeVar(varName) {
    return varName.charAt(0).toUpperCase() + varName.slice(1);
  }
}

/**
 * Create a new SPARQL Pattern Matcher
 * @param {Object} store - Store with match() method
 * @param {Object.<string, string>} [prefixes] - Namespace prefixes
 * @returns {SPARQLPatternMatcher} Pattern matcher instance
 */
export function createSPARQLPatternMatcher(store, prefixes = {}) {
  return new SPARQLPatternMatcher(store, prefixes);
}

export default SPARQLPatternMatcher;
