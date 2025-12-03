/**
 * @file SPARQL Query Analyzer - Extract query structure and patterns
 * @module @unrdf/dark-matter/query-analyzer
 */

import { z } from 'zod';

/**
 * Query analysis result schema
 */
const AnalysisResultSchema = z.object({
  type: z.enum(['select', 'construct', 'ask', 'describe', 'unknown']),
  patterns: z.array(
    z.object({
      subject: z.string(),
      predicate: z.string(),
      object: z.string(),
      selectivity: z.number().min(0).max(1),
    })
  ),
  variables: z.array(z.string()),
  joins: z.array(
    z.object({
      variable: z.string(),
      patterns: z.array(z.number()),
    })
  ),
  filters: z.array(z.string()),
  prefixes: z.record(z.string()),
  estimatedResults: z.number(),
  criticalPath: z.array(z.number()),
});

/**
 * Analyze SPARQL query structure
 * @param {string} query - SPARQL query string
 * @returns {Object} Analysis result with patterns, joins, complexity
 *
 * @throws {TypeError} If query is not a string
 *
 * @example
 * const analysis = analyzeSparqlQuery(`
 *   PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *   SELECT ?name ?email WHERE {
 *     ?person foaf:name ?name .
 *     ?person foaf:mbox ?email .
 *   }
 * `);
 *
 * console.log('Patterns:', analysis.patterns);
 * console.log('Joins:', analysis.joins);
 */
export function analyzeSparqlQuery(query) {
  if (typeof query !== 'string') {
    throw new TypeError('analyzeSparqlQuery: query must be a string');
  }

  // Detect query type
  const trimmed = query.trim().toUpperCase();
  let type = 'unknown';

  if (trimmed.includes('SELECT')) {
    type = 'select';
  } else if (trimmed.includes('CONSTRUCT')) {
    type = 'construct';
  } else if (trimmed.includes('ASK')) {
    type = 'ask';
  } else if (trimmed.includes('DESCRIBE')) {
    type = 'describe';
  }

  // Extract prefixes
  const prefixes = {};
  const prefixMatches = query.matchAll(/PREFIX\s+(\w+):\s*<([^>]+)>/gi);
  for (const match of prefixMatches) {
    prefixes[match[1]] = match[2];
  }

  // Extract variables
  const variables = [];
  const varMatches = query.match(/\?(\w+)/g);
  if (varMatches) {
    const uniqueVars = new Set(varMatches.map(v => v.slice(1)));
    variables.push(...uniqueVars);
  }

  // Extract triple patterns from WHERE clause
  const whereMatch = query.match(/WHERE\s*\{([^}]+)\}/is);
  const patterns = [];

  if (whereMatch) {
    const whereClause = whereMatch[1];
    // Enhanced pattern extraction: subject predicate object
    // Handles URIs with : and / characters
    const patternMatches = whereClause.matchAll(
      /(<?[?:\w/._#-]+>?)\s+(<?[?:\w/._#:-]+>?)\s+(<?[?:\w/._#"@-]+>?)/g
    );

    for (const match of patternMatches) {
      const [, subject, predicate, object] = match;

      // Skip if this looks like a FILTER or other clause
      if (subject.toUpperCase() === 'FILTER' || predicate.toUpperCase() === 'FILTER') {
        continue;
      }

      // Calculate selectivity based on specificity
      // More specific terms (URIs, literals) have higher selectivity
      const selectivity = calculateSelectivity(subject, predicate, object);

      patterns.push({
        subject: subject.trim(),
        predicate: predicate.trim(),
        object: object.trim(),
        selectivity,
      });
    }
  }

  // Identify joins (shared variables across patterns)
  const joins = identifyJoins(patterns);

  // Extract FILTER clauses
  const filters = [];
  const filterMatches = query.matchAll(/FILTER\s*\(([^)]+)\)/gi);
  for (const match of filterMatches) {
    filters.push(match[1].trim());
  }

  // Estimate result count (heuristic-based)
  const estimatedResults = estimateResultCount(patterns, joins, filters);

  // Find critical path (most selective patterns)
  const criticalPath = findCriticalPath(patterns);

  const result = {
    type,
    patterns,
    variables,
    joins,
    filters,
    prefixes,
    estimatedResults,
    criticalPath,
  };

  return AnalysisResultSchema.parse(result);
}

/**
 * Calculate selectivity for a triple pattern
 * @param {string} subject - Subject term
 * @param {string} predicate - Predicate term
 * @param {string} object - Object term
 * @returns {number} Selectivity score 0-1
 */
function calculateSelectivity(subject, predicate, object) {
  let score = 0;

  // Subject selectivity
  if (subject.startsWith('<')) {
    score += 0.4; // URI is specific
  } else if (!subject.startsWith('?')) {
    score += 0.3; // Literal/prefixed
  } else {
    score += 0.1; // Variable is least specific
  }

  // Predicate selectivity
  if (predicate.startsWith('<')) {
    score += 0.3;
  } else if (!predicate.startsWith('?')) {
    score += 0.2;
  } else {
    score += 0.05;
  }

  // Object selectivity
  if (object.startsWith('<') || object.startsWith('"')) {
    score += 0.3; // URI or literal
  } else if (!object.startsWith('?')) {
    score += 0.2;
  } else {
    score += 0.05;
  }

  return Math.min(score, 1.0);
}

/**
 * Identify join points (shared variables)
 * @param {Array} patterns - Triple patterns
 * @returns {Array} Join specifications
 */
function identifyJoins(patterns) {
  const variableOccurrences = new Map();

  patterns.forEach((pattern, idx) => {
    [pattern.subject, pattern.predicate, pattern.object].forEach(term => {
      if (term.startsWith('?')) {
        if (!variableOccurrences.has(term)) {
          variableOccurrences.set(term, []);
        }
        variableOccurrences.get(term).push(idx);
      }
    });
  });

  const joins = [];
  for (const [variable, patternIndices] of variableOccurrences) {
    if (patternIndices.length > 1) {
      joins.push({
        variable,
        patterns: patternIndices,
      });
    }
  }

  return joins;
}

/**
 * Estimate result count (heuristic)
 * @param {Array} patterns - Triple patterns
 * @param {Array} joins - Join specifications
 * @param {Array} filters - FILTER clauses
 * @returns {number} Estimated result count
 */
function estimateResultCount(patterns, joins, filters) {
  // Base estimate from pattern selectivity
  let estimate = 1000;

  if (patterns.length === 0) {
    return 0;
  }

  // Reduce by average selectivity
  const avgSelectivity = patterns.reduce((sum, p) => sum + p.selectivity, 0) / patterns.length;
  estimate *= avgSelectivity;

  // Joins reduce results (more joins = fewer results)
  estimate /= Math.pow(joins.length + 1, 0.5);

  // Filters reduce results
  estimate /= Math.pow(filters.length + 1, 0.3);

  return Math.max(1, Math.round(estimate));
}

/**
 * Find critical path (most selective patterns in execution order)
 * @param {Array} patterns - Triple patterns
 * @returns {Array<number>} Pattern indices in critical path
 */
function findCriticalPath(patterns) {
  // Sort patterns by selectivity (descending)
  const indexed = patterns.map((p, idx) => ({ idx, selectivity: p.selectivity }));
  indexed.sort((a, b) => b.selectivity - a.selectivity);

  // Take top 20% or at least 1 pattern
  const criticalCount = Math.max(1, Math.ceil(patterns.length * 0.2));
  return indexed.slice(0, criticalCount).map(p => p.idx);
}

/**
 * Estimate query complexity (1-10 scale)
 * @param {string} query - SPARQL query string
 * @returns {number} Complexity score 1-10
 *
 * @throws {TypeError} If query is not a string
 *
 * @example
 * const complexity = estimateComplexity(`SELECT * WHERE { ?s ?p ?o }`);
 * console.log('Complexity:', complexity); // Low complexity
 */
export function estimateComplexity(query) {
  if (typeof query !== 'string') {
    throw new TypeError('estimateComplexity: query must be a string');
  }

  const analysis = analyzeSparqlQuery(query);

  let score = 1;

  // Pattern count contributes to complexity
  score += Math.min(analysis.patterns.length * 0.5, 3);

  // Join count increases complexity
  score += Math.min(analysis.joins.length * 1.0, 3);

  // Filters add complexity
  score += Math.min(analysis.filters.length * 0.5, 2);

  // Variable count affects complexity
  score += Math.min(analysis.variables.length * 0.3, 1);

  return Math.min(Math.round(score), 10);
}

/**
 * Identify bottlenecks in query
 * @param {string} query - SPARQL query string
 * @returns {Array<Object>} Bottleneck descriptions
 *
 * @throws {TypeError} If query is not a string
 *
 * @example
 * const bottlenecks = identifyBottlenecks(query);
 * bottlenecks.forEach(b => console.log(b.type, b.description));
 */
export function identifyBottlenecks(query) {
  if (typeof query !== 'string') {
    throw new TypeError('identifyBottlenecks: query must be a string');
  }

  const analysis = analyzeSparqlQuery(query);
  const bottlenecks = [];

  // Too many patterns without specific predicates
  const lowSelectivityPatterns = analysis.patterns.filter(p => p.selectivity < 0.3);
  if (lowSelectivityPatterns.length > 3) {
    bottlenecks.push({
      type: 'low_selectivity',
      severity: 'high',
      description: `${lowSelectivityPatterns.length} patterns with low selectivity (high cardinality)`,
      recommendation: 'Add more specific predicates or URIs to patterns',
    });
  }

  // Many joins can be expensive
  if (analysis.joins.length > 5) {
    bottlenecks.push({
      type: 'many_joins',
      severity: 'medium',
      description: `${analysis.joins.length} joins detected`,
      recommendation: 'Consider breaking query into smaller queries or adding indexes',
    });
  }

  // Complex filters can be slow
  const complexFilters = analysis.filters.filter(f => f.length > 50 || f.includes('REGEX'));
  if (complexFilters.length > 0) {
    bottlenecks.push({
      type: 'complex_filter',
      severity: 'medium',
      description: `${complexFilters.length} complex FILTER expressions`,
      recommendation: 'Move filter logic to WHERE patterns when possible',
    });
  }

  // Wildcard object in many patterns
  const wildcardObjects = analysis.patterns.filter(p => p.object.startsWith('?'));
  if (wildcardObjects.length / analysis.patterns.length > 0.7) {
    bottlenecks.push({
      type: 'many_wildcards',
      severity: 'low',
      description: 'High percentage of wildcard objects',
      recommendation: 'Specify object values when known',
    });
  }

  return bottlenecks;
}
