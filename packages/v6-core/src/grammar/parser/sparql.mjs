/**
 * @fileoverview SPARQL grammar parser and complexity estimator
 * @module @unrdf/v6-core/grammar/parser/sparql
 * @version 6.0.0-alpha.1
 */

import { GRAMMAR_TYPES } from '../parser.mjs';
import { countASTNodes } from './utils.mjs';

/**
 * Parse SPARQL query to AST
 * @param {string} input - SPARQL query
 * @returns {Object} Parse result
 */
export function parseSPARQL(input) {
  // Basic SPARQL parsing - detects query type, patterns, complexity
  // In production, use full SPARQL 1.1 parser (e.g., sparqljs)

  const ast = {
    type: 'sparql',
    queryType: detectSPARQLQueryType(input),
    prefixes: extractPrefixes(input),
    patterns: extractTriplePatterns(input),
    filters: extractFilters(input),
    modifiers: extractModifiers(input),
  };

  const complexity = estimateSPARQLComplexity(ast);
  ast.complexity = complexity; // Attach complexity to AST

  return {
    success: true,
    grammarType: GRAMMAR_TYPES.SPARQL,
    ast,
    complexity,
  };
}

/**
 * Estimate SPARQL query complexity
 * @param {Object} ast - SPARQL AST
 * @returns {Object} Complexity bounds
 */
export function estimateSPARQLComplexity(ast) {
  const triplePatterns = ast.patterns?.length || 0;
  const filters = ast.filters?.length || 0;
  const joinDepth = estimateJoinDepth(ast.patterns);

  // Rough complexity estimation (O(n^joinDepth) worst case)
  const estimatedTimeMs = Math.min(
    100 + (triplePatterns * 10) + (filters * 50) + (joinDepth * 100),
    5000 // Cap at 5s (default timeout from CLAUDE.md)
  );

  return {
    estimatedTimeMs,
    astNodeCount: countASTNodes(ast),
    maxDepth: joinDepth,
    triplePatterns,
    joinDepth,
    filterComplexity: filters,
  };
}

/**
 * Detect SPARQL query type
 * @param {string} query - SPARQL query
 * @returns {string} Query type
 */
function detectSPARQLQueryType(query) {
  const trimmed = query.trim().toUpperCase();
  if (trimmed.includes('SELECT')) return 'SELECT';
  if (trimmed.includes('CONSTRUCT')) return 'CONSTRUCT';
  if (trimmed.includes('ASK')) return 'ASK';
  if (trimmed.includes('DESCRIBE')) return 'DESCRIBE';
  return 'UNKNOWN';
}

/**
 * Extract SPARQL prefixes
 * @param {string} query - SPARQL query
 * @returns {Object} Prefix mappings
 */
function extractPrefixes(query) {
  const prefixes = {};
  const regex = /PREFIX\s+(\w+):\s*<([^>]+)>/gi;
  let match;

  while ((match = regex.exec(query)) !== null) {
    prefixes[match[1]] = match[2];
  }

  return prefixes;
}

/**
 * Extract triple patterns (simplified)
 * @param {string} query - SPARQL query
 * @returns {Array} Triple patterns
 */
function extractTriplePatterns(query) {
  // Simplified - count lines with triple pattern syntax
  const whereMatch = query.match(/WHERE\s*\{([^}]+)\}/i);
  if (!whereMatch) return [];

  const whereClause = whereMatch[1];
  const lines = whereClause.split(/[.\n]/).filter(l => l.trim().length > 0);

  return lines.map(line => ({ pattern: line.trim() }));
}

/**
 * Extract FILTER clauses
 * @param {string} query - SPARQL query
 * @returns {Array} Filters
 */
function extractFilters(query) {
  const filters = [];
  const regex = /FILTER\s*\(([^)]+)\)/gi;
  let match;

  while ((match = regex.exec(query)) !== null) {
    filters.push(match[1]);
  }

  return filters;
}

/**
 * Extract query modifiers (LIMIT, ORDER BY, etc.)
 * @param {string} query - SPARQL query
 * @returns {Object} Modifiers
 */
function extractModifiers(query) {
  const modifiers = {};

  const limitMatch = query.match(/LIMIT\s+(\d+)/i);
  if (limitMatch) modifiers.limit = parseInt(limitMatch[1], 10);

  const offsetMatch = query.match(/OFFSET\s+(\d+)/i);
  if (offsetMatch) modifiers.offset = parseInt(offsetMatch[1], 10);

  modifiers.hasOrderBy = /ORDER BY/i.test(query);
  modifiers.hasGroupBy = /GROUP BY/i.test(query);

  return modifiers;
}

/**
 * Estimate join depth from patterns
 * @param {Array} patterns - Triple patterns
 * @returns {number} Join depth
 */
function estimateJoinDepth(patterns) {
  if (!patterns || patterns.length === 0) return 0;

  // Simplified - assume one join per pattern
  return Math.min(patterns.length, 10);
}
