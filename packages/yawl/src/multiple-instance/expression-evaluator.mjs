/**
 * @file Expression Evaluator - Safe expression evaluation for YAWL
 * @module @unrdf/yawl/multiple-instance/expression-evaluator
 * @description
 * Evaluates count expressions for multiple instance tasks without arbitrary code execution.
 * Supports JSONPath-like expressions, SPARQL COUNT queries, and safe function evaluation.
 */

import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Expression type enumeration
 */
export const ExpressionType = Object.freeze({
  JSONPATH: 'jsonpath',
  SPARQL: 'sparql',
  FUNCTION: 'function',
  LITERAL: 'literal',
});

/**
 * Expression schema
 */
export const ExpressionSchema = z.object({
  type: z.enum([
    ExpressionType.JSONPATH,
    ExpressionType.SPARQL,
    ExpressionType.FUNCTION,
    ExpressionType.LITERAL,
  ]),
  expression: z.string(),
  context: z.any().optional(),
});

/**
 * Evaluation result schema
 */
export const EvaluationResultSchema = z.object({
  count: z.number().int().nonnegative(),
  type: z.string(),
  expression: z.string(),
  evaluatedAt: z.bigint(),
  proof: z.object({
    inputHash: z.string(),
    resultHash: z.string(),
    method: z.string(),
  }).optional(),
});

// =============================================================================
// JSONPath-like Evaluator
// =============================================================================

/**
 * Evaluate JSONPath-like expression
 * Supports:
 * - $.items - Get items array
 * - $.items.length - Get array length
 * - count($.items) - Count items in array
 * - $.data.users.length - Nested property access
 *
 * @param {string} expression - JSONPath expression
 * @param {Object} data - Input data
 * @returns {number} Count result
 * @throws {Error} If expression is invalid or evaluation fails
 */
export function evaluateJSONPath(expression, data) {
  // Sanitize expression - only allow safe characters
  if (!/^[a-zA-Z0-9.$_\s()]+$/.test(expression)) {
    throw new Error(`Invalid JSONPath expression: ${expression}`);
  }

  // Handle count() function wrapper
  const countMatch = expression.match(/^count\((.*)\)$/);
  if (countMatch) {
    const innerExpr = countMatch[1];
    const result = evaluateJSONPath(innerExpr, data);
    return result;
  }

  // Handle $.path.to.property
  if (expression.startsWith('$.')) {
    const path = expression.slice(2); // Remove '$.'
    const value = getNestedProperty(data, path);

    if (value === null || value === undefined) {
      return 0;
    }

    // If it's an array, return length
    if (Array.isArray(value)) {
      return value.length;
    }

    // If it's a number, return it
    if (typeof value === 'number') {
      return Math.floor(value);
    }

    // If it's an object, count keys
    if (typeof value === 'object') {
      return Object.keys(value).length;
    }

    // Otherwise, it exists so count as 1
    return 1;
  }

  // Handle simple property access without $.
  const value = getNestedProperty(data, expression);
  if (Array.isArray(value)) {
    return value.length;
  }
  if (typeof value === 'number') {
    return Math.floor(value);
  }

  throw new Error(`Cannot evaluate JSONPath expression: ${expression}`);
}

/**
 * Get nested property from object using dot notation
 * @param {Object} obj - Source object
 * @param {string} path - Dot-separated path
 * @returns {*} Value at path or null
 */
function getNestedProperty(obj, path) {
  if (!path) return obj;

  const parts = path.split('.');
  let current = obj;

  for (const part of parts) {
    if (current === null || current === undefined) {
      return null;
    }
    current = current[part];
  }

  return current;
}

// =============================================================================
// SPARQL COUNT Evaluator
// =============================================================================

/**
 * Evaluate SPARQL COUNT query
 * Simplified evaluation - extracts count from SPARQL result
 *
 * @param {string} query - SPARQL query with COUNT
 * @param {Object} data - Query context with store or results
 * @returns {Promise<number>} Count result
 * @throws {Error} If query is invalid
 */
export async function evaluateSPARQL(query, data) {
  // Validate SPARQL query structure
  if (!query.includes('COUNT')) {
    throw new Error('SPARQL query must include COUNT');
  }

  // If data has a store, execute query
  if (data.store && typeof data.store.query === 'function') {
    const results = await data.store.query(query);

    // Extract count from first binding
    if (results && results.length > 0) {
      const firstResult = results[0];
      // Look for count binding
      for (const [key, value] of Object.entries(firstResult)) {
        if (key.toLowerCase().includes('count')) {
          return parseInt(value.value, 10);
        }
      }
    }
    return 0;
  }

  // If data has pre-computed count result
  if (data.count !== undefined) {
    return typeof data.count === 'number' ? data.count : parseInt(data.count, 10);
  }

  throw new Error('SPARQL evaluation requires store or count in context');
}

// =============================================================================
// Function Evaluator
// =============================================================================

/**
 * Evaluate safe function expression
 * Only allows predefined safe functions
 *
 * @param {string} expression - Function name
 * @param {Object} data - Input data
 * @returns {number} Count result
 * @throws {Error} If function is not allowed
 */
export function evaluateFunction(expression, data) {
  const safeFunctions = {
    countItems: (d) => Array.isArray(d.items) ? d.items.length : 0,
    countKeys: (d) => Object.keys(d).length,
    countAll: (d) => {
      if (Array.isArray(d)) return d.length;
      if (typeof d === 'object') return Object.keys(d).length;
      return 0;
    },
  };

  const fn = safeFunctions[expression];
  if (!fn) {
    throw new Error(`Function ${expression} is not allowed. Allowed: ${Object.keys(safeFunctions).join(', ')}`);
  }

  return fn(data);
}

// =============================================================================
// Unified Evaluator
// =============================================================================

/**
 * Evaluate expression and return count
 * Automatically detects expression type
 *
 * @param {string|Object} expression - Expression string or object
 * @param {Object} data - Input data
 * @returns {Promise<Object>} Evaluation result with count and metadata
 * @throws {Error} If evaluation fails
 */
export async function evaluateExpression(expression, data) {
  const now = BigInt(Date.now()) * 1_000_000n;

  // Parse expression if it's a string
  let expr;
  if (typeof expression === 'string') {
    expr = detectExpressionType(expression);
  } else {
    expr = ExpressionSchema.parse(expression);
  }

  let count;
  let method;

  try {
    switch (expr.type) {
      case ExpressionType.JSONPATH:
        count = evaluateJSONPath(expr.expression, data);
        method = 'jsonpath';
        break;

      case ExpressionType.SPARQL:
        count = await evaluateSPARQL(expr.expression, data);
        method = 'sparql';
        break;

      case ExpressionType.FUNCTION:
        count = evaluateFunction(expr.expression, data);
        method = 'function';
        break;

      case ExpressionType.LITERAL:
        count = parseInt(expr.expression, 10);
        if (isNaN(count) || count < 0) {
          throw new Error(`Invalid literal count: ${expr.expression}`);
        }
        method = 'literal';
        break;

      default:
        throw new Error(`Unknown expression type: ${expr.type}`);
    }

    // Validate count is a non-negative integer
    if (!Number.isInteger(count) || count < 0) {
      throw new Error(`Expression must evaluate to non-negative integer, got: ${count}`);
    }

    return {
      count,
      type: expr.type,
      expression: expr.expression,
      evaluatedAt: now,
      proof: {
        inputHash: simpleHash(JSON.stringify(data)),
        resultHash: simpleHash(count.toString()),
        method,
      },
    };
  } catch (error) {
    throw new Error(`Expression evaluation failed: ${error.message}`);
  }
}

/**
 * Detect expression type from string
 * @param {string} expression - Expression string
 * @returns {Object} Expression object with type
 */
function detectExpressionType(expression) {
  // SPARQL: starts with SELECT or contains COUNT(
  if (expression.trim().toUpperCase().startsWith('SELECT') ||
      expression.includes('COUNT(') && expression.includes('?')) {
    return { type: ExpressionType.SPARQL, expression };
  }

  // JSONPath: starts with $. or contains count(
  if (expression.startsWith('$.') || expression.startsWith('count(')) {
    return { type: ExpressionType.JSONPATH, expression };
  }

  // Function: valid identifier
  if (/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(expression)) {
    return { type: ExpressionType.FUNCTION, expression };
  }

  // Literal: numeric (including negative)
  if (/^-?\d+$/.test(expression)) {
    return { type: ExpressionType.LITERAL, expression };
  }

  // Default to JSONPath
  return { type: ExpressionType.JSONPATH, expression };
}

/**
 * Simple hash function for proof generation
 * @param {string} input - Input string
 * @returns {string} Hash
 */
function simpleHash(input) {
  let hash = 0;
  for (let i = 0; i < input.length; i++) {
    const char = input.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32-bit integer
  }
  return Math.abs(hash).toString(16).padStart(8, '0');
}
