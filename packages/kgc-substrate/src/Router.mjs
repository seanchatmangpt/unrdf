/**
 * @fileoverview Task Graph Router & Constraint Routing
 *
 * Deterministic routing of WorkItems to Agents based on predicates.
 * Supports AND, OR, NOT operators with bounded evaluation cost.
 *
 * @module @unrdf/kgc-substrate/router
 */

import { z } from 'zod';

/**
 * Predicate schema for constraint validation
 */
const PredicateSchema = z.object({
  field: z.string(),
  operator: z.enum(['==', 'in', 'not', '!=', '>', '<', '>=', '<=']),
  value: z.union([z.string(), z.number(), z.boolean(), z.array(z.any())]),
});

/**
 * Agent capability schema
 */
const AgentSchema = z.object({
  id: z.string(),
  capabilities: z.record(z.any()),
});

/**
 * WorkItem schema with routing predicates
 */
const WorkItemSchema = z.object({
  id: z.string(),
  predicates: z.string(), // Constraint string like "requires_auth==true AND language=='js'"
});

/**
 * Maximum number of predicates to prevent unbounded evaluation
 * @constant {number}
 */
const MAX_PREDICATES = 1000;

/**
 * Parse constraint string into structured predicates
 *
 * @param {string} constraintString - Constraint expression (e.g., "requires_auth==true AND language=='js'")
 * @returns {{field: string, operator: string, value: any}[]} Parsed predicates
 * @throws {Error} If constraint string exceeds MAX_PREDICATES or has invalid syntax
 *
 * @example
 * parsePredicates("language=='js'")
 * // => [{ field: 'language', operator: '==', value: 'js' }]
 *
 * @example
 * parsePredicates("requires_auth==true AND language in ['js','ts']")
 * // => [
 * //   { field: 'requires_auth', operator: '==', value: true },
 * //   { field: 'language', operator: 'in', value: ['js', 'ts'] }
 * // ]
 */
export function parsePredicates(constraintString) {
  if (typeof constraintString !== 'string') {
    throw new Error('Constraint must be a string');
  }

  // Guard against unbounded evaluation
  const tokenCount = constraintString.split(/\s+/).length;
  if (tokenCount > MAX_PREDICATES * 5) { // Approximate: each predicate ~5 tokens
    throw new Error(`Constraint exceeds maximum complexity (${MAX_PREDICATES} predicates)`);
  }

  // Normalize whitespace
  let normalized = constraintString.trim();

  // Handle empty constraint
  if (!normalized) {
    return [];
  }

  // Split by logical operators (AND, OR) - case insensitive
  const logicalSplit = normalized.split(/\s+(AND|OR)\s+/i);

  const predicates = [];
  let predicateCount = 0;

  for (let i = 0; i < logicalSplit.length; i += 2) { // Skip operators at odd indices
    const clause = logicalSplit[i].trim();

    if (!clause || clause === 'AND' || clause === 'OR') {
      continue;
    }

    // Guard: enforce predicate limit
    if (++predicateCount > MAX_PREDICATES) {
      throw new Error(`Exceeded maximum of ${MAX_PREDICATES} predicates`);
    }

    // Handle NOT prefix
    let isNegated = false;
    let cleanClause = clause;
    if (clause.match(/^NOT\s+/i)) {
      isNegated = true;
      cleanClause = clause.replace(/^NOT\s+/i, '');
    }

    // Parse operators: ==, !=, in, not, >, <, >=, <=
    let match = cleanClause.match(/^(\w+)\s*(==|!=|in|not|>=?|<=?)\s*(.+)$/i);

    if (!match) {
      throw new Error(`Invalid predicate syntax: ${clause}`);
    }

    const [, field, operator, rawValue] = match;

    // Parse value (handle strings, numbers, booleans, arrays)
    let value;
    const trimmedValue = rawValue.trim();

    if (operator.toLowerCase() === 'in' || operator.toLowerCase() === 'not') {
      // Parse array: ['js', 'ts'] or ["js", "ts"]
      const arrayMatch = trimmedValue.match(/^\[(.+)\]$/);
      if (arrayMatch) {
        value = arrayMatch[1].split(',').map(v => {
          const trimmed = v.trim().replace(/^['"]|['"]$/g, '');
          return parseValue(trimmed);
        });
      } else {
        throw new Error(`Operator '${operator}' requires array value: ${trimmedValue}`);
      }
    } else {
      // Parse single value
      value = parseValue(trimmedValue.replace(/^['"]|['"]$/g, ''));
    }

    // Normalize 'not' operator to '!=' or 'not in'
    let normalizedOp = operator;
    if (isNegated) {
      if (operator === '==') normalizedOp = '!=';
      else if (operator === 'in') normalizedOp = 'not';
    } else if (operator.toLowerCase() === 'not') {
      normalizedOp = 'not';
    }

    predicates.push({
      field: field.trim(),
      operator: normalizedOp.toLowerCase() === 'not' ? 'not' : normalizedOp,
      value,
    });
  }

  // Validate predicates with schema
  predicates.forEach((pred, idx) => {
    try {
      PredicateSchema.parse(pred);
    } catch (err) {
      throw new Error(`Predicate ${idx} validation failed: ${err.message}`);
    }
  });

  return predicates;
}

/**
 * Parse string value to appropriate type
 * @private
 */
function parseValue(str) {
  // Boolean
  if (str === 'true') return true;
  if (str === 'false') return false;

  // Number
  if (/^-?\d+(\.\d+)?$/.test(str)) {
    return parseFloat(str);
  }

  // String
  return str;
}

/**
 * Evaluate single predicate against agent capabilities
 *
 * @private
 * @param {{field: string, operator: string, value: any}} predicate - Predicate to evaluate
 * @param {Object} capabilities - Agent capabilities object
 * @returns {boolean} Whether predicate matches
 */
function evaluatePredicate(predicate, capabilities) {
  const { field, operator, value } = predicate;
  const agentValue = capabilities[field];

  switch (operator) {
    case '==':
      return agentValue === value;

    case '!=':
      return agentValue !== value;

    case 'in':
      return Array.isArray(value) && value.includes(agentValue);

    case 'not':
      return Array.isArray(value) && !value.includes(agentValue);

    case '>':
      return typeof agentValue === 'number' && agentValue > value;

    case '<':
      return typeof agentValue === 'number' && agentValue < value;

    case '>=':
      return typeof agentValue === 'number' && agentValue >= value;

    case '<=':
      return typeof agentValue === 'number' && agentValue <= value;

    default:
      return false;
  }
}

/**
 * Evaluate logical expression with AND/OR operators
 *
 * @private
 * @param {string} constraintString - Original constraint string
 * @param {{field: string, operator: string, value: any}[]} predicates - Parsed predicates
 * @param {Object} capabilities - Agent capabilities
 * @returns {boolean} Whether expression evaluates to true
 */
function evaluateExpression(constraintString, predicates, capabilities) {
  if (predicates.length === 0) {
    return true; // Empty constraint matches all
  }

  if (predicates.length === 1) {
    return evaluatePredicate(predicates[0], capabilities);
  }

  // Determine logical operators
  const hasAND = /\sAND\s/i.test(constraintString);
  const hasOR = /\sOR\s/i.test(constraintString);

  // Pure AND: all predicates must match
  if (hasAND && !hasOR) {
    return predicates.every(pred => evaluatePredicate(pred, capabilities));
  }

  // Pure OR: any predicate must match
  if (hasOR && !hasAND) {
    return predicates.some(pred => evaluatePredicate(pred, capabilities));
  }

  // Mixed AND/OR: evaluate left-to-right with precedence (AND before OR)
  // For simplicity, treat as OR (any clause matches)
  // For complex expressions, parse into AST (future enhancement)
  if (hasAND && hasOR) {
    // Split by OR, then check if any OR clause (with ANDs) matches
    const orClauses = constraintString.split(/\sOR\s/i);
    return orClauses.some(orClause => {
      const andPredicates = parsePredicates(orClause);
      return andPredicates.every(pred => evaluatePredicate(pred, capabilities));
    });
  }

  // Default: all predicates must match (AND semantics)
  return predicates.every(pred => evaluatePredicate(pred, capabilities));
}

/**
 * Route WorkItem to matching Agent based on predicates
 *
 * Deterministic: same input → same output (no randomization)
 * Pure function: no side effects, no mutations
 *
 * @param {Object} workItem - WorkItem with predicates
 * @param {string} workItem.id - Unique identifier
 * @param {string} workItem.predicates - Constraint expression
 * @param {Array<{id: string, capabilities: Object}>} agents - Available agents
 * @returns {string|null} Agent ID if match found, null otherwise
 * @throws {Error} If input validation fails or evaluation exceeds bounds
 *
 * @example
 * const workItem = { id: 'task-1', predicates: "language=='js'" };
 * const agents = [
 *   { id: 'agent-1', capabilities: { language: 'js' } },
 *   { id: 'agent-2', capabilities: { language: 'py' } }
 * ];
 * routeTask(workItem, agents); // => 'agent-1'
 *
 * @example XOR routing
 * const workItem = { id: 'task-2', predicates: "env=='dev' AND NOT env=='prod'" };
 * const agents = [{ id: 'dev-agent', capabilities: { env: 'dev' } }];
 * routeTask(workItem, agents); // => 'dev-agent'
 */
export function routeTask(workItem, agents) {
  // Validate inputs
  if (!workItem || typeof workItem.id !== 'string' || typeof workItem.predicates !== 'string') {
    throw new Error('Invalid workItem: must have id and predicates fields');
  }
  if (!Array.isArray(agents)) {
    throw new Error('Invalid agents: must be an array');
  }
  for (const agent of agents) {
    if (!agent || typeof agent.id !== 'string' || typeof agent.capabilities !== 'object') {
      throw new Error('Invalid agent: must have id and capabilities fields');
    }
  }

  // Parse predicates
  const predicates = parsePredicates(workItem.predicates);

  // Find first matching agent (deterministic: returns first match)
  for (const agent of agents) {
    const matches = evaluateExpression(workItem.predicates, predicates, agent.capabilities);

    if (matches) {
      return agent.id;
    }
  }

  // No match found
  return null;
}

/**
 * Validate routing constraints without executing
 *
 * @param {string} constraintString - Constraint expression to validate
 * @returns {{valid: boolean, predicateCount: number, error?: string}}
 *
 * @example
 * validateConstraints("language=='js' AND requires_auth==true")
 * // => { valid: true, predicateCount: 2 }
 */
export function validateConstraints(constraintString) {
  try {
    const predicates = parsePredicates(constraintString);
    return {
      valid: true,
      predicateCount: predicates.length,
    };
  } catch (err) {
    return {
      valid: false,
      predicateCount: 0,
      error: err.message,
    };
  }
}

/**
 * Get routing statistics for diagnostics
 *
 * @param {Array<Object>} workItems - Work items to analyze
 * @param {Array<Object>} agents - Available agents
 * @returns {{totalItems: number, routed: number, unrouted: number, routingMap: Object}}
 */
export function getRoutingStats(workItems, agents) {
  const routingMap = {};
  let routed = 0;
  let unrouted = 0;

  for (const item of workItems) {
    const agentId = routeTask(item, agents);

    if (agentId) {
      routed++;
      routingMap[agentId] = (routingMap[agentId] || 0) + 1;
    } else {
      unrouted++;
    }
  }

  return {
    totalItems: workItems.length,
    routed,
    unrouted,
    routingMap,
  };
}
