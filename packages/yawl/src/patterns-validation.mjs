/**
 * YAWL Workflow Patterns - Pattern Validation
 *
 * Comprehensive validation functions for workflow patterns.
 *
 * @module @unrdf/yawl/patterns-validation
 */

import { PATTERNS } from './patterns-registry.mjs';

/**
 * Validation result type
 * @typedef {Object} PatternValidationResult
 * @property {boolean} valid - Whether validation passed
 * @property {string[]} errors - Validation error messages
 * @property {string[]} warnings - Validation warnings
 */

// =============================================================================
// Pattern Validation
// =============================================================================

/**
 * Validate pattern context against pattern requirements
 *
 * @param {PatternDefinition} patternDef - Pattern definition
 * @param {Object} context - Pattern application context
 * @returns {PatternValidationResult}
 */
export function validatePatternContext(patternDef, context) {
  const errors = [];
  const warnings = [];

  // Validate based on split type
  if (patternDef.splitType !== 'none') {
    if (!context.sourceId) {
      errors.push(`Pattern ${patternDef.name} requires sourceId`);
    }

    // Validate branch count for split patterns
    const branchCount = context.targetIds?.length || context.branches?.length || 0;
    if (branchCount < patternDef.minBranches) {
      errors.push(
        `Pattern ${patternDef.name} requires at least ${patternDef.minBranches} branches, got ${branchCount}`
      );
    }
  }

  // Validate based on join type
  if (patternDef.joinType !== 'none') {
    if (!context.targetId) {
      errors.push(`Pattern ${patternDef.name} requires targetId`);
    }

    // Validate source count for join patterns
    const sourceCount = context.sourceIds?.length || 0;
    if (sourceCount < patternDef.minBranches) {
      errors.push(
        `Pattern ${patternDef.name} requires at least ${patternDef.minBranches} source tasks, got ${sourceCount}`
      );
    }
  }

  // Validate sequence pattern
  if (patternDef.name === 'Sequence') {
    if (!context.sourceId) {
      errors.push('Sequence pattern requires sourceId');
    }
    if (!context.targetId) {
      errors.push('Sequence pattern requires targetId');
    }
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

/**
 * Validate cardinality constraints for a pattern
 *
 * @param {string} patternName - Pattern name
 * @param {Object} config - Pattern configuration
 * @param {number} [config.branchCount] - Number of branches
 * @param {number} [config.sourceCount] - Number of source tasks
 * @returns {PatternValidationResult}
 */
export function validateCardinality(patternName, config) {
  const errors = [];
  const warnings = [];
  const pattern = PATTERNS[patternName];

  if (!pattern) {
    errors.push(`Unknown pattern: ${patternName}`);
    return { valid: false, errors, warnings };
  }

  const count = config.branchCount || config.sourceCount || 0;

  if (count < pattern.minBranches) {
    errors.push(
      `${patternName} requires at least ${pattern.minBranches} branches/sources, got ${count}`
    );
  }

  // Specific cardinality checks
  switch (patternName) {
    case 'PARALLEL_SPLIT':
    case 'SYNCHRONIZATION':
    case 'EXCLUSIVE_CHOICE':
    case 'SIMPLE_MERGE':
    case 'MULTI_CHOICE':
    case 'STRUCTURED_SYNC_MERGE':
    case 'DEFERRED_CHOICE':
      if (count < 2) {
        errors.push(`${patternName} requires at least 2 branches`);
      }
      break;

    case 'SEQUENCE':
    case 'ARBITRARY_CYCLE':
      if (count < 1 && pattern.minBranches > 0) {
        errors.push(`${patternName} requires at least 1 branch`);
      }
      break;
  }

  return { valid: errors.length === 0, errors, warnings };
}

/**
 * Validate split/join type compatibility
 *
 * @param {Object} config - Configuration to validate
 * @param {string} config.splitType - Split type of source task
 * @param {string} config.joinType - Join type of target task
 * @param {string} config.patternName - Pattern being applied
 * @returns {PatternValidationResult}
 */
export function validateSplitJoinMatch(config) {
  const errors = [];
  const warnings = [];

  const { splitType, joinType, patternName } = config;
  const pattern = PATTERNS[patternName];

  if (!pattern) {
    errors.push(`Unknown pattern: ${patternName}`);
    return { valid: false, errors, warnings };
  }

  // Validate split type if pattern requires one
  if (pattern.splitType !== 'none') {
    if (splitType && splitType !== pattern.splitType && splitType !== 'sequence') {
      warnings.push(
        `Pattern ${patternName} expects split type '${pattern.splitType}', got '${splitType}'`
      );
    }
  }

  // Validate join type if pattern requires one
  if (pattern.joinType !== 'none') {
    if (joinType && joinType !== pattern.joinType && joinType !== 'sequence') {
      warnings.push(
        `Pattern ${patternName} expects join type '${pattern.joinType}', got '${joinType}'`
      );
    }
  }

  // Check for common mismatches
  if (splitType === 'and' && joinType === 'xor') {
    warnings.push('AND-split with XOR-join may lose tokens (use AND-join for synchronization)');
  }

  if (splitType === 'or' && joinType === 'and') {
    warnings.push('OR-split with AND-join may deadlock (use OR-join for structured sync merge)');
  }

  return { valid: errors.length === 0, errors, warnings };
}

/**
 * Detect cycles in a set of flows
 *
 * @param {Array<{from: string, to: string, isCycle?: boolean}>} flows - Flow definitions
 * @param {Object} [options] - Detection options
 * @param {boolean} [options.allowMarkedCycles=true] - Allow flows marked as cycles
 * @returns {{hasCycle: boolean, cycleNodes: string[], allowedCycles: string[][]}}
 */
export function detectCycles(flows, options = {}) {
  const { allowMarkedCycles = true } = options;

  // Build adjacency list (excluding marked cycles if allowed)
  /** @type {Map<string, string[]>} */
  const graph = new Map();
  /** @type {string[][]} */
  const allowedCycles = [];

  for (const flow of flows) {
    // Track marked cycles separately
    if (flow.isCycle && allowMarkedCycles) {
      allowedCycles.push([flow.from, flow.to]);
      continue;
    }

    if (!graph.has(flow.from)) {
      graph.set(flow.from, []);
    }
    graph.get(flow.from).push(flow.to);
  }

  // DFS cycle detection
  /** @type {Set<string>} */
  const visited = new Set();
  /** @type {Set<string>} */
  const recStack = new Set();
  /** @type {string[]} */
  const cycleNodes = [];

  /**
   * @param {string} node
   * @returns {boolean}
   */
  function hasCycleDFS(node) {
    if (recStack.has(node)) {
      cycleNodes.push(node);
      return true;
    }
    if (visited.has(node)) {
      return false;
    }

    visited.add(node);
    recStack.add(node);

    const neighbors = graph.get(node) || [];
    for (const neighbor of neighbors) {
      if (hasCycleDFS(neighbor)) {
        if (cycleNodes[0] !== node) {
          cycleNodes.push(node);
        }
        return true;
      }
    }

    recStack.delete(node);
    return false;
  }

  // Check all nodes
  let hasCycle = false;
  for (const node of graph.keys()) {
    if (hasCycleDFS(node)) {
      hasCycle = true;
      break;
    }
  }

  return {
    hasCycle,
    cycleNodes: hasCycle ? cycleNodes.reverse() : [],
    allowedCycles,
  };
}

/**
 * Validate that flows have no invalid cycles
 *
 * @param {Array<{from: string, to: string, isCycle?: boolean}>} flows - Flow definitions
 * @returns {PatternValidationResult}
 */
export function validateNoCycles(flows) {
  const errors = [];
  const warnings = [];

  const result = detectCycles(flows, { allowMarkedCycles: true });

  if (result.hasCycle) {
    errors.push(
      `Invalid cycle detected: ${result.cycleNodes.join(' -> ')}. ` +
      `Mark cycle flows with isCycle: true or restructure workflow.`
    );
  }

  if (result.allowedCycles.length > 0) {
    warnings.push(
      `${result.allowedCycles.length} explicit cycle(s) found (marked with isCycle: true)`
    );
  }

  return { valid: errors.length === 0, errors, warnings };
}

/**
 * Comprehensive pattern validation
 *
 * @param {Object} config - Validation configuration
 * @param {string} config.patternName - Pattern name
 * @param {Object} [config.sourceTask] - Source task definition
 * @param {Object} [config.targetTask] - Target task definition
 * @param {Array<{from: string, to: string}>} [config.flows] - Flow definitions
 * @param {number} [config.branchCount] - Number of branches
 * @returns {PatternValidationResult}
 */
export function validatePattern(config) {
  const allErrors = [];
  const allWarnings = [];

  // Cardinality validation
  const cardinalityResult = validateCardinality(config.patternName, {
    branchCount: config.branchCount,
    sourceCount: config.branchCount,
  });
  allErrors.push(...cardinalityResult.errors);
  allWarnings.push(...cardinalityResult.warnings);

  // Split/join match validation
  if (config.sourceTask || config.targetTask) {
    const matchResult = validateSplitJoinMatch({
      splitType: config.sourceTask?.splitType,
      joinType: config.targetTask?.joinType,
      patternName: config.patternName,
    });
    allErrors.push(...matchResult.errors);
    allWarnings.push(...matchResult.warnings);
  }

  // Cycle validation
  if (config.flows && config.flows.length > 0) {
    const pattern = PATTERNS[config.patternName];
    if (pattern && !pattern.allowsCycles) {
      const cycleResult = validateNoCycles(config.flows);
      allErrors.push(...cycleResult.errors);
      allWarnings.push(...cycleResult.warnings);
    }
  }

  return {
    valid: allErrors.length === 0,
    errors: allErrors,
    warnings: allWarnings,
  };
}
