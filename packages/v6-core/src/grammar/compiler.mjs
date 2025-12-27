/**
 * @fileoverview AOT Grammar Compiler - V6 Grammar Closure
 *
 * Compile parsed grammar to runtime form with complexity gating.
 * Reject overly complex constructs with denial receipts (never crashes).
 *
 * @module @unrdf/v6-core/grammar/compiler
 * @version 6.0.0-alpha.1
 */

import { z } from 'zod';
import { createHash } from 'crypto';

/**
 * Complexity bounds per grammar (from GRAMMAR_MATRIX.md)
 */
export const COMPLEXITY_BOUNDS = {
  sparql: {
    maxTriplePatterns: 1000,
    maxJoinDepth: 10,
    maxFilterComplexity: 100,
    maxAggregations: 50,
    maxSubqueries: 5,
    estimatedTimeMs: 5000,
  },
  shacl: {
    maxShapesDepth: 20,
    maxPropertyPaths: 100,
    maxTargetNodes: 10000,
    maxValidationRules: 500,
    estimatedTimeMs: 10000,
  },
  n3: {
    maxRuleDepth: 15,
    maxFormulaSize: 500,
    maxLogicQuantifiers: 50,
    maxBuiltinCalls: 200,
    estimatedTimeMs: 15000,
  },
  owl: {
    maxOWLAxioms: 10000,
    maxClassHierarchyDepth: 50,
    maxPropertyChainLength: 10,
    maxReasoningIterations: 1000,
    estimatedTimeMs: 20000,
  },
  shex: {
    maxShapeDepth: 30,
    maxTripleConstraints: 200,
    maxShapeReferences: 100,
    maxRegexComplexity: 50,
    estimatedTimeMs: 8000,
  },
};

/**
 * Compile options schema
 */
const CompileOptionsSchema = z.object({
  strict: z.boolean().optional().default(true),
  customBounds: z.record(z.any()).optional(),
  emitReceipts: z.boolean().optional().default(true),
}).optional();

/**
 * Compile result schema
 */
const CompileResultSchema = z.object({
  success: z.boolean(),
  compiled: z.any().optional(),
  denial: z.any().optional(),
  compileReceipt: z.object({
    timestamp: z.string(),
    compileTimeMs: z.number(),
    grammarType: z.string(),
    decision: z.enum(['ACCEPT', 'REJECT']),
    reason: z.string().optional(),
  }),
  denialReceipt: z.object({
    timestamp: z.string(),
    merkleProof: z.string(),
    deniedInput: z.string(),
    reason: z.string(),
    details: z.record(z.any()),
  }).optional(),
});

/**
 * Compile grammar with AOT complexity gating
 *
 * **Guarantee**: Returns { denial, denialReceipt } instead of throwing.
 *
 * @param {Object} ast - Parsed AST from parser.parseGrammar()
 * @param {Object} [options] - Compile options
 * @param {boolean} [options.strict=true] - Strict bounds enforcement
 * @param {Object} [options.customBounds] - Override default bounds
 * @param {boolean} [options.emitReceipts=true] - Emit receipts
 * @returns {Object} Compile result { compiled, compileReceipt } OR { denial, denialReceipt }
 *
 * @example
 * const result = compileGrammar(ast, { strict: true });
 * if (result.success) {
 *   // Execute result.compiled
 * } else {
 *   // Handle result.denialReceipt
 * }
 */
export function compileGrammar(ast, options = {}) {
  const startTime = Date.now();
  const opts = CompileOptionsSchema.parse(options);

  // Validate AST
  if (!ast || !ast.type || !ast.complexity) {
    return createDenialResult(
      'INVALID_AST',
      'AST must have type and complexity fields',
      ast,
      startTime,
      opts
    );
  }

  // Get bounds for this grammar type
  const bounds = opts.customBounds || COMPLEXITY_BOUNDS[ast.type] || {};

  // Check if complexity exceeds bounds
  const rejection = rejectIfTooComplex(ast, bounds, opts);
  if (rejection) {
    return {
      ...rejection,
      compileReceipt: {
        timestamp: new Date().toISOString(),
        compileTimeMs: Date.now() - startTime,
        grammarType: ast.type,
        decision: 'REJECT',
        reason: rejection.reason,
      },
    };
  }

  // Compile AST to runtime form
  try {
    const compiled = compileASTToRuntime(ast);

    return {
      success: true,
      compiled,
      compileReceipt: {
        timestamp: new Date().toISOString(),
        compileTimeMs: Date.now() - startTime,
        grammarType: ast.type,
        decision: 'ACCEPT',
      },
    };
  } catch (error) {
    // Compilation failed (shouldn't happen with valid AST)
    return createDenialResult(
      'COMPILATION_ERROR',
      error.message,
      ast,
      startTime,
      opts
    );
  }
}

/**
 * Check if AST complexity exceeds bounds - returns denial if so
 *
 * @param {Object} ast - Parsed AST
 * @param {Object} bounds - Complexity bounds
 * @param {Object} opts - Compile options
 * @returns {Object|null} Denial result or null if acceptable
 */
export function rejectIfTooComplex(ast, bounds, opts = {}) {
  const { complexity } = ast;

  // Check estimated time
  if (bounds.estimatedTimeMs && complexity.estimatedTimeMs > bounds.estimatedTimeMs) {
    return {
      success: false,
      denial: {
        reason: 'COMPLEXITY_EXCEEDED',
        constraint: 'estimatedTimeMs',
        limit: bounds.estimatedTimeMs,
        actual: complexity.estimatedTimeMs,
      },
      denialReceipt: opts.emitReceipts !== false ? emitDenialReceipt(ast, {
        reason: 'COMPLEXITY_EXCEEDED',
        constraint: 'estimatedTimeMs',
        limit: bounds.estimatedTimeMs,
        actual: complexity.estimatedTimeMs,
        suggestion: 'Simplify query or split into smaller operations',
      }) : undefined,
    };
  }

  // Check grammar-specific bounds
  const grammarChecks = {
    sparql: () => checkSPARQLBounds(complexity, bounds),
    shacl: () => checkSHACLBounds(complexity, bounds),
    n3: () => checkN3Bounds(complexity, bounds),
    owl: () => checkOWLBounds(complexity, bounds),
    shex: () => checkShExBounds(complexity, bounds),
  };

  const checker = grammarChecks[ast.type];
  if (checker) {
    const violation = checker();
    if (violation) {
      return {
        success: false,
        denial: violation,
        denialReceipt: opts.emitReceipts !== false ? emitDenialReceipt(ast, violation) : undefined,
      };
    }
  }

  return null; // Complexity acceptable
}

/**
 * Emit denial receipt with Merkle proof
 *
 * @param {Object} ast - Denied AST
 * @param {Object} details - Denial details
 * @returns {Object} Denial receipt
 */
export function emitDenialReceipt(ast, details) {
  const timestamp = new Date().toISOString();
  const deniedInput = JSON.stringify(ast);

  // Generate Merkle proof (simplified - hash of denial)
  const merkleProof = generateMerkleProof(deniedInput, timestamp, details);

  return {
    timestamp,
    merkleProof,
    deniedInput: deniedInput.slice(0, 500), // Truncate for storage
    reason: details.reason || 'COMPLEXITY_EXCEEDED',
    details: {
      constraint: details.constraint,
      limit: details.limit,
      actual: details.actual,
      suggestion: details.suggestion || 'Review grammar complexity',
      grammarType: ast.type,
    },
  };
}

/**
 * Emit compile receipt with metadata
 *
 * @param {Object} ast - Compiled AST
 * @param {Object} compiled - Compiled output
 * @param {string} grammarType - Grammar type
 * @returns {Object} Compile receipt
 */
export function emitCompileReceipt(ast, compiled, grammarType) {
  const timestamp = new Date().toISOString();

  return {
    timestamp,
    grammarType,
    decision: 'ACCEPT',
    inputHash: hashInput(JSON.stringify(ast)),
    outputHash: hashInput(JSON.stringify(compiled)),
    complexity: ast.complexity,
  };
}

// ============================================================================
// Grammar-Specific Bound Checkers
// ============================================================================

/**
 * Check SPARQL complexity bounds
 * @param {Object} complexity - Complexity metrics
 * @param {Object} bounds - Bounds to check
 * @returns {Object|null} Violation details or null
 */
function checkSPARQLBounds(complexity, bounds) {
  if (bounds.maxTriplePatterns && complexity.triplePatterns > bounds.maxTriplePatterns) {
    return {
      reason: 'COMPLEXITY_EXCEEDED',
      constraint: 'maxTriplePatterns',
      limit: bounds.maxTriplePatterns,
      actual: complexity.triplePatterns,
      suggestion: 'Reduce triple patterns or use federated queries',
    };
  }

  if (bounds.maxJoinDepth && complexity.joinDepth > bounds.maxJoinDepth) {
    return {
      reason: 'COMPLEXITY_EXCEEDED',
      constraint: 'maxJoinDepth',
      limit: bounds.maxJoinDepth,
      actual: complexity.joinDepth,
      suggestion: 'Reduce OPTIONAL/UNION nesting depth',
    };
  }

  if (bounds.maxFilterComplexity && complexity.filterComplexity > bounds.maxFilterComplexity) {
    return {
      reason: 'COMPLEXITY_EXCEEDED',
      constraint: 'maxFilterComplexity',
      limit: bounds.maxFilterComplexity,
      actual: complexity.filterComplexity,
      suggestion: 'Simplify FILTER expressions',
    };
  }

  return null;
}

/**
 * Check SHACL complexity bounds
 * @param {Object} complexity - Complexity metrics
 * @param {Object} bounds - Bounds to check
 * @returns {Object|null} Violation details or null
 */
function checkSHACLBounds(complexity, bounds) {
  if (bounds.maxShapesDepth && complexity.shapesDepth > bounds.maxShapesDepth) {
    return {
      reason: 'COMPLEXITY_EXCEEDED',
      constraint: 'maxShapesDepth',
      limit: bounds.maxShapesDepth,
      actual: complexity.shapesDepth,
      suggestion: 'Reduce shape nesting depth',
    };
  }

  return null;
}

/**
 * Check N3 complexity bounds
 * @param {Object} complexity - Complexity metrics
 * @param {Object} bounds - Bounds to check
 * @returns {Object|null} Violation details or null
 */
function checkN3Bounds(complexity, bounds) {
  if (bounds.maxRuleDepth && complexity.ruleDepth > bounds.maxRuleDepth) {
    return {
      reason: 'COMPLEXITY_EXCEEDED',
      constraint: 'maxRuleDepth',
      limit: bounds.maxRuleDepth,
      actual: complexity.ruleDepth,
      suggestion: 'Reduce rule nesting or split into multiple files',
    };
  }

  return null;
}

/**
 * Check OWL complexity bounds
 * @param {Object} complexity - Complexity metrics
 * @param {Object} bounds - Bounds to check
 * @returns {Object|null} Violation details or null
 */
function checkOWLBounds(complexity, bounds) {
  if (bounds.maxOWLAxioms && complexity.astNodeCount > bounds.maxOWLAxioms) {
    return {
      reason: 'COMPLEXITY_EXCEEDED',
      constraint: 'maxOWLAxioms',
      limit: bounds.maxOWLAxioms,
      actual: complexity.astNodeCount,
      suggestion: 'Split ontology into modules',
    };
  }

  return null;
}

/**
 * Check ShEx complexity bounds
 * @param {Object} complexity - Complexity metrics
 * @param {Object} bounds - Bounds to check
 * @returns {Object|null} Violation details or null
 */
function checkShExBounds(complexity, bounds) {
  if (bounds.maxShapeDepth && complexity.maxDepth > bounds.maxShapeDepth) {
    return {
      reason: 'COMPLEXITY_EXCEEDED',
      constraint: 'maxShapeDepth',
      limit: bounds.maxShapeDepth,
      actual: complexity.maxDepth,
      suggestion: 'Reduce shape reference depth',
    };
  }

  return null;
}

// ============================================================================
// Compilation Functions
// ============================================================================

/**
 * Compile AST to runtime executable form
 * @param {Object} ast - Parsed AST
 * @returns {Object} Compiled runtime form
 */
function compileASTToRuntime(ast) {
  // In production, this would transform AST to optimized execution plan
  // For now, return AST with compilation metadata

  return {
    type: 'compiled',
    grammarType: ast.type,
    ast,
    executable: true,
    optimizations: [],
    metadata: {
      compiledAt: new Date().toISOString(),
      compiler: 'v6-aot-compiler',
      version: '6.0.0-alpha.1',
    },
  };
}

/**
 * Create denial result
 * @param {string} reason - Denial reason
 * @param {string} message - Error message
 * @param {Object} ast - AST that was denied
 * @param {number} startTime - Compilation start time
 * @param {Object} opts - Options
 * @returns {Object} Denial result
 */
function createDenialResult(reason, message, ast, startTime, opts) {
  return {
    success: false,
    denial: { reason, message },
    compileReceipt: {
      timestamp: new Date().toISOString(),
      compileTimeMs: Date.now() - startTime,
      grammarType: ast?.type || 'unknown',
      decision: 'REJECT',
      reason,
    },
    denialReceipt: opts.emitReceipts !== false ? {
      timestamp: new Date().toISOString(),
      merkleProof: generateMerkleProof(JSON.stringify(ast), new Date().toISOString(), { reason }),
      deniedInput: JSON.stringify(ast).slice(0, 500),
      reason,
      details: { message },
    } : undefined,
  };
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Generate Merkle proof for denial receipt
 * @param {string} input - Input that was denied
 * @param {string} timestamp - Timestamp
 * @param {Object} details - Denial details
 * @returns {string} Merkle proof hash
 */
function generateMerkleProof(input, timestamp, details) {
  const data = JSON.stringify({ input, timestamp, details });
  return hashInput(data);
}

/**
 * Hash input for receipts
 * @param {string} input - Input to hash
 * @returns {string} Hash hex string
 */
function hashInput(input) {
  return createHash('sha256').update(input).digest('hex');
}
