/**
 * On-Bounds-Exceeded Hook
 *
 * Enforces computational bounds before executing kgc:query or kgc:proof blocks.
 *
 * RULES:
 * 1. Bounds in frontmatter (maxQueries, maxRuntime, maxFileScans) must not be exceeded
 * 2. Predict cost before execution (query cardinality, file count, estimated runtime)
 * 3. DENY if bounds exceeded; emit denial receipt with reason
 * 4. Suggest increasing bounds with /kgc:refresh
 *
 * @module on-bounds-exceeded-hook
 */

import { readdir } from 'node:fs/promises';
import { join } from 'node:path';
import {
  isDocsPath,
  extractFrontmatter,
  validateFrontmatter,
  generateDenialReceipt,
  safeReadFile,
  DENIAL_REASONS,
  DOCS_DIR,
} from './hooks-shared.mjs';

// =============================================================================
// Hook Configuration
// =============================================================================

export const HOOK_NAME = 'on-bounds-exceeded';
export const HOOK_DESCRIPTION = 'Validates computational bounds before query/proof execution';

export const TRIGGERS = [
  'kgc:query:before',
  'kgc:proof:before',
  'kgc:render:before',
];

/**
 * Default bounds if not specified in frontmatter
 */
export const DEFAULT_BOUNDS = {
  maxQueries: 100,
  maxRuntime: 5000, // 5 seconds (Andon principle)
  maxFileScans: 1000,
};

// =============================================================================
// Cost Estimation
// =============================================================================

/**
 * Estimate query cardinality based on SPARQL pattern
 *
 * @param {string} sparqlQuery - SPARQL query string
 * @returns {number} Estimated result count (order of magnitude)
 */
export function estimateQueryCardinality(sparqlQuery) {
  // Simple heuristic based on query structure
  const normalized = sparqlQuery.toLowerCase();

  // COUNT queries
  if (normalized.includes('count(')) {
    return 1;
  }

  // LIMIT clause
  const limitMatch = normalized.match(/limit\s+(\d+)/);
  if (limitMatch) {
    return parseInt(limitMatch[1], 10);
  }

  // ASK queries
  if (normalized.startsWith('ask')) {
    return 1;
  }

  // DESCRIBE queries - assume moderate size
  if (normalized.startsWith('describe')) {
    return 50;
  }

  // CONSTRUCT queries - assume high cardinality
  if (normalized.startsWith('construct')) {
    return 500;
  }

  // SELECT with specific variables - moderate
  if (normalized.includes('select ') && !normalized.includes('select *')) {
    return 100;
  }

  // SELECT * - assume high cardinality
  if (normalized.includes('select *')) {
    return 1000;
  }

  // Default moderate estimate
  return 100;
}

/**
 * Estimate runtime based on query complexity
 *
 * @param {string} sparqlQuery - SPARQL query string
 * @returns {number} Estimated runtime in milliseconds
 */
export function estimateQueryRuntime(sparqlQuery) {
  const normalized = sparqlQuery.toLowerCase();

  // Simple queries
  if (normalized.startsWith('ask') || normalized.includes('limit 1')) {
    return 10; // 10ms
  }

  // COUNT queries
  if (normalized.includes('count(')) {
    return 100; // 100ms
  }

  // Queries with filters
  const filterCount = (normalized.match(/filter\s*\(/g) || []).length;
  const baseRuntime = 50; // 50ms base

  // Join complexity
  const triplePatterns = (normalized.match(/\?\w+\s+\S+\s+\S+/g) || []).length;
  const joinComplexity = Math.max(0, triplePatterns - 1) * 20; // 20ms per join

  // Filter complexity
  const filterComplexity = filterCount * 30; // 30ms per filter

  return baseRuntime + joinComplexity + filterComplexity;
}

/**
 * Estimate file scan count for document operations
 *
 * @param {string} operation - Operation type ('query', 'proof', 'render')
 * @param {Object} params - Operation parameters
 * @returns {Promise<number>} Estimated file scan count
 */
export async function estimateFileScanCount(operation, params) {
  // For proof operations, estimate based on receipt chain depth
  if (operation === 'proof') {
    const { receiptChainDepth = 1 } = params;
    return receiptChainDepth * 2; // 2 files per receipt (receipt + state)
  }

  // For query operations, estimate based on scope
  if (operation === 'query') {
    const { scope = 'document' } = params;

    if (scope === 'document') {
      return 1;
    }

    if (scope === 'directory') {
      try {
        const files = await readdir(DOCS_DIR);
        return files.filter(f => f.endsWith('.md') || f.endsWith('.kgcmd')).length;
      } catch {
        return 100; // Conservative estimate
      }
    }

    if (scope === 'global') {
      return 500; // Conservative estimate for repo-wide scans
    }
  }

  // For render operations, estimate based on dynamic blocks
  if (operation === 'render') {
    const { dynamicBlockCount = 1 } = params;
    return dynamicBlockCount * 5; // 5 files per dynamic block
  }

  return 10; // Default conservative estimate
}

// =============================================================================
// Bounds Validation
// =============================================================================

/**
 * Get bounds from frontmatter or use defaults
 *
 * @param {Object} frontmatter - Document frontmatter
 * @returns {Object} Bounds configuration
 */
export function getBounds(frontmatter) {
  return {
    maxQueries: frontmatter?.maxQueries || DEFAULT_BOUNDS.maxQueries,
    maxRuntime: frontmatter?.maxRuntime || DEFAULT_BOUNDS.maxRuntime,
    maxFileScans: frontmatter?.maxFileScans || DEFAULT_BOUNDS.maxFileScans,
  };
}

/**
 * Check if operation would exceed bounds
 *
 * @param {Object} cost - Estimated cost
 * @param {number} cost.queries - Query count
 * @param {number} cost.runtime - Runtime in ms
 * @param {number} cost.fileScans - File scan count
 * @param {Object} bounds - Bounds limits
 * @returns {{ exceeded: boolean, violations: Array<Object> }}
 */
export function checkBounds(cost, bounds) {
  const violations = [];

  if (cost.queries > bounds.maxQueries) {
    violations.push({
      type: 'maxQueries',
      estimated: cost.queries,
      limit: bounds.maxQueries,
      exceeded: cost.queries - bounds.maxQueries,
    });
  }

  if (cost.runtime > bounds.maxRuntime) {
    violations.push({
      type: 'maxRuntime',
      estimated: cost.runtime,
      limit: bounds.maxRuntime,
      exceeded: cost.runtime - bounds.maxRuntime,
    });
  }

  if (cost.fileScans > bounds.maxFileScans) {
    violations.push({
      type: 'maxFileScans',
      estimated: cost.fileScans,
      limit: bounds.maxFileScans,
      exceeded: cost.fileScans - bounds.maxFileScans,
    });
  }

  return {
    exceeded: violations.length > 0,
    violations,
  };
}

// =============================================================================
// Validation Logic
// =============================================================================

/**
 * Main validation function
 *
 * @param {Object} params - Hook parameters
 * @param {string} params.filePath - Document path
 * @param {string} params.operation - Operation type
 * @param {Object} params.operationParams - Operation parameters
 * @param {string} [params.sparqlQuery] - SPARQL query for kgc:query
 * @param {boolean} [params.dryRun=false] - Don't write denial receipt
 * @returns {Promise<{ allowed: boolean, receipt?: Object, errors?: Array<string>, cost?: Object }>}
 */
export async function validate(params) {
  const {
    filePath,
    operation,
    operationParams = {},
    sparqlQuery,
    dryRun = false,
  } = params;

  const errors = [];

  // Step 1: Read document frontmatter
  const fileRead = await safeReadFile(filePath);
  if (!fileRead.success) {
    // Document doesn't exist - allow (will be caught by other hooks)
    return { allowed: true };
  }

  const { frontmatter } = extractFrontmatter(fileRead.content);
  const bounds = getBounds(frontmatter);

  // Step 2: Estimate cost
  const cost = {
    queries: 0,
    runtime: 0,
    fileScans: 0,
  };

  if (sparqlQuery) {
    cost.queries = 1;
    cost.runtime = estimateQueryRuntime(sparqlQuery);
  }

  cost.fileScans = await estimateFileScanCount(operation, operationParams);

  // For multiple queries, accumulate
  if (operationParams.queryCount) {
    cost.queries = operationParams.queryCount;
    cost.runtime *= operationParams.queryCount;
  }

  // Step 3: Check bounds
  const boundsCheck = checkBounds(cost, bounds);

  if (boundsCheck.exceeded) {
    const violationSummary = boundsCheck.violations
      .map(v => `${v.type}: ${v.estimated} > ${v.limit} (+${v.exceeded})`)
      .join(', ');

    errors.push(`Computational bounds exceeded: ${violationSummary}`);

    if (!dryRun) {
      const receipt = await generateDenialReceipt({
        operation,
        targetPath: filePath,
        reasonCode: DENIAL_REASONS.BOUNDS_EXCEEDED,
        message: `Operation denied: Computational bounds exceeded`,
        details: {
          errors,
          estimatedCost: cost,
          bounds,
          violations: boundsCheck.violations,
          operation,
          sparqlQuery: sparqlQuery?.slice(0, 200),
        },
        remediation: {
          command: '/kgc:refresh',
          steps: [
            'Operation would exceed computational bounds',
            '',
            'Estimated cost:',
            `  - Queries: ${cost.queries} (limit: ${bounds.maxQueries})`,
            `  - Runtime: ${cost.runtime}ms (limit: ${bounds.maxRuntime}ms)`,
            `  - File scans: ${cost.fileScans} (limit: ${bounds.maxFileScans})`,
            '',
            'Options:',
            '1. Increase bounds in frontmatter:',
            '   ---',
            `   maxQueries: ${Math.ceil(cost.queries * 1.5)}`,
            `   maxRuntime: ${Math.ceil(cost.runtime * 1.5)}`,
            `   maxFileScans: ${Math.ceil(cost.fileScans * 1.5)}`,
            '   ---',
            '',
            '2. Optimize query to reduce cost:',
            '   - Add LIMIT clause',
            '   - Reduce join complexity',
            '   - Use more specific filters',
            '',
            '3. Use /kgc:refresh to regenerate with updated bounds',
          ],
          documentation: 'docs/ANDON-SIGNALS-INDEX.md',
        },
      });

      return { allowed: false, receipt, errors, cost };
    }

    return { allowed: false, errors, cost };
  }

  // Bounds satisfied
  return { allowed: true, cost };
}

// =============================================================================
// Hook Executor
// =============================================================================

/**
 * Execute hook (called by Claude Code hooks system)
 *
 * @param {Object} context - Hook execution context
 * @param {string} context.operation - Operation name
 * @param {Object} context.params - Operation parameters
 * @returns {Promise<{ proceed: boolean, message?: string, receipt?: Object }>}
 */
export async function execute(context) {
  const { operation, params } = context;

  const result = await validate({
    filePath: params.filePath || params.documentPath,
    operation,
    operationParams: params,
    sparqlQuery: params.query || params.sparql,
    dryRun: false,
  });

  if (!result.allowed) {
    return {
      proceed: false,
      message: result.errors?.join('\n') || 'Bounds exceeded',
      receipt: result.receipt,
      cost: result.cost,
    };
  }

  return {
    proceed: true,
    cost: result.cost,
  };
}

// =============================================================================
// Exports
// =============================================================================

export default {
  name: HOOK_NAME,
  description: HOOK_DESCRIPTION,
  triggers: TRIGGERS,
  execute,
  validate,
  getBounds,
  estimateQueryCardinality,
  estimateQueryRuntime,
  estimateFileScanCount,
  checkBounds,
};
