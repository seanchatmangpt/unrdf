/**
 * @file MAPEK Orchestration - Unified execution with all innovations
 * @module project-engine/mapek-orchestration
 */

import { z } from 'zod';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { findMissingRoles } from './gap-finder.mjs';
import { auditTypeConsistency } from './type-auditor.mjs';
import { analyzeHotspots } from './hotspot-analyzer.mjs';
import { generateTestSuggestions } from './auto-test-generator.mjs';
import { checkDocDrift } from './doc-drift-checker.mjs';
import { buildDependencyGraph } from './dependency-graph.mjs';
import { generateAllAPISchemas, validateAPIFiles } from './api-contract-validator.mjs';
import { lintStack } from './stack-linter.mjs';
import { generateRefactoringGuide } from './refactoring-guide.mjs';
import { generateDocSuggestions } from './doc-generator.mjs';
import { promises as fs } from 'fs';
import path from 'path';

const { namedNode } = DataFactory;

const NS = {
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

// Scoring configuration constants
const SCORE_MULTIPLIERS = {
  GAP: 10,
  TYPE: 15,
  HOTSPOT: 20,
  DEPENDENCY: 30,
};

const SCORE_WEIGHTS = {
  GAP: 0.15,
  TYPE: 0.15,
  HOTSPOT: 0.1,
  TEST: 0.15,
  DOC_DRIFT: 0.05,
  DEPENDENCY: 0.1,
  API_CONTRACT: 0.1,
  STACK_LINT: 0.05,
  REFACTORING: 0.1,
  DOC_COVERAGE: 0.05,
};

const SCORE_THRESHOLDS = {
  MAX_SCORE: 100,
  TYPE_ISSUE: 50,
  TYPE_CRITICAL: 80,
  TEST_ISSUE: 30,
  TEST_HIGH: 60,
  DOC_ISSUE: 30,
  API_CONTRACT_ISSUE: 40,
  API_CONTRACT_CRITICAL: 70,
  REFACTORING_ISSUE: 40,
  GAP_CRITICAL: 80,
  HEALTH_REPEAT_THRESHOLD: 70,
  COMMON_ISSUE_THRESHOLD: 50,
};

// Kaizen improvement: Extract magic number to named constant
const PRIORITIZED_ISSUES_LIMIT = 20;

// Kaizen improvement: Extract severity order mapping to constant
const SEVERITY_ORDER = {
  critical: 0,
  high: 1,
  medium: 2,
  low: 3,
};

// Kaizen improvement: Extract issue type strings to constants
const ISSUE_TYPES = {
  GAP: 'gap',
  TYPE_MISMATCH: 'type-mismatch',
  DEPENDENCY: 'dependency',
  API_CONTRACT: 'api-contract',
  LINT: 'lint',
};

// Kaizen improvement: Add type safety for severity values
const SEVERITY_LEVELS = {
  CRITICAL: 'critical',
  HIGH: 'high',
  MEDIUM: 'medium',
  LOW: 'low',
};

/**
 * Calculate risk score from issue count and multiplier
 * Converts issue counts to risk scores (0-100)
 * @param {number|undefined} count - Number of issues
 * @param {number} multiplier - Score multiplier per issue
 * @returns {number} Risk score (0-100), capped at MAX_SCORE
 */
function calculateRiskScore(count, multiplier) {
  if (!count || count === 0) return 0;
  return Math.min(SCORE_THRESHOLDS.MAX_SCORE, count * multiplier);
}

/**
 * Calculate coverage-based score from a value
 * Converts coverage/health values (0-100) to risk scores (0-100)
 * Higher coverage = lower risk score
 * @param {number|undefined} value - Coverage or health score value (0-100)
 * @returns {number} Risk score (0-100), where 0 = perfect, 100 = worst
 */
function calculateCoverageScore(value) {
  return SCORE_THRESHOLDS.MAX_SCORE - (value || SCORE_THRESHOLDS.MAX_SCORE);
}

/**
 * Map lint severity to issue severity
 * Converts ESLint severity levels to standardized issue severity levels
 * @param {string} lintSeverity - ESLint severity ("error", "warning", "info")
 * @returns {string} Issue severity (SEVERITY_LEVELS.HIGH, MEDIUM, or LOW)
 */
function mapLintSeverity(lintSeverity) {
  if (lintSeverity === 'error') return SEVERITY_LEVELS.HIGH;
  if (lintSeverity === 'warning') return SEVERITY_LEVELS.MEDIUM;
  return SEVERITY_LEVELS.LOW;
}

/**
 * Extract issues from results and add to allIssues array
 * Reduces repetitive issue aggregation code
 * @param {Array} items - Array of items to convert to issues
 * @param {string} issueType - Type of issue (from ISSUE_TYPES)
 * @param {Function} [severityMapper] - Optional function to map item severity
 * @returns {Array} Array of issue objects
 */
function extractIssues(items, issueType, severityMapper = item => item.severity) {
  if (!items || items.length === 0) return [];
  return items.map(item => ({
    type: issueType,
    severity: severityMapper(item),
    item,
  }));
}

const OrchestrationOptionsSchema = z.object({
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  domainStore: z
    .custom(val => val && typeof val.getQuads === 'function', {
      message: 'domainStore must be an RDF store with getQuads method',
    })
    .optional(),
  projectRoot: z.string().optional(),
  stackProfile: z.object({}).passthrough().optional(),
  innovations: z.array(z.string()).optional(),
});

const ALL_INNOVATIONS = [
  'gap-finder',
  'type-auditor',
  'hotspot-analyzer',
  'auto-test-generator',
  'doc-drift-checker',
  'dependency-graph',
  'api-contract-validator',
  'stack-linter',
  'refactoring-guide',
  'doc-generator',
];

/**
 * Innovation execution strategies - replaces large switch statement
 * Maps innovation names to their execution functions
 */
const INNOVATION_STRATEGIES = {
  'gap-finder': ({ domainStore, projectStore, stackProfile }) =>
    findMissingRoles({ domainStore, projectStore, stackProfile }),
  'type-auditor': async ({ domainStore, projectStore, stackProfile, projectRoot }) =>
    await auditTypeConsistency({ domainStore, fsStore: projectStore, stackProfile, projectRoot }),
  'hotspot-analyzer': ({ projectStore, domainStore, stackProfile }) =>
    analyzeHotspots({ projectStore, domainStore, stackProfile }),
  'auto-test-generator': ({ projectStore, domainStore, stackProfile }) =>
    generateTestSuggestions({ projectStore, domainStore, stackProfile }),
  'doc-drift-checker': ({ projectStore, projectRoot }) =>
    checkDocDrift({ projectStore, projectRoot }),
  'dependency-graph': ({ projectStore, projectRoot }) =>
    buildDependencyGraph({ projectStore, projectRoot }),
  'api-contract-validator': async ({ projectStore, domainStore, projectRoot }) =>
    await validateApiContracts({ projectStore, domainStore, projectRoot }),
  'stack-linter': ({ projectStore, stackProfile, projectRoot }) =>
    lintStack({ projectStore, stackProfile, projectRoot }),
  'refactoring-guide': ({ projectStore, domainStore, stackProfile }) =>
    generateRefactoringGuide({ projectStore, domainStore, stackProfile }),
  'doc-generator': ({ projectStore, domainStore, stackProfile, projectRoot }) =>
    generateDocSuggestions({ projectStore, domainStore, stackProfile, projectRoot }),
};

/**
 * Validate API contracts by matching API files to domain schemas
 * @param {Object} options
 * @param {Store} options.projectStore - RDF project structure
 * @param {Store} options.domainStore - RDF domain model
 * @param {string} [options.projectRoot] - File system root
 * @returns {Promise<Object>} Validation results
 */
async function validateApiContracts({ projectStore, domainStore, projectRoot }) {
  if (!domainStore) {
    return {
      violations: [],
      coverage: 0,
      breaking: false,
      summary: 'No domain store provided',
    };
  }

  // Generate all schemas from domain store
  const schemas = generateAllAPISchemas(domainStore);
  if (schemas.length === 0) {
    return {
      violations: [],
      coverage: 0,
      breaking: false,
      summary: 'No domain entities found',
    };
  }

  // Extract API files from project store
  const apiFileQuads = projectStore
    .getQuads(null, namedNode(`${NS.proj}roleString`), null)
    .filter(quad => {
      const role = quad.object.value;
      return role === 'Api' || role === 'Route';
    });

  const apiFiles = [];
  for (const roleQuad of apiFileQuads) {
    const fileIri = roleQuad.subject;
    const pathQuads = projectStore.getQuads(fileIri, namedNode(`${NS.fs}relativePath`), null);
    if (pathQuads.length > 0) {
      const filePath = pathQuads[0].object.value;
      let content = '';
      if (projectRoot) {
        try {
          const fullPath = path.join(projectRoot, filePath);
          content = await fs.readFile(fullPath, 'utf-8');
        } catch (err) {
          // File doesn't exist or can't be read, skip
          continue;
        }
      }
      apiFiles.push({ path: filePath, content });
    }
  }

  if (apiFiles.length === 0) {
    return {
      violations: [],
      coverage: 0,
      breaking: false,
      summary: 'No API files found in project',
    };
  }

  // Determine framework from stack profile or infer from file paths
  const framework = apiFiles.some(f => f.path.includes('app/api') || f.path.includes('route.ts'))
    ? 'nextjs'
    : apiFiles.some(f => f.path.includes('routes') || f.path.includes('router'))
      ? 'express'
      : undefined;

  // Validate each schema against matching API files
  const allViolations = [];
  let totalCoverage = 0;
  let hasBreaking = false;

  for (const schema of schemas) {
    // Find API files that might match this entity (by name pattern)
    const entityName = schema.entityName.toLowerCase();
    const matchingFiles = apiFiles.filter(file => {
      const fileName = path.basename(file.path, path.extname(file.path)).toLowerCase();
      const dirName = path.dirname(file.path).toLowerCase();
      return (
        fileName.includes(entityName) ||
        dirName.includes(entityName) ||
        file.path.toLowerCase().includes(`/${entityName}`)
      );
    });

    if (matchingFiles.length > 0) {
      const result = validateAPIFiles(matchingFiles, schema, { framework });
      allViolations.push(...result.violations);
      totalCoverage += result.coverage;
      if (result.breaking) {
        hasBreaking = true;
      }
    }
  }

  const avgCoverage = schemas.length > 0 ? Math.round(totalCoverage / schemas.length) : 0;
  const criticalCount = allViolations.filter(v => v.severity === SEVERITY_LEVELS.CRITICAL).length;
  const highCount = allViolations.filter(v => v.severity === SEVERITY_LEVELS.HIGH).length;

  let summary = `${allViolations.length} violations found across ${schemas.length} entities`;
  if (criticalCount > 0) {
    summary += ` (${criticalCount} critical)`;
  } else if (highCount > 0) {
    summary += ` (${highCount} high severity)`;
  }
  if (allViolations.length === 0) {
    summary = 'API contracts are valid';
  }

  return {
    violations: allViolations,
    coverage: avgCoverage,
    breaking: hasBreaking,
    summary,
  };
}

/**
 * Run all innovation analyzers in parallel
 * @param {Object} options
 * @returns {Promise<Object>} Combined findings from all innovations
 */
export async function runInnovationsParallel(options) {
  const validated = OrchestrationOptionsSchema.parse(options);
  const { projectStore, domainStore, stackProfile, projectRoot, innovations } = validated;

  const enabledInnovations = innovations || ALL_INNOVATIONS;
  const results = {};
  const errors = [];

  // Kaizen improvement: Add error handling for missing innovation results
  if (!projectStore) {
    errors.push({ innovation: 'all', error: 'projectStore is required' });
    return { results, errors, innovationsRun: 0 };
  }

  // Strategy map replaces large switch statement - reduces complexity
  const resultKeys = {
    'gap-finder': 'gaps',
    'type-auditor': 'typeIssues',
    'hotspot-analyzer': 'hotspots',
    'auto-test-generator': 'testSuggestions',
    'doc-drift-checker': 'docDrift',
    'dependency-graph': 'dependencyGraph',
    'api-contract-validator': 'apiContracts',
    'stack-linter': 'stackLint',
    'refactoring-guide': 'refactoring',
    'doc-generator': 'docSuggestions',
  };

  const tasks = enabledInnovations.map(async name => {
    try {
      const strategy = INNOVATION_STRATEGIES[name];
      if (!strategy) {
        errors.push({ innovation: name, error: 'Unknown innovation' });
        return;
      }

      const context = { domainStore, projectStore, stackProfile, projectRoot };
      const result = await strategy(context);
      const resultKey = resultKeys[name];
      if (resultKey) {
        results[resultKey] = result;
      }
    } catch (err) {
      errors.push({ innovation: name, error: err.message });
    }
  });

  await Promise.all(tasks);

  return { results, errors, innovationsRun: enabledInnovations.length };
}

/**
 * Aggregate findings from all innovations into unified metrics
 *
 * @description
 * Calculates unified health scores from all innovation findings using weighted averages.
 * Score formula: score = min(MAX_SCORE, issueCount * multiplier) for risk scores,
 * or score = MAX_SCORE - coverage for coverage-based scores.
 * Overall health = MAX_SCORE - weighted sum of all risk scores.
 *
 * @param {Object} innovationResults - Results from runInnovationsParallel
 * @returns {Object} Aggregated findings with priorities
 */
export function aggregateInnovationFindings(innovationResults) {
  const { results } = innovationResults;

  // Calculate unified scores
  const scores = {
    gapScore: calculateRiskScore(results.gaps?.gaps?.length, SCORE_MULTIPLIERS.GAP),
    typeScore: calculateRiskScore(results.typeIssues?.mismatches?.length, SCORE_MULTIPLIERS.TYPE),
    hotspotScore: calculateRiskScore(
      results.hotspots?.hotspots?.filter(h => h.risk === 'HIGH').length,
      SCORE_MULTIPLIERS.HOTSPOT
    ),
    testScore: calculateCoverageScore(results.testSuggestions?.coverage),
    docDriftScore: calculateCoverageScore(results.docDrift?.healthScore),
    dependencyScore: calculateRiskScore(
      results.dependencyGraph?.issues?.filter(i => i.severity === SEVERITY_LEVELS.CRITICAL).length,
      SCORE_MULTIPLIERS.DEPENDENCY
    ),
    apiContractScore: calculateCoverageScore(results.apiContracts?.coverage),
    stackLintScore: calculateCoverageScore(results.stackLint?.score),
    refactoringScore: results.refactoring?.technicalDebt || 0,
    docCoverageScore: calculateCoverageScore(results.docSuggestions?.coverage),
  };

  // Calculate overall health (weighted average)
  const weights = {
    gapScore: SCORE_WEIGHTS.GAP,
    typeScore: SCORE_WEIGHTS.TYPE,
    hotspotScore: SCORE_WEIGHTS.HOTSPOT,
    testScore: SCORE_WEIGHTS.TEST,
    docDriftScore: SCORE_WEIGHTS.DOC_DRIFT,
    dependencyScore: SCORE_WEIGHTS.DEPENDENCY,
    apiContractScore: SCORE_WEIGHTS.API_CONTRACT,
    stackLintScore: SCORE_WEIGHTS.STACK_LINT,
    refactoringScore: SCORE_WEIGHTS.REFACTORING,
    docCoverageScore: SCORE_WEIGHTS.DOC_COVERAGE,
  };

  // Kaizen improvement: Validate score weights sum to 1.0
  const weightSum = Object.values(weights).reduce((sum, weight) => sum + weight, 0);
  if (Math.abs(weightSum - 1.0) > 0.001) {
    console.warn(`Warning: SCORE_WEIGHTS sum to ${weightSum}, expected 1.0`);
  }

  const overallRisk = Object.entries(scores).reduce((total, [key, score]) => {
    return total + score * (weights[key] || 0);
  }, 0);

  const overallHealth = Math.round(SCORE_THRESHOLDS.MAX_SCORE - overallRisk);

  // Prioritize issues - extract repetitive pattern
  const allIssues = [
    ...extractIssues(results.gaps?.gaps, ISSUE_TYPES.GAP, g =>
      g.score > SCORE_THRESHOLDS.GAP_CRITICAL ? SEVERITY_LEVELS.CRITICAL : SEVERITY_LEVELS.MEDIUM
    ),
    ...extractIssues(results.typeIssues?.mismatches, ISSUE_TYPES.TYPE_MISMATCH),
    ...extractIssues(results.dependencyGraph?.issues, ISSUE_TYPES.DEPENDENCY),
    ...extractIssues(results.apiContracts?.violations, ISSUE_TYPES.API_CONTRACT),
    ...extractIssues(results.stackLint?.issues, ISSUE_TYPES.LINT, i => mapLintSeverity(i.severity)),
  ];

  // Sort by severity
  allIssues.sort((a, b) => SEVERITY_ORDER[a.severity] - SEVERITY_ORDER[b.severity]);

  return {
    scores,
    overallHealth,
    overallRisk: Math.round(overallRisk),
    totalIssues: allIssues.length,
    criticalIssues: allIssues.filter(i => i.severity === SEVERITY_LEVELS.CRITICAL).length,
    highIssues: allIssues.filter(i => i.severity === SEVERITY_LEVELS.HIGH).length,
    prioritizedIssues: allIssues.slice(0, PRIORITIZED_ISSUES_LIMIT),
    summaries: {
      gaps: results.gaps?.summary || 'N/A',
      types: results.typeIssues?.summary || 'N/A',
      hotspots: results.hotspots?.summary || 'N/A',
      tests: results.testSuggestions?.summary || 'N/A',
      docDrift: results.docDrift?.summary || 'N/A',
      dependencies: results.dependencyGraph?.summary || 'N/A',
      apiContracts: results.apiContracts?.summary || 'N/A',
      stackLint: results.stackLint?.summary || 'N/A',
      refactoring: results.refactoring?.summary || 'N/A',
      docSuggestions: results.docSuggestions?.summary || 'N/A',
    },
  };
}

/**
 * Create a decision object - reduces repetitive decision generation code
 * @param {string} issue - Issue identifier
 * @param {string} severity - Severity level
 * @param {string} action - Action to take
 * @param {boolean} autoFixable - Whether action can be auto-fixed
 * @param {string} description - Description of the decision
 * @returns {Object} Decision object
 */
function createDecision(issue, severity, action, autoFixable, description) {
  return { issue, severity, action, autoFixable, description };
}

/**
 * Kaizen improvement: Extract decision generation logic to helper function
 * Generates decisions based on aggregated findings and innovation results
 * @param {Object} aggregated - Aggregated findings from aggregateInnovationFindings
 * @param {Object} innovationResults - Results from runInnovationsParallel
 * @returns {Array<Object>} Array of decision objects
 */
function generateDecisions(aggregated, innovationResults) {
  const decisions = [];

  // Decision: Fix type mismatches
  if (aggregated.scores.typeScore > SCORE_THRESHOLDS.TYPE_ISSUE) {
    decisions.push(
      createDecision(
        'type-mismatch',
        aggregated.scores.typeScore > SCORE_THRESHOLDS.TYPE_CRITICAL
          ? SEVERITY_LEVELS.CRITICAL
          : SEVERITY_LEVELS.HIGH,
        'sync-zod-ts-types',
        true,
        'Sync Zod schemas with TypeScript types'
      )
    );
  }

  // Decision: Generate missing tests
  if (aggregated.scores.testScore > SCORE_THRESHOLDS.TEST_ISSUE) {
    decisions.push(
      createDecision(
        'low-test-coverage',
        aggregated.scores.testScore > SCORE_THRESHOLDS.TEST_HIGH
          ? SEVERITY_LEVELS.HIGH
          : SEVERITY_LEVELS.MEDIUM,
        'generate-tests',
        true,
        'Generate tests for uncovered files'
      )
    );
  }

  // Decision: Update documentation
  if (
    aggregated.scores.docDriftScore > SCORE_THRESHOLDS.DOC_ISSUE ||
    aggregated.scores.docCoverageScore > SCORE_THRESHOLDS.DOC_ISSUE
  ) {
    decisions.push(
      createDecision(
        'documentation-drift',
        SEVERITY_LEVELS.MEDIUM,
        'update-docs',
        false,
        'Update out-of-sync documentation'
      )
    );
  }

  // Decision: Fix circular dependencies
  if (innovationResults.results.dependencyGraph?.metrics?.circularCount > 0) {
    decisions.push(
      createDecision(
        'circular-dependency',
        SEVERITY_LEVELS.CRITICAL,
        'refactor-dependencies',
        false,
        'Break circular dependency chains'
      )
    );
  }

  // Decision: Fix API contract violations
  if (aggregated.scores.apiContractScore > SCORE_THRESHOLDS.API_CONTRACT_ISSUE) {
    decisions.push(
      createDecision(
        'api-contract-violation',
        aggregated.scores.apiContractScore > SCORE_THRESHOLDS.API_CONTRACT_CRITICAL
          ? SEVERITY_LEVELS.CRITICAL
          : SEVERITY_LEVELS.HIGH,
        'fix-api-contracts',
        true,
        'Add missing API validation and documentation'
      )
    );
  }

  // Decision: Address stack lint errors
  const lintErrors =
    innovationResults.results.stackLint?.issues?.filter(i => i.severity === 'error').length || 0;
  if (lintErrors > 0) {
    decisions.push(
      createDecision(
        'lint-errors',
        SEVERITY_LEVELS.HIGH,
        'fix-lint-errors',
        true,
        `${lintErrors} lint errors require fixing`
      )
    );
  }

  // Decision: Refactoring
  if (aggregated.scores.refactoringScore > SCORE_THRESHOLDS.REFACTORING_ISSUE) {
    decisions.push(
      createDecision(
        'technical-debt',
        SEVERITY_LEVELS.MEDIUM,
        'refactor-code',
        false,
        'Address high technical debt areas'
      )
    );
  }

  return decisions;
}

/**
 * Run full MAPEK cycle with all 10 innovations
 * @param {Object} options
 * @returns {Promise<Object>} Complete MAPEK result with all findings and decisions
 */
export async function runFullMapekWithAllInnovations(options) {
  const _validated = OrchestrationOptionsSchema.parse(options);
  const startTime = Date.now();

  // Phase 1: Monitor - Run all innovations in parallel
  const innovationResults = await runInnovationsParallel(options);

  // Phase 2: Analyze - Aggregate findings
  const aggregated = aggregateInnovationFindings(innovationResults);

  // Phase 3: Plan - Generate decisions based on findings
  const decisions = generateDecisions(aggregated, innovationResults);

  // Phase 4: Execute - Plan actions
  const actions = decisions
    .filter(d => d.autoFixable)
    .map(d => ({
      type: d.action,
      status: 'planned',
      timestamp: new Date().toISOString(),
    }));

  // Phase 5: Knowledge - Extract learnings
  const learnings = {
    timestamp: new Date().toISOString(),
    patterns: {
      highRiskAreas: aggregated.prioritizedIssues.slice(0, 5).map(i => i.type),
      commonIssueTypes: Object.entries(aggregated.scores)
        .filter(([_k, v]) => v > SCORE_THRESHOLDS.COMMON_ISSUE_THRESHOLD)
        .map(([k]) => k.replace('Score', '')),
    },
    thresholds: {
      criticalTypeScore: SCORE_THRESHOLDS.TYPE_CRITICAL,
      criticalGapScore: SCORE_THRESHOLDS.GAP_CRITICAL,
      criticalApiScore: SCORE_THRESHOLDS.API_CONTRACT_CRITICAL,
    },
  };

  const duration = Date.now() - startTime;

  return {
    phase: 'knowledge',
    timestamp: new Date().toISOString(),
    duration,
    overallHealth: aggregated.overallHealth,
    allFindings: innovationResults.results,
    aggregated,
    decisions,
    actions,
    learnings,
    shouldRepeat:
      aggregated.overallHealth < SCORE_THRESHOLDS.HEALTH_REPEAT_THRESHOLD && actions.length > 0,
    errors: innovationResults.errors,
  };
}

export { ALL_INNOVATIONS };
