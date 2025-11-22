/**
 * @file MAPEK Orchestration - Unified execution with all innovations
 * @module project-engine/mapek-orchestration
 */

import { z } from "zod"
import { findMissingRoles } from "./gap-finder.mjs"
import { auditTypeConsistency } from "./type-auditor.mjs"
import { analyzeHotspots } from "./hotspot-analyzer.mjs"
import { generateTestSuggestions } from "./auto-test-generator.mjs"
import { checkDocDrift } from "./doc-drift-checker.mjs"
import { buildDependencyGraph } from "./dependency-graph.mjs"
import { validateApiContracts } from "./api-contract-validator.mjs"
import { lintStack } from "./stack-linter.mjs"
import { generateRefactoringGuide } from "./refactoring-guide.mjs"
import { generateDocSuggestions } from "./doc-generator.mjs"

const OrchestrationOptionsSchema = z.object({
  projectStore: z.custom((val) => val && typeof val.getQuads === "function", {
    message: "projectStore must be an RDF store with getQuads method",
  }),
  domainStore: z.custom((val) => val && typeof val.getQuads === "function", {
    message: "domainStore must be an RDF store with getQuads method",
  }).optional(),
  projectRoot: z.string().optional(),
  stackProfile: z.object({}).passthrough().optional(),
  innovations: z.array(z.string()).optional(),
})

const ALL_INNOVATIONS = [
  "gap-finder",
  "type-auditor",
  "hotspot-analyzer",
  "auto-test-generator",
  "doc-drift-checker",
  "dependency-graph",
  "api-contract-validator",
  "stack-linter",
  "refactoring-guide",
  "doc-generator",
]

/**
 * Run all innovation analyzers in parallel
 * @param {Object} options
 * @returns {Promise<Object>} Combined findings from all innovations
 */
export async function runInnovationsParallel(options) {
  const validated = OrchestrationOptionsSchema.parse(options)
  const { projectStore, domainStore, stackProfile, projectRoot, innovations } = validated

  const enabledInnovations = innovations || ALL_INNOVATIONS
  const results = {}
  const errors = []

  const tasks = enabledInnovations.map(async (name) => {
    try {
      switch (name) {
        case "gap-finder":
          results.gaps = findMissingRoles({ domainStore, projectStore, stackProfile })
          break
        case "type-auditor":
          results.typeIssues = await auditTypeConsistency({ domainStore, fsStore: projectStore, stackProfile, projectRoot })
          break
        case "hotspot-analyzer":
          results.hotspots = analyzeHotspots({ projectStore, domainStore, stackProfile })
          break
        case "auto-test-generator":
          results.testSuggestions = generateTestSuggestions({ projectStore, domainStore, stackProfile })
          break
        case "doc-drift-checker":
          results.docDrift = checkDocDrift({ projectStore, projectRoot })
          break
        case "dependency-graph":
          results.dependencyGraph = buildDependencyGraph({ projectStore, projectRoot })
          break
        case "api-contract-validator":
          results.apiContracts = validateApiContracts({ projectStore, domainStore, projectRoot })
          break
        case "stack-linter":
          results.stackLint = lintStack({ projectStore, stackProfile, projectRoot })
          break
        case "refactoring-guide":
          results.refactoring = generateRefactoringGuide({ projectStore, domainStore, stackProfile })
          break
        case "doc-generator":
          results.docSuggestions = generateDocSuggestions({ projectStore, domainStore, stackProfile, projectRoot })
          break
        default:
          errors.push({ innovation: name, error: "Unknown innovation" })
      }
    } catch (err) {
      errors.push({ innovation: name, error: err.message })
    }
  })

  await Promise.all(tasks)

  return { results, errors, innovationsRun: enabledInnovations.length }
}

/**
 * Aggregate findings from all innovations into unified metrics
 * @param {Object} innovationResults - Results from runInnovationsParallel
 * @returns {Object} Aggregated findings with priorities
 */
export function aggregateInnovationFindings(innovationResults) {
  const { results } = innovationResults

  // Calculate unified scores
  const scores = {
    gapScore: results.gaps?.gaps?.length > 0 ? Math.min(100, results.gaps.gaps.length * 10) : 0,
    typeScore: results.typeIssues?.mismatches?.length > 0 ? Math.min(100, results.typeIssues.mismatches.length * 15) : 0,
    hotspotScore: results.hotspots?.hotspots?.filter(h => h.risk === "HIGH").length > 0
      ? Math.min(100, results.hotspots.hotspots.filter(h => h.risk === "HIGH").length * 20)
      : 0,
    testScore: 100 - (results.testSuggestions?.coverage || 100),
    docDriftScore: 100 - (results.docDrift?.healthScore || 100),
    dependencyScore: results.dependencyGraph?.issues?.filter(i => i.severity === "critical").length > 0
      ? Math.min(100, results.dependencyGraph.issues.filter(i => i.severity === "critical").length * 30)
      : 0,
    apiContractScore: 100 - (results.apiContracts?.score || 100),
    stackLintScore: 100 - (results.stackLint?.score || 100),
    refactoringScore: results.refactoring?.technicalDebt || 0,
    docCoverageScore: 100 - (results.docSuggestions?.coverage || 100),
  }

  // Calculate overall health (weighted average)
  const weights = {
    gapScore: 0.15,
    typeScore: 0.15,
    hotspotScore: 0.10,
    testScore: 0.15,
    docDriftScore: 0.05,
    dependencyScore: 0.10,
    apiContractScore: 0.10,
    stackLintScore: 0.05,
    refactoringScore: 0.10,
    docCoverageScore: 0.05,
  }

  const overallRisk = Object.entries(scores).reduce((total, [key, score]) => {
    return total + (score * (weights[key] || 0))
  }, 0)

  const overallHealth = Math.round(100 - overallRisk)

  // Prioritize issues
  const allIssues = []

  if (results.gaps?.gaps) {
    results.gaps.gaps.forEach(g => {
      allIssues.push({ type: "gap", severity: g.score > 80 ? "critical" : "medium", item: g })
    })
  }

  if (results.typeIssues?.mismatches) {
    results.typeIssues.mismatches.forEach(m => {
      allIssues.push({ type: "type-mismatch", severity: m.severity, item: m })
    })
  }

  if (results.dependencyGraph?.issues) {
    results.dependencyGraph.issues.forEach(i => {
      allIssues.push({ type: "dependency", severity: i.severity, item: i })
    })
  }

  if (results.apiContracts?.violations) {
    results.apiContracts.violations.forEach(v => {
      allIssues.push({ type: "api-contract", severity: v.severity, item: v })
    })
  }

  if (results.stackLint?.issues) {
    results.stackLint.issues.forEach(i => {
      const severity = i.severity === "error" ? "high" : i.severity === "warning" ? "medium" : "low"
      allIssues.push({ type: "lint", severity, item: i })
    })
  }

  // Sort by severity
  const severityOrder = { critical: 0, high: 1, medium: 2, low: 3 }
  allIssues.sort((a, b) => severityOrder[a.severity] - severityOrder[b.severity])

  return {
    scores,
    overallHealth,
    overallRisk: Math.round(overallRisk),
    totalIssues: allIssues.length,
    criticalIssues: allIssues.filter(i => i.severity === "critical").length,
    highIssues: allIssues.filter(i => i.severity === "high").length,
    prioritizedIssues: allIssues.slice(0, 20),
    summaries: {
      gaps: results.gaps?.summary || "N/A",
      types: results.typeIssues?.summary || "N/A",
      hotspots: results.hotspots?.summary || "N/A",
      tests: results.testSuggestions?.summary || "N/A",
      docDrift: results.docDrift?.summary || "N/A",
      dependencies: results.dependencyGraph?.summary || "N/A",
      apiContracts: results.apiContracts?.summary || "N/A",
      stackLint: results.stackLint?.summary || "N/A",
      refactoring: results.refactoring?.summary || "N/A",
      docSuggestions: results.docSuggestions?.summary || "N/A",
    },
  }
}

/**
 * Run full MAPEK cycle with all 10 innovations
 * @param {Object} options
 * @returns {Promise<Object>} Complete MAPEK result with all findings and decisions
 */
export async function runFullMapekWithAllInnovations(options) {
  const validated = OrchestrationOptionsSchema.parse(options)
  const startTime = Date.now()

  // Phase 1: Monitor - Run all innovations in parallel
  const innovationResults = await runInnovationsParallel(options)

  // Phase 2: Analyze - Aggregate findings
  const aggregated = aggregateInnovationFindings(innovationResults)

  // Phase 3: Plan - Generate decisions based on findings
  const decisions = []

  // Decision: Fix type mismatches
  if (aggregated.scores.typeScore > 50) {
    decisions.push({
      issue: "type-mismatch",
      severity: aggregated.scores.typeScore > 80 ? "critical" : "high",
      action: "sync-zod-ts-types",
      autoFixable: true,
      description: "Sync Zod schemas with TypeScript types",
    })
  }

  // Decision: Generate missing tests
  if (aggregated.scores.testScore > 30) {
    decisions.push({
      issue: "low-test-coverage",
      severity: aggregated.scores.testScore > 60 ? "high" : "medium",
      action: "generate-tests",
      autoFixable: true,
      description: "Generate tests for uncovered files",
    })
  }

  // Decision: Update documentation
  if (aggregated.scores.docDriftScore > 30 || aggregated.scores.docCoverageScore > 30) {
    decisions.push({
      issue: "documentation-drift",
      severity: "medium",
      action: "update-docs",
      autoFixable: false,
      description: "Update out-of-sync documentation",
    })
  }

  // Decision: Fix circular dependencies
  if (innovationResults.results.dependencyGraph?.metrics?.circularCount > 0) {
    decisions.push({
      issue: "circular-dependency",
      severity: "critical",
      action: "refactor-dependencies",
      autoFixable: false,
      description: "Break circular dependency chains",
    })
  }

  // Decision: Fix API contract violations
  if (aggregated.scores.apiContractScore > 40) {
    decisions.push({
      issue: "api-contract-violation",
      severity: aggregated.scores.apiContractScore > 70 ? "critical" : "high",
      action: "fix-api-contracts",
      autoFixable: true,
      description: "Add missing API validation and documentation",
    })
  }

  // Decision: Address stack lint errors
  const lintErrors = innovationResults.results.stackLint?.issues?.filter(i => i.severity === "error").length || 0
  if (lintErrors > 0) {
    decisions.push({
      issue: "lint-errors",
      severity: "high",
      action: "fix-lint-errors",
      autoFixable: true,
      description: lintErrors + " lint errors require fixing",
    })
  }

  // Decision: Refactoring
  if (aggregated.scores.refactoringScore > 40) {
    decisions.push({
      issue: "technical-debt",
      severity: "medium",
      action: "refactor-code",
      autoFixable: false,
      description: "Address high technical debt areas",
    })
  }

  // Phase 4: Execute - Plan actions
  const actions = decisions.filter(d => d.autoFixable).map(d => ({
    type: d.action,
    status: "planned",
    timestamp: new Date().toISOString(),
  }))

  // Phase 5: Knowledge - Extract learnings
  const learnings = {
    timestamp: new Date().toISOString(),
    patterns: {
      highRiskAreas: aggregated.prioritizedIssues.slice(0, 5).map(i => i.type),
      commonIssueTypes: Object.entries(aggregated.scores)
        .filter(([k, v]) => v > 50)
        .map(([k]) => k.replace("Score", "")),
    },
    thresholds: {
      criticalTypeScore: 80,
      criticalGapScore: 80,
      criticalApiScore: 70,
    },
  }

  const duration = Date.now() - startTime

  return {
    phase: "knowledge",
    timestamp: new Date().toISOString(),
    duration,
    overallHealth: aggregated.overallHealth,
    allFindings: innovationResults.results,
    aggregated,
    decisions,
    actions,
    learnings,
    shouldRepeat: aggregated.overallHealth < 70 && actions.length > 0,
    errors: innovationResults.errors,
  }
}

export { ALL_INNOVATIONS }
