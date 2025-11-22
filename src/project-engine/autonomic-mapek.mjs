/**
 * @file Autonomic MAPEK Loop - Self-healing knowledge system
 * @module project-engine/autonomic-mapek
 *
 * @description
 * Full autonomic system using MAPEK (Monitor-Analyze-Plan-Execute-Knowledge):
 * - Monitor: Track drift, type mismatches, hotspots continuously
 * - Analyze: Gap finder, type auditor, complexity analysis
 * - Plan: Generate fix plans for detected issues
 * - Execute: Apply fixes via knowledge hooks
 * - Knowledge: Learn patterns and update policies
 *
 * Integrated with UNRDF Knowledge Hooks for autonomous self-healing.
 */

import { z } from 'zod'
import { DataFactory } from 'n3'
import { findMissingRoles, scoreMissingRole } from './gap-finder.mjs'
import { auditTypeConsistency } from './type-auditor.mjs'
import { analyzeHotspots } from './hotspot-analyzer.mjs'
import { computeDrift } from './drift-snapshot.mjs'
import { planMaterialization, validatePlan } from './materialize-plan.mjs'
import { deriveHooksFromStructure } from './policy-derivation.mjs'

const { namedNode, literal } = DataFactory

const MapekStateSchema = z.object({
  phase: z.enum(['monitor', 'analyze', 'plan', 'execute', 'knowledge']),
  timestamp: z.string().datetime(),
  findings: z.object({}).passthrough(),
  actions: z.array(z.object({}).passthrough()).default([]),
  metrics: z.object({
    gapScore: z.number().default(0),
    typeScore: z.number().default(0),
    hotspotScore: z.number().default(0),
    driftSeverity: z.string().default('none'),
  }),
})

/**
 * Autonomic MAPEK Loop - Single iteration
 *
 * @param {Object} options
 * @param {Store} options.projectStore - RDF project structure
 * @param {Store} options.domainStore - RDF domain model
 * @param {Store} options.baselineSnapshot - From createStructureSnapshot
 * @param {string} options.projectRoot - File system root
 * @param {Object} options.stackProfile - Stack detection result
 * @param {Object} options.knowledge - Learned patterns from previous runs
 * @returns {Object} { phase, findings, actions, nextPhase, shouldRepeat }
 */
export async function runMapekIteration(options) {
  const validated = z
    .object({
      projectStore: z.object({}).passthrough(),
      domainStore: z.object({}).passthrough(),
      baselineSnapshot: z.object({}).passthrough().optional(),
      projectRoot: z.string(),
      stackProfile: z.object({}).passthrough().optional(),
      knowledge: z.object({}).passthrough().optional(),
    })
    .parse(options)

  const state = {
    phase: 'monitor',
    timestamp: new Date().toISOString(),
    findings: {},
    actions: [],
    metrics: {
      gapScore: 0,
      typeScore: 0,
      hotspotScore: 0,
      driftSeverity: 'none',
    },
  }

  // ===== PHASE 1: MONITOR =====
  // Continuously observe system state
  state.findings.gaps = findMissingRoles({
    domainStore: validated.domainStore,
    projectStore: validated.projectStore,
    stackProfile: validated.stackProfile,
  })

  state.findings.typeIssues = await auditTypeConsistency({
    domainStore: validated.domainStore,
    fsStore: validated.projectStore,
    stackProfile: validated.stackProfile,
    projectRoot: validated.projectRoot,
  })

  state.findings.hotspots = analyzeHotspots({
    projectStore: validated.projectStore,
    domainStore: validated.domainStore,
    stackProfile: validated.stackProfile,
  })

  if (validated.baselineSnapshot) {
    state.findings.drift = computeDrift(validated.projectStore, validated.baselineSnapshot)
  }

  // ===== PHASE 2: ANALYZE =====
  // Interpret findings and calculate health metrics
  state.phase = 'analyze'

  // Gap severity (0-100, higher = more missing)
  const gapCount = state.findings.gaps?.gaps?.length || 0
  const criticalGaps = state.findings.gaps?.gaps?.filter((g) => g.score > 80).length || 0
  state.metrics.gapScore = Math.min(100, gapCount * 10 + criticalGaps * 20)

  // Type safety score (0-100, higher = more problems)
  const typeCount = state.findings.typeIssues?.mismatches?.length || 0
  const highSeverityTypes = state.findings.typeIssues?.mismatches?.filter(
    (m) => m.severity === 'high'
  ).length || 0
  state.metrics.typeScore = Math.min(100, typeCount * 15 + highSeverityTypes * 25)

  // Hotspot score (average risk of identified hotspots)
  const hotspotCount = state.findings.hotspots?.hotspots?.length || 0
  const highRiskCount = state.findings.hotspots?.hotspots?.filter(
    (h) => h.risk === 'HIGH'
  ).length || 0
  state.metrics.hotspotScore = Math.min(100, hotspotCount * 5 + highRiskCount * 30)

  // Drift severity (from computeDrift)
  state.metrics.driftSeverity = state.findings.drift?.driftSeverity || 'none'

  // Overall health
  const overallHealth = (
    state.metrics.gapScore * 0.3 +
    state.metrics.typeScore * 0.3 +
    state.metrics.hotspotScore * 0.2 +
    (state.metrics.driftSeverity === 'major' ? 50 : state.metrics.driftSeverity === 'minor' ? 25 : 0) *
      0.2
  ) / 100

  // ===== PHASE 3: PLAN =====
  // Decide what to do about findings
  state.phase = 'plan'

  const decisions = []

  // Decision 1: Fix type mismatches (highest priority)
  if (state.metrics.typeScore > 60) {
    decisions.push({
      issue: 'type-mismatch',
      severity: 'critical',
      action: 'sync-zod-ts-types',
      description: `${typeCount} type mismatches detected. Sync Zod schemas with TypeScript types.`,
      autoFixable: true,
    })
  }

  // Decision 2: Fill gaps (medium priority)
  if (state.metrics.gapScore > 50) {
    const topGaps = state.findings.gaps?.gaps?.slice(0, 3) || []
    decisions.push({
      issue: 'missing-roles',
      severity: criticalGaps > 0 ? 'high' : 'medium',
      action: 'generate-missing-files',
      targets: topGaps.map((g) => ({
        entity: g.entity,
        roles: g.missingRoles,
        score: g.score,
      })),
      autoFixable: true,
      description: `Generate ${topGaps.length} missing files for ${topGaps.map((g) => g.entity).join(', ')}`,
    })
  }

  // Decision 3: Refactor hotspots (low priority, requires human review)
  if (state.metrics.hotspotScore > 70) {
    const topRisks = state.findings.hotspots?.topRisks?.slice(0, 2) || []
    decisions.push({
      issue: 'high-complexity',
      severity: 'medium',
      action: 'refactor-hotspots',
      targets: topRisks,
      autoFixable: false, // Requires human decision
      description: `Features ${topRisks.map((r) => r.feature).join(', ')} have high complexity.`,
      recommendation: 'Add tests or refactor into smaller modules.',
    })
  }

  // Decision 4: Fix drift (if baseline exists)
  if (state.metrics.driftSeverity === 'major') {
    decisions.push({
      issue: 'major-drift',
      severity: 'high',
      action: 'resync-model',
      autoFixable: true,
      description: 'Project structure has drifted significantly from baseline.',
      recommendation: 'Run: unrdf init --skip-snapshot to resync',
    })
  }

  state.findings.decisions = decisions

  // ===== PHASE 4: EXECUTE =====
  // Apply fixes autonomously (for auto-fixable issues)
  state.phase = 'execute'

  for (const decision of decisions) {
    if (decision.autoFixable) {
      if (decision.action === 'generate-missing-files') {
        // Plan and queue file generation
        const targets = decision.targets || []
        for (const target of targets) {
          state.actions.push({
            type: 'generate-files',
            entity: target.entity,
            roles: target.roles,
            status: 'planned',
            timestamp: new Date().toISOString(),
          })
        }
      } else if (decision.action === 'sync-zod-ts-types') {
        state.actions.push({
          type: 'sync-types',
          mismatches: state.findings.typeIssues?.mismatches?.length || 0,
          status: 'planned',
          timestamp: new Date().toISOString(),
        })
      } else if (decision.action === 'resync-model') {
        state.actions.push({
          type: 'reinitialize',
          reason: 'drift-recovery',
          status: 'planned',
          timestamp: new Date().toISOString(),
        })
      }
    }
  }

  // ===== PHASE 5: KNOWLEDGE =====
  // Learn from outcomes and update policies
  state.phase = 'knowledge'

  // Extract learning
  const learnings = {
    timestamp: new Date().toISOString(),
    gapPatterns: (state.findings.gaps?.gaps || [])
      .filter((g) => g.score > 80)
      .map((g) => ({
        entity: g.entity,
        missingRoles: g.missingRoles,
        frequency: 1,
      })),
    typePatterns: (state.findings.typeIssues?.mismatches || [])
      .filter((m) => m.severity === 'high')
      .map((m) => ({
        entity: m.entity,
        issueType: 'type-mismatch',
        frequency: m.issues?.length || 1,
      })),
    hotspotThresholds: {
      fileCountHighRisk: 40,
      testCoverageHighRisk: 70,
      dependenciesHighRisk: 12,
    },
  }

  state.findings.learnings = learnings

  // Determine if another iteration is needed
  const shouldRepeat = overallHealth < 0.7 && state.actions.length > 0

  return {
    state,
    overallHealth: Math.round(overallHealth * 100),
    phase: state.phase,
    findings: state.findings,
    decisions: decisions.filter((d) => d.autoFixable),
    actions: state.actions,
    learnings,
    shouldRepeat,
  }
}

/**
 * Create Knowledge Hooks for autonomous execution
 *
 * @param {Object} mapekFindings - From runMapekIteration
 * @param {Store} projectStore - Project RDF store
 * @returns {Hook[]} Array of hooks for KnowledgeHookManager
 */
export function createAutonomicHooks(mapekFindings, projectStore) {
  const hooks = []

  // Hook 1: Auto-generate missing files on gap detection
  if (mapekFindings.findings.gaps?.gaps?.some((g) => g.score > 80)) {
    hooks.push({
      meta: {
        name: 'autonomic:auto-generate-missing-files',
        description: 'Autonomously generate missing files for high-score gaps',
        source: 'mapek:execute',
      },
      channel: { graphs: ['urn:graph:project'], view: 'after' },
      when: {
        kind: 'sparql-ask',
        query: `
          ASK {
            ?entity rdf:type dom:Entity .
            FILTER NOT EXISTS { ?file project:belongsToFeature ?entity . }
          }
        `,
      },
      run: async ({ payload, context }) => {
        // Trigger file generation for missing entity
        return {
          result: { action: 'generate', entity: payload.subject },
        }
      },
    })
  }

  // Hook 2: Auto-sync types when mismatch detected
  if (mapekFindings.findings.typeIssues?.mismatches?.some((m) => m.severity === 'high')) {
    hooks.push({
      meta: {
        name: 'autonomic:auto-sync-types',
        description: 'Autonomously sync Zod schemas with TypeScript types',
        source: 'mapek:execute',
      },
      channel: { graphs: ['urn:graph:project'], view: 'after' },
      when: {
        kind: 'custom',
      },
      run: async ({ payload, context }) => {
        // Detect type change and sync
        return {
          result: { action: 'sync', type: 'zod-ts' },
        }
      },
    })
  }

  // Hook 3: Alert on hotspots (for human review)
  if (mapekFindings.findings.hotspots?.hotspots?.some((h) => h.risk === 'HIGH')) {
    hooks.push({
      meta: {
        name: 'autonomic:hotspot-alert',
        description: 'Alert on high-risk features that need attention',
        source: 'mapek:execute',
      },
      channel: { graphs: ['urn:graph:project'], view: 'after' },
      when: {
        kind: 'custom',
      },
      run: async ({ payload, context }) => {
        const topRisk = mapekFindings.findings.hotspots.topRisks[0]
        return {
          result: {
            action: 'alert',
            feature: topRisk.feature,
            reason: topRisk.reason,
            level: 'warning',
          },
        }
      },
    })
  }

  return hooks
}

/**
 * Continuous autonomic loop (polling-based)
 *
 * @param {Object} options
 * @param {Function} options.getState - Function to get current project state
 * @param {Function} options.applyActions - Function to apply decisions
 * @param {number} [options.intervalMs] - Polling interval (default 5000)
 * @param {number} [options.maxIterations] - Stop after N iterations (default: run forever)
 * @returns {Promise<Object>} Final state after convergence
 */
export async function runContinuousMapekLoop(options) {
  const {
    getState,
    applyActions,
    intervalMs = 5000,
    maxIterations = 10,
  } = z
    .object({
      getState: z.function(),
      applyActions: z.function(),
      intervalMs: z.number().default(5000),
      maxIterations: z.number().default(10),
    })
    .parse(options)

  let iteration = 0
  let lastState = null
  let converged = false

  while (iteration < maxIterations && !converged) {
    iteration++

    // Get current state
    const state = await getState()

    // Run MAPEK iteration
    const result = await runMapekIteration(state)

    // Apply auto-fixable actions
    if (result.actions.length > 0) {
      await applyActions(result.actions, state)
    }

    lastState = result

    // Check convergence
    if (result.overallHealth > 80) {
      converged = true
    } else if (!result.shouldRepeat) {
      converged = true
    }

    // Wait before next iteration
    if (!converged && iteration < maxIterations) {
      await new Promise((resolve) => setTimeout(resolve, intervalMs))
    }
  }

  return {
    converged,
    iterations: iteration,
    finalHealth: lastState?.overallHealth || 0,
    finalState: lastState,
  }
}

/**
 * MAPEK Status reporter
 *
 * @param {Object} mapekState - From runMapekIteration
 * @returns {string} Human-readable status report
 */
export function reportMapekStatus(mapekState) {
  const lines = [
    '\n╔════════════════════════════════════════╗',
    '║         AUTONOMIC SYSTEM STATUS        ║',
    '╚════════════════════════════════════════╝\n',
    `🔄 Overall Health: ${mapekState.overallHealth}%`,
    '',
    '📊 Metrics:',
    `   Gap Score: ${mapekState.metrics.gapScore}/100`,
    `   Type Score: ${mapekState.metrics.typeScore}/100`,
    `   Hotspot Score: ${mapekState.metrics.hotspotScore}/100`,
    `   Drift: ${mapekState.metrics.driftSeverity}`,
    '',
    '🔍 Findings:',
    `   Gaps: ${mapekState.findings.gaps?.gaps?.length || 0} missing roles`,
    `   Type Issues: ${mapekState.findings.typeIssues?.mismatches?.length || 0} mismatches`,
    `   Hotspots: ${mapekState.findings.hotspots?.hotspots?.length || 0} high-risk features`,
  ]

  if (mapekState.findings.decisions?.length > 0) {
    lines.push('')
    lines.push('📋 Decisions:')
    mapekState.findings.decisions.forEach((d) => {
      const icon = d.autoFixable ? '⚙️ ' : '🤔'
      lines.push(`   ${icon} ${d.description}`)
    })
  }

  if (mapekState.actions?.length > 0) {
    lines.push('')
    lines.push('⚡ Planned Actions:')
    mapekState.actions.forEach((a) => {
      lines.push(`   → ${a.type}: ${a.status}`)
    })
  }

  lines.push('')
  return lines.join('\n')
}

export { findMissingRoles, scoreMissingRole } from './gap-finder.mjs'
export { auditTypeConsistency, compareTypes } from './type-auditor.mjs'
export { analyzeHotspots, scoreFeature } from './hotspot-analyzer.mjs'
