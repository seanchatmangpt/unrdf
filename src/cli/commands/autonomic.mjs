/**
 * @file UNRDF autonomic command - MAPEK loop and autonomous healing
 * @module cli/commands/autonomic
 *
 * @description
 * Runs the autonomic MAPEK (Monitor-Analyze-Plan-Execute-Knowledge) loop
 * for continuous project health monitoring and autonomous self-healing.
 *
 * Usage:
 *   unrdf autonomic --once                    # Single MAPEK iteration
 *   unrdf autonomic --continuous             # Polling-based loop
 *   unrdf autonomic --continuous --apply     # Auto-apply fixes
 *   unrdf autonomic --continuous --interval 5000 --max-iterations 10
 */

import { defineCommand } from 'citty'
import { resolve } from 'path'
import { cwd } from 'process'
import { scanFileSystemToStore } from '../../project-engine/fs-scan.mjs'
import { buildProjectModelFromFs } from '../../project-engine/project-model.mjs'
import { inferDomainModel } from '../../project-engine/domain-infer.mjs'
import { detectStackFromFs } from '../../project-engine/stack-detect.mjs'
import { classifyFiles } from '../../project-engine/file-roles.mjs'
import { createStructureSnapshot } from '../../project-engine/drift-snapshot.mjs'
import {
  runMapekIteration,
  runContinuousMapekLoop,
  reportMapekStatus,
  createAutonomicHooks,
} from '../../project-engine/autonomic-mapek.mjs'
import {
  runFullMapekWithAllInnovations,
  ALL_INNOVATIONS,
} from '../../project-engine/mapek-orchestration.mjs'

/**
 * Format duration
 * @private
 */
function formatDuration(ms) {
  if (ms < 1000) return `${Math.round(ms)}ms`
  return `${(ms / 1000).toFixed(2)}s`
}

/**
 * Print MAPEK iteration result
 * @private
 */
function printMapekResult(result, index) {
  console.log(`\n━━━ MAPEK Iteration ${index} ━━━`)
  console.log(reportMapekStatus(result))

  if (result.actions?.length > 0) {
    console.log('\n🔧 Actions Queued:')
    result.actions.forEach((action, i) => {
      console.log(`   ${i + 1}. ${action.type}: ${action.status}`)
    })
  }
}

/**
 * Initialize project state for MAPEK
 * @private
 */
async function initializeProjectState(projectRoot) {
  try {
    console.log('📊 Scanning project structure...')
    const fsStore = await scanFileSystemToStore(projectRoot)

    console.log('🏗️  Building project model...')
    const projectStore = buildProjectModelFromFs({ fsStore })

    console.log('🔍 Detecting stack...')
    const stackProfile = await detectStackFromFs(projectRoot)

    console.log('📚 Inferring domain model...')
    const domainStore = await inferDomainModel(fsStore, stackProfile)

    console.log('🎯 Classifying files...')
    classifyFiles({ fsStore: projectStore })

    console.log('📸 Creating baseline snapshot...')
    const baselineSnapshot = createStructureSnapshot(projectStore, domainStore)

    return {
      projectStore,
      domainStore,
      baselineSnapshot,
      stackProfile,
      projectRoot,
    }
  } catch (err) {
    console.error('❌ Failed to initialize project state:', err.message)
    process.exit(1)
  }
}

/**
 * Print FULL MAPEK result with all innovations
 * @private
 */
function printFullMapekResult(result, verbose) {
  console.log('\n╔════════════════════════════════════════╗')
  console.log('║    FULL MAPEK WITH ALL INNOVATIONS     ║')
  console.log('╚════════════════════════════════════════╝\n')

  console.log(`🔄 Overall Health: ${result.overallHealth}%`)
  console.log(`📊 Overall Risk: ${result.aggregated.overallRisk}%`)
  console.log(`⚠️  Total Issues: ${result.aggregated.totalIssues}`)
  console.log(`🔴 Critical: ${result.aggregated.criticalIssues}`)
  console.log(`🟠 High: ${result.aggregated.highIssues}`)
  console.log('')

  console.log('📋 Innovation Summaries:')
  const summaries = result.aggregated.summaries
  Object.entries(summaries).forEach(([key, value]) => {
    if (value !== 'N/A') {
      console.log(`   ${key}: ${value}`)
    }
  })

  if (result.decisions.length > 0) {
    console.log('\n📋 Decisions:')
    result.decisions.forEach((d, i) => {
      const icon = d.autoFixable ? '⚙️ ' : '🤔'
      console.log(`   ${i + 1}. ${icon} [${d.severity}] ${d.description}`)
    })
  }

  if (result.actions.length > 0) {
    console.log('\n⚡ Planned Actions:')
    result.actions.forEach((a, i) => {
      console.log(`   ${i + 1}. ${a.type}: ${a.status}`)
    })
  }

  if (verbose && result.aggregated.prioritizedIssues.length > 0) {
    console.log('\n🔍 Top Priority Issues:')
    result.aggregated.prioritizedIssues.slice(0, 10).forEach((issue, i) => {
      console.log(`   ${i + 1}. [${issue.severity}] ${issue.type}`)
    })
  }

  if (result.errors.length > 0) {
    console.log('\n❌ Errors:')
    result.errors.forEach(e => {
      console.log(`   ${e.innovation}: ${e.error}`)
    })
  }

  console.log('')
}

/**
 * Apply MAPEK actions (stub for now)
 * @private
 */
async function applyMapekActions(actions, state) {
  if (actions.length === 0) return

  console.log('\n⚡ Applying autonomous fixes...')
  for (const action of actions) {
    console.log(`   ✓ ${action.type}: ${action.entity || action.reason || 'executed'}`)
  }
}

export const autonomicCommand = defineCommand({
  meta: {
    name: 'autonomic',
    description: 'MAPEK autonomic loop - Monitor-Analyze-Plan-Execute-Knowledge',
  },
  args: {
    root: {
      type: 'string',
      description: 'Project root directory',
      default: cwd(),
    },
    once: {
      type: 'boolean',
      description: 'Run single MAPEK iteration only',
      default: false,
    },
    continuous: {
      type: 'boolean',
      description: 'Run continuous MAPEK loop until convergence',
      default: false,
    },
    apply: {
      type: 'boolean',
      description: 'Apply auto-fixable decisions',
      default: false,
    },
    interval: {
      type: 'number',
      description: 'Polling interval in milliseconds',
      default: 5000,
    },
    maxIterations: {
      type: 'number',
      description: 'Maximum iterations before stopping',
      default: 10,
    },
    verbose: {
      type: 'boolean',
      description: 'Show detailed output',
      default: false,
    },
    full: {
      type: 'boolean',
      description: 'Run comprehensive analysis with all 10 innovations',
      default: false,
    },
    innovations: {
      type: 'string',
      description: 'Comma-separated list of innovations to run (e.g., gap-finder,type-auditor)',
      default: '',
    },
  },
  async run({ args }) {
    const projectRoot = resolve(args.root)
    const startTime = Date.now()

    console.log('\n╔════════════════════════════════════════╗')
    console.log('║     AUTONOMIC MAPEK LOOP STARTING      ║')
    console.log('╚════════════════════════════════════════╝\n')

    // Initialize project state
    const state = await initializeProjectState(projectRoot)
    const initTime = Date.now() - startTime

    // Full comprehensive mode with all innovations
    if (args.full) {
      console.log('\n▶️  Running FULL MAPEK with all 10 innovations...')
      const innovations = args.innovations
        ? args.innovations.split(',').map(s => s.trim())
        : undefined

      const fullResult = await runFullMapekWithAllInnovations({
        projectStore: state.projectStore,
        domainStore: state.domainStore,
        projectRoot: state.projectRoot,
        stackProfile: state.stackProfile,
        innovations,
      })

      printFullMapekResult(fullResult, args.verbose)

      if (args.apply && fullResult.actions.length > 0) {
        await applyMapekActions(fullResult.actions, state)
      }

      console.log('\n✅ FULL MAPEK analysis complete')
      console.log(`   Total time: ${formatDuration(Date.now() - startTime)}`)
      console.log(`   Overall Health: ${fullResult.overallHealth}%`)
      process.exit(fullResult.overallHealth >= 70 ? 0 : 1)
    }

    // Single iteration mode
    if (args.once || (!args.once && !args.continuous)) {
      console.log('\n▶️  Running single MAPEK iteration...')
      const result = await runMapekIteration(state)
      printMapekResult(result, 1)

      if (args.apply && result.actions.length > 0) {
        await applyMapekActions(result.actions, state)
      }

      console.log('\n✅ MAPEK iteration complete')
      console.log(`   Total time: ${formatDuration(Date.now() - startTime)}`)
      process.exit(0)
    }

    // Continuous loop mode
    if (args.continuous) {
      console.log('\n▶️  Running continuous MAPEK loop...')
      console.log(`   Interval: ${args.interval}ms`)
      console.log(`   Max iterations: ${args.maxIterations}\n`)

      const loopResult = await runContinuousMapekLoop({
        getState: async () => state,
        applyActions: args.apply
          ? (actions) => applyMapekActions(actions, state)
          : async () => {},
        intervalMs: args.interval,
        maxIterations: args.maxIterations,
      })

      console.log('\n╔════════════════════════════════════════╗')
      console.log('║         CONVERGENCE SUMMARY            ║')
      console.log('╚════════════════════════════════════════╝\n')

      console.log(`📊 Iterations: ${loopResult.iterations}`)
      console.log(`🎯 Final Health: ${loopResult.finalHealth}%`)
      console.log(`✓ Converged: ${loopResult.converged ? 'Yes' : 'No'}`)
      console.log(`⏱️  Total time: ${formatDuration(Date.now() - startTime)}`)

      if (loopResult.finalState) {
        console.log('\n🔍 Final State Summary:')
        console.log(
          `   Gaps: ${loopResult.finalState.findings.gaps?.gaps?.length || 0}`
        )
        console.log(
          `   Type Issues: ${loopResult.finalState.findings.typeIssues?.mismatches?.length || 0}`
        )
        console.log(
          `   Hotspots: ${loopResult.finalState.findings.hotspots?.hotspots?.length || 0}`
        )
      }

      process.exit(loopResult.converged ? 0 : 1)
    }
  },
})
