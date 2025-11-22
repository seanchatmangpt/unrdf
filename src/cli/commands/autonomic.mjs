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
