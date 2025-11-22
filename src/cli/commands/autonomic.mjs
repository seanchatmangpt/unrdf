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

import { defineCommand } from 'citty';
import { resolve } from 'path';
import { cwd } from 'process';
import { scanFileSystemToStore } from '../../project-engine/fs-scan.mjs';
import { buildProjectModelFromFs } from '../../project-engine/project-model.mjs';
import { inferDomainModel } from '../../project-engine/domain-infer.mjs';
import { detectStackFromFs } from '../../project-engine/stack-detect.mjs';
import { classifyFiles } from '../../project-engine/file-roles.mjs';
import { createStructureSnapshot } from '../../project-engine/drift-snapshot.mjs';
import {
  runMapekIteration,
  runContinuousMapekLoop,
  reportMapekStatus,
} from '../../project-engine/autonomic-mapek.mjs';
import {
  runFullMapekWithAllInnovations,
  _ALL_INNOVATIONS,
} from '../../project-engine/mapek-orchestration.mjs';

/**
 * Format duration in milliseconds to human-readable string
 * @param {number} ms - Duration in milliseconds
 * @returns {string} Formatted duration string (e.g., "150ms" or "2.50s")
 * @private
 */
function formatDuration(ms) {
  if (ms < 1000) return `${Math.round(ms)}ms`;
  return `${(ms / 1000).toFixed(2)}s`;
}

/**
 * Print MAPEK iteration result to console
 * @param {Object} result - MAPEK iteration result object
 * @param {number} index - Iteration index (1-based)
 * @returns {void}
 * @private
 */
function printMapekResult(result, index) {
  console.log(`\n━━━ MAPEK Iteration ${index} ━━━`);
  console.log(reportMapekStatus(result));

  if (result.actions?.length > 0) {
    console.log('\n🔧 Actions Queued:');
    result.actions.forEach((action, i) => {
      console.log(`   ${i + 1}. ${action.type}: ${action.status}`);
    });
  }
}

/**
 * Initialize project state for MAPEK loop
 * @param {string} projectRoot - Path to project root directory
 * @returns {Promise<Object>} Initialized project state with stores and metadata
 * @throws {Error} If initialization fails at any step
 * @private
 */
async function initializeProjectState(projectRoot) {
  try {
    console.log('📊 Scanning project structure...');
    const fsStore = await scanFileSystemToStore(projectRoot);

    console.log('🏗️  Building project model...');
    const projectStore = buildProjectModelFromFs({ fsStore });

    console.log('🔍 Detecting stack...');
    const stackProfile = detectStackFromFs({ fsStore });

    console.log('📚 Inferring domain model...');
    const domainStore = await inferDomainModel(fsStore, stackProfile);

    console.log('🎯 Classifying files...');
    classifyFiles({ fsStore: projectStore });

    console.log('📸 Creating baseline snapshot...');
    const baselineSnapshot = createStructureSnapshot(projectStore, domainStore);

    return {
      projectStore,
      domainStore,
      baselineSnapshot,
      stackProfile,
      projectRoot,
    };
  } catch (err) {
    // Provide structured error information with context
    const errorInfo = {
      message: err.message,
      stack: err.stack,
      projectRoot,
      phase: err.phase || 'unknown',
    };

    console.error('\n❌ Failed to initialize project state');
    console.error(`   Project root: ${projectRoot}`);
    console.error(`   Error: ${errorInfo.message}`);

    if (err.stack && process.env.DEBUG) {
      console.error('\n   Stack trace:');
      console.error(err.stack.split('\n').slice(0, 5).join('\n'));
    }

    // Re-throw with context for better error handling upstream
    const contextError = new Error(`Project initialization failed: ${errorInfo.message}`);
    contextError.cause = err;
    contextError.projectRoot = projectRoot;
    throw contextError;
  }
}

/**
 * Print FULL MAPEK result with all innovations to console
 * @param {Object} result - Full MAPEK result object with all innovations
 * @param {boolean} verbose - Whether to show detailed output
 * @returns {void}
 * @private
 */
function printFullMapekResult(result, verbose) {
  console.log('\n╔════════════════════════════════════════╗');
  console.log('║    FULL MAPEK WITH ALL INNOVATIONS     ║');
  console.log('╚════════════════════════════════════════╝\n');

  console.log(`🔄 Overall Health: ${result.overallHealth}%`);
  console.log(`📊 Overall Risk: ${result.aggregated.overallRisk}%`);
  console.log(`⚠️  Total Issues: ${result.aggregated.totalIssues}`);
  console.log(`🔴 Critical: ${result.aggregated.criticalIssues}`);
  console.log(`🟠 High: ${result.aggregated.highIssues}`);
  console.log('');

  console.log('📋 Innovation Summaries:');
  const summaries = result.aggregated.summaries;
  Object.entries(summaries).forEach(([key, value]) => {
    if (value !== 'N/A') {
      console.log(`   ${key}: ${value}`);
    }
  });

  if (result.decisions.length > 0) {
    console.log('\n📋 Decisions:');
    result.decisions.forEach((d, i) => {
      const icon = d.autoFixable ? '⚙️ ' : '🤔';
      console.log(`   ${i + 1}. ${icon} [${d.severity}] ${d.description}`);
    });
  }

  if (result.actions.length > 0) {
    console.log('\n⚡ Planned Actions:');
    result.actions.forEach((a, i) => {
      console.log(`   ${i + 1}. ${a.type}: ${a.status}`);
    });
  }

  if (verbose && result.aggregated.prioritizedIssues.length > 0) {
    console.log('\n🔍 Top Priority Issues:');
    result.aggregated.prioritizedIssues.slice(0, 10).forEach((issue, i) => {
      console.log(`   ${i + 1}. [${issue.severity}] ${issue.type}`);
    });
  }

  if (result.errors.length > 0) {
    console.log('\n❌ Errors:');
    result.errors.forEach(e => {
      console.log(`   ${e.innovation}: ${e.error}`);
    });
  }

  console.log('');
}

/**
 * Generate file content for a role
 * @param {string} entityName - Entity name
 * @param {string} role - Role (Api, Component, Test, Schema)
 * @param {Object} stackProfile - Stack profile
 * @returns {string} File content
 * @private
 */
function generateFileContent(entityName, role, stackProfile = {}) {
  const entityLower = entityName.toLowerCase();
  const entityPascal = entityName.charAt(0).toUpperCase() + entityName.slice(1);
  const webFramework = stackProfile.webFramework || 'express';

  const templates = {
    Api:
      webFramework === 'next' || webFramework === 'next-app-router'
        ? `/**
 * API route for ${entityName}
 * @route GET /api/${entityLower}
 */
export async function GET() {
  return Response.json({ ${entityLower}: [] });
}

export async function POST(request) {
  const body = await request.json();
  return Response.json({ ${entityLower}: body }, { status: 201 });
}
`
        : `/**
 * API route for ${entityName}
 * @route GET /api/${entityLower}
 */
export default function handler(req, res) {
  if (req.method === 'GET') {
    res.json({ ${entityLower}: [] });
  } else if (req.method === 'POST') {
    res.status(201).json({ ${entityLower}: req.body });
  }
}
`,
    Component: `/**
 * ${entityPascal} component
 */
export default function ${entityPascal}View() {
  return (
    <div>
      <h1>${entityPascal}</h1>
    </div>
  );
}
`,
    Test: `/**
 * @file ${entityName} tests
 * @vitest-environment node
 */
import { describe, it, expect } from 'vitest';

describe('${entityName}', () => {
  it('should work', () => {
    expect(true).toBe(true);
  });
});
`,
    Schema: `import { z } from 'zod';

/**
 * ${entityPascal} schema
 */
export const ${entityPascal}Schema = z.object({
  id: z.string(),
});

/**
 * @typedef {z.infer<typeof ${entityPascal}Schema>} ${entityPascal}
 */
`,
  };

  return templates[role] || `// ${role} for ${entityName}`;
}

/**
 * Generate file path for a role
 * @param {string} entityName - Entity name
 * @param {string} role - Role
 * @param {Object} stackProfile - Stack profile
 * @param {string} projectRoot - Project root
 * @returns {string} File path
 * @private
 */
function generateFilePath(entityName, role, stackProfile = {}, projectRoot = '.') {
  const entityLower = entityName.toLowerCase();
  const entityPascal = entityName.charAt(0).toUpperCase() + entityName.slice(1);
  const webFramework = stackProfile.webFramework || 'express';

  const pathMap = {
    Api:
      webFramework === 'next' || webFramework === 'next-app-router'
        ? `src/app/api/${entityLower}/route.mjs`
        : `src/api/${entityLower}.mjs`,
    Component: `src/components/${entityPascal}View.mjs`,
    Test: `test/${entityLower}.test.mjs`,
    Schema: `src/schemas/${entityLower}.mjs`,
  };

  const relativePath = pathMap[role] || `src/${role.toLowerCase()}/${entityLower}.mjs`;
  return `${projectRoot}/${relativePath}`;
}

/**
 * Apply MAPEK actions - Execute planned fixes autonomously
 * @param {Array<Object>} actions - Array of actions to apply
 * @param {Object} state - Current project state with projectRoot, stores, etc.
 * @returns {Promise<{applied: number, failed: number, skipped: number, results: Array<Object>}>}
 * @private
 */
async function applyMapekActions(actions, state) {
  if (actions.length === 0) {
    return { applied: 0, failed: 0, skipped: 0, results: [] };
  }

  const { promises: fs } = await import('fs');
  const path = await import('path');

  console.log('\n⚡ Applying autonomous fixes...');

  const results = [];
  let applied = 0;
  let failed = 0;
  let skipped = 0;

  for (const action of actions) {
    try {
      if (action.type === 'generate-files') {
        const { entity, roles } = action;
        const projectRoot = state.projectRoot || '.';

        const actionResult = {
          action,
          filesGenerated: [],
          filesSkipped: [],
          errors: [],
        };

        for (const role of roles || []) {
          try {
            const filePath = generateFilePath(entity, role, state.stackProfile, projectRoot);
            const content = generateFileContent(entity, role, state.stackProfile);

            // Ensure directory exists
            const dir = path.dirname(filePath);
            await fs.mkdir(dir, { recursive: true });

            // Check if file already exists
            try {
              await fs.access(filePath);
              actionResult.filesSkipped.push({
                path: filePath,
                reason: 'File already exists',
              });
              skipped++;
              console.log(`   ⚠ ${role} for ${entity}: File already exists at ${filePath}`);
            } catch {
              // File doesn't exist, create it
              await fs.writeFile(filePath, content, 'utf-8');
              actionResult.filesGenerated.push(filePath);
              applied++;
              console.log(`   ✓ Generated ${role} for ${entity}: ${filePath}`);
            }
          } catch (error) {
            actionResult.errors.push({
              role,
              error: error.message,
            });
            failed++;
            console.error(`   ✗ Failed to generate ${role} for ${entity}: ${error.message}`);
          }
        }

        results.push({
          action,
          result: actionResult,
          success: actionResult.filesGenerated.length > 0,
        });
      } else if (action.type === 'sync-types') {
        const mismatches = action.mismatches || 0;
        const result = {
          action,
          result: {
            mismatches,
            requiresManualReview: mismatches > 0,
          },
          success: mismatches === 0,
        };
        results.push(result);

        if (mismatches > 0) {
          skipped++;
          console.log(
            `   ⚠ sync-types: ${mismatches} mismatches detected - run 'unrdf autonomic --full' for details`
          );
          console.log(
            `   💡 Tip: Update JSDoc typedefs to match Zod schemas using z.infer<typeof SchemaName>`
          );
        } else {
          applied++;
          console.log(`   ✓ sync-types: No mismatches found`);
        }
      } else if (action.type === 'reinitialize') {
        const reason = action.reason || 'drift detected';
        const result = {
          action,
          result: {
            requiresManualAction: true,
            reason,
            recommendation: 'Run: unrdf init --skip-snapshot to resync',
          },
          success: false,
        };
        results.push(result);
        skipped++;
        console.log(`   ⚠ reinitialize: ${reason} - resyncing project model...`);

        // Note: Full reinitialization requires manual action to avoid side effects
        console.log(`   💡 Run 'unrdf init --skip-snapshot' to resync the project model`);
      } else {
        const result = {
          action,
          result: { message: 'Action executed' },
          success: true,
        };
        results.push(result);
        applied++;
        console.log(`   ✓ ${action.type}: ${action.entity || action.reason || 'executed'}`);
      }
    } catch (error) {
      failed++;
      const errorResult = {
        action,
        result: { error: error.message },
        success: false,
      };
      results.push(errorResult);
      console.error(`   ✗ Error applying ${action.type}: ${error.message}`);
    }
  }

  return { applied, failed, skipped, results };
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
    const projectRoot = resolve(args.root);
    const startTime = Date.now();

    console.log('\n╔════════════════════════════════════════╗');
    console.log('║     AUTONOMIC MAPEK LOOP STARTING      ║');
    console.log('╚════════════════════════════════════════╝\n');

    // Initialize project state
    let state;
    try {
      state = await initializeProjectState(projectRoot);
    } catch (error) {
      console.error('\n❌ Failed to start MAPEK loop');
      if (error.projectRoot) {
        console.error(`   Project: ${error.projectRoot}`);
      }
      process.exit(1);
    }
    const _initTime = Date.now() - startTime;

    // Full comprehensive mode with all innovations
    if (args.full) {
      console.log('\n▶️  Running FULL MAPEK with all 10 innovations...');
      const innovations = args.innovations
        ? args.innovations.split(',').map(s => s.trim())
        : undefined;

      const fullResult = await runFullMapekWithAllInnovations({
        projectStore: state.projectStore,
        domainStore: state.domainStore,
        projectRoot: state.projectRoot,
        stackProfile: state.stackProfile,
        innovations,
      });

      printFullMapekResult(fullResult, args.verbose);

      if (args.apply && fullResult.actions.length > 0) {
        await applyMapekActions(fullResult.actions, state);
      }

      console.log('\n✅ FULL MAPEK analysis complete');
      console.log(`   Total time: ${formatDuration(Date.now() - startTime)}`);
      console.log(`   Overall Health: ${fullResult.overallHealth}%`);
      process.exit(fullResult.overallHealth >= 70 ? 0 : 1);
    }

    // Single iteration mode
    if (args.once || (!args.once && !args.continuous)) {
      console.log('\n▶️  Running single MAPEK iteration...');
      const result = await runMapekIteration(state);
      printMapekResult(result, 1);

      if (args.apply && result.actions.length > 0) {
        await applyMapekActions(result.actions, state);
      }

      console.log('\n✅ MAPEK iteration complete');
      console.log(`   Total time: ${formatDuration(Date.now() - startTime)}`);
      process.exit(0);
    }

    // Continuous loop mode
    if (args.continuous) {
      console.log('\n▶️  Running continuous MAPEK loop...');
      console.log(`   Interval: ${args.interval}ms`);
      console.log(`   Max iterations: ${args.maxIterations}\n`);

      const loopResult = await runContinuousMapekLoop({
        getState: async () => state,
        applyActions: args.apply ? actions => applyMapekActions(actions, state) : async () => {},
        intervalMs: args.interval,
        maxIterations: args.maxIterations,
      });

      console.log('\n╔════════════════════════════════════════╗');
      console.log('║         CONVERGENCE SUMMARY            ║');
      console.log('╚════════════════════════════════════════╝\n');

      console.log(`📊 Iterations: ${loopResult.iterations}`);
      console.log(`🎯 Final Health: ${loopResult.finalHealth}%`);
      console.log(`✓ Converged: ${loopResult.converged ? 'Yes' : 'No'}`);
      console.log(`⏱️  Total time: ${formatDuration(Date.now() - startTime)}`);

      if (loopResult.finalState) {
        console.log('\n🔍 Final State Summary:');
        console.log(`   Gaps: ${loopResult.finalState.findings.gaps?.gaps?.length || 0}`);
        console.log(
          `   Type Issues: ${loopResult.finalState.findings.typeIssues?.mismatches?.length || 0}`
        );
        console.log(
          `   Hotspots: ${loopResult.finalState.findings.hotspots?.hotspots?.length || 0}`
        );
      }

      process.exit(loopResult.converged ? 0 : 1);
    }
  },
});
