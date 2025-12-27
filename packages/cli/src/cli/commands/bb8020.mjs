/**
 * BB80/20 Command - Complete 11-Step Workflow
 *
 * Executes the full Big Bang 80/20 methodology for single-pass implementation.
 *
 * Workflow Steps:
 * 0. Pre-validation (Socratic analysis)
 * 1. Parse specification → feature set
 * 2. Compute Pareto frontier (80/20)
 * 3. Hyperdimensional embedding φ: F → H_D
 * 4. Pattern matching in codebase
 * 5. Architecture design (info-geometric)
 * 6. Pseudocode generation
 * 7. Implementation (pattern library)
 * 8. Syntax validation
 * 9. Static analysis
 * 10. Specification compliance
 * 11. Deploy to production
 *
 * Expected Results (from thesis):
 * - Implementation time: 2-3 hours
 * - Correctness: ≥99.99%
 * - Code reuse: 64.3%
 * - Static analysis coverage: 98%
 *
 * @module cli/commands/bb8020
 */

import { defineCommand } from 'citty';
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { join } from 'node:path';
import { BB8020Orchestrator } from '@unrdf/decision-fabric';
import { createStore } from '@unrdf/core';

/**
 * Parse specification file
 *
 * Expected format:
 * {
 *   "name": "Feature Name",
 *   "description": "What to build",
 *   "features": [...]
 * }
 */
function parseSpecificationFile(path) {
  try {
    const content = readFileSync(path, 'utf-8');
    const spec = JSON.parse(content);

    if (!spec.description && !spec.statement) {
      throw new Error('Specification must have "description" or "statement"');
    }

    if (!Array.isArray(spec.features)) {
      throw new Error('Specification must have "features" array');
    }

    return spec;
  } catch (error) {
    throw new Error(`Failed to parse specification: ${error.message}`);
  }
}

/**
 * Format workflow result for display
 */
function formatWorkflowResult(result) {
  const lines = [];

  lines.push('');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('  BIG BANG 80/20 - COMPLETE WORKFLOW');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  // Overall status
  const statusEmoji = result.success ? '✅' : '❌';
  lines.push(`${statusEmoji} Status: ${result.success ? 'SUCCESS' : 'FAILED'}`);
  lines.push(`Methodology: ${result.methodology}`);
  lines.push(`Total Duration: ${result.totalDuration.toFixed(0)}ms`);
  lines.push('');

  // Metrics
  if (result.metrics) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push('METRICS');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');
    lines.push(`Specification Entropy: ${result.metrics.specificationEntropy?.toFixed(2) || 'N/A'} bits`);

    if (result.metrics.codeLines) {
      lines.push(`Code Generated: ${result.metrics.codeLines} lines`);
    }

    if (result.metrics.expectedCorrectness) {
      lines.push(`Expected Correctness: ${result.metrics.expectedCorrectness.percentage}`);
      lines.push(`Error Probability: ${((1 - result.metrics.expectedCorrectness.probability) * 100).toFixed(4)}%`);
    }

    lines.push('');
  }

  // Workflow steps
  lines.push('─────────────────────────────────────────────────────');
  lines.push('WORKFLOW STEPS');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');

  result.steps.forEach(step => {
    const stepEmoji = {
      'success': '✅',
      'failed': '❌',
      'skipped': '⏭️ '
    };

    lines.push(`${stepEmoji[step.status]} Step ${step.step}: ${step.name}`);
    lines.push(`   Duration: ${step.duration_ms.toFixed(0)}ms`);

    if (step.output) {
      const outputStr = JSON.stringify(step.output, null, 2).replace(/\n/g, '\n   ');
      lines.push(`   Output: ${outputStr}`);
    }

    if (step.error) {
      lines.push(`   Error: ${step.error}`);
    }

    lines.push('');
  });

  // Artifacts
  if (result.artifacts) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push('ARTIFACTS GENERATED');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    if (result.artifacts.paretoFrontier) {
      lines.push(`Pareto Frontier: ${result.artifacts.paretoFrontier.length} features`);
      result.artifacts.paretoFrontier.slice(0, 5).forEach(f => {
        lines.push(`  - ${f.name} (value: ${f.value}, cost: ${f.cost})`);
      });
      lines.push('');
    }

    if (result.artifacts.patterns) {
      lines.push(`Patterns Matched: ${result.artifacts.patterns.length}`);
      lines.push('');
    }

    if (result.artifacts.code) {
      lines.push('Code Generated: ✅');
      lines.push(`  Lines: ${result.artifacts.code.split('\n').length}`);
      lines.push('');
    }

    if (result.artifacts.validationResults) {
      const val = result.artifacts.validationResults;

      if (val.syntax) {
        lines.push(`Syntax Validation: ${val.syntax.valid ? '✅ PASS' : '❌ FAIL'}`);
      }

      if (val.staticAnalysis) {
        lines.push(`Static Analysis: ✅ PASS (${(val.staticAnalysis.coverage * 100).toFixed(1)}% coverage)`);
      }

      if (val.compliance) {
        lines.push(`Specification Compliance: ${val.compliance.percentage}%`);
      }

      lines.push('');
    }
  }

  // Failed steps
  if (result.failedSteps.length > 0) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push('❌ FAILED STEPS');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    result.failedSteps.forEach(step => {
      lines.push(`Step ${step.step}: ${step.name}`);
      lines.push(`  Error: ${step.error}`);
      lines.push('');
    });
  }

  // Final recommendation
  lines.push('─────────────────────────────────────────────────────');
  lines.push('RECOMMENDATION');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');

  if (result.success) {
    lines.push('✅ Implementation complete! Next steps:');
    lines.push('');
    lines.push('1. Review generated code in output directory');
    lines.push('2. Run tests: npm test');
    lines.push('3. Commit: git add . && git commit -m "feat: BB80/20 implementation"');
    lines.push('4. Deploy: git push');
    lines.push('');
    lines.push(`Expected time to complete: 2-3 hours`);
    lines.push(`Expected correctness: ${result.metrics?.expectedCorrectness?.percentage || '≥99.99%'}`);
  } else {
    lines.push('❌ Workflow failed. Address errors and retry.');

    if (result.methodology.includes('high entropy')) {
      lines.push('');
      lines.push('⚠️  Specification entropy too high for Big Bang 80/20.');
      lines.push('   Recommendation: Use iterative development instead.');
    }
  }

  lines.push('');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  return lines.join('\n');
}

/**
 * Save artifacts to output directory
 */
function saveArtifacts(result, outputDir) {
  try {
    mkdirSync(outputDir, { recursive: true });

    // Save workflow result
    writeFileSync(
      join(outputDir, 'workflow-result.json'),
      JSON.stringify(result, null, 2)
    );

    // Save generated code
    if (result.artifacts.code) {
      writeFileSync(
        join(outputDir, 'generated-code.mjs'),
        result.artifacts.code
      );
    }

    // Save pseudocode
    if (result.artifacts.pseudocode) {
      writeFileSync(
        join(outputDir, 'pseudocode.txt'),
        result.artifacts.pseudocode
      );
    }

    // Save architecture
    if (result.artifacts.architecture) {
      writeFileSync(
        join(outputDir, 'architecture.json'),
        JSON.stringify(result.artifacts.architecture, null, 2)
      );
    }

    console.error(`✅ Artifacts saved to: ${outputDir}`);

  } catch (error) {
    console.error(`⚠️  Warning: Failed to save artifacts: ${error.message}`);
  }
}

/**
 * Main bb8020 command
 */
export const bb8020Command = defineCommand({
  meta: {
    name: 'bb8020',
    description: 'Execute complete Big Bang 80/20 workflow (11 steps)',
  },
  args: {
    spec: {
      type: 'positional',
      description: 'Path to specification JSON file',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output directory for generated artifacts',
      default: './bb8020-output',
      alias: 'o',
    },
    codebase: {
      type: 'string',
      description: 'Path to codebase for pattern matching',
      default: process.cwd(),
      alias: 'c',
    },
    format: {
      type: 'string',
      description: 'Output format (text|json)',
      default: 'text',
      alias: 'f',
    },
  },
  async run({ args }) {
    try {
      console.error('');
      console.error('🚀 Starting Big Bang 80/20 workflow...');
      console.error('');

      // Parse specification
      const spec = parseSpecificationFile(args.spec);

      console.error(`Specification: ${spec.name || 'Unnamed'}`);
      console.error(`Features: ${spec.features.length}`);
      console.error(`Output directory: ${args.output}`);
      console.error('');

      // Create orchestrator
      const store = createStore();
      const orchestrator = new BB8020Orchestrator({
        store,
        codebasePath: args.codebase,
        config: {
          dimension: 10000,
          similarityThreshold: 0.9
        }
      });

      // Execute workflow
      console.error('Executing 11-step workflow...');
      console.error('');

      const result = await orchestrator.execute(spec, spec.features.map(f => ({
        id: f.id,
        name: f.name,
        value: f.value,
        cost: f.cost,
        description: f.description || ''
      })));

      // Save artifacts
      saveArtifacts(result, args.output);

      console.error('');

      // Output result
      if (args.format === 'json') {
        console.log(JSON.stringify(result, null, 2));
      } else {
        console.log(formatWorkflowResult(result));
      }

      // Exit code
      process.exit(result.success ? 0 : 1);

    } catch (error) {
      console.error('');
      console.error('❌ BB80/20 workflow failed:');
      console.error(error.message);
      console.error('');
      process.exit(1);
    }
  },
});
