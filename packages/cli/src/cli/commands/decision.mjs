/**
 * Decision Command - Hyperdimensional Decision Fabric
 *
 * Processes strategic decisions through the complete workflow:
 * 1. Socratic analysis (assumption extraction)
 * 2. Pareto frontier analysis (if features provided)
 * 3. μ-operator validation (8 operators)
 * 4. Integrated recommendation
 *
 * Based on 2030 vision for eclipsing "Database as Canvas" whiteboards.
 *
 * @module cli/commands/decision
 */

import { defineCommand } from 'citty';
import { readFileSync } from 'node:fs';
import { createDecisionFabric, Feature } from '@unrdf/decision-fabric';
import { createStore } from '@unrdf/core';

/**
 * Parse features from JSON file
 */
function parseFeaturesFile(path) {
  try {
    const content = readFileSync(path, 'utf-8');
    const data = JSON.parse(content);

    if (!Array.isArray(data.features)) {
      throw new Error('Features file must have "features" array');
    }

    return data.features.map(f => new Feature({
      id: f.id,
      name: f.name,
      value: f.value,
      cost: f.cost,
      description: f.description || ''
    }));
  } catch (error) {
    throw new Error(`Failed to parse features file: ${error.message}`);
  }
}

/**
 * Format decision result for display
 */
function formatResult(result) {
  const lines = [];

  lines.push('');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('  HYPERDIMENSIONAL DECISION FABRIC - RESULT');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  // Status
  const statusEmoji = {
    'ACCEPTED': '✅',
    'REJECTED': '❌',
    'BLOCKED': '⛔'
  };
  lines.push(`Status: ${statusEmoji[result.status] || '❓'} ${result.status}`);

  if (result.confidence !== undefined) {
    lines.push(`Confidence: ${(result.confidence * 100).toFixed(2)}%`);
  }

  if (result.entropy_reduction !== undefined) {
    lines.push(`Entropy Reduction: ${result.entropy_reduction.toFixed(2)} nats`);
  }

  if (result.execution_time_us !== undefined) {
    lines.push(`Execution Time: ${result.execution_time_us.toFixed(2)}μs`);
  }

  lines.push('');

  // Socratic Analysis
  if (result.socratic_analysis) {
    const analysis = result.socratic_analysis;
    lines.push('─────────────────────────────────────────────────────');
    lines.push('SOCRATIC ANALYSIS');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    if (analysis.assumptions && analysis.assumptions.length > 0) {
      lines.push(`Assumptions Extracted: ${analysis.assumptions.length}`);
      analysis.assumptions.forEach((a, i) => {
        lines.push(`  ${i + 1}. [${a.classification}] ${a.statement}`);
        lines.push(`     Confidence: ${(a.confidence * 100).toFixed(0)}%`);
      });
      lines.push('');
    }

    if (analysis.challenges && analysis.challenges.length > 0) {
      lines.push(`Challenges Generated: ${analysis.challenges.length}`);
      analysis.challenges.forEach((c, i) => {
        const severityEmoji = { HIGH: '🔴', MEDIUM: '🟡', LOW: '🟢' };
        lines.push(`  ${i + 1}. ${severityEmoji[c.severity]} [${c.type}] ${c.question}`);
      });
      lines.push('');
    }

    if (analysis.recommendation) {
      lines.push('Recommendation:');
      lines.push(`  Proceed: ${analysis.recommendation.proceed ? 'YES ✅' : 'NO ❌'}`);
      lines.push(`  Reason: ${analysis.recommendation.reason}`);
      lines.push(`  Action: ${analysis.recommendation.action}`);
      lines.push('');
    }
  }

  // Pareto Analysis
  if (result.pareto_analysis) {
    const pareto = result.pareto_analysis;
    lines.push('─────────────────────────────────────────────────────');
    lines.push('PARETO FRONTIER ANALYSIS (Big Bang 80/20)');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    lines.push(`Methodology: ${pareto.methodology}`);
    lines.push(`Specification Entropy: ${pareto.specification_entropy.toFixed(2)} bits`);
    lines.push('');

    if (pareto.pareto_frontier) {
      lines.push(`Pareto Features: ${pareto.pareto_frontier.count} (${pareto.pareto_frontier.percentage_of_total.toFixed(1)}% of total)`);
      lines.push('');

      if (pareto.pareto_frontier.features && pareto.pareto_frontier.features.length > 0) {
        lines.push('Top Features (by efficiency):');
        pareto.pareto_frontier.features.slice(0, 5).forEach((f, i) => {
          lines.push(`  ${i + 1}. ${f.name}`);
          lines.push(`     Value: ${f.value}, Cost: ${f.cost}, Efficiency: ${f.efficiency.toFixed(2)}`);
        });
        lines.push('');
      }
    }

    if (pareto.value_analysis) {
      lines.push('Value Analysis:');
      lines.push(`  Frontier captures: ${pareto.value_analysis.percentage.toFixed(1)}% of total value`);
      lines.push(`  Meets 80/20 rule: ${pareto.value_analysis.meets_8020 ? 'YES ✅' : 'NO ❌'}`);
      lines.push('');
    }

    if (pareto.cost_analysis) {
      lines.push('Cost Analysis:');
      lines.push(`  Cost savings: ${pareto.cost_analysis.savings.toFixed(0)} units`);
      lines.push(`  Efficiency gain: ${pareto.cost_analysis.efficiency_gain}`);
      lines.push('');
    }
  }

  // Final Recommendation
  if (result.recommendation) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push('FINAL RECOMMENDATION');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    lines.push(`Action: ${result.recommendation.action}`);
    lines.push(`Reason: ${result.recommendation.reason}`);

    if (result.recommendation.expected_time) {
      lines.push(`Expected Implementation Time: ${result.recommendation.expected_time}`);
    }

    if (result.recommendation.expected_correctness) {
      lines.push(`Expected Correctness: ${result.recommendation.expected_correctness}`);
    }

    if (result.recommendation.next_steps) {
      lines.push('');
      lines.push('Next Steps:');
      lines.push(`  ${result.recommendation.next_steps}`);
    }

    lines.push('');
  }

  // Blocked challenges
  if (result.status === 'BLOCKED' && result.challenges) {
    lines.push('─────────────────────────────────────────────────────');
    lines.push('⛔ BLOCKED - ADDRESS THESE CHALLENGES:');
    lines.push('─────────────────────────────────────────────────────');
    lines.push('');

    result.challenges.forEach((c, i) => {
      lines.push(`${i + 1}. [${c.type}] ${c.question}`);
    });

    lines.push('');
  }

  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  return lines.join('\n');
}

/**
 * Main decision command
 */
export const decisionCommand = defineCommand({
  meta: {
    name: 'decision',
    description: 'Process strategic decision through Hyperdimensional Decision Fabric',
  },
  args: {
    statement: {
      type: 'positional',
      description: 'Decision statement to analyze',
      required: true,
    },
    features: {
      type: 'string',
      description: 'Path to features JSON file (optional)',
      alias: 'f',
    },
    output: {
      type: 'string',
      description: 'Output format (text|json)',
      default: 'text',
      alias: 'o',
    },
  },
  async run({ args }) {
    try {
      // Parse features if provided
      let features = [];
      if (args.features) {
        features = parseFeaturesFile(args.features);
      }

      // Create decision fabric
      const store = createStore();
      const fabric = await createDecisionFabric({ store });

      // Process strategic decision
      console.error('Processing decision through Hyperdimensional Decision Fabric...');
      console.error('');

      const result = await fabric.processStrategicDecision(args.statement, features);

      // Output result
      if (args.output === 'json') {
        console.log(JSON.stringify(result, null, 2));
      } else {
        console.log(formatResult(result));
      }

      // Exit code based on status
      if (result.status === 'BLOCKED' || result.status === 'REJECTED') {
        process.exit(1);
      }

    } catch (error) {
      console.error('');
      console.error('❌ Error processing decision:');
      console.error(error.message);
      console.error('');
      process.exit(1);
    }
  },
});
