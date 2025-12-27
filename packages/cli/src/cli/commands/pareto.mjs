/**
 * Pareto Command - Big Bang 80/20 Feature Analysis
 *
 * Analyzes feature sets to identify Pareto-optimal features.
 * Implements the Big Bang 80/20 methodology for feature prioritization.
 *
 * Key Capabilities:
 * - Pareto frontier computation
 * - 80/20 rule validation
 * - Specification entropy analysis
 * - BB80/20 applicability determination
 *
 * @module cli/commands/pareto
 */

import { defineCommand } from 'citty';
import { readFileSync } from 'node:fs';
import { ParetoAnalyzer, Feature } from '@unrdf/decision-fabric';

/**
 * Parse features from JSON file
 *
 * Expected format:
 * {
 *   "features": [
 *     { "id": 1, "name": "Feature Name", "value": 95, "cost": 50 },
 *     ...
 *   ]
 * }
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
 * Format Pareto analysis result for display
 */
function formatResult(recommendation) {
  const lines = [];

  lines.push('');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('  BIG BANG 80/20 - PARETO FRONTIER ANALYSIS');
  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  // Methodology
  const methodologyEmoji = recommendation.methodology === 'Big Bang 80/20' ? '🚀' : '🔄';
  lines.push(`${methodologyEmoji} Methodology: ${recommendation.methodology}`);
  lines.push('');

  // Specification Entropy
  lines.push('─────────────────────────────────────────────────────');
  lines.push('SPECIFICATION ENTROPY ANALYSIS');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');
  lines.push(`H_spec: ${recommendation.specification_entropy.toFixed(2)} bits`);

  if (recommendation.specification_entropy <= 16) {
    lines.push(`Status: ✅ BOUNDED (≤16 bits) - BB80/20 applicable`);
  } else {
    lines.push(`Status: ⚠️  HIGH ENTROPY (>16 bits) - Iterative approach recommended`);
  }
  lines.push('');

  // Pareto Frontier
  lines.push('─────────────────────────────────────────────────────');
  lines.push('PARETO FRONTIER');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');

  const frontier = recommendation.pareto_frontier;
  lines.push(`Features on frontier: ${frontier.count} (${frontier.percentage_of_total.toFixed(1)}% of total)`);
  lines.push('');

  if (frontier.features && frontier.features.length > 0) {
    lines.push('Pareto-Optimal Features (ranked by efficiency):');
    lines.push('');

    frontier.features.forEach((f, i) => {
      lines.push(`  ${i + 1}. ${f.name}`);
      lines.push(`     Value: ${f.value}  Cost: ${f.cost}  Efficiency: ${f.efficiency.toFixed(2)}`);
      if (f.description) {
        lines.push(`     ${f.description}`);
      }
      lines.push('');
    });
  }

  // Value Analysis
  lines.push('─────────────────────────────────────────────────────');
  lines.push('80/20 RULE VALIDATION');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');

  const value = recommendation.value_analysis;
  lines.push(`Frontier Value: ${value.frontier_value} / ${value.total_value} (${value.percentage.toFixed(1)}%)`);

  if (value.meets_8020) {
    lines.push(`Status: ✅ MEETS 80/20 RULE (${frontier.percentage_of_total.toFixed(0)}% features → ${value.percentage.toFixed(0)}% value)`);
  } else {
    lines.push(`Status: ⚠️  Does not meet strict 80/20 rule`);
  }
  lines.push('');

  // Cost Analysis
  lines.push('─────────────────────────────────────────────────────');
  lines.push('COST EFFICIENCY');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');

  const cost = recommendation.cost_analysis;
  lines.push(`Frontier Cost: ${cost.frontier_cost} / ${cost.total_cost}`);
  lines.push(`Cost Savings: ${cost.savings} units (${cost.efficiency_gain} efficiency gain)`);
  lines.push('');

  // Final Recommendation
  lines.push('─────────────────────────────────────────────────────');
  lines.push('RECOMMENDATION');
  lines.push('─────────────────────────────────────────────────────');
  lines.push('');
  lines.push(recommendation.recommendation);
  lines.push('');

  lines.push('═══════════════════════════════════════════════════════');
  lines.push('');

  return lines.join('\n');
}

/**
 * Main pareto command
 */
export const paretoCommand = defineCommand({
  meta: {
    name: 'pareto',
    description: 'Analyze feature set for Pareto frontier (Big Bang 80/20)',
  },
  args: {
    file: {
      type: 'positional',
      description: 'Path to features JSON file',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output format (text|json|chart)',
      default: 'text',
      alias: 'o',
    },
  },
  async run({ args }) {
    try {
      // Parse features
      const features = parseFeaturesFile(args.file);

      if (features.length === 0) {
        throw new Error('No features found in file');
      }

      console.error(`Analyzing ${features.length} features...`);
      console.error('');

      // Create analyzer and add features
      const analyzer = new ParetoAnalyzer();
      analyzer.addFeatures(features);

      // Generate recommendation
      const recommendation = analyzer.generateRecommendation();

      // Output result
      if (args.output === 'json') {
        console.log(JSON.stringify(recommendation, null, 2));
      } else if (args.output === 'chart') {
        const chartData = analyzer.generateParetoChart();
        console.log(JSON.stringify(chartData, null, 2));
      } else {
        console.log(formatResult(recommendation));
      }

      // Exit code based on applicability
      if (recommendation.methodology !== 'Big Bang 80/20') {
        console.error('⚠️  Warning: Specification entropy too high for BB80/20');
        console.error('   Consider iterative development instead.');
        console.error('');
        process.exit(1);
      }

    } catch (error) {
      console.error('');
      console.error('❌ Error analyzing features:');
      console.error(error.message);
      console.error('');
      process.exit(1);
    }
  },
});
