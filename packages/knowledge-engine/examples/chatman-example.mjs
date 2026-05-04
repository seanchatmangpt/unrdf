/**
 * @file Chatman Equation Example
 * @description Example usage of Chatman Equation integration with knowledge engine
 */

import { createChatmanEngine, loadChatmanConfig } from '../src/index.mjs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

async function main() {
  console.log('Chatman Equation Example\n');

  // Create Chatman engine
  const engine = createChatmanEngine({
    observableRatio: 0.05,
    closureThreshold: 0.95,
    enableReceipts: true,
  });

  // Example 1: Market Analysis
  console.log('=== Example 1: Market Dynamics Analysis ===\n');

  const marketObservable = {
    type: 'market',
    patterns: [
      'declining_customer_satisfaction',
      'increasing_competitor_pricing_pressure',
      'market_share_erosion',
    ],
    visibility: 0.05,
  };

  const marketResult = await engine.executePipeline(marketObservable, {
    theorem: 'emergence',
  });

  console.log('Observable Patterns:', marketObservable.patterns);
  console.log('Dark Field Size:', marketResult.output.artifact.darkField.patterns.length);
  console.log('Completeness:', marketResult.output.artifact.completeness.toFixed(3));
  console.log(
    'Value Proposition:',
    marketResult.output.formation.output.valueProposition
  );
  console.log('\n');

  // Example 2: Organizational Analysis
  console.log('=== Example 2: Organizational Dynamics Analysis ===\n');

  const orgObservable = {
    type: 'organizational',
    patterns: [
      'formal_reporting_structure',
      'documented_processes',
      'official_communication_channels',
    ],
    visibility: 0.05,
  };

  const orgResult = await engine.executePipeline(orgObservable, {
    theorem: 'value_innovation',
  });

  console.log('Observable Patterns:', orgObservable.patterns);
  console.log('Dark Field Patterns (sample):');
  orgResult.output.artifact.darkField.patterns.slice(0, 5).forEach(p => {
    console.log('  -', p);
  });
  console.log('Formation Type:', orgResult.output.formation.theorem);
  console.log('\n');

  // Example 3: Strategic Analysis
  console.log('=== Example 3: Strategic Dynamics Analysis ===\n');

  const strategicObservable = {
    type: 'strategic',
    patterns: ['current_market_position', 'published_strategy', 'known_capabilities'],
    visibility: 0.05,
  };

  const strategicResult = await engine.executePipeline(strategicObservable, {
    theorem: 'strategic_canvas',
  });

  console.log('Observable Patterns:', strategicObservable.patterns);
  console.log('Strategic Moves:');
  strategicResult.output.formation.output.strategicMoves.forEach(move => {
    console.log('  -', move);
  });
  console.log('\n');

  // Example 4: Disruption Analysis
  console.log('=== Example 4: Disruption Arithmetic Analysis ===\n');

  const disruptionObservable = {
    type: 'disruption',
    patterns: ['emerging_technology', 'changing_regulations', 'new_entrants'],
    visibility: 0.05,
  };

  const disruptionResult = await engine.executePipeline(disruptionObservable, {
    theorem: 'four_actions',
  });

  console.log('Observable Patterns:', disruptionObservable.patterns);
  console.log(
    'Dark Field Coverage:',
    disruptionResult.output.artifact.darkField.coverage.toFixed(3)
  );
  console.log(
    'Value Proposition:',
    disruptionResult.output.formation.output.valueProposition
  );
  console.log('\n');

  // Load and use custom configuration
  console.log('=== Example 5: Loading Custom TOML Configuration ===\n');

  try {
    const configPath = join(__dirname, 'chatman.toml');
    const config = await loadChatmanConfig(configPath);

    console.log('Configuration loaded:');
    console.log('  Version:', config.chatman?.version);
    console.log('  Observable Ratio:', config.chatman?.observableRatio);
    console.log('  Market Rules:', config.marketDynamics?.length || 0);
    console.log('  Organizational Rules:', config.organizationalDynamics?.length || 0);
    console.log('  Strategic Rules:', config.strategicDynamics?.length || 0);
    console.log('  Disruption Rules:', config.disruptionArithmetic?.length || 0);
    console.log('\n');
  } catch (error) {
    console.log('Note: Custom TOML config not found, using defaults\n');
  }

  // Display metrics
  console.log('=== Engine Metrics ===\n');
  const metrics = engine.getMetrics();
  console.log('Operations Executed:', metrics.operationsExecuted);
  console.log('Receipts Generated:', metrics.receiptsGenerated);
  console.log(
    'Average Execution Time:',
    metrics.averageExecutionTime.toFixed(2),
    'ms'
  );
  console.log('\nComponent Metrics:');
  console.log(
    '  Operator Closures:',
    metrics.components.operator.closureOperations
  );
  console.log(
    '  Artifacts Generated:',
    metrics.components.artifactGenerator.artifactsGenerated
  );
  console.log(
    '  Detections Performed:',
    metrics.components.darkFieldDetector.detectionsPerformed
  );
  console.log(
    '  Theorems Applied:',
    metrics.components.formationTheorems.theoremsApplied
  );
}

main().catch(console.error);
