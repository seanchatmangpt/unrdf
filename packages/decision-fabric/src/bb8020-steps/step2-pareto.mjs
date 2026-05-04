/**
 * BB8020 Step 2: Pareto Frontier Computation
 * 80/20 analysis
 */

import { ParetoAnalyzer } from '../pareto-analyzer.mjs';

/**
 * Execute Step 2: Compute Pareto frontier
 * FAIL FAST - Throws on computation errors
 */
export async function executeStep2Pareto({ features }) {
  const start = Date.now();

  console.log(`\n[Step 2] Computing Pareto frontier for ${features.length} features...`);

  const analyzer = new ParetoAnalyzer();
  analyzer.addFeatures(features);

  const frontier = analyzer.computeParetoFrontier();
  const hSpec = analyzer.computeSpecificationEntropy();
  const applicability = analyzer.isBB8020Applicable();

  console.log(`[Step 2] ✓ Pareto frontier: ${frontier.length} features`);
  console.log(`[Step 2] ✓ H_spec: ${hSpec.toFixed(2)} bits`);
  console.log(`[Step 2] ✓ BB80/20 applicable: ${applicability.applicable}`);

  return {
    paretoFrontier: frontier,
    specificationEntropy: hSpec,
    applicability,
    duration_ms: Date.now() - start,
    summary: {
      frontier_size: frontier.length,
      h_spec: hSpec,
      applicable: applicability.applicable
    }
  };
}
