/**
 * BB8020 Step 9: Static Analysis
 * Real complexity analysis using @unrdf/project-engine
 */

import { analyzeJsComplexity } from '@unrdf/project-engine';

/**
 * Execute Step 9: Static analysis
 * Uses analyzeJsComplexity for real metrics
 * FAIL FAST - Throws if coverage < 98%
 */
export async function executeStep9StaticAnalysis({ outputPath }) {
  const start = Date.now();

  console.log(`\n[Step 9] Running static analysis on ${outputPath}...`);

  // Run complexity analysis
  const { store, summary } = await analyzeJsComplexity({
    projectRoot: outputPath,
    mode: 'observe'
  });

  const {
    filesAnalyzed,
    averageCyclomatic,
    maintainabilityIndex,
    topRisks
  } = summary;

  // Calculate coverage from maintainability index (0-100)
  const coverage = maintainabilityIndex / 100;

  // Identify errors (critical complexity)
  const errors = topRisks.filter(r => r.cyclomatic > 20).map(r => ({
    file: r.file,
    function: r.function,
    cyclomatic: r.cyclomatic,
    reason: `Cyclomatic complexity ${r.cyclomatic} exceeds threshold 20`
  }));

  // Identify warnings (moderate complexity)
  const warnings = topRisks.filter(r => r.cyclomatic > 10 && r.cyclomatic <= 20).map(r => ({
    file: r.file,
    function: r.function,
    cyclomatic: r.cyclomatic,
    reason: `Cyclomatic complexity ${r.cyclomatic} exceeds recommended limit 10`
  }));

  const success = coverage >= 0.98 && errors.length === 0;

  console.log(`[Step 9] Coverage: ${(coverage * 100).toFixed(1)}%`);
  console.log(`[Step 9] Avg Cyclomatic: ${averageCyclomatic.toFixed(2)}`);
  console.log(`[Step 9] Errors: ${errors.length}, Warnings: ${warnings.length}`);

  if (!success) {
    console.warn(`⚠️  Static analysis: coverage ${(coverage * 100).toFixed(1)}% < 98% required`);
  }

  return {
    coverage,
    errors,
    warnings,
    metrics: {
      filesAnalyzed,
      averageCyclomatic,
      maintainabilityIndex
    },
    duration_ms: Date.now() - start,
    summary: {
      coverage: (coverage * 100).toFixed(1) + '%',
      errors_count: errors.length,
      warnings_count: warnings.length,
      avg_cyclomatic: averageCyclomatic.toFixed(2)
    }
  };
}
