#!/usr/bin/env node

import { getOrchestrator } from './src/unrdf-runtime-orchestrator.mjs';

async function runLiveTest() {
  console.log('🚀 UNRDF Runtime Orchestrator - Live Test\n');

  try {
    const orchestrator = await getOrchestrator();

    console.log('📊 System Status:');
    const status = await orchestrator.getSystemStatus();
    console.log(`  Total Packages: ${status.packageSystem.totalPackages}`);
    console.log(`  Tiers: E=${status.packageSystem.tierSummary.Essential} X=${status.packageSystem.tierSummary.Extended} O=${status.packageSystem.tierSummary.Optional} I=${status.packageSystem.tierSummary.Internal}`);
    console.log(`  Loaded: ${status.federation.loaded}/${status.federation.total}`);
    console.log(`  Cache Hit Rate: ${status.cache.cache.hitRate}%\n`);

    console.log('🔍 Running Diagnostics:');
    const diagnostics = await orchestrator.runDiagnostics();
    console.log(`  Circular Dependencies: ${diagnostics.checks.circular.length}`);
    console.log(`  Unused Packages: ${diagnostics.checks.unused.length}`);
    console.log(`  Heavy Dependencies: ${diagnostics.checks.heavy.length}`);
    console.log(`  Tier Violations: ${diagnostics.checks.tierViolations.length}`);
    console.log(`  Boundary Violations: ${diagnostics.checks.boundaries.length}`);
    console.log(`  Overall Status: ${diagnostics.summary.isBoundaryValid ? '✅ HEALTHY' : '⚠️  ISSUES FOUND'}\n`);

    console.log('⚡ Loading Essential Tier:');
    const essentialLoading = await orchestrator.loadEssentialTier();
    console.log(`  Loaded: ${essentialLoading.loadedCount}/${essentialLoading.totalModules}`);
    console.log(`  Success: ${essentialLoading.success}\n`);

    console.log('💡 Optimization Analysis:');
    const optimization = await orchestrator.optimizePackages();
    console.log(`  Circularity Score: ${optimization.scores.circularityScore.toFixed(1)}/100`);
    console.log(`  Depth Score: ${optimization.scores.depthScore.toFixed(1)}/100`);
    console.log(`  Tier Score: ${optimization.scores.tierScore.toFixed(1)}/100`);
    console.log(`  Overall Score: ${optimization.scores.overallScore.toFixed(1)}/100\n`);

    if (optimization.analysis.heavy.length > 0) {
      console.log('⚠️  Heavy Dependencies Found:');
      optimization.analysis.heavy.slice(0, 3).forEach((h) => {
        console.log(
          `  ${h.package}: ${h.depCount} dependencies`
        );
      });
      console.log();
    }

    console.log('🔐 Boundary Validation:');
    const boundaries = await orchestrator.validateBoundaries();
    console.log(`  Valid: ${boundaries.valid}`);
    console.log(`  Violations: ${boundaries.violations.length}\n`);

    console.log('❤️  Health Assessment:');
    const health = await orchestrator.assessHealth();
    console.log(`  Status: ${health.overallStatus}`);
    console.log(`  Valid Packages: ${health.summary.validPackages}/${health.summary.totalPackages}`);
    console.log(`  Load Success Rate: ${health.rates.loadSuccessRate}%`);
    console.log(`  Validation Pass Rate: ${health.rates.validationPassRate}%\n`);

    console.log('📈 Performance Insights:');
    const report = await orchestrator.generateFullReport();
    console.log(`  Avg Load Time: ${report.system.health.performance.avgLoadTime}ms`);
    console.log(`  Avg Resolution Time: ${report.system.health.performance.avgResolutionTime}ms`);
    console.log(`  Cache Stats: ${report.system.cache.cache.hits} hits, ${report.system.cache.cache.misses} misses`);
    console.log(`  Total Operations: ${report.system.operations.total}\n`);

    if (report.parallelization.length > 0) {
      console.log('⚡ Parallelization Opportunities:');
      report.parallelization.forEach((group) => {
        console.log(
          `  ${group.tier}: ${group.packages.length} packages (~${group.estimatedParallelTime.toFixed(2)}ms parallel)`
        );
      });
      console.log();
    }

    console.log('🗂️  Caching Strategy:');
    const caching = report.caching;
    console.log(`  Current Hit Rate: ${caching.currentCacheHitRate}%`);
    console.log(`  Cached Modules: ${caching.totalCached}/${caching.maxCacheSize}`);
    if (caching.recommendations.length > 0) {
      console.log(`  Recommendations: ${caching.recommendations.length}`);
      caching.recommendations.forEach((rec) => {
        console.log(`    [${rec.priority.toUpperCase()}] ${rec.suggestion}`);
      });
    }
    console.log();

    console.log('═══════════════════════════════════════════════════════════');
    console.log('✨ Orchestrator Test Complete');
    console.log('═══════════════════════════════════════════════════════════\n');

    console.log('📋 Final Summary:');
    console.log(`  Packages Discovered: ${status.packageSystem.totalPackages}`);
    console.log(`  Modules Loaded: ${status.federation.loaded}`);
    console.log(`  System Health: ${health.overallStatus}`);
    console.log(`  Optimization Score: ${optimization.scores.overallScore.toFixed(1)}/100`);
    console.log(`  Boundaries Valid: ${boundaries.valid ? '✅' : '❌'}`);
    console.log(`  Uptime: ${(status.state.uptime / 1000).toFixed(2)}s\n`);

    await orchestrator.shutdown();
    process.exit(0);
  } catch (error) {
    console.error('❌ Test failed:');
    console.error(error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

runLiveTest();
