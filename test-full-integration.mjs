#!/usr/bin/env node

import { getOrchestrator } from './src/unrdf-runtime-orchestrator.mjs';

async function runFullIntegrationTest() {
  console.log('🚀 UNRDF Full System Integration Test - All 10 Layers\n');
  console.log('═══════════════════════════════════════════════════════════\n');

  try {
    const orchestrator = await getOrchestrator();

    // Layer 1-2: Discovery & Registry
    console.log('📦 LAYER 1-2: Discovery & Package Registry');
    const systemStatus = await orchestrator.getSystemStatus();
    console.log(`  Total Packages Discovered: ${systemStatus.packageSystem.totalPackages}`);
    console.log(`  Tier Distribution:`);
    console.log(`    Essential: ${systemStatus.packageSystem.tierSummary.Essential}`);
    console.log(`    Extended: ${systemStatus.packageSystem.tierSummary.Extended}`);
    console.log(`    Optional: ${systemStatus.packageSystem.tierSummary.Optional}`);
    console.log(`    Internal: ${systemStatus.packageSystem.tierSummary.Internal}\n`);

    // Layer 3: Dependency Resolution
    console.log('🔗 LAYER 3: Dependency Resolution');
    const diagnostics = await orchestrator.runDiagnostics();
    console.log(`  Circular Dependencies Detected: ${diagnostics.checks.circular ? diagnostics.checks.circular.length : 0}`);
    console.log(`  Dependency Depth Analysis: ${diagnostics.checks.deepPaths ? diagnostics.checks.deepPaths.length : 0} deep paths\n`);

    // Layer 4: Validation
    console.log('✅ LAYER 4: Tier Constraint Validation');
    const validation = await orchestrator.validateBoundaries();
    console.log(`  Boundary Valid: ${validation.valid}`);
    console.log(`  Tier Violations: ${validation.violations.length}`);
    if (validation.violations.length > 0) {
      validation.violations.slice(0, 2).forEach((v) => {
        console.log(`    - ${v.package} -> ${v.dependency} (${v.tier} -> ${v.depTier})`);
      });
    }
    console.log();

    // Layer 5: Module Federation
    console.log('🔀 LAYER 5: Module Federation & Runtime Boundaries');
    console.log(`  Modules Loaded: ${systemStatus.federation.loaded}/${systemStatus.federation.total}`);
    console.log(`  Cache Stats: ${systemStatus.cache.cache.hits} hits, ${systemStatus.cache.cache.misses} misses`);
    console.log(`  Cache Hit Rate: ${systemStatus.cache.cache.hitRate}%\n`);

    // Layer 6: Health Monitoring
    console.log('❤️  LAYER 6: Real-time Health Monitoring');
    const health = await orchestrator.assessHealth();
    console.log(`  System Health: ${health.overallStatus}`);
    const loadRate = typeof health.rates.loadSuccessRate === 'number' ? health.rates.loadSuccessRate.toFixed(1) : health.rates.loadSuccessRate;
    const validRate = typeof health.rates.validationPassRate === 'number' ? health.rates.validationPassRate.toFixed(1) : health.rates.validationPassRate;
    console.log(`  Load Success Rate: ${loadRate}%`);
    console.log(`  Validation Pass Rate: ${validRate}%\n`);

    // Layer 7: Optimization
    console.log('⚡ LAYER 7: Dependency Graph Optimization');
    const optimization = await orchestrator.optimizePackages();
    console.log(`  Overall Optimization Score: ${optimization.scores.overallScore.toFixed(1)}/100`);
    console.log(`  Circularity Score: ${optimization.scores.circularityScore.toFixed(1)}/100`);
    console.log(`  Heavy Dependencies: ${optimization.analysis.heavy.length}`);
    console.log(`  Unused Packages: ${optimization.analysis.unused.length}\n`);

    // Layer 8: Orchestration
    console.log('🎼 LAYER 8: Central Orchestration');
    console.log(`  System Uptime: ${(systemStatus.state.uptime / 1000).toFixed(2)}s`);
    console.log(`  Operations Executed: ${systemStatus.operations.total}`);
    console.log(`  Last Operations: ${systemStatus.operations.lastOperations.slice(-3).map((o) => o.name).join(', ')}\n`);

    // Layer 9: Verification & Coherence
    console.log('🔍 LAYER 9: System Verification & Coherence Detection');
    const coherence = await orchestrator.checkCoherence();
    console.log(`  State Consistency: ${(coherence.coherence.stateConsistency * 100).toFixed(1)}%`);
    console.log(`  Contradictions Detected: ${coherence.coherence.contradictionCount}`);
    console.log(`  Failure Modes Predicted: ${coherence.coherence.failureModeCount}`);
    console.log(`  System Can Continue: ${orchestrator.verificationState.canContinue() ? 'YES' : 'NO'}`);
    console.log(`  Assumptions Validity: ${Object.values(coherence.coherence.assumptionValidity).filter((v) => v).length}/${Object.keys(coherence.coherence.assumptionValidity).length}`);
    console.log(`  Limitations Acknowledged: ${coherence.coherence.limitationAcknowledgement ? 'YES' : 'NO'}\n`);

    // Layer 10: Pressure Mitigation
    console.log('💨 LAYER 10: Adaptive Pressure Mitigation');
    const pressureStatus = await orchestrator.getPressureStatus();
    if (pressureStatus && pressureStatus.currentPressures) {
      console.log(`  Current Pressure Sources: ${pressureStatus.currentPressures.count}`);
      console.log(`  Total Pressure Intensity: ${pressureStatus.currentPressures.totalIntensity}`);
      if (pressureStatus.currentPressures.sources) {
        pressureStatus.currentPressures.sources.forEach((p) => {
          console.log(`    - ${p.source}: ${p.intensity} intensity`);
        });
      }
      console.log(`  In Equilibrium: ${pressureStatus.inEquilibrium ? 'YES' : 'NO'}\n`);
    } else {
      console.log(`  Pressure status unavailable\n`);
    }

    // Detailed Coherence Status
    console.log('═══════════════════════════════════════════════════════════');
    console.log('📊 Detailed Coherence Report\n');

    const fullReport = await orchestrator.generateFullReport();
    console.log('Assumptions (System Presupposes):');
    coherence.assumptions.forEach((a) => {
      console.log(`  ✓ ${a.name}`);
      console.log(`    Statement: ${a.statement}`);
      console.log(`    Evidence: ${a.evidence}`);
    });
    console.log();

    console.log('Limitations (System Cannot Do):');
    coherence.limitations.forEach((l) => {
      console.log(`  ⚠️  ${l.name}`);
      console.log(`    Description: ${l.description}`);
      console.log(`    Impact: ${l.impact}`);
    });
    console.log();

    console.log('Contradictions (System Violates Its Rules):');
    if (coherence.contradictions.length === 0) {
      console.log(`  ✅ No contradictions detected`);
    } else {
      coherence.contradictions.slice(0, 3).forEach((c) => {
        console.log(`  ❌ ${c.type}`);
        console.log(`    ${c.package} -> ${c.dependency}`);
        console.log(`    Severity: ${c.severity}`);
      });
    }
    console.log();

    console.log('Failure Modes (System Could Break):');
    if (coherence.failureModes && coherence.failureModes.length > 0) {
      coherence.failureModes.forEach((f) => {
        console.log(`  🔴 ${f.mode} (${f.probability})`);
        console.log(`    Trigger: ${f.trigger}`);
      });
    } else {
      console.log(`  ✅ No failure modes identified`);
    }
    console.log();

    console.log('Pressures (Forces for Change):');
    if (coherence.pressures && coherence.pressures.length > 0) {
      coherence.pressures.forEach((p) => {
        console.log(`  💨 ${p.source}`);
        console.log(`    Direction: ${p.direction}`);
      });
    } else {
      console.log(`  ⚖️  System in equilibrium`);
    }
    console.log();

    // System Readiness
    console.log('═══════════════════════════════════════════════════════════');
    console.log('🎯 System Readiness Assessment\n');

    const readiness = {
      discovery: systemStatus.packageSystem.totalPackages > 0 ? 100 : 0,
      resolution: diagnostics.checks.circular.length === 0 ? 100 : 50,
      validation: validation.valid ? 100 : 0,
      federation: systemStatus.federation.loaded > 0 ? 100 : 50,
      health: health.overallStatus === 'healthy' ? 100 : 50,
      optimization: optimization.scores.overallScore,
      coherence: coherence.coherence.stateConsistency * 100,
      mitigation: pressureStatus.inEquilibrium ? 100 : 50,
    };

    const avgReadiness = Object.values(readiness).reduce((a, b) => a + b, 0) / Object.keys(readiness).length;

    Object.entries(readiness).forEach(([layer, score]) => {
      const bar = '█'.repeat(Math.round(score / 10)) + '░'.repeat(10 - Math.round(score / 10));
      console.log(`  ${layer.padEnd(15)} [${bar}] ${score.toFixed(0)}/100`);
    });
    console.log();
    console.log(`  Overall Readiness: ${avgReadiness.toFixed(1)}/100\n`);

    // Final Status
    console.log('═══════════════════════════════════════════════════════════');
    console.log('✨ Integration Test Complete\n');

    const systemState =
      coherence.coherence.contradictionCount === 0 && pressureStatus.inEquilibrium
        ? 'OPTIMAL'
        : orchestrator.verificationState.canContinue() && pressureStatus.inEquilibrium
          ? 'OPERATIONAL'
          : 'CONSTRAINED';

    console.log(`System State: ${systemState}`);
    console.log(`Packages: ${systemStatus.packageSystem.totalPackages}`);
    console.log(`Coherence: ${(coherence.coherence.stateConsistency * 100).toFixed(1)}%`);
    console.log(`Optimization: ${optimization.scores.overallScore.toFixed(1)}/100`);
    console.log(`Health: ${health.overallStatus}`);
    console.log(`Pressures: ${pressureStatus.currentPressures.count} sources, intensity ${pressureStatus.currentPressures.totalIntensity}`);
    console.log(`Equilibrium: ${pressureStatus.inEquilibrium ? 'REACHED' : 'EVOLVING'}\n`);

    await orchestrator.shutdown();
    process.exit(0);
  } catch (error) {
    console.error('❌ Test failed:');
    console.error(error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

runFullIntegrationTest();
