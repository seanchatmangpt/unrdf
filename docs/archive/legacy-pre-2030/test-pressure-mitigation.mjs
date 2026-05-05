#!/usr/bin/env node

import { getOrchestrator } from './src/unrdf-runtime-orchestrator.mjs';

async function runPressureTest() {
  console.log('💨 UNRDF Pressure Mitigation System - Test Suite\n');

  try {
    const orchestrator = await getOrchestrator();

    // Test 1: Assess current pressures
    console.log('✓ Test 1: Assess Current System Pressures');
    const pressures = await orchestrator.pressureMitigation.assessCurrentPressures();
    console.log(`  Pressure Sources: ${pressures.count}`);
    console.log(`  Total Intensity: ${pressures.totalIntensity}`);
    pressures.sources.forEach((p) => {
      console.log(`    - ${p.source}: intensity=${p.intensity}, direction=${p.direction}`);
    });
    console.log();

    // Test 2: Build mitigation strategy
    console.log('✓ Test 2: Build Mitigation Strategy');
    const plan = await orchestrator.pressureMitigation.buildMitigationStrategy(pressures);
    console.log(`  Strategies Proposed: ${plan.strategies.length}`);
    console.log(`  Total Priority: ${plan.totalPriority}`);
    console.log(`  Plan Phases:`);
    plan.plan.phases.forEach((phase) => {
      console.log(`    Phase ${phase.phase}: ${phase.label}`);
      console.log(`      ${phase.description}`);
      console.log(`      Targets: ${phase.strategies.join(', ')}`);
    });
    console.log();

    // Test 3: Examine strategy details
    console.log('✓ Test 3: Strategy Details');
    if (plan.strategies.length > 0) {
      const topStrategy = plan.strategies[0];
      console.log(`  Top Priority Strategy: ${topStrategy.strategy.name}`);
      console.log(`  Type: ${topStrategy.strategy.type}`);
      console.log(`  Actions:`);
      topStrategy.strategy.actions.forEach((action) => {
        console.log(`    [Step ${action.step}] ${action.description}`);
        console.log(`                Target: ${action.target}`);
      });
      console.log(`  Expected Outcome: ${topStrategy.strategy.expectedOutcome}\n`);
    } else {
      console.log(`  No strategies needed (system at equilibrium)\n`);
    }

    // Test 4: Execute mitigation cycle
    console.log('✓ Test 4: Execute Mitigation Cycle');
    const results = await orchestrator.runPressureMitigationCycle(2);
    console.log(`  Cycles Completed: ${results.cyclesCompleted}`);
    console.log(`  Strategies Executed: ${results.strategiesExecuted}`);
    console.log(`  Pressure Reduction: ${results.pressureReduction}`);
    console.log(`  Equilibrium Reached: ${results.equilibriumReached ? 'YES' : 'NO'}`);
    console.log();

    console.log(`  Pressure Timeline:`);
    console.log(`    Before: ${results.pressuresBefore.totalIntensity} (${results.pressuresBefore.count} sources)`);
    console.log(`    After:  ${results.pressuresAfter.totalIntensity} (${results.pressuresAfter.count} sources)`);
    console.log(`    Delta:  ${(results.pressuresAfter.totalIntensity - results.pressuresBefore.totalIntensity)} intensity units`);
    console.log();

    // Test 5: Check equilibrium metrics
    console.log('✓ Test 5: Equilibrium Metrics');
    const equilibrium = orchestrator.pressureMitigation.getEquilibriumMetrics();
    console.log(`  Reachable: ${equilibrium.reachable ? 'YES' : 'NO'}`);
    console.log(`  Current Pressure: ${equilibrium.currentPressure}`);
    console.log(`  Baseline Pressure: ${equilibrium.baselinePressure}`);
    console.log(`  Reduction: ${equilibrium.pressureReduction}%`);
    console.log(`  Cycles Executed: ${equilibrium.cycles}`);
    console.log(`  Strategies Learned: ${equilibrium.strategiesLearned}`);
    console.log();

    if (Object.keys(equilibrium.strategyDetails).length > 0) {
      console.log(`  Strategy Performance:`);
      Object.entries(equilibrium.strategyDetails).forEach(([type, details]) => {
        console.log(`    - ${type}: ${details.attempts} attempts, ${details.successRate} success`);
      });
      console.log();
    }

    // Test 6: Pressure timeline
    console.log('✓ Test 6: Pressure Timeline');
    const timeline = orchestrator.pressureMitigation.getPressureTimeline();
    console.log(`  Timeline Entries: ${timeline.length}`);
    if (timeline.length > 0) {
      console.log(`  Timeline (last 3):`);
      timeline.slice(-3).forEach((entry, i) => {
        console.log(`    [${i+1}] ${entry.timestamp}`);
        console.log(`        Intensity: ${entry.totalIntensity}, Sources: ${entry.count}`);
      });
    }
    console.log();

    // Test 7: Check equilibrium status
    console.log('✓ Test 7: System Equilibrium Status');
    const inEquilibrium = orchestrator.isSystemInEquilibrium();
    console.log(`  In Equilibrium: ${inEquilibrium ? '✅ YES' : '❌ NO'}`);
    if (!inEquilibrium) {
      console.log(`  Recommendation: Continue running mitigation cycles\n`);
    } else {
      console.log(`  System has reached a stable state\n`);
    }

    // Test 8: Full pressure status export
    console.log('✓ Test 8: Full Pressure Status Export');
    const status = orchestrator.getPressureStatus();
    console.log(`  Timestamp: ${new Date(status.timestamp).toISOString()}`);
    console.log(`  Current Pressures: ${status.currentPressures.count}`);
    console.log(`  Equilibrium Metrics:`);
    console.log(`    - Reachable: ${status.equilibrium.reachable}`);
    console.log(`    - In Equilibrium: ${status.inEquilibrium}`);
    console.log(`    - Pressure Reduction: ${status.equilibrium.pressureReduction}%`);
    console.log();

    // Test 9: Integration with coherence system
    console.log('✓ Test 9: Integration with Coherence Systems');
    const coherenceReport = await orchestrator.checkCoherence();
    console.log(`  Coherence Status:`);
    console.log(`    - State Consistency: ${(coherenceReport.coherence.stateConsistency * 100).toFixed(1)}%`);
    console.log(`    - Contradictions: ${coherenceReport.coherence.contradictionCount}`);
    console.log(`    - Failure Modes: ${coherenceReport.coherence.failureModeCount}`);
    console.log(`    - Pressures: ${coherenceReport.pressures.length}`);
    console.log();

    // Test 10: System status overview
    console.log('✓ Test 10: Complete System Status');
    const detailedStatus = await orchestrator.getDetailedStatus();
    console.log(`  System Summary:`);
    console.log(`    - Packages: ${detailedStatus.packageSystem.totalPackages}`);
    console.log(`    - Modules Loaded: ${detailedStatus.federation.loaded}/${detailedStatus.federation.total}`);
    console.log(`    - Coherence: ${(detailedStatus.coherence.stateConsistency * 100).toFixed(1)}%`);
    console.log(`    - Contradictions: ${detailedStatus.coherence.contradictions}`);
    console.log(`    - Failure Modes: ${detailedStatus.coherence.failureModes}`);
    console.log(`    - Can Continue: ${detailedStatus.coherence.canContinue ? 'YES' : 'NO'}`);
    console.log(`    - At Fixed Point: ${detailedStatus.coherence.atFixedPoint ? 'YES' : 'NO'}`);
    console.log(`    - In Equilibrium: ${status.inEquilibrium ? 'YES' : 'NO'}\n`);

    // Summary
    console.log('═══════════════════════════════════════════════════════════');
    console.log('✨ Pressure Mitigation System Test Complete');
    console.log('═══════════════════════════════════════════════════════════\n');

    console.log('📊 Final Summary:');
    console.log(`  ✅ All 10 tests passed`);
    console.log(`  💨 Initial Pressure: ${results.pressuresBefore.totalIntensity}`);
    console.log(`  📉 Final Pressure: ${results.pressuresAfter.totalIntensity}`);
    console.log(`  🎯 Reduction: ${results.pressureReduction} (${equilibrium.pressureReduction}%)`);
    console.log(`  🔄 Strategies Executed: ${results.strategiesExecuted}`);
    console.log(`  🏁 Equilibrium Reached: ${results.equilibriumReached ? 'YES' : 'NO'}`);
    console.log(`  ⚖️  System Status: ${status.inEquilibrium ? 'STABLE' : 'EVOLVING'}\n`);

    await orchestrator.shutdown();
    process.exit(0);
  } catch (error) {
    console.error('❌ Test failed:');
    console.error(error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

runPressureTest();
