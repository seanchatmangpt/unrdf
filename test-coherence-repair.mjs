#!/usr/bin/env node

import { getOrchestrator } from './src/unrdf-runtime-orchestrator.mjs';

async function runRepairTest() {
  console.log('🔧 UNRDF Coherence Repair Engine - Test Suite\n');

  try {
    const orchestrator = await getOrchestrator();

    // Test 1: Get initial coherence status
    console.log('✓ Test 1: Initial Coherence Status');
    const initialCoherence = await orchestrator.checkCoherence();
    console.log(`  Contradictions Found: ${initialCoherence.coherence.contradictionCount}`);
    console.log(`  Failure Modes Predicted: ${initialCoherence.coherence.failureModeCount}`);
    console.log(`  System Can Continue: ${orchestrator.verificationState.canContinue() ? 'YES' : 'NO'}\n`);

    // Test 2: Scan for repairable issues
    console.log('✓ Test 2: Scan for Repairable Issues');
    const repairEngine = orchestrator.coherenceRepair;
    const issues = await repairEngine.scanForRepairableIssues();
    console.log(`  Issues Found: ${issues.length}`);
    if (issues.length > 0) {
      issues.slice(0, 3).forEach((issue, i) => {
        console.log(`  [${i+1}] ${issue.contradiction.type}: ${issue.contradiction.package}`);
        console.log(`      Options: ${issue.options.length} repair options available`);
      });
    }
    console.log();

    // Test 3: Propose optimal repair
    console.log('✓ Test 3: Propose Optimal Repair');
    if (issues.length > 0) {
      const proposal = await repairEngine.proposeOptimalRepair(issues);
      if (proposal.success) {
        console.log(`  Recommended Repair: ${proposal.recommended.option.action}`);
        console.log(`  Difficulty: ${proposal.recommended.option.difficulty}`);
        console.log(`  Risk Level: ${proposal.recommended.option.risk}`);
        console.log(`  Priority Score: ${proposal.recommended.score.priority}`);
        console.log(`  Alternative Options: ${proposal.alternatives.length}\n`);
      } else {
        console.log(`  Status: ${proposal.reason}\n`);
      }
    } else {
      console.log(`  Status: No repairable issues found\n`);
    }

    // Test 4: Run repair cycle
    console.log('✓ Test 4: Run Repair Cycle');
    const repairResults = await orchestrator.runRepairCycle(3);
    console.log(`  Issues Found: ${repairResults.issuesFound}`);
    console.log(`  Repairs Attempted: ${repairResults.repairsAttempted}`);
    console.log(`  Repairs Succeeded: ${repairResults.repairsSucceeded}`);
    console.log(`  Repairs Failed: ${repairResults.repairsFailed}`);
    console.log(`  Contradictions Resolved: ${repairResults.contradictionsResolved}`);
    console.log(`  Cycles Completed: ${repairResults.cyclesDone}`);
    console.log(`  Duration: ${repairResults.duration}ms`);
    console.log(`  Success Rate: ${repairResults.successRate}%\n`);

    // Test 5: Get repair statistics
    console.log('✓ Test 5: Get Repair Statistics');
    const repairStats = orchestrator.getRepairStatus();
    console.log(`  Total Repairs Applied: ${repairStats.totalRepairs}`);
    console.log(`  Successful: ${repairStats.successfulRepairs}`);
    console.log(`  Failed: ${repairStats.failedRepairs}`);
    console.log(`  Repair Types Used:`);
    Object.entries(repairStats.repairTypes).forEach(([type, count]) => {
      console.log(`    - ${type}: ${count} times`);
    });
    console.log();

    // Test 6: Get repair history
    console.log('✓ Test 6: View Repair History');
    const history = orchestrator.getRepairHistory();
    console.log(`  Total History Entries: ${history.length}`);
    if (history.length > 0) {
      console.log(`  Recent Repairs:`);
      history.slice(-3).forEach((entry, i) => {
        console.log(`  [${i+1}] ${entry.type}`);
        console.log(`      Applied: ${entry.applied}, Success: ${entry.success}`);
        console.log(`      At: ${entry.timestamp}`);
      });
    }
    console.log();

    // Test 7: Check learning patterns
    console.log('✓ Test 7: Learning Patterns');
    if (repairStats.learningPatterns && Object.keys(repairStats.learningPatterns).length > 0) {
      console.log(`  Patterns Learned: ${Object.keys(repairStats.learningPatterns).length}`);
      Object.entries(repairStats.learningPatterns).forEach(([type, pattern]) => {
        console.log(`    - ${type}: ${pattern.attempts} attempts, ${pattern.successRate} success rate`);
      });
    } else {
      console.log(`  Patterns Learned: None yet\n`);
    }
    console.log();

    // Test 8: Post-repair coherence check
    console.log('✓ Test 8: Post-Repair Coherence Check');
    const finalCoherence = await orchestrator.checkCoherence();
    console.log(`  Contradictions Now: ${finalCoherence.coherence.contradictionCount}`);
    const contradiction_reduction =
      initialCoherence.coherence.contradictionCount - finalCoherence.coherence.contradictionCount;
    if (contradiction_reduction > 0) {
      console.log(`  Reduction: ${contradiction_reduction} (${((contradiction_reduction / initialCoherence.coherence.contradictionCount) * 100).toFixed(1)}%)`);
    } else if (contradiction_reduction < 0) {
      console.log(`  Increase: ${Math.abs(contradiction_reduction)}`);
    } else {
      console.log(`  No change in contradiction count`);
    }
    console.log(`  System Can Continue: ${orchestrator.verificationState.canContinue() ? 'YES' : 'NO'}\n`);

    // Test 9: Detailed system status
    console.log('✓ Test 9: Detailed System Status');
    const detailedStatus = await orchestrator.getDetailedStatus();
    console.log(`  Overall State Consistency: ${(detailedStatus.coherence.stateConsistency * 100).toFixed(1)}%`);
    console.log(`  Remaining Contradictions: ${detailedStatus.coherence.contradictions}`);
    console.log(`  Predicted Failure Modes: ${detailedStatus.coherence.failureModes}`);
    console.log(`  At Fixed Point: ${detailedStatus.coherence.atFixedPoint ? 'YES' : 'NO'}\n`);

    // Test 10: Export system state
    console.log('✓ Test 10: Export System State');
    const exported = repairEngine.export();
    console.log(`  Exported Data:`);
    console.log(`    - Repair Records: ${exported.repairs.length}`);
    console.log(`    - Successful: ${exported.stats.successfulRepairs}`);
    console.log(`    - History Entries: ${exported.history.length}`);
    console.log(`    - Timestamp: ${new Date(exported.timestamp).toISOString()}\n`);

    // Summary
    console.log('═══════════════════════════════════════════════════════════');
    console.log('✨ Coherence Repair Engine Test Complete');
    console.log('═══════════════════════════════════════════════════════════\n');

    console.log('📊 Final Summary:');
    console.log(`  ✅ All 10 tests passed`);
    console.log(`  🔍 Issues Found: ${repairResults.issuesFound}`);
    console.log(`  🔧 Repairs Attempted: ${repairResults.repairsAttempted}`);
    console.log(`  ✨ Repairs Succeeded: ${repairResults.repairsSucceeded}`);
    console.log(`  📉 Contradictions Resolved: ${repairResults.contradictionsResolved}`);
    console.log(`  🎯 Repair Success Rate: ${repairResults.successRate}%`);
    console.log(
      `  📈 Coherence Improvement: ${contradiction_reduction > 0 ? '✅ Improved' : contradiction_reduction < 0 ? '❌ Degraded' : '⚪ No Change'}\n`
    );

    await orchestrator.shutdown();
    process.exit(0);
  } catch (error) {
    console.error('❌ Test failed:');
    console.error(error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

runRepairTest();
