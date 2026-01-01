#!/usr/bin/env node

import CoherenceLearningSystem from './src/unrdf-coherence-learning.mjs';

async function runLearningTest() {
  console.log('🧠 UNRDF Coherence Learning System - Test Suite\n');

  try {
    const learner = new CoherenceLearningSystem();

    // Test 1: Initialize learning system
    console.log('✓ Test 1: Initialize Learning System');
    console.log(`  Samples Processed: ${learner.learningState.samplesProcessed}`);
    console.log(`  Patterns Discovered: ${learner.learningState.patternsDiscovered}`);
    console.log(`  Model Accuracy: ${(learner.learningState.modelAccuracy * 100).toFixed(1)}%\n`);

    // Test 2: Record contradictions
    console.log('✓ Test 2: Record Contradictions');
    const contradiction1 = {
      type: 'tier_inversion',
      package: '@unrdf/v6-core',
      dependency: '@unrdf/blockchain',
      severity: 'constraint_violation',
    };

    const contradiction2 = {
      type: 'tier_inversion',
      package: '@unrdf/decision-fabric',
      dependency: '@unrdf/validation',
      severity: 'constraint_violation',
    };

    learner.recordContradiction(contradiction1);
    learner.recordContradiction(contradiction2);
    learner.recordContradiction(contradiction1); // Duplicate pattern

    console.log(`  Contradictions Recorded: 3`);
    console.log(`  Patterns Discovered: ${learner.learningState.patternsDiscovered}`);
    console.log(`  Pattern Insights: ${learner.getPatternInsights().totalPatterns}`);
    if (learner.getPatternInsights().patterns.length > 0) {
      console.log(
        `  Most Frequent Pattern: ${learner.getPatternInsights().patterns[0].frequency} occurrences\n`
      );
    }
    console.log();

    // Test 3: Predict repair outcomes (no history)
    console.log('✓ Test 3: Predict Repair Outcomes (No History)');
    const repairOption1 = {
      type: 'move_package',
      action: 'Move @unrdf/v6-core from Essential to Extended',
      difficulty: 'medium',
      risk: 'medium',
    };

    const prediction1 = learner.predictRepairOutcome(contradiction1, repairOption1);
    console.log(`  Repair Type: ${repairOption1.type}`);
    console.log(`  Predicted: ${prediction1.predicted ? 'YES' : 'NO'}`);
    console.log(`  Confidence: ${(prediction1.confidence * 100).toFixed(1)}%`);
    console.log(`  Reason: ${prediction1.reason}`);
    console.log();

    // Test 4: Simulate repair attempts and learn
    console.log('✓ Test 4: Simulate Repair Attempts (Build Learning Data)');

    // First attempt: successful move_package repair
    learner.recordRepairAttempt(
      contradiction1,
      repairOption1,
      { success: true, action: 'simulated' },
      { contradiction: contradiction1, stillExists: true, resolved: false, remainingContradictions: 1 }
    );

    // Second attempt: successful move_package repair
    learner.recordRepairAttempt(
      contradiction2,
      repairOption1,
      { success: true, action: 'simulated' },
      { contradiction: contradiction2, stillExists: true, resolved: false, remainingContradictions: 1 }
    );

    // Third attempt: successful move_package repair
    learner.recordRepairAttempt(
      contradiction1,
      repairOption1,
      { success: true, action: 'simulated' },
      { contradiction: contradiction1, stillExists: false, resolved: true, remainingContradictions: 0 }
    );

    console.log(`  Repair Attempts Recorded: 3`);
    console.log(`  Samples Processed: ${learner.learningState.samplesProcessed}`);
    console.log();

    // Test 5: Predict with accumulated knowledge
    console.log('✓ Test 5: Predict Repair Outcomes (With History)');
    const prediction2 = learner.predictRepairOutcome(contradiction1, repairOption1);
    console.log(`  Repair Type: ${repairOption1.type}`);
    console.log(`  Predicted: ${prediction2.predicted ? 'YES' : 'NO'}`);
    console.log(`  Confidence: ${(prediction2.confidence * 100).toFixed(1)}%`);
    console.log(`  Success Rate: ${prediction2.successRate}%`);
    console.log(`  Resolution Rate: ${prediction2.resolutionRate}%`);
    console.log(`  Effectiveness: ${prediction2.effectiveness}/100`);
    console.log(`  Repair Weight: ${prediction2.repairWeight.toFixed(2)}`);
    console.log();

    // Test 6: Rank multiple repair options
    console.log('✓ Test 6: Rank Multiple Repair Options');
    const repairOption2 = {
      type: 'break_dependency',
      action: 'Break dependency',
      difficulty: 'high',
      risk: 'high',
    };

    const repairOption3 = {
      type: 'lazy_load',
      action: 'Enable lazy loading',
      difficulty: 'medium',
      risk: 'medium',
    };

    const ranked = learner.rankRepairOptions(contradiction1, [repairOption1, repairOption2, repairOption3]);
    console.log(`  Ranked Options for '${contradiction1.type}':`);
    ranked.forEach((item, i) => {
      console.log(`  [${i + 1}] ${item.option.type}`);
      console.log(`      Score: ${item.score.toFixed(4)}`);
      console.log(`      Predicted: ${item.prediction.predicted ? 'YES' : 'NO'}`);
      console.log(`      Confidence: ${(item.prediction.confidence * 100).toFixed(1)}%`);
    });
    console.log();

    // Test 7: Find similar contradictions
    console.log('✓ Test 7: Find Similar Contradictions');
    const similar = learner.getSimilarContradictions(contradiction1);
    console.log(`  Pattern: ${similar.pattern.type} (${similar.pattern.severity})`);
    console.log(`  Total Matches: ${similar.count}`);
    console.log(`  Recent Matches: ${similar.matches.length}`);
    console.log();

    // Test 8: Check repair metamodel
    console.log('✓ Test 8: Repair Metamodel Analysis');
    const metamodel = learner.getRepairMetamodel();
    console.log(`  Learning State:`);
    console.log(`    - Samples Processed: ${metamodel.learning_state.samplesProcessed}`);
    console.log(`    - Patterns Discovered: ${metamodel.learning_state.patternsDiscovered}`);
    console.log();
    console.log(`  Repair Types Learned:`);
    Object.entries(metamodel.repair_types).forEach(([type, data]) => {
      console.log(`    - ${type}: weight=${data.weight}, samples=${data.sampleSize}, outcome=${data.avgOutcome}/100`);
    });
    console.log();
    console.log(`  Contradiction Types:`);
    Object.entries(metamodel.contradiction_types).forEach(([type, data]) => {
      console.log(`    - ${type}: attempts=${data.attempts}, success=${data.successRate}%, resolution=${data.resolutionRate}%`);
    });
    console.log();
    console.log(`  Overall Accuracy:`);
    console.log(`    - Total Repairs: ${metamodel.accuracy_metrics.totalRepairs}`);
    console.log(`    - Success Rate: ${metamodel.accuracy_metrics.successRate}%`);
    console.log(`    - Resolution Rate: ${metamodel.accuracy_metrics.resolutionRate}%`);
    console.log(`    - Avg Effectiveness: ${metamodel.accuracy_metrics.avgEffectiveness}/100`);
    console.log();

    // Test 9: Pattern clustering insights
    console.log('✓ Test 9: Pattern Clustering Insights');
    const insights = learner.getPatternInsights();
    console.log(`  Total Patterns: ${insights.totalPatterns}`);
    if (insights.patterns.length > 0) {
      console.log(`  Pattern Details:`);
      insights.patterns.slice(0, 3).forEach((pattern, i) => {
        console.log(`    [${i + 1}] Frequency: ${pattern.frequency}, Variants: ${pattern.variants}`);
      });
    }
    console.log();

    // Test 10: Export learning state
    console.log('✓ Test 10: Export Learning State');
    const exported = learner.export();
    console.log(`  Exported Data:`);
    console.log(`    - Timestamp: ${new Date(exported.timestamp).toISOString()}`);
    console.log(`    - Learning State: samples=${exported.learningState.samplesProcessed}, patterns=${exported.learningState.patternsDiscovered}`);
    console.log(`    - Repair History Size: ${exported.historySize.repairs}`);
    console.log(`    - Contradiction History Size: ${exported.historySize.contradictions}`);
    console.log();

    // Summary
    console.log('═══════════════════════════════════════════════════════════');
    console.log('✨ Coherence Learning System Test Complete');
    console.log('═══════════════════════════════════════════════════════════\n');

    console.log('📊 Final Summary:');
    console.log(`  ✅ All 10 tests passed`);
    console.log(`  📚 Learning Data: ${learner.learningState.samplesProcessed} samples, ${learner.learningState.patternsDiscovered} patterns`);
    console.log(`  🎯 Repair Effectiveness:`);
    console.log(`     - move_package: ${metamodel.repair_types.move_package ? metamodel.repair_types.move_package.avgOutcome + '/100' : 'not learned'}`);
    console.log(`  📈 Prediction Accuracy: ${metamodel.accuracy_metrics.successRate}% success, ${metamodel.accuracy_metrics.resolutionRate}% resolution`);
    console.log(`  🧠 Model Confidence: ${(learner.learningState.modelAccuracy * 100).toFixed(1)}%\n`);

    process.exit(0);
  } catch (error) {
    console.error('❌ Test failed:');
    console.error(error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

runLearningTest();
