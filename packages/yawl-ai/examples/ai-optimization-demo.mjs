#!/usr/bin/env node
/**
 * @file AI Workflow Optimization Demo
 * @description
 * Comprehensive demonstration of ML-powered workflow optimization using
 * TensorFlow.js and YAWL patterns. Shows path prediction, performance
 * optimization, and anomaly detection in action.
 */

import { createPredictor, createOptimizer, createDetector } from '../src/index.mjs';

// =============================================================================
// Demo Data Generator
// =============================================================================

/**
 * Generate synthetic workflow execution data
 */
function generateTrainingData() {
  console.log('\n=== Generating Synthetic Workflow Data ===\n');

  // Define common workflow patterns
  const normalPatterns = [
    ['start', 'validate', 'process', 'review', 'approve', 'end'],
    ['start', 'validate', 'process', 'approve', 'end'],
    ['start', 'validate', 'review', 'process', 'approve', 'end'],
    ['start', 'validate', 'process', 'review', 'reject', 'revise', 'process', 'approve', 'end'],
  ];

  const sequences = [];
  const executions = [];
  const workflowExecutions = [];

  // Generate 100 normal workflow executions
  for (let i = 0; i < 100; i++) {
    const pattern = normalPatterns[Math.floor(Math.random() * normalPatterns.length)];
    const workflowId = `workflow-${Math.floor(Math.random() * 3) + 1}`;
    const caseId = `case-${i}`;

    // Add some variance to durations
    const baseDurations = {
      start: 100,
      validate: 500,
      process: 2000,
      review: 1500,
      approve: 300,
      reject: 200,
      revise: 1800,
      end: 100,
    };

    const durations = pattern.map((task) => {
      const base = baseDurations[task] || 1000;
      return base + (Math.random() - 0.5) * base * 0.3; // ±15% variance
    });

    // Workflow sequence for predictor
    sequences.push({
      taskIds: pattern,
      durations,
      workflowId,
    });

    // Execution data for optimizer
    const tasks = pattern.map((taskId, idx) => ({
      taskId,
      duration: durations[idx],
      startTime: Date.now() + durations.slice(0, idx).reduce((a, b) => a + b, 0),
      dependencies: idx > 0 ? [pattern[idx - 1]] : [],
    }));

    executions.push({
      workflowId,
      tasks,
    });

    // Workflow execution for anomaly detector
    const events = pattern.map((taskId, idx) => ({
      taskId,
      duration: durations[idx],
    }));

    workflowExecutions.push({
      caseId,
      workflowId,
      events,
      totalDuration: durations.reduce((a, b) => a + b, 0),
    });
  }

  // Add some anomalous executions (for testing detector)
  for (let i = 0; i < 10; i++) {
    const anomalousPattern = ['start', 'validate', 'unknown-task', 'end']; // Unusual sequence
    const workflowId = `workflow-1`;
    const caseId = `anomaly-case-${i}`;

    const events = anomalousPattern.map((taskId) => ({
      taskId,
      duration: Math.random() * 10000, // Unusual durations
    }));

    workflowExecutions.push({
      caseId,
      workflowId,
      events,
      totalDuration: events.reduce((sum, e) => sum + e.duration, 0),
    });
  }

  console.log(`Generated ${sequences.length} workflow sequences`);
  console.log(`Generated ${executions.length} execution traces`);
  console.log(`Generated ${workflowExecutions.length} workflow executions (10 anomalous)`);

  return { sequences, executions, workflowExecutions };
}

// =============================================================================
// Demo Functions
// =============================================================================

/**
 * Demo 1: Workflow Path Prediction
 */
async function demoPathPrediction(sequences) {
  console.log('\n=== Demo 1: Workflow Path Prediction ===\n');

  const predictor = createPredictor({
    maxTasks: 20,
    sequenceLength: 5,
    hiddenUnits: 32,
    learningRate: 0.01,
  });

  console.log('Training path predictor...');
  await predictor.train(sequences, {
    epochs: 30,
    batchSize: 16,
    validationSplit: 0.2,
  });

  console.log('\nModel Summary:');
  console.log(JSON.stringify(predictor.getSummary(), null, 2));

  // Test prediction
  console.log('\n--- Prediction Test ---');
  const testSequence = ['start', 'validate', 'process'];
  console.log(`Input sequence: ${testSequence.join(' -> ')}`);

  const prediction = await predictor.predict(testSequence);
  console.log(`\nPredicted next task: ${prediction.nextTask}`);
  console.log(`Confidence: ${(prediction.confidence * 100).toFixed(2)}%`);
  console.log(`Estimated duration: ${prediction.estimatedDuration.toFixed(0)}ms`);

  console.log('\nAlternative paths:');
  const sorted = Object.entries(prediction.alternatives)
    .sort((a, b) => b[1] - a[1])
    .slice(0, 3);
  sorted.forEach(([task, prob]) => {
    console.log(`  - ${task}: ${(prob * 100).toFixed(2)}%`);
  });

  // Test resource suggestions
  console.log('\n--- Resource Allocation Suggestions ---');
  const resources = await predictor.suggestResources(testSequence, 3);
  console.log(`Critical path: ${resources.criticalPath.join(' -> ')}`);
  console.log(
    `Estimated total duration: ${resources.estimatedTotalDuration.toFixed(0)}ms`,
  );
  if (resources.recommendedParallelization.length > 0) {
    console.log(
      `Parallelization candidates: ${resources.recommendedParallelization.join(', ')}`,
    );
  }

  predictor.dispose();
  console.log('\nPath prediction demo completed ✓');
}

/**
 * Demo 2: Performance Optimization
 */
async function demoPerformanceOptimization(executions) {
  console.log('\n=== Demo 2: Performance Optimization ===\n');

  const optimizer = createOptimizer({
    bottleneckThreshold: 0.75,
    minExecutions: 3,
  });

  console.log('Analyzing workflow performance...');
  await optimizer.analyze(executions);

  console.log('\nOptimizer Summary:');
  console.log(JSON.stringify(optimizer.getSummary(), null, 2));

  // Identify bottlenecks
  console.log('\n--- Bottleneck Analysis ---');
  const bottlenecks = optimizer.identifyBottlenecks();

  if (bottlenecks.length > 0) {
    console.log(`Found ${bottlenecks.length} bottlenecks:\n`);
    bottlenecks.slice(0, 3).forEach((bottleneck, idx) => {
      console.log(`${idx + 1}. Task: ${bottleneck.taskId}`);
      console.log(`   Impact: ${bottleneck.impact.toFixed(0)}ms potential savings`);
      console.log(`   Reason: ${bottleneck.reason}`);
      console.log(`   Recommendations:`);
      bottleneck.recommendations.forEach((rec) => {
        console.log(`     • ${rec}`);
      });
      console.log('');
    });
  } else {
    console.log('No significant bottlenecks detected');
  }

  // Find parallelization opportunities
  console.log('--- Parallelization Opportunities ---');
  const opportunities = optimizer.findParallelizationOpportunities();
  const oppCount = Object.keys(opportunities).length;

  if (oppCount > 0) {
    console.log(`Found ${oppCount} parallelization opportunities:\n`);
    Object.entries(opportunities)
      .slice(0, 3)
      .forEach(([task, siblings]) => {
        console.log(`Task "${task}" can run in parallel with:`);
        siblings.forEach((sibling) => console.log(`  • ${sibling}`));
        console.log('');
      });
  } else {
    console.log('No clear parallelization opportunities detected');
  }

  // Generate full report
  console.log('--- Optimization Report ---');
  const report = await optimizer.generateReport();
  console.log(`\nEstimated speedup: ${report.estimatedSpeedup.toFixed(2)}%`);
  console.log(
    `Total bottlenecks: ${report.bottlenecks.length}`,
  );
  console.log(
    `Parallelization opportunities: ${Object.keys(report.parallelizationOpportunities).length}`,
  );

  if (Object.keys(report.resourceRecommendations).length > 0) {
    console.log('\nTop Resource Recommendations:');
    Object.entries(report.resourceRecommendations)
      .slice(0, 3)
      .forEach(([task, rec]) => {
        console.log(`  • ${task}: Priority ${rec.priority}, ${rec.suggestedResources} resources`);
      });
  }

  console.log('\nPerformance optimization demo completed ✓');
}

/**
 * Demo 3: Anomaly Detection
 */
async function demoAnomalyDetection(workflowExecutions) {
  console.log('\n=== Demo 3: Anomaly Detection ===\n');

  const detector = createDetector({
    encodingDim: 8,
    sequenceLength: 6,
    anomalyThreshold: 2.0,
  });

  // Train on normal executions only (first 100)
  const normalExecutions = workflowExecutions.slice(0, 100);
  const testExecutions = workflowExecutions.slice(100); // Anomalous ones

  console.log(`Training detector on ${normalExecutions.length} normal executions...`);
  await detector.train(normalExecutions, {
    epochs: 30,
    batchSize: 8,
  });

  console.log('\nDetector Summary:');
  console.log(JSON.stringify(detector.getSummary(), null, 2));

  // Test on normal execution
  console.log('\n--- Testing on Normal Execution ---');
  const normalTest = normalExecutions[0];
  console.log(`Testing case: ${normalTest.caseId}`);
  console.log(
    `Sequence: ${normalTest.events.map((e) => e.taskId).join(' -> ')}`,
  );

  const normalAnomalies = await detector.detect(normalTest);
  if (normalAnomalies.length === 0) {
    console.log('✓ No anomalies detected (expected)');
  } else {
    console.log(`⚠ Detected ${normalAnomalies.length} anomalies (unexpected)`);
  }

  // Test on anomalous execution
  console.log('\n--- Testing on Anomalous Execution ---');
  const anomalousTest = testExecutions[0];
  console.log(`Testing case: ${anomalousTest.caseId}`);
  console.log(
    `Sequence: ${anomalousTest.events.map((e) => e.taskId).join(' -> ')}`,
  );

  const detectedAnomalies = await detector.detect(anomalousTest);
  if (detectedAnomalies.length > 0) {
    console.log(`✓ Detected ${detectedAnomalies.length} anomalies:\n`);

    detectedAnomalies.forEach((anomaly, idx) => {
      console.log(`${idx + 1}. Type: ${anomaly.type}`);
      console.log(`   Severity: ${(anomaly.severity * 100).toFixed(2)}%`);
      console.log(`   Description: ${anomaly.description}`);
      console.log(`   Suggestions:`);
      anomaly.suggestions.forEach((suggestion) => {
        console.log(`     • ${suggestion}`);
      });
      console.log('');
    });
  } else {
    console.log('⚠ No anomalies detected (unexpected)');
  }

  // Test multiple anomalies
  console.log('--- Batch Anomaly Detection ---');
  let totalAnomalies = 0;
  const anomalyTypes = { timing: 0, sequence: 0, pattern: 0, resource: 0 };

  for (const exec of testExecutions) {
    const anomalies = await detector.detect(exec);
    totalAnomalies += anomalies.length;
    anomalies.forEach((a) => {
      anomalyTypes[a.type]++;
    });
  }

  console.log(`\nProcessed ${testExecutions.length} test executions`);
  console.log(`Total anomalies found: ${totalAnomalies}`);
  console.log('Breakdown by type:');
  Object.entries(anomalyTypes).forEach(([type, count]) => {
    if (count > 0) {
      console.log(`  • ${type}: ${count}`);
    }
  });

  // Cleanup (detector will be reused in demo 4, don't dispose yet)
  console.log('\nAnomaly detection demo completed ✓');
}

/**
 * Demo 4: Integrated ML Workflow
 */
async function demoIntegratedWorkflow(data) {
  console.log('\n=== Demo 4: Integrated ML Workflow ===\n');

  console.log('Simulating complete ML-powered workflow optimization...\n');

  // Create all components
  const predictor = createPredictor({ maxTasks: 20, hiddenUnits: 32 });
  const optimizer = createOptimizer();
  const detector = createDetector({ encodingDim: 8 });

  // Train all models
  console.log('1. Training all ML models...');
  const [predHistory, _, detHistory] = await Promise.all([
    predictor.train(data.sequences, { epochs: 20, batchSize: 16 }),
    optimizer.analyze(data.executions),
    detector.train(data.workflowExecutions.slice(0, 100), {
      epochs: 20,
      batchSize: 8,
    }),
  ]);

  console.log('   ✓ All models trained successfully\n');

  // Simulate workflow execution with ML guidance
  console.log('2. Simulating workflow execution with AI guidance...');
  const currentSequence = ['start', 'validate'];
  console.log(`   Current path: ${currentSequence.join(' -> ')}`);

  // Get prediction
  const prediction = await predictor.predict(currentSequence);
  console.log(`   → AI suggests next task: ${prediction.nextTask} (${(prediction.confidence * 100).toFixed(1)}% confidence)`);

  // Check for optimization
  const report = await optimizer.generateReport();
  if (report.bottlenecks.length > 0) {
    const topBottleneck = report.bottlenecks[0];
    console.log(
      `   ⚠ Warning: "${topBottleneck.taskId}" is a bottleneck (${topBottleneck.impact.toFixed(0)}ms impact)`,
    );
  }

  // Simulate anomaly check
  const testExec = data.workflowExecutions[0];
  const anomalies = await detector.detect(testExec);
  console.log(
    `   ✓ Anomaly scan: ${anomalies.length === 0 ? 'Normal execution' : `${anomalies.length} issues detected`}`,
  );

  console.log('\n3. ML Insights Summary:');
  console.log(`   • Predictor: ${predictor.getSummary().vocabularySize} tasks in vocabulary`);
  console.log(`   • Optimizer: ${optimizer.getSummary().totalTasks} tasks analyzed`);
  console.log(`   • Detector: ${detector.getSummary().normalPatterns} normal patterns learned`);
  console.log(`   • Estimated speedup potential: ${report.estimatedSpeedup.toFixed(1)}%`);

  // Cleanup
  try {
    predictor.dispose();
  } catch (e) {
    // Already disposed
  }

  console.log('\nIntegrated ML workflow demo completed ✓');
}

// =============================================================================
// Main Demo
// =============================================================================

async function main() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║   AI-Powered Workflow Optimization Demo                   ║');
  console.log('║   Using TensorFlow.js + YAWL Patterns                      ║');
  console.log('╚════════════════════════════════════════════════════════════╝');

  try {
    // Generate training data
    const data = generateTrainingData();

    // Run all demos
    await demoPathPrediction(data.sequences);
    await demoPerformanceOptimization(data.executions);
    await demoAnomalyDetection(data.workflowExecutions);
    await demoIntegratedWorkflow(data);

    console.log('\n╔════════════════════════════════════════════════════════════╗');
    console.log('║   All Demos Completed Successfully! ✓                     ║');
    console.log('╚════════════════════════════════════════════════════════════╝\n');

    console.log('Summary:');
    console.log('  ✓ Workflow path prediction with neural networks');
    console.log('  ✓ Performance bottleneck identification');
    console.log('  ✓ Parallelization opportunity detection');
    console.log('  ✓ Anomaly detection with autoencoders');
    console.log('  ✓ Integrated ML-powered workflow optimization\n');

    process.exit(0);
  } catch (error) {
    console.error('\n❌ Demo failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run demo
main();
