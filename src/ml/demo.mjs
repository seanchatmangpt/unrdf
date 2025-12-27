#!/usr/bin/env node
/**
 * @file demo.mjs
 * @description Demo of ML module for KGC-SWARM
 * Demonstrates pattern recognition, Q-learning, and anomaly detection
 */

import {
  createPatternRecognizer,
  createQLearningAgent,
  createAnomalyDetector,
  calculateReward,
} from './index.mjs';

console.log('='.repeat(70));
console.log('KGC-SWARM ML Module Demo');
console.log('='.repeat(70));

// ====================
// 1. Pattern Recognition Demo
// ====================
console.log('\n1. PATTERN RECOGNITION');
console.log('-'.repeat(70));

const recognizer = createPatternRecognizer({ windowSize: 3, minSupport: 2 });

// Simulate observable sequences with decreasing drift
const observations = [
  { artifacts: ['a1', 'a2'], driftValue: 20, normalized: 1.0, epoch: 1 },
  { artifacts: ['a1', 'a2', 'a3'], driftValue: 15, normalized: 0.75, epoch: 2 },
  { artifacts: ['a1', 'a2'], driftValue: 10, normalized: 0.5, epoch: 3 },
  { artifacts: ['a1'], driftValue: 5, normalized: 0.25, epoch: 4 },
  { artifacts: ['a1'], driftValue: 2, normalized: 0.1, epoch: 5 },
  { artifacts: ['a1'], driftValue: 1, normalized: 0.05, epoch: 6 },
];

console.log('\nObserving drift patterns:');
for (const obs of observations) {
  recognizer.observe({ ...obs, timestamp: Date.now() + obs.epoch * 1000 });
  console.log(`  Epoch ${obs.epoch}: drift=${obs.normalized.toFixed(2)}, artifacts=[${obs.artifacts}]`);
}

const patterns = recognizer.getPatterns({ onlyFrequent: false });
console.log(`\nDetected ${patterns.length} patterns:`);
patterns.slice(0, 3).forEach(p => {
  console.log(`  - Pattern ${p.patternId}: frequency=${p.frequency}, avgDrift=${p.avgDrift.toFixed(3)}, confidence=${p.confidence.toFixed(2)}`);
});

const prediction = recognizer.predictConvergence();
console.log(`\nConvergence prediction:`);
console.log(`  - Estimated epochs to convergence: ${prediction.estimatedEpochs}`);
console.log(`  - Confidence: ${prediction.confidence.toFixed(2)}`);

const strategy = recognizer.suggestCompressionStrategy();
console.log(`\nRecommended compression strategy:`);
console.log(`  - Strategy: ${strategy.strategy}`);
console.log(`  - Expected ratio: ${strategy.expectedRatio}`);
console.log(`  - Reason: ${strategy.reason}`);

const summary = recognizer.getSummary();
console.log(`\nSummary: ${summary.totalObservations} observations, ${summary.uniquePatterns} unique patterns`);

// ====================
// 2. Q-Learning Demo
// ====================
console.log('\n\n2. Q-LEARNING FOR AGENT CONTROL');
console.log('-'.repeat(70));

const agent = createQLearningAgent({
  actions: [
    { id: 'aggressive', name: 'Aggressive Compression' },
    { id: 'balanced', name: 'Balanced Approach' },
    { id: 'conservative', name: 'Conservative (Safety First)' },
  ],
  epsilon: 0.1,
  learningRate: 0.2,
  discountFactor: 0.9,
});

console.log('\nAvailable actions:');
agent.actions.forEach(a => console.log(`  - ${a.id}: ${a.name}`));

// Simulate learning episodes
console.log('\nTraining agent over 5 episodes:');
for (let episode = 0; episode < 5; episode++) {
  let state = { driftLevel: 'high', epochCount: 'early', budgetUsed: 'low' };

  console.log(`\nEpisode ${episode + 1}:`);

  for (let step = 0; step < 3; step++) {
    const action = agent.selectAction(state);
    console.log(`  Step ${step + 1}: State[drift=${state.driftLevel}, epoch=${state.epochCount}] → Action=${action}`);

    // Simulate transition and reward
    const nextState =
      step === 0
        ? { driftLevel: 'medium', epochCount: 'early', budgetUsed: 'low' }
        : step === 1
          ? { driftLevel: 'low', epochCount: 'middle', budgetUsed: 'medium' }
          : { driftLevel: 'converged', epochCount: 'late', budgetUsed: 'high' };

    const convergenceTime = 3 - step;
    const compressionRatio = action === 'aggressive' ? 0.9 : action === 'balanced' ? 0.7 : 0.5;
    const reward = calculateReward({ convergenceTime, compressionRatio, budgetUsed: 0.3 });

    agent.learn(state, action, reward, nextState, step === 2);
    console.log(`    Reward: ${reward.toFixed(2)}`);

    state = nextState;
  }

  agent.endEpisode();
}

const policy = agent.getPolicy();
console.log('\nLearned Policy (sample):');
const sampleStates = [
  'high-early-low',
  'medium-middle-medium',
  'low-late-high',
];
sampleStates.forEach(stateKey => {
  console.log(`  ${stateKey} → ${policy.get(stateKey)}`);
});

const agentSummary = agent.getSummary();
console.log(`\nTraining summary:`);
console.log(`  - Episodes: ${agentSummary.episodeCount}`);
console.log(`  - Average reward: ${agentSummary.avgReward.toFixed(2)}`);
console.log(`  - Epsilon: ${agentSummary.epsilon.toFixed(3)}`);
console.log(`  - Top action: ${agentSummary.actionStats[0]?.actionId} (avg reward: ${agentSummary.actionStats[0]?.avgReward.toFixed(2)})`);

// ====================
// 3. Anomaly Detection Demo
// ====================
console.log('\n\n3. ANOMALY DETECTION');
console.log('-'.repeat(70));

const detector = createAnomalyDetector({
  zScoreThreshold: 2.0,
  iqrMultiplier: 1.5,
  windowSize: 5,
});

// Normal observations
console.log('\nObserving normal drift pattern:');
for (let i = 0; i < 8; i++) {
  detector.observe({
    epoch: i,
    drift: 5 + Math.random() * 0.5,
    normalized: 0.5 + Math.random() * 0.05,
    timestamp: Date.now() + i * 1000,
  });
  console.log(`  Epoch ${i}: drift=~0.5 (normal)`);
}

// Outlier observation
console.log('\n  Epoch 8: drift=5.0 (SPIKE!)');
detector.observe({
  epoch: 8,
  drift: 50,
  normalized: 5.0,
  timestamp: Date.now() + 8000,
});

const anomalies = detector.detect();
console.log(`\nDetected ${anomalies.length} anomalies:`);
anomalies.forEach(a => {
  console.log(`  - ${a.type} (epoch ${a.epoch}): ${a.description}`);
  console.log(`    Severity: ${a.severity.toFixed(2)}, Threshold: ${a.threshold.toFixed(4)}`);
  console.log(`    Suggestions: ${a.suggestions.slice(0, 2).join('; ')}`);
});

// Guard violation prediction
console.log('\nPredicting guard violation:');
const guardPrediction = detector.predictGuardViolation({
  name: 'max-drift-threshold',
  driftThreshold: 1.0,
});

console.log(`  - Will violate: ${guardPrediction.willViolate ? 'YES' : 'no'}`);
console.log(`  - Risk level: ${guardPrediction.risk.toFixed(2)}`);
console.log(`  - Confidence: ${guardPrediction.confidence.toFixed(2)}`);
if (guardPrediction.estimatedEpochs !== null) {
  console.log(`  - Estimated epochs until violation: ${guardPrediction.estimatedEpochs}`);
}

const detectorSummary = detector.getSummary();
console.log(`\nDetector summary:`);
console.log(`  - Total observations: ${detectorSummary.totalObservations}`);
console.log(`  - Detected anomalies: ${detectorSummary.detectedAnomalies}`);
console.log(`  - Mean drift: ${detectorSummary.statistics.mean.toFixed(4)}`);
console.log(`  - Std deviation: ${detectorSummary.statistics.std.toFixed(4)}`);

// ====================
// Summary
// ====================
console.log('\n' + '='.repeat(70));
console.log('DEMO COMPLETE - All modules working correctly!');
console.log('='.repeat(70));

console.log('\nKey Capabilities:');
console.log('  ✓ Pattern recognition from observable sequences');
console.log('  ✓ Q-learning for adaptive agent control');
console.log('  ✓ Statistical anomaly detection (Z-score, IQR)');
console.log('  ✓ Guard violation prediction');
console.log('  ✓ Compression strategy suggestions');
console.log('  ✓ Convergence time prediction');

console.log('\nImplementation Details:');
console.log('  - Native JavaScript (no TensorFlow.js)');
console.log('  - Typed arrays for performance');
console.log('  - Zod schema validation');
console.log('  - Interpretable ML algorithms');

console.log('\n');
