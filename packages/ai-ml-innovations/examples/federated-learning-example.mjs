/**
 * @file Federated Learning Example
 * @module ai-ml-innovations/examples/federated-learning
 *
 * @description
 * Complete example of federated learning with differential privacy
 * for training knowledge graph embeddings across distributed nodes.
 */

import {
  FederatedEmbeddingTrainer,
  FedAvgAggregator,
  DPMechanism,
  PrivacyBudgetTracker,
  SecureAggregation,
} from '../src/index.mjs';

/**
 * Example 1: Basic Federated Learning
 */
async function basicFederatedLearning() {
  console.log('\n=== Example 1: Basic Federated Learning ===\n');

  // Simulate 5 federated nodes with local knowledge graphs
  const nodes = [
    {
      id: 'hospital-1',
      graph: [
        { subject: 'Patient1', predicate: 'diagnosed_with', object: 'Diabetes' },
        { subject: 'Patient2', predicate: 'diagnosed_with', object: 'Hypertension' },
        { subject: 'Diabetes', predicate: 'treated_by', object: 'Insulin' },
      ],
    },
    {
      id: 'hospital-2',
      graph: [
        { subject: 'Patient3', predicate: 'diagnosed_with', object: 'Diabetes' },
        { subject: 'Patient4', predicate: 'diagnosed_with', object: 'Asthma' },
        { subject: 'Asthma', predicate: 'treated_by', object: 'Inhaler' },
      ],
    },
    {
      id: 'hospital-3',
      graph: [
        { subject: 'Patient5', predicate: 'diagnosed_with', object: 'Hypertension' },
        { subject: 'Patient6', predicate: 'diagnosed_with', object: 'Diabetes' },
        { subject: 'Hypertension', predicate: 'treated_by', object: 'ACE_Inhibitor' },
      ],
    },
    {
      id: 'hospital-4',
      graph: [
        { subject: 'Patient7', predicate: 'diagnosed_with', object: 'Asthma' },
        { subject: 'Patient8', predicate: 'diagnosed_with', object: 'Diabetes' },
      ],
    },
    {
      id: 'hospital-5',
      graph: [
        { subject: 'Patient9', predicate: 'diagnosed_with', object: 'Hypertension' },
        { subject: 'Patient10', predicate: 'diagnosed_with', object: 'Asthma' },
      ],
    },
  ];

  // Create federated trainer with differential privacy
  const trainer = new FederatedEmbeddingTrainer({
    nodes,
    embeddingDim: 64,
    aggregationStrategy: 'fedavg',
    privacyBudget: 1.0, // ε = 1.0 (strong privacy)
    noiseMultiplier: 0.5,
    clippingNorm: 1.0,
    enableDifferentialPrivacy: true,
  });

  console.log('Configuration:');
  console.log('  Nodes:', nodes.length);
  console.log('  Embedding dimension:', trainer.config.embeddingDim);
  console.log('  Privacy budget (ε):', trainer.config.privacyBudget);
  console.log('  Differential privacy:', trainer.config.enableDifferentialPrivacy);

  // Train federated embeddings
  const result = await trainer.trainFederated({
    epochs: 20,
    localEpochs: 5,
    batchSize: 2,
    convergenceThreshold: 0.01,
  });

  console.log('\nTraining Results:');
  console.log('  Final model version:', result.model.version);
  console.log('  Privacy spent:', result.privacySpent.toFixed(4), 'ε');
  console.log('  Convergence round:', result.stats.convergenceRound || 'N/A');
  console.log('  Total rounds:', result.stats.rounds);
  console.log('  Avg communication time:', result.stats.avgCommunicationTime.toFixed(2), 'ms');

  // Show learned embeddings
  console.log('\nLearned Entity Embeddings:');
  for (const [entity, embedding] of Object.entries(result.model.entityEmbeddings)) {
    console.log(`  ${entity}: [${embedding.slice(0, 5).map(v => v.toFixed(3)).join(', ')}...]`);
  }

  console.log('\nLearned Relation Embeddings:');
  for (const [relation, embedding] of Object.entries(result.model.relationEmbeddings)) {
    console.log(`  ${relation}: [${embedding.slice(0, 5).map(v => v.toFixed(3)).join(', ')}...]`);
  }

  return result;
}

/**
 * Example 2: Manual FedAvg Aggregation
 */
function manualFedAvg() {
  console.log('\n=== Example 2: Manual FedAvg Aggregation ===\n');

  // Create FedAvg aggregator
  const aggregator = new FedAvgAggregator({
    learningRate: 0.01,
    momentum: 0.9,
    weightDecay: 0.0001,
  });

  // Simulate client updates
  const clientUpdates = [
    {
      nodeId: 'client-1',
      gradients: {
        'entity_Alice': [0.5, -0.3, 0.8],
        'relation_knows': [0.1, 0.2],
      },
      sampleCount: 100,
      epoch: 0,
      timestamp: Date.now(),
    },
    {
      nodeId: 'client-2',
      gradients: {
        'entity_Alice': [0.4, -0.2, 0.9],
        'relation_knows': [0.15, 0.25],
      },
      sampleCount: 150,
      epoch: 0,
      timestamp: Date.now(),
    },
    {
      nodeId: 'client-3',
      gradients: {
        'entity_Alice': [0.6, -0.4, 0.7],
        'relation_knows': [0.12, 0.18],
      },
      sampleCount: 120,
      epoch: 0,
      timestamp: Date.now(),
    },
  ];

  const globalModel = {
    entityEmbeddings: { Alice: [0, 0, 0] },
    relationEmbeddings: { knows: [0, 0] },
    version: 0,
    timestamp: Date.now(),
  };

  console.log('Client Updates:');
  for (const update of clientUpdates) {
    console.log(`  ${update.nodeId}: ${update.sampleCount} samples`);
  }

  // Aggregate
  const aggregated = aggregator.aggregate(clientUpdates, globalModel);

  console.log('\nAggregated Model:');
  console.log('  Version:', aggregated.version);
  console.log('  Alice embedding:', aggregated.entityEmbeddings.Alice.map(v => v.toFixed(4)));
  console.log('  knows embedding:', aggregated.relationEmbeddings.knows.map(v => v.toFixed(4)));

  return aggregated;
}

/**
 * Example 3: Differential Privacy Mechanism
 */
function differentialPrivacy() {
  console.log('\n=== Example 3: Differential Privacy ===\n');

  // Create DP mechanism
  const mechanism = new DPMechanism({
    mechanism: 'gaussian',
    epsilon: 1.0,
    delta: 1e-5,
    sensitivity: 1.0,
    clippingNorm: 1.0,
  });

  console.log('DP Mechanism Configuration:');
  console.log('  Mechanism:', mechanism.mechanism);
  console.log('  Privacy parameter (ε):', mechanism.epsilon);
  console.log('  Failure probability (δ):', mechanism.delta);
  console.log('  Clipping norm:', mechanism.clippingNorm);
  console.log('  Noise scale (σ):', mechanism.noiseScale.toFixed(4));

  // Original gradients
  const gradients = {
    'entity_Bob': [3.5, -4.2, 1.8], // L2 norm > 1.0
    'relation_likes': [0.5, 0.3],
  };

  console.log('\nOriginal Gradients:');
  console.log('  Bob:', gradients['entity_Bob']);
  console.log('  likes:', gradients['relation_likes']);

  // Clip gradients
  const clipped = mechanism.clipGradients(gradients);
  console.log('\nClipped Gradients (norm ≤ 1.0):');
  console.log('  Bob:', clipped['entity_Bob'].map(v => v.toFixed(4)));
  console.log('  likes:', clipped['relation_likes'].map(v => v.toFixed(4)));

  // Add noise
  const privatized = mechanism.privatize(gradients);
  console.log('\nPrivatized Gradients (clipped + noised):');
  console.log('  Bob:', privatized['entity_Bob'].map(v => v.toFixed(4)));
  console.log('  likes:', privatized['relation_likes'].map(v => v.toFixed(4)));

  return { original: gradients, clipped, privatized };
}

/**
 * Example 4: Privacy Budget Tracking
 */
function privacyBudgetTracking() {
  console.log('\n=== Example 4: Privacy Budget Tracking ===\n');

  // Create privacy budget tracker with moments accountant
  const tracker = new PrivacyBudgetTracker({
    epsilon: 1.0,
    delta: 1e-5,
    composition: 'moments',
  });

  console.log('Initial Budget:');
  console.log('  Total (ε):', tracker.epsilon);
  console.log('  Spent:', tracker.spent);
  console.log('  Remaining:', tracker.epsilon - tracker.spent);

  // Simulate training rounds
  const rounds = 10;
  console.log(`\nSimulating ${rounds} training rounds...`);

  for (let i = 0; i < rounds; i++) {
    tracker.accountRound({
      noiseMultiplier: 1.0,
      samplingRate: 0.2,
      steps: 1,
    });

    console.log(`  Round ${i + 1}: spent = ${tracker.spent.toFixed(4)}ε, remaining = ${(tracker.epsilon - tracker.spent).toFixed(4)}ε`);

    if (!tracker.canContinue(0.01)) {
      console.log('  ⚠️  Privacy budget nearly exhausted!');
      break;
    }
  }

  const status = tracker.getStatus();
  console.log('\nFinal Status:');
  console.log('  Total spent:', status.spent.toFixed(4), 'ε');
  console.log('  Remaining:', status.remaining.toFixed(4), 'ε');
  console.log('  Exhausted:', status.exhausted);

  return status;
}

/**
 * Example 5: Secure Aggregation
 */
function secureAggregation() {
  console.log('\n=== Example 5: Secure Aggregation ===\n');

  // Create secure aggregation protocol
  const protocol = new SecureAggregation({
    threshold: 3,
    totalNodes: 5,
    enableEncryption: true,
  });

  console.log('Protocol Configuration:');
  console.log('  Threshold:', protocol.threshold);
  console.log('  Total nodes:', protocol.totalNodes);
  console.log('  Encryption:', protocol.enableEncryption);

  // Generate shares for each node
  const nodeIds = ['node-0', 'node-1', 'node-2', 'node-3', 'node-4'];
  for (const nodeId of nodeIds) {
    protocol.generateShares(nodeId);
  }

  console.log('\nShares generated for all nodes');

  // Original gradients (same for all nodes for demonstration)
  const originalGradients = {
    'entity_Charlie': [1.0, 2.0, 3.0],
  };

  console.log('\nOriginal Gradients:');
  console.log('  Charlie:', originalGradients['entity_Charlie']);

  // Each node masks their gradients
  const maskedUpdates = nodeIds.slice(0, 4).map(nodeId => ({
    nodeId,
    gradients: protocol.maskGradients(nodeId, originalGradients),
    sampleCount: 100,
    epoch: 0,
    timestamp: Date.now(),
  }));

  console.log('\nMasked Gradients (server sees only masked values):');
  for (const update of maskedUpdates) {
    console.log(`  ${update.nodeId}: [${update.gradients['entity_Charlie'].map(v => v.toFixed(4)).join(', ')}]`);
  }

  // Server aggregates (masks cancel out)
  const aggregated = protocol.aggregateMasked(maskedUpdates);

  console.log('\nAggregated Gradients (masks canceled):');
  console.log('  Charlie:', aggregated['entity_Charlie'].map(v => v.toFixed(4)));
  console.log('\nNote: Aggregated values should match original (all nodes had same gradients)');

  return aggregated;
}

/**
 * Main execution
 */
async function main() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║  Federated Learning for Knowledge Graphs                  ║');
  console.log('║  Privacy-Preserving Distributed Training                  ║');
  console.log('╚════════════════════════════════════════════════════════════╝');

  try {
    // Run examples
    await basicFederatedLearning();
    manualFedAvg();
    differentialPrivacy();
    privacyBudgetTracking();
    secureAggregation();

    console.log('\n✅ All examples completed successfully!');
  } catch (error) {
    console.error('\n❌ Error:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export {
  basicFederatedLearning,
  manualFedAvg,
  differentialPrivacy,
  privacyBudgetTracking,
  secureAggregation,
};
