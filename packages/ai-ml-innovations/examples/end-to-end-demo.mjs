/**
 * @file End-to-end demo of AI/ML innovations
 * @module ai-ml-innovations/examples
 *
 * @description
 * Demonstrates all three implemented patterns:
 * - Temporal GNN for link prediction
 * - Neural-Symbolic reasoning
 * - Federated learning
 */

import {
  createTemporalGNN,
  createNeuralSymbolicReasoner,
  createFederatedEmbeddingTrainer,
} from '../src/index.mjs';

/**
 * Demo 1: Temporal GNN Link Prediction
 */
async function demoTemporalGNN() {
  console.log('\n=== Temporal GNN Demo ===\n');

  // Create TGNN
  const tgnn = createTemporalGNN({
    embeddingDim: 64,
    temporalWindow: 5,
    aggregation: 'attention',
    attentionHeads: 2,
  });

  // Create temporal snapshots
  const snapshots = [];
  for (let t = 0; t < 5; t++) {
    const graph = createMockGraph(10 + t * 2); // Growing graph
    snapshots.push({
      timestamp: Date.now() + t * 1000,
      receiptId: `receipt_t${t}`,
      graph,
    });
  }

  console.log(`Training on ${snapshots.length} temporal snapshots...`);

  // Train
  const trainResults = await tgnn.train(snapshots, { epochs: 20 });
  console.log('Training complete:', trainResults);

  // Setup temporal history for prediction
  const nodeId = 'http://example.org/entity1';
  tgnn.temporalHistory.set(nodeId, snapshots);

  // Predict future links
  console.log(`\nPredicting future links for ${nodeId}...`);
  const predictions = await tgnn.predictFutureLinks(nodeId, 5, {
    topK: 5,
    threshold: 0.6,
  });

  console.log(`\nTop ${predictions.length} predictions:`);
  predictions.forEach((pred, idx) => {
    console.log(
      `${idx + 1}. ${pred.subject} -> ${pred.predicate} -> ${pred.object} ` +
      `(score: ${pred.score.toFixed(3)}, confidence: ${pred.confidence.toFixed(3)})`
    );
  });

  const stats = tgnn.getStats();
  console.log('\nTGNN Statistics:', stats);
}

/**
 * Demo 2: Neural-Symbolic Reasoning
 */
async function demoNeuralSymbolic() {
  console.log('\n=== Neural-Symbolic Reasoning Demo ===\n');

  // Create reasoner
  const reasoner = createNeuralSymbolicReasoner({
    embeddingDim: 64,
    symbolicWeight: 0.6,
    neuralWeight: 0.4,
    minConfidence: 0.5,
    enableExplanations: true,
  });

  // Define SHACL rules
  const shaclShapes = [
    {
      id: 'rule_employment',
      name: 'Employment Rule',
      description: 'If person works at company, they are employed by company',
      conditions: [
        {
          subject: '?person',
          predicate: 'http://example.org/worksAt',
          object: '?company',
        },
      ],
      conclusion: {
        subject: '?person',
        predicate: 'http://example.org/employedBy',
        object: '?company',
      },
    },
    {
      id: 'rule_friendship',
      name: 'Friendship Rule',
      description: 'If person knows another person, they may be friends',
      conditions: [
        {
          subject: '?person1',
          predicate: 'http://example.org/knows',
          object: '?person2',
        },
      ],
      conclusion: {
        subject: '?person1',
        predicate: 'http://example.org/friendOf',
        object: '?person2',
      },
    },
  ];

  console.log(`Learning ${shaclShapes.length} rule embeddings...`);
  const learnResults = await reasoner.learnRuleEmbeddings(shaclShapes);
  console.log('Rule learning complete:', learnResults);

  // Test inference
  const testTriples = [
    {
      subject: 'http://example.org/Alice',
      predicate: 'http://example.org/worksAt',
      object: 'http://example.org/CompanyA',
    },
    {
      subject: 'http://example.org/Bob',
      predicate: 'http://example.org/knows',
      object: 'http://example.org/Charlie',
    },
  ];

  for (const triple of testTriples) {
    console.log(`\nInferring from: ${triple.subject} -> ${triple.predicate} -> ${triple.object}`);

    const inferences = await reasoner.infer(triple);

    console.log(`Found ${inferences.length} inferences:`);
    inferences.forEach((inf, idx) => {
      console.log(
        `${idx + 1}. [${inf.method.toUpperCase()}] ${inf.triple.subject} -> ${inf.triple.predicate} -> ${inf.triple.object} ` +
        `(confidence: ${inf.confidence.toFixed(3)})`
      );
      if (inf.explanation) {
        console.log(`   Explanation: ${inf.explanation}`);
      }
    });
  }

  const stats = reasoner.getStats();
  console.log('\nReasoner Statistics:', stats);
}

/**
 * Demo 3: Federated Learning
 */
async function demoFederatedLearning() {
  console.log('\n=== Federated Learning Demo ===\n');

  // Create mock federated nodes
  const nodes = [];
  for (let i = 0; i < 3; i++) {
    nodes.push({
      id: `node_${i}`,
      name: `Node ${i}`,
      graph: createMockGraph(15 + i * 5), // Different sizes
    });
  }

  console.log(`Created ${nodes.length} federated nodes:`);
  nodes.forEach((node, idx) => {
    console.log(`  ${idx + 1}. ${node.name} (${node.graph.length} triples)`);
  });

  // Create trainer
  const trainer = createFederatedEmbeddingTrainer({
    nodes,
    embeddingDim: 32,
    aggregationStrategy: 'fedavg',
    privacyBudget: 1.0,
    enableDifferentialPrivacy: true,
    noiseMultiplier: 0.1,
    clippingNorm: 1.0,
  });

  console.log('\nStarting federated training...');

  // Train
  const results = await trainer.trainFederated({
    epochs: 5,
    localEpochs: 3,
    batchSize: 8,
    convergenceThreshold: 0.001,
  });

  console.log('\nFederated training complete!');
  console.log(`Model version: ${results.model.version}`);
  console.log(`Privacy spent: ${results.privacySpent.toFixed(4)}ε / ${trainer.config.privacyBudget}ε`);
  console.log(`Privacy remaining: ${(trainer.config.privacyBudget - results.privacySpent).toFixed(4)}ε`);

  console.log('\nTraining history:');
  results.trainingHistory.forEach((entry, idx) => {
    console.log(
      `  Epoch ${entry.epoch}: Loss=${entry.loss.toFixed(4)}, ` +
      `Accuracy=${(entry.accuracy * 100).toFixed(2)}%, ` +
      `Time=${entry.roundTime}ms`
    );
  });

  const stats = trainer.getStats();
  console.log('\nFederated Trainer Statistics:', stats);

  // Show model size
  const entityCount = Object.keys(results.model.entityEmbeddings).length;
  const relationCount = Object.keys(results.model.relationEmbeddings).length;
  console.log(`\nLearned embeddings:`);
  console.log(`  Entities: ${entityCount}`);
  console.log(`  Relations: ${relationCount}`);
}

/**
 * Helper: Create mock RDF graph
 */
function createMockGraph(numTriples) {
  const graph = [];

  for (let i = 0; i < numTriples; i++) {
    graph.push({
      subject: { value: `http://example.org/entity${i % 10}` },
      predicate: { value: `http://example.org/relatedTo` },
      object: { value: `http://example.org/entity${(i + 1) % 10}` },
    });
  }

  // Add iterator
  graph[Symbol.iterator] = function* () {
    for (const triple of this) {
      yield triple;
    }
  };

  return graph;
}

/**
 * Run all demos
 */
async function runAllDemos() {
  console.log('╔══════════════════════════════════════════════════════════╗');
  console.log('║  UNRDF AI/ML Innovations - End-to-End Demo              ║');
  console.log('╚══════════════════════════════════════════════════════════╝');

  try {
    await demoTemporalGNN();
    await demoNeuralSymbolic();
    await demoFederatedLearning();

    console.log('\n╔══════════════════════════════════════════════════════════╗');
    console.log('║  All demos completed successfully!                      ║');
    console.log('╚══════════════════════════════════════════════════════════╝\n');
  } catch (error) {
    console.error('\n❌ Demo failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllDemos();
}

export { demoTemporalGNN, demoNeuralSymbolic, demoFederatedLearning };
