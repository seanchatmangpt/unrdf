/**
 * Image Classifier Training Example with Automatic Versioning
 *
 * This example demonstrates:
 * 1. Training a simple neural network classifier
 * 2. Automatic versioning after each training epoch
 * 3. Time-travel queries to previous model states
 * 4. Hash chain verification for provenance
 */

import * as tf from '@tensorflow/tfjs-node';
import { MLVersionStore } from '../version-store.mjs';

/**
 * Generate synthetic classification data (simulating images)
 */
function generateTrainingData(numSamples = 100) {
  const data = [];
  const labels = [];

  for (let i = 0; i < numSamples; i++) {
    // Generate random features (simulating flattened image pixels)
    const features = Array.from({ length: 28 }, () => Math.random());

    // Simple classification: sum > 14 = class 1, else class 0
    const sum = features.reduce((a, b) => a + b, 0);
    const label = sum > 14 ? 1 : 0;

    data.push(features);
    labels.push(label);
  }

  return {
    x: tf.tensor2d(data),
    y: tf.oneHot(tf.tensor1d(labels, 'int32'), 2),
  };
}

/**
 * Create a simple image classifier model
 */
function createModel() {
  const model = tf.sequential({
    layers: [
      tf.layers.dense({ units: 16, inputShape: [28], activation: 'relu' }),
      tf.layers.dropout({ rate: 0.2 }),
      tf.layers.dense({ units: 8, activation: 'relu' }),
      tf.layers.dense({ units: 2, activation: 'softmax' }),
    ],
  });

  model.compile({
    optimizer: tf.train.adam(0.001),
    loss: 'categoricalCrossentropy',
    metrics: ['accuracy'],
  });

  return model;
}

/**
 * Main training loop with automatic versioning
 */
async function trainWithVersioning() {
  console.log('🚀 Starting Image Classifier Training with ML Versioning\n');

  // Initialize version store
  const versionStore = new MLVersionStore({
    nodeId: 'trainer-001',
    storageDir: '.ml-models',
  });

  // Create model and training data
  const model = createModel();
  const { x, y } = generateTrainingData(200);

  console.log('📊 Model Architecture:');
  model.summary();
  console.log('');

  // Store initial model version (untrained)
  console.log('💾 Saving initial model version...');
  const initialReceipt = await versionStore.saveVersion(
    model,
    {
      name: 'image-classifier',
      version: 'v0.0-untrained',
      description: 'Initial untrained model',
      architecture: 'simple-feedforward',
    },
    {
      epoch: 0,
      accuracy: 0.0,
      loss: 0.0,
      batchSize: 32,
      learningRate: 0.001,
    }
  );

  console.log(`✅ Initial version saved: ${initialReceipt.versionId}`);
  console.log(`   Hash: ${initialReceipt.hash.substring(0, 16)}...`);
  console.log(`   Event count: ${initialReceipt.receipt.event_count}\n`);

  // Training loop with versioning after each epoch
  const epochs = 5;
  const versionReceipts = [initialReceipt];

  for (let epoch = 1; epoch <= epochs; epoch++) {
    console.log(`🏋️  Training Epoch ${epoch}/${epochs}...`);

    const history = await model.fit(x, y, {
      epochs: 1,
      batchSize: 32,
      validationSplit: 0.2,
      verbose: 0,
    });

    const loss = history.history.loss[0];
    const accuracy = history.history.acc[0];

    console.log(`   Loss: ${loss.toFixed(4)}, Accuracy: ${accuracy.toFixed(4)}`);

    // Save model version after epoch
    const receipt = await versionStore.saveVersion(
      model,
      {
        name: 'image-classifier',
        version: `v0.${epoch}`,
        description: `Model after epoch ${epoch}`,
        architecture: 'simple-feedforward',
      },
      {
        epoch,
        accuracy,
        loss,
        batchSize: 32,
        learningRate: 0.001,
      }
    );

    console.log(`   ✅ Version saved: ${receipt.versionId.substring(0, 8)}...`);
    console.log(`   Hash: ${receipt.hash.substring(0, 16)}...`);
    console.log(`   Previous Hash: ${receipt.previousHash?.substring(0, 16) || 'null'}...\n`);

    versionReceipts.push(receipt);
  }

  // Demonstrate time-travel capabilities
  console.log('⏰ Time-Travel Demonstration\n');

  // Get version history
  const history = await versionStore.getVersionHistory('image-classifier');
  console.log(`📜 Version History (${history.length} versions):`);
  history.forEach((v, i) => {
    console.log(`   ${i + 1}. ${new Date(v.timestamp).toISOString()} - Accuracy: ${v.metrics.accuracy?.toFixed(4) || 'N/A'}`);
  });
  console.log('');

  // Time-travel to epoch 3
  const epoch3Time = versionReceipts[3].timestamp;
  console.log(`🕐 Time-traveling to epoch 3 (${new Date(epoch3Time).toISOString()})...`);
  const epoch3Version = await versionStore.getVersionAtTime('image-classifier', epoch3Time);
  console.log(`   Loaded version: ${epoch3Version.versionId.substring(0, 8)}...`);
  console.log(`   Accuracy at that time: ${epoch3Version.metrics.accuracy.toFixed(4)}`);
  console.log(`   Loss at that time: ${epoch3Version.metrics.loss.toFixed(4)}\n`);

  // Verify hash chain integrity
  console.log('🔐 Verifying Hash Chain Integrity...');
  const verification = await versionStore.verifyHashChain('image-classifier');
  console.log(`   Chain Valid: ${verification.valid ? '✅ YES' : '❌ NO'}`);
  console.log(`   Total Versions: ${verification.totalVersions}`);
  console.log(`   Verifications:`);
  verification.verifications.forEach((v, i) => {
    const status = v.valid ? '✅' : '❌';
    console.log(`      ${i + 1}. ${status} ${v.versionId.substring(0, 8)}... -> ${v.hash.substring(0, 12)}...`);
  });
  console.log('');

  // Compare first and last versions
  console.log('📊 Comparing Initial vs Final Model...');
  const comparison = await versionStore.compareVersions(
    versionReceipts[0].versionId,
    versionReceipts[versionReceipts.length - 1].versionId
  );

  console.log('   Metrics Delta:');
  Object.entries(comparison.metricsDelta).forEach(([key, delta]) => {
    if (delta.change !== undefined) {
      const arrow = delta.change > 0 ? '↑' : '↓';
      console.log(`      ${key}: ${delta.from.toFixed(4)} → ${delta.to.toFixed(4)} ${arrow} ${Math.abs(delta.change).toFixed(4)}`);
    }
  });
  console.log(`   Time Elapsed: ${(comparison.timestampDelta / 1000).toFixed(2)}s\n`);

  // Cleanup
  tf.dispose([x, y]);

  console.log('✨ Training and versioning demonstration complete!');
  console.log(`\n📈 Final Model Performance:`);
  console.log(`   Accuracy: ${versionReceipts[versionReceipts.length - 1].receipt.event_count} versions created`);
  console.log(`   All versions cryptographically linked via BLAKE3 hash chain`);
  console.log(`   Time-travel enabled for any point in training history\n`);

  return {
    model,
    versionStore,
    versionReceipts,
    verification,
  };
}

// Run example if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  trainWithVersioning()
    .then(() => {
      console.log('Example completed successfully');
      process.exit(0);
    })
    .catch((error) => {
      console.error('Example failed:', error);
      process.exit(1);
    });
}

export { trainWithVersioning, createModel, generateTrainingData };
