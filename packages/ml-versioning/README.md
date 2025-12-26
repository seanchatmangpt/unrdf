# @unrdf/ml-versioning

**ML Model Versioning System with RDF Knowledge Graphs and Time-Travel Debugging**

A revolutionary approach to ML model versioning that combines TensorFlow.js with UNRDF's KGC-4D (Knowledge Graph Chronicler 4-Dimensional) time-travel capabilities. Every model version is stored as RDF triples with cryptographic BLAKE3 hash chain proofs, enabling complete provenance tracking and time-travel debugging.

## Features

- **RDF-Based Model Storage**: Serialize TensorFlow.js models as RDF knowledge graph triples
- **Cryptographic Provenance**: BLAKE3 hash chains create immutable lineage proofs
- **Time-Travel Debugging**: Query and restore model state at any point in training history
- **Nanosecond Precision**: KGC-4D timestamps with vector clock causality tracking
- **Version Comparison**: Compute deltas between any two model versions
- **SPARQL Queries**: Leverage full SPARQL capabilities for advanced model exploration

## Installation

```bash
pnpm add @unrdf/ml-versioning
```

## Quick Start

```javascript
import * as tf from '@tensorflow/tfjs-node';
import { MLVersionStore } from '@unrdf/ml-versioning';

// Initialize version store
const versionStore = new MLVersionStore({
  nodeId: 'trainer-001',
  storageDir: '.ml-models'
});

// Create and train a model
const model = tf.sequential({
  layers: [
    tf.layers.dense({ units: 16, inputShape: [8], activation: 'relu' }),
    tf.layers.dense({ units: 2, activation: 'softmax' })
  ]
});

model.compile({
  optimizer: 'adam',
  loss: 'categoricalCrossentropy',
  metrics: ['accuracy']
});

// Save a version with metadata
const receipt = await versionStore.saveVersion(
  model,
  {
    name: 'my-classifier',
    version: 'v1.0',
    description: 'Initial trained model',
    architecture: 'feedforward'
  },
  {
    epoch: 10,
    accuracy: 0.95,
    loss: 0.15,
    batchSize: 32,
    learningRate: 0.001
  }
);

console.log('Version ID:', receipt.versionId);
console.log('Hash:', receipt.hash);
console.log('Previous Hash:', receipt.previousHash);
```

## Core Concepts

### 1. RDF Triple Serialization

Each model version is decomposed into RDF triples:

```turtle
# Model version metadata
<ml-version:abc-123> rdf:type ml:ModelVersion ;
  ml:versionId "abc-123" ;
  ml:modelId "my-classifier" ;
  ml:timestamp "1703001234567" ;
  ml:hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" ;
  ml:previousHash "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592" ;
  ml:modelJson "{...}" ;
  ml:metadata "{...}" ;
  ml:metrics "{...}" .
```

### 2. BLAKE3 Hash Chain

Each version's hash is computed from:
- Model architecture and weights (JSON serialization)
- Metadata (name, version, description)
- Training metrics
- **Previous version's hash** (creating the chain)

```
v0 -> hash0
v1 -> hash1 = BLAKE3(model1 + metadata1 + metrics1 + hash0)
v2 -> hash2 = BLAKE3(model2 + metadata2 + metrics2 + hash1)
v3 -> hash3 = BLAKE3(model3 + metadata3 + metrics3 + hash2)
```

This creates an **immutable provenance chain** - any tampering breaks the chain.

### 3. KGC-4D Time-Travel

KGC-4D provides:
- **Nanosecond timestamps** for each version
- **Vector clocks** for distributed causality
- **Event log** with full history
- **Universe snapshots** at any point in time

## API Reference

### `MLVersionStore`

#### Constructor

```javascript
new MLVersionStore(options)
```

**Options:**
- `nodeId` (string): Identifier for distributed versioning (default: auto-generated)
- `storageDir` (string): Directory for model artifacts (default: '.ml-models')

#### Methods

##### `saveVersion(model, metadata, metrics)`

Save a new model version with cryptographic proof.

```javascript
const receipt = await versionStore.saveVersion(
  model,              // TensorFlow.js LayersModel
  {
    name: string,           // Required: Model identifier
    version?: string,       // Optional: Version label
    description?: string,   // Optional: Description
    architecture?: string   // Optional: Architecture type
  },
  {
    loss?: number,
    accuracy?: number,
    epoch?: number,
    batchSize?: number,
    learningRate?: number,
    custom?: Record<string, any>  // Any custom metrics
  }
);
```

**Returns:**
```javascript
{
  versionId: string,       // Unique version identifier
  modelId: string,         // Model name
  hash: string,            // BLAKE3 hash (64 chars)
  previousHash: string|null,  // Previous version's hash
  timestamp: number,       // Unix timestamp (ms)
  receipt: {
    id: string,            // KGC-4D event ID
    t_ns: string,          // Nanosecond timestamp
    timestamp_iso: string, // ISO 8601 timestamp
    event_count: number    // Total events in store
  }
}
```

##### `loadVersion(versionId)`

Load a specific model version by ID.

```javascript
const { model, metadata, metrics, hash } = await versionStore.loadVersion(versionId);

// Use the model
const predictions = model.predict(inputTensor);

// Remember to dispose when done
model.dispose();
```

**Returns:**
```javascript
{
  model: LayersModel,      // TensorFlow.js model
  metadata: Object,        // Model metadata
  metrics: Object,         // Training metrics
  hash: string,            // Version hash
  previousHash: string|null,
  timestamp: number,
  versionId: string
}
```

##### `getVersionAtTime(modelId, timestamp)`

Time-travel: Get model version at specific timestamp.

```javascript
const pastTime = Date.now() - (60 * 60 * 1000); // 1 hour ago
const version = await versionStore.getVersionAtTime('my-classifier', pastTime);
```

**Returns:** Same as `loadVersion()`

##### `getVersionHistory(modelId)`

Get chronological version history for a model.

```javascript
const history = await versionStore.getVersionHistory('my-classifier');

history.forEach(v => {
  console.log(`${new Date(v.timestamp).toISOString()}: Accuracy ${v.metrics.accuracy}`);
});
```

**Returns:**
```javascript
[
  {
    versionId: string,
    timestamp: number,
    hash: string,
    previousHash: string|null,
    metrics: Object
  },
  ...
]
```

##### `verifyHashChain(modelId)`

Verify cryptographic integrity of version history.

```javascript
const verification = await versionStore.verifyHashChain('my-classifier');

if (verification.valid) {
  console.log('✅ Hash chain integrity verified!');
} else {
  console.error('❌ Hash chain corrupted!');
}
```

**Returns:**
```javascript
{
  modelId: string,
  valid: boolean,          // Overall chain validity
  totalVersions: number,
  verifications: [
    {
      versionId: string,
      valid: boolean,      // This link's validity
      hash: string,
      previousHash: string|null,
      expectedPreviousHash: string|null
    },
    ...
  ]
}
```

##### `compareVersions(versionId1, versionId2)`

Compare two model versions.

```javascript
const comparison = await versionStore.compareVersions(versionA, versionB);

console.log('Accuracy change:', comparison.metricsDelta.accuracy.change);
console.log('Loss improvement:', comparison.metricsDelta.loss.percentChange, '%');
```

**Returns:**
```javascript
{
  version1: {
    id: string,
    metadata: Object,
    metrics: Object,
    hash: string,
    timestamp: number
  },
  version2: { ... },
  metricsDelta: {
    [metricName]: {
      from: number,
      to: number,
      change: number,
      percentChange: number
    }
  },
  timestampDelta: number  // Milliseconds between versions
}
```

## Advanced Usage

### Training Loop with Automatic Versioning

```javascript
const epochs = 10;
const versionReceipts = [];

for (let epoch = 0; epoch < epochs; epoch++) {
  // Train one epoch
  const history = await model.fit(trainX, trainY, {
    epochs: 1,
    batchSize: 32,
    validationSplit: 0.2
  });

  // Save version after each epoch
  const receipt = await versionStore.saveVersion(
    model,
    {
      name: 'continuous-model',
      version: `epoch-${epoch + 1}`,
      description: `Model after epoch ${epoch + 1}`
    },
    {
      epoch: epoch + 1,
      accuracy: history.history.acc[0],
      loss: history.history.loss[0],
      valAccuracy: history.history.val_acc[0],
      valLoss: history.history.val_loss[0]
    }
  );

  versionReceipts.push(receipt);
  console.log(`Epoch ${epoch + 1} saved: ${receipt.hash.substring(0, 12)}...`);
}
```

### Custom SPARQL Queries

Since versions are stored as RDF, you can use SPARQL for advanced queries:

```javascript
// Find all versions with accuracy > 0.9
const highAccuracyQuery = `
  PREFIX ml: <http://ml-versioning.io/ontology#>

  SELECT ?versionId ?metrics
  WHERE {
    ?version ml:modelId "my-classifier" ;
             ml:versionId ?versionId ;
             ml:metrics ?metrics .
  }
`;

const results = await versionStore.store.query(highAccuracyQuery);

// Parse and filter results
const highAccuracy = results
  .map(r => ({
    versionId: r.versionId.value,
    metrics: JSON.parse(r.metrics.value)
  }))
  .filter(v => v.metrics.accuracy > 0.9);
```

### Rollback to Best Performing Version

```javascript
// Get version history
const history = await versionStore.getVersionHistory('production-model');

// Find best accuracy
const bestVersion = history.reduce((best, current) => {
  return current.metrics.accuracy > best.metrics.accuracy ? current : best;
});

console.log(`Best version: ${bestVersion.versionId} (accuracy: ${bestVersion.metrics.accuracy})`);

// Load the best model
const { model: bestModel } = await versionStore.loadVersion(bestVersion.versionId);

// Use for inference
const predictions = bestModel.predict(newData);
```

## Architecture

### System Components

```
┌─────────────────────────────────────────┐
│      TensorFlow.js Model                │
│  (Layers, Weights, Architecture)        │
└──────────────┬──────────────────────────┘
               │
               │ Serialization
               ▼
┌─────────────────────────────────────────┐
│      MLVersionStore                     │
│  • Model JSON Serialization             │
│  • BLAKE3 Hash Computation              │
│  • RDF Triple Generation                │
└──────────────┬──────────────────────────┘
               │
               │ Storage
               ▼
┌─────────────────────────────────────────┐
│      KGC-4D Store                       │
│  • Named Graphs (Universe + EventLog)   │
│  • Nanosecond Timestamps                │
│  • Vector Clock Causality               │
└──────────────┬──────────────────────────┘
               │
               │ Persistence
               ▼
┌─────────────────────────────────────────┐
│      Oxigraph RDF Store                 │
│  • SPARQL 1.1 Query Engine              │
│  • RDF/Turtle Serialization             │
│  • Transaction Support                  │
└─────────────────────────────────────────┘
```

### Hash Chain Structure

```
Genesis Version (v0)
├─ hash0 = BLAKE3(model0 + metadata0 + metrics0)
├─ previousHash: null
│
├─> Version 1 (v1)
    ├─ hash1 = BLAKE3(model1 + metadata1 + metrics1 + hash0)
    ├─ previousHash: hash0
    │
    ├─> Version 2 (v2)
        ├─ hash2 = BLAKE3(model2 + metadata2 + metrics2 + hash1)
        ├─ previousHash: hash1
        │
        └─> Version 3 (v3)
            ├─ hash3 = BLAKE3(model3 + metadata3 + metrics3 + hash2)
            └─ previousHash: hash2
```

### Time-Travel Query Flow

```
1. User: "Get model at timestamp T"
              ↓
2. Query EventLog for latest event ≤ T
              ↓
3. Find version ID from event
              ↓
4. Load model JSON from RDF triples
              ↓
5. Reconstruct TensorFlow.js model
              ↓
6. Return model + metadata + metrics
```

## Testing

Run the comprehensive test suite:

```bash
# Fast tests (no coverage)
pnpm test

# With coverage
pnpm test:coverage

# Watch mode
pnpm test:watch
```

## Example

Run the image classifier example:

```bash
cd packages/ml-versioning
pnpm example
```

This will:
1. Train a simple neural network for 5 epochs
2. Version the model after each epoch
3. Demonstrate time-travel queries
4. Verify hash chain integrity
5. Compare initial vs final model

Expected output:
```
🚀 Starting Image Classifier Training with ML Versioning

📊 Model Architecture:
_________________________________________________________________
Layer (type)                 Output shape              Param #
=================================================================
dense_Dense1 (Dense)         [null,16]                 464
dropout_Dropout1 (Dropout)   [null,16]                 0
dense_Dense2 (Dense)         [null,8]                  136
dense_Dense3 (Dense)         [null,2]                  18
=================================================================
Total params: 618
Trainable params: 618
Non-trainable params: 0

💾 Saving initial model version...
✅ Initial version saved: abc-123...
   Hash: e3b0c44298fc1c14...
   Event count: 1

🏋️  Training Epoch 1/5...
   Loss: 0.6234, Accuracy: 0.6750
   ✅ Version saved: def-456...
   Hash: 4f7c3ae5b1d9a2e8...
   Previous Hash: e3b0c44298fc1c14...

...

⏰ Time-Travel Demonstration
📜 Version History (6 versions)
🕐 Time-traveling to epoch 3...
🔐 Verifying Hash Chain Integrity... ✅ YES
📊 Comparing Initial vs Final Model...

✨ Training and versioning demonstration complete!
```

## Performance Characteristics

- **Serialization**: ~10-50ms per model (depends on size)
- **Hash computation**: ~5-15ms (BLAKE3 is very fast)
- **RDF storage**: ~20-100ms (Oxigraph transaction)
- **Load version**: ~30-150ms (RDF query + model reconstruction)
- **History query**: ~10-50ms (SPARQL query)

## Use Cases

1. **Experiment Tracking**: Version every training run with full hyperparameters
2. **Model Debugging**: Time-travel to any epoch to debug training issues
3. **Reproducibility**: Cryptographic proof ensures exact model provenance
4. **A/B Testing**: Compare different model versions scientifically
5. **Audit Trails**: Complete lineage for regulated industries (healthcare, finance)
6. **Collaborative ML**: Distributed versioning with vector clocks
7. **Model Governance**: SPARQL queries for compliance and discovery

## Why RDF + KGC-4D?

Traditional ML versioning systems (MLflow, DVC, Weights & Biases) use:
- File-based storage (models as binary blobs)
- Relational databases (limited query capabilities)
- No cryptographic guarantees

**@unrdf/ml-versioning** offers:

✅ **Knowledge Graph Benefits**
- Models as first-class semantic entities
- SPARQL query capabilities
- Natural integration with domain ontologies
- Linked data for model relationships

✅ **KGC-4D Benefits**
- True time-travel (not just snapshots)
- Nanosecond precision timestamps
- Vector clock causality for distributed training
- Event sourcing architecture

✅ **Cryptographic Benefits**
- BLAKE3 hash chains (immutable provenance)
- Tamper detection
- Zero-trust verification
- Git-compatible lineage

## Limitations

- Models must be TensorFlow.js compatible (no PyTorch/JAX yet)
- Large models (>100MB) may have slower serialization
- RDF storage is more verbose than binary formats
- SPARQL queries require learning curve

## Future Roadmap

- [ ] PyTorch model support via ONNX
- [ ] Federated learning integration
- [ ] Automatic hyperparameter extraction
- [ ] Model diff visualization
- [ ] Integration with TensorBoard
- [ ] Cloud storage backends (S3, GCS)
- [ ] Compression for large models
- [ ] GraphQL query interface

## Contributing

Contributions welcome! Please read the [UNRDF Contributing Guide](../../CONTRIBUTING.md).

## License

MIT License - see [LICENSE](LICENSE)

## Citation

If you use this work in research, please cite:

```bibtex
@software{unrdf_ml_versioning,
  title={ML Model Versioning with RDF Knowledge Graphs and Time-Travel},
  author={UNRDF Contributors},
  year={2024},
  url={https://github.com/unrdf/unrdf/tree/main/packages/ml-versioning}
}
```

## Acknowledgments

Built on:
- [TensorFlow.js](https://www.tensorflow.org/js) - Machine learning in JavaScript
- [UNRDF KGC-4D](../kgc-4d) - 4D knowledge graph time-travel
- [Oxigraph](https://github.com/oxigraph/oxigraph) - Fast RDF triple store
- [BLAKE3](https://github.com/BLAKE3-team/BLAKE3) - Cryptographic hash function
