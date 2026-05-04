# @unrdf/ml-inference

High-performance machine learning inference pipeline for RDF streams using ONNX Runtime.

## Features

- **ONNX Runtime Integration**: Native ONNX model execution with GPU support
- **Streaming Inference**: Process RDF change feeds with automatic batching
- **Model Registry**: Version management, A/B testing, and hot-swapping
- **Performance Optimized**: Batching, backpressure handling, and metrics tracking
- **Production Ready**: OpenTelemetry instrumentation and error handling

## Installation

```bash
pnpm add @unrdf/ml-inference
```

## Quick Start

### Basic Inference

```javascript
import { createONNXRunner } from '@unrdf/ml-inference';

const runner = createONNXRunner({
  executionProviders: ['cpu'], // or ['cuda', 'cpu'] for GPU
});

await runner.loadModel('./models/classifier.onnx');

const result = await runner.infer({
  input: new Float32Array([1.0, 2.0, 3.0])
});
```

### Streaming Pipeline

```javascript
import { createONNXRunner, createStreamingInferencePipeline } from '@unrdf/ml-inference';

const runner = createONNXRunner();
await runner.loadModel('./model.onnx');

const pipeline = createStreamingInferencePipeline(runner, {
  batchSize: 32,
  batchTimeoutMs: 100,
  enableBackpressure: true
});

// Subscribe to results
pipeline.subscribe((batch) => {
  console.log('Batch results:', batch);
});

// Stream data
for (const item of dataStream) {
  await pipeline.process({
    id: item.id,
    features: item.data
  });
}
```

### Model Registry

```javascript
import { createModelRegistry } from '@unrdf/ml-inference';

const registry = createModelRegistry();

// Register models
await registry.register('v1.0', './model-v1.onnx', {
  name: 'classifier',
  accuracy: 0.85
});

await registry.register('v2.0', './model-v2.onnx', {
  name: 'classifier',
  accuracy: 0.92
});

// Canary deployment (10% traffic to v2.0)
await registry.deploy('v2.0', 'canary', 10);

// Promote if successful
registry.promoteCanary();
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                 ML Inference Pipeline                   │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────────┐    ┌──────────────┐   ┌───────────┐ │
│  │ RDF Stream   │───▶│  Batching    │──▶│  ONNX     │ │
│  │ Input        │    │  Pipeline    │   │  Runtime  │ │
│  └──────────────┘    └──────────────┘   └───────────┘ │
│         │                   │                  │       │
│         ▼                   ▼                  ▼       │
│  ┌──────────────────────────────────────────────────┐ │
│  │           Model Registry                         │ │
│  │  - Version Management                            │ │
│  │  - A/B Testing                                   │ │
│  │  - Hot-swapping                                  │ │
│  └──────────────────────────────────────────────────┘ │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## Performance

Typical throughput on commodity hardware:

- **Single inference**: 2-5ms latency
- **Batch inference (32)**: 500-2000 inferences/sec
- **Streaming pipeline**: 1000-5000 items/sec (batch size 32)
- **GPU acceleration**: 5-10x improvement with CUDA

## API Reference

### ONNXRunner

#### `createONNXRunner(options)`

Create ONNX inference runner.

**Options:**
- `executionProviders`: Array of providers (e.g., `['cuda', 'cpu']`)
- `graphOptimizationLevel`: 'disabled' | 'basic' | 'extended' | 'all'
- `enableCpuMemArena`: Boolean (default: true)

#### `runner.loadModel(pathOrBuffer)`

Load ONNX model from file or buffer.

#### `runner.infer(inputs, batchSize?)`

Run inference on input tensors.

#### `runner.inferBatch(batchInputs)`

Run batched inference for multiple samples.

#### `runner.getMetrics()`

Get performance metrics (throughput, latency).

### StreamingInferencePipeline

#### `createStreamingInferencePipeline(runner, options)`

Create streaming inference pipeline.

**Options:**
- `batchSize`: Number of items per batch (default: 32)
- `batchTimeoutMs`: Max wait time for partial batch (default: 100)
- `maxQueueSize`: Maximum queue size (default: 1000)
- `enableBackpressure`: Enable backpressure handling (default: true)

#### `pipeline.process(item)`

Process single item through pipeline.

#### `pipeline.subscribe(callback)`

Subscribe to inference results.

#### `pipeline.flush()`

Flush remaining buffer.

### ModelRegistry

#### `createModelRegistry()`

Create model version registry.

#### `registry.register(version, modelPath, metadata, options?)`

Register new model version.

#### `registry.deploy(version, strategy?, canaryPercent?)`

Deploy model with strategy ('immediate', 'blue-green', 'canary').

#### `registry.getRunner()`

Get runner for inference (handles A/B routing).

## Examples

Run the demo:

```bash
cd packages/ml-inference
pnpm demo
```

## Testing

```bash
pnpm test
pnpm test:coverage
```

## Performance Tuning

### Batch Size

Larger batches = higher throughput, higher latency:

```javascript
// Low latency (real-time)
{ batchSize: 1, batchTimeoutMs: 10 }

// Balanced
{ batchSize: 32, batchTimeoutMs: 100 }

// High throughput
{ batchSize: 128, batchTimeoutMs: 500 }
```

### Execution Providers

```javascript
// CPU only
{ executionProviders: ['cpu'] }

// GPU with CPU fallback
{ executionProviders: ['cuda', 'cpu'] }

// TensorRT (NVIDIA)
{ executionProviders: ['tensorrt', 'cuda', 'cpu'] }
```

## License

MIT
