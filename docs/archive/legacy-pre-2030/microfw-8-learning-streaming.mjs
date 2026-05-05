#!/usr/bin/env node

/**
 * @fileoverview Learning-Streaming - Knowledge-engine + Streaming
 * @module @unrdf/microfw-8-learning-streaming
 *
 * Adversarial Innovation: AI pattern learning + real-time streams = adaptive streaming
 * Use Case: Streams that learn optimal routing and transformation patterns
 */

// ============================================================================
// MOCK IMPLEMENTATIONS
// ============================================================================

// Knowledge engine
class KnowledgeEngine {
  constructor() {
    this.patterns = [];
  }

  async extractPatterns(streamData) {
    const pattern = {
      source: streamData.source,
      avgThroughput: streamData.itemsProcessed / (streamData.duration / 1000),
      confidence: Math.random() * 0.3 + 0.7,
    };
    this.patterns.push(pattern);
    return [pattern];
  }

  async optimizeStream(streamName) {
    const relevantPatterns = this.patterns.filter(p => p.source === streamName);

    if (relevantPatterns.length === 0) {
      return { bufferSize: 10 };
    }

    // Optimize buffer size based on throughput
    const avgThroughput = relevantPatterns.reduce((sum, p) => sum + p.avgThroughput, 0) / relevantPatterns.length;

    return {
      bufferSize: Math.ceil(avgThroughput / 10),
      batchSize: Math.ceil(avgThroughput / 5),
    };
  }
}

// Streaming
class ChangeStream {
  constructor(name) {
    this.name = name;
    this.listeners = [];
    this.buffer = [];
    this.bufferSize = 10;
  }

  emit(event, data) {
    this.listeners
      .filter(l => l.event === event)
      .forEach(l => l.handler(data));
  }

  on(event, handler) {
    this.listeners.push({ event, handler });
  }

  push(item) {
    this.buffer.push(item);

    if (this.buffer.length >= this.bufferSize) {
      this.flush();
    }
  }

  flush() {
    if (this.buffer.length === 0) return;

    this.emit('batch', {
      items: [...this.buffer],
      count: this.buffer.length,
    });

    this.buffer = [];
  }

  setBufferSize(size) {
    this.bufferSize = size;
  }
}

function createChangeStream(name) {
  return new ChangeStream(name);
}

// ============================================================================
// LEARNING-STREAMING FRAMEWORK
// ============================================================================

/**
 * LearningStreamingFramework - Adaptive streaming with ML optimization
 */
class LearningStreamingFramework {
  constructor() {
    this.knowledge = new KnowledgeEngine();
    this.streams = new Map();
    this.streamStats = new Map();
    this.stats = {
      streamsCreated: 0,
      itemsProcessed: 0,
      optimizationsApplied: 0,
      patternsLearned: 0,
    };
  }

  /**
   * Create adaptive stream
   */
  createStream(name) {
    const stream = createChangeStream(name);

    // Setup learning-enabled batch processing
    stream.on('batch', async (batch) => {
      const startTime = Date.now();

      // Process batch
      console.log(`[Stream] ${name}: Processing batch of ${batch.count} items`);

      // Update stats
      const stats = this.streamStats.get(name) || {
        batches: 0,
        itemsProcessed: 0,
        totalDuration: 0,
      };

      stats.batches++;
      stats.itemsProcessed += batch.count;
      stats.totalDuration += Date.now() - startTime;

      this.streamStats.set(name, stats);
      this.stats.itemsProcessed += batch.count;

      // Learn from this batch
      const patterns = await this.knowledge.extractPatterns({
        source: name,
        itemsProcessed: batch.count,
        duration: Date.now() - startTime,
      });

      this.stats.patternsLearned += patterns.length;

      // Apply optimizations
      await this.optimizeStream(name);
    });

    this.streams.set(name, stream);
    this.stats.streamsCreated++;

    console.log(`[Stream] Created: ${name}`);

    return stream;
  }

  /**
   * Optimize stream based on learned patterns
   */
  async optimizeStream(name) {
    const stream = this.streams.get(name);
    if (!stream) return;

    const optimization = await this.knowledge.optimizeStream(name);

    // Apply optimization
    if (optimization.bufferSize) {
      stream.setBufferSize(optimization.bufferSize);
      this.stats.optimizationsApplied++;
      console.log(`  [Optimize] ${name}: buffer size = ${optimization.bufferSize}`);
    }

    return optimization;
  }

  /**
   * Push items to stream
   */
  async pushToStream(streamName, items) {
    const stream = this.streams.get(streamName);
    if (!stream) {
      throw new Error(`Stream not found: ${streamName}`);
    }

    for (const item of items) {
      stream.push(item);
    }

    // Flush remaining
    stream.flush();
  }

  /**
   * Get stream statistics
   */
  getStreamStats(streamName) {
    return this.streamStats.get(streamName);
  }

  /**
   * Get learned patterns
   */
  getLearnedPatterns() {
    return this.knowledge.patterns.sort((a, b) => b.confidence - a.confidence);
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      ...this.stats,
      streams: this.streams.size,
      patterns: this.knowledge.patterns.length,
    };
  }
}

// ============================================================================
// DEMO
// ============================================================================

async function demo() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║ Learning-Streaming Framework Demo                         ║');
  console.log('║ Knowledge-engine + Streaming = Adaptive streams            ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  const framework = new LearningStreamingFramework();

  // Create streams
  const dataStream = framework.createStream('data-pipeline');
  const eventStream = framework.createStream('event-processor');

  console.log('\n[Demo] Processing data with adaptive streaming...\n');

  // Push data to streams
  console.log('[Batch 1] Pushing 15 items to data-pipeline');
  await framework.pushToStream('data-pipeline', Array.from({ length: 15 }, (_, i) => ({ id: i })));

  console.log('\n[Batch 2] Pushing 25 items to data-pipeline');
  await framework.pushToStream('data-pipeline', Array.from({ length: 25 }, (_, i) => ({ id: i + 15 })));

  console.log('\n[Batch 3] Pushing 30 items to event-processor');
  await framework.pushToStream('event-processor', Array.from({ length: 30 }, (_, i) => ({ event: i })));

  console.log('\n[Demo] Stream statistics:\n');
  const dataPipelineStats = framework.getStreamStats('data-pipeline');
  console.log('  data-pipeline:', JSON.stringify(dataPipelineStats, null, 2));

  const eventProcessorStats = framework.getStreamStats('event-processor');
  console.log('  event-processor:', JSON.stringify(eventProcessorStats, null, 2));

  console.log('\n[Demo] Learned patterns:\n');
  const patterns = framework.getLearnedPatterns().slice(0, 5);
  patterns.forEach((p, i) => {
    console.log(`  ${i + 1}. ${p.source}: ${p.avgThroughput.toFixed(2)} items/sec (confidence: ${p.confidence.toFixed(3)})`);
  });

  console.log('\n[Stats] Final Statistics:');
  const stats = framework.getStats();
  Object.entries(stats).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });

  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║ Adversarial Innovation:                                    ║');
  console.log('║ - Knowledge engine learns stream throughput patterns       ║');
  console.log('║ - Streaming provides real-time data flow                   ║');
  console.log('║ - Streams auto-optimize buffer sizes based on learning     ║');
  console.log('║ - ML + streaming = adaptive data pipelines                 ║');
  console.log('╚════════════════════════════════════════════════════════════╝');
}

if (import.meta.url === `file://${process.argv[1]}`) {
  demo().catch(console.error);
}

export { LearningStreamingFramework, demo };
