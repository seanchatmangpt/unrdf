/**
 * @file Streaming Performance Benchmarks
 * @module benchmarks/integration/streaming-benchmark
 *
 * @description
 * Benchmarks for event streaming operations:
 * - Stream throughput (events/sec)
 * - Backpressure handling
 * - Memory under load
 * - Latency percentiles
 */

import { suite, randomString, randomInt } from '../framework.mjs';
import { EventEmitter } from 'node:events';

// =============================================================================
// Simple Stream Implementation (Mock)
// =============================================================================

class SimpleEventStream extends EventEmitter {
  constructor() {
    super();
    this.buffer = [];
    this.maxBufferSize = 1000;
    this.paused = false;
  }

  push(event) {
    if (this.buffer.length >= this.maxBufferSize) {
      this.paused = true;
      return false;
    }

    this.buffer.push(event);
    this.emit('data', event);
    return true;
  }

  consume() {
    if (this.buffer.length === 0) return null;
    const event = this.buffer.shift();

    if (this.paused && this.buffer.length < this.maxBufferSize * 0.5) {
      this.paused = false;
      this.emit('drain');
    }

    return event;
  }

  size() {
    return this.buffer.length;
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Generate sample event
 * @returns {object} Event object
 */
function generateEvent() {
  return {
    id: randomString(16),
    type: 'workflow.task.completed',
    timestamp: Date.now(),
    data: {
      workflowId: randomString(8),
      taskId: randomString(8),
      status: 'completed',
      result: randomString(32)
    }
  };
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const streamingBenchmarks = suite('Streaming Performance', {
  'create stream': {
    fn: () => {
      return new SimpleEventStream();
    },
    iterations: 10000,
    warmup: 1000
  },

  'push single event': {
    setup: () => {
      const stream = new SimpleEventStream();
      return { stream };
    },
    fn: function() {
      const event = generateEvent();
      return this.stream.push(event);
    },
    iterations: 100000,
    warmup: 10000
  },

  'push and consume event': {
    setup: () => {
      const stream = new SimpleEventStream();
      return { stream };
    },
    fn: function() {
      const event = generateEvent();
      this.stream.push(event);
      return this.stream.consume();
    },
    iterations: 50000,
    warmup: 5000
  },

  'batch push (100 events)': {
    setup: () => {
      const stream = new SimpleEventStream();
      return { stream };
    },
    fn: function() {
      for (let i = 0; i < 100; i++) {
        this.stream.push(generateEvent());
      }
    },
    iterations: 1000,
    warmup: 100
  },

  'batch consume (100 events)': {
    setup: () => {
      const stream = new SimpleEventStream();
      // Pre-fill stream
      for (let i = 0; i < 100; i++) {
        stream.push(generateEvent());
      }
      return { stream };
    },
    fn: function() {
      const events = [];
      for (let i = 0; i < 100; i++) {
        const event = this.stream.consume();
        if (event) events.push(event);
      }
      return events;
    },
    iterations: 1000,
    warmup: 100
  },

  'throughput test (1000 events)': {
    setup: () => {
      const stream = new SimpleEventStream();
      return { stream };
    },
    fn: function() {
      let pushed = 0;
      let consumed = 0;

      // Push events
      for (let i = 0; i < 1000; i++) {
        if (this.stream.push(generateEvent())) {
          pushed++;
        }
      }

      // Consume events
      while (this.stream.size() > 0) {
        if (this.stream.consume()) {
          consumed++;
        }
      }

      return { pushed, consumed };
    },
    iterations: 500,
    warmup: 50
  },

  'backpressure handling': {
    setup: () => {
      const stream = new SimpleEventStream();
      // Fill to capacity
      for (let i = 0; i < 1000; i++) {
        stream.push(generateEvent());
      }
      return { stream };
    },
    fn: function() {
      // Try to push more (should trigger backpressure)
      const canPush = this.stream.push(generateEvent());

      // Consume some to release pressure
      if (!canPush) {
        for (let i = 0; i < 500; i++) {
          this.stream.consume();
        }
      }

      return canPush;
    },
    iterations: 5000,
    warmup: 500
  },

  'event listener overhead': {
    setup: () => {
      const stream = new SimpleEventStream();
      let count = 0;
      stream.on('data', () => { count++; });
      return { stream };
    },
    fn: function() {
      return this.stream.push(generateEvent());
    },
    iterations: 50000,
    warmup: 5000
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await streamingBenchmarks();
  const { formatDetailedReport } = await import('../framework.mjs');
  console.log('\n' + formatDetailedReport(result));
  process.exit(0);
}
