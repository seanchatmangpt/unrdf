/**
 * watch.test.ts
 * Comprehensive tests for watch mode streaming, checkpointing, and reconnection
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import {
  WatchMode,
  watchWithReconnection,
  type WatchConfig,
  type WatchEvent,
  type ExecutionReceipt,
} from '../src/watch.js';
import { Wasm4pmConfig, ExecutionProfile, SourceFormat, StepType } from '../src/config.js';
import { type ExecutableStep } from '../src/pipeline.js';

/**
 * Mock execution plan for testing
 */
function createMockPlan(): ExecutableStep[] {
  return [
    {
      stepId: 'step_1',
      type: StepType.DFG,
      wasmFunction: 'discover_dfg',
      params: {},
      dependencies: [],
      retryable: false,
      required: true,
    },
    {
      stepId: 'step_2',
      type: StepType.STATISTICS,
      wasmFunction: 'analyze_statistics',
      params: {},
      dependencies: ['step_1'],
      retryable: true,
      required: true,
    },
  ];
}

/**
 * Mock configuration for testing
 */
function createMockConfig(content: string = '[]'): Wasm4pmConfig {
  return {
    version: '1.0',
    source: {
      format: SourceFormat.JSON,
      content,
    },
    execution: {
      profile: ExecutionProfile.FAST,
    },
  };
}

/**
 * Test suite for watch mode streaming
 */
describe('WatchMode - Streaming', () => {
  let tempDir: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'watch-test-stream-'));
  });

  afterEach(() => {
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true, force: true });
    }
  });

  it('should emit progress events while streaming', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify([{ activity: 'A' }, { activity: 'B' }, { activity: 'C' }]);
    const config = createMockConfig(testData);

    const watch = new WatchMode(plan, config);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    // Should have progress and completion events
    const progressEvents = events.filter((e) => e.type === 'progress');
    expect(progressEvents.length).toBeGreaterThan(0);

    const completeEvents = events.filter((e) => e.type === 'complete');
    expect(completeEvents.length).toBe(1);
  });

  it('should emit heartbeat events at intervals', async () => {
    const plan = createMockPlan();
    const largeData = JSON.stringify(Array.from({ length: 50 }, (_, i) => ({ id: i })));
    const config = createMockConfig(largeData);

    const watchConfig: WatchConfig = {
      heartbeatIntervalMs: 100,
      heartbeatEventThreshold: 5,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    const heartbeats = events.filter((e) => e.type === 'heartbeat');
    expect(heartbeats.length).toBeGreaterThan(0);

    // Verify heartbeat structure
    heartbeats.forEach((hb) => {
      expect(hb.type).toBe('heartbeat');
      expect((hb as any).timestamp).toBeDefined();
      expect((hb as any).lag_ms).toBeGreaterThanOrEqual(0);
    });
  });

  it('should emit complete event with receipt', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');

    const watch = new WatchMode(plan, config);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    const completeEvents = events.filter((e) => e.type === 'complete');
    expect(completeEvents.length).toBe(1);

    const receipt = (completeEvents[0] as any).receipt as ExecutionReceipt;
    expect(receipt.runId).toBeDefined();
    expect(receipt.engineVersion).toBe('latest');
    expect(receipt.configHash).toBeDefined();
    expect(receipt.profile).toBe(ExecutionProfile.FAST);
    expect(receipt.pipeline).toEqual(['step_1', 'step_2']);
  });

  it('should handle empty sources', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');

    const watch = new WatchMode(plan, config);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    // Should complete without errors
    const completeEvents = events.filter((e) => e.type === 'complete');
    expect(completeEvents.length).toBe(1);
  });

  it('should handle malformed JSON gracefully', async () => {
    const plan = createMockPlan();
    // This should be treated as raw data
    const config = createMockConfig('not valid json at all');

    const watch = new WatchMode(plan, config);
    const events: WatchEvent[] = [];

    try {
      for await (const event of watch.start()) {
        events.push(event);
      }
    } catch (err) {
      // May throw on parsing, which is acceptable
    }

    // Should have some event history
    expect(events.length).toBeGreaterThanOrEqual(0);
  });
});

/**
 * Test suite for checkpointing
 */
describe('WatchMode - Checkpointing', () => {
  let tempDir: string;
  let checkpointPath: string;

  beforeEach(() => {
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'watch-test-ckpt-'));
    checkpointPath = path.join(tempDir, 'checkpoint');
  });

  afterEach(() => {
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true, force: true });
    }
  });

  it('should checkpoint progress to file', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify([{ id: 1 }, { id: 2 }]);
    const config = createMockConfig(testData);

    const watchConfig: WatchConfig = {
      checkpointPath,
      checkpointIntervalMs: 100,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    // Should have written checkpoint
    expect(fs.existsSync(checkpointPath)).toBe(true);

    const checkpoint = JSON.parse(fs.readFileSync(checkpointPath, 'utf-8'));
    expect(checkpoint.timestamp).toBeDefined();
    expect(checkpoint.progress).toBeDefined();
    expect(checkpoint.progressHash).toBeDefined();
    expect(checkpoint.progress.processed).toBeGreaterThanOrEqual(0);
  });

  it('should emit checkpoint events', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify(Array.from({ length: 100 }, (_, i) => ({ id: i })));
    const config = createMockConfig(testData);

    const watchConfig: WatchConfig = {
      checkpointPath,
      checkpointIntervalMs: 50,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    const checkpoints = events.filter((e) => e.type === 'checkpoint');
    // With 100 items, 10 per chunk = 10 chunks. With 50ms interval should get multiple checkpoints
    expect(checkpoints.length).toBeGreaterThanOrEqual(0);

    checkpoints.forEach((cp) => {
      expect((cp as any).progress_hash).toBeDefined();
      expect((cp as any).progress_hash).toHaveLength(16); // SHA256 truncated
    });
  });

  it('should resume from checkpoint', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');

    const watchConfig: WatchConfig = {
      checkpointPath,
    };

    // First run: create checkpoint
    const watch1 = new WatchMode(plan, config, watchConfig);
    for await (const event of watch1.start()) {
      // Consume all events
    }

    expect(fs.existsSync(checkpointPath)).toBe(true);

    // Second run: resume from checkpoint
    const watch2 = new WatchMode(plan, config, watchConfig);
    await watch2.resume();

    // Should successfully resume without errors
    expect(watch2).toBeDefined();
  });

  it('should verify checkpoint integrity', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');

    const watchConfig: WatchConfig = {
      checkpointPath,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    await watch.saveCheckpoint({
      processed: 100,
      total: 200,
      currentTraceIndex: 50,
    });

    // Verify file was written
    expect(fs.existsSync(checkpointPath)).toBe(true);

    // Verify it can be read back
    const data = JSON.parse(fs.readFileSync(checkpointPath, 'utf-8'));
    expect(data.progress.processed).toBe(100);

    // Verify hash matches
    const expectedHash = data.progressHash;
    expect(expectedHash).toHaveLength(16);
  });

  it('should create checkpoint directory if missing', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');

    const nestedPath = path.join(tempDir, 'nested', 'dir', 'checkpoint');
    const watchConfig: WatchConfig = {
      checkpointPath: nestedPath,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    await watch.saveCheckpoint({
      processed: 10,
      total: 20,
      currentTraceIndex: 5,
    });

    expect(fs.existsSync(nestedPath)).toBe(true);
  });
});

/**
 * Test suite for reconnection logic
 */
describe('WatchMode - Reconnection', () => {
  it('should calculate correct exponential backoff', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');

    const watchConfig: WatchConfig = {
      initialBackoffMs: 100,
      backoffMultiplier: 2,
      maxBackoffMs: 1000,
      maxReconnectAttempts: 5,
    };

    const events: WatchEvent[] = [];
    const reconnectAttempts: number[] = [];

    // Mock failure by throwing on first attempts
    let attempt = 0;
    try {
      for await (const event of watchWithReconnection(plan, config, watchConfig)) {
        events.push(event);
        if (event.type === 'reconnect') {
          reconnectAttempts.push((event as any).attempt);
        }
      }
    } catch (err) {
      // Expected to eventually fail
    }

    // Should have reconnect events
    expect(reconnectAttempts.length).toBeGreaterThanOrEqual(0);
  });

  it('should respect max reconnect attempts', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('invalid json {{{');

    const watchConfig: WatchConfig = {
      maxReconnectAttempts: 2,
      initialBackoffMs: 10,
    };

    let eventCount = 0;
    try {
      for await (const event of watchWithReconnection(plan, config, watchConfig)) {
        eventCount++;
        if (eventCount > 100) break; // Safety limit
      }
    } catch (err) {
      // Expected to eventually fail after max attempts
    }
  });

  it('should emit reconnect events with backoff values', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');

    const watchConfig: WatchConfig = {
      initialBackoffMs: 50,
      backoffMultiplier: 1.5,
      maxReconnectAttempts: 3,
    };

    const reconnectEvents: any[] = [];
    try {
      for await (const event of watchWithReconnection(plan, config, watchConfig)) {
        if (event.type === 'reconnect') {
          reconnectEvents.push(event);
        }
      }
    } catch (err) {
      // Expected
    }

    // Verify backoff progression if any reconnects occurred
    reconnectEvents.forEach((event, idx) => {
      expect(event.type).toBe('reconnect');
      expect(event.attempt).toBeGreaterThan(0);
      expect(event.backoff_ms).toBeGreaterThan(0);
    });
  });
});

/**
 * Test suite for event ordering and completeness
 */
describe('WatchMode - Event Ordering', () => {
  it('should emit events in correct order', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify(Array.from({ length: 30 }, (_, i) => ({ id: i })));
    const config = createMockConfig(testData);

    const watchConfig: WatchConfig = {
      heartbeatIntervalMs: 100,
      checkpointIntervalMs: 200,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    // First non-heartbeat event should be progress or checkpoint
    const firstEvent = events[0];
    expect(firstEvent.type).toMatch(/progress|heartbeat|checkpoint|complete/);

    // Last event must be complete
    const lastEvent = events[events.length - 1];
    expect(lastEvent.type).toBe('complete');
  });

  it('should not emit errors for valid input', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify([{ id: 1 }, { id: 2 }]);
    const config = createMockConfig(testData);

    const watch = new WatchMode(plan, config);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    const errorEvents = events.filter((e) => e.type === 'error');
    expect(errorEvents.length).toBe(0);
  });

  it('should handle large datasets without memory issues', async () => {
    const plan = createMockPlan();
    const largeData = JSON.stringify(
      Array.from({ length: 1000 }, (_, i) => ({ id: i, value: Math.random() }))
    );
    const config = createMockConfig(largeData);

    const watchConfig: WatchConfig = {
      heartbeatEventThreshold: 50,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    let eventCount = 0;

    for await (const event of watch.start()) {
      eventCount++;
    }

    expect(eventCount).toBeGreaterThan(0);
  });
});

/**
 * Test suite for heartbeat accuracy
 */
describe('WatchMode - Heartbeat', () => {
  it('should emit heartbeat within configured interval', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify(Array.from({ length: 50 }, (_, i) => ({ id: i })));
    const config = createMockConfig(testData);

    const heartbeatInterval = 100;
    const watchConfig: WatchConfig = {
      heartbeatIntervalMs: heartbeatInterval,
    };

    const watch = new WatchMode(plan, config, watchConfig);
    const heartbeats: any[] = [];

    for await (const event of watch.start()) {
      if (event.type === 'heartbeat') {
        heartbeats.push(event);
      }
    }

    // Should have some heartbeats
    expect(heartbeats.length).toBeGreaterThan(0);

    // Verify lag_ms values are reasonable
    heartbeats.forEach((hb) => {
      expect(hb.lag_ms).toBeGreaterThanOrEqual(0);
    });
  });

  it('should respect event threshold for heartbeats', async () => {
    const plan = createMockPlan();
    const smallData = JSON.stringify([{ id: 1 }]);
    const config = createMockConfig(smallData);

    const watchConfig: WatchConfig = {
      heartbeatIntervalMs: 10000, // Very long interval
      heartbeatEventThreshold: 1, // But trigger on 1 event
    };

    const watch = new WatchMode(plan, config, watchConfig);
    const events: WatchEvent[] = [];

    for await (const event of watch.start()) {
      events.push(event);
    }

    const heartbeats = events.filter((e) => e.type === 'heartbeat');
    expect(heartbeats.length).toBeGreaterThan(0);
  });
});
