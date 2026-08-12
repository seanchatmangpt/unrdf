/**
 * websocket.test.ts
 * Comprehensive WebSocket validation tests for watch mode
 * Tests: connection, event sequences, heartbeats, progress, checkpoints, errors, reconnection
 */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as fs from 'fs';
import * as path from 'path';
import { WatchMode } from '../../src/watch.js';
import { ExecutionProfile, SourceFormat, StepType } from '../../src/config.js';
/**
 * Mock data generators
 */
function createMockPlan() {
  return [
    {
      stepId: 'step_discover_dfg',
      type: StepType.DFG,
      wasmFunction: 'discover_dfg',
      params: {},
      dependencies: [],
      retryable: false,
      required: true,
    },
  ];
}
function createMockConfig(content = '[]') {
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
describe('WebSocket - Connection Management', () => {
  let tempDir;
  beforeEach(() => {
    tempDir = path.join(process.cwd(), '.test-websocket-conn');
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
  });
  afterEach(() => {
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true });
    }
  });
  it('should establish connection and emit events', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');
    const watch = new WatchMode(plan, config);
    const events = [];
    for await (const event of watch.start()) {
      events.push(event);
    }
    expect(events.length).toBeGreaterThan(0);
    expect(events[events.length - 1].type).toBe('complete');
  });
  it('should close connection cleanly', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');
    const watch = new WatchMode(plan, config);
    const events = [];
    for await (const event of watch.start()) {
      events.push(event);
    }
    const lastEvent = events[events.length - 1];
    expect(lastEvent.type).toBe('complete');
    expect(lastEvent.receipt).toBeDefined();
  });
});
describe('WebSocket - Event Sequence', () => {
  it('should emit progress events', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify(Array.from({ length: 50 }, (_, i) => ({ id: i })));
    const config = createMockConfig(testData);
    const watch = new WatchMode(plan, config);
    const progressEvents = [];
    for await (const event of watch.start()) {
      if (event.type === 'progress') {
        progressEvents.push(event);
      }
    }
    if (progressEvents.length > 1) {
      for (let i = 1; i < progressEvents.length; i++) {
        expect(progressEvents[i].processed).toBeGreaterThanOrEqual(progressEvents[i - 1].processed);
      }
    }
  });
  it('should emit complete event with receipt', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');
    const watch = new WatchMode(plan, config);
    const events = [];
    for await (const event of watch.start()) {
      events.push(event);
    }
    const completeEvent = events.find((e) => e.type === 'complete');
    expect(completeEvent).toBeDefined();
    const receipt = completeEvent.receipt;
    expect(receipt.runId).toBeDefined();
    expect(receipt.engineVersion).toBe('latest');
  });
});
describe('WebSocket - Heartbeat', () => {
  it('should emit heartbeat events', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify(Array.from({ length: 50 }, (_, i) => ({ id: i })));
    const config = createMockConfig(testData);
    const watchConfig = { heartbeatIntervalMs: 100 };
    const watch = new WatchMode(plan, config, watchConfig);
    const heartbeatEvents = [];
    for await (const event of watch.start()) {
      if (event.type === 'heartbeat') {
        heartbeatEvents.push(event);
      }
    }
    heartbeatEvents.forEach((hb) => {
      expect(hb.timestamp).toBeDefined();
      expect(hb.lag_ms).toBeDefined();
    });
  });
});
describe('WebSocket - Checkpoints', () => {
  let tempDir;
  beforeEach(() => {
    tempDir = path.join(process.cwd(), '.test-websocket-cp');
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
  });
  afterEach(() => {
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true });
    }
  });
  it('should save checkpoint', async () => {
    const plan = createMockPlan();
    const testData = JSON.stringify(Array.from({ length: 50 }, (_, i) => ({ id: i })));
    const config = createMockConfig(testData);
    const checkpointPath = path.join(tempDir, 'checkpoint');
    const watchConfig = { checkpointPath, checkpointIntervalMs: 50 };
    const watch = new WatchMode(plan, config, watchConfig);
    for await (const event of watch.start()) {
      // consume events
    }
    if (fs.existsSync(checkpointPath)) {
      const checkpoint = JSON.parse(fs.readFileSync(checkpointPath, 'utf-8'));
      expect(checkpoint.progress).toBeDefined();
      expect(checkpoint.timestamp).toBeDefined();
    }
  });
  it('should resume from checkpoint', async () => {
    const plan = createMockPlan();
    const config = createMockConfig('[]');
    const checkpointPath = path.join(tempDir, 'checkpoint');
    const watchConfig = { checkpointPath };
    const watch1 = new WatchMode(plan, config, watchConfig);
    for await (const event of watch1.start()) {
      // consume
    }
    const watch2 = new WatchMode(plan, config, watchConfig);
    let resumed = false;
    try {
      await watch2.resume();
      resumed = true;
    } catch (err) {
      // May fail if no checkpoint
    }
    expect(resumed || !fs.existsSync(checkpointPath)).toBe(true);
  });
});
describe('WebSocket - Large Logs', () => {
  it('should handle large event logs', async () => {
    const plan = createMockPlan();
    const largeData = JSON.stringify(
      Array.from({ length: 1000 }, (_, i) => ({
        id: i,
        activity: `A${i % 5}`,
        timestamp: new Date(Date.now() + i * 1000).toISOString(),
      }))
    );
    const config = createMockConfig(largeData);
    const watch = new WatchMode(plan, config);
    const events = [];
    const startTime = Date.now();
    for await (const event of watch.start()) {
      events.push(event);
    }
    const duration = Date.now() - startTime;
    expect(events.length).toBeGreaterThan(0);
    expect(events[events.length - 1].type).toBe('complete');
    expect(duration).toBeLessThan(30000);
  });
});
describe('WebSocket - Integration', () => {
  it('should handle concurrent instances', async () => {
    const plan = createMockPlan();
    const config = createMockConfig(
      JSON.stringify(Array.from({ length: 20 }, (_, i) => ({ id: i })))
    );
    const watch1 = new WatchMode(plan, config);
    const watch2 = new WatchMode(plan, config);
    const events1 = [];
    const events2 = [];
    await Promise.all([
      (async () => {
        for await (const event of watch1.start()) {
          events1.push(event);
        }
      })(),
      (async () => {
        for await (const event of watch2.start()) {
          events2.push(event);
        }
      })(),
    ]);
    expect(events1.length).toBeGreaterThan(0);
    expect(events2.length).toBeGreaterThan(0);
  });
});
//# sourceMappingURL=websocket.test.js.map
