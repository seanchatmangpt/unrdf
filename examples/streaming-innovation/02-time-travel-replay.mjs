/**
 * @file Time-Travel Stream Replay Example
 * @description Demonstrates replaying historical KGC-4D deltas as a real-time stream
 *
 * This example shows how to:
 * 1. Replay historical deltas from KGC-4D storage
 * 2. Control playback speed (1x, 10x, 100x, instant)
 * 3. Filter and transform events during replay
 * 4. Implement temporal queries
 *
 * Expected output: Historical events replayed with timing controls
 */

import { createChangeFeed } from '../../packages/streaming/src/index.mjs';
import { EventEmitter } from 'events';

// ============================================================================
// Time-Travel Stream Engine (from research document)
// ============================================================================

class TimeTravelStream extends EventEmitter {
  constructor(kgc4dStore, config = {}) {
    super();
    this.store = kgc4dStore;
    this.playbackSpeed = config.playbackSpeed || 1.0;
    this.isPlaying = false;
    this.currentPosition = null;
    this.changeFeed = createChangeFeed();
    this.stats = {
      deltasReplayed: 0,
      startTime: null,
      endTime: null,
    };
  }

  async replay(fromTimestamp, toTimestamp = null, options = {}) {
    if (this.isPlaying) {
      throw new Error('Replay already in progress');
    }

    this.isPlaying = true;
    this.currentPosition = fromTimestamp;
    this.stats.startTime = Date.now();
    this.stats.deltasReplayed = 0;

    const filter = options.filter || (() => true);
    const transform = options.transform || (d => d);

    try {
      const deltas = await this.store.getDeltas({
        from: fromTimestamp,
        to: toTimestamp,
        limit: options.limit || 10000,
      });

      this.emit('replay:started', {
        from: fromTimestamp,
        to: toTimestamp,
        deltaCount: deltas.length,
      });

      for (let i = 0; i < deltas.length && this.isPlaying; i++) {
        const delta = deltas[i];
        const nextDelta = deltas[i + 1];

        if (!filter(delta)) {
          continue;
        }

        const transformed = transform(delta);

        this.changeFeed.emitChange({
          type: this._deltaToChangeType(delta),
          quad: this._deltaToQuad(delta),
          timestamp: delta.timestamp_ns,
          metadata: {
            deltaId: delta.id,
            isReplay: true,
            replayPosition: i,
            replayTotal: deltas.length,
          },
        });

        this.emit('replay:delta', {
          delta: transformed,
          position: i,
          total: deltas.length,
        });

        this.stats.deltasReplayed++;

        // Calculate delay based on playback speed
        if (nextDelta && this.playbackSpeed > 0 && this.playbackSpeed < Infinity) {
          const realDelay = Number(nextDelta.timestamp_ns - delta.timestamp_ns) / 1_000_000;
          const scaledDelay = realDelay / this.playbackSpeed;

          if (scaledDelay > 0) {
            await this._delay(scaledDelay);
          }
        }

        this.currentPosition = delta.timestamp_ns;
      }

      this.stats.endTime = Date.now();

      this.emit('replay:completed', {
        deltasProcessed: this.stats.deltasReplayed,
        duration: this.stats.endTime - this.stats.startTime,
      });
    } catch (error) {
      this.emit('replay:error', error);
      throw error;
    } finally {
      this.isPlaying = false;
    }
  }

  pause() {
    this.isPlaying = false;
    this.emit('replay:paused', { position: this.currentPosition });
  }

  resume() {
    if (!this.currentPosition) {
      throw new Error('No replay in progress');
    }
    return this.replay(this.currentPosition);
  }

  setSpeed(speed) {
    this.playbackSpeed = speed;
    this.emit('speed:changed', { speed });
  }

  subscribe(callback) {
    return this.changeFeed.subscribe(callback);
  }

  _deltaToChangeType(delta) {
    const hasInserts = delta.operations?.some(op => op.op === 'insert' || op.op === 'set');
    const hasDeletes = delta.operations?.some(op => op.op === 'delete');

    if (hasInserts && hasDeletes) return 'update';
    if (hasInserts) return 'add';
    if (hasDeletes) return 'remove';
    return 'update';
  }

  _deltaToQuad(delta) {
    return {
      subject: { value: delta.source?.actor || 'unknown' },
      predicate: { value: 'http://kgc.io/applied' },
      object: { value: delta.id },
      graph: { value: 'http://kgc.io/history' },
    };
  }

  _delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  getStatus() {
    return {
      isPlaying: this.isPlaying,
      currentPosition: this.currentPosition,
      playbackSpeed: this.playbackSpeed,
      stats: this.stats,
    };
  }
}

// ============================================================================
// Temporal Query Builder
// ============================================================================

class TemporalQueryBuilder {
  constructor(timeTravelStream) {
    this.stream = timeTravelStream;
    this.filters = [];
  }

  between(start, end) {
    this.filters.push((delta) => {
      const ts = delta.timestamp_ns;
      return ts >= start && ts <= end;
    });
    return this;
  }

  operationType(type) {
    this.filters.push((delta) =>
      delta.operations?.some(op => op.op === type)
    );
    return this;
  }

  sourcePackage(packageName) {
    this.filters.push((delta) =>
      delta.source?.package === packageName
    );
    return this;
  }

  async execute(fromTimestamp, toTimestamp) {
    const combinedFilter = (delta) =>
      this.filters.every(f => f(delta));

    return this.stream.replay(fromTimestamp, toTimestamp, {
      filter: combinedFilter,
    });
  }
}

// ============================================================================
// Mock KGC-4D Store (for demonstration)
// ============================================================================

class MockKGC4DStore {
  constructor() {
    this.deltas = this._generateMockDeltas();
  }

  async getDeltas(options) {
    const { from, to, limit = 10000 } = options;

    let filtered = this.deltas.filter(d => {
      if (from && d.timestamp_ns < from) return false;
      if (to && d.timestamp_ns > to) return false;
      return true;
    });

    return filtered.slice(0, limit);
  }

  _generateMockDeltas() {
    const deltas = [];
    const baseTime = BigInt(Date.now()) * 1_000_000n;
    const packages = ['@unrdf/yawl', '@unrdf/streaming', '@unrdf/daemon'];
    const operations = ['set', 'delete', 'insert'];

    for (let i = 0; i < 100; i++) {
      deltas.push({
        id: `delta-${i}`,
        timestamp_ns: baseTime + BigInt(i * 1000000000), // 1 second apart
        operations: [
          {
            op: operations[i % operations.length],
            path: `state.item${i}`,
            newValue: `value-${i}`,
          },
        ],
        source: {
          package: packages[i % packages.length],
          actor: `user-${i % 5}`,
        },
      });
    }

    return deltas;
  }
}

// ============================================================================
// Example Usage
// ============================================================================

async function main() {
  console.log('='.repeat(70));
  console.log('Time-Travel Stream Replay Example');
  console.log('='.repeat(70));
  console.log('');

  // Create mock KGC-4D store with historical deltas
  const store = new MockKGC4DStore();
  console.log(`Mock store created with ${store.deltas.length} historical deltas\n`);

  // Create time-travel stream
  const timeTravel = new TimeTravelStream(store, {
    playbackSpeed: 10.0, // 10x speed
  });

  // Listen for replay events
  timeTravel.on('replay:started', (info) => {
    console.log(`🎬 Replay started:`);
    console.log(`   From: ${info.from}`);
    console.log(`   To: ${info.to || 'end'}`);
    console.log(`   Deltas: ${info.deltaCount}`);
    console.log('');
  });

  let deltaCount = 0;
  timeTravel.on('replay:delta', (info) => {
    deltaCount++;
    const percent = Math.floor((info.position / info.total) * 100);
    if (deltaCount % 10 === 0) {
      console.log(`   ⏩ Progress: ${percent}% (${info.position}/${info.total})`);
    }
  });

  timeTravel.on('replay:completed', (info) => {
    console.log(`\n✅ Replay completed:`);
    console.log(`   Deltas processed: ${info.deltasProcessed}`);
    console.log(`   Duration: ${info.duration}ms`);
    console.log(`   Throughput: ${Math.floor(info.deltasProcessed / (info.duration / 1000))} deltas/sec`);
  });

  // Subscribe to replayed events
  let eventCount = 0;
  timeTravel.subscribe((change) => {
    eventCount++;
  });

  // Example 1: Replay all events at 10x speed
  console.log('Example 1: Replay all events at 10x speed\n');

  const baseTime = BigInt(Date.now()) * 1_000_000n;
  await timeTravel.replay(baseTime, null, { limit: 50 });

  console.log(`\nReceived ${eventCount} change events via subscription\n`);

  // Example 2: Temporal query - filter by package and operation type
  console.log('='.repeat(70));
  console.log('Example 2: Temporal Query - YAWL package, SET operations only\n');

  const query = new TemporalQueryBuilder(timeTravel)
    .sourcePackage('@unrdf/yawl')
    .operationType('set');

  eventCount = 0;
  await query.execute(baseTime, baseTime + BigInt(50) * 1_000_000_000n);

  console.log(`\nFiltered query received ${eventCount} events\n`);

  // Example 3: Variable speed playback
  console.log('='.repeat(70));
  console.log('Example 3: Variable speed playback\n');

  console.log('Starting at 1x speed...');
  timeTravel.setSpeed(1.0);

  const replayPromise = timeTravel.replay(baseTime, baseTime + BigInt(10) * 1_000_000_000n);

  // Change speed after 2 seconds
  setTimeout(() => {
    console.log('Increasing to 100x speed...');
    timeTravel.setSpeed(100.0);
  }, 2000);

  await replayPromise;

  // Display final status
  console.log('\n' + '='.repeat(70));
  console.log('Final Status:');
  console.log('='.repeat(70));
  const status = timeTravel.getStatus();
  console.log(JSON.stringify(status, null, 2));

  console.log('\nExample complete!');
}

// Run example
main().catch(console.error);
