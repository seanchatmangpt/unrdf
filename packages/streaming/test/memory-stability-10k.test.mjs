/**
 * @vitest-environment node
 * @fileoverview Memory stability test for Change Feed and RDF Stream Parser
 */

import { describe, it, expect } from 'vitest';
import { createChangeFeed } from '../src/streaming/change-feed.mjs';
import { namedNode, literal } from '@unrdf/core';

describe('Memory Stability (10,000+ events)', () => {
  function getHeapUsed() {
    if (global.gc) {
      global.gc();
    }
    return process.memoryUsage().heapUsed;
  }

  it('ChangeFeed should maintain stable memory over 50,000 events', { timeout: 60000 }, async () => {
    const maxHistory = 1000;
    const feed = createChangeFeed(null, { maxHistorySize: maxHistory });
    
    // Warm up
    for (let i = 0; i < maxHistory; i++) {
      feed.emitChange({
        type: 'add',
        quad: {
          subject: namedNode(`http://example.org/s${i}`),
          predicate: namedNode('http://example.org/p'),
          object: literal(`v${i}`)
        }
      });
    }

    const memoryAfterWarmup = getHeapUsed();
    
    // Cycle 50,000 more events
    for (let i = 0; i < 50000; i++) {
      feed.emitChange({
        type: 'add',
        quad: {
          subject: namedNode(`http://example.org/s${i}`),
          predicate: namedNode('http://example.org/p'),
          object: literal(`v${i}`)
        },
        metadata: {
           large: ' '.repeat(100) // 100 bytes
        }
      });
    }

    const memoryAfterCycle = getHeapUsed();
    
    console.log(`Memory after warmup: ${(memoryAfterWarmup / 1024 / 1024).toFixed(2)} MB`);
    console.log(`Memory after 50k cycles: ${(memoryAfterCycle / 1024 / 1024).toFixed(2)} MB`);

    // Memory should not have grown significantly (allowing some for GC lag/V8 overhead)
    // We expect it to be within 10MB of warmup memory if ring buffer is working
    expect(memoryAfterCycle - memoryAfterWarmup).toBeLessThan(10 * 1024 * 1024);
  });
});
