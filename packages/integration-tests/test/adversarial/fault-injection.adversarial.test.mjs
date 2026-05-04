/**
 * Fault Injection Adversarial Security Tests
 * Phase 5: 5 tests covering Worker crash, Disk I/O failure, Network timeout, Receipt corruption, Clock skew
 *
 * @module @unrdf/integration-tests/test/adversarial/fault-injection
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { blake3 } from 'hash-wasm';

/**
 * Simulated Worker with crash recovery
 */
class ResilientWorker {
  constructor(id) {
    this.id = id;
    this.running = false;
    this.restartCount = 0;
    this.maxRestarts = 3;
    this.tasks = [];
    this.crashOnNext = false;
  }

  start() {
    this.running = true;
  }

  stop() {
    this.running = false;
  }

  /**
   * Execute task with crash simulation
   * @param {Function} task
   */
  async execute(task) {
    if (this.crashOnNext) {
      this.crashOnNext = false;
      this.running = false;
      throw new Error(`Worker ${this.id} crashed`);
    }

    if (!this.running) {
      throw new Error(`Worker ${this.id} is not running`);
    }

    return task();
  }

  /**
   * Attempt restart
   * @returns {boolean}
   */
  restart() {
    if (this.restartCount >= this.maxRestarts) {
      return false;
    }

    this.restartCount++;
    this.running = true;
    return true;
  }

  /**
   * Schedule crash on next execution
   */
  scheduleCrash() {
    this.crashOnNext = true;
  }
}

/**
 * Simulated Storage with I/O failure injection
 */
class FaultyStorage {
  constructor() {
    this.data = new Map();
    this.failNextWrite = false;
    this.failNextRead = false;
  }

  async write(key, value) {
    if (this.failNextWrite) {
      this.failNextWrite = false;
      throw new Error('Disk I/O write failure');
    }

    this.data.set(key, value);
    return { success: true };
  }

  async read(key) {
    if (this.failNextRead) {
      this.failNextRead = false;
      throw new Error('Disk I/O read failure');
    }

    if (!this.data.has(key)) {
      return { success: false, error: 'Key not found' };
    }

    return { success: true, value: this.data.get(key) };
  }

  injectWriteFailure() {
    this.failNextWrite = true;
  }

  injectReadFailure() {
    this.failNextRead = true;
  }
}

/**
 * Network client with timeout and retry
 */
class ResilientNetworkClient {
  constructor(options = {}) {
    this.timeout = options.timeout || 1000;
    this.maxRetries = options.maxRetries || 3;
    this.backoffMs = options.backoffMs || 100;
    this.shouldTimeout = false;
    this.timeoutCount = 0;
  }

  /**
   * Fetch with retry and backoff
   * @param {string} url
   */
  async fetch(url) {
    let lastError;

    for (let attempt = 0; attempt <= this.maxRetries; attempt++) {
      try {
        if (this.shouldTimeout && this.timeoutCount > 0) {
          this.timeoutCount--;
          throw new Error('Network timeout');
        }

        // Simulate successful request
        return { success: true, data: { url, timestamp: Date.now() } };
      } catch (error) {
        lastError = error;

        if (attempt < this.maxRetries) {
          // Exponential backoff
          await new Promise(r => setTimeout(r, this.backoffMs * Math.pow(2, attempt)));
        }
      }
    }

    throw lastError;
  }

  /**
   * Inject timeout failures
   * @param {number} count - Number of timeouts before success
   */
  injectTimeouts(count) {
    this.shouldTimeout = true;
    this.timeoutCount = count;
  }
}

/**
 * Receipt validator with corruption detection
 */
class ReceiptValidator {
  /**
   * Validate receipt integrity
   * @param {Object} receipt
   * @returns {Promise<Object>}
   */
  async validate(receipt) {
    const errors = [];

    // Check required fields
    if (!receipt.id) errors.push('Missing receipt ID');
    if (!receipt.hash) errors.push('Missing hash');
    if (!receipt.data) errors.push('Missing data');
    if (receipt.timestamp === undefined) errors.push('Missing timestamp');

    // Validate hash format
    if (receipt.hash && !/^[a-f0-9]{64}$/.test(receipt.hash)) {
      errors.push('Invalid hash format');
    }

    // Verify hash matches content
    if (receipt.data) {
      const serialized = JSON.stringify(receipt.data, (key, value) =>
        typeof value === 'bigint' ? value.toString() : value
      );
      const expectedHash = await blake3(serialized);

      if (receipt.hash !== expectedHash) {
        errors.push('Hash mismatch - receipt corrupted');
      }
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  }
}

/**
 * Clock with skew detection
 */
class SkewAwareClock {
  constructor(maxSkewMinutes = 5) {
    this.maxSkewMs = maxSkewMinutes * 60 * 1000;
    this.offset = 0;
  }

  /**
   * Get current time
   */
  now() {
    return Date.now() + this.offset;
  }

  /**
   * Inject clock skew
   * @param {number} offsetMs
   */
  injectSkew(offsetMs) {
    this.offset = offsetMs;
  }

  /**
   * Validate timestamp is within acceptable range
   * @param {number} timestamp
   * @returns {Object}
   */
  validateTimestamp(timestamp) {
    const currentTime = this.now();
    const diff = Math.abs(currentTime - timestamp);

    if (diff > this.maxSkewMs) {
      return {
        valid: false,
        error: `Timestamp skew ${Math.round(diff / 1000)}s exceeds max ${this.maxSkewMs / 60000} minutes`,
        skewMs: diff,
      };
    }

    return { valid: true, skewMs: diff };
  }

  /**
   * Reset to no skew
   */
  reset() {
    this.offset = 0;
  }
}

describe('Fault Injection Adversarial Tests', () => {
  // Test 1: Worker thread crash - automatic restart
  it('should recover from worker thread crash', async () => {
    const worker = new ResilientWorker('worker-1');
    worker.start();

    // Normal execution
    const result1 = await worker.execute(() => 'task-1-complete');
    expect(result1).toBe('task-1-complete');

    // Schedule crash
    worker.scheduleCrash();

    // Should crash
    await expect(worker.execute(() => 'task-2')).rejects.toThrow('crashed');
    expect(worker.running).toBe(false);

    // Restart worker
    const restarted = worker.restart();
    expect(restarted).toBe(true);
    expect(worker.running).toBe(true);
    expect(worker.restartCount).toBe(1);

    // Should work after restart
    const result2 = await worker.execute(() => 'task-3-complete');
    expect(result2).toBe('task-3-complete');

    // Exhaust restart attempts
    for (let i = 0; i < 3; i++) {
      worker.scheduleCrash();
      try {
        await worker.execute(() => 'crash');
      } catch (e) {
        worker.restart();
      }
    }

    // Should fail to restart after max attempts
    worker.scheduleCrash();
    try {
      await worker.execute(() => 'crash');
    } catch (e) {
      const canRestart = worker.restart();
      expect(canRestart).toBe(false);
    }
  });

  // Test 2: Disk I/O failure - graceful degradation
  it('should handle disk I/O failure gracefully', async () => {
    const storage = new FaultyStorage();

    // Normal write
    const writeResult1 = await storage.write('key1', 'value1');
    expect(writeResult1.success).toBe(true);

    // Inject write failure
    storage.injectWriteFailure();

    // Should fail gracefully
    await expect(storage.write('key2', 'value2')).rejects.toThrow('Disk I/O write failure');

    // Subsequent writes should work
    const writeResult3 = await storage.write('key3', 'value3');
    expect(writeResult3.success).toBe(true);

    // Normal read
    const readResult1 = await storage.read('key1');
    expect(readResult1.success).toBe(true);
    expect(readResult1.value).toBe('value1');

    // Inject read failure
    storage.injectReadFailure();

    // Should fail gracefully
    await expect(storage.read('key1')).rejects.toThrow('Disk I/O read failure');

    // Subsequent reads should work
    const readResult2 = await storage.read('key1');
    expect(readResult2.success).toBe(true);
  });

  // Test 3: Network timeout - retry with backoff
  it('should retry network requests with backoff', async () => {
    const client = new ResilientNetworkClient({
      timeout: 100,
      maxRetries: 3,
      backoffMs: 10,
    });

    // Normal request
    const result1 = await client.fetch('http://example.com/api');
    expect(result1.success).toBe(true);

    // Inject 2 timeouts (should succeed on 3rd attempt)
    client.injectTimeouts(2);

    const startTime = performance.now();
    const result2 = await client.fetch('http://example.com/api');
    const duration = performance.now() - startTime;

    expect(result2.success).toBe(true);
    // Should have waited for backoff (10ms + 20ms = 30ms minimum)
    expect(duration).toBeGreaterThan(20);

    // Inject more timeouts than retries
    client.injectTimeouts(5);

    await expect(client.fetch('http://example.com/api')).rejects.toThrow('Network timeout');
  });

  // Test 4: Partial receipt corruption - validation fails
  it('should detect partial receipt corruption', async () => {
    const validator = new ReceiptValidator();

    // Create valid receipt
    const data = { action: 'transfer', amount: 100, to: 'alice' };
    const serialized = JSON.stringify(data);
    const hash = await blake3(serialized);

    const validReceipt = {
      id: 'receipt-001',
      hash,
      data,
      timestamp: Date.now(),
    };

    // Valid receipt should pass
    const validResult = await validator.validate(validReceipt);
    expect(validResult.valid).toBe(true);
    expect(validResult.errors).toHaveLength(0);

    // Corrupt the hash
    const corruptedHashReceipt = {
      ...validReceipt,
      hash: 'a'.repeat(64), // Wrong hash
    };

    const corruptedResult = await validator.validate(corruptedHashReceipt);
    expect(corruptedResult.valid).toBe(false);
    expect(corruptedResult.errors).toContain('Hash mismatch - receipt corrupted');

    // Corrupt the data
    const corruptedDataReceipt = {
      ...validReceipt,
      data: { ...data, amount: 999 }, // Modified amount
    };

    const dataCorruptedResult = await validator.validate(corruptedDataReceipt);
    expect(dataCorruptedResult.valid).toBe(false);
    expect(dataCorruptedResult.errors).toContain('Hash mismatch - receipt corrupted');

    // Missing fields
    const incompleteReceipt = {
      id: 'incomplete',
    };

    const incompleteResult = await validator.validate(incompleteReceipt);
    expect(incompleteResult.valid).toBe(false);
    expect(incompleteResult.errors.length).toBeGreaterThan(0);
  });

  // Test 5: Clock skew >5min - timestamp rejection
  it('should reject timestamps with excessive clock skew', () => {
    const clock = new SkewAwareClock(5); // 5 minute max skew

    // Normal timestamp should pass
    const validTimestamp = clock.now();
    const validResult = clock.validateTimestamp(validTimestamp);
    expect(validResult.valid).toBe(true);

    // 2 minute skew should pass
    const twoMinAgo = clock.now() - 2 * 60 * 1000;
    const twoMinResult = clock.validateTimestamp(twoMinAgo);
    expect(twoMinResult.valid).toBe(true);

    // 10 minute skew should fail
    const tenMinAgo = clock.now() - 10 * 60 * 1000;
    const tenMinResult = clock.validateTimestamp(tenMinAgo);
    expect(tenMinResult.valid).toBe(false);
    expect(tenMinResult.error).toContain('exceeds max');

    // Future timestamp with large skew should fail
    const tenMinFuture = clock.now() + 10 * 60 * 1000;
    const futureResult = clock.validateTimestamp(tenMinFuture);
    expect(futureResult.valid).toBe(false);

    // Inject clock skew
    clock.injectSkew(6 * 60 * 1000); // 6 minute forward skew

    // Old timestamp relative to skewed clock should fail
    const oldTimestamp = Date.now(); // Real time, not skewed
    const skewedResult = clock.validateTimestamp(oldTimestamp);
    expect(skewedResult.valid).toBe(false);

    // Reset clock
    clock.reset();
    const resetResult = clock.validateTimestamp(Date.now());
    expect(resetResult.valid).toBe(true);
  });
});
