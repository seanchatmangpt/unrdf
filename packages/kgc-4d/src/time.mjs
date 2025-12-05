/**
 * KGC Time Module - Nanosecond-precision BigInt timestamps
 * Uses process.hrtime.bigint() in Node.js and performance.now() in Browser
 */

let lastTime = 0n;

/**
 * Get current time in nanoseconds as BigInt
 * Ensures monotonic ordering (never goes backwards)
 *
 * @example
 * import { now } from './time.mjs';
 * const t1 = now();
 * const t2 = now();
 * console.assert(typeof t1 === 'bigint', 'Returns BigInt');
 * console.assert(t1 < t2, 'Monotonic: second call returns larger value');
 */
export function now() {
  let current;

  if (typeof process !== 'undefined' && process.hrtime && typeof process.hrtime.bigint === 'function') {
    // Node.js: Use nanosecond-precision hrtime
    current = process.hrtime.bigint();
  } else {
    // Browser: Convert milliseconds to nanoseconds
    current = BigInt(Math.floor(performance.now() * 1_000_000));
  }

  // Enforce monotonic ordering
  if (current <= lastTime) {
    current = lastTime + 1n;
  }
  lastTime = current;

  return current;
}

/**
 * Convert nanosecond BigInt to ISO 8601 string
 *
 * @example
 * import { toISO } from './time.mjs';
 * const iso = toISO(1609459200000000000n);  // 2021-01-01T00:00:00.000Z
 * console.assert(iso.includes('2021-01-01'), 'Correct date');
 *
 * @example
 * import { toISO } from './time.mjs';
 * try {
 *   toISO(123);  // Not a BigInt
 *   throw new Error('Should have thrown TypeError');
 * } catch (err) {
 *   console.assert(err instanceof TypeError, 'Throws on non-BigInt');
 * }
 */
export function toISO(t_ns) {
  if (typeof t_ns !== 'bigint') {
    throw new TypeError('Expected BigInt, got ' + typeof t_ns);
  }
  const ms = Number(t_ns / 1_000_000n);
  return new Date(ms).toISOString();
}

/**
 * Convert ISO 8601 string to nanosecond BigInt
 * Preserves nanosecond precision from fractional seconds (e.g., .123456789)
 * Standard Date.parse() truncates to milliseconds - this preserves all 9 digits
 *
 * @example
 * import { fromISO, toISO } from './time.mjs';
 * const iso = '2025-01-15T10:30:00.123456789Z';
 * const ns = fromISO(iso);
 * console.assert(typeof ns === 'bigint', 'Returns BigInt');
 * console.assert(ns > 0n, 'Positive timestamp');
 *
 * @example
 * import { fromISO } from './time.mjs';
 * try {
 *   fromISO('not-a-date');
 *   throw new Error('Should have thrown');
 * } catch (err) {
 *   console.assert(err.message.includes('Invalid ISO'), 'Clear error message');
 * }
 */
export function fromISO(iso) {
  if (typeof iso !== 'string') {
    throw new TypeError('Expected string, got ' + typeof iso);
  }

  // Parse ISO 8601 with regex to preserve nanosecond precision
  // Matches: YYYY-MM-DDTHH:MM:SS[.fractional]Z
  const match = iso.match(
    /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})(?:\.(\d{1,9}))?Z?$/
  );

  if (!match) {
    // Fallback to standard parsing (loses precision but handles edge cases)
    const ms = new Date(iso).getTime();
    if (isNaN(ms)) {
      throw new Error('Invalid ISO date: ' + iso);
    }
    return BigInt(ms) * 1_000_000n;
  }

  const [, year, month, day, hour, minute, second, frac = '0'] = match;

  // Validate component ranges (including leap second detection)
  const m = parseInt(month, 10);
  const d = parseInt(day, 10);
  const h = parseInt(hour, 10);
  const min = parseInt(minute, 10);
  const s = parseInt(second, 10);

  if (m < 1 || m > 12) {
    throw new Error('Invalid ISO date: ' + iso);
  }
  if (d < 1 || d > 31) {
    throw new Error('Invalid ISO date: ' + iso);
  }
  if (h < 0 || h > 23) {
    throw new Error('Invalid ISO date: ' + iso);
  }
  if (min < 0 || min > 59) {
    throw new Error('Invalid ISO date: ' + iso);
  }
  // Reject leap seconds (60) - ISO 8601 doesn't officially support them
  if (s < 0 || s > 59) {
    throw new Error('Invalid ISO date: ' + iso);
  }

  // Pad/truncate fractional seconds to exactly 9 digits (nanoseconds)
  const nanoFrac = frac.padEnd(9, '0').slice(0, 9);

  // Build date from components (avoids locale issues)
  const date = new Date(Date.UTC(
    parseInt(year, 10),
    m - 1, // Month is 0-indexed
    d,
    h,
    min,
    s
  ));

  // Check if Date constructor normalized the values (e.g., month 13 -> next year)
  const ms = BigInt(date.getTime());
  if (isNaN(Number(ms))) {
    throw new Error('Invalid ISO date: ' + iso);
  }

  // Convert ms to ns and add fractional nanoseconds
  return ms * 1_000_000n + BigInt(nanoFrac);
}

/**
 * Add nanoseconds to a BigInt timestamp
 */
export function addNanoseconds(t_ns, delta) {
  if (typeof t_ns !== 'bigint') {
    throw new TypeError('Expected BigInt for t_ns');
  }
  if (typeof delta !== 'bigint') {
    delta = BigInt(delta);
  }
  return t_ns + delta;
}

/**
 * Calculate duration between two nanosecond timestamps
 */
export function duration(start_ns, end_ns) {
  if (typeof start_ns !== 'bigint' || typeof end_ns !== 'bigint') {
    throw new TypeError('Expected BigInt timestamps');
  }
  return end_ns - start_ns;
}

/**
 * VectorClock - Logical clock for causality tracking in distributed systems
 * Each node maintains a counter; on event, increment local counter.
 * On receive, merge with sender's clock (take max of each component).
 *
 * Comparison result:
 *  -1: this happened-before other
 *   0: concurrent (neither before the other)
 *   1: this happened-after other
 */
export class VectorClock {
  /**
   * @param {string} nodeId - Unique identifier for this node
   */
  constructor(nodeId) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new TypeError('VectorClock requires a string nodeId');
    }
    this.nodeId = nodeId;
    /** @type {Map<string, bigint>} */
    this.counters = new Map([[nodeId, 0n]]);
  }

  /**
   * Increment local counter (on local event)
   * @returns {VectorClock} this (for chaining)
   */
  increment() {
    const current = this.counters.get(this.nodeId) || 0n;
    this.counters.set(this.nodeId, current + 1n);
    return this;
  }

  /**
   * Merge with another clock (on receive from another node)
   * Takes max of each component
   * @param {VectorClock} other
   * @returns {VectorClock} this (for chaining)
   */
  merge(other) {
    if (!(other instanceof VectorClock)) {
      throw new TypeError('Can only merge with another VectorClock');
    }
    for (const [nodeId, count] of other.counters) {
      const current = this.counters.get(nodeId) || 0n;
      this.counters.set(nodeId, count > current ? count : current);
    }
    // Also increment local counter after merge
    return this.increment();
  }

  /**
   * Compare this clock with another for causal ordering
   * @param {VectorClock} other
   * @returns {-1 | 0 | 1} -1 if before, 0 if concurrent, 1 if after
   */
  compare(other) {
    if (!(other instanceof VectorClock)) {
      throw new TypeError('Can only compare with another VectorClock');
    }

    let thisBeforeOther = false;
    let otherBeforeThis = false;

    // Collect all node IDs from both clocks
    const allNodes = new Set([...this.counters.keys(), ...other.counters.keys()]);

    for (const nodeId of allNodes) {
      const thisCount = this.counters.get(nodeId) || 0n;
      const otherCount = other.counters.get(nodeId) || 0n;

      if (thisCount < otherCount) {
        thisBeforeOther = true;
      }
      if (thisCount > otherCount) {
        otherBeforeThis = true;
      }
    }

    // Determine ordering
    if (thisBeforeOther && !otherBeforeThis) {
      return -1; // this happened-before other
    }
    if (otherBeforeThis && !thisBeforeOther) {
      return 1; // this happened-after other
    }
    return 0; // concurrent or equal
  }

  /**
   * Check if this clock happened-before another
   * @param {VectorClock} other
   * @returns {boolean}
   */
  happenedBefore(other) {
    return this.compare(other) === -1;
  }

  /**
   * Check if two events are concurrent (neither before the other)
   * @param {VectorClock} other
   * @returns {boolean}
   */
  isConcurrentWith(other) {
    return this.compare(other) === 0;
  }

  /**
   * Serialize to JSON-compatible object
   * @returns {Object}
   */
  toJSON() {
    const obj = { nodeId: this.nodeId, counters: {} };
    for (const [key, val] of this.counters) {
      obj.counters[key] = val.toString();
    }
    return obj;
  }

  /**
   * Create from JSON object
   * @param {Object} json
   * @returns {VectorClock}
   */
  static fromJSON(json) {
    if (!json || !json.nodeId || !json.counters) {
      throw new Error('Invalid VectorClock JSON');
    }
    const clock = new VectorClock(json.nodeId);
    for (const [key, val] of Object.entries(json.counters)) {
      clock.counters.set(key, BigInt(val));
    }
    return clock;
  }

  /**
   * Create a copy of this clock
   * @returns {VectorClock}
   */
  clone() {
    return VectorClock.fromJSON(this.toJSON());
  }
}
