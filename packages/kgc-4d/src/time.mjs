/**
 * KGC Time Module - Nanosecond-precision BigInt timestamps
 * Uses process.hrtime.bigint() in Node.js and performance.now() in Browser
 */

let lastTime = 0n;

/**
 * Get current time in nanoseconds as BigInt
 * Ensures monotonic ordering (never goes backwards)
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
 */
export function fromISO(iso) {
  if (typeof iso !== 'string') {
    throw new TypeError('Expected string, got ' + typeof iso);
  }
  const ms = new Date(iso).getTime();
  if (isNaN(ms)) {
    throw new Error('Invalid ISO date: ' + iso);
  }
  return BigInt(ms) * 1_000_000n;
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
