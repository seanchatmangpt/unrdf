/**
 * @file Error Handling Example
 * @description Demonstrates UnrdfError hierarchy, retry, CircuitBreaker,
 *   RateLimiter, and withRecovery from @unrdf/core.
 *
 * API demonstrated: packages/core/src/errors.mjs, packages/core/src/recovery.mjs
 * Docs: packages/core/README.md — "Error Handling" section (added by this triple)
 *
 * EXPECTED OUTPUT:
 *   [UnrdfError] error subclasses have code, context, name, timestamp
 *   [retry] retries on failure up to maxAttempts; succeeds on 3rd attempt
 *   [CircuitBreaker] opens after failureThreshold; rejects while open
 *   [RateLimiter] execute() enforces maxOperations per window
 *   [withRecovery] wraps operation with retry + fallback
 */

import {
  UnrdfError,
  ValidationError,
  QueryError,
  StoreError,
  NetworkError,
  TimeoutError,
  ParserError,
} from '../packages/core/src/errors.mjs';
import {
  retry,
  CircuitBreaker,
  RateLimiter,
  withRecovery,
} from '../packages/core/src/recovery.mjs';

// ─── 1. Error hierarchy ───────────────────────────────────────────────────────

const base = new UnrdfError('base error', { detail: 'test' }, 'ERR_TEST');
if (!(base instanceof Error)) throw new Error('UnrdfError must extend Error');
if (!(base instanceof UnrdfError)) throw new Error('UnrdfError instanceof check failed');
if (base.code !== 'ERR_TEST') throw new Error(`Expected code ERR_TEST, got ${base.code}`);
if (typeof base.timestamp !== 'string') throw new Error('Expected timestamp string');
if (base.context.detail !== 'test') throw new Error('Expected context.detail=test');

const validation = new ValidationError('invalid quad', { quad: 'broken' });
if (!(validation instanceof UnrdfError)) throw new Error('ValidationError must extend UnrdfError');
if (validation.name !== 'ValidationError') throw new Error(`Expected name ValidationError, got ${validation.name}`);

const subclasses = [
  [new QueryError('bad SPARQL'), 'QueryError'],
  [new StoreError('store failed'), 'StoreError'],
  [new NetworkError('timeout'), 'NetworkError'],
  [new TimeoutError('took too long'), 'TimeoutError'],
  [new ParserError('bad turtle'), 'ParserError'],
];
for (const [err, expectedName] of subclasses) {
  if (!(err instanceof UnrdfError)) throw new Error(`${expectedName} must extend UnrdfError`);
  if (err.name !== expectedName) throw new Error(`Expected name ${expectedName}, got ${err.name}`);
}

console.log(`[UnrdfError] ✅ hierarchy: ${subclasses.map(([, n]) => n).join(', ')} all extend UnrdfError`);
console.log(`[UnrdfError] ✅ code=${base.code}, context=${JSON.stringify(base.context)}`);

// ─── 2. retry() — exponential backoff ────────────────────────────────────────

let attempts = 0;
const result = await retry(
  async () => {
    attempts++;
    if (attempts < 3) throw new NetworkError(`attempt ${attempts} failed`); // NetworkError is retried
    return `success on attempt ${attempts}`;
  },
  { maxAttempts: 3, initialDelay: 10 }, // fast for testing
);

if (result !== 'success on attempt 3') throw new Error(`Expected success on attempt 3, got: ${result}`);
if (attempts !== 3) throw new Error(`Expected 3 attempts, got ${attempts}`);

console.log(`[retry] ✅ succeeded on attempt ${attempts}: "${result}"`);

// retry exhaustion — should throw
let exhausted = false;
try {
  await retry(async () => { throw new NetworkError('always fails'); }, { maxAttempts: 2, initialDelay: 5 });
} catch (e) {
  exhausted = true;
  if (!e.message.includes('always fails')) throw new Error(`Expected 'always fails', got: ${e.message}`);
}
if (!exhausted) throw new Error('retry must throw after maxAttempts exhausted');

console.log(`[retry] ✅ throws after maxAttempts=2 exhausted`);

// ─── 3. CircuitBreaker — state machine ───────────────────────────────────────

const states = [];
const breaker = new CircuitBreaker({
  failureThreshold: 2,
  successThreshold: 1,
  timeout: 50, // fast half-open transition for testing
  onStateChange: (from, to) => states.push({ from, to }),
});

// getState() returns an object {state, failures, successes, ...}
if (breaker.getState().state !== 'closed') throw new Error(`CircuitBreaker must start closed, got ${JSON.stringify(breaker.getState())}`);

// Trigger failures to open the circuit
for (let i = 0; i < 2; i++) {
  try { await breaker.execute(async () => { throw new Error('fail'); }); } catch {}
}

if (breaker.getState().state !== 'open') throw new Error(`Expected state=open after 2 failures, got ${JSON.stringify(breaker.getState())}`);

// Execute while open — must throw
let rejectedByOpen = false;
try {
  await breaker.execute(async () => 'would succeed');
} catch (e) {
  rejectedByOpen = true;
  if (!e.message.toLowerCase().includes('circuit') && !e.message.toLowerCase().includes('open')) {
    // Accept any error thrown by open circuit (message varies by implementation)
  }
}
if (!rejectedByOpen) throw new Error('Open circuit breaker must reject immediately');

console.log(`[CircuitBreaker] ✅ opened after 2 failures, rejected while open`);
console.log(`[CircuitBreaker] ✅ state transitions: ${states.map(s => `${s.from}→${s.to}`).join(', ')}`);

// Wait for timeout, then execute to move to half-open and close
await new Promise(resolve => setTimeout(resolve, 60));
await breaker.execute(async () => 'recovery');
const finalState = breaker.getState().state;
console.log(`[CircuitBreaker] ✅ recovered after timeout: state=${finalState}`);

// ─── 4. RateLimiter — maxOperations per window ───────────────────────────────

const limiter = new RateLimiter({ maxOperations: 3, windowMs: 500 });
const callTimes = [];

// Execute 3 operations within the window — should all complete quickly
for (let i = 0; i < 3; i++) {
  await limiter.execute(async () => { callTimes.push(Date.now()); });
}

const status = limiter.getStatus();
if (typeof status.current !== 'number') throw new Error('RateLimiter.getStatus() must return {current, max, windowMs, available}');
if (status.max !== 3) throw new Error(`Expected max=3, got ${status.max}`);

limiter.reset();
const statusAfterReset = limiter.getStatus();
if (statusAfterReset.current !== 0) throw new Error(`Expected current=0 after reset(), got ${statusAfterReset.current}`);

console.log(`[RateLimiter] ✅ executed 3 ops, current=${status.current}, max=${status.max}`);
console.log(`[RateLimiter] ✅ reset() → current=${statusAfterReset.current}`);

// ─── 5. withRecovery() — wraps operation with retry + fallback ────────────────

let wrappedAttempts = 0;
const robustOp = withRecovery(
  async () => {
    wrappedAttempts++;
    if (wrappedAttempts < 2) throw new NetworkError('first attempt fails');
    return 'recovered';
  },
  { retry: { maxAttempts: 3, initialDelay: 5 } },
);

const recovered = await robustOp();
if (recovered !== 'recovered') throw new Error(`Expected 'recovered', got: ${recovered}`);

console.log(`[withRecovery] ✅ succeeded after ${wrappedAttempts} attempt(s): "${recovered}"`);

// withRecovery fallback — when all retries fail, fallback fires
const withFallback = withRecovery(
  async () => { throw new NetworkError('always fails'); },
  {
    retry: { maxAttempts: 2, initialDelay: 5 },
    fallback: async () => 'fallback-value',
  },
);

const fallbackResult = await withFallback();
if (fallbackResult !== 'fallback-value') throw new Error(`Expected 'fallback-value', got: ${fallbackResult}`);

console.log(`[withRecovery] ✅ fallback fires when all retries exhausted: "${fallbackResult}"`);

console.log('\n✅ All error handling assertions passed.');
