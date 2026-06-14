/**
 * @file Cross-Product: Streaming + Receipts + QualityMetrics
 * @description Demonstrates how streaming chunk processing with per-chunk receipts
 *   feeds quality metrics — a pipeline where processChunk emits L5 receipts,
 *   generateStreamMerkleProof aggregates them into a tamper-evident root,
 *   and QualityMetricsCollector records throughput and defects.
 *
 * APIs composed:
 *   packages/streaming/src/streaming-receipts.mjs — processChunk, generateStreamMerkleProof
 *   packages/hooks/src/hooks/quality-metrics.mjs — QualityMetricsCollector
 *
 * Docs:
 *   packages/streaming/README.md — "Streaming Receipts" section
 *   packages/hooks/README.md — "QualityMetricsCollector" section
 *
 * Pattern: "Receipted Stream Quality Audit"
 *   1. Process N chunks through processChunk (each gets an L5 receipt)
 *   2. Aggregate receipts into a Merkle proof (tamper detection)
 *   3. Record per-chunk latency in QualityMetricsCollector
 *   4. Inject a known-bad chunk; record it as a defect
 *   5. Export audit trail — shows the stream's quality history
 *
 * EXPECTED OUTPUT:
 *   [processChunk] each chunk gets a receipt with receiptHash
 *   [merkle proof] root hash covers all chunk receipts
 *   [QualityMetricsCollector] records chunk latency and defect count
 *   [audit trail] all defect events in audit log
 */

import {
  processChunk,
  generateStreamMerkleProof,
} from '../packages/streaming/src/streaming-receipts.mjs';
import { createContext } from '../packages/v6-core/src/receipt-pattern.mjs';
import { QualityMetricsCollector } from '../packages/hooks/src/hooks/quality-metrics.mjs';

// ─── 1. Process stream chunks with receipts ───────────────────────────────────

const CHUNK_COUNT = 5;
const receipts = [];

const collector = new QualityMetricsCollector({ maxDataPoints: 1000 });

// processChunk is wrapped with withReceipt and requires a DeterministicContext
// as its first argument. Chain receipts by passing previous hash each time.
let previousReceiptHash = null;

for (let i = 0; i < CHUNK_COUNT; i++) {
  const chunk = { index: i, data: `rdf-quad-batch-${i}`, isLast: i === CHUNK_COUNT - 1 };
  const start = Date.now();

  const ctx = createContext({
    nodeId: 'stream-processor-node',
    t_ns: BigInt(Date.now()) * 1000000n + BigInt(i),
    previousReceiptHash,
  });

  // processChunk returns {result, receipt} — result is the transformed chunk
  const outcome = await processChunk(ctx, chunk);

  const latencyMs = Date.now() - start;

  if (!outcome || typeof outcome !== 'object') {
    throw new Error(`processChunk must return an object, got ${typeof outcome}`);
  }

  // Receipt is on outcome.receipt (withReceipt pattern)
  const receipt = outcome.receipt;
  if (!receipt) throw new Error(`processChunk must return .receipt; got: ${JSON.stringify(Object.keys(outcome))}`);
  if (typeof receipt.receiptHash !== 'string') {
    throw new Error(`receipt.receiptHash must be a string, got: ${JSON.stringify(receipt)}`);
  }

  receipts.push(receipt);
  previousReceiptHash = receipt.receiptHash;

  // Record throughput metric
  collector.record('stream.chunk.latency', latencyMs, { chunkIndex: i });
}

console.log(`[processChunk] ✅ processed ${CHUNK_COUNT} chunks, each with a receipt`);
console.log(`  first receiptHash: ${receipts[0].receiptHash.slice(0, 16)}...`);

// ─── 2. Generate Merkle proof over all chunk receipts ────────────────────────

// NOTE: generateStreamMerkleProof is synchronous but blake3Hash is async.
// For a single receipt, root is the receipt hash string (no hashing needed).
// For multiple receipts, root becomes a Promise<string> — a source-level bug.
// We witness the single-receipt case (correct) and document the multi-receipt behavior.

const singleProof = generateStreamMerkleProof([receipts[0]]);
if (typeof singleProof.root !== 'string') {
  throw new Error(`single-receipt root must be string, got ${typeof singleProof.root}`);
}
if (singleProof.root !== receipts[0].receiptHash) {
  throw new Error('single-receipt root must equal the receipt hash');
}
if (singleProof.chunkCount !== 1) throw new Error('chunkCount must be 1 for single receipt');
console.log(`[generateStreamMerkleProof] ✅ single-receipt root=${singleProof.root.slice(0, 16)}..., chunkCount=1`);

// Multi-receipt: root is a Promise (blake3Hash is async but function is sync)
// This is a known source discrepancy — document it, don't paper over it.
const multiProof = generateStreamMerkleProof(receipts);
if (!Array.isArray(multiProof.tree)) throw new Error('proof.tree must be an array');
if (multiProof.chunkCount !== CHUNK_COUNT) {
  throw new Error(`proof.chunkCount=${multiProof.chunkCount}, expected ${CHUNK_COUNT}`);
}
const rootIsAsync = multiProof.root instanceof Promise;
if (rootIsAsync) {
  // Source discrepancy: blake3Hash is async but generateStreamMerkleProof is sync
  // The root for multi-chunk proofs is a Promise, not a resolved string.
  const resolvedRoot = await multiProof.root;
  console.log(`[generateStreamMerkleProof] ℹ️  multi-receipt root is Promise (blake3Hash async) — resolved: ${resolvedRoot.slice(0,16)}...`);
  console.log(`[generateStreamMerkleProof] ✅ chunkCount=${multiProof.chunkCount}, treeLevels=${multiProof.tree.length}`);
} else {
  console.log(`[generateStreamMerkleProof] ✅ root=${String(multiProof.root).slice(0,16)}..., chunkCount=${multiProof.chunkCount}`);
}

// ─── 3. Inject a bad chunk — record as a defect ──────────────────────────────

let defectDetected = false;
try {
  // index=-1 is invalid (nonnegative constraint)
  const badCtx = createContext({ nodeId: 'stream-processor-node', previousReceiptHash: null });
  await processChunk(badCtx, { index: -1, data: 'bad', isLast: false });
} catch (e) {
  defectDetected = true;
  collector.recordDefect('stream.chunk.validation', {
    chunkIndex: -1,
    reason: e.message,
  });
}

if (!defectDetected) throw new Error('processChunk must throw for index=-1 (schema violation)');

console.log(`[defect recording] ✅ invalid chunk rejected; defect recorded in QualityMetricsCollector`);

// ─── 4. Verify QualityMetricsCollector captured the stream activity ───────────

const summary = collector.getSummary();

if (summary.dataPointsStored !== CHUNK_COUNT) {
  throw new Error(`Expected dataPointsStored=${CHUNK_COUNT}, got ${summary.dataPointsStored}`);
}
if (summary.totalDefects !== 1) {
  throw new Error(`Expected 1 defect (bad chunk), got ${summary.totalDefects}`);
}

console.log(`[QualityMetricsCollector] ✅ dataPointsStored=${summary.dataPointsStored}, totalDefects=${summary.totalDefects}`);
console.log(`[QualityMetricsCollector] ✅ dpmo=${summary.dpmo.toFixed(0)}, sigmaLevel=${summary.sigmaLevel.toFixed(2)}`);

// ─── 5. Export audit trail ────────────────────────────────────────────────────

const trail = collector.exportAuditTrail({ type: 'defect-recorded' });

if (!Array.isArray(trail)) throw new Error('exportAuditTrail must return an array');
if (trail.length !== 1) {
  throw new Error(`Expected 1 defect audit entry, got ${trail.length}`);
}
const entry = trail[0];
if (entry.type !== 'defect-recorded') throw new Error('Audit entry type must be defect-recorded');

console.log(`[audit trail] ✅ ${trail.length} defect entry in audit log: type=${entry.type}`);

// ─── 6. Forgery witnesses ─────────────────────────────────────────────────────

// Empty receipts must throw
let threwOnEmpty = false;
try { generateStreamMerkleProof([]); } catch { threwOnEmpty = true; }
if (!threwOnEmpty) throw new Error('generateStreamMerkleProof([]) must throw');
console.log(`[forgery witness] ✅ empty receipts array throws`);

console.log('\n✅ All cross-product (streaming + receipts + quality metrics) assertions passed.');
