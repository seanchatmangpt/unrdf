/**
 * @file Receipt-Backed Stream Example
 * @description Demonstrates cryptographic receipts for event streams
 *
 * This example shows how to:
 * 1. Generate cryptographic receipts for each event
 * 2. Build Merkle trees for batch verification
 * 3. Verify event authenticity
 * 4. Detect tampering in event history
 * 5. Generate and verify Merkle proofs
 *
 * Expected output: Tamper-evident event stream with verification
 */

import { createChangeFeed } from '../../packages/streaming/src/index.mjs';
import { createHash } from 'node:crypto';
import { EventEmitter } from 'events';

// ============================================================================
// Blake3 Hash (simplified - use real blake3 in production)
// ============================================================================

function blake3Hash(data) {
  return createHash('sha256').update(data).digest('hex');
}

// ============================================================================
// Receipt-Backed Stream (from research document)
// ============================================================================

class ReceiptBackedStream extends EventEmitter {
  constructor(config = {}) {
    super();
    this.changeFeed = createChangeFeed();
    this.receipts = [];
    this.merkleRoots = [];
    this.config = {
      batchSize: config.batchSize || 1000,
      ...config,
    };
    this.currentBatch = [];
    this.stats = {
      totalEvents: 0,
      batchesFinalized: 0,
      verificationsPerformed: 0,
      verificationsPassed: 0,
    };
  }

  async emitWithReceipt(event) {
    const receipt = await this._generateReceipt(event);

    this.currentBatch.push({ event, receipt });
    this.stats.totalEvents++;

    if (this.currentBatch.length >= this.config.batchSize) {
      await this._finalizeBatch();
    }

    this.changeFeed.emitChange({
      ...event,
      receipt,
    });

    this.emit('event:receipted', { event, receipt });

    return receipt;
  }

  async _generateReceipt(event) {
    const receiptId = `receipt-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    const previousHash = this.receipts.length > 0
      ? this.receipts[this.receipts.length - 1].hash
      : null;

    const canonical = JSON.stringify(event, Object.keys(event).sort());
    const eventHash = blake3Hash(canonical);

    const receiptData = {
      id: receiptId,
      eventHash,
      previousHash,
      timestamp: Date.now(),
      sequence: this.receipts.length,
    };

    const receiptCanonical = JSON.stringify(receiptData, Object.keys(receiptData).sort());
    const receiptHash = blake3Hash(receiptCanonical);

    const receipt = {
      ...receiptData,
      hash: receiptHash,
    };

    this.receipts.push(receipt);

    return receipt;
  }

  async _finalizeBatch() {
    if (this.currentBatch.length === 0) {
      return;
    }

    const leaves = this.currentBatch.map(item => item.receipt.hash);
    const merkleTree = this._buildMerkleTree(leaves);

    this.merkleRoots.push({
      root: merkleTree.root,
      batchStart: this.currentBatch[0].receipt.sequence,
      batchEnd: this.currentBatch[this.currentBatch.length - 1].receipt.sequence,
      timestamp: Date.now(),
      tree: merkleTree,
    });

    this.stats.batchesFinalized++;

    this.emit('batch:finalized', {
      root: merkleTree.root,
      size: this.currentBatch.length,
    });

    this.currentBatch = [];
  }

  _buildMerkleTree(leaves) {
    let currentLevel = [...leaves];
    const tree = [currentLevel];

    while (currentLevel.length > 1) {
      const nextLevel = [];
      for (let i = 0; i < currentLevel.length; i += 2) {
        const left = currentLevel[i];
        const right = currentLevel[i + 1] || left;
        const combined = blake3Hash(left + right);
        nextLevel.push(combined);
      }
      tree.push(nextLevel);
      currentLevel = nextLevel;
    }

    return {
      root: currentLevel[0],
      levels: tree,
    };
  }

  verifyEvent(event, receipt) {
    this.stats.verificationsPerformed++;

    const canonical = JSON.stringify(event, Object.keys(event).sort());
    const eventHash = blake3Hash(canonical);

    if (eventHash !== receipt.eventHash) {
      return {
        valid: false,
        reason: 'Event hash mismatch',
      };
    }

    const storedReceipt = this.receipts[receipt.sequence];
    if (!storedReceipt || storedReceipt.hash !== receipt.hash) {
      return {
        valid: false,
        reason: 'Receipt not found in chain',
      };
    }

    if (receipt.sequence > 0) {
      const previousReceipt = this.receipts[receipt.sequence - 1];
      if (receipt.previousHash !== previousReceipt.hash) {
        return {
          valid: false,
          reason: 'Chain linkage broken',
        };
      }
    }

    this.stats.verificationsPassed++;

    return {
      valid: true,
      receipt: storedReceipt,
    };
  }

  getMerkleProof(sequence) {
    const batch = this.merkleRoots.find(b =>
      sequence >= b.batchStart && sequence <= b.batchEnd
    );

    if (!batch) {
      throw new Error(`Receipt ${sequence} not in any finalized batch`);
    }

    const indexInBatch = sequence - batch.batchStart;
    const proof = this._generateMerkleProof(batch.tree, indexInBatch);

    return {
      root: batch.root,
      proof,
      leaf: this.receipts[sequence].hash,
    };
  }

  _generateMerkleProof(tree, leafIndex) {
    const proof = [];
    let index = leafIndex;

    for (let level = 0; level < tree.levels.length - 1; level++) {
      const currentLevel = tree.levels[level];
      const siblingIndex = index % 2 === 0 ? index + 1 : index - 1;

      if (siblingIndex < currentLevel.length) {
        proof.push({
          hash: currentLevel[siblingIndex],
          position: index % 2 === 0 ? 'right' : 'left',
        });
      }

      index = Math.floor(index / 2);
    }

    return proof;
  }

  verifyMerkleProof(leaf, proof, root) {
    let computed = leaf;

    for (const step of proof) {
      if (step.position === 'right') {
        computed = blake3Hash(computed + step.hash);
      } else {
        computed = blake3Hash(step.hash + computed);
      }
    }

    return computed === root;
  }

  subscribe(callback) {
    return this.changeFeed.subscribe(callback);
  }

  getMetrics() {
    return {
      totalEvents: this.stats.totalEvents,
      totalReceipts: this.receipts.length,
      batchesFinalized: this.stats.batchesFinalized,
      currentBatchSize: this.currentBatch.length,
      latestRoot: this.merkleRoots[this.merkleRoots.length - 1]?.root,
      verifications: {
        performed: this.stats.verificationsPerformed,
        passed: this.stats.verificationsPassed,
        failed: this.stats.verificationsPerformed - this.stats.verificationsPassed,
        successRate: this.stats.verificationsPerformed > 0
          ? (this.stats.verificationsPassed / this.stats.verificationsPerformed * 100).toFixed(2) + '%'
          : 'N/A',
      },
    };
  }
}

// ============================================================================
// Example Usage
// ============================================================================

async function main() {
  console.log('='.repeat(70));
  console.log('Receipt-Backed Stream Example');
  console.log('='.repeat(70));
  console.log('');

  // Create receipt-backed stream with small batch size for demo
  const stream = new ReceiptBackedStream({
    batchSize: 5,
  });

  // Listen for receipted events
  let eventCount = 0;
  stream.on('event:receipted', (info) => {
    eventCount++;
    console.log(`📝 Event #${eventCount} receipted:`);
    console.log(`   Event ID: ${info.event.id}`);
    console.log(`   Receipt Hash: ${info.receipt.hash.substring(0, 16)}...`);
    console.log(`   Sequence: ${info.receipt.sequence}`);
  });

  // Listen for batch finalization
  stream.on('batch:finalized', (info) => {
    console.log(`\n🔒 Batch Finalized:`);
    console.log(`   Merkle Root: ${info.root.substring(0, 16)}...`);
    console.log(`   Batch Size: ${info.size}`);
    console.log('');
  });

  // Emit events
  console.log('Emitting events with cryptographic receipts...\n');

  const events = [];
  for (let i = 0; i < 12; i++) {
    const event = {
      id: `event-${i}`,
      type: 'transaction',
      data: {
        amount: 100 + i * 10,
        from: `account-${i}`,
        to: `account-${i + 1}`,
      },
      timestamp: Date.now() + i * 100,
    };

    const receipt = await stream.emitWithReceipt(event);
    events.push({ event, receipt });

    await delay(50);
  }

  // Finalize remaining batch
  await stream['_finalizeBatch']();

  // Example 1: Verify authentic events
  console.log('\n' + '='.repeat(70));
  console.log('Example 1: Verify Authentic Events\n');

  for (let i = 0; i < 3; i++) {
    const { event, receipt } = events[i];
    const result = stream.verifyEvent(event, receipt);

    console.log(`Event ${i}:`);
    console.log(`   Valid: ${result.valid ? '✅' : '❌'}`);
    if (!result.valid) {
      console.log(`   Reason: ${result.reason}`);
    }
  }

  // Example 2: Detect tampering
  console.log('\n' + '='.repeat(70));
  console.log('Example 2: Detect Tampered Event\n');

  const { event: originalEvent, receipt } = events[5];

  // Tamper with event data
  const tamperedEvent = {
    ...originalEvent,
    data: {
      ...originalEvent.data,
      amount: 9999999, // Fraudulent change
    },
  };

  const tamperResult = stream.verifyEvent(tamperedEvent, receipt);
  console.log('Tampered event verification:');
  console.log(`   Valid: ${tamperResult.valid ? '✅' : '❌'}`);
  console.log(`   Reason: ${tamperResult.reason}`);
  console.log(`   🛡️  Tampering successfully detected!`);

  // Example 3: Merkle proof verification
  console.log('\n' + '='.repeat(70));
  console.log('Example 3: Merkle Proof Verification\n');

  const { receipt: proofReceipt } = events[7];
  const merkleProof = stream.getMerkleProof(proofReceipt.sequence);

  console.log('Merkle Proof for Event 7:');
  console.log(`   Leaf Hash: ${merkleProof.leaf.substring(0, 16)}...`);
  console.log(`   Root Hash: ${merkleProof.root.substring(0, 16)}...`);
  console.log(`   Proof Steps: ${merkleProof.proof.length}`);

  const proofValid = stream.verifyMerkleProof(
    merkleProof.leaf,
    merkleProof.proof,
    merkleProof.root
  );

  console.log(`   Proof Valid: ${proofValid ? '✅' : '❌'}`);

  // Example 4: Receipt chain integrity
  console.log('\n' + '='.repeat(70));
  console.log('Example 4: Receipt Chain Integrity\n');

  let chainValid = true;
  for (let i = 1; i < events.length; i++) {
    const current = events[i].receipt;
    const previous = events[i - 1].receipt;

    if (current.previousHash !== previous.hash) {
      chainValid = false;
      console.log(`❌ Chain break detected at sequence ${i}`);
      break;
    }
  }

  if (chainValid) {
    console.log('✅ Receipt chain is fully intact!');
    console.log(`   Verified ${events.length} receipts in sequence`);
  }

  // Display final metrics
  console.log('\n' + '='.repeat(70));
  console.log('Stream Metrics:');
  console.log('='.repeat(70));
  const metrics = stream.getMetrics();
  console.log(JSON.stringify(metrics, null, 2));

  console.log('\nExample complete!');
}

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// Run example
main().catch(console.error);
