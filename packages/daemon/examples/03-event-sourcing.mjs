/**
 * @file Event Sourcing with KGC-4D Integration Example
 * @module examples/03-event-sourcing
 * @description Demonstrates daemon operations with KGC-4D style event sourcing:
 * - Operations generate receipts with execution proof
 * - Events are captured as immutable records
 * - Operation history can be replayed
 * - Cryptographic proofs validate execution
 *
 * This example shows how daemon fits into an event-sourced architecture
 * where every operation is logged with cryptographic proof.
 */

import { Daemon } from '../src/daemon.mjs';
import crypto from 'crypto';

/**
 * Simple receipt generator (mimics KGC-4D style)
 * In production, use @unrdf/kgc-4d/receipts
 */
class ReceiptGenerator {
  constructor() {
    this.eventLog = [];
    this.hashChain = [];
  }

  /**
   * Generate receipt for operation execution
   */
  generateReceipt(operationId, operationType, result, duration) {
    const timestamp = new Date();
    const receiptId = crypto.randomUUID();

    // Create proof hash from operation data
    const proofData = JSON.stringify({
      operationId,
      operationType,
      timestamp: timestamp.toISOString(),
      duration,
      resultHash: this.hashValue(result),
    });

    const hash = crypto.createHash('sha256')
      .update(proofData)
      .digest('hex');

    const receipt = {
      id: receiptId,
      operationId,
      operationType,
      status: 'success',
      startedAt: new Date(timestamp.getTime() - duration),
      completedAt: timestamp,
      duration,
      result,
      attempts: 1,
      proof: {
        hash,
        timestamp,
        chainHash: this.getChainHash(),
      },
      metadata: {
        version: '1.0.0',
        source: 'daemon',
      },
    };

    // Add to event log and chain
    this.eventLog.push(receipt);
    this.hashChain.push(hash);

    return receipt;
  }

  /**
   * Hash value to 64-char string
   */
  hashValue(value) {
    return crypto.createHash('sha256')
      .update(JSON.stringify(value))
      .digest('hex');
  }

  /**
   * Get chain hash (previous hash for merkle chain)
   */
  getChainHash() {
    if (this.hashChain.length === 0) {
      return '0'.repeat(64); // Genesis hash
    }
    return this.hashChain[this.hashChain.length - 1];
  }

  /**
   * Verify receipt integrity
   */
  verifyReceipt(receipt) {
    const proofData = JSON.stringify({
      operationId: receipt.operationId,
      operationType: receipt.operationType,
      timestamp: receipt.proof.timestamp.toISOString(),
      duration: receipt.duration,
      resultHash: this.hashValue(receipt.result),
    });

    const expectedHash = crypto.createHash('sha256')
      .update(proofData)
      .digest('hex');

    return expectedHash === receipt.proof.hash;
  }

  /**
   * Get audit trail (event log)
   */
  getAuditTrail() {
    return this.eventLog.map(receipt => ({
      id: receipt.id,
      operationId: receipt.operationId,
      type: receipt.operationType,
      timestamp: receipt.completedAt,
      duration: receipt.duration,
      status: receipt.status,
      proofHash: receipt.proof.hash,
    }));
  }
}

/**
 * Example: Event-sourced daemon with cryptographic receipts
 */
async function eventSourcingExample() {
  console.log('=== Event Sourcing with Daemon Example ===\n');

  const daemon = new Daemon({
    id: 'event-sourced-daemon',
    logger: console,
  });

  const receiptGen = new ReceiptGenerator();

  // Define operations that generate events
  const createUser = {
    id: 'create-user',
    name: 'Create User Account',
    handler: async () => {
      const userId = `user_${Date.now()}`;
      const result = {
        userId,
        email: `user+${userId}@example.com`,
        createdAt: new Date().toISOString(),
        status: 'active',
      };
      return result;
    },
  };

  const sendWelcomeEmail = {
    id: 'send-welcome-email',
    name: 'Send Welcome Email',
    handler: async () => {
      const result = {
        status: 'sent',
        recipient: 'user@example.com',
        templateId: 'welcome-v2',
        sentAt: new Date().toISOString(),
      };
      return result;
    },
  };

  const initializeUserStorage = {
    id: 'init-user-storage',
    name: 'Initialize User Storage',
    handler: async () => {
      const result = {
        status: 'initialized',
        storageId: `storage_${Date.now()}`,
        quota: 5368709120, // 5GB
        allocated: 0,
      };
      return result;
    },
  };

  // Track operations and generate receipts
  const operationReceipts = new Map();

  daemon.on('operation:success', (event) => {
    console.log(`✓ Operation completed: ${event.name}`);
  });

  daemon.on('operation:failure', (event) => {
    console.error(`✗ Operation failed: ${event.name} - ${event.error}`);
  });

  // Start daemon
  await daemon.start();
  console.log('📍 Daemon started with event sourcing\n');

  // Schedule operations
  daemon.schedule(createUser);
  daemon.schedule(sendWelcomeEmail);
  daemon.schedule(initializeUserStorage);

  console.log('📋 Scheduled Operations:');
  daemon.listOperations().forEach(op => {
    console.log(`  • ${op.name}`);
  });
  console.log('');

  // Execute workflow: Create User → Generate Receipt → Chain Event
  console.log('▶️  Executing Event-Sourced Workflow\n');
  console.log('Step 1: Create User\n');

  const startTime1 = Date.now();
  const createUserResult = await daemon.execute('create-user');
  const duration1 = Date.now() - startTime1;

  console.log(`Result: ${JSON.stringify(createUserResult, null, 2)}`);

  // Generate receipt with cryptographic proof
  const receipt1 = receiptGen.generateReceipt(
    'create-user',
    'user_created',
    createUserResult,
    duration1
  );

  console.log(`\n📜 Receipt Generated:`);
  console.log(`  ID: ${receipt1.id}`);
  console.log(`  Operation: ${receipt1.operationType}`);
  console.log(`  Proof Hash: ${receipt1.proof.hash.substring(0, 16)}...`);
  console.log(`  Chain Hash: ${receipt1.proof.chainHash.substring(0, 16)}...`);
  console.log(`  Verified: ${receiptGen.verifyReceipt(receipt1) ? '✓ Valid' : '✗ Invalid'}`);

  operationReceipts.set('create-user', receipt1);

  // Execute second operation
  await new Promise(resolve => setTimeout(resolve, 500));
  console.log('\nStep 2: Send Welcome Email\n');

  const startTime2 = Date.now();
  const emailResult = await daemon.execute('send-welcome-email');
  const duration2 = Date.now() - startTime2;

  console.log(`Result: ${JSON.stringify(emailResult, null, 2)}`);

  const receipt2 = receiptGen.generateReceipt(
    'send-welcome-email',
    'welcome_email_sent',
    emailResult,
    duration2
  );

  console.log(`\n📜 Receipt Generated:`);
  console.log(`  ID: ${receipt2.id}`);
  console.log(`  Operation: ${receipt2.operationType}`);
  console.log(`  Proof Hash: ${receipt2.proof.hash.substring(0, 16)}...`);
  console.log(`  Verified: ${receiptGen.verifyReceipt(receipt2) ? '✓ Valid' : '✗ Invalid'}`);

  operationReceipts.set('send-welcome-email', receipt2);

  // Execute third operation
  await new Promise(resolve => setTimeout(resolve, 500));
  console.log('\nStep 3: Initialize Storage\n');

  const startTime3 = Date.now();
  const storageResult = await daemon.execute('init-user-storage');
  const duration3 = Date.now() - startTime3;

  console.log(`Result: ${JSON.stringify(storageResult, null, 2)}`);

  const receipt3 = receiptGen.generateReceipt(
    'init-user-storage',
    'storage_initialized',
    storageResult,
    duration3
  );

  console.log(`\n📜 Receipt Generated:`);
  console.log(`  ID: ${receipt3.id}`);
  console.log(`  Operation: ${receipt3.operationType}`);
  console.log(`  Verified: ${receiptGen.verifyReceipt(receipt3) ? '✓ Valid' : '✗ Invalid'}`);

  operationReceipts.set('init-user-storage', receipt3);

  // Display audit trail
  console.log('\n📋 Complete Audit Trail (Event Log):');
  console.log('');
  const auditTrail = receiptGen.getAuditTrail();
  auditTrail.forEach((entry, idx) => {
    console.log(`[${idx + 1}] ${entry.timestamp.toISOString()}`);
    console.log(`    Operation: ${entry.type}`);
    console.log(`    Duration: ${entry.duration}ms`);
    console.log(`    Proof: ${entry.proofHash.substring(0, 16)}...`);
  });

  // Display daemon metrics
  console.log('\n📈 Daemon Metrics:');
  const metrics = daemon.getMetrics();
  console.log(`  Total Operations: ${metrics.totalOperations}`);
  console.log(`  Successful: ${metrics.successfulOperations}`);
  console.log(`  Total Duration: ${metrics.totalDuration.toFixed(0)}ms`);
  console.log(`  Average Duration: ${metrics.averageDuration.toFixed(2)}ms`);

  // Verify all receipts
  console.log('\n🔐 Receipt Verification:');
  let allValid = true;
  for (const [opId, receipt] of operationReceipts) {
    const valid = receiptGen.verifyReceipt(receipt);
    allValid = allValid && valid;
    console.log(`  ${receipt.operationType}: ${valid ? '✓ Valid' : '✗ Invalid'}`);
  }
  console.log(`\n  Overall Integrity: ${allValid ? '✓ All Verified' : '✗ Some Invalid'}`);

  // Demonstrate replay (can reconstruct state from audit log)
  console.log('\n🔄 Operation Replay Capability:');
  console.log('Event log can be used to:');
  console.log('  • Reconstruct execution history');
  console.log('  • Audit compliance (immutable record)');
  console.log('  • Debug failures (trace root cause)');
  console.log('  • Verify integrity (cryptographic proof)');
  console.log('  • Replay events (time-travel)');

  // Cleanup
  console.log('\n⏹️  Stopping daemon...');
  await daemon.stop();
  console.log('✓ Daemon stopped\n');
}

// Run example
await eventSourcingExample();
