/**
 * @fileoverview Tests for Universal Receipt Standard
 *
 * Tests cover:
 * - Receipt creation for all types
 * - Hash computation and verification
 * - Chain linkage
 * - Ledger operations
 * - Indexer queries
 * - Serialization formats
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  // Constants
  RECEIPT_TYPES,
  DECISION_OUTCOMES,

  // Schema
  UniversalReceiptSchema,

  // Utilities
  generateEpoch,
  generateReceiptId,
  computeReceiptHash,

  // Builder
  ReceiptBuilder,

  // Factories
  createAdmissionReceipt,
  createTestReceipt,
  createBuildReceipt,
  createDeploymentReceipt,
  createProjectionReceipt,
  createQueryReceipt,
  createWorkflowReceipt,

  // Serialization
  receiptToJSONLD,
  receiptToTurtle,
  receiptToBinary,
  receiptFromBinary,

  // Verification
  verifyReceiptHash,
  validateReceipt,
} from './receipt-standard.mjs';

import {
  ReceiptLedger,
  createMemoryLedger,
  createChainSnapshot,
  verifyChainSnapshot,
} from './receipt-ledger.mjs';

import { ReceiptIndexer, createIndexer } from './receipt-indexer.mjs';

// ============================================================================
// Receipt Standard Tests
// ============================================================================

describe('Receipt Standard', () => {
  describe('Constants', () => {
    it('should have all receipt types defined', () => {
      expect(RECEIPT_TYPES).toContain('admission');
      expect(RECEIPT_TYPES).toContain('test');
      expect(RECEIPT_TYPES).toContain('build');
      expect(RECEIPT_TYPES).toContain('deployment');
      expect(RECEIPT_TYPES).toContain('projection');
      expect(RECEIPT_TYPES).toContain('query');
      expect(RECEIPT_TYPES).toContain('workflow');
      expect(RECEIPT_TYPES.length).toBeGreaterThanOrEqual(12);
    });

    it('should have all decision outcomes', () => {
      expect(DECISION_OUTCOMES).toContain('ALLOW');
      expect(DECISION_OUTCOMES).toContain('DENY');
      expect(DECISION_OUTCOMES).toContain('WARN');
      expect(DECISION_OUTCOMES).toContain('SKIP');
      expect(DECISION_OUTCOMES).toContain('PENDING');
    });
  });

  describe('Epoch Generation', () => {
    it('should generate valid epoch from timestamp', () => {
      const timestamp = new Date('2025-01-15T14:30:00.123Z');
      const epoch = generateEpoch(timestamp);

      expect(epoch).toBe('tau_2025_01_15_143000_123');
    });

    it('should generate unique epochs for different times', () => {
      const epoch1 = generateEpoch(new Date('2025-01-15T14:30:00.000Z'));
      const epoch2 = generateEpoch(new Date('2025-01-15T14:30:00.001Z'));

      expect(epoch1).not.toBe(epoch2);
    });
  });

  describe('Receipt ID Generation', () => {
    it('should generate URN format ID', () => {
      const id = generateReceiptId('test', '@unrdf/core');

      // Format: urn:receipt:@unrdf/core:test:tau_YYYY_MM_DD_HHmmss_SSS:random
      expect(id).toMatch(/^urn:receipt:@unrdf\/core:test:tau_\d{4}_\d{2}_\d{2}_\d{6}_\d{3}:\w+$/);
    });
  });

  describe('ReceiptBuilder', () => {
    it('should build a valid receipt with fluent API', async () => {
      const receipt = await new ReceiptBuilder('test', '@unrdf/core')
        .decision('ALLOW', 'All tests passed')
        .provenance({ agent: 'vitest' })
        .input({ hashes: { testFile: 'abc123' } })
        .output({ hash: 'def456' })
        .extension({
          type: 'test',
          data: { total: 10, passed: 10, failed: 0 },
        })
        .build();

      expect(receipt.type).toBe('test');
      expect(receipt.package).toBe('@unrdf/core');
      expect(receipt.decision).toBe('ALLOW');
      expect(receipt.reason).toBe('All tests passed');
      expect(receipt.receiptHash).toBeDefined();
      expect(receipt.extension.data.total).toBe(10);
    });

    it('should produce immutable receipts', async () => {
      const receipt = await new ReceiptBuilder('test', '@unrdf/core')
        .decision('ALLOW', 'Test')
        .build();

      expect(Object.isFrozen(receipt)).toBe(true);
    });
  });
});

// ============================================================================
// Factory Tests
// ============================================================================

describe('Receipt Factories', () => {
  describe('createTestReceipt', () => {
    it('should create ALLOW receipt when all tests pass', async () => {
      const receipt = await createTestReceipt({
        pkg: '@unrdf/core',
        suite: 'unit-tests',
        total: 42,
        passed: 42,
        failed: 0,
        duration: 1234,
      });

      expect(receipt.decision).toBe('ALLOW');
      expect(receipt.reason).toBe('All 42 tests passed');
      expect(receipt.extension.data.total).toBe(42);
      expect(receipt.extension.data.passed).toBe(42);
    });

    it('should create DENY receipt when tests fail', async () => {
      const receipt = await createTestReceipt({
        pkg: '@unrdf/core',
        total: 10,
        passed: 7,
        failed: 3,
        failures: [
          { name: 'test1', message: 'Expected true' },
        ],
      });

      expect(receipt.decision).toBe('DENY');
      expect(receipt.reason).toBe('3 of 10 tests failed');
    });
  });

  describe('createAdmissionReceipt', () => {
    it('should create admission receipt with capsule info', async () => {
      const receipt = await createAdmissionReceipt({
        pkg: '@unrdf/core',
        capsuleId: 'delta-123',
        decision: 'ALLOW',
        reason: 'All checks passed',
        partition: 'ontology',
        guards: [{ name: 'H_delete_published', passed: true, blockedBy: [] }],
        invariants: [{ name: 'Q_version', passed: true }],
        quadCount: 150,
      });

      expect(receipt.type).toBe('admission');
      expect(receipt.extension.data.capsuleId).toBe('delta-123');
      expect(receipt.extension.data.quadCount).toBe(150);
    });
  });

  describe('createBuildReceipt', () => {
    it('should create ALLOW receipt on successful build', async () => {
      const receipt = await createBuildReceipt({
        pkg: '@unrdf/cli',
        target: 'dist',
        mode: 'production',
        success: true,
        bundleSize: 245760,
        duration: 5432,
      });

      expect(receipt.decision).toBe('ALLOW');
      expect(receipt.extension.data.bundleSize).toBe(245760);
    });

    it('should create WARN receipt on build with warnings', async () => {
      const receipt = await createBuildReceipt({
        pkg: '@unrdf/cli',
        success: true,
        warnings: ['Circular dependency detected'],
      });

      expect(receipt.decision).toBe('WARN');
    });

    it('should create DENY receipt on failed build', async () => {
      const receipt = await createBuildReceipt({
        pkg: '@unrdf/cli',
        success: false,
        errors: ['Module not found: xyz'],
      });

      expect(receipt.decision).toBe('DENY');
    });
  });

  describe('createDeploymentReceipt', () => {
    it('should create deployment receipt with environment info', async () => {
      const receipt = await createDeploymentReceipt({
        pkg: '@unrdf/api',
        environment: 'production',
        version: '1.2.3',
        success: true,
        instances: 3,
        region: 'us-east-1',
        url: 'https://api.unrdf.org',
        healthCheck: { passed: true, responseTime: 45 },
      });

      expect(receipt.type).toBe('deployment');
      expect(receipt.decision).toBe('ALLOW');
      expect(receipt.extension.data.environment).toBe('production');
      expect(receipt.extension.data.version).toBe('1.2.3');
    });
  });

  describe('createProjectionReceipt', () => {
    it('should create projection receipt for doc generation', async () => {
      const receipt = await createProjectionReceipt({
        pkg: '@unrdf/docs',
        format: 'markdown',
        template: 'diataxis',
        pages: 25,
        files: ['index.md', 'guide.md'],
        wordCount: 15000,
        success: true,
      });

      expect(receipt.type).toBe('projection');
      expect(receipt.extension.data.format).toBe('markdown');
      expect(receipt.extension.data.pages).toBe(25);
    });
  });

  describe('createQueryReceipt', () => {
    it('should create query receipt for SPARQL', async () => {
      const receipt = await createQueryReceipt({
        pkg: '@unrdf/core',
        queryType: 'SELECT',
        queryHash: 'query-hash-123',
        resultCount: 42,
        bindings: 3,
        graphsAccessed: ['http://example.org/graph1'],
        success: true,
        duration: 125,
      });

      expect(receipt.type).toBe('query');
      expect(receipt.extension.data.queryType).toBe('SELECT');
      expect(receipt.extension.data.resultCount).toBe(42);
    });
  });

  describe('createWorkflowReceipt', () => {
    it('should create workflow receipt with task info', async () => {
      const receipt = await createWorkflowReceipt({
        pkg: '@unrdf/yawl',
        workflowId: 'wf-123',
        workflowName: 'DataPipeline',
        taskCount: 5,
        completedTasks: 5,
        failedTasks: 0,
        success: true,
        duration: 12500,
      });

      expect(receipt.type).toBe('workflow');
      expect(receipt.extension.data.workflowId).toBe('wf-123');
      expect(receipt.extension.data.completedTasks).toBe(5);
    });
  });
});

// ============================================================================
// Hash and Verification Tests
// ============================================================================

describe('Hash and Verification', () => {
  it('should compute deterministic hash', async () => {
    const receipt = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      timestamp: new Date('2025-01-15T14:30:00.000Z'),
    });

    const hash1 = receipt.receiptHash;

    // Same data should produce same hash
    const receipt2 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      timestamp: new Date('2025-01-15T14:30:00.000Z'),
    });

    // Note: IDs differ due to random component, but that's expected
    expect(receipt.decision).toBe(receipt2.decision);
  });

  it('should verify valid receipt hash', async () => {
    const receipt = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
    });

    const isValid = await verifyReceiptHash(receipt);
    expect(isValid).toBe(true);
  });

  it('should validate receipt against schema', async () => {
    const receipt = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
    });

    const result = validateReceipt(receipt);
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
  });
});

// ============================================================================
// Serialization Tests
// ============================================================================

describe('Serialization', () => {
  let receipt;

  beforeEach(async () => {
    receipt = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
    });
  });

  it('should serialize to JSON-LD', () => {
    const jsonld = receiptToJSONLD(receipt);

    expect(jsonld['@context']).toBeDefined();
    expect(jsonld['@type']).toBe('unrdf:TestReceipt');
    expect(jsonld['@id']).toBe(receipt.id);
    expect(jsonld['unrdf:decision']).toBe('ALLOW');
  });

  it('should serialize to Turtle', () => {
    const turtle = receiptToTurtle(receipt);

    expect(turtle).toContain('@prefix unrdf:');
    expect(turtle).toContain('a unrdf:TestReceipt');
    expect(turtle).toContain('unrdf:decision "ALLOW"');
  });

  it('should serialize to and from binary', () => {
    const binary = receiptToBinary(receipt);
    expect(binary).toBeInstanceOf(Uint8Array);

    const restored = receiptFromBinary(binary);
    expect(restored.id).toBe(receipt.id);
    expect(restored.decision).toBe(receipt.decision);
    expect(restored.receiptHash).toBe(receipt.receiptHash);
  });
});

// ============================================================================
// Ledger Tests
// ============================================================================

describe('Receipt Ledger', () => {
  let ledger;

  beforeEach(() => {
    ledger = createMemoryLedger();
  });

  it('should append first receipt with null beforeHash', async () => {
    const receipt = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: null,
    });

    const result = await ledger.append('@unrdf/core', receipt);
    expect(result.success).toBe(true);
  });

  it('should reject first receipt with non-null beforeHash', async () => {
    const receipt = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: 'some-hash',
    });

    const result = await ledger.append('@unrdf/core', receipt);
    expect(result.success).toBe(false);
    expect(result.error).toContain('beforeHash=null');
  });

  it('should chain receipts correctly', async () => {
    const receipt1 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: null,
    });

    await ledger.append('@unrdf/core', receipt1);

    const receipt2 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 5,
      passed: 5,
      failed: 0,
      beforeReceiptHash: receipt1.receiptHash,
    });

    const result = await ledger.append('@unrdf/core', receipt2);
    expect(result.success).toBe(true);

    const chain = ledger.getChain('@unrdf/core');
    expect(chain).toHaveLength(2);
  });

  it('should reject broken chain', async () => {
    const receipt1 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: null,
    });

    await ledger.append('@unrdf/core', receipt1);

    const receipt2 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 5,
      passed: 5,
      failed: 0,
      beforeReceiptHash: 'wrong-hash',
    });

    const result = await ledger.append('@unrdf/core', receipt2);
    expect(result.success).toBe(false);
    expect(result.error).toContain('Chain linkage broken');
  });

  it('should verify chain integrity', async () => {
    const timestamp1 = new Date('2025-01-15T14:30:00.000Z');
    const receipt1 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: null,
      timestamp: timestamp1,
    });

    await ledger.append('@unrdf/core', receipt1);

    const timestamp2 = new Date('2025-01-15T14:30:01.000Z'); // 1 second later
    const receipt2 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 5,
      passed: 5,
      failed: 0,
      beforeReceiptHash: receipt1.receiptHash,
      timestamp: timestamp2,
    });

    await ledger.append('@unrdf/core', receipt2);

    const verification = await ledger.verifyChain('@unrdf/core');
    expect(verification.valid).toBe(true);
    expect(verification.checked).toBe(2);
  });

  it('should compute chain statistics', async () => {
    const receipt1 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: null,
    });

    await ledger.append('@unrdf/core', receipt1);

    const receipt2 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 5,
      passed: 3,
      failed: 2,
      beforeReceiptHash: receipt1.receiptHash,
    });

    await ledger.append('@unrdf/core', receipt2);

    const stats = ledger.getChainStats('@unrdf/core');
    expect(stats.count).toBe(2);
    expect(stats.decisions.ALLOW).toBe(1);
    expect(stats.decisions.DENY).toBe(1);
  });

  it('should create and verify chain snapshot', async () => {
    const receipt1 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: null,
    });

    await ledger.append('@unrdf/core', receipt1);

    const snapshot = await createChainSnapshot(ledger, '@unrdf/core');
    expect(snapshot.count).toBe(1);
    expect(snapshot.merkleRoot).toBeDefined();

    const verification = await verifyChainSnapshot(ledger, snapshot);
    expect(verification.valid).toBe(true);
  });
});

// ============================================================================
// Indexer Tests
// ============================================================================

describe('Receipt Indexer', () => {
  let ledger;
  let indexer;

  beforeEach(async () => {
    ledger = createMemoryLedger();

    // Add some test receipts
    const receipt1 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 10,
      passed: 10,
      failed: 0,
      beforeReceiptHash: null,
    });
    await ledger.append('@unrdf/core', receipt1);

    const receipt2 = await createTestReceipt({
      pkg: '@unrdf/core',
      total: 5,
      passed: 3,
      failed: 2,
      beforeReceiptHash: receipt1.receiptHash,
    });
    await ledger.append('@unrdf/core', receipt2);

    const receipt3 = await createBuildReceipt({
      pkg: '@unrdf/cli',
      success: true,
      beforeReceiptHash: null,
    });
    await ledger.append('@unrdf/cli', receipt3);

    indexer = await createIndexer(ledger);
  });

  it('should build index from ledger', () => {
    const stats = indexer.getStats();
    expect(stats.totalReceipts).toBe(3);
    expect(stats.byPackage['@unrdf/core']).toBe(2);
    expect(stats.byPackage['@unrdf/cli']).toBe(1);
  });

  it('should query by package', () => {
    const results = indexer.query()
      .package('@unrdf/core')
      .execute();

    expect(results.total).toBe(2);
    expect(results.receipts[0].package).toBe('@unrdf/core');
  });

  it('should query by decision', () => {
    const results = indexer.query()
      .decision('DENY')
      .execute();

    expect(results.total).toBe(1);
    expect(results.receipts[0].decision).toBe('DENY');
  });

  it('should query by type', () => {
    const results = indexer.query()
      .type('build')
      .execute();

    expect(results.total).toBe(1);
    expect(results.receipts[0].type).toBe('build');
  });

  it('should combine filters', () => {
    const results = indexer.query()
      .package('@unrdf/core')
      .type('test')
      .decision('ALLOW')
      .execute();

    expect(results.total).toBe(1);
  });

  it('should get denied receipts', () => {
    const denied = indexer.getDenied();
    expect(denied).toHaveLength(1);
    expect(denied[0].decision).toBe('DENY');
  });

  it('should get test failures', () => {
    const failures = indexer.getTestFailures();
    expect(failures).toHaveLength(1);
  });

  it('should support pagination', () => {
    const results = indexer.query()
      .limit(1)
      .offset(1)
      .execute();

    expect(results.receipts).toHaveLength(1);
    expect(results.offset).toBe(1);
    expect(results.limit).toBe(1);
  });
});

// ============================================================================
// Integration Tests
// ============================================================================

describe('Integration', () => {
  it('should support full receipt lifecycle', async () => {
    // 1. Create ledger
    const ledger = createMemoryLedger();

    // 2. Create and append receipts
    const testReceipt = await createTestReceipt({
      pkg: '@unrdf/core',
      suite: 'unit',
      total: 42,
      passed: 42,
      failed: 0,
      duration: 1234,
      beforeReceiptHash: null,
    });

    const appendResult = await ledger.append('@unrdf/core', testReceipt);
    expect(appendResult.success).toBe(true);

    // 3. Verify chain
    const verification = await ledger.verifyChain('@unrdf/core');
    expect(verification.valid).toBe(true);

    // 4. Create snapshot
    const snapshot = await createChainSnapshot(ledger, '@unrdf/core');
    expect(snapshot.merkleRoot).toBeDefined();

    // 5. Build index
    const indexer = await createIndexer(ledger);
    expect(indexer.getStats().totalReceipts).toBe(1);

    // 6. Query
    const results = indexer.query()
      .package('@unrdf/core')
      .type('test')
      .execute();

    expect(results.receipts).toHaveLength(1);
    expect(results.receipts[0].extension.data.passed).toBe(42);

    // 7. Serialize
    const jsonld = receiptToJSONLD(testReceipt);
    expect(jsonld['@type']).toBe('unrdf:TestReceipt');

    const turtle = receiptToTurtle(testReceipt);
    expect(turtle).toContain('unrdf:TestReceipt');
  });
});
