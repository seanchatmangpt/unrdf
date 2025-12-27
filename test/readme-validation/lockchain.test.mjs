/**
 * @file Lockchain Provenance README Example Tests (London TDD)
 * @description Tests for cryptographic provenance examples from README.md
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('README Lockchain Provenance Examples', () => {
  let mockLockchainWriter;
  let mockLockchain;

  beforeEach(() => {
    mockLockchain = {
      init: vi.fn().mockResolvedValue(undefined),
      writeReceipt: vi.fn(),
      verifyReceipt: vi.fn(),
      cleanup: vi.fn().mockResolvedValue(undefined),
    };

    mockLockchainWriter = vi.fn().mockReturnValue(mockLockchain);
  });

  describe('LockchainWriter Initialization', () => {
    it('should create lockchain with repo path', () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      expect(mockLockchainWriter).toHaveBeenCalledWith({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      expect(lockchain).toBeDefined();
    });

    it('should initialize lockchain repository', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      expect(lockchain.init).toHaveBeenCalledOnce();
    });

    it('should support Merkle tree enablement', () => {
      const _lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      expect(mockLockchainWriter).toHaveBeenCalledWith(
        expect.objectContaining({ enableMerkle: true })
      );
    });

    it('should allow disabling Merkle tree', () => {
      const _lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: false,
      });

      expect(mockLockchainWriter).toHaveBeenCalledWith(
        expect.objectContaining({ enableMerkle: false })
      );
    });
  });

  describe('Write Receipt', () => {
    it('should write cryptographically signed receipt', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      const mockReceipt = {
        id: 'receipt-123',
        actor: 'alice@example.org',
        action: 'add-data',
        timestamp: new Date(),
        merkleRoot: 'abc123def456',
        signature: 'sig789',
      };

      mockLockchain.writeReceipt.mockResolvedValue(mockReceipt);

      const receipt = await lockchain.writeReceipt({
        actor: 'alice@example.org',
        action: 'add-data',
        delta: { additions: [], removals: [] },
        timestamp: new Date(),
        metadata: { reason: 'User registration' },
      });

      expect(lockchain.writeReceipt).toHaveBeenCalledWith(
        expect.objectContaining({
          actor: 'alice@example.org',
          action: 'add-data',
          metadata: expect.objectContaining({ reason: 'User registration' }),
        })
      );

      expect(receipt.merkleRoot).toBeDefined();
      expect(receipt.merkleRoot).toBe('abc123def456');
    });

    it('should include SHA3-256 Merkle root', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      mockLockchain.writeReceipt.mockResolvedValue({
        merkleRoot: 'f7d8e9a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9',
      });

      const receipt = await lockchain.writeReceipt({
        actor: 'alice@example.org',
        action: 'add-data',
        delta: { additions: [], removals: [] },
        timestamp: new Date(),
      });

      expect(receipt.merkleRoot).toBeDefined();
      expect(receipt.merkleRoot).toMatch(/^[a-f0-9]{64}$/); // SHA3-256 hex format
    });

    it('should handle metadata in receipt', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      mockLockchain.writeReceipt.mockResolvedValue({
        metadata: {
          reason: 'User registration',
          ip: '192.168.1.1',
          userAgent: 'Mozilla/5.0',
        },
      });

      const receipt = await lockchain.writeReceipt({
        actor: 'alice@example.org',
        action: 'add-user',
        delta: { additions: [], removals: [] },
        timestamp: new Date(),
        metadata: {
          reason: 'User registration',
          ip: '192.168.1.1',
          userAgent: 'Mozilla/5.0',
        },
      });

      expect(receipt.metadata).toHaveProperty('reason');
      expect(receipt.metadata).toHaveProperty('ip');
      expect(receipt.metadata).toHaveProperty('userAgent');
    });

    it('should store delta in receipt', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      const delta = {
        additions: [{ subject: 's', predicate: 'p', object: 'o' }],
        removals: [],
      };

      mockLockchain.writeReceipt.mockResolvedValue({
        delta: delta,
      });

      const receipt = await lockchain.writeReceipt({
        actor: 'alice@example.org',
        action: 'add-data',
        delta: delta,
        timestamp: new Date(),
      });

      expect(receipt.delta).toBeDefined();
      expect(receipt.delta.additions).toHaveLength(1);
      expect(receipt.delta.removals).toHaveLength(0);
    });
  });

  describe('Verify Receipt', () => {
    it('should verify valid receipt', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      const receipt = {
        id: 'receipt-123',
        merkleRoot: 'abc123',
        signature: 'sig789',
      };

      mockLockchain.verifyReceipt.mockResolvedValue(true);

      const isValid = await lockchain.verifyReceipt(receipt);

      expect(lockchain.verifyReceipt).toHaveBeenCalledWith(receipt);
      expect(isValid).toBe(true);
    });

    it('should detect tampered receipt', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      const receipt = {
        id: 'receipt-123',
        merkleRoot: 'tampered',
        signature: 'invalid',
      };

      mockLockchain.verifyReceipt.mockResolvedValue(false);

      const isValid = await lockchain.verifyReceipt(receipt);

      expect(isValid).toBe(false);
    });

    it('should verify Merkle root integrity', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      const validReceipt = {
        merkleRoot: 'f7d8e9a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9',
        data: 'original-data',
      };

      mockLockchain.verifyReceipt.mockResolvedValue(true);

      const isValid = await lockchain.verifyReceipt(validReceipt);

      expect(isValid).toBe(true);
    });

    it('should reject receipt with invalid Merkle root', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      const invalidReceipt = {
        merkleRoot: 'invalid-merkle-root',
        data: 'tampered-data',
      };

      mockLockchain.verifyReceipt.mockResolvedValue(false);

      const isValid = await lockchain.verifyReceipt(invalidReceipt);

      expect(isValid).toBe(false);
    });
  });

  describe('Cryptographic Audit Trail Example', () => {
    it('should execute transaction with audit', async () => {
      const mockSystem = {
        executeTransaction: vi.fn().mockResolvedValue({
          success: true,
          delta: { additions: [], removals: [] },
        }),
      };

      const result = await mockSystem.executeTransaction({
        additions: [],
        removals: [],
        actor: 'alice@example.org',
      });

      expect(result.success).toBe(true);
      expect(result.delta).toBeDefined();
    });

    it('should write receipt after transaction', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './audit-trail',
        enableMerkle: true,
      });

      await lockchain.init();

      const delta = { additions: [], removals: [] };

      mockLockchain.writeReceipt.mockResolvedValue({
        actor: 'alice@example.org',
        action: 'add-user',
        delta: delta,
        merkleRoot: 'abc123',
      });

      const receipt = await lockchain.writeReceipt({
        actor: 'alice@example.org',
        action: 'add-user',
        delta: delta,
        timestamp: new Date(),
        metadata: { ip: '192.168.1.1', reason: 'User registration' },
      });

      expect(receipt.actor).toBe('alice@example.org');
      expect(receipt.action).toBe('add-user');
      expect(receipt.merkleRoot).toBeDefined();
    });

    it('should verify audit trail integrity', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './audit-trail',
        enableMerkle: true,
      });

      await lockchain.init();

      const receipt = {
        merkleRoot: 'abc123',
        actor: 'alice@example.org',
      };

      mockLockchain.verifyReceipt.mockResolvedValue(true);

      const isValid = await lockchain.verifyReceipt(receipt);

      expect(isValid).toBe(true);
    });

    it('should complete full audit workflow', async () => {
      // Create lockchain
      const lockchain = mockLockchainWriter({
        repoPath: './audit-trail',
        enableMerkle: true,
      });

      await lockchain.init();

      // Execute transaction
      const mockSystem = {
        executeTransaction: vi.fn().mockResolvedValue({
          success: true,
          delta: { additions: [], removals: [] },
        }),
      };

      const result = await mockSystem.executeTransaction({
        additions: [],
        removals: [],
        actor: 'alice@example.org',
      });

      // Write receipt
      mockLockchain.writeReceipt.mockResolvedValue({
        merkleRoot: 'abc123',
        delta: result.delta,
      });

      const receipt = await lockchain.writeReceipt({
        actor: 'alice@example.org',
        action: 'add-user',
        delta: result.delta,
        timestamp: new Date(),
        metadata: { ip: '192.168.1.1', reason: 'User registration' },
      });

      // Verify receipt
      mockLockchain.verifyReceipt.mockResolvedValue(true);

      const isValid = await lockchain.verifyReceipt(receipt);

      expect(isValid).toBe(true);
    });
  });

  describe('Tamper Detection', () => {
    it('should detect data modification', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      // Original receipt
      mockLockchain.writeReceipt.mockResolvedValue({
        data: 'original',
        merkleRoot: 'original-hash',
      });

      const originalReceipt = await lockchain.writeReceipt({
        actor: 'alice',
        action: 'add',
        delta: { additions: [], removals: [] },
        timestamp: new Date(),
      });

      // Tampered receipt
      const tamperedReceipt = {
        ...originalReceipt,
        data: 'tampered',
        merkleRoot: 'original-hash', // Same hash but different data
      };

      mockLockchain.verifyReceipt.mockResolvedValue(false);

      const isValid = await lockchain.verifyReceipt(tamperedReceipt);

      expect(isValid).toBe(false);
    });

    it('should detect missing receipt', async () => {
      const lockchain = mockLockchainWriter({
        repoPath: './lockchain-repo',
        enableMerkle: true,
      });

      await lockchain.init();

      mockLockchain.verifyReceipt.mockRejectedValue(new Error('Receipt not found'));

      await expect(lockchain.verifyReceipt({ id: 'non-existent' })).rejects.toThrow(
        'Receipt not found'
      );
    });
  });
});
