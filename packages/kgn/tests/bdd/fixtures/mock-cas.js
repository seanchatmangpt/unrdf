/**
 * Mock Content-Addressed Storage for BDD Testing
 * London-style test double with realistic behavior
 */
import { vi } from 'vitest';
import crypto from 'crypto';

export function createMockCAS() {
  // Simulate in-memory storage
  const storage = new Map();
  const attestations = new Map();

  // Performance metrics simulation
  const metrics = {
    storageOperations: 0,
    retrievalOperations: 0,
    bytesStored: 0,
    bytesRetrieved: 0,
    cacheHits: 0,
    cacheMisses: 0
  };

  const mockCAS = {
    // Core storage operations
    store: vi.fn(async (content, options = {}) => {
      const hash = crypto.createHash('sha256').update(content).digest('hex');
      const size = Buffer.byteLength(content, 'utf8');

      // Simulate storage delay
      await new Promise(resolve => setTimeout(resolve, Math.random() * 10));

      storage.set(hash, {
        content,
        hash,
        size,
        storedAt: new Date(),
        metadata: options.metadata || {}
      });

      metrics.storageOperations++;
      metrics.bytesStored += size;

      // Create attestation if requested
      if (options.createAttestation) {
        const attestation = {
          contentHash: hash,
          timestamp: new Date(),
          metadata: options.attestationMetadata || {},
          signature: `mock-signature-${hash.slice(0, 8)}`
        };
        attestations.set(hash, attestation);
      }

      return {
        hash,
        size,
        stored: true,
        attestation: attestations.get(hash)
      };
    }),

    retrieve: vi.fn(async (hash) => {
      // Simulate retrieval delay
      await new Promise(resolve => setTimeout(resolve, Math.random() * 5));

      if (storage.has(hash)) {
        const item = storage.get(hash);
        metrics.retrievalOperations++;
        metrics.bytesRetrieved += item.size;
        metrics.cacheHits++;

        return {
          content: item.content,
          hash: item.hash,
          size: item.size,
          metadata: item.metadata,
          found: true
        };
      }

      metrics.cacheMisses++;
      return { found: false, hash };
    }),

    exists: vi.fn(async (hash) => {
      return storage.has(hash);
    }),

    delete: vi.fn(async (hash) => {
      const existed = storage.has(hash);
      storage.delete(hash);
      attestations.delete(hash);
      return { deleted: existed, hash };
    }),

    // Attestation operations
    attest: vi.fn(async (hash, attestationData) => {
      if (!storage.has(hash)) {
        throw new Error(`Content not found for hash: ${hash}`);
      }

      const attestation = {
        contentHash: hash,
        timestamp: new Date(),
        metadata: attestationData,
        signature: `mock-signature-${hash.slice(0, 8)}`,
        verified: true
      };

      attestations.set(hash, attestation);
      return attestation;
    }),

    getAttestation: vi.fn(async (hash) => {
      return attestations.get(hash) || null;
    }),

    verifyAttestation: vi.fn(async (hash) => {
      const attestation = attestations.get(hash);
      return {
        valid: !!attestation,
        attestation,
        verified: true
      };
    }),

    // Performance and metrics
    getMetrics: vi.fn(() => ({
      ...metrics,
      hitRate: metrics.cacheHits / (metrics.cacheHits + metrics.cacheMisses) || 0,
      storageEfficiency: metrics.bytesRetrieved / metrics.bytesStored || 0
    })),

    // Batch operations
    batchStore: vi.fn(async (items) => {
      const results = [];

      for (const { content, options = {} } of items) {
        const result = await mockCAS.store(content, options);
        results.push(result);
      }

      return results;
    }),

    batchRetrieve: vi.fn(async (hashes) => {
      const results = {};

      for (const hash of hashes) {
        results[hash] = await mockCAS.retrieve(hash);
      }

      return results;
    }),

    // Garbage collection simulation
    garbageCollect: vi.fn(async (referencedHashes = []) => {
      const allHashes = Array.from(storage.keys());
      const unreferencedHashes = allHashes.filter(hash => !referencedHashes.includes(hash));

      let removedCount = 0;
      let reclaimedBytes = 0;

      for (const hash of unreferencedHashes) {
        const item = storage.get(hash);
        reclaimedBytes += item.size;
        storage.delete(hash);
        attestations.delete(hash);
        removedCount++;
      }

      return {
        removedCount,
        reclaimedBytes,
        remainingCount: storage.size
      };
    }),

    // Deduplication simulation
    deduplicate: vi.fn(async () => {
      // Mock implementation - in real CAS, identical content would already be deduplicated
      const contentHashes = {};
      let duplicates = 0;

      for (const [hash, item] of storage.entries()) {
        const contentHash = crypto.createHash('md5').update(item.content).digest('hex');
        if (contentHashes[contentHash]) {
          duplicates++;
        } else {
          contentHashes[contentHash] = hash;
        }
      }

      return {
        totalItems: storage.size,
        uniqueContent: Object.keys(contentHashes).length,
        duplicatesFound: duplicates,
        deduplicationRatio: duplicates / storage.size || 0
      };
    }),

    // Storage health check
    healthCheck: vi.fn(async () => {
      return {
        status: 'healthy',
        itemCount: storage.size,
        attestationCount: attestations.size,
        metrics: mockCAS.getMetrics(),
        timestamp: new Date()
      };
    }),

    // Test utilities
    __reset: () => {
      storage.clear();
      attestations.clear();
      Object.keys(metrics).forEach(key => metrics[key] = 0);

      // Reset all mock call history
      Object.values(mockCAS).forEach(fn => {
        if (vi.isMockFunction(fn)) {
          fn.mockClear();
        }
      });
    },

    __getStorage: () => new Map(storage),
    __getAttestations: () => new Map(attestations),
    __getMetrics: () => ({ ...metrics }),

    // Simulate high-throughput scenarios
    __simulateHighThroughput: async (operationCount = 1000) => {
      const startTime = Date.now();
      const operations = [];

      for (let i = 0; i < operationCount; i++) {
        const content = `test-content-${i}`;
        operations.push(mockCAS.store(content));
      }

      await Promise.all(operations);

      const endTime = Date.now();
      const throughput = operationCount / ((endTime - startTime) / 1000);

      return {
        operationCount,
        duration: endTime - startTime,
        throughputOpsPerSecond: throughput,
        throughputMBPerSecond: (throughput * 20) / (1024 * 1024) // Assuming ~20 bytes per operation
      };
    }
  };

  return mockCAS;
}