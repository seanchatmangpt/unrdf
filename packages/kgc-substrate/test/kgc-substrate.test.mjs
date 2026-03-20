/**
 * @file Comprehensive tests for @unrdf/kgc-substrate package
 * @module @unrdf/kgc-substrate/test
 *
 * Tests ReceiptChain, TamperDetector, Allocator, Router, Workspace, and type validators.
 * External dependencies (KGCStore, freezeUniverse, GitBackbone, oxigraph, hash-wasm)
 * are mocked where needed.
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';

// Mock hash-wasm before imports
vi.mock('hash-wasm', () => ({
  sha256: vi.fn(async (input) => {
    // Deterministic mock: produce hex string based on input
    const { createHash } = await import('node:crypto');
    return createHash('sha256').update(input).digest('hex');
  }),
  blake3: vi.fn(async (input) => {
    const { createHash } = await import('node:crypto');
    return createHash('sha256').update(input).digest('hex');
  }),
}));

// Mock @unrdf/kgc-4d
vi.mock('@unrdf/kgc-4d', () => {
  const mockStore = {
    appendEvent: vi.fn().mockResolvedValue({
      receipt: { t_ns: '1000000000', id: 'mock-receipt-id' },
    }),
    match: vi.fn().mockReturnValue([]),
  };
  return {
    KGCStore: vi.fn().mockImplementation(() => mockStore),
    freezeUniverse: vi.fn().mockResolvedValue({
      t_ns: '2000000000',
      universe_hash: 'mock-universe-hash',
      git_ref: 'mock-git-ref',
      id: '00000000-0000-0000-0000-000000000001',
    }),
    GitBackbone: vi.fn().mockImplementation(() => ({})),
  };
});

// Mock @unrdf/oxigraph
vi.mock('@unrdf/oxigraph', () => ({
  dataFactory: {
    namedNode: vi.fn((value) => ({ value, termType: 'NamedNode' })),
    literal: vi.fn((value) => ({ value, termType: 'Literal' })),
  },
}));

import { ReceiptChain } from '../src/ReceiptChain.mjs';
import { TamperDetector } from '../src/TamperDetector.mjs';
import { allocate, remainingSlots, systemUtilization, waitlistDepth, validateAllocation } from '../src/Allocator.mjs';
import { parsePredicates, routeTask, validateConstraints, getRoutingStats } from '../src/Router.mjs';
import {
  validateStorageSnapshot,
  validateQueryPattern,
  validateTripleEntry,
  validateStateCommitment,
  StorageSnapshotSchema,
  QueryPatternSchema,
} from '../src/types.mjs';
import { KnowledgeStore } from '../src/KnowledgeStore.mjs';

// =============================================================================
// ReceiptChain Tests
// =============================================================================

describe('ReceiptChain', () => {
  /** @type {ReceiptChain} */
  let chain;

  beforeEach(() => {
    chain = new ReceiptChain({ enforce_monotonic_time: false });
  });

  it('should create receipt chain with default genesis hash', () => {
    // Act
    const c = new ReceiptChain();

    // Assert
    expect(c.getLength()).toBe(0);
    expect(c.getHeadHash()).toBe('0'.repeat(64));
    expect(c.genesis_hash).toBe(ReceiptChain.GENESIS_HASH);
  });

  it('should create receipt chain with custom genesis hash', () => {
    // Arrange
    const customGenesis = 'a'.repeat(64);

    // Act
    const c = new ReceiptChain({ genesis_hash: customGenesis });

    // Assert
    expect(c.genesis_hash).toBe(customGenesis);
    expect(c.getHeadHash()).toBe(customGenesis);
  });

  it('should append a block and increment length', async () => {
    // Arrange
    const blockData = {
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [{ type: 'code', path: 'src/foo.mjs', hash: 'abc123' + '0'.repeat(58), size_bytes: 512 }],
      timestamp_ns: 1000000000n,
    };

    // Act
    const result = await chain.append(blockData);

    // Assert
    expect(result.index).toBe(0);
    expect(result.block.agent_id).toBe('agent-1');
    expect(result.block.before_hash).toBe('0'.repeat(64));
    expect(result.block.after_hash).toHaveLength(64);
    expect(result.merkle_root).toHaveLength(64);
    expect(chain.getLength()).toBe(1);
  });

  it('should chain blocks with correct before_hash linkage', async () => {
    // Arrange & Act
    const result1 = await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    const result2 = await chain.append({
      agent_id: 'agent-2',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 2000000000n,
    });

    // Assert
    expect(result2.block.before_hash).toBe(result1.block.after_hash);
    expect(chain.getLength()).toBe(2);
    expect(chain.getHeadHash()).toBe(result2.block.after_hash);
  });

  it('should enforce monotonic timestamps when enabled', async () => {
    // Arrange
    const strictChain = new ReceiptChain({ enforce_monotonic_time: true });
    await strictChain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 2000000000n,
    });

    // Act & Assert
    await expect(
      strictChain.append({
        agent_id: 'agent-2',
        toolchain_version: '1.0.0',
        artifacts: [],
        timestamp_ns: 1000000000n, // Earlier timestamp
      })
    ).rejects.toThrow('Timestamp not monotonic');
  });

  it('should freeze appended blocks (immutable)', async () => {
    // Arrange
    const result = await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    // Act & Assert
    expect(() => {
      result.block.agent_id = 'tampered';
    }).toThrow(); // Object.freeze prevents mutation
  });

  it('should get block by index and return null for out of bounds', async () => {
    // Arrange
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    // Act & Assert
    expect(chain.getBlock(0)).not.toBeNull();
    expect(chain.getBlock(0).agent_id).toBe('agent-1');
    expect(chain.getBlock(1)).toBeNull();
    expect(chain.getBlock(-1)).toBeNull();
  });

  it('should serialize to JSON and deserialize back', async () => {
    // Arrange
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [{ type: 'test', path: 'test.mjs', hash: 'b'.repeat(64), size_bytes: 100 }],
      timestamp_ns: 1000000000n,
    });

    // Act
    const json = chain.toJSON();
    const restored = ReceiptChain.fromJSON(json);

    // Assert
    expect(restored.getLength()).toBe(1);
    expect(restored.getBlock(0).agent_id).toBe('agent-1');
    expect(restored.genesis_hash).toBe(chain.genesis_hash);
  });

  it('should serialize to base64 and deserialize back', async () => {
    // Arrange
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '2.0.0',
      artifacts: [],
      timestamp_ns: 5000000000n,
    });

    // Act
    const base64 = chain.toBase64();
    const restored = ReceiptChain.fromBase64(base64);

    // Assert
    expect(restored.getLength()).toBe(1);
    expect(restored.getBlock(0).toolchain_version).toBe('2.0.0');
  });

  it('should reject append with invalid agent_id', async () => {
    // Act & Assert
    await expect(
      chain.append({
        agent_id: '',
        toolchain_version: '1.0.0',
        artifacts: [],
        timestamp_ns: 1000000000n,
      })
    ).rejects.toThrow();
  });

  it('should reject append with non-array artifacts', async () => {
    // Act & Assert
    await expect(
      chain.append({
        agent_id: 'agent-1',
        toolchain_version: '1.0.0',
        artifacts: 'not-an-array',
        timestamp_ns: 1000000000n,
      })
    ).rejects.toThrow('artifacts must be an array');
  });
});

// =============================================================================
// TamperDetector Tests
// =============================================================================

describe('TamperDetector', () => {
  /** @type {TamperDetector} */
  let detector;

  beforeEach(() => {
    detector = new TamperDetector();
  });

  it('should verify a valid chain as untampered', async () => {
    // Arrange
    const chain = new ReceiptChain({ enforce_monotonic_time: false });
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });
    await chain.append({
      agent_id: 'agent-2',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 2000000000n,
    });

    // Act
    const result = await detector.verify(chain);

    // Assert
    expect(result.valid).toBe(true);
    expect(result.errors).toHaveLength(0);
    expect(result.details.blocks_checked).toBe(2);
    expect(result.details.chain_length).toBe(2);
  });

  it('should detect content hash tampering', async () => {
    // Arrange
    const chain = new ReceiptChain({ enforce_monotonic_time: false });
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    // Tamper with block by creating a modified JSON representation
    const json = chain.toJSON();
    json.blocks[0].after_hash = 'f'.repeat(64); // Tamper hash

    // Act
    const result = await detector.verify(json);

    // Assert
    expect(result.valid).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
    expect(result.errors.some(e => e.includes('content hash mismatch'))).toBe(true);
  });

  it('should detect chain linkage tampering (before_hash mismatch)', async () => {
    // Arrange
    const chain = new ReceiptChain({ enforce_monotonic_time: false });
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });
    await chain.append({
      agent_id: 'agent-2',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 2000000000n,
    });

    // Tamper chain linkage
    const json = chain.toJSON();
    json.blocks[1].before_hash = 'e'.repeat(64);

    // Act
    const result = await detector.verify(json);

    // Assert
    expect(result.valid).toBe(false);
    expect(result.errors.some(e => e.includes('before_hash mismatch'))).toBe(true);
  });

  it('should verify merkle proof for a specific block', async () => {
    // Arrange
    const chain = new ReceiptChain({ enforce_monotonic_time: false });
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    // Act
    const proof = await detector.verifyMerkleProof(chain, 0);

    // Assert
    expect(proof.valid).toBe(true);
    expect(proof.block_index).toBe(0);
    expect(proof.merkle_root).toHaveLength(64);
  });

  it('should return invalid for out-of-bounds merkle proof', async () => {
    // Arrange
    const chain = new ReceiptChain({ enforce_monotonic_time: false });

    // Act
    const proof = await detector.verifyMerkleProof(chain, 5);

    // Assert
    expect(proof.valid).toBe(false);
    expect(proof.error).toBe('Block index out of bounds');
  });

  it('should detect tampering between original and suspect chains', async () => {
    // Arrange
    const original = new ReceiptChain({ enforce_monotonic_time: false });
    await original.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    const suspect = new ReceiptChain({ enforce_monotonic_time: false });
    await suspect.append({
      agent_id: 'agent-TAMPERED',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    // Act
    const analysis = await detector.detectTampering(original, suspect);

    // Assert
    expect(analysis.tamper_type).not.toBe('none');
    expect(analysis.tampered_blocks.length).toBeGreaterThan(0);
  });

  it('should detect length mismatch between chains', async () => {
    // Arrange
    const original = new ReceiptChain({ enforce_monotonic_time: false });
    await original.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    const suspect = new ReceiptChain({ enforce_monotonic_time: false });

    // Act
    const analysis = await detector.detectTampering(original, suspect);

    // Assert
    expect(analysis.tamper_type).toBe('length_mismatch');
  });

  it('should generate a tamper report', async () => {
    // Arrange
    const chain = new ReceiptChain({ enforce_monotonic_time: false });
    await chain.append({
      agent_id: 'agent-1',
      toolchain_version: '1.0.0',
      artifacts: [],
      timestamp_ns: 1000000000n,
    });

    // Act
    const report = await detector.generateReport(chain);

    // Assert
    expect(report.chain_length).toBe(1);
    expect(report.verification.valid).toBe(true);
    expect(report.summary).toContain('Valid: true');
    expect(report.blocks_analyzed).toHaveLength(1);
    expect(report.blocks_analyzed[0].agent_id).toBe('agent-1');
  });
});

// =============================================================================
// Allocator Tests
// =============================================================================

describe('Allocator', () => {
  it('should allocate work items to agents via round-robin', () => {
    // Arrange
    const items = [
      { id: 'wi-001', type: 'implement' },
      { id: 'wi-002', type: 'test' },
    ];
    const agents = [
      { id: 'agent-1', maxConcurrent: 2, capabilities: [] },
    ];

    // Act
    const result = allocate(items, agents);

    // Assert
    expect(result.totalAssigned).toBe(2);
    expect(result.totalWaitlisted).toBe(0);
    expect(result.assignments).toHaveLength(2);
  });

  it('should respect agent capacity limits', () => {
    // Arrange
    const items = [
      { id: 'wi-001', type: 'a' },
      { id: 'wi-002', type: 'b' },
      { id: 'wi-003', type: 'c' },
    ];
    const agents = [
      { id: 'agent-1', maxConcurrent: 1, capabilities: [] },
    ];

    // Act
    const result = allocate(items, agents);

    // Assert
    expect(result.totalAssigned).toBe(1);
    expect(result.totalWaitlisted).toBe(2);
    expect(result.waitlist).toHaveLength(2);
  });

  it('should produce deterministic allocation (same input -> same output)', () => {
    // Arrange
    const items = [
      { id: 'wi-003', type: 'c' },
      { id: 'wi-001', type: 'a' },
      { id: 'wi-002', type: 'b' },
    ];
    const agents = [
      { id: 'agent-1', maxConcurrent: 2, capabilities: [] },
      { id: 'agent-2', maxConcurrent: 1, capabilities: [] },
    ];

    // Act
    const result1 = allocate(items, agents);
    const result2 = allocate(items, agents);

    // Assert
    expect(result1.assignments).toEqual(result2.assignments);
    expect(result1.waitlist).toEqual(result2.waitlist);
  });

  it('should sort items lexicographically by ID before allocation', () => {
    // Arrange
    const items = [
      { id: 'wi-003', type: 'c' },
      { id: 'wi-001', type: 'a' },
      { id: 'wi-002', type: 'b' },
    ];
    const agents = [
      { id: 'agent-1', maxConcurrent: 3, capabilities: [] },
    ];

    // Act
    const result = allocate(items, agents);

    // Assert - first assigned should be wi-001 (lexicographically first)
    expect(result.assignments[0].workItemId).toBe('wi-001');
    expect(result.assignments[1].workItemId).toBe('wi-002');
    expect(result.assignments[2].workItemId).toBe('wi-003');
  });

  it('should filter agents by capability requirements', () => {
    // Arrange
    const items = [
      { id: 'wi-001', type: 'review', requiredCapabilities: ['code-review'] },
    ];
    const agents = [
      { id: 'agent-1', maxConcurrent: 2, capabilities: ['backend'] },
      { id: 'agent-2', maxConcurrent: 1, capabilities: ['code-review'] },
    ];

    // Act
    const result = allocate(items, agents);

    // Assert
    expect(result.totalAssigned).toBe(1);
    expect(result.assignments[0].agentId).toBe('agent-2');
  });

  it('should calculate utilization per agent', () => {
    // Arrange
    const items = [
      { id: 'wi-001', type: 'a' },
      { id: 'wi-002', type: 'b' },
    ];
    const agents = [
      { id: 'agent-1', maxConcurrent: 2, capabilities: [] },
      { id: 'agent-2', maxConcurrent: 2, capabilities: [] },
    ];

    // Act
    const result = allocate(items, agents);

    // Assert
    expect(result.utilization['agent-1']).toBe(50);
    expect(result.utilization['agent-2']).toBe(50);
  });

  it('should calculate remaining slots correctly', () => {
    // Arrange
    const agents = [{ id: 'agent-1', maxConcurrent: 3, capabilities: [] }];
    const items = [{ id: 'wi-001', type: 'a' }];
    const result = allocate(items, agents);

    // Act
    const remaining = remainingSlots('agent-1', result, agents);

    // Assert
    expect(remaining).toBe(2);
  });

  it('should throw for unknown agent in remainingSlots', () => {
    // Arrange
    const result = allocate([], []);

    // Act & Assert
    expect(() => remainingSlots('unknown', result, [])).toThrow('Agent unknown not found');
  });

  it('should calculate system utilization', () => {
    // Arrange
    const items = [{ id: 'wi-001', type: 'a' }];
    const agents = [
      { id: 'agent-1', maxConcurrent: 2, capabilities: [] },
      { id: 'agent-2', maxConcurrent: 2, capabilities: [] },
    ];
    const result = allocate(items, agents);

    // Act
    const util = systemUtilization(result);

    // Assert
    expect(util).toBe(25); // 1 of 4 slots used
  });

  it('should return waitlist depth', () => {
    // Arrange
    const items = [
      { id: 'wi-001', type: 'a' },
      { id: 'wi-002', type: 'b' },
    ];
    const agents = [{ id: 'agent-1', maxConcurrent: 1, capabilities: [] }];
    const result = allocate(items, agents);

    // Act
    const depth = waitlistDepth(result);

    // Assert
    expect(depth).toBe(1);
  });

  it('should validate allocation correctness', () => {
    // Arrange
    const items = [
      { id: 'wi-001', type: 'a' },
      { id: 'wi-002', type: 'b' },
    ];
    const agents = [{ id: 'agent-1', maxConcurrent: 2, capabilities: [] }];
    const result = allocate(items, agents);

    // Act
    const validation = validateAllocation(items, agents, result);

    // Assert
    expect(validation.valid).toBe(true);
    expect(validation.errors).toHaveLength(0);
  });
});

// =============================================================================
// Router Tests
// =============================================================================

describe('Router', () => {
  it('should parse simple equality predicate', () => {
    // Act
    const predicates = parsePredicates("language=='js'");

    // Assert
    expect(predicates).toHaveLength(1);
    expect(predicates[0].field).toBe('language');
    expect(predicates[0].operator).toBe('==');
    expect(predicates[0].value).toBe('js');
  });

  it('should parse AND predicates', () => {
    // Act
    const predicates = parsePredicates("language=='js' AND requires_auth==true");

    // Assert
    expect(predicates).toHaveLength(2);
    expect(predicates[0].field).toBe('language');
    expect(predicates[1].field).toBe('requires_auth');
    expect(predicates[1].value).toBe(true);
  });

  it('should parse in operator with array', () => {
    // Act
    const predicates = parsePredicates("language in ['js','ts']");

    // Assert
    expect(predicates).toHaveLength(1);
    expect(predicates[0].operator).toBe('in');
    expect(predicates[0].value).toEqual(['js', 'ts']);
  });

  it('should return empty array for empty constraint', () => {
    // Act
    const predicates = parsePredicates('');

    // Assert
    expect(predicates).toEqual([]);
  });

  it('should throw on invalid predicate syntax', () => {
    // Act & Assert
    expect(() => parsePredicates('invalid syntax here')).toThrow('Invalid predicate syntax');
  });

  it('should route task to matching agent', () => {
    // Arrange
    const workItem = { id: 'task-1', predicates: "language=='js'" };
    const agents = [
      { id: 'agent-1', capabilities: { language: 'py' } },
      { id: 'agent-2', capabilities: { language: 'js' } },
    ];

    // Act
    const result = routeTask(workItem, agents);

    // Assert
    expect(result).toBe('agent-2');
  });

  it('should return null when no agent matches', () => {
    // Arrange
    const workItem = { id: 'task-1', predicates: "language=='rust'" };
    const agents = [
      { id: 'agent-1', capabilities: { language: 'js' } },
    ];

    // Act
    const result = routeTask(workItem, agents);

    // Assert
    expect(result).toBeNull();
  });

  it('should route with empty predicates (matches any agent)', () => {
    // Arrange
    const workItem = { id: 'task-1', predicates: '' };
    const agents = [
      { id: 'agent-1', capabilities: { language: 'js' } },
    ];

    // Act
    const result = routeTask(workItem, agents);

    // Assert
    expect(result).toBe('agent-1');
  });

  it('should validate constraint strings', () => {
    // Act
    const valid = validateConstraints("language=='js' AND version>=2");
    const invalid = validateConstraints('totally broken !!!');

    // Assert
    expect(valid.valid).toBe(true);
    expect(valid.predicateCount).toBe(2);
    expect(invalid.valid).toBe(false);
    expect(invalid.error).toBeDefined();
  });

  it('should compute routing statistics', () => {
    // Arrange
    const items = [
      { id: 'task-1', predicates: "language=='js'" },
      { id: 'task-2', predicates: "language=='py'" },
      { id: 'task-3', predicates: "language=='rust'" },
    ];
    const agents = [
      { id: 'agent-1', capabilities: { language: 'js' } },
      { id: 'agent-2', capabilities: { language: 'py' } },
    ];

    // Act
    const stats = getRoutingStats(items, agents);

    // Assert
    expect(stats.totalItems).toBe(3);
    expect(stats.routed).toBe(2);
    expect(stats.unrouted).toBe(1);
    expect(stats.routingMap['agent-1']).toBe(1);
    expect(stats.routingMap['agent-2']).toBe(1);
  });

  it('should throw for invalid workItem input', () => {
    // Act & Assert
    expect(() => routeTask(null, [])).toThrow('Invalid workItem');
    expect(() => routeTask({ id: 'x', predicates: '' }, 'not-array')).toThrow('Invalid agents');
  });
});

// =============================================================================
// Type Validators Tests
// =============================================================================

describe('Type Validators', () => {
  it('should validate a correct StorageSnapshot', () => {
    // Arrange
    const snapshot = {
      epoch: 0,
      timestamp_ns: 1000000000n,
      quads_hash: 'abc123',
      commit_hash: 'def456',
      snapshot_id: '00000000-0000-0000-0000-000000000001',
      quad_count: 5,
    };

    // Act & Assert
    expect(() => validateStorageSnapshot(snapshot)).not.toThrow();
  });

  it('should reject invalid StorageSnapshot (negative epoch)', () => {
    // Arrange
    const snapshot = {
      epoch: -1,
      timestamp_ns: 1000000000n,
      quads_hash: 'abc',
      commit_hash: 'def',
      snapshot_id: '00000000-0000-0000-0000-000000000001',
    };

    // Act & Assert
    expect(() => validateStorageSnapshot(snapshot)).toThrow();
  });

  it('should validate a correct QueryPattern', () => {
    // Arrange
    const pattern = {
      subject: { value: 'http://ex.org/s1' },
      predicate: null,
      object: null,
    };

    // Act & Assert
    expect(() => validateQueryPattern(pattern)).not.toThrow();
  });

  it('should validate a correct StateCommitment', () => {
    // Arrange
    const commitment = {
      state_hash: 'abc123def',
      log_index: 5n,
      timestamp_ns: 2000000000n,
      quad_count: 10,
    };

    // Act & Assert
    expect(() => validateStateCommitment(commitment)).not.toThrow();
  });

  it('should validate a correct TripleEntry', () => {
    // Arrange
    const entry = {
      index: 0n,
      timestamp_ns: 1000000000n,
      operation: 'add',
      subject: { value: 'http://ex.org/s' },
      predicate: { value: 'http://ex.org/p' },
      object: { value: 'http://ex.org/o' },
    };

    // Act & Assert
    expect(() => validateTripleEntry(entry)).not.toThrow();
  });

  it('should reject TripleEntry with invalid operation', () => {
    // Arrange
    const entry = {
      index: 0n,
      timestamp_ns: 1000000000n,
      operation: 'update', // invalid
      subject: {},
      predicate: {},
      object: {},
    };

    // Act & Assert
    expect(() => validateTripleEntry(entry)).toThrow();
  });
});

// =============================================================================
// KnowledgeStore Tests (with mocked dependencies)
// =============================================================================

describe('KnowledgeStore', () => {
  /** @type {KnowledgeStore} */
  let store;

  beforeEach(() => {
    store = new KnowledgeStore({ nodeId: 'test-node' });
  });

  it('should create store with specified nodeId', () => {
    // Assert
    expect(store.getNodeId()).toBe('test-node');
    expect(store.getLogIndex()).toBe(0n);
    expect(store.getEpoch()).toBe(0);
  });

  it('should create store with auto-generated nodeId', () => {
    // Act
    const autoStore = new KnowledgeStore();

    // Assert
    expect(autoStore.getNodeId()).toMatch(/^ks-/);
  });

  it('should throw for invalid options', () => {
    // Act & Assert
    expect(() => new KnowledgeStore('bad')).toThrow('options must be an object');
  });

  it('should append a triple and increment log index', async () => {
    // Arrange
    const subject = { value: 'http://ex.org/s' };
    const predicate = { value: 'http://ex.org/p' };
    const object = { value: 'http://ex.org/o' };

    // Act
    const result = await store.appendTriple('add', subject, predicate, object);

    // Assert
    expect(result.index).toBe(0n);
    expect(typeof result.timestamp_ns).toBe('bigint');
    expect(store.getLogIndex()).toBe(1n);
  });

  it('should reject appendTriple with invalid operation', async () => {
    // Arrange
    const subject = { value: 'http://ex.org/s' };
    const predicate = { value: 'http://ex.org/p' };
    const object = { value: 'http://ex.org/o' };

    // Act & Assert
    await expect(
      store.appendTriple('update', subject, predicate, object)
    ).rejects.toThrow("operation must be 'add' or 'delete'");
  });

  it('should reject appendTriple with invalid subject', async () => {
    // Act & Assert
    await expect(
      store.appendTriple('add', null, { value: 'p' }, { value: 'o' })
    ).rejects.toThrow('subject must be a valid RDF term');
  });
});
