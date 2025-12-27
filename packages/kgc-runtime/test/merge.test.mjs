/**
 * @fileoverview Tests for multi-agent capsule merge and conflict resolution
 */

import { describe, it, expect } from 'vitest';
import {
  shardMerge,
  mergeCapsules,
  ConflictDetector,
  ConflictResolver,
} from '../src/merge.mjs';

// ============================================================================
// Test Data Fixtures
// ============================================================================

const createCapsule = (id, o_hash, file_edits) => ({
  id,
  o_hash,
  file_edits,
  metadata: { created_at: new Date().toISOString() },
});

const createFileEdit = (file_path, line_start, line_end, content, operation = 'replace') => ({
  file_path,
  line_start,
  line_end,
  content,
  operation,
});

const defaultTotalOrder = {
  rules: [],
  default_rule: {
    strategy: 'earlier_wins',
  },
};

// ============================================================================
// ConflictDetector Tests
// ============================================================================

describe('ConflictDetector', () => {
  describe('rangesOverlap', () => {
    it('should detect overlapping ranges', () => {
      expect(ConflictDetector.rangesOverlap(1, 5, 3, 7)).toBe(true);
      expect(ConflictDetector.rangesOverlap(1, 5, 5, 10)).toBe(true);
      expect(ConflictDetector.rangesOverlap(3, 7, 1, 5)).toBe(true);
    });

    it('should detect non-overlapping ranges', () => {
      expect(ConflictDetector.rangesOverlap(1, 5, 6, 10)).toBe(false);
      expect(ConflictDetector.rangesOverlap(6, 10, 1, 5)).toBe(false);
    });

    it('should handle edge cases', () => {
      expect(ConflictDetector.rangesOverlap(1, 5, 1, 5)).toBe(true);
      expect(ConflictDetector.rangesOverlap(1, 1, 1, 1)).toBe(true);
    });
  });

  describe('detectConflicts', () => {
    it('should detect no conflicts when ranges do not overlap', () => {
      const capsules = [
        createCapsule('c1', 'hash1', [
          createFileEdit('file.js', 1, 5, 'content1'),
        ]),
        createCapsule('c2', 'hash2', [
          createFileEdit('file.js', 10, 15, 'content2'),
        ]),
      ];

      const conflicts = ConflictDetector.detectConflicts(capsules);
      expect(conflicts).toHaveLength(0);
    });

    it('should detect conflict when two capsules edit same file region', () => {
      const capsules = [
        createCapsule('c1', 'hash1', [
          createFileEdit('file.js', 1, 10, 'content1'),
        ]),
        createCapsule('c2', 'hash2', [
          createFileEdit('file.js', 5, 15, 'content2'),
        ]),
      ];

      const conflicts = ConflictDetector.detectConflicts(capsules);
      expect(conflicts).toHaveLength(1);
      expect(conflicts[0].file_path).toBe('file.js');
      expect(conflicts[0].overlapping_edits).toHaveLength(2);
    });

    it('should detect conflicts in different files independently', () => {
      const capsules = [
        createCapsule('c1', 'hash1', [
          createFileEdit('file1.js', 1, 10, 'content1'),
          createFileEdit('file2.js', 1, 10, 'content2'),
        ]),
        createCapsule('c2', 'hash2', [
          createFileEdit('file1.js', 5, 15, 'content3'),
          createFileEdit('file3.js', 1, 10, 'content4'),
        ]),
      ];

      const conflicts = ConflictDetector.detectConflicts(capsules);
      expect(conflicts).toHaveLength(1);
      expect(conflicts[0].file_path).toBe('file1.js');
    });
  });

  describe('createReceipt', () => {
    it('should create valid conflict receipt', () => {
      const conflict = {
        file_path: 'test.js',
        line_start: 1,
        line_end: 10,
        overlapping_edits: [
          { capsule_id: 'c1', edit: createFileEdit('test.js', 1, 5, 'content1') },
          { capsule_id: 'c2', edit: createFileEdit('test.js', 3, 10, 'content2') },
        ],
      };

      const receipt = ConflictDetector.createReceipt(
        conflict,
        'earlier_wins',
        'c1',
        ['c2']
      );

      expect(receipt).toMatchObject({
        file_path: 'test.js',
        capsules_involved: ['c1', 'c2'],
        resolution_rule: 'earlier_wins',
        winner: 'c1',
        denied: ['c2'],
      });
      expect(receipt.conflict_id).toBeDefined();
      expect(receipt.timestamp).toBeDefined();
      expect(receipt.line_ranges).toHaveLength(2);
    });
  });
});

// ============================================================================
// ConflictResolver Tests
// ============================================================================

describe('ConflictResolver', () => {
  const createConflict = (edits) => ({
    file_path: 'test.js',
    line_start: 1,
    line_end: 10,
    overlapping_edits: edits,
  });

  describe('earlierWins', () => {
    it('should select capsule with earliest o_hash', () => {
      const capsules = [
        createCapsule('c1', 'hash_c', []),
        createCapsule('c2', 'hash_a', []),
        createCapsule('c3', 'hash_b', []),
      ];

      const conflict = createConflict([
        { capsule_id: 'c1', edit: createFileEdit('test.js', 1, 5, 'content1') },
        { capsule_id: 'c2', edit: createFileEdit('test.js', 3, 7, 'content2') },
        { capsule_id: 'c3', edit: createFileEdit('test.js', 5, 10, 'content3') },
      ]);

      const result = ConflictResolver.earlierWins(conflict, capsules);
      expect(result.winner).toBe('c2'); // hash_a is earliest
      expect(result.denied).toContain('c1');
      expect(result.denied).toContain('c3');
    });
  });

  describe('laterWins', () => {
    it('should select capsule with latest o_hash', () => {
      const capsules = [
        createCapsule('c1', 'hash_a', []),
        createCapsule('c2', 'hash_c', []),
        createCapsule('c3', 'hash_b', []),
      ];

      const conflict = createConflict([
        { capsule_id: 'c1', edit: createFileEdit('test.js', 1, 5, 'content1') },
        { capsule_id: 'c2', edit: createFileEdit('test.js', 3, 7, 'content2') },
      ]);

      const result = ConflictResolver.laterWins(conflict, capsules);
      expect(result.winner).toBe('c2'); // hash_c is latest
      expect(result.denied).toContain('c1');
    });
  });

  describe('lexicographic', () => {
    it('should select capsule with lexicographically first id', () => {
      const conflict = createConflict([
        { capsule_id: 'capsule_3', edit: createFileEdit('test.js', 1, 5, 'content1') },
        { capsule_id: 'capsule_1', edit: createFileEdit('test.js', 3, 7, 'content2') },
        { capsule_id: 'capsule_2', edit: createFileEdit('test.js', 5, 10, 'content3') },
      ]);

      const result = ConflictResolver.lexicographic(conflict);
      expect(result.winner).toBe('capsule_1');
      expect(result.denied).toEqual(['capsule_2', 'capsule_3']);
    });
  });

  describe('resolveConflict', () => {
    it('should apply specified strategy', () => {
      const capsules = [
        createCapsule('c1', 'hash_b', []),
        createCapsule('c2', 'hash_a', []),
      ];

      const conflict = createConflict([
        { capsule_id: 'c1', edit: createFileEdit('test.js', 1, 5, 'content1') },
        { capsule_id: 'c2', edit: createFileEdit('test.js', 3, 7, 'content2') },
      ]);

      const rule = { strategy: 'earlier_wins' };
      const result = ConflictResolver.resolveConflict(conflict, rule, capsules);

      expect(result.winner).toBe('c2');
      expect(result.rule).toBe('earlier_wins');
    });

    it('should handle merge_all strategy', () => {
      const capsules = [
        createCapsule('c1', 'hash_a', []),
        createCapsule('c2', 'hash_b', []),
      ];

      const conflict = createConflict([
        { capsule_id: 'c1', edit: createFileEdit('test.js', 1, 5, 'content1') },
        { capsule_id: 'c2', edit: createFileEdit('test.js', 3, 7, 'content2') },
      ]);

      const rule = { strategy: 'merge_all' };
      const result = ConflictResolver.resolveConflict(conflict, rule, capsules);

      expect(result.winner).toBeNull();
      expect(result.denied).toEqual([]);
    });
  });
});

// ============================================================================
// shardMerge Tests
// ============================================================================

describe('shardMerge', () => {
  it('should merge capsules with no conflicts', () => {
    const capsules = [
      createCapsule('c1', 'hash1', [
        createFileEdit('file1.js', 1, 5, 'content1'),
      ]),
      createCapsule('c2', 'hash2', [
        createFileEdit('file2.js', 1, 5, 'content2'),
      ]),
    ];

    const result = shardMerge(capsules, defaultTotalOrder);

    expect(result.merged_state).toBeDefined();
    expect(Object.keys(result.merged_state)).toHaveLength(2);
    expect(result.conflict_receipts).toHaveLength(0);
  });

  it('should handle two-capsule merge with conflict', () => {
    const capsules = [
      createCapsule('c1', 'hash_b', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('c2', 'hash_a', [
        createFileEdit('file.js', 5, 15, 'content2'),
      ]),
    ];

    const result = shardMerge(capsules, defaultTotalOrder);

    expect(result.conflict_receipts).toHaveLength(1);
    expect(result.conflict_receipts[0].winner).toBe('c2'); // hash_a wins
    expect(result.conflict_receipts[0].denied).toContain('c1');
    expect(Object.keys(result.merged_state)).toHaveLength(1);
    expect(result.merged_state.c2).toBeDefined();
  });

  it('should handle three-capsule merge (multi-way conflict)', () => {
    const capsules = [
      createCapsule('c1', 'hash_c', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('c2', 'hash_a', [
        createFileEdit('file.js', 5, 15, 'content2'),
      ]),
      createCapsule('c3', 'hash_b', [
        createFileEdit('file.js', 7, 20, 'content3'),
      ]),
    ];

    const result = shardMerge(capsules, defaultTotalOrder);

    expect(result.conflict_receipts).toHaveLength(1);
    expect(result.conflict_receipts[0].capsules_involved).toHaveLength(3);
    expect(result.conflict_receipts[0].winner).toBe('c2'); // hash_a wins
    expect(result.conflict_receipts[0].denied).toContain('c1');
    expect(result.conflict_receipts[0].denied).toContain('c3');
  });

  it('should produce deterministic ordering', () => {
    const capsules = [
      createCapsule('c1', 'hash_b', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('c2', 'hash_a', [
        createFileEdit('file.js', 5, 15, 'content2'),
      ]),
    ];

    // Run multiple times
    const result1 = shardMerge(capsules, defaultTotalOrder);
    const result2 = shardMerge(capsules, defaultTotalOrder);
    const result3 = shardMerge(capsules, defaultTotalOrder);

    expect(result1.conflict_receipts[0].winner).toBe(result2.conflict_receipts[0].winner);
    expect(result2.conflict_receipts[0].winner).toBe(result3.conflict_receipts[0].winner);
    expect(result1.conflict_receipts[0].winner).toBe('c2');
  });
});

// ============================================================================
// mergeCapsules Tests
// ============================================================================

describe('mergeCapsules', () => {
  it('should merge capsules with no conflicts', () => {
    const capsules = [
      createCapsule('c1', 'hash1', [
        createFileEdit('file1.js', 1, 5, 'content1'),
      ]),
      createCapsule('c2', 'hash2', [
        createFileEdit('file2.js', 1, 5, 'content2'),
      ]),
    ];

    const result = mergeCapsules(capsules, defaultTotalOrder);

    expect(result.admitted).toEqual(['c1', 'c2']);
    expect(result.denied).toEqual([]);
    expect(result.conflict_receipts).toHaveLength(0);
    expect(result.merged_state).toBeDefined();
  });

  it('should handle two-capsule conflict', () => {
    const capsules = [
      createCapsule('c1', 'hash_b', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('c2', 'hash_a', [
        createFileEdit('file.js', 5, 15, 'content2'),
      ]),
    ];

    const result = mergeCapsules(capsules, defaultTotalOrder);

    expect(result.admitted).toEqual(['c2']);
    expect(result.denied).toEqual(['c1']);
    expect(result.conflict_receipts).toHaveLength(1);
    expect(result.conflict_receipts[0].winner).toBe('c2');
  });

  it('should handle three-capsule multi-way conflict', () => {
    const capsules = [
      createCapsule('c1', 'hash_c', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('c2', 'hash_a', [
        createFileEdit('file.js', 5, 15, 'content2'),
      ]),
      createCapsule('c3', 'hash_b', [
        createFileEdit('file.js', 7, 20, 'content3'),
      ]),
    ];

    const result = mergeCapsules(capsules, defaultTotalOrder);

    expect(result.admitted).toEqual(['c2']);
    expect(result.denied).toContain('c1');
    expect(result.denied).toContain('c3');
    expect(result.conflict_receipts).toHaveLength(1);
  });

  it('should generate conflict receipts for each conflict', () => {
    const capsules = [
      createCapsule('c1', 'hash_b', [
        createFileEdit('file1.js', 1, 10, 'content1'),
        createFileEdit('file2.js', 1, 10, 'content2'),
      ]),
      createCapsule('c2', 'hash_a', [
        createFileEdit('file1.js', 5, 15, 'content3'),
        createFileEdit('file2.js', 5, 15, 'content4'),
      ]),
    ];

    const result = mergeCapsules(capsules, defaultTotalOrder);

    expect(result.conflict_receipts).toHaveLength(2);
    expect(result.conflict_receipts[0].file_path).not.toBe(
      result.conflict_receipts[1].file_path
    );
    expect(result.conflict_receipts.every((r) => r.winner === 'c2')).toBe(true);
  });

  it('should enforce deterministic ordering across multiple runs', () => {
    const capsules = [
      createCapsule('capsule_3', 'hash_c', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('capsule_1', 'hash_a', [
        createFileEdit('file.js', 5, 15, 'content2'),
      ]),
      createCapsule('capsule_2', 'hash_b', [
        createFileEdit('file.js', 7, 20, 'content3'),
      ]),
    ];

    const result1 = mergeCapsules(capsules, defaultTotalOrder);
    const result2 = mergeCapsules(capsules, defaultTotalOrder);
    const result3 = mergeCapsules(capsules, defaultTotalOrder);

    expect(result1.admitted).toEqual(result2.admitted);
    expect(result2.admitted).toEqual(result3.admitted);
    expect(result1.denied.sort()).toEqual(result2.denied.sort());
  });

  it('should use lexicographic strategy when specified', () => {
    const totalOrder = {
      rules: [],
      default_rule: { strategy: 'lexicographic' },
    };

    const capsules = [
      createCapsule('capsule_z', 'hash_a', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('capsule_a', 'hash_z', [
        createFileEdit('file.js', 5, 15, 'content2'),
      ]),
    ];

    const result = mergeCapsules(capsules, totalOrder);

    expect(result.admitted).toEqual(['capsule_a']);
    expect(result.denied).toEqual(['capsule_z']);
    expect(result.conflict_receipts[0].resolution_rule).toBe('lexicographic');
  });

  it('should handle validation errors gracefully', () => {
    const invalidCapsules = [
      { id: 'c1' }, // Missing required fields
    ];

    expect(() => {
      mergeCapsules(invalidCapsules, defaultTotalOrder);
    }).toThrow();
  });

  it('should preserve metadata in merged state', () => {
    const capsules = [
      createCapsule('c1', 'hash1', [
        createFileEdit('file.js', 1, 5, 'content1'),
      ]),
    ];
    capsules[0].metadata = { custom_field: 'value' };

    const result = mergeCapsules(capsules, defaultTotalOrder);

    expect(result.merged_state.c1.metadata).toEqual({ custom_field: 'value' });
  });
});

// ============================================================================
// Integration Tests
// ============================================================================

describe('Integration Tests', () => {
  it('should handle complex multi-file, multi-capsule scenario', () => {
    const capsules = [
      createCapsule('agent1', 'hash_1', [
        createFileEdit('src/main.js', 1, 10, 'agent1 main changes'),
        createFileEdit('src/utils.js', 20, 30, 'agent1 utils changes'),
      ]),
      createCapsule('agent2', 'hash_2', [
        createFileEdit('src/main.js', 5, 15, 'agent2 main changes'),
        createFileEdit('src/config.js', 1, 5, 'agent2 config changes'),
      ]),
      createCapsule('agent3', 'hash_3', [
        createFileEdit('src/utils.js', 25, 35, 'agent3 utils changes'),
        createFileEdit('src/tests.js', 1, 20, 'agent3 test changes'),
      ]),
    ];

    const result = mergeCapsules(capsules, defaultTotalOrder);

    expect(result.admitted.length + result.denied.length).toBe(3);
    expect(result.conflict_receipts.length).toBeGreaterThan(0);
    expect(result.merged_state).toBeDefined();
  });

  it('should handle empty capsule list', () => {
    const result = mergeCapsules([], defaultTotalOrder);

    expect(result.admitted).toEqual([]);
    expect(result.denied).toEqual([]);
    expect(result.conflict_receipts).toEqual([]);
  });

  it('should handle single capsule (no conflicts possible)', () => {
    const capsules = [
      createCapsule('c1', 'hash1', [
        createFileEdit('file.js', 1, 10, 'content'),
      ]),
    ];

    const result = mergeCapsules(capsules, defaultTotalOrder);

    expect(result.admitted).toEqual(['c1']);
    expect(result.denied).toEqual([]);
    expect(result.conflict_receipts).toEqual([]);
  });
});
