/**
 * @fileoverview Tests for RunCapsule canonicalization and replay
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdirSync, rmSync, existsSync, readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import {
  RunCapsule,
  storeCapsule,
  replayCapsule,
  listCapsules,
} from '../src/capsule.mjs';

const TEST_CAPSULE_DIR = './var/kgc/capsules-test';

describe('RunCapsule', () => {
  beforeEach(() => {
    // Clean up test directory
    if (existsSync(TEST_CAPSULE_DIR)) {
      rmSync(TEST_CAPSULE_DIR, { recursive: true, force: true });
    }
    mkdirSync(TEST_CAPSULE_DIR, { recursive: true });
  });

  afterEach(() => {
    // Clean up after tests
    if (existsSync(TEST_CAPSULE_DIR)) {
      rmSync(TEST_CAPSULE_DIR, { recursive: true, force: true });
    }
  });

  describe('Canonicalization', () => {
    it('should produce deterministic hash for same inputs', () => {
      const data1 = {
        inputs: { prompt: 'test', model: 'gpt-4' },
        tool_trace: [
          { tool: 'bash', args: ['ls'], output: 'file1.txt\nfile2.txt' },
        ],
        edits: [{ file: 'test.mjs', old: 'foo', new: 'bar' }],
        artifacts: ['output.json'],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'abc123',
        o_hash_after: 'def456',
        receipts: [],
      };

      const data2 = {
        inputs: { model: 'gpt-4', prompt: 'test' }, // Different order
        tool_trace: [
          { tool: 'bash', args: ['ls'], output: 'file1.txt\nfile2.txt' },
        ],
        edits: [{ file: 'test.mjs', old: 'foo', new: 'bar' }],
        artifacts: ['output.json'],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'abc123',
        o_hash_after: 'def456',
        receipts: [],
      };

      const capsule1 = new RunCapsule(data1);
      const capsule2 = new RunCapsule(data2);

      expect(capsule1.capsule_hash).toBe(capsule2.capsule_hash);
    });

    it('should produce different hash for different inputs', () => {
      const data1 = {
        inputs: { prompt: 'test1' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'abc123',
        o_hash_after: 'def456',
        receipts: [],
      };

      const data2 = {
        inputs: { prompt: 'test2' }, // Different prompt
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'abc123',
        o_hash_after: 'def456',
        receipts: [],
      };

      const capsule1 = new RunCapsule(data1);
      const capsule2 = new RunCapsule(data2);

      expect(capsule1.capsule_hash).not.toBe(capsule2.capsule_hash);
    });

    it('should handle Unicode normalization in canonicalization', () => {
      // Unicode café: é can be represented as single char (U+00E9) or e + combining accent (U+0065 + U+0301)
      const data1 = {
        inputs: { text: 'café' }, // Single character é
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'abc',
        o_hash_after: 'def',
        receipts: [],
      };

      const data2 = {
        inputs: { text: 'café' }, // Composed é (should normalize to same)
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'abc',
        o_hash_after: 'def',
        receipts: [],
      };

      const capsule1 = new RunCapsule(data1);
      const capsule2 = new RunCapsule(data2);

      // Should produce same hash after normalization
      expect(capsule1.capsule_hash).toBe(capsule2.capsule_hash);
    });

    it('should handle nested object canonicalization', () => {
      const data = {
        inputs: {
          nested: {
            z: 'last',
            a: 'first',
            m: { x: 1, b: 2 },
          },
        },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'abc',
        o_hash_after: 'def',
        receipts: [],
      };

      const capsule = new RunCapsule(data);

      // Should have valid hash
      expect(capsule.capsule_hash).toMatch(/^[a-f0-9]{64}$/);
    });
  });

  describe('Hash Consistency', () => {
    it('should produce BLAKE3 hash of 64 hex characters', () => {
      const data = {
        inputs: { test: 'value' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before',
        o_hash_after: 'after',
        receipts: [],
      };

      const capsule = new RunCapsule(data);

      expect(capsule.capsule_hash).toMatch(/^[a-f0-9]{64}$/);
    });

    it('should maintain hash stability across multiple instantiations', () => {
      const data = {
        inputs: { stable: 'test' },
        tool_trace: [{ tool: 'read', file: 'test.txt' }],
        edits: [],
        artifacts: [],
        bounds: { start: 5000, end: 6000 },
        o_hash_before: 'hash1',
        o_hash_after: 'hash2',
        receipts: [],
      };

      const hashes = Array.from({ length: 10 }, () => {
        const capsule = new RunCapsule(data);
        return capsule.capsule_hash;
      });

      // All hashes should be identical
      expect(new Set(hashes).size).toBe(1);
    });

    it('should handle empty arrays consistently', () => {
      const data1 = {
        inputs: {},
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 0, end: 0 },
        o_hash_before: '',
        o_hash_after: '',
        receipts: [],
      };

      const data2 = {
        inputs: {},
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 0, end: 0 },
        o_hash_before: '',
        o_hash_after: '',
        receipts: [],
      };

      const capsule1 = new RunCapsule(data1);
      const capsule2 = new RunCapsule(data2);

      expect(capsule1.capsule_hash).toBe(capsule2.capsule_hash);
    });
  });

  describe('storeCapsule', () => {
    it('should store capsule to correct location', async () => {
      const data = {
        inputs: { action: 'store_test' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before',
        o_hash_after: 'after',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const result = await storeCapsule(capsule, TEST_CAPSULE_DIR);

      const expectedPath = join(
        TEST_CAPSULE_DIR,
        `${capsule.capsule_hash}.json`
      );
      expect(result.path).toBe(expectedPath);
      expect(result.deduplicated).toBe(false);
      expect(existsSync(result.path)).toBe(true);
    });

    it('should create manifest entry', async () => {
      const data = {
        inputs: { action: 'manifest_test' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before',
        o_hash_after: 'after',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      await storeCapsule(capsule, TEST_CAPSULE_DIR);

      const manifestPath = join(TEST_CAPSULE_DIR, 'manifest.json');
      expect(existsSync(manifestPath)).toBe(true);

      const manifest = JSON.parse(readFileSync(manifestPath, 'utf-8'));
      expect(manifest.capsules).toHaveLength(1);
      expect(manifest.capsules[0].hash).toBe(capsule.capsule_hash);
      expect(manifest.capsules[0]).toHaveProperty('stored_at');
    });

    it('should handle multiple capsule storage', async () => {
      const capsules = [
        new RunCapsule({
          inputs: { id: 1 },
          tool_trace: [],
          edits: [],
          artifacts: [],
          bounds: { start: 1000, end: 2000 },
          o_hash_before: 'a',
          o_hash_after: 'b',
          receipts: [],
        }),
        new RunCapsule({
          inputs: { id: 2 },
          tool_trace: [],
          edits: [],
          artifacts: [],
          bounds: { start: 2000, end: 3000 },
          o_hash_before: 'c',
          o_hash_after: 'd',
          receipts: [],
        }),
      ];

      for (const capsule of capsules) {
        await storeCapsule(capsule, TEST_CAPSULE_DIR);
      }

      const files = readdirSync(TEST_CAPSULE_DIR).filter((f) =>
        f.endsWith('.json')
      );
      expect(files.length).toBeGreaterThanOrEqual(3); // 2 capsules + manifest + index
    });
  });

  describe('Replay', () => {
    it('should replay capsule with matching output', async () => {
      const data = {
        inputs: { command: 'echo test' },
        tool_trace: [
          { tool: 'bash', command: 'echo test', output: 'test\n' },
        ],
        edits: [{ file: 'output.txt', old: '', new: 'test\n' }],
        artifacts: ['output.txt'],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'initial',
        o_hash_after: 'final_hash',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = { state: 'initial' };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      expect(result).toBe('admit');
      expect(receipt.status).toBe('admit');
      expect(receipt.capsule_hash).toBe(capsule.capsule_hash);
      expect(receipt.output_hash).toBe('final_hash');
      expect(receipt.verified).toBe(true);
    });

    it('should deny replay with divergent state', async () => {
      const data = {
        inputs: { command: 'test' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'expected_initial',
        o_hash_after: 'expected_final',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      // Simulate divergent state with different hash
      const o_snapshot = { state: 'divergent', hash: 'different_hash' };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      expect(result).toBe('deny');
      expect(receipt.status).toBe('deny');
      expect(receipt.capsule_hash).toBe(capsule.capsule_hash);
      expect(receipt.error).toContain('Output hash mismatch');
    });

    it('should apply edits during replay', async () => {
      const data = {
        inputs: { task: 'edit_file' },
        tool_trace: [],
        edits: [
          { file: 'test.mjs', old: 'const x = 1;', new: 'const x = 2;' },
          { file: 'test.mjs', old: 'const y = 3;', new: 'const y = 4;' },
        ],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before_edits',
        o_hash_after: 'after_edits',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = { files: { 'test.mjs': 'const x = 1;\nconst y = 3;' } };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      expect(receipt.edits_applied).toBe(2);
      expect(receipt.tool_traces_executed).toBe(0);
    });

    it('should execute tool traces during replay', async () => {
      const data = {
        inputs: { task: 'run_tools' },
        tool_trace: [
          { tool: 'read', file: 'input.txt', output: 'data' },
          { tool: 'write', file: 'output.txt', content: 'processed data' },
        ],
        edits: [],
        artifacts: ['output.txt'],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before_tools',
        o_hash_after: 'after_tools',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = { state: 'ready' };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      expect(receipt.tool_traces_executed).toBe(2);
      expect(receipt.edits_applied).toBe(0);
    });

    it('should compute proper output hash from snapshot', async () => {
      const data = {
        inputs: { task: 'hash_test' },
        tool_trace: [],
        edits: [
          { file: 'test.txt', old: 'a', new: 'b' },
        ],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before',
        o_hash_after: 'after',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = { files: { 'test.txt': 'abc' } };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      expect(receipt.output_hash).toBeDefined();
      expect(receipt.output_hash).toMatch(/^[a-f0-9]{64}$/);
      expect(receipt.expected_hash).toBe('after');
    });

    it('should handle snapshot with files correctly', async () => {
      const data = {
        inputs: { task: 'file_edit' },
        tool_trace: [],
        edits: [
          { file: 'code.js', old: 'foo', new: 'bar' },
        ],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'start',
        o_hash_after: 'end',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = { files: { 'code.js': 'function foo() {}' } };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      expect(receipt.edits_applied).toBe(1);
      expect(receipt.verified).toBeDefined();
    });

    it('should continue on edit errors', async () => {
      const data = {
        inputs: { task: 'partial_edits' },
        tool_trace: [],
        edits: [
          { invalid: 'edit1' }, // Missing required fields
          { file: 'valid.txt', old: 'x', new: 'y' },
        ],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'start',
        o_hash_after: 'end',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = { files: { 'valid.txt': 'xyz' } };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      // Should apply the valid edit
      expect(receipt.edits_applied).toBeGreaterThanOrEqual(1);
    });

    it('should handle tool traces with validation', async () => {
      const data = {
        inputs: { task: 'tool_validation' },
        tool_trace: [
          { tool: 'bash', command: 'ls', output: 'file.txt' },
          { tool: 'read', file: 'data.json' },
          { invalid_trace: true }, // Invalid trace
        ],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'initial',
        o_hash_after: 'final',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = { state: 'ready' };

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      // Should execute valid tool traces
      expect(receipt.tool_traces_executed).toBe(2);
    });

    it('should clone snapshot without mutation', async () => {
      const data = {
        inputs: { task: 'immutable_test' },
        tool_trace: [],
        edits: [
          { file: 'test.txt', old: 'original', new: 'modified' },
        ],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before',
        o_hash_after: 'after',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const originalSnapshot = { files: { 'test.txt': 'original content' } };
      const snapshotCopy = JSON.parse(JSON.stringify(originalSnapshot));

      await replayCapsule(capsule, originalSnapshot);

      // Original snapshot should be unchanged
      expect(originalSnapshot).toEqual(snapshotCopy);
    });

    it('should include replay duration in receipt', async () => {
      const data = {
        inputs: { task: 'timing_test' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'start',
        o_hash_after: 'end',
        receipts: [],
      };

      const capsule = new RunCapsule(data);
      const o_snapshot = {};

      const { result, receipt } = await replayCapsule(capsule, o_snapshot);

      expect(receipt.replay_duration_ms).toBeDefined();
      expect(receipt.replay_duration_ms).toBeGreaterThanOrEqual(0);
    });
  });

  describe('listCapsules', () => {
    it('should return empty array when no capsules exist', async () => {
      const capsules = await listCapsules(TEST_CAPSULE_DIR);
      expect(capsules).toEqual([]);
    });

    it('should list all stored capsules with hashes', async () => {
      const testData = {
        inputs: { test: 'capsule1' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'a',
        o_hash_after: 'b',
        receipts: [],
      };

      const testCapsules = [
        new RunCapsule(testData),
        new RunCapsule({
          inputs: { test: 'capsule2' },
          tool_trace: [],
          edits: [],
          artifacts: [],
          bounds: { start: 2000, end: 3000 },
          o_hash_before: 'c',
          o_hash_after: 'd',
          receipts: [],
        }),
      ];

      for (const capsule of testCapsules) {
        await storeCapsule(capsule, TEST_CAPSULE_DIR);
      }

      const capsules = await listCapsules(TEST_CAPSULE_DIR);

      expect(capsules).toHaveLength(2);
      expect(capsules[0]).toHaveProperty('hash');
      expect(capsules[0]).toHaveProperty('stored_at');
      expect(capsules[0]).toHaveProperty('inputs');
      expect(capsules[0]).toHaveProperty('bounds');

      const hashes = capsules.map((c) => c.hash);
      expect(hashes).toContain(testCapsules[0].capsule_hash);
      expect(hashes).toContain(testCapsules[1].capsule_hash);
    });

    it('should handle corrupt capsule files gracefully', async () => {
      // Create a valid capsule first
      const validCapsule = new RunCapsule({
        inputs: { valid: true },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'a',
        o_hash_after: 'b',
        receipts: [],
      });
      await storeCapsule(validCapsule, TEST_CAPSULE_DIR);

      // Create a corrupt file
      const { writeFileSync } = await import('node:fs');
      writeFileSync(
        join(TEST_CAPSULE_DIR, 'corrupt.json'),
        'invalid json {'
      );

      const capsules = await listCapsules(TEST_CAPSULE_DIR);

      // Should still return valid capsule, skipping corrupt one
      expect(capsules.length).toBeGreaterThanOrEqual(1);
      expect(capsules.some((c) => c.hash === validCapsule.capsule_hash)).toBe(
        true
      );
    });
  });
});
