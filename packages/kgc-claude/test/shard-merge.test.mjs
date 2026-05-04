/**
 * Shard Merge Tests - Multi-agent concurrency without conflicts
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  createShard,
  addDelta,
  mergeDeltas,
  checkShardOverlap,
  createMultiAgentSession,
  getPendingDeltas,
  clearShards,
  getShards,
} from '../src/shard-merge.mjs';

describe('Shard Merge', () => {
  afterEach(() => {
    clearShards();
  });

  describe('createShard', () => {
    it('should create a shard with scope', () => {
      const shard = createShard('agent-1', {
        files: ['src/components/**'],
        graphs: ['http://kgc.io/Universe'],
      });

      expect(shard.id).toBeDefined();
      expect(shard.agentId).toBe('agent-1');
      expect(shard.scope.files).toContain('src/components/**');
    });

    it('should register shard in registry', () => {
      createShard('agent-1', {});
      createShard('agent-2', {});

      expect(getShards().length).toBe(2);
    });

    it('should support priority', () => {
      const shard = createShard('agent-1', {}, { priority: 10 });

      expect(shard.priority).toBe(10);
    });
  });

  describe('addDelta', () => {
    it('should add delta to shard', () => {
      const shard = createShard('agent-1', {
        files: ['src/**'],
      });

      const result = addDelta(shard.id, {
        type: 'add',
        target: 'src/foo.mjs',
        after: { content: 'new content' },
      });

      expect(result.success).toBe(true);
      expect(getPendingDeltas(shard.id).length).toBe(1);
    });

    it('should reject delta outside scope', () => {
      const shard = createShard('agent-1', {
        files: ['src/**'],
      });

      const result = addDelta(shard.id, {
        type: 'add',
        target: 'test/foo.mjs',
      });

      expect(result.success).toBe(false);
      expect(result.reason).toContain('outside shard scope');
    });

    it('should increment vector clock on delta', () => {
      const shard = createShard('agent-1', {});

      addDelta(shard.id, { type: 'add', target: 'x' });
      addDelta(shard.id, { type: 'add', target: 'y' });

      const deltas = getPendingDeltas(shard.id);
      expect(deltas.length).toBe(2);
    });
  });

  describe('mergeDeltas', () => {
    it('should merge non-conflicting deltas', async () => {
      const shard1 = createShard('agent-1', { files: ['a/**'] });
      const shard2 = createShard('agent-2', { files: ['b/**'] });

      addDelta(shard1.id, { type: 'add', target: 'a/foo.mjs' });
      addDelta(shard2.id, { type: 'add', target: 'b/bar.mjs' });

      const result = await mergeDeltas([
        getPendingDeltas(shard1.id),
        getPendingDeltas(shard2.id),
      ]);

      expect(result.merged.length).toBe(2);
      expect(result.conflicts.length).toBe(0);
    });

    it('should resolve conflicts by priority', async () => {
      const shard1 = createShard('agent-1', { files: ['**'] }, { priority: 1 });
      const shard2 = createShard('agent-2', { files: ['**'] }, { priority: 10 });

      addDelta(shard1.id, { type: 'add', target: 'shared.mjs', after: 'v1' });
      addDelta(shard2.id, { type: 'add', target: 'shared.mjs', after: 'v2' });

      const result = await mergeDeltas([
        getPendingDeltas(shard1.id),
        getPendingDeltas(shard2.id),
      ]);

      expect(result.merged.length).toBe(1);
      expect(result.conflicts.length).toBe(1);
      expect(result.merged[0].agentId).toBe('agent-2'); // Higher priority wins
    });

    it('should resolve conflicts by timestamp when priority equal', async () => {
      const shard1 = createShard('agent-1', { files: ['**'] });
      const shard2 = createShard('agent-2', { files: ['**'] });

      addDelta(shard1.id, { type: 'add', target: 'shared.mjs' });
      // Small delay
      await new Promise(r => setTimeout(r, 1));
      addDelta(shard2.id, { type: 'add', target: 'shared.mjs' });

      const result = await mergeDeltas([
        getPendingDeltas(shard1.id),
        getPendingDeltas(shard2.id),
      ]);

      expect(result.merged.length).toBe(1);
      expect(result.merged[0].agentId).toBe('agent-1'); // Earlier timestamp wins
    });

    it('should generate receipt hash', async () => {
      const shard1 = createShard('agent-1', {});
      addDelta(shard1.id, { type: 'add', target: 'foo' });

      const result = await mergeDeltas([getPendingDeltas(shard1.id)]);

      expect(result.receiptHash).toBeDefined();
      expect(result.receiptHash.length).toBe(64);
    });

    it('should sort merged deltas by timestamp', async () => {
      const shard = createShard('agent-1', {});

      addDelta(shard.id, { type: 'add', target: 'a' });
      await new Promise(r => setTimeout(r, 1));
      addDelta(shard.id, { type: 'add', target: 'b' });

      const result = await mergeDeltas([getPendingDeltas(shard.id)]);

      expect(result.merged[0].delta.target).toBe('a');
      expect(result.merged[1].delta.target).toBe('b');
    });
  });

  describe('checkShardOverlap', () => {
    it('should detect overlapping targets', () => {
      const shard1 = createShard('agent-1', { files: ['**'] });
      const shard2 = createShard('agent-2', { files: ['**'] });

      addDelta(shard1.id, { type: 'add', target: 'shared.mjs' });
      addDelta(shard2.id, { type: 'add', target: 'shared.mjs' });
      addDelta(shard2.id, { type: 'add', target: 'unique.mjs' });

      const result = checkShardOverlap(shard1.id, shard2.id);

      expect(result.overlap).toBe(true);
      expect(result.overlappingTargets).toContain('shared.mjs');
    });

    it('should return no overlap for disjoint shards', () => {
      const shard1 = createShard('agent-1', { files: ['**'] });
      const shard2 = createShard('agent-2', { files: ['**'] });

      addDelta(shard1.id, { type: 'add', target: 'a.mjs' });
      addDelta(shard2.id, { type: 'add', target: 'b.mjs' });

      const result = checkShardOverlap(shard1.id, shard2.id);

      expect(result.overlap).toBe(false);
    });
  });

  describe('createMultiAgentSession', () => {
    it('should create session with multiple shards', () => {
      const session = createMultiAgentSession([
        { id: 'frontend', scope: { files: ['src/components/**'] }, priority: 1 },
        { id: 'backend', scope: { files: ['src/api/**'] }, priority: 2 },
      ]);

      expect(session.shards.length).toBe(2);
      expect(session.shards[0].agentId).toBe('frontend');
      expect(session.shards[1].agentId).toBe('backend');
    });

    it('should merge all shards on session.merge()', async () => {
      const session = createMultiAgentSession([
        { id: 'agent-1', scope: { files: ['a/**'] } },
        { id: 'agent-2', scope: { files: ['b/**'] } },
      ]);

      addDelta(session.shards[0].id, { type: 'add', target: 'a/foo.mjs' });
      addDelta(session.shards[1].id, { type: 'add', target: 'b/bar.mjs' });

      const result = await session.merge();

      expect(result.merged.length).toBe(2);
    });

    it('should clear pending deltas after merge', async () => {
      const session = createMultiAgentSession([
        { id: 'agent-1', scope: {} },
      ]);

      addDelta(session.shards[0].id, { type: 'add', target: 'foo' });
      await session.merge();

      expect(getPendingDeltas(session.shards[0].id).length).toBe(0);
    });
  });
});
