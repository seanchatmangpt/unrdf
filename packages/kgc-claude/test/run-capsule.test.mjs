/**
 * Run Capsule Tests - Deterministic Δ_run objects
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createRunCapsule,
  checkAdmission,
  RunCapsuleSchema,
} from '../src/run-capsule.mjs';
import { RUN_STATUS } from '../src/constants.mjs';

describe('Run Capsule', () => {
  describe('createRunCapsule', () => {
    it('should create a run capsule builder', () => {
      const builder = createRunCapsule();

      expect(builder.id).toBeDefined();
      expect(typeof builder.id).toBe('string');
      expect(builder.t_ns).toBeDefined();
      expect(typeof builder.t_ns).toBe('bigint');
    });

    it('should add tool calls to trace', () => {
      const builder = createRunCapsule();

      const callId = builder.addToolCall({
        name: 'Read',
        input: { file: 'test.txt' },
      });

      expect(callId).toBeDefined();

      const metrics = builder.getMetrics();
      expect(metrics.toolOps).toBe(1);
    });

    it('should complete tool calls', async () => {
      const builder = createRunCapsule();

      const callId = builder.addToolCall({
        name: 'Read',
        input: { file: 'test.txt' },
      });

      builder.completeToolCall(callId, { output: 'file contents' });

      const capsule = await builder.seal();
      const call = capsule.toolTrace.find(c => c.id === callId);

      expect(call.status).toBe('success');
      expect(call.output).toBe('file contents');
      expect(call.endTime).toBeDefined();
    });

    it('should track artifacts', async () => {
      const builder = createRunCapsule();

      const artId = builder.addArtifact({
        type: 'file',
        path: 'src/foo.mjs',
        contentHash: 'abc123',
      });

      expect(artId).toBeDefined();

      const metrics = builder.getMetrics();
      expect(metrics.filesTouched).toBe(1);
    });

    it('should track deltas across all four dimensions', async () => {
      const builder = createRunCapsule();

      builder.addDeltaO({ type: 'add', target: 'http://example.org/thing' });
      builder.addDeltaPi({ type: 'add', target: 'projection:view1' });
      builder.addDeltaLambda({ type: 'add', target: 'law:constraint1' });
      builder.addDeltaQ({ type: 'add', target: 'invariant:q1' });

      const capsule = await builder.seal();

      expect(capsule.deltaO.length).toBe(1);
      expect(capsule.deltaPi.length).toBe(1);
      expect(capsule.deltaLambda.length).toBe(1);
      expect(capsule.deltaQ.length).toBe(1);
    });

    it('should seal with deterministic hash', async () => {
      const builder = createRunCapsule();

      builder.addToolCall({ name: 'Read', input: { file: 'test.txt' } });

      const capsule = await builder.seal();

      expect(capsule.runHash).toBeDefined();
      expect(capsule.runHash.length).toBe(64); // BLAKE3 hex
      expect(capsule.status).toBe(RUN_STATUS.COMPLETED);
      expect(capsule.admitted).toBe(true);
    });

    it('should chain to previous run', async () => {
      const builder1 = createRunCapsule();
      const capsule1 = await builder1.seal();

      const builder2 = createRunCapsule({
        previousRunHash: capsule1.runHash,
      });
      const capsule2 = await builder2.seal();

      expect(capsule2.previousRunHash).toBe(capsule1.runHash);
    });

    it('should support parent run for nested execution', async () => {
      const parent = createRunCapsule();
      const parentCapsule = await parent.seal();

      const child = createRunCapsule({
        parentRunId: parentCapsule.id,
      });
      const childCapsule = await child.seal();

      expect(childCapsule.parentRunId).toBe(parentCapsule.id);
    });

    it('should handle denial', async () => {
      const builder = createRunCapsule();

      builder.deny('Budget exceeded');

      const capsule = await builder.seal();

      expect(capsule.status).toBe(RUN_STATUS.DENIED);
      expect(capsule.admitted).toBe(false);
      expect(capsule.denialReason).toBe('Budget exceeded');
    });
  });

  describe('checkAdmission', () => {
    it('should admit valid run capsule', async () => {
      const builder = createRunCapsule();
      const capsule = await builder.seal();

      const result = checkAdmission(capsule, {
        history: new Set(),
        preserveQ: () => true,
      });

      expect(result.admitted).toBe(true);
    });

    it('should deny duplicate runs', async () => {
      const builder = createRunCapsule();
      const capsule = await builder.seal();

      const history = new Set([capsule.runHash]);

      const result = checkAdmission(capsule, { history });

      expect(result.admitted).toBe(false);
      expect(result.reason).toContain('Duplicate');
    });

    it('should deny when invariant not preserved', async () => {
      const builder = createRunCapsule();
      const capsule = await builder.seal();

      const result = checkAdmission(capsule, {
        preserveQ: () => false,
      });

      expect(result.admitted).toBe(false);
      expect(result.reason).toContain('Invariant');
    });
  });

  describe('RunCapsuleSchema', () => {
    it('should validate complete capsule', async () => {
      const builder = createRunCapsule();
      builder.addToolCall({ name: 'Test', input: {} });
      const capsule = await builder.seal();

      expect(() => RunCapsuleSchema.parse(capsule)).not.toThrow();
    });
  });
});
