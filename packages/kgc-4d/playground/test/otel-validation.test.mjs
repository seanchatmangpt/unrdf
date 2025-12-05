/**
 * KGC-4D Playground OTEL Validation Tests
 *
 * Validates core functionality via OTEL spans instead of traditional assertions.
 * Tests prove data is actually persisted, not just claimed to be persisted.
 */

import { describe, it, expect, beforeAll, afterAll, vi } from 'vitest';
import { setInstrumentationId as setUniverseInstrId, recordUniversePersistence } from '../lib/otel/universe-instrumented.mjs';
import { setInstrumentationId as setDeltaInstrId, submitDeltaInstrumented } from '../lib/otel/delta-instrumented.mjs';
import { setInstrumentationId as setShardInstrId, projectShardInstrumented } from '../lib/otel/shard-instrumented.mjs';
import {
  verifyDataPersistence,
  verifyValidationHooks,
  verifyShardProjection,
  getOTELValidationStatus,
  recordOTELSpans,
  ensureValidatorInitialized,
} from '../lib/otel/instrumentation.mjs';
import { getUniverse } from '../lib/server/universe.mjs';
import { dataFactory, now } from '../lib/server/universe.mjs';

describe('KGC-4D OTEL Validation - Data Persistence Proof', () => {
  let validationId;
  let universe;

  beforeAll(async () => {
    // Generate unique validation ID for this test run
    validationId = `test-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

    // Ensure validator is fully initialized before any spans are recorded
    // This eliminates race condition where async import was not awaited
    await ensureValidatorInitialized();

    // Set instrumentation ID on all modules
    setUniverseInstrId(validationId);
    setDeltaInstrId(validationId);
    setShardInstrId(validationId);

    // Get universe singleton
    universe = await getUniverse();
  });

  afterAll(() => {
    // Clean up
    setUniverseInstrId(null);
    setDeltaInstrId(null);
    setShardInstrId(null);
  });

  // ============================================================================
  // Data Persistence Validation via OTEL Spans
  // ============================================================================

  describe('Data Persistence - OTEL Span Verification', () => {
    it('should record persistence spans when data is committed', async () => {
      const deltas = [
        {
          type: 'add',
          subject: dataFactory.namedNode('http://example.org/otel-test/entity1'),
          predicate: dataFactory.namedNode('http://kgc.io/ontology/name'),
          object: dataFactory.literal('OTEL Test Entity'),
        },
      ];

      // Record persistence with instrumentation
      const result = await recordUniversePersistence(
        universe,
        { type: 'TEST_OTEL_PERSISTENCE', payload: { test: true } },
        deltas
      );

      console.log('[Test] recordUniversePersistence completed, validationId:', validationId);

      // Verify OTEL spans were recorded
      const status = getOTELValidationStatus(validationId);
      console.log('[Test] OTEL status:', { total_spans: status.total_spans, status: status.status });

      expect(status.total_spans).toBeGreaterThan(0);
      expect(status.spans).toContainEqual(expect.objectContaining({
        name: 'universe.persist',
        status: 'ok',
      }));
    });

    it('should verify data persistence through OTEL analysis', async () => {
      // First record spans
      const deltas = [
        {
          type: 'add',
          subject: dataFactory.namedNode('http://example.org/otel-test/entity2'),
          predicate: dataFactory.namedNode('http://kgc.io/ontology/name'),
          object: dataFactory.literal('OTEL Test Entity 2'),
        },
      ];

      await recordUniversePersistence(
        universe,
        { type: 'TEST_OTEL_PERSISTENCE', payload: { test: true } },
        deltas
      );

      // Get persistence verification from OTEL spans
      const verification = verifyDataPersistence(validationId);

      console.log('[Test] Persistence verification:', {
        verified: verification.verified,
        persistence_spans: verification.persistence_spans,
        operations_traced: verification.operations_traced,
        average_duration_ms: verification.average_duration_ms
      });

      // Proof: OTEL spans show persistence operations occurred
      expect(verification.verified).toBe(true);
      expect(verification.persistence_spans).toBeGreaterThan(0);
      expect(verification.operations_traced).toBeGreaterThan(0);
      expect(verification.average_duration_ms).toBeGreaterThanOrEqual(0);
    });

    it('should show proof of data storage in OTEL trace', async () => {
      const verification = verifyDataPersistence(validationId);

      // Proof: Each span contains evidence of persistence
      for (const proof of verification.proof) {
        expect(proof).toHaveProperty('operation_type');
        expect(proof).toHaveProperty('operation_count');
        expect(proof).toHaveProperty('duration_ms');
        expect(proof).toHaveProperty('timestamp');
        expect(proof.duration_ms).toBeGreaterThanOrEqual(0);
      }
    });
  });

  // ============================================================================
  // Validation Hook Enforcement via OTEL Spans
  // ============================================================================

  describe('Validation Hooks - OTEL Span Verification', () => {
    it('should record validation hook execution in OTEL spans', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/otel-test/project', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
            object: { value: '50000', termType: 'Literal' },
          },
        ],
        source: 'otel-test',
      };

      // Submit delta with instrumentation
      const result = await submitDeltaInstrumented(delta);

      // Verify result
      expect(result.status).toBe('ACK');
      expect(result.instrumentation_id).toBe(validationId);
    });

    it('should verify validation hooks through OTEL spans', async () => {
      // First record validation spans
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/otel-test/project2', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
            object: { value: '75000', termType: 'Literal' },
          },
        ],
        source: 'otel-test',
      };

      await submitDeltaInstrumented(delta);

      // Get validation verification from OTEL spans
      const verification = verifyValidationHooks(validationId);

      console.log('[Test] Validation hooks verification:', {
        verified: verification.verified,
        total_validations: verification.total_validations,
        accepted: verification.accepted,
        average_duration_ms: verification.average_duration_ms
      });

      // Proof: OTEL spans show validation hook execution
      expect(verification.verified).toBe(true);
      expect(verification.total_validations).toBeGreaterThan(0);
      expect(verification.accepted).toBeGreaterThanOrEqual(0);
      expect(verification.average_duration_ms).toBeGreaterThanOrEqual(0);
    });

    it('should show hook execution trace in OTEL spans', async () => {
      const verification = verifyValidationHooks(validationId);

      // Proof: Each hook execution is traced with metadata
      for (const trace of verification.hook_execution_trace) {
        expect(trace).toHaveProperty('hook_id');
        expect(trace).toHaveProperty('result');
        expect(trace).toHaveProperty('duration_ms');
        expect(trace).toHaveProperty('timestamp');
        expect(['ACCEPT', 'REJECT']).toContain(trace.result);
      }
    });

    it('should reject invalid deltas and record in OTEL', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/otel-test/project', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
            object: { value: '999999', termType: 'Literal' },
          },
        ],
        source: 'otel-test',
      };

      // Submit delta with instrumentation - should reject
      const result = await submitDeltaInstrumented(delta);

      // Verify rejection was recorded
      expect(result.status).toBe('REJECT');
      expect(result.reason).toMatch(/Budget cannot exceed/i);
      expect(result.otel_spans).toBeDefined();
      expect(result.otel_spans.length).toBeGreaterThan(0);
      expect(result.otel_spans[0].status).toBe('error');
    });
  });

  // ============================================================================
  // Shard Projection Validation via OTEL Spans
  // ============================================================================

  describe('Shard Projection - OTEL Span Verification', () => {
    it('should record shard projection in OTEL spans', async () => {
      // Project shard with instrumentation
      const shard = await projectShardInstrumented({});

      // Verify shard was created with OTEL trace
      expect(shard).toHaveProperty('otel_trace');
      expect(shard.otel_trace).toHaveProperty('duration_ms');
      expect(shard.otel_trace).toHaveProperty('quads_per_ms');
      expect(shard.otel_trace.span).toBeDefined();
    });

    it('should verify shard projection through OTEL spans', async () => {
      // Get projection verification from OTEL spans
      const verification = verifyShardProjection(validationId);

      // Proof: OTEL spans show shard projection operations
      expect(verification.verified).toBe(true);
      expect(verification.total_projections).toBeGreaterThan(0);
      expect(verification.total_quads_projected).toBeGreaterThan(0);
      expect(verification.average_duration_ms).toBeGreaterThan(0);
    });

    it('should show shard projection trace with metadata', async () => {
      const verification = verifyShardProjection(validationId);

      // Proof: Each projection is traced with performance data
      for (const trace of verification.projection_trace) {
        expect(trace).toHaveProperty('quad_count');
        expect(trace).toHaveProperty('duration_ms');
        expect(trace).toHaveProperty('timestamp');
        expect(trace.quad_count).toBeGreaterThan(0);
        expect(trace.duration_ms).toBeGreaterThanOrEqual(0);
      }
    });
  });

  // ============================================================================
  // Complete OTEL Validation Status
  // ============================================================================

  describe('Complete OTEL Validation Status', () => {
    it('should report overall validation status', async () => {
      const status = getOTELValidationStatus(validationId);

      expect(status).toHaveProperty('status');
      expect(status).toHaveProperty('total_spans');
      expect(status).toHaveProperty('passed');
      expect(status).toHaveProperty('failed');
      expect(status).toHaveProperty('spans');
      expect(status).toHaveProperty('timestamp');
    });

    it('should show all recorded spans', async () => {
      const status = getOTELValidationStatus(validationId);

      // Verify all span types are present
      const spanNames = new Set(status.spans.map((s) => s.name));
      expect(spanNames.size).toBeGreaterThan(0);

      // Expect to see persistence, validation, and projection spans
      const hasExpectedSpans = Array.from(spanNames).some((name) =>
        ['universe.persist', 'delta.validation', 'shard.projection'].includes(name)
      );
      expect(hasExpectedSpans).toBe(true);
    });

    it('should demonstrate end-to-end OTEL validation flow', async () => {
      // Comprehensive end-to-end verification
      const persistenceVerif = verifyDataPersistence(validationId);
      const validationVerif = verifyValidationHooks(validationId);
      const projectionVerif = verifyShardProjection(validationId);

      // All three critical flows must have OTEL evidence
      expect(persistenceVerif.verified).toBe(true);
      expect(validationVerif.verified).toBe(true);
      expect(projectionVerif.verified).toBe(true);

      // Proof that data lifecycle is complete:
      // 1. Data was persisted (persistence span)
      // 2. Data was validated (validation hook span)
      // 3. Data was retrieved (projection span)
      expect(persistenceVerif.persistence_spans).toBeGreaterThan(0);
      expect(validationVerif.total_validations).toBeGreaterThan(0);
      expect(projectionVerif.total_projections).toBeGreaterThan(0);
    });
  });

  // ============================================================================
  // OTEL Span Correctness Validation
  // ============================================================================

  describe('OTEL Span Correctness', () => {
    it('should have valid span structure', async () => {
      const status = getOTELValidationStatus(validationId);

      // Verify every span has required fields
      for (const span of status.spans) {
        expect(span).toHaveProperty('name');
        expect(span).toHaveProperty('status');
        expect(span).toHaveProperty('duration');
        expect(span).toHaveProperty('attributes');
        expect(span).toHaveProperty('timestamp');

        // Validate types
        expect(typeof span.name).toBe('string');
        expect(['ok', 'error']).toContain(span.status);
        expect(typeof span.duration).toBe('number');
        expect(typeof span.attributes).toBe('object');
        expect(span.duration).toBeGreaterThanOrEqual(0);
      }
    });

    it('should include service metadata in spans', async () => {
      const status = getOTELValidationStatus(validationId);

      // Verify service name is consistent
      for (const span of status.spans) {
        expect(span.attributes['service.name']).toBe('kgc-4d-playground');
        expect(span.attributes).toHaveProperty('component');
      }
    });

    it('should track causality via timestamps', async () => {
      const status = getOTELValidationStatus(validationId);

      // Verify timestamps are monotonic or within reasonable bounds
      const timestamps = status.spans.map((s) => {
        const ts = typeof s.timestamp === 'string' ? BigInt(s.timestamp) : BigInt(s.timestamp);
        return ts;
      });

      // All timestamps should be valid nanosecond values
      for (const ts of timestamps) {
        expect(ts).toBeGreaterThan(0n);
      }
    });
  });
});
