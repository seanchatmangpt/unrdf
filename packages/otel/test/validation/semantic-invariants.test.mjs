/**
 * Semantic Invariants Validator Tests
 *
 * Tests the 7 semantic invariants validation layer:
 * 1. Semantic Causality
 * 2. OTel-OCEL Isomorphism
 * 3. Temporal Consistency
 * 4. Replay Determinism
 * 5. Closed-Loop Completion
 * 6. Minimality
 * 7. Semantic Validity
 */

import { describe, it, expect } from 'vitest';
import {
  SemanticInvariantsValidator,
  validateSemanticInvariants,
  InvariantViolation
} from '../../src/validation/index.mjs';

describe('SemanticInvariantsValidator', () => {
  describe('Invariant 1: Semantic Causality', () => {
    it('passes when task_id, workflow_id, case_id are stable across trace', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          name: 'businessos.create_deal',
          start_time: 1000,
          end_time: 2000,
          attributes: {
            'chatman.comm.trace_id': 'trace-abc',
            'chatman.comm.source_system': 'businessos',
            'chatman.comm.target_system': 'canopy',
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.workflow_id': 'workflow-456',
            'chatman.invariant.case_id': 'case-789'
          }
        },
        {
          span_id: 'span-2',
          name: 'canopy.process_deal',
          start_time: 2000,
          end_time: 3000,
          attributes: {
            'chatman.comm.trace_id': 'trace-abc',
            'chatman.comm.source_system': 'canopy',
            'chatman.comm.target_system': 'osa',
            'chatman.invariant.task_id': 'task-123',  // Same task_id
            'chatman.invariant.workflow_id': 'workflow-456',  // Same workflow_id
            'chatman.invariant.case_id': 'case-789'  // Same case_id
          }
        }
      ];

      const result = await validator.validateSemanticCausality(spans, []);

      expect(result.valid).toBe(true);
      expect(result.violations).toHaveLength(0);
    });

    it('fails when task_id differs across spans in same trace', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          name: 'businessos.create_deal',
          attributes: {
            'chatman.comm.trace_id': 'trace-abc',
            'chatman.invariant.task_id': 'task-123'
          }
        },
        {
          span_id: 'span-2',
          name: 'canopy.process_deal',
          attributes: {
            'chatman.comm.trace_id': 'trace-abc',
            'chatman.invariant.task_id': 'task-999'  // Different task_id!
          }
        }
      ];

      const result = await validator.validateSemanticCausality(spans, []);

      expect(result.valid).toBe(false);
      expect(result.violations).toHaveLength(1);
      expect(result.violations[0].code).toBe('semantic.causality.violation.task_id_mismatch');
    });
  });

  describe('Invariant 2: OTel-OCEL Isomorphism', () => {
    it('passes when OTel spans and OCEL events have 1:1 mapping', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          name: 'businessos.create_deal',
          attributes: {
            'chatman.comm.trace_id': 'trace-abc',
            'chatman.invariant.ocel_mapping_id': 'map-123',
            'chatman.invariant.isomorphism_hash': 'blake3:abc123'
          }
        }
      ];

      const ocelEvents = [
        {
          id: 'event-1',
          activity: 'create_deal',
          timestamp: 1000,
          attributes: {
            'chatman.invariant.ocel_mapping_id': 'map-123',
            'chatman.invariant.isomorphism_hash': 'blake3:abc123'
          }
        }
      ];

      const result = await validator.validateOtelOcelIsomorphism(spans, ocelEvents);

      expect(result.valid).toBe(true);
      expect(result.violations).toHaveLength(0);
    });

    it('fails when OTel span has no corresponding OCEL event', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.comm.trace_id': 'trace-abc',
            'chatman.invariant.ocel_mapping_id': 'map-orphan'
          }
        }
      ];

      const ocelEvents = [];

      const result = await validator.validateOtelOcelIsomorphism(spans, ocelEvents);

      expect(result.valid).toBe(false);
      expect(result.violations[0].code).toBe('isomorphism.violation.orphan_otel_span');
    });
  });

  describe('Invariant 3: Temporal Consistency', () => {
    it('passes when all tasks complete within observation window', async () => {
      const validator = new SemanticInvariantsValidator({ observationWindowMs: 300000 });

      const now = Date.now();

      const spans = [
        {
          span_id: 'span-1',
          name: 'task.execute',
          start_time: now - 10000,  // Started 10s ago
          end_time: now - 5000,       // Completed 5s ago
          attributes: {
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.state_before': 'running',
            'chatman.invariant.state_after': 'completed'
          }
        }
      ];

      const result = await validator.validateTemporalConsistency(spans, []);

      expect(result.valid).toBe(true);
    });

    it('fails when task is orphaned (exceeds observation window)', async () => {
      const validator = new SemanticInvariantsValidator({ observationWindowMs: 300000 });

      const now = Date.now();

      const spans = [
        {
          span_id: 'span-1',
          name: 'task.execute',
          start_time: now - 400000,  // Started 400s ago (> 300s window)
          end_time: now - 390000,
          attributes: {
            'chatman.invariant.task_id': 'task-orphan',
            'chatman.invariant.state_before': 'running',
            'chatman.invariant.state_after': 'running'  // Still running!
          }
        }
      ];

      const result = await validator.validateTemporalConsistency(spans, []);

      expect(result.valid).toBe(false);
      expect(result.violations[0].code).toBe('temporal.violation.orphan_task');
    });

    it('fails when state transition is invalid', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          name: 'task.transition',
          attributes: {
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.state_before': 'completed',
            'chatman.invariant.state_after': 'running'  // Invalid: completed → running
          }
        }
      ];

      const result = await validator.validateTemporalConsistency(spans, []);

      expect(result.valid).toBe(false);
      expect(result.violations[0].code).toBe('temporal.violation.invalid_state_transition');
    });
  });

  describe('Invariant 4: Replay Determinism', () => {
    it('passes when replay produces identical hash', async () => {
      const validator = new SemanticInvariantsValidator();

      const originalSpans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.invariant.replay_id': 'replay-123',
            'chatman.invariant.output_hash': 'blake3:abc123',
            'chatman.comm.trace_id': 'trace-xyz'
          }
        }
      ];

      const replaySpans = [
        {
          span_id: 'span-2',
          attributes: {
            'chatman.invariant.replay_id': 'replay-123',
            'chatman.invariant.output_hash': 'blake3:abc123',  // Same hash
            'chatman.comm.trace_id': 'trace-xyz'  // Same trace
          }
        }
      ];

      const result = await validator.validateReplayDeterminism(originalSpans, replaySpans);

      expect(result.valid).toBe(true);
    });

    it('fails when replay produces different hash', async () => {
      const validator = new SemanticInvariantsValidator();

      const originalSpans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.invariant.replay_id': 'replay-456',
            'chatman.invariant.output_hash': 'blake3:abc123'
          }
        }
      ];

      const replaySpans = [
        {
          span_id: 'span-2',
          attributes: {
            'chatman.invariant.replay_id': 'replay-456',
            'chatman.invariant.output_hash': 'blake3:def456'  // Different hash!
          }
        }
      ];

      const result = await validator.validateReplayDeterminism(originalSpans, replaySpans);

      expect(result.valid).toBe(false);
      expect(result.violations[0].code).toBe('determinism.violation.hash_mismatch');
    });
  });

  describe('Invariant 5: Closed-Loop Completion', () => {
    it('passes when trace completes businessos→canopy→osa→businessos', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.comm.source_system': 'businessos',
            'chatman.comm.target_system': 'canopy',
            'chatman.comm.trace_id': 'trace-loop-1',
            'chatman.invariant.initial_trace_id': 'trace-loop-1',
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.workflow_id': 'workflow-456'
          }
        },
        {
          span_id: 'span-2',
          attributes: {
            'chatman.comm.source_system': 'canopy',
            'chatman.comm.target_system': 'osa',
            'chatman.comm.trace_id': 'trace-loop-1',
            'chatman.invariant.initial_trace_id': 'trace-loop-1',
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.workflow_id': 'workflow-456'
          }
        },
        {
          span_id: 'span-3',
          attributes: {
            'chatman.comm.source_system': 'osa',
            'chatman.comm.target_system': 'businessos',
            'chatman.comm.trace_id': 'trace-loop-1',
            'chatman.invariant.initial_trace_id': 'trace-loop-1',
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.workflow_id': 'workflow-456'
          }
        }
      ];

      const result = await validator.validateClosedLoopCompletion(spans, []);

      expect(result.valid).toBe(true);
    });

    it('fails when loop missing osa system', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.comm.source_system': 'businessos',
            'chatman.comm.target_system': 'canopy',
            'chatman.invariant.initial_trace_id': 'trace-incomplete'
          }
        }
        // No OSA span!
      ];

      const result = await validator.validateClosedLoopCompletion(spans, []);

      expect(result.valid).toBe(false);
      expect(result.violations[0].code).toBe('loop.violation.incomplete_chain');
    });
  });

  describe('Invariant 6: Minimality', () => {
    it('passes when no redundant paths exist', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          name: 'operation.create_deal',
          attributes: {
            'chatman.invariant.operation_hash': 'hash-unique-1'
          }
        },
        {
          span_id: 'span-2',
          name: 'operation.validate_deal',
          attributes: {
            'chatman.invariant.operation_hash': 'hash-unique-2'
          }
        }
      ];

      const result = await validator.validateMinimality(spans, []);

      expect(result.valid).toBe(true);
    });

    it('fails when duplicate operation with same inputs exists', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          name: 'operation.create_deal',
          attributes: {
            'chatman.invariant.operation_hash': 'hash-duplicate'
          }
        },
        {
          span_id: 'span-2',
          name: 'operation.create_deal',  // Same operation
          attributes: {
            'chatman.invariant.operation_hash': 'hash-duplicate'  // Same inputs
          }
        }
      ];

      const result = await validator.validateMinimality(spans, []);

      expect(result.valid).toBe(false);
      expect(result.violations[0].code).toBe('minimality.violation.redundant_path');
    });
  });

  describe('Invariant 7: Semantic Validity', () => {
    it('passes when all spans have valid validation status', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.invariant.validation_status': 'valid',
            'chatman.invariant.schema_id': 'https://ontology.businessos.dev/fibo-deals',
            'chatman.invariant.constraint_id': 'DealShape'
          }
        }
      ];

      const result = await validator.validateSemanticValidity(spans, []);

      expect(result.valid).toBe(true);
    });

    it('fails when span marked valid but missing schema_id', async () => {
      const validator = new SemanticInvariantsValidator();

      const spans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.invariant.validation_status': 'valid'
            // Missing schema_id!
          }
        }
      ];

      const result = await validator.validateSemanticValidity(spans, []);

      expect(result.valid).toBe(false);
      expect(result.violations[0].code).toBe('validity.violation.missing_schema');
    });
  });

  describe('Full Trace Validation', () => {
    it('validates all 7 invariants and returns aggregate result', async () => {
      const validator = new SemanticInvariantsValidator({
        observationWindowMs: 300000,
        enableBlocking: false  // Don't throw, just report violations
      });

      // Create a valid trace with complete data for all 7 invariants
      const now = Date.now();
      const spans = [
        {
          span_id: 'span-1',
          name: 'businessos.create_deal',
          start_time: now - 10000,
          end_time: now - 5000,
          attributes: {
            'chatman.comm.trace_id': 'trace-full-test',
            'chatman.comm.source_system': 'businessos',
            'chatman.comm.target_system': 'canopy',
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.workflow_id': 'workflow-456',
            'chatman.invariant.case_id': 'case-789',
            'chatman.invariant.ocel_mapping_id': 'map-123',
            'chatman.invariant.isomorphism_hash': 'blake3:abc123',
            'chatman.invariant.state_before': 'running',
            'chatman.invariant.state_after': 'completed',
            'chatman.invariant.validation_status': 'valid',
            'chatman.invariant.schema_id': 'https://ontology.businessos.dev/fibo-deals',
            'chatman.invariant.constraint_id': 'DealShape',
            'chatman.invariant.operation_hash': 'hash-unique-1',
            'chatman.invariant.initial_trace_id': 'trace-full-test'
            // Note: replay_id and output_hash removed - these are for replay determinism tests only
          }
        },
        {
          span_id: 'span-2',
          name: 'canopy.process_deal',
          start_time: now - 5000,
          end_time: now - 2000,
          attributes: {
            'chatman.comm.trace_id': 'trace-full-test',
            'chatman.comm.source_system': 'canopy',
            'chatman.comm.target_system': 'osa',
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.workflow_id': 'workflow-456',
            'chatman.invariant.case_id': 'case-789',
            'chatman.invariant.ocel_mapping_id': 'map-456',
            'chatman.invariant.isomorphism_hash': 'blake3:def456',
            'chatman.invariant.state_before': 'pending',
            'chatman.invariant.state_after': 'running',
            'chatman.invariant.validation_status': 'valid',
            'chatman.invariant.schema_id': 'https://ontology.businessos.dev/fibo-deals',
            'chatman.invariant.constraint_id': 'DealShape',
            'chatman.invariant.operation_hash': 'hash-unique-2',
            'chatman.invariant.initial_trace_id': 'trace-full-test'
            // Note: replay_id and output_hash removed - these are for replay determinism tests only
          }
        },
        {
          span_id: 'span-3',
          name: 'osa.finalize_deal',
          start_time: now - 2000,
          end_time: now - 1000,
          attributes: {
            'chatman.comm.trace_id': 'trace-full-test',
            'chatman.comm.source_system': 'osa',
            'chatman.comm.target_system': 'businessos',
            'chatman.invariant.task_id': 'task-123',
            'chatman.invariant.workflow_id': 'workflow-456',
            'chatman.invariant.case_id': 'case-789',
            'chatman.invariant.ocel_mapping_id': 'map-789',
            'chatman.invariant.isomorphism_hash': 'blake3:ghi789',
            'chatman.invariant.state_before': 'running',
            'chatman.invariant.state_after': 'completed',
            'chatman.invariant.validation_status': 'valid',
            'chatman.invariant.schema_id': 'https://ontology.businessos.dev/fibo-deals',
            'chatman.invariant.constraint_id': 'DealShape',
            'chatman.invariant.operation_hash': 'hash-unique-3',
            'chatman.invariant.initial_trace_id': 'trace-full-test'
            // Note: replay_id and output_hash removed - these are for replay determinism tests only
          }
        }
      ];

      const ocelEvents = [
        {
          id: 'event-1',
          activity: 'create_deal',
          timestamp: now - 10000,
          attributes: {
            'chatman.invariant.ocel_mapping_id': 'map-123',
            'chatman.invariant.isomorphism_hash': 'blake3:abc123'
          }
        },
        {
          id: 'event-2',
          activity: 'process_deal',
          timestamp: now - 5000,
          attributes: {
            'chatman.invariant.ocel_mapping_id': 'map-456',
            'chatman.invariant.isomorphism_hash': 'blake3:def456'
          }
        },
        {
          id: 'event-3',
          activity: 'finalize_deal',
          timestamp: now - 2000,
          attributes: {
            'chatman.invariant.ocel_mapping_id': 'map-789',
            'chatman.invariant.isomorphism_hash': 'blake3:ghi789'
          }
        }
      ];

      const result = await validator.validateTrace(spans, ocelEvents);

      expect(result.valid).toBe(true);
      expect(result.violations).toHaveLength(0);
      expect(Object.keys(result.checks)).toHaveLength(7);
    });

    it('throws InvariantViolation when enableBlocking=true and violation detected', async () => {
      const validator = new SemanticInvariantsValidator({
        enableBlocking: true
      });

      const spans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.comm.trace_id': 'trace-violation',
            'chatman.invariant.task_id': 'task-1'
          }
        },
        {
          span_id: 'span-2',
          attributes: {
            'chatman.comm.trace_id': 'trace-violation',
            'chatman.invariant.task_id': 'task-999'  // Violation!
          }
        }
      ];

      await expect(
        validator.validateTrace(spans, [])
      ).rejects.toThrow(InvariantViolation);
    });
  });

  describe('Convenience Function', () => {
    it('provides simple validateSemanticInvariants function', async () => {
      const spans = [
        {
          span_id: 'span-1',
          attributes: {
            'chatman.comm.trace_id': 'trace-quick',
            'chatman.invariant.task_id': 'task-1',
            'chatman.invariant.validation_status': 'valid',
            'chatman.invariant.schema_id': 'https://ontology.example.com/test'
          }
        }
      ];

      const result = await validateSemanticInvariants(spans, [], {
        enableBlocking: false
      });

      expect(result.valid).toBe(true);
    });
  });
});
