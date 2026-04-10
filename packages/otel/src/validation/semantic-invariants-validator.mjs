/**
 * Semantic Invariants Validator — Formal μ(O) Validation Layer
 *
 * Implements the 7 semantic invariants from Weaver constraint spec:
 * 1. Semantic Causality (task_id/workflow_id/case_id stability)
 * 2. OTel-OCEL Isomorphism (1:1 mapping, lossless)
 * 3. Temporal Consistency (no orphan tasks, broken workflows)
 * 4. Replay Determinism (hash equivalence)
 * 5. Closed-Loop Completion (full semantic loops)
 * 6. Minimality (no redundant paths)
 * 7. Semantic Validity (schema/constraint compliance)
 *
 * This is the LAW enforcement layer — not just logging, but blocking violations.
 */

import {
  ATTR_CHATMAN_INVARIANT_TASK_ID,
  ATTR_CHATMAN_INVARIANT_WORKFLOW_ID,
  ATTR_CHATMAN_INVARIANT_CASE_ID,
  ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID,
  ATTR_CHATMAN_INVARIANT_ISOMORPHISM_HASH,
  ATTR_CHATMAN_INVARIANT_OBSERVATION_WINDOW_MS,
  ATTR_CHATMAN_INVARIANT_STATE_BEFORE,
  ATTR_CHATMAN_INVARIANT_STATE_AFTER,
  ATTR_CHATMAN_INVARIANT_REPLAY_ID,
  ATTR_CHATMAN_INVARIANT_OUTPUT_HASH,
  ATTR_CHATMAN_INVARIANT_LOOP_CHAIN,
  ATTR_CHATMAN_INVARIANT_INITIAL_TRACE_ID,
  ATTR_CHATMAN_INVARIANT_OPERATION_HASH,
  ATTR_CHATMAN_INVARIANT_SEMANTIC_HASH,
  ATTR_CHATMAN_INVARIANT_SCHEMA_ID,
  ATTR_CHATMAN_INVARIANT_CONSTRAINT_ID,
  ATTR_CHATMAN_INVARIANT_VALIDATION_STATUS
} from '../generated/invariant-attributes.mjs';

import {
  ATTR_CHATMAN_COMM_TRACE_ID,
  ATTR_CHATMAN_COMM_SOURCE_SYSTEM,
  ATTR_CHATMAN_COMM_TARGET_SYSTEM
} from '../generated/attributes.mjs';

import { createHash } from 'crypto';

/**
 * Violation Error — thrown when invariant check fails
 */
export class InvariantViolation extends Error {
  constructor(invariantType, violationCode, details) {
    super(`Invariant violation: ${invariantType}::${violationCode} - ${details}`);
    this.name = 'InvariantViolation';
    this.invariantType = invariantType;
    this.violationCode = violationCode;
    this.details = details;
  }
}

/**
 * Semantic Invariants Validator
 */
export class SemanticInvariantsValidator {
  constructor(options = {}) {
    this.observationWindowMs = options.observationWindowMs ?? 300000; // 5 minutes default
    this.enableBlocking = options.enableBlocking ?? true; // Block violations or just warn
    this.validators = [
      this.validateSemanticCausality.bind(this),
      this.validateOtelOcelIsomorphism.bind(this),
      this.validateTemporalConsistency.bind(this),
      this.validateReplayDeterminism.bind(this),
      this.validateClosedLoopCompletion.bind(this),
      this.validateMinimality.bind(this),
      this.validateSemanticValidity.bind(this)
    ];
  }

  /**
   * Validate all spans in a trace against semantic invariants
   */
  async validateTrace(spans, ocelEvents, options = {}) {
    const results = {
      valid: true,
      violations: [],
      checks: {}
    };

    // Merge instance options with call-time options
    const mergedOptions = {
      ...options,
      observationWindowMs: options.observationWindowMs ?? this.observationWindowMs
    };

    for (const validator of this.validators) {
      const checkResult = await validator(spans, ocelEvents, mergedOptions);
      results.checks[checkResult.name] = checkResult;

      // Debug: log validation results

      if (!checkResult.valid) {
        results.valid = false;
        results.violations.push(...checkResult.violations);

        if (this.enableBlocking) {
          throw new InvariantViolation(
            checkResult.name,
            checkResult.violations[0].code,
            checkResult.violations[0].message
          );
        }
      }
    }

    return results;
  }

  /**
   * Invariant 1: Semantic Causality
   * task_id, workflow_id, case_id MUST be stable across all systems in trace
   */
  async validateSemanticCausality(spans, ocelEvents, options = {}) {
    const violations = [];
    const traceGroups = new Map();

    // Group spans by trace_id
    for (const span of spans) {
      const traceId = span.attributes?.[ATTR_CHATMAN_COMM_TRACE_ID];
      if (!traceId) continue;

      if (!traceGroups.has(traceId)) {
        traceGroups.set(traceId, []);
      }
      traceGroups.get(traceId).push(span);
    }

    // Debug: Check what we found

    // Check semantic identifier stability within each trace
    for (const [traceId, traceSpans] of traceGroups) {
      const taskIds = new Set();
      const workflowIds = new Set();
      const caseIds = new Set();

      for (const span of traceSpans) {
        const taskId = span.attributes?.[ATTR_CHATMAN_INVARIANT_TASK_ID];
        const workflowId = span.attributes?.[ATTR_CHATMAN_INVARIANT_WORKFLOW_ID];
        const caseId = span.attributes?.[ATTR_CHATMAN_INVARIANT_CASE_ID];

        if (taskId) taskIds.add(taskId);
        if (workflowId) workflowIds.add(workflowId);
        if (caseId) caseIds.add(caseId);
      }


      // Invariant: task_id must be identical across all spans in trace
      if (taskIds.size > 1) {
        violations.push({
          code: 'semantic.causality.violation.task_id_mismatch',
          message: `Trace ${traceId} has ${taskIds.size} distinct task_ids: ${Array.from(taskIds).join(', ')}`,
          severity: 'critical',
          traceId
        });
      }

      // Invariant: workflow_id must be identical across all spans in trace
      if (workflowIds.size > 1) {
        violations.push({
          code: 'semantic.causality.violation.workflow_id_mismatch',
          message: `Trace ${traceId} has ${workflowIds.size} distinct workflow_ids: ${Array.from(workflowIds).join(', ')}`,
          severity: 'critical',
          traceId
        });
      }

      // Invariant: case_id must be stable (no duplicates across different traces)
      if (caseIds.size > 1 && options.checkCrossTraceDuplication) {
        violations.push({
          code: 'semantic.causality.violation.case_id_instability',
          message: `Trace ${traceId} has ${caseIds.size} distinct case_ids: ${Array.from(caseIds).join(', ')}`,
          severity: 'critical',
          traceId
        });
      }
    }

    return {
      name: 'semantic_causality',
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Invariant 2: OTel-OCEL Isomorphism
   * 1:1 mapping between OTel spans and OCEL events, lossless conversion
   */
  async validateOtelOcelIsomorphism(spans, ocelEvents, options = {}) {
    const violations = [];
    const traceId = spans[0]?.attributes?.[ATTR_CHATMAN_COMM_TRACE_ID];

    // If no OCEL events provided, only check for orphan spans with mapping_id
    if (!ocelEvents || ocelEvents.length === 0) {
      for (const span of spans) {
        const mappingId = span.attributes?.[ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID];
        if (mappingId) {
          violations.push({
            code: 'isomorphism.violation.orphan_otel_span',
            message: `OTel span ${span.span_id} has no corresponding OCEL event (no OCEL events provided, mapping_id: ${mappingId})`,
            severity: 'critical',
            traceId,
            spanId: span.span_id
          });
        }
      }

      return {
        name: 'otel_ocel_isomorphism',
        valid: violations.length === 0,
        violations
      };
    }

    // Check 1: Count bijection (same number of spans and events)
    if (spans.length !== ocelEvents.length) {
      violations.push({
        code: 'isomorphism.violation.count_mismatch',
        message: `Trace ${traceId}: ${spans.length} OTel spans vs ${ocelEvents.length} OCEL events (not 1:1)`,
        severity: 'critical',
        traceId
      });
    }

    // Check 2: All OTel spans have corresponding OCEL event
    const spanMappingIds = new Set(spans.map(s => s.attributes?.[ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID]).filter(Boolean));
    const ocelMappingIds = new Set(ocelEvents.map(e => e.attributes?.[ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID] || e.id));

    for (const span of spans) {
      const mappingId = span.attributes?.[ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID];
      if (mappingId && !ocelMappingIds.has(mappingId)) {
        violations.push({
          code: 'isomorphism.violation.orphan_otel_span',
          message: `OTel span ${span.span_id} has no corresponding OCEL event (mapping_id: ${mappingId})`,
          severity: 'critical',
          traceId,
          spanId: span.span_id
        });
      }
    }

    // Check 3: All OCEL events have corresponding OTel span
    for (const event of ocelEvents) {
      const mappingId = event.attributes?.[ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID] || event.id;
      if (mappingId && !spanMappingIds.has(mappingId)) {
        violations.push({
          code: 'isomorphism.violation.orphan_ocel_event',
          message: `OCEL event ${event.id} has no corresponding OTel span (mapping_id: ${mappingId})`,
          severity: 'critical',
          traceId,
          eventId: event.id
        });
      }
    }

    // Check 4: Isomorphism hash matches (semantic equivalence)
    for (const span of spans) {
      const mappingId = span.attributes?.[ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID];
      const ocelEvent = ocelEvents.find(e => (e.attributes?.[ATTR_CHATMAN_INVARIANT_OCEL_MAPPING_ID] || e.id) === mappingId);

      if (ocelEvent) {
        const spanHash = span.attributes?.[ATTR_CHATMAN_INVARIANT_ISOMORPHISM_HASH];
        const ocelHash = ocelEvent.attributes?.[ATTR_CHATMAN_INVARIANT_ISOMORPHISM_HASH];

        if (spanHash && ocelHash && spanHash !== ocelHash) {
          violations.push({
            code: 'isomorphism.violation.semantic_drift',
            message: `Span ${span.span_id} and OCEL event ${ocelEvent.id} have different hashes (OTel: ${spanHash}, OCEL: ${ocelHash})`,
            severity: 'critical',
            traceId,
            spanId: span.span_id,
            eventId: ocelEvent.id
          });
        }
      }
    }

    return {
      name: 'otel_ocel_isomorphism',
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Invariant 3: Temporal Consistency
   * No orphan tasks, no broken workflows, no duplicate identifiers
   */
  async validateTemporalConsistency(spans, ocelEvents, options = {}) {
    const violations = [];
    const now = Date.now();
    const observationWindow = (options ?? {}).observationWindowMs ?? this.observationWindowMs;

    // Group spans by process identifiers
    const tasks = new Map(); // task_id -> {start, end, state, spans}
    const workflows = new Map(); // workflow_id -> {start, end, state, spans}

    for (const span of spans) {
      const taskId = span.attributes?.[ATTR_CHATMAN_INVARIANT_TASK_ID];
      const workflowId = span.attributes?.[ATTR_CHATMAN_INVARIANT_WORKFLOW_ID];
      const stateBefore = span.attributes?.[ATTR_CHATMAN_INVARIANT_STATE_BEFORE];
      const stateAfter = span.attributes?.[ATTR_CHATMAN_INVARIANT_STATE_AFTER];

      if (taskId) {
        if (!tasks.has(taskId)) {
          tasks.set(taskId, { start: span.start_time, end: span.end_time, state: stateAfter, spans: [] });
        }
        const task = tasks.get(taskId);
        task.spans.push(span);
        if (span.end_time > task.end) task.end = span.end_time;
        if (stateAfter) task.state = stateAfter;
      }

      if (workflowId) {
        if (!workflows.has(workflowId)) {
          workflows.set(workflowId, { start: span.start_time, end: span.end_time, state: stateAfter, spans: [] });
        }
        const workflow = workflows.get(workflowId);
        workflow.spans.push(span);
        if (span.end_time > workflow.end) workflow.end = span.end_time;
        if (stateAfter) workflow.state = stateAfter;
      }
    }

    // Check 1: No orphan tasks (started but not completed within observation window)
    for (const [taskId, task] of tasks) {
      // Skip tasks without start time (can't determine age)
      if (!task.start) continue;

      const age = now - task.start;
      const isTerminal = ['completed', 'failed', 'cancelled'].includes(task.state);


      if (!isTerminal && age > observationWindow) {
        violations.push({
          code: 'temporal.violation.orphan_task',
          message: `Task ${taskId} is orphaned (state: ${task.state}, age: ${age}ms > ${observationWindow}ms)`,
          severity: 'critical',
          taskId
        });
      }
    }

    // Check 2: No duplicate identifiers in observation window
    // Use operation_hash + task_id as signature to distinguish different operations
    const operationSignatures = new Map(); // (task_id, operation_hash) -> [timestamps]
    for (const span of spans) {
      const taskId = span.attributes?.[ATTR_CHATMAN_INVARIANT_TASK_ID];
      const operationHash = span.attributes?.[ATTR_CHATMAN_INVARIANT_OPERATION_HASH];
      const signature = `${taskId}:${operationHash || span.name}`;

      if (taskId) {
        if (!operationSignatures.has(signature)) {
          operationSignatures.set(signature, []);
        }
        operationSignatures.get(signature).push(span.start_time);
      }
    }

    for (const [signature, timestamps] of operationSignatures) {
      // Check for duplicates within observation window
      for (let i = 0; i < timestamps.length; i++) {
        for (let j = i + 1; j < timestamps.length; j++) {
          const diff = Math.abs(timestamps[j] - timestamps[i]);
          if (diff < observationWindow) {
            violations.push({
              code: 'temporal.violation.duplicate_identifier',
              message: `Duplicate executions within observation window (${diff}ms apart) for ${signature}`,
              severity: 'critical',
              signature,
              timestamp1: timestamps[i],
              timestamp2: timestamps[j]
            });
          }
        }
      }
    }

    // Check 3: Valid state transitions (state machine validation)
    const validTransitions = {
      pending: ['running', 'cancelled'],
      running: ['completed', 'failed', 'waiting'],
      waiting: ['running', 'cancelled'],
      completed: [], // Terminal state
      failed: [], // Terminal state
      cancelled: [] // Terminal state
    };

    for (const [taskId, task] of tasks) {
      for (const span of task.spans) {
        const stateBefore = span.attributes?.[ATTR_CHATMAN_INVARIANT_STATE_BEFORE];
        const stateAfter = span.attributes?.[ATTR_CHATMAN_INVARIANT_STATE_AFTER];

        if (stateBefore && stateAfter) {
          const allowed = validTransitions[stateBefore] || [];
          if (!allowed.includes(stateAfter)) {
            violations.push({
              code: 'temporal.violation.invalid_state_transition',
              message: `Task ${taskId} has invalid state transition: ${stateBefore} → ${stateAfter}`,
              severity: 'critical',
              taskId,
              stateBefore,
              stateAfter
            });
          }
        }
      }
    }

    return {
      name: 'temporal_consistency',
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Invariant 4: Replay Determinism
   * Same inputs → same outputs (hash equivalence)
   */
  async validateReplayDeterminism(spans, ocelEvents, options = {}) {
    const violations = [];

    // Case 1: ocelEvents is actually replaySpans array (separate original/replay)
    // This happens when tests call validateReplayDeterminism(originalSpans, replaySpans)
    // Detect by checking if ocelEvents array contains span-like objects with replay_id attributes
    const hasReplayIds = ocelEvents && ocelEvents.length > 0 && ocelEvents.some(
      event => event.attributes?.[ATTR_CHATMAN_INVARIANT_REPLAY_ID]
    );

    if (hasReplayIds) {
      const originalSpans = spans;
      const replaySpans = ocelEvents;

      // Group original and replay spans by replay_id
      const originalByReplayId = new Map();
      for (const span of originalSpans) {
        const replayId = span.attributes?.[ATTR_CHATMAN_INVARIANT_REPLAY_ID];
        if (replayId) {
          originalByReplayId.set(replayId, span);
        }
      }

      for (const replaySpan of replaySpans) {
        const replayId = replaySpan.attributes?.[ATTR_CHATMAN_INVARIANT_REPLAY_ID];
        const originalSpan = originalByReplayId.get(replayId);

        if (!originalSpan) {
          violations.push({
            code: 'determinism.violation.missing_original',
            message: `Replay span ${replaySpan.span_id} has no corresponding original execution (replay_id: ${replayId})`,
            severity: 'critical',
            replayId,
            spanId: replaySpan.span_id
          });
          continue;
        }

        // Check 1: Output hash equivalence
        const originalHash = originalSpan.attributes?.[ATTR_CHATMAN_INVARIANT_OUTPUT_HASH];
        const replayHash = replaySpan.attributes?.[ATTR_CHATMAN_INVARIANT_OUTPUT_HASH];

        if (originalHash !== replayHash) {
          violations.push({
            code: 'determinism.violation.hash_mismatch',
            message: `Replay hash mismatch for replay_id ${replayId} (original: ${originalHash}, replay: ${replayHash})`,
            severity: 'critical',
            replayId,
            originalHash,
            replayHash
          });
        }
      }

      return {
        name: 'replay_determinism',
        valid: violations.length === 0,
        violations
      };
    }

    // Case 2: Replay spans are mixed with original spans (same array)
    // Extract spans with replay_id attribute (replays are marked within the same trace)
    const replayIds = new Set();
    for (const span of spans) {
      const replayId = span.attributes?.[ATTR_CHATMAN_INVARIANT_REPLAY_ID];
      if (replayId) {
        replayIds.add(replayId);
      }
    }

    // If no replay_id found, skip validation (no replay being performed)
    if (replayIds.size === 0) {
      return {
        name: 'replay_determinism',
        valid: true,
        violations: []
      };
    }

    // Group spans by replay_id
    const spansByReplayId = new Map();
    for (const span of spans) {
      const replayId = span.attributes?.[ATTR_CHATMAN_INVARIANT_REPLAY_ID];
      if (replayId) {
        if (!spansByReplayId.has(replayId)) {
          spansByReplayId.set(replayId, []);
        }
        spansByReplayId.get(replayId).push(span);
      }
    }

    // For each replay_id, verify all spans with that id have identical hashes
    for (const [replayId, replayGroup] of spansByReplayId) {
      if (replayGroup.length < 2) {
        // Only one execution with this replay_id, no replay to validate
        continue;
      }

      // Check output hash equivalence across all executions
      const outputHashes = new Set();
      for (const span of replayGroup) {
        const outputHash = span.attributes?.[ATTR_CHATMAN_INVARIANT_OUTPUT_HASH];
        if (outputHash) {
          outputHashes.add(outputHash);
        }
      }

      if (outputHashes.size > 1) {
        violations.push({
          code: 'determinism.violation.hash_mismatch',
          message: `Replay hash mismatch for replay_id ${replayId} (${outputHashes.size} different hashes: ${Array.from(outputHashes).join(', ')})`,
          severity: 'critical',
          replayId,
          hashes: Array.from(outputHashes)
        });
      }
    }

    return {
      name: 'replay_determinism',
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Invariant 5: Closed-Loop Completion
   * BusinessOS → Canopy → OSA → BusinessOS with preserved semantic identifiers
   */
  async validateClosedLoopCompletion(spans, ocelEvents, options = {}) {
    const violations = [];
    const requiredSystems = ['businessos', 'canopy', 'osa'];

    // Group spans by initial_trace_id
    const loopChains = new Map(); // initial_trace_id -> {businessos: [], canopy: [], osa: []}

    for (const span of spans) {
      const sourceSystem = span.attributes?.[ATTR_CHATMAN_COMM_SOURCE_SYSTEM];
      const targetSystem = span.attributes?.[ATTR_CHATMAN_COMM_TARGET_SYSTEM];
      const initialTraceId = span.attributes?.[ATTR_CHATMAN_INVARIANT_INITIAL_TRACE_ID];

      if (!initialTraceId) continue;

      if (!loopChains.has(initialTraceId)) {
        loopChains.set(initialTraceId, { businessos: [], canopy: [], osa: [] });
      }
      const chain = loopChains.get(initialTraceId);

      // Add span based on its source system
      if (sourceSystem && chain[sourceSystem]) {
        chain[sourceSystem].push(span);
      }
    }

    // Check each loop chain for completeness
    for (const [initialTraceId, chain] of loopChains) {
      // Check 1: All required systems present
      for (const system of requiredSystems) {
        if (chain[system].length === 0) {
          violations.push({
            code: 'loop.violation.incomplete_chain',
            message: `Loop ${initialTraceId} missing system: ${system} (has: businessos=${chain.businessos.length}, canopy=${chain.canopy.length}, osa=${chain.osa.length})`,
            severity: 'critical',
            initialTraceId
          });
        }
      }

      // Check 2: Semantic preservation (task_id/workflow_id stable across loop)
      const taskIds = new Set();
      const workflowIds = new Set();

      for (const system of requiredSystems) {
        for (const span of chain[system]) {
          const taskId = span.attributes?.[ATTR_CHATMAN_INVARIANT_TASK_ID];
          const workflowId = span.attributes?.[ATTR_CHATMAN_INVARIANT_WORKFLOW_ID];

          if (taskId) taskIds.add(taskId);
          if (workflowId) workflowIds.add(workflowId);
        }
      }

      if (taskIds.size > 1) {
        violations.push({
          code: 'loop.violation.semantic_drift',
          message: `Loop ${initialTraceId} has ${taskIds.size} distinct task_ids: ${Array.from(taskIds).join(', ')}`,
          severity: 'critical',
          initialTraceId
        });
      }

      if (workflowIds.size > 1) {
        violations.push({
          code: 'loop.violation.semantic_drift',
          message: `Loop ${initialTraceId} has ${workflowIds.size} distinct workflow_ids: ${Array.from(workflowIds).join(', ')}`,
          severity: 'critical',
          initialTraceId
        });
      }

      // Check 3: Response correlation (OSA response correlates to BusinessOS request)
      const businessosRequest = chain.businessos[0];
      const osaResponse = chain.osa[chain.osa.length - 1];

      if (businessosRequest && osaResponse) {
        const businessosTrace = businessosRequest.attributes?.[ATTR_CHATMAN_COMM_TRACE_ID];
        const osaTrace = osaResponse.attributes?.[ATTR_CHATMAN_COMM_TRACE_ID];

        // OSA should have trace_id referencing BusinessOS request
        if (businessosTrace && osaTrace && osaTrace !== businessosTrace) {
          violations.push({
            code: 'loop.violation.missing_response',
            message: `Loop ${initialTraceId}: OSA response trace_id (${osaTrace}) does not match BusinessOS request trace_id (${businessosTrace})`,
            severity: 'critical',
            initialTraceId,
            businessosTrace,
            osaTrace
          });
        }
      }
    }

    return {
      name: 'closed_loop_completion',
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Invariant 6: Minimality
   * No redundant paths, no duplicate events, no unnecessary transformations
   */
  async validateMinimality(spans, ocelEvents, options = {}) {
    const violations = [];

    // Check 1: Redundant path detection (same operation + inputs in same trace)
    const operationSignatures = new Map(); // (operation, inputs_hash) -> [span_ids]
    for (const span of spans) {
      const operation = span.name;
      const inputsHash = span.attributes?.[ATTR_CHATMAN_INVARIANT_OPERATION_HASH];
      const signature = `${operation}:${inputsHash}`;

      if (!operationSignatures.has(signature)) {
        operationSignatures.set(signature, []);
      }
      operationSignatures.get(signature).push(span.span_id);
    }

    for (const [signature, spanIds] of operationSignatures) {
      if (spanIds.length > 1) {
        violations.push({
          code: 'minimality.violation.redundant_path',
          message: `Redundant execution path: ${spanIds.length} spans with same signature ${signature}`,
          severity: 'high',
          signature,
          spanIds
        });
      }
    }

    // Check 2: Duplicate event detection (same activity + objectIds + timestamp)
    if (ocelEvents && ocelEvents.length > 0) {
      const eventSignatures = new Map(); // (activity, objectIds, timestamp) -> [event_ids]
      for (const event of ocelEvents) {
        const activity = event.activity;
        const objectIds = JSON.stringify(event.objectIds?.sort());
        const timestamp = event.timestamp;
        const signature = `${activity}:${objectIds}:${timestamp}`;

        if (!eventSignatures.has(signature)) {
          eventSignatures.set(signature, []);
        }
        eventSignatures.get(signature).push(event.id);
      }

      for (const [signature, eventIds] of eventSignatures) {
        if (eventIds.length > 1) {
          violations.push({
            code: 'minimality.violation.duplicate_event',
            message: `Duplicate OCEL events: ${eventIds.length} events with same signature ${signature}`,
            severity: 'high',
            signature,
            eventIds
          });
        }
      }
    }

    // Check 3: Unnecessary transformation detection (no semantic change)
    for (const span of spans) {
      const stateBefore = span.attributes?.[ATTR_CHATMAN_INVARIANT_STATE_BEFORE];
      const stateAfter = span.attributes?.[ATTR_CHATMAN_INVARIANT_STATE_AFTER];

      if (stateBefore && stateAfter && stateBefore === stateAfter) {
        violations.push({
          code: 'minimality.violation.noop_transformation',
          message: `Span ${span.span_id} performs identity transformation (${stateBefore} → ${stateAfter})`,
          severity: 'high',
          spanId: span.span_id,
          stateBefore,
          stateAfter
        });
      }
    }

    return {
      name: 'minimality',
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Invariant 7: Semantic Validity
   * All transformations valid against schemas/constraints/ontologies
   */
  async validateSemanticValidity(spans, ocelEvents, options = {}) {
    const violations = [];

    for (const span of spans) {
      const schemaId = span.attributes?.[ATTR_CHATMAN_INVARIANT_SCHEMA_ID];
      const constraintId = span.attributes?.[ATTR_CHATMAN_INVARIANT_CONSTRAINT_ID];
      const validationStatus = span.attributes?.[ATTR_CHATMAN_INVARIANT_VALIDATION_STATUS];

      // Check 1: Validation status is set
      if (!validationStatus) {
        violations.push({
          code: 'validity.violation.missing_validation',
          message: `Span ${span.span_id} has no validation status attribute`,
          severity: 'critical',
          spanId: span.span_id
        });
        continue;
      }

      // Check 2: Failed validations are marked correctly
      if (validationStatus === 'invalid' || validationStatus === 'error') {
        violations.push({
          code: 'validity.violation.semantic_error',
          message: `Span ${span.span_id} failed semantic validation (status: ${validationStatus}, schema: ${schemaId}, constraint: ${constraintId})`,
          severity: 'critical',
          spanId: span.span_id,
          validationStatus,
          schemaId,
          constraintId
        });
      }

      // Check 3: Schema and constraint IDs are present for validated spans
      if (validationStatus === 'valid') {
        if (!schemaId) {
          violations.push({
            code: 'validity.violation.missing_schema',
            message: `Span ${span.span_id} marked valid but missing schema_id`,
            severity: 'critical',
            spanId: span.span_id
          });
        }
      }
    }

    return {
      name: 'semantic_validity',
      valid: violations.length === 0,
      violations
    };
  }

  /**
   * Helper: Extract semantic attributes (excluding ignored ones)
   */
  _extractSemanticAttributes(attributes, ignoreAttrs) {
    const result = {};
    for (const [key, value] of Object.entries(attributes || {})) {
      if (!ignoreAttrs.has(key)) {
        result[key] = value;
      }
    }
    return result;
  }
}

/**
 * Singleton validator instance
 */
export const validator = new SemanticInvariantsValidator();

/**
 * Convenience function: Validate a trace
 */
export async function validateSemanticInvariants(spans, ocelEvents, options) {
  return validator.validateTrace(spans, ocelEvents, options);
}
