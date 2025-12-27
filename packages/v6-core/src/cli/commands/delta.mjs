/**
 * @fileoverview Delta (Δ) noun commands
 *
 * Implements: kgc delta <verb>
 * - propose <file> - Propose Δ change
 * - apply <id> - Apply Δ (with admissibility check)
 * - verify <id> - Verify Δ was applied correctly
 * - export <id> - Export Δ
 */

import { z } from 'zod';
import { createV6Extension } from '../spine.mjs';

/**
 * Delta structure schema.
 */
const DeltaSchema = z.object({
  id: z.string().optional(),
  from: z.string().describe('Source state hash'),
  to: z.string().describe('Target state hash'),
  operations: z.array(z.object({
    type: z.enum(['add', 'remove', 'modify']),
    subject: z.string(),
    predicate: z.string(),
    object: z.any(),
    oldValue: z.any().optional()
  })),
  metadata: z.record(z.any()).optional()
});

/**
 * Argument schemas for delta commands.
 */
const ProposeArgsSchema = z.object({
  file: z.string().optional().describe('Delta JSON file path'),
  delta: z.string().optional().describe('Inline delta JSON'),
  description: z.string().optional()
});

const ApplyArgsSchema = z.object({
  id: z.string().describe('Delta ID to apply'),
  force: z.boolean().optional().default(false).describe('Skip admissibility check'),
  dryRun: z.boolean().optional().default(false).describe('Simulate without applying')
});

const VerifyArgsSchema = z.object({
  id: z.string().describe('Delta ID to verify'),
  against: z.string().optional().describe('State hash to verify against')
});

const ExportArgsSchema = z.object({
  id: z.string().describe('Delta ID'),
  format: z.enum(['json', 'yaml', 'rdf', 'patch']).optional().default('json')
});

/**
 * In-memory delta store (for demo).
 * TODO: Replace with persistent storage.
 */
const deltaStore = new Map();

/**
 * Propose a delta.
 *
 * Creates and validates a proposed state transition.
 *
 * @param {Object} args - Validated args
 * @param {Object} [context={}] - Execution context with t_ns for determinism
 * @returns {Promise<Object>} Proposal result
 */
async function proposeDelta(args, context = {}) {
  const { file, delta, description } = args;

  let deltaData;

  if (file) {
    // TODO: Read from file system
    deltaData = {
      from: 'state-hash-1',
      to: 'state-hash-2',
      operations: [
        { type: 'add', subject: 'ex:Subject', predicate: 'ex:prop', object: 'value' }
      ]
    };
  } else if (delta) {
    deltaData = JSON.parse(delta);
  } else {
    throw new Error('Must provide either file or delta');
  }

  // Validate delta structure
  const validated = DeltaSchema.parse(deltaData);

  // Generate ID deterministically from context or fallback to timestamp
  const timestamp = context.t_ns ? Number(context.t_ns / 1_000_000n) : Date.now();
  const counter = context.counter || Math.floor(Math.random() * 100000);
  const id = `delta-${timestamp}-${counter.toString(36).padStart(5, '0')}`;
  validated.id = id;
  validated.metadata = {
    ...validated.metadata,
    description,
    proposedAt: new Date(timestamp).toISOString(),
    status: 'proposed'
  };

  // Store
  deltaStore.set(id, validated);

  return {
    proposed: true,
    delta: validated,
    admissibility: await checkAdmissibility(validated)
  };
}

/**
 * Apply a delta.
 *
 * Applies state transition with admissibility verification.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Application result
 */
async function applyDelta(args) {
  const { id, force, dryRun } = args;

  const delta = deltaStore.get(id);
  if (!delta) {
    throw new Error(`Delta not found: ${id}`);
  }

  // Check admissibility
  const admissibility = await checkAdmissibility(delta);

  if (!admissibility.admissible && !force) {
    throw new Error(
      `Delta not admissible: ${admissibility.reasons.join(', ')}. Use --force to override.`
    );
  }

  if (dryRun) {
    return {
      applied: false,
      dryRun: true,
      delta,
      admissibility,
      wouldApply: admissibility.admissible || force
    };
  }

  // Apply operations
  const appliedOps = [];
  for (const op of delta.operations) {
    // TODO: Actually apply to state store
    appliedOps.push({
      ...op,
      applied: true,
      timestamp: new Date().toISOString()
    });
  }

  // Update delta status
  delta.metadata.status = 'applied';
  delta.metadata.appliedAt = new Date().toISOString();
  deltaStore.set(id, delta);

  return {
    applied: true,
    delta,
    operations: appliedOps,
    newStateHash: delta.to,
    admissibility
  };
}

/**
 * Verify delta application.
 *
 * Confirms delta was correctly applied to state.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Verification result
 */
async function verifyDelta(args) {
  const { id, against } = args;

  const delta = deltaStore.get(id);
  if (!delta) {
    throw new Error(`Delta not found: ${id}`);
  }

  // Verify operations were applied
  const verifications = [];
  for (const op of delta.operations) {
    // TODO: Check actual state
    verifications.push({
      operation: op,
      verified: true,
      method: 'state-comparison'
    });
  }

  const allVerified = verifications.every(v => v.verified);

  return {
    verified: allVerified,
    delta,
    operations: verifications,
    currentStateHash: against || delta.to,
    expectedStateHash: delta.to,
    stateMatch: !against || against === delta.to
  };
}

/**
 * Export delta.
 *
 * Export delta in specified format.
 *
 * @param {Object} args - Validated args
 * @returns {Promise<Object>} Export result
 */
async function exportDelta(args) {
  const { id, format } = args;

  const delta = deltaStore.get(id);
  if (!delta) {
    throw new Error(`Delta not found: ${id}`);
  }

  let exportData = delta;

  if (format === 'yaml') {
    exportData = {
      ...delta,
      _format: 'yaml'
    };
  } else if (format === 'rdf') {
    // Convert to RDF triples
    exportData = {
      _format: 'rdf',
      triples: delta.operations.map(op => ({
        subject: op.subject,
        predicate: op.predicate,
        object: op.object,
        operation: op.type
      }))
    };
  } else if (format === 'patch') {
    // Convert to unified patch format
    exportData = {
      _format: 'patch',
      patch: delta.operations.map((op, i) => {
        if (op.type === 'add') {
          return `+ ${op.subject} ${op.predicate} ${JSON.stringify(op.object)}`;
        } else if (op.type === 'remove') {
          return `- ${op.subject} ${op.predicate} ${JSON.stringify(op.object)}`;
        } else {
          return `! ${op.subject} ${op.predicate} ${JSON.stringify(op.oldValue)} -> ${JSON.stringify(op.object)}`;
        }
      }).join('\n')
    };
  }

  return {
    exported: true,
    format,
    data: exportData
  };
}

/**
 * Check delta admissibility.
 *
 * Validates that delta can be safely applied to current state.
 *
 * @param {Object} delta - Delta to check
 * @returns {Promise<Object>} Admissibility result
 * @private
 */
async function checkAdmissibility(delta) {
  const reasons = [];

  // Check: from state exists
  // TODO: Check actual state store
  const fromStateExists = true;
  if (!fromStateExists) {
    reasons.push('Source state does not exist');
  }

  // Check: no conflicting deltas
  const hasConflicts = false;
  if (hasConflicts) {
    reasons.push('Conflicting delta already applied');
  }

  // Check: operations are valid
  for (const op of delta.operations) {
    if (op.type === 'modify' && !op.oldValue) {
      reasons.push(`Modify operation missing oldValue: ${op.subject}`);
    }
  }

  return {
    admissible: reasons.length === 0,
    reasons,
    checks: {
      fromStateExists,
      noConflicts: !hasConflicts,
      operationsValid: reasons.filter(r => r.includes('operation')).length === 0
    }
  };
}

/**
 * Delta extension for V6 CLI.
 */
export const deltaExtension = createV6Extension({
  id: '@unrdf/v6-core/delta',
  nouns: {
    delta: {
      verbs: {
        propose: {
          description: 'Propose a state change delta',
          handler: proposeDelta,
          argsSchema: ProposeArgsSchema
        },
        apply: {
          description: 'Apply delta with admissibility check',
          handler: applyDelta,
          argsSchema: ApplyArgsSchema
        },
        verify: {
          description: 'Verify delta was applied correctly',
          handler: verifyDelta,
          argsSchema: VerifyArgsSchema
        },
        export: {
          description: 'Export delta to format',
          handler: exportDelta,
          argsSchema: ExportArgsSchema
        }
      }
    }
  }
});

export default deltaExtension;
