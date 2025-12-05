/**
 * KGC-4D Delta Submission - Server-Side mu(O) Validation
 *
 * Deltas are user intents submitted from the Browser (Shard).
 * The Server validates them against Knowledge Hooks and either:
 * - ACK: Accept and commit to Universe
 * - REJECT: Refuse with reason
 *
 * This is the "Check-In" operation in the Shard-Based Architecture.
 */

import { getUniverse, broadcastUpdate, GRAPHS, dataFactory, now, VectorClock } from './universe.mjs';
import { HookRegistry } from '@unrdf/kgc-4d';

/**
 * Initialize validation registry with domain-specific rules
 */
const hooks = new HookRegistry();

// Budget validation: must not exceed 100,000
hooks.register('http://kgc.io/ontology/budget', {
  validate: (value) => {
    const budget = parseInt(value, 10);
    if (isNaN(budget)) return { valid: false, reason: 'Budget must be a number' };
    if (budget < 0) return { valid: false, reason: 'Budget cannot be negative' };
    if (budget > 100000) return { valid: false, reason: 'Budget cannot exceed $100,000' };
    return { valid: true };
  },
});

// Status validation: must be one of allowed values
hooks.register('http://kgc.io/ontology/status', {
  validate: (value) => {
    const allowed = ['active', 'paused', 'completed', 'cancelled'];
    if (!allowed.includes(value)) {
      return { valid: false, reason: `Status must be one of: ${allowed.join(', ')}` };
    }
    return { valid: true };
  },
});

// Name validation: must not be empty
hooks.register('http://kgc.io/ontology/name', {
  validate: (value) => {
    if (!value || value.trim().length === 0) {
      return { valid: false, reason: 'Name cannot be empty' };
    }
    if (value.length > 100) {
      return { valid: false, reason: 'Name cannot exceed 100 characters' };
    }
    return { valid: true };
  },
});

// Title validation
hooks.register('http://kgc.io/ontology/title', {
  validate: (value) => {
    if (!value || value.trim().length === 0) {
      return { valid: false, reason: 'Title cannot be empty' };
    }
    return { valid: true };
  },
});

/**
 * Run validation hooks on a delta operation
 * @param {Object} operation
 * @returns {Object} { valid: boolean, reason?: string }
 */
function runValidationHooks(operation) {
  const predicate = operation.predicate.value || operation.predicate;
  return hooks.validate(predicate, operation.object.value);
}

/**
 * Submit a delta to the Universe
 *
 * @param {Object} delta - The delta to submit
 * @param {Array} delta.operations - Array of { type, subject, predicate, object }
 * @param {Object} delta.client_vector_clock - Client's vector clock for causality check
 * @param {string} delta.source - Client identifier
 * @returns {Promise<Object>} ACK or REJECT response
 */
export async function submitDelta(delta) {
  const store = await getUniverse();

  // 1. Causality check using vector clocks
  if (delta.client_vector_clock) {
    const clientClock = VectorClock.fromJSON(delta.client_vector_clock);
    const serverClock = store.vectorClock;

    // Check if client is behind (concurrent modification)
    const comparison = clientClock.compare(serverClock);
    if (comparison === -1) {
      // Client is behind - this is okay, we'll merge
      console.log('[Delta] Client clock is behind, merging...');
    }
  }

  // 2. Validate each operation through hooks
  const operations = delta.operations || [];
  for (const op of operations) {
    const validation = runValidationHooks(op);
    if (!validation.valid) {
      return {
        status: 'REJECT',
        reason: validation.reason,
        operation: op,
        t_ns: now().toString(),
      };
    }
  }

  // 3. Convert operations to internal delta format
  const deltas = operations.map((op) => {
    // Reconstruct subject
    const subject =
      op.subject.termType === 'BlankNode'
        ? dataFactory.blankNode(op.subject.value)
        : dataFactory.namedNode(op.subject.value);

    // Reconstruct predicate
    const predicate = dataFactory.namedNode(op.predicate.value);

    // Reconstruct object
    let object;
    if (op.object.termType === 'Literal') {
      if (op.object.language) {
        object = dataFactory.literal(op.object.value, op.object.language);
      } else if (op.object.datatype) {
        object = dataFactory.literal(op.object.value, dataFactory.namedNode(op.object.datatype));
      } else {
        object = dataFactory.literal(op.object.value);
      }
    } else if (op.object.termType === 'BlankNode') {
      object = dataFactory.blankNode(op.object.value);
    } else {
      object = dataFactory.namedNode(op.object.value);
    }

    return {
      type: op.type, // 'add' or 'delete'
      subject,
      predicate,
      object,
    };
  });

  // 4. Atomic commit to Universe
  const { receipt } = await store.appendEvent(
    {
      type: 'UPDATE',
      payload: {
        source: delta.source || 'browser',
        operation_count: operations.length,
      },
    },
    deltas
  );

  // 5. Broadcast to other subscribers
  for (const d of deltas) {
    broadcastUpdate({
      type: d.type,
      subject: d.subject.value,
      predicate: d.predicate.value,
      object: d.object.value,
    });
  }

  // 6. Return ACK with updated vector clock
  return {
    status: 'ACK',
    t_ns: receipt.t_ns,
    timestamp_iso: receipt.timestamp_iso,
    event_id: receipt.id,
    event_count: receipt.event_count,
    vector_clock: store.vectorClock.toJSON(),
  };
}

/**
 * Submit a simple update (convenience method)
 *
 * @param {string} subject - Subject IRI
 * @param {string} predicate - Predicate IRI
 * @param {string} newValue - New value
 * @param {Object} [options] - Additional options
 * @returns {Promise<Object>} ACK or REJECT response
 */
export async function submitUpdate(subject, predicate, newValue, options = {}) {
  const store = await getUniverse();
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);

  // Find current value to delete
  const subjNode = dataFactory.namedNode(subject);
  const predNode = dataFactory.namedNode(predicate);
  const current = [...store.match(subjNode, predNode, null, universeGraph)];

  const operations = [];

  // Delete old value if exists
  if (current.length > 0) {
    const oldQuad = current[0];
    operations.push({
      type: 'delete',
      subject: { value: subject, termType: 'NamedNode' },
      predicate: { value: predicate, termType: 'NamedNode' },
      object: {
        value: oldQuad.object.value,
        termType: oldQuad.object.termType,
        datatype: oldQuad.object.datatype?.value,
        language: oldQuad.object.language,
      },
    });
  }

  // Add new value
  operations.push({
    type: 'add',
    subject: { value: subject, termType: 'NamedNode' },
    predicate: { value: predicate, termType: 'NamedNode' },
    object: {
      value: newValue,
      termType: options.termType || 'Literal',
      datatype: options.datatype,
      language: options.language,
    },
  });

  return submitDelta({
    operations,
    source: options.source || 'api',
    client_vector_clock: options.client_vector_clock,
  });
}
