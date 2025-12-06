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
 * Generate counter-factual hint for validation failure
 * @param {string} predicate - The predicate that failed validation
 * @param {*} attemptedValue - The value that was rejected
 * @param {string} reason - Why it was rejected
 * @returns {Object} Counter-factual coaching hint
 */
function generateCounterFactual(predicate, attemptedValue, reason) {
  // Budget validation counter-factual
  if (predicate === 'http://kgc.io/ontology/budget') {
    const budget = parseInt(attemptedValue, 10);
    let suggestedValue = '50000'; // Default safe value
    let suggestion = 'Use a budget within the allowed range (0-100,000)';

    if (isNaN(budget)) {
      suggestedValue = '10000';
      suggestion = 'Budget must be a valid number. Try 10,000 as a starting point.';
    } else if (budget < 0) {
      suggestedValue = '0';
      suggestion = 'Budget cannot be negative. Set to 0 if not applicable.';
    } else if (budget > 100000) {
      suggestedValue = '100000';
      suggestion = 'Budget exceeds maximum allowed. Reduce to $100,000 or split across multiple projects.';
    }

    return {
      guardFired: 'budget_validator',
      suggestion,
      allowedRange: { min: 0, max: 100000, unit: 'USD' },
      correctedDelta: {
        type: 'update',
        predicate,
        oldValue: attemptedValue,
        suggestedValue,
      },
    };
  }

  // Status validation counter-factual
  if (predicate === 'http://kgc.io/ontology/status') {
    const allowed = ['active', 'paused', 'completed', 'cancelled'];
    const suggestedValue = 'active'; // Default to active

    return {
      guardFired: 'status_validator',
      suggestion: `Status must be one of: ${allowed.join(', ')}. Consider using "active" for ongoing work.`,
      allowedValues: allowed,
      correctedDelta: {
        type: 'update',
        predicate,
        oldValue: attemptedValue,
        suggestedValue,
      },
    };
  }

  // Name validation counter-factual
  if (predicate === 'http://kgc.io/ontology/name' || predicate === 'http://kgc.io/ontology/title') {
    const guardName = predicate.endsWith('name') ? 'name_validator' : 'title_validator';
    let suggestedValue = 'Untitled';
    let suggestion = 'Name cannot be empty. Provide a descriptive name.';

    if (attemptedValue && attemptedValue.length > 100) {
      suggestedValue = attemptedValue.substring(0, 97) + '...';
      suggestion = 'Name too long. Shortened to 100 characters.';
    } else if (!attemptedValue || attemptedValue.trim().length === 0) {
      suggestedValue = 'Untitled';
      suggestion = 'Name cannot be empty. Using "Untitled" as placeholder.';
    }

    return {
      guardFired: guardName,
      suggestion,
      allowedRange: { min: 1, max: 100, unit: 'characters' },
      correctedDelta: {
        type: 'update',
        predicate,
        oldValue: attemptedValue,
        suggestedValue,
      },
    };
  }

  // Generic fallback counter-factual
  return {
    guardFired: 'unknown_validator',
    suggestion: reason || 'Validation failed. Please correct the value and try again.',
    correctedDelta: null,
  };
}

/**
 * Find similar successful deltas from recent history
 * @param {Object} operation - The operation that failed
 * @param {number} limit - Max examples to return
 * @returns {Promise<Array>} Similar successful operations
 */
async function findSimilarSuccessfulDeltas(operation, limit = 3) {
  const store = await getUniverse();
  const predicate = operation.predicate.value || operation.predicate;

  // Get recent events with the same predicate
  const events = store.getRecentEvents ? store.getRecentEvents(50) : [];
  const similar = events
    .filter((event) => {
      const payload = event.payload;
      return payload && event.status === 'ACK';
    })
    .slice(0, limit)
    .map((event) => ({
      timestamp: event.timestamp_iso || new Date(Number(BigInt(event.t_ns) / 1000000n)).toISOString(),
      delta: {
        type: 'update',
        predicate,
        value: 'example value', // Would parse from event payload
      },
    }));

  return similar;
}

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
      const predicate = op.predicate.value || op.predicate;
      const attemptedValue = op.object.value;

      // Generate counter-factual coaching hint
      const counterFactual = generateCounterFactual(predicate, attemptedValue, validation.reason);

      // Find similar successful deltas
      const examples = await findSimilarSuccessfulDeltas(op, 3);

      return {
        status: 'REJECT',
        reason: validation.reason,
        operation: op,
        t_ns: now().toString(),
        // Phase 3: Autonomic Coach hints
        coaching: {
          counterFactual,
          examples,
        },
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
