/**
 * IDE Autocomplete Demo
 *
 * This file demonstrates the improved autocomplete and type hints.
 * Open this file in VS Code to see:
 * - Parameter hints when calling functions
 * - Type information on hover
 * - Autocomplete for object properties
 * - Inline documentation
 *
 * Try typing the following to test autocomplete:
 * 1. Type "createWork" and press Ctrl+Space to see createWorkflow suggestion
 * 2. Type "workflowSpec." to see available properties
 * 3. Hover over function names to see documentation
 * 4. Use Ctrl+Space inside function calls to see parameter hints
 */

import { createWorkflow, createCase, enableTask, startTask, completeTask, replayCase } from '@unrdf/yawl';
import { createStore, dataFactory } from '@unrdf/oxigraph';

// ============================================================================
// Example 1: Basic Workflow Creation
// ============================================================================

/**
 *
 */
async function example1_basicWorkflow() {
  const store = createStore();

  // Type "workflowSpec." to see autocomplete for: id, name, tasks, flow, resources, etc.
  const workflowSpec = {
    id: 'purchase-order',
    name: 'Purchase Order Approval',
    tasks: [
      // Type inside the object to see: id, kind, name, split, join, etc.
      { id: 'submit', kind: 'atomic', name: 'Submit Order' },
      { id: 'review', kind: 'atomic', name: 'Review Order', split: 'xor' },
      { id: 'approve', kind: 'atomic', name: 'Approve Order' },
      { id: 'reject', kind: 'atomic', name: 'Reject Order' },
      { id: 'fulfill', kind: 'atomic', name: 'Fulfill Order' }
    ],
    flow: [
      // Type inside to see: from, to, condition, priority, isDefault
      { from: 'submit', to: 'review' },
      { from: 'review', to: 'approve', condition: 'amount < 1000' },
      { from: 'review', to: 'reject', isDefault: true },
      { from: 'approve', to: 'fulfill' }
    ]
  };

  // Hover over createWorkflow to see full documentation
  // Ctrl+Space inside the function call to see parameter hints
  const receipt = await createWorkflow(workflowSpec, { store });

  console.log('Workflow created:', receipt.id);
  return receipt;
}

// ============================================================================
// Example 2: Case Execution
// ============================================================================

/**
 *
 */
async function example2_caseExecution() {
  const store = createStore();

  // First create the workflow
  await example1_basicWorkflow();

  // Type "caseOptions." to see: initialData, metadata
  const caseOptions = {
    initialData: {
      orderId: 'PO-12345',
      amount: 750.00,
      requestedBy: 'user@example.com'
    },
    metadata: {
      department: 'Engineering'
    }
  };

  // Autocomplete shows all 3 parameters when you type inside createCase()
  const caseReceipt = await createCase('purchase-order', store, caseOptions);

  const caseId = caseReceipt.metadata.caseId;
  console.log('Case started:', caseId);

  return { store, caseId };
}

// ============================================================================
// Example 3: Task Lifecycle
// ============================================================================

/**
 *
 */
async function example3_taskLifecycle() {
  const { store, caseId } = await example2_caseExecution();

  // Enable task - autocomplete shows 4 parameters
  const enableReceipt = await enableTask(
    caseId,
    'review',
    store,
    {
      // Type here to see: data, allocatedResources
      data: { reviewedBy: 'manager@example.com' }
    }
  );

  const workItemId = `${caseId}-review-1`;

  // Start task - autocomplete shows 3 parameters
  const startReceipt = await startTask(caseId, workItemId, store);

  // Complete task - autocomplete shows 4 parameters
  // Type inside outputData to see properties from your workflow
  const completeReceipt = await completeTask(
    caseId,
    workItemId,
    store,
    {
      approved: true,
      approvedAt: new Date().toISOString(),
      approvedBy: 'manager@example.com'
    }
  );

  console.log('Task completed:', completeReceipt.id);
  return { store, caseId };
}

// ============================================================================
// Example 4: RDF Store Operations
// ============================================================================

/**
 *
 */
async function example4_rdfStore() {
  const store = createStore();

  // Type "dataFactory." to see: namedNode, blankNode, literal, quad, triple, defaultGraph
  const { namedNode, literal, quad } = dataFactory;

  // Autocomplete shows parameters when creating terms
  const alice = namedNode('http://example.org/alice');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');
  const aliceName = literal('Alice');

  // Type inside quad() to see all 4 parameters with types
  const statement = quad(alice, name, aliceName);

  store.add(statement);

  // Type "store." to see all methods: add, delete, has, match, query, size, etc.
  const results = store.query(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name WHERE {
      ?person foaf:name ?name .
    }
  `);

  // Type "results." to see: bindings, variables
  console.log('Found names:', results.bindings.map(b => b.name.value));

  return store;
}

// ============================================================================
// Example 5: Parallel Split Pattern
// ============================================================================

/**
 *
 */
async function example5_parallelPattern() {
  const store = createStore();

  const workflowSpec = {
    id: 'parallel-demo',
    name: 'Parallel Processing Demo',
    tasks: [
      { id: 'start', kind: 'atomic', name: 'Start', split: 'and' },
      { id: 'task-a', kind: 'atomic', name: 'Process A' },
      { id: 'task-b', kind: 'atomic', name: 'Process B' },
      { id: 'task-c', kind: 'atomic', name: 'Process C' },
      { id: 'join', kind: 'atomic', name: 'Combine Results', join: 'and' }
    ],
    flow: [
      { from: 'start', to: 'task-a' },
      { from: 'start', to: 'task-b' },
      { from: 'start', to: 'task-c' },
      { from: 'task-a', to: 'join' },
      { from: 'task-b', to: 'join' },
      { from: 'task-c', to: 'join' }
    ]
  };

  const receipt = await createWorkflow(workflowSpec, { store });
  console.log('Parallel workflow created:', receipt.id);

  return receipt;
}

// ============================================================================
// Example 6: Time Travel with Replay
// ============================================================================

/**
 *
 */
async function example6_timeTravel() {
  const { store, caseId } = await example3_taskLifecycle();

  // Replay entire case history - autocomplete shows 3 parameters
  const caseState = await replayCase(caseId, store);

  // Type "caseState." to see: caseId, workflowId, status, workItems, data, events
  console.log('Case status:', caseState.status);
  console.log('Work items:', caseState.workItems.length);

  // Type "caseState.workItems[0]." to see WorkItem properties
  caseState.workItems.forEach(wi => {
    // Autocomplete shows: id, caseId, taskId, status, createdAt, startedAt, completedAt, data, allocatedResources
    console.log(`WorkItem ${wi.id}: ${wi.status}`);
  });

  // Replay to specific point in time
  const historicalState = await replayCase(
    caseId,
    store,
    '2025-12-25T12:00:00Z' // Optional timestamp parameter
  );

  console.log('Historical events:', historicalState.events.length);
  return caseState;
}

// ============================================================================
// Example 7: Pattern Matching RDF Quads
// ============================================================================

/**
 *
 */
async function example7_quadMatching() {
  const store = await example4_rdfStore();
  const { namedNode } = dataFactory;

  // Type "store.match(" to see parameter hints: subject?, predicate?, object?, graph?
  // Use null to match any value
  const allQuads = store.match(null, null, null, null);
  console.log('Total quads:', allQuads.length);

  // Find all quads about Alice
  const aliceQuads = store.match(
    namedNode('http://example.org/alice'),
    null,
    null
  );
  console.log('Alice quads:', aliceQuads.length);

  // Find all names (any subject)
  const nameQuads = store.match(
    null,
    namedNode('http://xmlns.com/foaf/0.1/name'),
    null
  );

  // Type "nameQuads[0]." to see: subject, predicate, object, graph
  nameQuads.forEach(q => {
    console.log(`Name: ${q.object.value}`);
  });

  return store;
}

// ============================================================================
// Run Examples
// ============================================================================

async function runAllExamples() {
  console.log('='.repeat(80));
  console.log('IDE Autocomplete Demo - UNRDF');
  console.log('='.repeat(80));

  try {
    console.log('\n1. Basic Workflow Creation');
    await example1_basicWorkflow();

    console.log('\n2. Case Execution');
    await example2_caseExecution();

    console.log('\n3. Task Lifecycle');
    await example3_taskLifecycle();

    console.log('\n4. RDF Store Operations');
    await example4_rdfStore();

    console.log('\n5. Parallel Split Pattern');
    await example5_parallelPattern();

    console.log('\n6. Time Travel with Replay');
    await example6_timeTravel();

    console.log('\n7. Pattern Matching RDF Quads');
    await example7_quadMatching();

    console.log('\n' + '='.repeat(80));
    console.log('All examples completed successfully!');
    console.log('='.repeat(80));
  } catch (error) {
    console.error('Error running examples:', error);
  }
}

// Uncomment to run:
// runAllExamples();

export {
  example1_basicWorkflow,
  example2_caseExecution,
  example3_taskLifecycle,
  example4_rdfStore,
  example5_parallelPattern,
  example6_timeTravel,
  example7_quadMatching
};
