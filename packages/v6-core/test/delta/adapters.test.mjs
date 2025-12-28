/**
 * Comprehensive Delta Adapter Tests - v6-core
 *
 * Integration tests covering:
 * - GraphQL Adapter: mutation to delta conversion
 * - Resource Adapter: resource allocation and management
 * - Workflow Adapter: workflow state transitions
 *
 * Each adapter tested for:
 * - Constructor initialization
 * - All public methods
 * - Error handling
 * - Edge cases
 * - Delta generation and validation
 *
 * @module @unrdf/v6-core/test/delta/adapters
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';
import crypto from 'node:crypto';
import {
  GraphQLAdapter,
  createGraphQLAdapter,
} from '../../src/delta/adapters/graphql-adapter.mjs';
import {
  ResourceAdapter,
  createResourceAdapter,
} from '../../src/delta/adapters/resource-adapter.mjs';
import {
  WorkflowAdapter,
  createWorkflowAdapter,
} from '../../src/delta/adapters/workflow-adapter.mjs';

// ============================================================================
// Test Suite 1: GraphQL Adapter (12 tests)
// ============================================================================

test('GraphQLAdapter - constructor with default options', () => {
  const adapter = new GraphQLAdapter();

  assert.strictEqual(adapter.namespace, 'http://unrdf.io/entity/');
  assert.strictEqual(adapter.graphUri, 'http://unrdf.io/graph/graphql');
  assert.deepStrictEqual(adapter.typeMapping, {});
});

test('GraphQLAdapter - constructor with custom options', () => {
  const adapter = new GraphQLAdapter({
    namespace: 'http://example.org/entities/',
    graphUri: 'http://example.org/graph',
    typeMapping: { userName: 'http://schema.org/name' },
  });

  assert.strictEqual(adapter.namespace, 'http://example.org/entities/');
  assert.strictEqual(adapter.graphUri, 'http://example.org/graph');
  assert.strictEqual(adapter.typeMapping.userName, 'http://schema.org/name');
});

test('GraphQLAdapter - createEntity generates valid delta', () => {
  const adapter = new GraphQLAdapter();
  const uuid = crypto.randomUUID();
  const entityUuid = crypto.randomUUID();
  const timestamp = new Date().toISOString();
  const t_ns = BigInt(Date.now()) * 1_000_000n;

  const delta = adapter.createEntity(
    'User',
    { id: 'user-1', name: 'Alice', email: 'alice@example.com' },
    { uuid, entityUuid, timestamp_iso: timestamp, t_ns }
  );

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.timestamp_iso, timestamp);
  assert.strictEqual(delta.t_ns, t_ns);
  assert.ok(Array.isArray(delta.operations));
  assert.strictEqual(delta.operations.length, 3); // type + name + email
  assert.strictEqual(delta.source.package, '@unrdf/graphql');
  assert.strictEqual(delta.source.actor, 'graphql-api');

  // Check type triple
  const typeOp = delta.operations[0];
  assert.strictEqual(typeOp.op, 'add');
  assert.strictEqual(typeOp.subject, 'http://unrdf.io/entity/User/user-1');
  assert.strictEqual(typeOp.predicate, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  assert.strictEqual(typeOp.object, 'http://unrdf.io/entity/User');
});

test('GraphQLAdapter - updateEntity generates valid delta', () => {
  const adapter = new GraphQLAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.updateEntity(
    'User',
    { id: 'user-1', name: 'Alice Smith', email: 'alice.smith@example.com' },
    { uuid }
  );

  assert.strictEqual(delta.id, uuid);
  assert.ok(Array.isArray(delta.operations));
  assert.strictEqual(delta.operations.length, 2); // name + email (id excluded)
  assert.strictEqual(delta.source.context.mutation, 'update');

  // Check operations are all adds (update semantics handled by reconciliation)
  delta.operations.forEach(op => {
    assert.strictEqual(op.op, 'add');
    assert.ok(op.subject.includes('user-1'));
  });
});

test('GraphQLAdapter - updateEntity throws without id', () => {
  const adapter = new GraphQLAdapter();

  assert.throws(
    () => adapter.updateEntity('User', { name: 'Alice' }),
    { message: 'Update mutation must include entity id' }
  );
});

test('GraphQLAdapter - deleteEntity generates valid delta', () => {
  const adapter = new GraphQLAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.deleteEntity('User', 'user-1', { uuid });

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.operations.length, 1);
  assert.strictEqual(delta.operations[0].op, 'delete');
  assert.strictEqual(delta.operations[0].subject, 'http://unrdf.io/entity/User/user-1');
  assert.strictEqual(delta.operations[0].predicate, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  assert.strictEqual(delta.source.context.mutation, 'delete');
});

test('GraphQLAdapter - mutationToDelta handles createUser mutation', () => {
  const adapter = new GraphQLAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.mutationToDelta(
    'createUser',
    { id: 'user-1', name: 'Bob' },
    { uuid }
  );

  assert.strictEqual(delta.id, uuid);
  assert.ok(delta.operations.some(op => op.object === 'http://unrdf.io/entity/User'));
  assert.strictEqual(delta.source.context.entityType, 'User');
  assert.strictEqual(delta.source.context.mutation, 'create');
});

test('GraphQLAdapter - mutationToDelta handles updateTask mutation', () => {
  const adapter = new GraphQLAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.mutationToDelta(
    'updateTask',
    { id: 'task-1', status: 'completed' },
    { uuid }
  );

  assert.strictEqual(delta.source.context.entityType, 'Task');
  assert.strictEqual(delta.source.context.mutation, 'update');
});

test('GraphQLAdapter - mutationToDelta handles deleteProject mutation', () => {
  const adapter = new GraphQLAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.mutationToDelta(
    'deleteProject',
    { id: 'project-1' },
    { uuid }
  );

  assert.strictEqual(delta.source.context.entityType, 'Project');
  assert.strictEqual(delta.source.context.mutation, 'delete');
  assert.strictEqual(delta.operations[0].op, 'delete');
});

test('GraphQLAdapter - mutationToDelta throws on unknown mutation type', () => {
  const adapter = new GraphQLAdapter();

  assert.throws(
    () => adapter.mutationToDelta('invalidMutation', {}),
    { message: /Invalid mutation name format/ }
  );
});

test('GraphQLAdapter - _parseMutationName parses correctly', () => {
  const adapter = new GraphQLAdapter();

  const result1 = adapter._parseMutationName('createUser');
  assert.strictEqual(result1.type, 'create');
  assert.strictEqual(result1.entityType, 'User');

  const result2 = adapter._parseMutationName('updateTask');
  assert.strictEqual(result2.type, 'update');
  assert.strictEqual(result2.entityType, 'Task');

  const result3 = adapter._parseMutationName('deleteProject');
  assert.strictEqual(result3.type, 'delete');
  assert.strictEqual(result3.entityType, 'Project');
});

test('GraphQLAdapter - createGraphQLAdapter factory function', () => {
  const adapter = createGraphQLAdapter({
    namespace: 'http://test.org/',
  });

  assert.ok(adapter instanceof GraphQLAdapter);
  assert.strictEqual(adapter.namespace, 'http://test.org/');
});

// ============================================================================
// Test Suite 2: Resource Adapter (11 tests)
// ============================================================================

test('ResourceAdapter - constructor with default options', () => {
  const adapter = new ResourceAdapter();

  assert.strictEqual(adapter.namespace, 'http://unrdf.io/resource/');
  assert.strictEqual(adapter.graphUri, 'http://unrdf.io/graph/resource');
});

test('ResourceAdapter - constructor with custom options', () => {
  const adapter = new ResourceAdapter({
    namespace: 'http://example.org/res/',
    graphUri: 'http://example.org/graph/res',
  });

  assert.strictEqual(adapter.namespace, 'http://example.org/res/');
  assert.strictEqual(adapter.graphUri, 'http://example.org/graph/res');
});

test('ResourceAdapter - allocate generates valid delta', () => {
  const adapter = new ResourceAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.allocate('agent-42', 'task-1', { uuid, priority: 'high' });

  assert.strictEqual(delta.id, uuid);
  assert.ok(Array.isArray(delta.operations));
  assert.strictEqual(delta.operations.length, 3); // status update + allocatedTo + allocatedAt
  assert.strictEqual(delta.source.package, '@unrdf/resource');
  assert.strictEqual(delta.source.actor, 'resource-allocator');
  assert.strictEqual(delta.source.context.priority, 'high');

  // Check status update operation
  const statusOp = delta.operations.find(op => op.predicate.includes('status'));
  assert.strictEqual(statusOp.op, 'update');
  assert.strictEqual(statusOp.oldObject, 'available');
  assert.strictEqual(statusOp.newObject, 'allocated');
});

test('ResourceAdapter - deallocate generates valid delta', () => {
  const adapter = new ResourceAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.deallocate('agent-42', 'task-1', { uuid });

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.operations.length, 3); // status update + delete allocatedTo + add deallocatedAt

  // Check status update operation
  const statusOp = delta.operations.find(op => op.predicate.includes('status'));
  assert.strictEqual(statusOp.op, 'update');
  assert.strictEqual(statusOp.oldObject, 'allocated');
  assert.strictEqual(statusOp.newObject, 'available');

  // Check allocatedTo deletion
  const deleteOp = delta.operations.find(op => op.predicate.includes('allocatedTo'));
  assert.strictEqual(deleteOp.op, 'delete');
});

test('ResourceAdapter - registerCapability with basic metadata', () => {
  const adapter = new ResourceAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.registerCapability(
    'agent-42',
    'code-generation',
    { context: { uuid } }
  );

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.operations.length, 2); // capability + registeredAt
  assert.strictEqual(delta.source.actor, 'capability-registry');

  // Check capability link
  const capOp = delta.operations.find(op => op.predicate.includes('capability'));
  assert.strictEqual(capOp.op, 'add');
  assert.ok(capOp.object.includes('code-generation'));
});

test('ResourceAdapter - registerCapability with extended metadata', () => {
  const adapter = new ResourceAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.registerCapability(
    'agent-42',
    'code-generation',
    {
      context: { uuid },
      level: 'expert',
      languages: 'javascript,python',
    }
  );

  // Should have capability + registeredAt + level + languages
  assert.ok(delta.operations.length >= 4);

  // Check metadata properties are added
  const levelOp = delta.operations.find(op => op.predicate.includes('level'));
  assert.ok(levelOp);
  assert.strictEqual(levelOp.object, 'expert');
});

test('ResourceAdapter - updateAvailability to available', () => {
  const adapter = new ResourceAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.updateAvailability('agent-42', true, { uuid });

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.operations.length, 2); // status + statusUpdatedAt

  const statusOp = delta.operations.find(op => op.predicate.includes('status'));
  assert.strictEqual(statusOp.op, 'add');
  assert.strictEqual(statusOp.object, 'available');
});

test('ResourceAdapter - updateAvailability to unavailable with reason', () => {
  const adapter = new ResourceAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.updateAvailability(
    'agent-42',
    false,
    { uuid, reason: 'maintenance' }
  );

  const statusOp = delta.operations.find(op => op.predicate.includes('status'));
  assert.strictEqual(statusOp.object, 'unavailable');
  assert.strictEqual(delta.source.context.reason, 'maintenance');
});

test('ResourceAdapter - UUID generation is deterministic with context', () => {
  const adapter = new ResourceAdapter();
  const fixedUuid = crypto.randomUUID();

  const delta1 = adapter.allocate('agent-1', 'task-1', { uuid: fixedUuid });
  const delta2 = adapter.deallocate('agent-2', 'task-2', { uuid: fixedUuid });

  // Both should use the same UUID when provided in context
  assert.strictEqual(delta1.id, fixedUuid);
  assert.strictEqual(delta2.id, fixedUuid);
});

test('ResourceAdapter - operations include proper graph URIs', () => {
  const adapter = new ResourceAdapter({ graphUri: 'http://test.org/graph' });
  const delta = adapter.allocate('agent-1', 'task-1');

  delta.operations.forEach(op => {
    assert.strictEqual(op.graph, 'http://test.org/graph');
  });
});

test('ResourceAdapter - createResourceAdapter factory function', () => {
  const adapter = createResourceAdapter({
    namespace: 'http://test.org/res/',
  });

  assert.ok(adapter instanceof ResourceAdapter);
  assert.strictEqual(adapter.namespace, 'http://test.org/res/');
});

// ============================================================================
// Test Suite 3: Workflow Adapter (12 tests)
// ============================================================================

test('WorkflowAdapter - constructor with default options', () => {
  const adapter = new WorkflowAdapter();

  assert.strictEqual(adapter.namespace, 'http://unrdf.io/workflow/');
  assert.strictEqual(adapter.graphUri, 'http://unrdf.io/graph/workflow');
});

test('WorkflowAdapter - constructor with custom options', () => {
  const adapter = new WorkflowAdapter({
    namespace: 'http://example.org/wf/',
    graphUri: 'http://example.org/graph/wf',
  });

  assert.strictEqual(adapter.namespace, 'http://example.org/wf/');
  assert.strictEqual(adapter.graphUri, 'http://example.org/graph/wf');
});

test('WorkflowAdapter - taskTransition from enabled to executing', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.taskTransition(
    'task-1',
    'enabled',
    'executing',
    { uuid, workflowId: 'wf-123' }
  );

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.operations.length, 2); // state update + timestamp
  assert.strictEqual(delta.source.package, '@unrdf/yawl');
  assert.strictEqual(delta.source.context.transition, 'enabled → executing');

  // Check state update operation
  const stateOp = delta.operations.find(op => op.predicate.includes('state'));
  assert.strictEqual(stateOp.op, 'update');
  assert.strictEqual(stateOp.oldObject, 'enabled');
  assert.strictEqual(stateOp.newObject, 'executing');
});

test('WorkflowAdapter - taskTransition from executing to completed', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.taskTransition(
    'task-1',
    'executing',
    'completed',
    { uuid }
  );

  const stateOp = delta.operations.find(op => op.predicate.includes('state'));
  assert.strictEqual(stateOp.oldObject, 'executing');
  assert.strictEqual(stateOp.newObject, 'completed');
});

test('WorkflowAdapter - taskTransition from enabled to cancelled', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.taskTransition(
    'task-1',
    'enabled',
    'cancelled',
    { uuid }
  );

  const stateOp = delta.operations.find(op => op.predicate.includes('state'));
  assert.strictEqual(stateOp.newObject, 'cancelled');
  assert.strictEqual(delta.source.context.transition, 'enabled → cancelled');
});

test('WorkflowAdapter - workflowCreation generates valid delta', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.workflowCreation(
    'wf-123',
    'order-processing-v1',
    { uuid }
  );

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.operations.length, 4); // type + spec + state + createdAt
  assert.strictEqual(delta.source.context.workflowId, 'wf-123');
  assert.strictEqual(delta.source.context.specId, 'order-processing-v1');

  // Check type operation
  const typeOp = delta.operations.find(op =>
    op.predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
  );
  assert.strictEqual(typeOp.op, 'add');
  assert.ok(typeOp.object.includes('WorkflowInstance'));

  // Check initial state
  const stateOp = delta.operations.find(op => op.predicate.includes('state'));
  assert.strictEqual(stateOp.object, 'running');
});

test('WorkflowAdapter - resourceAssignment generates valid delta', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.resourceAssignment('task-1', 'agent-42', { uuid });

  assert.strictEqual(delta.id, uuid);
  assert.strictEqual(delta.operations.length, 2); // assignedTo + assignedAt
  assert.strictEqual(delta.source.package, '@unrdf/yawl');
  assert.strictEqual(delta.source.actor, 'resource-allocator');

  // Check assignment operation
  const assignOp = delta.operations.find(op => op.predicate.includes('assignedTo'));
  assert.strictEqual(assignOp.op, 'add');
  assert.strictEqual(assignOp.object, 'agent-42');
});

test('WorkflowAdapter - cancellationRegion with multiple tasks', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();
  const taskIds = ['task-2', 'task-3', 'task-4'];

  const delta = adapter.cancellationRegion('region-1', taskIds, { uuid });

  assert.strictEqual(delta.id, uuid);
  // Each task gets 3 operations: state + cancelledAt + cancelledBy
  assert.strictEqual(delta.operations.length, taskIds.length * 3);
  assert.strictEqual(delta.source.context.regionId, 'region-1');
  assert.deepStrictEqual(delta.source.context.taskIds, taskIds);

  // Check that all tasks are cancelled
  const stateOps = delta.operations.filter(op => op.predicate.includes('state'));
  assert.strictEqual(stateOps.length, taskIds.length);
  stateOps.forEach(op => {
    assert.strictEqual(op.object, 'cancelled');
  });
});

test('WorkflowAdapter - cancellationRegion with single task', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();

  const delta = adapter.cancellationRegion('region-1', ['task-1'], { uuid });

  assert.strictEqual(delta.operations.length, 3);
});

test('WorkflowAdapter - cancellationRegion with empty task list throws validation error', () => {
  const adapter = new WorkflowAdapter();
  const uuid = crypto.randomUUID();

  // Empty task list produces 0 operations, which violates delta schema (min 1 operation)
  assert.throws(
    () => adapter.cancellationRegion('region-1', [], { uuid }),
    { message: /Too small/ }
  );
});

test('WorkflowAdapter - delta structure validation', () => {
  const adapter = new WorkflowAdapter();
  const delta = adapter.taskTransition('task-1', 'enabled', 'executing');

  // Verify required delta fields
  assert.ok(delta.id);
  assert.ok(delta.timestamp_iso);
  assert.ok(delta.t_ns);
  assert.ok(Array.isArray(delta.operations));
  assert.ok(delta.source);
  assert.ok(delta.source.package);
  assert.ok(delta.source.actor);

  // Verify timestamp format
  assert.ok(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/.test(delta.timestamp_iso));

  // Verify t_ns is BigInt
  assert.strictEqual(typeof delta.t_ns, 'bigint');
});

test('WorkflowAdapter - createWorkflowAdapter factory function', () => {
  const adapter = createWorkflowAdapter({
    namespace: 'http://test.org/wf/',
  });

  assert.ok(adapter instanceof WorkflowAdapter);
  assert.strictEqual(adapter.namespace, 'http://test.org/wf/');
});

// ============================================================================
// Test Suite 4: Cross-Adapter Integration (5 tests)
// ============================================================================

test('All adapters - consistent delta structure', () => {
  const graphqlAdapter = new GraphQLAdapter();
  const resourceAdapter = new ResourceAdapter();
  const workflowAdapter = new WorkflowAdapter();

  const uuid = crypto.randomUUID();
  const context = { uuid };

  const graphqlDelta = graphqlAdapter.createEntity('User', { id: '1', name: 'Alice' }, context);
  const resourceDelta = resourceAdapter.allocate('agent-1', 'task-1', context);
  const workflowDelta = workflowAdapter.taskTransition('task-1', 'enabled', 'executing', context);

  // All should have the same UUID when provided
  assert.strictEqual(graphqlDelta.id, uuid);
  assert.strictEqual(resourceDelta.id, uuid);
  assert.strictEqual(workflowDelta.id, uuid);

  // All should have required fields
  [graphqlDelta, resourceDelta, workflowDelta].forEach(delta => {
    assert.ok(delta.id);
    assert.ok(delta.timestamp_iso);
    assert.ok(delta.t_ns);
    assert.ok(Array.isArray(delta.operations));
    assert.ok(delta.source);
  });
});

test('All adapters - timestamp consistency', () => {
  const graphqlAdapter = new GraphQLAdapter();
  const resourceAdapter = new ResourceAdapter();
  const workflowAdapter = new WorkflowAdapter();

  const timestamp = new Date().toISOString();
  const t_ns = BigInt(Date.now()) * 1_000_000n;
  const context = { timestamp_iso: timestamp, t_ns };

  const graphqlDelta = graphqlAdapter.createEntity('User', { id: '1' }, context);
  const resourceDelta = resourceAdapter.allocate('agent-1', 'task-1', context);
  const workflowDelta = workflowAdapter.taskTransition('task-1', 'enabled', 'executing', context);

  // All should use the provided timestamp
  assert.strictEqual(graphqlDelta.timestamp_iso, timestamp);
  assert.strictEqual(resourceDelta.timestamp_iso, timestamp);
  assert.strictEqual(workflowDelta.timestamp_iso, timestamp);

  // All should use the provided t_ns
  assert.strictEqual(graphqlDelta.t_ns, t_ns);
  assert.strictEqual(resourceDelta.t_ns, t_ns);
  assert.strictEqual(workflowDelta.t_ns, t_ns);
});

test('All adapters - custom actor in context', () => {
  const graphqlAdapter = new GraphQLAdapter();
  const resourceAdapter = new ResourceAdapter();
  const workflowAdapter = new WorkflowAdapter();

  const context = { actor: 'custom-actor' };

  const graphqlDelta = graphqlAdapter.createEntity('User', { id: '1' }, context);
  const resourceDelta = resourceAdapter.allocate('agent-1', 'task-1', context);
  const workflowDelta = workflowAdapter.taskTransition('task-1', 'enabled', 'executing', context);

  assert.strictEqual(graphqlDelta.source.actor, 'custom-actor');
  assert.strictEqual(resourceDelta.source.actor, 'custom-actor');
  assert.strictEqual(workflowDelta.source.actor, 'custom-actor');
});

test('All adapters - operations have required fields', () => {
  const graphqlAdapter = new GraphQLAdapter();
  const resourceAdapter = new ResourceAdapter();
  const workflowAdapter = new WorkflowAdapter();

  const graphqlDelta = graphqlAdapter.createEntity('User', { id: '1', name: 'Alice' });
  const resourceDelta = resourceAdapter.allocate('agent-1', 'task-1');
  const workflowDelta = workflowAdapter.taskTransition('task-1', 'enabled', 'executing');

  const allOperations = [
    ...graphqlDelta.operations,
    ...resourceDelta.operations,
    ...workflowDelta.operations,
  ];

  allOperations.forEach(op => {
    assert.ok(op.op); // operation type
    assert.ok(op.subject); // subject URI
    assert.ok(op.predicate); // predicate URI
    assert.ok(op.graph); // graph URI

    // Depending on operation type, should have object or oldObject/newObject
    if (op.op === 'update') {
      assert.ok(op.oldObject !== undefined);
      assert.ok(op.newObject !== undefined);
    } else {
      assert.ok(op.object !== undefined);
    }
  });
});

test('All adapters - validateDelta is called internally', () => {
  const graphqlAdapter = new GraphQLAdapter();
  const resourceAdapter = new ResourceAdapter();
  const workflowAdapter = new WorkflowAdapter();

  // These should not throw because validateDelta is called internally
  assert.doesNotThrow(() => {
    graphqlAdapter.createEntity('User', { id: '1', name: 'Alice' });
  });

  assert.doesNotThrow(() => {
    resourceAdapter.allocate('agent-1', 'task-1');
  });

  assert.doesNotThrow(() => {
    workflowAdapter.taskTransition('task-1', 'enabled', 'executing');
  });
});
