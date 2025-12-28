/**
 * @file YAWL Workflow API - Core Workflow and Case Management
 * @module @unrdf/yawl/api/workflow-api-core
 *
 * @description
 * Implements core workflow creation, case instantiation, and control flow graph management.
 * All functions are pure with explicit RDF deltas and event logging.
 */

import {
  YAWL_NS,
  YAWL_EVENT_TYPES,
  WORK_ITEM_STATUS,
  WorkflowSpecSchema,
  WorkflowOptionsSchema,
  CaseOptionsSchema,
  WorkItemSchema,
  generateId,
  now,
  toISO,
  createReceipt,
} from './workflow-api-validation.mjs';
import {
  tracer,
  workflowsCreatedCounter,
  casesCreatedCounter,
  workflowCreationHistogram,
  caseCreationHistogram,
  activeCasesGauge,
} from '../otel.mjs';

// ============================================================================
// Core API Functions
// ============================================================================

/**
 * Create a new YAWL workflow from specification.
 *
 * Creates a workflow object with methods for case management, task enablement,
 * and control flow evaluation. Optionally creates RDF representation in store.
 *
 * @param {Object} spec - Workflow specification
 * @param {string} spec.id - Unique workflow identifier
 * @param {Array<Object>} spec.tasks - Task definitions
 * @param {Array<Object>} [spec.controlFlow] - Control flow definitions
 * @param {Array<Object>} [spec.resources] - Resource definitions
 * @param {Object} [options] - Creation options
 * @param {Object} [options.store] - KGC-4D store for RDF representation
 * @param {Object} [options.gitBackbone] - Git backbone for snapshots
 * @param {Object} [options.hookRegistry] - Hook registry for policy execution
 * @param {boolean} [options.validateSpec=true] - Validate spec structure
 * @param {boolean} [options.createRDF=true] - Create RDF representation
 * @returns {Promise<Object>} Workflow object with methods
 *
 * @example
 * const workflow = await createWorkflow({
 *   id: 'order-processing',
 *   tasks: [
 *     { id: 'receive-order', name: 'Receive Order' },
 *     { id: 'process-payment', name: 'Process Payment' },
 *     { id: 'ship-order', name: 'Ship Order' },
 *   ],
 *   controlFlow: [
 *     { id: 'cf1', type: 'sequence', from: 'receive-order', to: 'process-payment' },
 *     { id: 'cf2', type: 'sequence', from: 'process-payment', to: 'ship-order' },
 *   ],
 * });
 */
export async function createWorkflow(spec, options = {}) {
  const span = tracer.startSpan('workflow.create');
  const startTime = Date.now();

  try {
    // Validate options
    const validOptions = WorkflowOptionsSchema.parse(options);

    // Validate spec structure if requested
    let validSpec;
    if (validOptions?.validateSpec !== false) {
      validSpec = WorkflowSpecSchema.parse(spec);
    } else {
      validSpec = spec;
    }

    const t_ns = now();
    const workflowId = validSpec.id;

    span.setAttributes({
      'workflow.id': workflowId,
      'workflow.name': validSpec.name || workflowId,
      'workflow.taskCount': validSpec.tasks.length,
      'workflow.controlFlowCount': validSpec.controlFlow?.length || 0,
      'workflow.resourceCount': validSpec.resources?.length || 0,
    });

  // Build task index for O(1) lookup
  const taskIndex = new Map();
  for (const task of validSpec.tasks) {
    taskIndex.set(task.id, { ...task, _validated: true });
  }

  // Build control flow graph
  const controlFlowGraph = buildControlFlowGraph(validSpec.controlFlow || []);

  // Find initial tasks (tasks with no incoming edges)
  const initialTasks = findInitialTasks(validSpec.tasks, controlFlowGraph);

  // Build cancellation regions index
  const cancellationRegions = validSpec.cancellationRegions || {};

  // Create RDF representation if store provided and createRDF is true
  let rdfDeltas = [];
  if (validOptions?.store && validOptions?.createRDF !== false) {
    rdfDeltas = createWorkflowRDF(validSpec, validOptions.store);
  }

  // Append creation event to KGC-4D store if available
  let receipt = null;
  if (validOptions?.store?.appendEvent) {
    const { receipt: eventReceipt } = await validOptions.store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.WORKFLOW_CREATED,
        payload: {
          workflowId,
          taskCount: validSpec.tasks.length,
          controlFlowCount: validSpec.controlFlow?.length || 0,
          resourceCount: validSpec.resources?.length || 0,
        },
      },
      rdfDeltas
    );
    receipt = await createReceipt(YAWL_EVENT_TYPES.WORKFLOW_CREATED, {
      workflowId,
      eventId: eventReceipt.id,
    });
  } else {
    receipt = await createReceipt(YAWL_EVENT_TYPES.WORKFLOW_CREATED, {
      workflowId,
    });
  }

    // Return workflow object with methods
    const workflow = {
      id: workflowId,
      name: validSpec.name || workflowId,
      version: validSpec.version || '1.0.0',
      spec: validSpec,
      taskIndex,
      controlFlowGraph,
      initialTasks,
      cancellationRegions,
      createdAt: toISO(t_ns),
      t_ns: t_ns.toString(),
      receipt,
      _store: validOptions?.store,
      _gitBackbone: validOptions?.gitBackbone,
      _hookRegistry: validOptions?.hookRegistry,
      _policyPacks: validOptions?.policyPacks || [],

      getTask(taskId) {
        return taskIndex.get(taskId) || null;
      },
      getTasks() {
        return Array.from(taskIndex.values());
      },
      getDownstreamTasks(taskId) {
        const edges = controlFlowGraph.outgoing.get(taskId) || [];
        return edges.map((edge) => taskIndex.get(edge.to)).filter((task) => task !== undefined);
      },
      getUpstreamTasks(taskId) {
        const edges = controlFlowGraph.incoming.get(taskId) || [];
        return edges.map((edge) => taskIndex.get(edge.from)).filter((task) => task !== undefined);
      },
      isInitialTask(taskId) {
        return initialTasks.includes(taskId);
      },
      getCancellationRegion(regionId) {
        return cancellationRegions[regionId] || [];
      },
    };

    // Record metrics
    const duration = Date.now() - startTime;
    workflowsCreatedCounter.add(1, { 'workflow.id': workflowId });
    workflowCreationHistogram.record(duration, { 'workflow.id': workflowId });

    span.setStatus({ code: 1 }); // OK
    return workflow;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message }); // ERROR
    throw error;
  } finally {
    span.end();
  }
}

/**
 * Create a new case (workflow instance) from a workflow.
 *
 * Instantiates workflow case with initial work items for enabled tasks.
 * Creates immutable event in KGC-4D and returns case object with status.
 *
 * @param {Object} workflow - Workflow object from createWorkflow
 * @param {Object} [options] - Case creation options
 * @param {string} [options.caseId] - Custom case ID (auto-generated if not provided)
 * @param {Object} [options.initialVariables] - Initial case variables
 * @param {number} [options.priority] - Case priority (0-100)
 * @param {string} [options.deadline] - ISO 8601 deadline
 * @param {string} [options.parent] - Parent case ID for sub-workflows
 * @returns {Promise<Object>} Case object with caseId, status, workItems
 *
 * @example
 * const caseObj = await createCase(workflow, {
 *   caseId: 'order-12345',
 *   initialVariables: { customerId: 'C001', amount: 150.00 },
 *   priority: 75,
 * });
 */
export async function createCase(workflow, options = {}) {
  const span = tracer.startSpan('case.create');
  const startTime = Date.now();

  try {
    // Validate options
    const validOptions = CaseOptionsSchema.parse(options);

    const t_ns = now();
    const caseId = validOptions?.caseId || generateId();

    span.setAttributes({
      'case.id': caseId,
      'workflow.id': workflow.id,
      'case.priority': validOptions?.priority || 50,
    });

  // Initialize work items for all tasks
  const workItems = new Map();
  for (const task of workflow.getTasks()) {
    const workItemId = `${caseId}-${task.id}`;
    const isInitial = workflow.isInitialTask(task.id);

    const workItem = WorkItemSchema.parse({
      id: workItemId,
      caseId,
      taskId: task.id,
      status: isInitial ? WORK_ITEM_STATUS.ENABLED : WORK_ITEM_STATUS.PENDING,
      variables: validOptions?.initialVariables || {},
    });

    workItems.set(task.id, workItem);
  }

  // Get initially enabled work items
  const enabledWorkItems = Array.from(workItems.values()).filter(
    (wi) => wi.status === WORK_ITEM_STATUS.ENABLED
  );

  // Create RDF representation if store available
  let rdfDeltas = [];
  if (workflow._store) {
    rdfDeltas = createCaseRDF(caseId, workflow.id, workItems, workflow._store);
  }

  // Append creation event to KGC-4D store
  let receipt = null;
  if (workflow._store?.appendEvent) {
    const { receipt: eventReceipt } = await workflow._store.appendEvent(
      {
        type: YAWL_EVENT_TYPES.CASE_CREATED,
        payload: {
          caseId,
          workflowId: workflow.id,
          workItemCount: workItems.size,
          enabledTaskIds: enabledWorkItems.map((wi) => wi.taskId),
          initialVariables: validOptions?.initialVariables || {},
        },
      },
      rdfDeltas
    );
    receipt = await createReceipt(YAWL_EVENT_TYPES.CASE_CREATED, {
      caseId,
      workflowId: workflow.id,
      eventId: eventReceipt.id,
    });
  } else {
    receipt = await createReceipt(YAWL_EVENT_TYPES.CASE_CREATED, {
      caseId,
      workflowId: workflow.id,
    });
  }

    // Return case object
    const caseObj = {
      caseId,
      workflowId: workflow.id,
      status: 'active',
      workItems,
      variables: validOptions?.initialVariables || {},
      priority: validOptions?.priority || 50,
      deadline: validOptions?.deadline,
      parent: validOptions?.parent,
      createdAt: toISO(t_ns),
      t_ns: t_ns.toString(),
      receipt,
      _workflow: workflow,

      getWorkItem(taskId) {
        return workItems.get(taskId) || null;
      },
      getWorkItems() {
        return Array.from(workItems.values());
      },
      getWorkItemsByStatus(status) {
        return Array.from(workItems.values()).filter((wi) => wi.status === status);
      },
      isComplete() {
        return Array.from(workItems.values()).every(
          (wi) => wi.status === WORK_ITEM_STATUS.COMPLETED || wi.status === WORK_ITEM_STATUS.CANCELLED
        );
      },
      getEnabledWorkItems() {
        return Array.from(workItems.values()).filter((wi) => wi.status === WORK_ITEM_STATUS.ENABLED);
      },
      getActiveWorkItems() {
        return Array.from(workItems.values()).filter((wi) => wi.status === WORK_ITEM_STATUS.ACTIVE);
      },
    };

    // Record metrics
    const duration = Date.now() - startTime;
    casesCreatedCounter.add(1, { 'case.id': caseId, 'workflow.id': workflow.id });
    caseCreationHistogram.record(duration, { 'case.id': caseId });
    activeCasesGauge.add(1, { 'workflow.id': workflow.id });

    span.setAttributes({
      'case.workItemCount': workItems.size,
      'case.enabledTaskCount': enabledWorkItems.length,
    });
    span.setStatus({ code: 1 }); // OK
    return caseObj;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: 2, message: error.message }); // ERROR
    throw error;
  } finally {
    span.end();
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Build control flow graph from control flow definitions
 * @param {Array<Object>} controlFlow - Control flow definitions
 * @returns {Object} Graph with incoming and outgoing edge maps
 */
export function buildControlFlowGraph(controlFlow) {
  const incoming = new Map();
  const outgoing = new Map();

  for (const cf of controlFlow) {
    // Handle 'to' as string or array
    const targets = Array.isArray(cf.to) ? cf.to : [cf.to];

    // Add outgoing edges from source
    if (!outgoing.has(cf.from)) {
      outgoing.set(cf.from, []);
    }
    for (const target of targets) {
      outgoing.get(cf.from).push({
        id: cf.id,
        type: cf.type,
        from: cf.from,
        to: target,
        condition: cf.condition,
        weight: cf.weight,
      });

      // Add incoming edges to target
      if (!incoming.has(target)) {
        incoming.set(target, []);
      }
      incoming.get(target).push({
        id: cf.id,
        type: cf.type,
        from: cf.from,
        to: target,
        condition: cf.condition,
        weight: cf.weight,
      });
    }
  }

  return { incoming, outgoing };
}

/**
 * Find initial tasks (tasks with no incoming edges)
 * @param {Array<Object>} tasks - Task definitions
 * @param {Object} controlFlowGraph - Control flow graph
 * @returns {Array<string>} Array of initial task IDs
 */
export function findInitialTasks(tasks, controlFlowGraph) {
  const initialTasks = [];
  for (const task of tasks) {
    const incoming = controlFlowGraph.incoming.get(task.id) || [];
    if (incoming.length === 0) {
      initialTasks.push(task.id);
    }
  }

  // If no initial tasks found (circular workflow), use first task
  if (initialTasks.length === 0 && tasks.length > 0) {
    initialTasks.push(tasks[0].id);
  }

  return initialTasks;
}

/**
 * Create RDF representation of workflow
 * @param {Object} spec - Workflow specification
 * @param {Object} store - RDF store
 * @returns {Array<Object>} RDF deltas
 */
export function createWorkflowRDF(spec, store) {
  const deltas = [];
  const df = store._store?.dataFactory || {
    namedNode: (v) => ({ termType: 'NamedNode', value: v }),
    literal: (v) => ({ termType: 'Literal', value: v }),
  };

  const workflowUri = `${YAWL_NS.WORKFLOW}${spec.id}`;
  const workflowNode = df.namedNode(workflowUri);

  // Add workflow type
  deltas.push({
    type: 'add',
    subject: workflowNode,
    predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: df.namedNode(`${YAWL_NS.BASE}Workflow`),
  });

  // Add workflow name
  if (spec.name) {
    deltas.push({
      type: 'add',
      subject: workflowNode,
      predicate: df.namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      object: df.literal(spec.name),
    });
  }

  // Add tasks
  for (const task of spec.tasks) {
    const taskUri = `${workflowUri}/task/${task.id}`;
    const taskNode = df.namedNode(taskUri);

    deltas.push({
      type: 'add',
      subject: workflowNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}hasTask`),
      object: taskNode,
    });

    deltas.push({
      type: 'add',
      subject: taskNode,
      predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: df.namedNode(`${YAWL_NS.BASE}Task`),
    });

    deltas.push({
      type: 'add',
      subject: taskNode,
      predicate: df.namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      object: df.literal(task.name),
    });
  }

  return deltas;
}

/**
 * Create RDF representation of case
 * @param {string} caseId - Case identifier
 * @param {string} workflowId - Workflow identifier
 * @param {Map<string, Object>} workItems - Work items map
 * @param {Object} store - RDF store
 * @returns {Array<Object>} RDF deltas
 */
export function createCaseRDF(caseId, workflowId, workItems, store) {
  const deltas = [];
  const df = store._store?.dataFactory || {
    namedNode: (v) => ({ termType: 'NamedNode', value: v }),
    literal: (v) => ({ termType: 'Literal', value: v }),
  };

  const caseUri = `${YAWL_NS.CASE}${caseId}`;
  const caseNode = df.namedNode(caseUri);
  const workflowUri = `${YAWL_NS.WORKFLOW}${workflowId}`;

  // Add case type
  deltas.push({
    type: 'add',
    subject: caseNode,
    predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: df.namedNode(`${YAWL_NS.BASE}Case`),
  });

  // Link to workflow
  deltas.push({
    type: 'add',
    subject: caseNode,
    predicate: df.namedNode(`${YAWL_NS.BASE}instanceOf`),
    object: df.namedNode(workflowUri),
  });

  // Add work items
  for (const [taskId, workItem] of workItems) {
    const workItemUri = `${YAWL_NS.WORK_ITEM}${workItem.id}`;
    const workItemNode = df.namedNode(workItemUri);

    deltas.push({
      type: 'add',
      subject: caseNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}hasWorkItem`),
      object: workItemNode,
    });

    deltas.push({
      type: 'add',
      subject: workItemNode,
      predicate: df.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      object: df.namedNode(`${YAWL_NS.BASE}WorkItem`),
    });

    deltas.push({
      type: 'add',
      subject: workItemNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}forTask`),
      object: df.namedNode(`${YAWL_NS.WORKFLOW}${workflowId}/task/${taskId}`),
    });

    deltas.push({
      type: 'add',
      subject: workItemNode,
      predicate: df.namedNode(`${YAWL_NS.BASE}status`),
      object: df.literal(workItem.status),
    });
  }

  return deltas;
}
