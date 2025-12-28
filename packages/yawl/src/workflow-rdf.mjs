/**
 * @file YAWL Workflow - RDF serialization and deserialization
 * @module @unrdf/yawl/workflow-rdf
 *
 * @description
 * RDF integration for YAWL workflows including:
 * - Serialization to RDF representation
 * - Deserialization from RDF stores
 * - YAWL ontology mapping
 */

import { dataFactory } from '@unrdf/oxigraph';
import { WorkflowError } from '../errors.mjs';
import {
  YAWL,
  YAWL_TASK,
  WorkflowSpec,
  Task,
  Flow,
  AtomicTask,
  CompositeTask,
  MultipleInstanceTask,
  XOR_Split,
  AND_Split,
  OR_Split,
  XOR_Join,
  AND_Join,
  OR_Join,
  rdfType,
  rdfsLabel,
  taskName as taskNameProp,
  taskId as taskIdProp,
  kind,
  joinsTo,
  joinsFrom,
  splitBehavior,
  joinBehavior,
  hasTasks,
  cancellationSet,
  sourceTask,
  targetTask,
  flowCondition,
  flowPriority,
  isDefaultFlow,
  specUri,
  taskUri,
  flowUri,
  stringLiteral,
  integerLiteral,
  booleanLiteral,
} from './ontology/yawl-ontology.mjs';

const { quad, namedNode, literal } = dataFactory;

// =============================================================================
// RDF Type Mappings
// =============================================================================

/**
 * Split type to RDF node mapping
 */
const SPLIT_TYPE_TO_RDF = {
  sequence: null,
  and: AND_Split,
  xor: XOR_Split,
  or: OR_Split,
};

/**
 * Join type to RDF node mapping
 */
const JOIN_TYPE_TO_RDF = {
  sequence: null,
  and: AND_Join,
  xor: XOR_Join,
  or: OR_Join,
};

/**
 * Task kind to RDF node mapping
 */
const TASK_KIND_TO_RDF = {
  atomic: AtomicTask,
  composite: CompositeTask,
  multiple: MultipleInstanceTask,
};

/**
 * RDF split type to internal type mapping
 */
const RDF_TO_SPLIT_TYPE = {
  [AND_Split.value]: 'and',
  [XOR_Split.value]: 'xor',
  [OR_Split.value]: 'or',
};

/**
 * RDF join type to internal type mapping
 */
const RDF_TO_JOIN_TYPE = {
  [AND_Join.value]: 'and',
  [XOR_Join.value]: 'xor',
  [OR_Join.value]: 'or',
};

/**
 * RDF task kind to internal kind mapping
 */
const RDF_TO_TASK_KIND = {
  [AtomicTask.value]: 'atomic',
  [CompositeTask.value]: 'composite',
  [MultipleInstanceTask.value]: 'multiple',
};

// =============================================================================
// Serialization
// =============================================================================

/**
 * Serialize workflow to RDF representation in store
 *
 * Creates YAWL RDF representation using yawl-ontology predicates and classes.
 *
 * @param {Workflow} workflow - Workflow to serialize
 * @param {Object} store - RDF store (from @unrdf/oxigraph)
 * @param {Object} [options={}] - Serialization options
 * @param {string} [options.graph] - Named graph URI (defaults to spec-based)
 * @returns {Object} Result with specUri and quad count
 *
 * @example
 * import { createStore } from '@unrdf/oxigraph';
 * import { createWorkflow, workflowToRDF } from '@unrdf/yawl';
 *
 * const store = createStore();
 * const workflow = createWorkflow({ ... });
 * const { specUri, quadCount } = workflowToRDF(workflow, store);
 */
export function workflowToRDF(workflow, store, options = {}) {
  try {
    const specNode = specUri(workflow.id);
    const graphUri = options.graph || `${YAWL}specs/${workflow.id}`;
    const graph = namedNode(graphUri);

    let quadCount = 0;

    // Helper to add quad and count
    const addQuad = (s, p, o, g = graph) => {
      store.add(quad(s, p, o, g));
      quadCount++;
    };

  // Add workflow spec type
  addQuad(specNode, rdfType, WorkflowSpec);

  // Add workflow name
  if (workflow.name) {
    addQuad(specNode, rdfsLabel, stringLiteral(workflow.name));
  }

  // Add workflow version
  if (workflow.version) {
    addQuad(specNode, namedNode(YAWL + 'version'), stringLiteral(workflow.version));
  }

  // Add description
  if (workflow.description) {
    addQuad(specNode, namedNode(YAWL + 'description'), stringLiteral(workflow.description));
  }

  // Add tasks
  for (const task of workflow.getTasks()) {
    const taskNode = taskUri(task.id);

    // Link spec to task
    addQuad(specNode, hasTasks, taskNode);

    // Add task type
    addQuad(taskNode, rdfType, Task);

    // Add task ID
    addQuad(taskNode, taskIdProp, stringLiteral(task.id));

    // Add task name
    if (task.name) {
      addQuad(taskNode, taskNameProp, stringLiteral(task.name));
    }

    // Add task kind
    const kindNode = TASK_KIND_TO_RDF[task.kind];
    if (kindNode) {
      addQuad(taskNode, kind, kindNode);
    }

    // Add split behavior
    const splitNode = SPLIT_TYPE_TO_RDF[task.splitType];
    if (splitNode) {
      addQuad(taskNode, splitBehavior, splitNode);
    }

    // Add join behavior
    const joinNode = JOIN_TYPE_TO_RDF[task.joinType];
    if (joinNode) {
      addQuad(taskNode, joinBehavior, joinNode);
    }

    // Add cancellation set
    if (task.cancellationSet) {
      for (const cancelTaskId of task.cancellationSet) {
        addQuad(taskNode, cancellationSet, taskUri(cancelTaskId));
      }
    }

    // Add documentation
    if (task.documentation) {
      addQuad(taskNode, namedNode(YAWL + 'documentation'), stringLiteral(task.documentation));
    }

    // Add priority
    if (task.priority !== undefined) {
      addQuad(taskNode, namedNode(YAWL + 'priority'), integerLiteral(task.priority));
    }
  }

  // Add flows
  for (const flow of workflow.getFlows()) {
    const flowNode = flowUri(flow.from, flow.to);

    // Add flow type
    addQuad(flowNode, rdfType, Flow);

    // Add source and target
    addQuad(flowNode, sourceTask, taskUri(flow.from));
    addQuad(flowNode, targetTask, taskUri(flow.to));

    // Add outgoing/incoming relations
    addQuad(taskUri(flow.from), joinsTo, taskUri(flow.to));
    addQuad(taskUri(flow.to), joinsFrom, taskUri(flow.from));

    // Add priority
    if (flow.priority !== undefined && flow.priority !== 0) {
      addQuad(flowNode, flowPriority, integerLiteral(flow.priority));
    }

    // Add default flag
    if (flow.isDefault) {
      addQuad(flowNode, isDefaultFlow, booleanLiteral(true));
    }

    // Add documentation
    if (flow.documentation) {
      addQuad(flowNode, namedNode(YAWL + 'documentation'), stringLiteral(flow.documentation));
    }
  }

  // Add start/end markers
  if (workflow.getStartTaskId()) {
    addQuad(specNode, namedNode(YAWL + 'startTask'), taskUri(workflow.getStartTaskId()));
  }

  for (const endTaskId of workflow.getEndTaskIds()) {
    addQuad(specNode, namedNode(YAWL + 'endTask'), taskUri(endTaskId));
  }

    return {
      specUri: specNode.value,
      graph: graphUri,
      quadCount,
    };
  } catch (err) {
    throw new WorkflowError('Failed to serialize workflow to RDF', {
      cause: err,
      context: { workflowId: workflow?.id },
    });
  }
}

// =============================================================================
// Deserialization
// =============================================================================

/**
 * Load workflow from RDF store
 *
 * Reconstructs a Workflow instance from its RDF representation.
 *
 * @param {Object} store - RDF store (from @unrdf/oxigraph)
 * @param {string} workflowId - Workflow specification ID
 * @param {Object} [options={}] - Loading options
 * @param {string} [options.graph] - Named graph URI to query
 * @param {Function} WorkflowClass - Workflow constructor to use
 * @returns {Promise<Workflow|null>} Workflow instance or null if not found
 *
 * @example
 * import { createStore } from '@unrdf/oxigraph';
 * import { workflowFromRDF } from '@unrdf/yawl';
 *
 * const workflow = await workflowFromRDF(store, 'expense-approval');
 * if (workflow) {
 *   console.log(`Loaded workflow with ${workflow.getTasks().length} tasks`);
 * }
 */
export async function workflowFromRDF(store, workflowId, options = {}, WorkflowClass) {
  try {
    const specNode = specUri(workflowId);
    const graphUri = options.graph || `${YAWL}specs/${workflowId}`;
    const graph = namedNode(graphUri);

    // Check if workflow exists
    const typeQuads = store.match(specNode, rdfType, WorkflowSpec, graph);
    if (typeQuads.length === 0) {
      // Try without graph constraint
      const allTypeQuads = store.match(specNode, rdfType, WorkflowSpec, null);
      if (allTypeQuads.length === 0) {
        return null;
      }
    }

  // Get workflow name
  const labelQuads = store.match(specNode, rdfsLabel, null, null);
  const name = labelQuads.length > 0 ? labelQuads[0].object.value : workflowId;

  // Get workflow version
  const versionQuads = store.match(specNode, namedNode(YAWL + 'version'), null, null);
  const version = versionQuads.length > 0 ? versionQuads[0].object.value : '1.0.0';

  // Get description
  const descQuads = store.match(specNode, namedNode(YAWL + 'description'), null, null);
  const description = descQuads.length > 0 ? descQuads[0].object.value : undefined;

  // Get tasks
  const taskQuads = store.match(specNode, hasTasks, null, null);
  const tasks = [];

  for (const taskQuad of taskQuads) {
    const taskNode = taskQuad.object;

    // Get task ID
    const taskIdQuads = store.match(taskNode, taskIdProp, null, null);
    const taskId = taskIdQuads.length > 0
      ? taskIdQuads[0].object.value
      : taskNode.value.replace(YAWL_TASK, '');

    // Get task name
    const taskNameQuads = store.match(taskNode, taskNameProp, null, null);
    const taskName = taskNameQuads.length > 0 ? taskNameQuads[0].object.value : taskId;

    // Get task kind
    const kindQuads = store.match(taskNode, kind, null, null);
    let taskKind = 'atomic';
    if (kindQuads.length > 0) {
      taskKind = RDF_TO_TASK_KIND[kindQuads[0].object.value] ?? 'atomic';
    }

    // Get split behavior
    const splitQuads = store.match(taskNode, splitBehavior, null, null);
    let splitType = 'sequence';
    if (splitQuads.length > 0) {
      splitType = RDF_TO_SPLIT_TYPE[splitQuads[0].object.value] ?? 'sequence';
    }

    // Get join behavior
    const joinQuads = store.match(taskNode, joinBehavior, null, null);
    let joinType = 'sequence';
    if (joinQuads.length > 0) {
      joinType = RDF_TO_JOIN_TYPE[joinQuads[0].object.value] ?? 'sequence';
    }

    // Get cancellation set
    const cancelQuads = store.match(taskNode, cancellationSet, null, null);
    const cancelSet = cancelQuads.map(q => q.object.value.replace(YAWL_TASK, ''));

    // Get priority
    const priorityQuads = store.match(taskNode, namedNode(YAWL + 'priority'), null, null);
    const priority = priorityQuads.length > 0
      ? parseInt(priorityQuads[0].object.value, 10)
      : undefined;

    // Get documentation
    const docQuads = store.match(taskNode, namedNode(YAWL + 'documentation'), null, null);
    const documentation = docQuads.length > 0 ? docQuads[0].object.value : undefined;

    tasks.push({
      id: taskId,
      name: taskName,
      kind: taskKind,
      splitType,
      joinType,
      cancellationSet: cancelSet.length > 0 ? cancelSet : undefined,
      priority,
      documentation,
    });
  }

  // Get flows by querying joinsTo relationships
  const flows = [];
  for (const task of tasks) {
    const flowQuads = store.match(taskUri(task.id), joinsTo, null, null);
    for (const flowQuad of flowQuads) {
      const toTaskId = flowQuad.object.value.replace(YAWL_TASK, '');

      // Check for flow node with properties
      const flowNode = flowUri(task.id, toTaskId);
      const priorityQuads = store.match(flowNode, flowPriority, null, null);
      const priority = priorityQuads.length > 0
        ? parseInt(priorityQuads[0].object.value, 10)
        : 0;

      const defaultQuads = store.match(flowNode, isDefaultFlow, null, null);
      const isDefault = defaultQuads.length > 0
        && defaultQuads[0].object.value === 'true';

      flows.push({
        from: task.id,
        to: toTaskId,
        priority,
        isDefault: isDefault || undefined,
      });
    }
  }

  // Get start task
  const startQuads = store.match(specNode, namedNode(YAWL + 'startTask'), null, null);
  const startTaskId = startQuads.length > 0
    ? startQuads[0].object.value.replace(YAWL_TASK, '')
    : undefined;

  // Get end tasks
  const endQuads = store.match(specNode, namedNode(YAWL + 'endTask'), null, null);
  const endTaskIds = endQuads.map(q => q.object.value.replace(YAWL_TASK, ''));

    // Create workflow using provided class
    return new WorkflowClass({
      id: workflowId,
      name,
      version,
      description,
      tasks,
      flows,
      startTaskId,
      endTaskIds,
    });
  } catch (err) {
    throw new WorkflowError('Failed to deserialize workflow from RDF', {
      cause: err,
      context: { workflowId },
    });
  }
}
