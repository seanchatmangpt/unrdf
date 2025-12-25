/**
 * @file YAWL Store - RDF store integration with SPARQL queries
 * @module @unrdf/yawl/store
 *
 * Provides store operations for YAWL workflow management using Oxigraph.
 * All operations use RDF quads with named graphs for case isolation.
 *
 * @example
 * import { createYawlStore, addCase, getCase, queryWorkItems } from '@unrdf/yawl/store';
 *
 * const store = createYawlStore();
 * addCase(store, {
 *   id: 'case-123',
 *   specId: 'expense-approval',
 *   status: 'active',
 *   createdAt: new Date()
 * });
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  // Namespaces
  YAWL,
  YAWL_CASE,
  SPARQL_PREFIXES,

  // Classes
  WorkflowCase,
  WorkflowSpec,
  Task,
  WorkItem,
  AtomicTask,
  CompositeTask,
  ManualTask,
  AutomatedTask,

  // Status values
  Case_Active,
  Case_Completed,
  Case_Suspended,
  Case_Cancelled,
  Case_Failed,
  WorkItem_Enabled,
  WorkItem_Fired,
  WorkItem_Allocated,
  WorkItem_Started,
  WorkItem_Completed,
  WorkItem_Suspended,
  WorkItem_Cancelled,
  WorkItem_Failed,

  // Split/Join
  XOR_Split,
  AND_Split,
  OR_Split,
  XOR_Join,
  AND_Join,
  OR_Join,

  // Properties
  specId,
  status,
  createdAt,
  updatedAt,
  completedAt,
  workItems,
  taskName,
  taskId as taskIdProp,
  kind,
  joinsTo,
  joinsFrom,
  splitBehavior,
  joinBehavior,
  hasTasks,
  taskRef,
  caseRef,
  owner,
  startedBy,
  completedBy,
  workItemData,

  // RDF terms
  rdfType,
  rdfsLabel,

  // URI factories
  caseUri,
  taskUri,
  workItemUri,
  specUri,
  caseGraph,

  // Literal factories
  dateTimeLiteral,
  stringLiteral,
} from '../ontology/yawl-ontology.mjs';

const { quad, namedNode } = dataFactory;

// =============================================================================
// STATUS MAPPINGS
// =============================================================================

/**
 * Map case status string to RDF term
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const CASE_STATUS_MAP = {
  active: Case_Active,
  completed: Case_Completed,
  suspended: Case_Suspended,
  cancelled: Case_Cancelled,
  failed: Case_Failed,
};

/**
 * Map work item status string to RDF term
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const WORKITEM_STATUS_MAP = {
  enabled: WorkItem_Enabled,
  fired: WorkItem_Fired,
  allocated: WorkItem_Allocated,
  started: WorkItem_Started,
  completed: WorkItem_Completed,
  suspended: WorkItem_Suspended,
  cancelled: WorkItem_Cancelled,
  failed: WorkItem_Failed,
};

/**
 * Map task kind string to RDF term
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const TASK_KIND_MAP = {
  atomic: AtomicTask,
  composite: CompositeTask,
  manual: ManualTask,
  automated: AutomatedTask,
};

/**
 * Map split behavior string to RDF term
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const SPLIT_BEHAVIOR_MAP = {
  xor: XOR_Split,
  and: AND_Split,
  or: OR_Split,
};

/**
 * Map join behavior string to RDF term
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const JOIN_BEHAVIOR_MAP = {
  xor: XOR_Join,
  and: AND_Join,
  or: OR_Join,
};

// =============================================================================
// STORE CREATION
// =============================================================================

/**
 * Create a new YAWL store with ontology loaded
 * @returns {import('@unrdf/oxigraph').OxigraphStore} Configured store instance
 * @example
 * const store = createYawlStore();
 * // Store is ready for YAWL operations
 */
export function createYawlStore() {
  const store = createStore();

  // Load YAWL ontology metadata (minimal bootstrap)
  const ontologyGraph = namedNode(YAWL + 'ontology');

  store.add(
    quad(namedNode(YAWL), rdfType, namedNode('http://www.w3.org/2002/07/owl#Ontology'), ontologyGraph)
  );
  store.add(quad(namedNode(YAWL), rdfsLabel, stringLiteral('YAWL Ontology'), ontologyGraph));

  return store;
}

// =============================================================================
// CASE OPERATIONS
// =============================================================================

/**
 * @typedef {Object} CaseInput
 * @property {string} id - Unique case identifier
 * @property {string} specId - Workflow specification ID
 * @property {string} [status='active'] - Case status
 * @property {Date} [createdAt] - Creation timestamp
 * @property {Object} [data] - Case data/variables
 */

/**
 * Add a workflow case to the store
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {CaseInput} caseInput - Case data
 * @returns {import('oxigraph').NamedNode} The case URI
 * @example
 * const uri = addCase(store, {
 *   id: 'case-123',
 *   specId: 'expense-approval',
 *   status: 'active',
 *   createdAt: new Date()
 * });
 */
export function addCase(store, caseInput) {
  const { id, specId: specIdValue, status: statusValue = 'active', createdAt: createdAtValue, data } = caseInput;

  const caseNode = caseUri(id);
  const graph = caseGraph(id);

  // Add case type
  store.add(quad(caseNode, rdfType, WorkflowCase, graph));

  // Add specification reference
  store.add(quad(caseNode, specId, specUri(specIdValue), graph));

  // Add status
  const statusNode = CASE_STATUS_MAP[statusValue] || Case_Active;
  store.add(quad(caseNode, status, statusNode, graph));

  // Add creation timestamp
  const timestamp = createdAtValue || new Date();
  store.add(quad(caseNode, createdAt, dateTimeLiteral(timestamp), graph));

  // Add case data if provided
  if (data) {
    store.add(quad(caseNode, namedNode(YAWL + 'caseData'), stringLiteral(JSON.stringify(data)), graph));
  }

  return caseNode;
}

/**
 * @typedef {Object} CaseResult
 * @property {string} id - Case identifier
 * @property {string} specId - Workflow specification ID
 * @property {string} status - Case status
 * @property {string} [createdAt] - Creation timestamp
 * @property {string} [updatedAt] - Last update timestamp
 * @property {Array<WorkItemResult>} workItems - Associated work items
 */

/**
 * Get a case by ID with all work items
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @returns {CaseResult|null} Case data or null if not found
 * @example
 * const caseData = getCase(store, 'case-123');
 * console.log(caseData.status); // 'active'
 * console.log(caseData.workItems.length); // 2
 */
export function getCase(store, caseId) {
  const caseNode = caseUri(caseId);
  const graph = caseGraph(caseId);

  // Check if case exists
  const typeQuads = store.match(caseNode, rdfType, WorkflowCase, graph);
  if (typeQuads.length === 0) {
    return null;
  }

  // Get case properties
  const result = {
    id: caseId,
    specId: null,
    status: null,
    createdAt: null,
    updatedAt: null,
    workItems: [],
  };

  // Extract specId
  const specQuads = store.match(caseNode, specId, null, graph);
  if (specQuads.length > 0) {
    const specValue = specQuads[0].object.value;
    result.specId = specValue.replace(YAWL + 'spec-', '');
  }

  // Extract status
  const statusQuads = store.match(caseNode, status, null, graph);
  if (statusQuads.length > 0) {
    const statusValue = statusQuads[0].object.value;
    result.status = statusValue.replace(YAWL, '').replace('Case_', '').toLowerCase();
  }

  // Extract timestamps
  const createdQuads = store.match(caseNode, createdAt, null, graph);
  if (createdQuads.length > 0) {
    result.createdAt = createdQuads[0].object.value;
  }

  const updatedQuads = store.match(caseNode, updatedAt, null, graph);
  if (updatedQuads.length > 0) {
    result.updatedAt = updatedQuads[0].object.value;
  }

  // Get work items
  result.workItems = queryWorkItems(store, caseId);

  return result;
}

/**
 * Update case status
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @param {string} newStatus - New status value
 * @returns {boolean} True if updated successfully
 */
export function updateCaseStatus(store, caseId, newStatus) {
  const caseNode = caseUri(caseId);
  const graph = caseGraph(caseId);

  // Remove old status
  const oldQuads = store.match(caseNode, status, null, graph);
  for (const q of oldQuads) {
    store.delete(q);
  }

  // Add new status
  const statusNode = CASE_STATUS_MAP[newStatus];
  if (!statusNode) {
    throw new Error(`Invalid case status: ${newStatus}`);
  }

  store.add(quad(caseNode, status, statusNode, graph));

  // Update timestamp
  const oldUpdated = store.match(caseNode, updatedAt, null, graph);
  for (const q of oldUpdated) {
    store.delete(q);
  }
  store.add(quad(caseNode, updatedAt, dateTimeLiteral(new Date()), graph));

  return true;
}

// =============================================================================
// WORK ITEM OPERATIONS
// =============================================================================

/**
 * @typedef {Object} WorkItemInput
 * @property {string} id - Work item identifier
 * @property {string} taskId - Task reference
 * @property {string} caseId - Case reference
 * @property {string} [status='enabled'] - Work item status
 * @property {string} [owner] - Assigned owner
 * @property {Date} [createdAt] - Creation timestamp
 * @property {Object} [data] - Work item data
 */

/**
 * Add a work item to the store
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {WorkItemInput} workItemInput - Work item data
 * @returns {import('oxigraph').NamedNode} The work item URI
 */
export function addWorkItem(store, workItemInput) {
  const {
    id,
    taskId: taskIdValue,
    caseId: caseIdValue,
    status: statusValue = 'enabled',
    owner: ownerValue,
    createdAt: createdAtValue,
    data,
  } = workItemInput;

  const workItemNode = workItemUri(id);
  const graph = caseGraph(caseIdValue);

  // Add work item type
  store.add(quad(workItemNode, rdfType, WorkItem, graph));

  // Add task reference
  store.add(quad(workItemNode, taskRef, taskUri(taskIdValue), graph));

  // Add case reference
  store.add(quad(workItemNode, caseRef, caseUri(caseIdValue), graph));

  // Add status
  const statusNode = WORKITEM_STATUS_MAP[statusValue] || WorkItem_Enabled;
  store.add(quad(workItemNode, status, statusNode, graph));

  // Add owner if provided
  if (ownerValue) {
    store.add(quad(workItemNode, owner, namedNode(ownerValue), graph));
  }

  // Add creation timestamp
  const timestamp = createdAtValue || new Date();
  store.add(quad(workItemNode, createdAt, dateTimeLiteral(timestamp), graph));

  // Add work item data if provided
  if (data) {
    store.add(quad(workItemNode, workItemData, stringLiteral(JSON.stringify(data)), graph));
  }

  // Link from case to work item
  const caseNode = caseUri(caseIdValue);
  store.add(quad(caseNode, workItems, workItemNode, graph));

  return workItemNode;
}

/**
 * @typedef {Object} WorkItemResult
 * @property {string} id - Work item identifier
 * @property {string} taskId - Task reference
 * @property {string} status - Work item status
 * @property {string} [owner] - Assigned owner
 * @property {string} [createdAt] - Creation timestamp
 */

/**
 * Query all work items for a case
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @returns {Array<WorkItemResult>} Array of work items
 */
export function queryWorkItems(store, caseId) {
  const graph = caseGraph(caseId);
  const caseNode = caseUri(caseId);

  // Find all work items linked to this case
  const workItemQuads = store.match(null, caseRef, caseNode, graph);

  const results = [];

  for (const q of workItemQuads) {
    const workItemNode = q.subject;
    const workItemId = workItemNode.value.replace(YAWL_CASE.replace('/case#', '/workitem#'), '');

    const item = {
      id: workItemId,
      taskId: null,
      status: null,
      owner: null,
      createdAt: null,
    };

    // Get task reference
    const taskQuads = store.match(workItemNode, taskRef, null, graph);
    if (taskQuads.length > 0) {
      item.taskId = taskQuads[0].object.value.replace(YAWL_CASE.replace('/case#', '/task#'), '');
    }

    // Get status
    const statusQuads = store.match(workItemNode, status, null, graph);
    if (statusQuads.length > 0) {
      const statusValue = statusQuads[0].object.value;
      item.status = statusValue.replace(YAWL, '').replace('WorkItem_', '').toLowerCase();
    }

    // Get owner
    const ownerQuads = store.match(workItemNode, owner, null, graph);
    if (ownerQuads.length > 0) {
      item.owner = ownerQuads[0].object.value;
    }

    // Get creation timestamp
    const createdQuads = store.match(workItemNode, createdAt, null, graph);
    if (createdQuads.length > 0) {
      item.createdAt = createdQuads[0].object.value;
    }

    results.push(item);
  }

  return results;
}

/**
 * Query enabled tasks for a case (ready for execution)
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @returns {Array<WorkItemResult>} Array of enabled work items
 */
export function queryEnabledTasks(store, caseId) {
  const allWorkItems = queryWorkItems(store, caseId);
  return allWorkItems.filter(item => item.status === 'enabled');
}

/**
 * Update work item status
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} workItemId - Work item identifier
 * @param {string} caseId - Case identifier (for graph context)
 * @param {string} newStatus - New status value
 * @param {Object} [options] - Additional options
 * @param {string} [options.completedBy] - Who completed the work item
 * @returns {boolean} True if updated successfully
 */
export function updateWorkItemStatus(store, workItemId, caseId, newStatus, options = {}) {
  const workItemNode = workItemUri(workItemId);
  const graph = caseGraph(caseId);

  // Remove old status
  const oldQuads = store.match(workItemNode, status, null, graph);
  for (const q of oldQuads) {
    store.delete(q);
  }

  // Add new status
  const statusNode = WORKITEM_STATUS_MAP[newStatus];
  if (!statusNode) {
    throw new Error(`Invalid work item status: ${newStatus}`);
  }

  store.add(quad(workItemNode, status, statusNode, graph));

  // Update timestamp
  const oldUpdated = store.match(workItemNode, updatedAt, null, graph);
  for (const q of oldUpdated) {
    store.delete(q);
  }
  store.add(quad(workItemNode, updatedAt, dateTimeLiteral(new Date()), graph));

  // Add completion info
  if (newStatus === 'completed' && options.completedBy) {
    store.add(quad(workItemNode, completedBy, namedNode(options.completedBy), graph));
    store.add(quad(workItemNode, completedAt, dateTimeLiteral(new Date()), graph));
  }

  return true;
}

// =============================================================================
// TASK/SPECIFICATION OPERATIONS
// =============================================================================

/**
 * @typedef {Object} TaskInput
 * @property {string} id - Task identifier
 * @property {string} name - Task name
 * @property {string} [kind='atomic'] - Task kind
 * @property {string} [splitBehavior] - Split behavior
 * @property {string} [joinBehavior] - Join behavior
 * @property {Array<string>} [joinsTo] - Outgoing task IDs
 * @property {Array<string>} [joinsFrom] - Incoming task IDs
 */

/**
 * @typedef {Object} WorkflowSpecInput
 * @property {string} id - Specification identifier
 * @property {string} name - Specification name
 * @property {Array<TaskInput>} tasks - Task definitions
 */

/**
 * Add a workflow specification to the store
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {WorkflowSpecInput} specInput - Specification data
 * @returns {import('oxigraph').NamedNode} The specification URI
 */
export function addWorkflowSpec(store, specInput) {
  const { id, name, tasks = [] } = specInput;

  const specNode = specUri(id);
  const graph = namedNode(YAWL + 'specs');

  // Add spec type
  store.add(quad(specNode, rdfType, WorkflowSpec, graph));

  // Add spec name
  store.add(quad(specNode, rdfsLabel, stringLiteral(name), graph));

  // Add tasks
  for (const taskInput of tasks) {
    const taskNode = addTask(store, taskInput, specNode, graph);
    store.add(quad(specNode, hasTasks, taskNode, graph));
  }

  return specNode;
}

/**
 * Add a task to the store
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {TaskInput} taskInput - Task data
 * @param {import('oxigraph').NamedNode} specNode - Parent specification
 * @param {import('oxigraph').NamedNode} graph - Named graph
 * @returns {import('oxigraph').NamedNode} The task URI
 */
export function addTask(store, taskInput, specNode, graph) {
  const {
    id,
    name,
    kind: kindValue = 'atomic',
    splitBehavior: splitValue,
    joinBehavior: joinValue,
    joinsTo: joinsToValue = [],
    joinsFrom: joinsFromValue = [],
  } = taskInput;

  const taskNode = taskUri(id);

  // Add task type
  store.add(quad(taskNode, rdfType, Task, graph));

  // Add task ID
  store.add(quad(taskNode, taskIdProp, stringLiteral(id), graph));

  // Add task name
  store.add(quad(taskNode, taskName, stringLiteral(name), graph));

  // Add task kind
  const kindNode = TASK_KIND_MAP[kindValue] || AtomicTask;
  store.add(quad(taskNode, kind, kindNode, graph));

  // Add split behavior if specified
  if (splitValue && SPLIT_BEHAVIOR_MAP[splitValue]) {
    store.add(quad(taskNode, splitBehavior, SPLIT_BEHAVIOR_MAP[splitValue], graph));
  }

  // Add join behavior if specified
  if (joinValue && JOIN_BEHAVIOR_MAP[joinValue]) {
    store.add(quad(taskNode, joinBehavior, JOIN_BEHAVIOR_MAP[joinValue], graph));
  }

  // Add outgoing flows
  for (const targetId of joinsToValue) {
    store.add(quad(taskNode, joinsTo, taskUri(targetId), graph));
  }

  // Add incoming flows
  for (const sourceId of joinsFromValue) {
    store.add(quad(taskNode, joinsFrom, taskUri(sourceId), graph));
  }

  return taskNode;
}

// =============================================================================
// SPARQL QUERY EXECUTION
// =============================================================================

/**
 * Execute a SPARQL SELECT query
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} query - SPARQL query string
 * @returns {Array<Object>} Query results
 */
export function executeSparqlSelect(store, query) {
  return store.query(query);
}

/**
 * Execute a SPARQL ASK query
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} query - SPARQL ASK query string
 * @returns {boolean} Query result
 */
export function executeSparqlAsk(store, query) {
  return store.query(query);
}

/**
 * Get work items for a case using SPARQL
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @returns {Array<Object>} Work items
 */
export function sparqlQueryWorkItems(store, caseId) {
  const query = `
${SPARQL_PREFIXES}
SELECT ?workItem ?task ?status ?owner ?createdAt
WHERE {
  GRAPH <${YAWL_CASE}${caseId}/graph> {
    ?workItem rdf:type yawl:WorkItem ;
              yawl:taskRef ?task ;
              yawl:status ?status .
    OPTIONAL { ?workItem yawl:owner ?owner }
    OPTIONAL { ?workItem yawl:createdAt ?createdAt }
  }
}
ORDER BY ?createdAt
`;

  return store.query(query);
}

/**
 * Get enabled work items using SPARQL
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @returns {Array<Object>} Enabled work items
 */
export function sparqlQueryEnabledTasks(store, caseId) {
  const query = `
${SPARQL_PREFIXES}
SELECT ?workItem ?task ?taskName
WHERE {
  GRAPH <${YAWL_CASE}${caseId}/graph> {
    ?workItem rdf:type yawl:WorkItem ;
              yawl:taskRef ?task ;
              yawl:status yawl:WorkItem_Enabled .
  }
  GRAPH <${YAWL}specs> {
    ?task yawl:taskName ?taskName .
  }
}
`;

  return store.query(query);
}

/**
 * Check if AND-join is satisfied using SPARQL
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} taskId - Task identifier
 * @param {string} caseId - Case identifier
 * @returns {boolean} True if all predecessors completed
 */
export function sparqlCheckAndJoinSatisfied(store, taskId, caseId) {
  const query = `
${SPARQL_PREFIXES}
ASK {
  GRAPH <${YAWL}specs> {
    <${YAWL_CASE.replace('/case#', '/task#')}${taskId}> yawl:joinBehavior yawl:AND_Join .

    # Ensure all predecessor tasks have completed work items
    FILTER NOT EXISTS {
      ?predecessorTask yawl:joinsTo <${YAWL_CASE.replace('/case#', '/task#')}${taskId}> .
      FILTER NOT EXISTS {
        GRAPH <${YAWL_CASE}${caseId}/graph> {
          ?workItem yawl:taskRef ?predecessorTask ;
                    yawl:status yawl:WorkItem_Completed .
        }
      }
    }
  }
}
`;

  return store.query(query);
}

/**
 * Find next eligible tasks after completion using SPARQL
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} completedTaskId - Completed task ID
 * @param {string} caseId - Case identifier
 * @returns {Array<Object>} Next eligible tasks
 */
export function sparqlFindNextTasks(store, completedTaskId, caseId) {
  const query = `
${SPARQL_PREFIXES}
SELECT DISTINCT ?nextTask ?taskName ?joinBehavior
WHERE {
  GRAPH <${YAWL}specs> {
    <${YAWL_CASE.replace('/case#', '/task#')}${completedTaskId}> yawl:joinsTo ?nextTask .
    ?nextTask yawl:taskName ?taskName .
    OPTIONAL { ?nextTask yawl:joinBehavior ?joinBehavior }
  }

  # Exclude tasks that already have active work items
  FILTER NOT EXISTS {
    GRAPH <${YAWL_CASE}${caseId}/graph> {
      ?existingItem yawl:taskRef ?nextTask ;
                    yawl:status ?existingStatus .
      FILTER (?existingStatus IN (yawl:WorkItem_Enabled, yawl:WorkItem_Started, yawl:WorkItem_Allocated))
    }
  }
}
`;

  return store.query(query);
}

// =============================================================================
// UTILITY FUNCTIONS
// =============================================================================

/**
 * Get store statistics
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @returns {Object} Store statistics
 */
export function getStoreStats(store) {
  return {
    totalQuads: store.size,
    timestamp: new Date().toISOString(),
  };
}

/**
 * Clear all data for a case
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @returns {number} Number of quads removed
 */
export function clearCase(store, caseId) {
  const graph = caseGraph(caseId);
  const quads = store.match(null, null, null, graph);
  let count = 0;

  for (const q of quads) {
    store.delete(q);
    count++;
  }

  return count;
}

/**
 * Export case data as Turtle
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - The RDF store
 * @param {string} caseId - Case identifier
 * @returns {string} Turtle serialization
 */
export function exportCaseAsTurtle(store, caseId) {
  const graph = caseGraph(caseId);
  const quads = store.match(null, null, null, graph);

  // Build Turtle output
  const lines = [
    `@prefix yawl: <${YAWL}> .`,
    `@prefix yawl-case: <${YAWL_CASE}> .`,
    `@prefix yawl-task: <${YAWL_CASE.replace('/case#', '/task#')}> .`,
    `@prefix yawl-work: <${YAWL_CASE.replace('/case#', '/workitem#')}> .`,
    `@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .`,
    `@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .`,
    '',
  ];

  // Group by subject
  const bySubject = new Map();
  for (const q of quads) {
    const subject = q.subject.value;
    if (!bySubject.has(subject)) {
      bySubject.set(subject, []);
    }
    bySubject.get(subject).push(q);
  }

  for (const [subject, subjectQuads] of bySubject) {
    lines.push(`<${subject}>`);
    const predicates = subjectQuads.map((q, i) => {
      const pred = q.predicate.value;
      const obj = q.object;
      let objStr;
      if (obj.termType === 'NamedNode') {
        objStr = `<${obj.value}>`;
      } else if (obj.termType === 'Literal') {
        if (obj.datatype && obj.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
          objStr = `"${obj.value}"^^<${obj.datatype.value}>`;
        } else {
          objStr = `"${obj.value}"`;
        }
      } else {
        objStr = obj.value;
      }
      const sep = i === subjectQuads.length - 1 ? ' .' : ' ;';
      return `  <${pred}> ${objStr}${sep}`;
    });
    lines.push(...predicates);
    lines.push('');
  }

  return lines.join('\n');
}

export default {
  createYawlStore,
  addCase,
  getCase,
  updateCaseStatus,
  addWorkItem,
  queryWorkItems,
  queryEnabledTasks,
  updateWorkItemStatus,
  addWorkflowSpec,
  addTask,
  executeSparqlSelect,
  executeSparqlAsk,
  sparqlQueryWorkItems,
  sparqlQueryEnabledTasks,
  sparqlCheckAndJoinSatisfied,
  sparqlFindNextTasks,
  getStoreStats,
  clearCase,
  exportCaseAsTurtle,
};
