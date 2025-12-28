/**
 * @file YAWL Case RDF - RDF serialization and deserialization
 * @module @unrdf/yawl/case-rdf
 */

import { toISO } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { YawlTask, TaskStatus } from './task.mjs';
import { CaseStatus } from './case-core.mjs';
import {
  // Namespaces
  YAWL,
  YAWL_WORK,

  // Classes
  WorkflowCase,
  WorkItem as WorkItemClass,
  Condition,

  // Status values
  Case_Active,
  Case_Completed,
  Case_Suspended,
  Case_Cancelled,
  Case_Failed,
  WorkItem_Enabled,
  WorkItem_Started,
  WorkItem_Completed,
  WorkItem_Cancelled,

  // Properties
  specId,
  status as statusProp,
  createdAt,
  completedAt,
  workItems as workItemsProp,
  caseData,
  taskRef,
  caseRef,

  // RDF terms
  rdfType,

  // URI factories
  caseUri,
  workItemUri,
  specUri,
  caseGraph,
  conditionUri,

  // Literal factories
  dateTimeLiteral,
  stringLiteral,
  integerLiteral,
} from './ontology/yawl-ontology.mjs';

const { quad, namedNode } = dataFactory;

// =============================================================================
// RDF Status Mappings
// =============================================================================

/**
 * Map CaseStatus to RDF terms
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const CASE_STATUS_RDF_MAP = {
  [CaseStatus.CREATED]: Case_Active,
  [CaseStatus.RUNNING]: Case_Active,
  [CaseStatus.COMPLETED]: Case_Completed,
  [CaseStatus.SUSPENDED]: Case_Suspended,
  [CaseStatus.CANCELLED]: Case_Cancelled,
  [CaseStatus.FAILED]: Case_Failed,
};

/**
 * Map WorkItem status to RDF terms
 * @type {Object<string, import('oxigraph').NamedNode>}
 */
const WORKITEM_STATUS_RDF_MAP = {
  [TaskStatus.ENABLED]: WorkItem_Enabled,
  [TaskStatus.ACTIVE]: WorkItem_Started,
  [TaskStatus.COMPLETED]: WorkItem_Completed,
  [TaskStatus.CANCELLED]: WorkItem_Cancelled,
};

// =============================================================================
// RDF Serialization
// =============================================================================

/**
 * Convert a case instance to RDF quads and add to store.
 *
 * Creates YAWL RDF representation including:
 * - Case metadata (id, spec, status, timestamps)
 * - Work items with status and task references
 * - Petri net marking (token counts in conditions)
 *
 * @param {import('./case.mjs').Case} caseInstance - Case to serialize
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store to add quads to
 * @returns {Promise<{quadCount: number, caseUri: import('oxigraph').NamedNode}>}
 *
 * @example
 * import { caseToRDF } from '@unrdf/yawl/case';
 * import { createStore } from '@unrdf/oxigraph';
 *
 * const store = createStore();
 * const { quadCount, caseUri } = await caseToRDF(caseInstance, store);
 * console.log(`Added ${quadCount} quads for case ${caseUri.value}`);
 */
export async function caseToRDF(caseInstance, store) {
  const caseNode = caseUri(caseInstance.id);
  const graph = caseGraph(caseInstance.id);
  let quadCount = 0;

  // Case type
  store.add(quad(caseNode, rdfType, WorkflowCase, graph));
  quadCount++;

  // Specification reference
  store.add(quad(caseNode, specId, specUri(caseInstance.workflowId), graph));
  quadCount++;

  // Status
  const statusNode = CASE_STATUS_RDF_MAP[caseInstance._status] || Case_Active;
  store.add(quad(caseNode, statusProp, statusNode, graph));
  quadCount++;

  // Timestamps
  if (caseInstance.createdAt) {
    store.add(quad(caseNode, createdAt, dateTimeLiteral(toISO(caseInstance.createdAt)), graph));
    quadCount++;
  }

  if (caseInstance.startedAt) {
    store.add(quad(caseNode, namedNode(YAWL + 'startedAt'), dateTimeLiteral(toISO(caseInstance.startedAt)), graph));
    quadCount++;
  }

  if (caseInstance.completedAt) {
    store.add(quad(caseNode, completedAt, dateTimeLiteral(toISO(caseInstance.completedAt)), graph));
    quadCount++;
  }

  // Case data
  if (Object.keys(caseInstance.data).length > 0) {
    store.add(quad(caseNode, caseData, stringLiteral(JSON.stringify(caseInstance.data)), graph));
    quadCount++;
  }

  // Work items
  for (const [wiId, workItem] of caseInstance.workItems) {
    const wiNode = workItemUri(wiId);

    // Work item type
    store.add(quad(wiNode, rdfType, WorkItemClass, graph));
    quadCount++;

    // Case reference
    store.add(quad(wiNode, caseRef, caseNode, graph));
    quadCount++;

    // Task reference
    const taskDefId = caseInstance.getTaskDefIdForWorkItem(wiId);
    store.add(quad(wiNode, taskRef, namedNode(YAWL + 'task-' + taskDefId), graph));
    quadCount++;

    // Status
    const wiStatusNode = WORKITEM_STATUS_RDF_MAP[workItem.status] || WorkItem_Enabled;
    store.add(quad(wiNode, statusProp, wiStatusNode, graph));
    quadCount++;

    // Timestamps
    if (workItem.enabledAt) {
      store.add(quad(wiNode, namedNode(YAWL + 'enabledAt'), dateTimeLiteral(toISO(workItem.enabledAt)), graph));
      quadCount++;
    }

    if (workItem.startedAt) {
      store.add(quad(wiNode, namedNode(YAWL + 'startedAt'), dateTimeLiteral(toISO(workItem.startedAt)), graph));
      quadCount++;
    }

    if (workItem.completedAt) {
      store.add(quad(wiNode, completedAt, dateTimeLiteral(toISO(workItem.completedAt)), graph));
      quadCount++;
    }

    // Link from case
    store.add(quad(caseNode, workItemsProp, wiNode, graph));
    quadCount++;
  }

  // Petri net marking (tokens in conditions)
  for (const [condId, tokenCount] of caseInstance._marking) {
    const condNode = conditionUri(condId);

    store.add(quad(condNode, rdfType, Condition, graph));
    quadCount++;

    store.add(quad(condNode, caseRef, caseNode, graph));
    quadCount++;

    store.add(quad(condNode, namedNode(YAWL + 'tokenCount'), integerLiteral(tokenCount), graph));
    quadCount++;
  }

  return { quadCount, caseUri: caseNode };
}

// =============================================================================
// RDF Deserialization
// =============================================================================

/**
 * Load case state from RDF store.
 *
 * Reconstructs a Case instance from its RDF representation.
 * Requires the workflow definition to be provided separately.
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - RDF store to read from
 * @param {string} caseId - Case identifier to load
 * @param {import('./workflow.mjs').YawlWorkflow} workflow - Workflow definition
 * @returns {Promise<import('./case.mjs').Case|null>} Loaded case or null if not found
 *
 * @example
 * import { caseFromRDF } from '@unrdf/yawl/case';
 *
 * const caseInstance = await caseFromRDF(store, 'case-123', workflow);
 * if (caseInstance) {
 *   console.log(`Loaded case with ${caseInstance.getWorkItems().length} work items`);
 * }
 */
export async function caseFromRDF(store, caseId, workflow) {
  // Import Case class dynamically to avoid circular dependency
  const { Case } = await import('./case.mjs');

  const caseNode = caseUri(caseId);
  const graph = caseGraph(caseId);

  // Check if case exists
  const typeQuads = [...store.match(caseNode, rdfType, WorkflowCase, graph)];
  if (typeQuads.length === 0) {
    return null;
  }

  // Extract case data
  const caseDataQuads = {
    id: caseId,
    workflowId: workflow.id,
    status: CaseStatus.CREATED,
    data: {},
  };

  // Get status
  const statusQuads = [...store.match(caseNode, statusProp, null, graph)];
  if (statusQuads.length > 0) {
    const statusValue = statusQuads[0].object.value;
    // Reverse lookup status
    for (const [status, rdfTerm] of Object.entries(CASE_STATUS_RDF_MAP)) {
      if (rdfTerm.value === statusValue) {
        caseDataQuads.status = status;
        break;
      }
    }
  }

  // Get timestamps
  const createdAtQuads = [...store.match(caseNode, createdAt, null, graph)];
  if (createdAtQuads.length > 0) {
    const dateStr = createdAtQuads[0].object.value;
    caseDataQuads.createdAt = BigInt(new Date(dateStr).getTime() * 1000000);
  }

  const startedAtQuads = [...store.match(caseNode, namedNode(YAWL + 'startedAt'), null, graph)];
  if (startedAtQuads.length > 0) {
    const dateStr = startedAtQuads[0].object.value;
    caseDataQuads.startedAt = BigInt(new Date(dateStr).getTime() * 1000000);
  }

  const completedAtQuads = [...store.match(caseNode, completedAt, null, graph)];
  if (completedAtQuads.length > 0) {
    const dateStr = completedAtQuads[0].object.value;
    caseDataQuads.completedAt = BigInt(new Date(dateStr).getTime() * 1000000);
  }

  // Get case data
  const caseDataLiteralQuads = [...store.match(caseNode, caseData, null, graph)];
  if (caseDataLiteralQuads.length > 0) {
    try {
      caseDataQuads.data = JSON.parse(caseDataLiteralQuads[0].object.value);
    } catch {
      // Invalid JSON, keep empty data
    }
  }

  // Create case instance
  const caseInstance = new Case(caseDataQuads, workflow);

  // Clear auto-initialized marking (will restore from RDF)
  caseInstance._marking.clear();

  // Load work items
  const wiLinkQuads = [...store.match(caseNode, workItemsProp, null, graph)];
  for (const wiLinkQuad of wiLinkQuads) {
    const wiNode = wiLinkQuad.object;
    const wiId = wiNode.value.replace(YAWL_WORK, '');

    // Get task reference
    const taskRefQuads = [...store.match(wiNode, taskRef, null, graph)];
    const taskDefId = taskRefQuads.length > 0
      ? taskRefQuads[0].object.value.replace(YAWL + 'task-', '')
      : wiId;

    // Get status
    const wiStatusQuads = [...store.match(wiNode, statusProp, null, graph)];
    let wiStatus = TaskStatus.INACTIVE;
    if (wiStatusQuads.length > 0) {
      const statusValue = wiStatusQuads[0].object.value;
      for (const [status, rdfTerm] of Object.entries(WORKITEM_STATUS_RDF_MAP)) {
        if (rdfTerm.value === statusValue) {
          wiStatus = status;
          break;
        }
      }
    }

    // Get timestamps
    let enabledAt, startedAt, wiCompletedAt;

    const enabledAtQuads = [...store.match(wiNode, namedNode(YAWL + 'enabledAt'), null, graph)];
    if (enabledAtQuads.length > 0) {
      enabledAt = BigInt(new Date(enabledAtQuads[0].object.value).getTime() * 1000000);
    }

    const wiStartedAtQuads = [...store.match(wiNode, namedNode(YAWL + 'startedAt'), null, graph)];
    if (wiStartedAtQuads.length > 0) {
      startedAt = BigInt(new Date(wiStartedAtQuads[0].object.value).getTime() * 1000000);
    }

    const wiCompletedAtQuads = [...store.match(wiNode, completedAt, null, graph)];
    if (wiCompletedAtQuads.length > 0) {
      wiCompletedAt = BigInt(new Date(wiCompletedAtQuads[0].object.value).getTime() * 1000000);
    }

    // Create task
    const task = new YawlTask({
      id: wiId,
      name: taskDefId,
      caseId: caseId,
      status: wiStatus,
      enabledAt,
      startedAt,
      completedAt: wiCompletedAt,
    });

    caseInstance.workItems.set(wiId, task);

    if (!caseInstance.workItemsByTask.has(taskDefId)) {
      caseInstance.workItemsByTask.set(taskDefId, new Set());
    }
    caseInstance.workItemsByTask.get(taskDefId).add(wiId);

    // Track completed tasks
    if (wiStatus === TaskStatus.COMPLETED) {
      caseInstance.completedTasks.add(taskDefId);
    }
    caseInstance.activatedTasks.add(taskDefId);
  }

  // Load Petri net marking (conditions with tokens)
  const conditionQuads = [...store.match(null, rdfType, Condition, graph)];
  for (const condQuad of conditionQuads) {
    const condNode = condQuad.subject;

    // Check this condition belongs to this case
    const condCaseRefQuads = [...store.match(condNode, caseRef, caseNode, graph)];
    if (condCaseRefQuads.length === 0) continue;

    // Get token count
    const tokenQuads = [...store.match(condNode, namedNode(YAWL + 'tokenCount'), null, graph)];
    if (tokenQuads.length > 0) {
      const tokenCount = parseInt(tokenQuads[0].object.value, 10);
      const condId = condNode.value.replace(YAWL + 'condition-', '');
      caseInstance._marking.set(condId, tokenCount);
    }
  }

  return caseInstance;
}
