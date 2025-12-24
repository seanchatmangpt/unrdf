/**
 * @file YAWL RDF Ontology - Namespace definitions, classes, and properties
 * @module @unrdf/yawl/ontology
 *
 * YAWL (Yet Another Workflow Language) RDF representation layer.
 * Maps YAWL workflow concepts to RDF following semantic web best practices.
 *
 * @example
 * // Import namespaces and terms
 * import { YAWL, YAWL_CASE, YAWL_TASK, YAWL_WORK, rdfType } from '@unrdf/yawl/ontology';
 *
 * // Create a workflow case quad
 * const caseQuad = quad(
 *   caseUri('case-123'),
 *   rdfType,
 *   WorkflowCase,
 *   caseGraph('case-123')
 * );
 */

import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal } = dataFactory;

// =============================================================================
// NAMESPACE DEFINITIONS
// =============================================================================

/**
 * YAWL Core namespace - base workflow concepts
 * @type {string}
 */
export const YAWL = 'http://unrdf.org/yawl#';

/**
 * YAWL Case namespace - workflow case instances
 * @type {string}
 */
export const YAWL_CASE = 'http://unrdf.org/yawl/case#';

/**
 * YAWL Task namespace - task definitions
 * @type {string}
 */
export const YAWL_TASK = 'http://unrdf.org/yawl/task#';

/**
 * YAWL Work Item namespace - work item instances
 * @type {string}
 */
export const YAWL_WORK = 'http://unrdf.org/yawl/workitem#';

/**
 * Common RDF vocabulary namespaces
 */
export const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
export const RDFS = 'http://www.w3.org/2000/01/rdf-schema#';
export const XSD = 'http://www.w3.org/2001/XMLSchema#';
export const FOAF = 'http://xmlns.com/foaf/0.1/';

/**
 * SPARQL prefix declarations for YAWL queries
 * @type {string}
 */
export const SPARQL_PREFIXES = `
PREFIX yawl: <${YAWL}>
PREFIX yawl-case: <${YAWL_CASE}>
PREFIX yawl-task: <${YAWL_TASK}>
PREFIX yawl-work: <${YAWL_WORK}>
PREFIX rdf: <${RDF}>
PREFIX rdfs: <${RDFS}>
PREFIX xsd: <${XSD}>
PREFIX foaf: <${FOAF}>
`;

/**
 * Prefix map for serialization
 * @type {Object<string, string>}
 */
export const YAWL_PREFIXES = {
  yawl: YAWL,
  'yawl-case': YAWL_CASE,
  'yawl-task': YAWL_TASK,
  'yawl-work': YAWL_WORK,
  rdf: RDF,
  rdfs: RDFS,
  xsd: XSD,
  foaf: FOAF,
};

// =============================================================================
// RDF VOCABULARY TERMS
// =============================================================================

/**
 * rdf:type predicate
 */
export const rdfType = namedNode(RDF + 'type');

/**
 * rdfs:label predicate
 */
export const rdfsLabel = namedNode(RDFS + 'label');

/**
 * rdfs:comment predicate
 */
export const rdfsComment = namedNode(RDFS + 'comment');

// =============================================================================
// YAWL CLASSES
// =============================================================================

/**
 * WorkflowCase class - represents a running workflow instance
 * @example
 * <yawl-case:case-123> rdf:type yawl:WorkflowCase .
 */
export const WorkflowCase = namedNode(YAWL + 'WorkflowCase');

/**
 * WorkflowSpec class - represents a workflow specification/template
 */
export const WorkflowSpec = namedNode(YAWL + 'WorkflowSpec');

/**
 * Task class - base class for all task types
 */
export const Task = namedNode(YAWL + 'Task');

/**
 * WorkItem class - represents a work item instance
 */
export const WorkItem = namedNode(YAWL + 'WorkItem');

/**
 * Condition class - represents a workflow condition/place
 */
export const Condition = namedNode(YAWL + 'Condition');

/**
 * Flow class - represents a flow between tasks/conditions
 */
export const Flow = namedNode(YAWL + 'Flow');

// =============================================================================
// TASK KIND CLASSES (YAWL Task Types)
// =============================================================================

/**
 * AtomicTask - indivisible unit of work
 */
export const AtomicTask = namedNode(YAWL + 'AtomicTask');

/**
 * CompositeTask - task that expands to a sub-net
 */
export const CompositeTask = namedNode(YAWL + 'CompositeTask');

/**
 * MultipleInstanceTask - task that can have multiple concurrent instances
 */
export const MultipleInstanceTask = namedNode(YAWL + 'MultipleInstanceTask');

/**
 * AutomatedTask - task executed without human intervention
 */
export const AutomatedTask = namedNode(YAWL + 'AutomatedTask');

/**
 * ManualTask - task requiring human action
 */
export const ManualTask = namedNode(YAWL + 'ManualTask');

// =============================================================================
// CASE STATUS VALUES
// =============================================================================

/**
 * Case status: Active - case is currently executing
 */
export const Case_Active = namedNode(YAWL + 'Case_Active');

/**
 * Case status: Completed - case has finished successfully
 */
export const Case_Completed = namedNode(YAWL + 'Case_Completed');

/**
 * Case status: Suspended - case is temporarily paused
 */
export const Case_Suspended = namedNode(YAWL + 'Case_Suspended');

/**
 * Case status: Cancelled - case was terminated before completion
 */
export const Case_Cancelled = namedNode(YAWL + 'Case_Cancelled');

/**
 * Case status: Failed - case terminated due to error
 */
export const Case_Failed = namedNode(YAWL + 'Case_Failed');

// =============================================================================
// WORK ITEM STATUS VALUES
// =============================================================================

/**
 * WorkItem status: Enabled - work item is ready to be started
 */
export const WorkItem_Enabled = namedNode(YAWL + 'WorkItem_Enabled');

/**
 * WorkItem status: Fired - work item has been triggered but not yet allocated
 */
export const WorkItem_Fired = namedNode(YAWL + 'WorkItem_Fired');

/**
 * WorkItem status: Allocated - work item assigned to a resource
 */
export const WorkItem_Allocated = namedNode(YAWL + 'WorkItem_Allocated');

/**
 * WorkItem status: Started - work item is currently being executed
 */
export const WorkItem_Started = namedNode(YAWL + 'WorkItem_Started');

/**
 * WorkItem status: Completed - work item has finished successfully
 */
export const WorkItem_Completed = namedNode(YAWL + 'WorkItem_Completed');

/**
 * WorkItem status: Suspended - work item is temporarily paused
 */
export const WorkItem_Suspended = namedNode(YAWL + 'WorkItem_Suspended');

/**
 * WorkItem status: Cancelled - work item was terminated
 */
export const WorkItem_Cancelled = namedNode(YAWL + 'WorkItem_Cancelled');

/**
 * WorkItem status: Failed - work item terminated due to error
 */
export const WorkItem_Failed = namedNode(YAWL + 'WorkItem_Failed');

// =============================================================================
// SPLIT/JOIN BEHAVIOR VALUES
// =============================================================================

/**
 * XOR split - exactly one outgoing flow activated
 */
export const XOR_Split = namedNode(YAWL + 'XOR_Split');

/**
 * AND split - all outgoing flows activated in parallel
 */
export const AND_Split = namedNode(YAWL + 'AND_Split');

/**
 * OR split - one or more outgoing flows activated
 */
export const OR_Split = namedNode(YAWL + 'OR_Split');

/**
 * XOR join - waits for exactly one incoming flow
 */
export const XOR_Join = namedNode(YAWL + 'XOR_Join');

/**
 * AND join - waits for all incoming flows
 */
export const AND_Join = namedNode(YAWL + 'AND_Join');

/**
 * OR join - waits for active incoming flows (cancellation regions)
 */
export const OR_Join = namedNode(YAWL + 'OR_Join');

// =============================================================================
// YAWL PROPERTIES - Case Properties
// =============================================================================

/**
 * specId - links case to its workflow specification
 * Domain: WorkflowCase, Range: WorkflowSpec
 */
export const specId = namedNode(YAWL + 'specId');

/**
 * status - current status of a case or work item
 * Domain: WorkflowCase | WorkItem, Range: Status value
 */
export const status = namedNode(YAWL + 'status');

/**
 * createdAt - timestamp when entity was created
 * Domain: WorkflowCase | WorkItem, Range: xsd:dateTime
 */
export const createdAt = namedNode(YAWL + 'createdAt');

/**
 * updatedAt - timestamp when entity was last updated
 * Domain: WorkflowCase | WorkItem, Range: xsd:dateTime
 */
export const updatedAt = namedNode(YAWL + 'updatedAt');

/**
 * completedAt - timestamp when entity completed
 * Domain: WorkflowCase | WorkItem, Range: xsd:dateTime
 */
export const completedAt = namedNode(YAWL + 'completedAt');

/**
 * workItems - links case to its work items
 * Domain: WorkflowCase, Range: WorkItem
 */
export const workItems = namedNode(YAWL + 'workItems');

/**
 * caseData - links to case data/variables
 * Domain: WorkflowCase, Range: Literal or Resource
 */
export const caseData = namedNode(YAWL + 'caseData');

/**
 * parentCase - links child case to parent (for composite tasks)
 * Domain: WorkflowCase, Range: WorkflowCase
 */
export const parentCase = namedNode(YAWL + 'parentCase');

// =============================================================================
// YAWL PROPERTIES - Task Properties
// =============================================================================

/**
 * taskName - human-readable name of the task
 * Domain: Task, Range: xsd:string
 */
export const taskName = namedNode(YAWL + 'taskName');

/**
 * taskId - unique identifier within specification
 * Domain: Task, Range: xsd:string
 */
export const taskId = namedNode(YAWL + 'taskId');

/**
 * kind - type of task (AtomicTask, CompositeTask, etc.)
 * Domain: Task, Range: Task type class
 */
export const kind = namedNode(YAWL + 'kind');

/**
 * joinsTo - outgoing flow to another task
 * Domain: Task, Range: Task
 */
export const joinsTo = namedNode(YAWL + 'joinsTo');

/**
 * joinsFrom - incoming flow from another task
 * Domain: Task, Range: Task
 */
export const joinsFrom = namedNode(YAWL + 'joinsFrom');

/**
 * splitBehavior - how outgoing flows are activated
 * Domain: Task, Range: Split behavior value
 */
export const splitBehavior = namedNode(YAWL + 'splitBehavior');

/**
 * joinBehavior - how incoming flows are synchronized
 * Domain: Task, Range: Join behavior value
 */
export const joinBehavior = namedNode(YAWL + 'joinBehavior');

/**
 * decomposes - links composite task to its sub-net specification
 * Domain: CompositeTask, Range: WorkflowSpec
 */
export const decomposes = namedNode(YAWL + 'decomposes');

/**
 * inputCondition - the starting condition of a net
 * Domain: WorkflowSpec, Range: Condition
 */
export const inputCondition = namedNode(YAWL + 'inputCondition');

/**
 * outputCondition - the ending condition of a net
 * Domain: WorkflowSpec, Range: Condition
 */
export const outputCondition = namedNode(YAWL + 'outputCondition');

/**
 * hasTasks - links spec to its tasks
 * Domain: WorkflowSpec, Range: Task
 */
export const hasTasks = namedNode(YAWL + 'hasTasks');

// =============================================================================
// YAWL PROPERTIES - Work Item Properties
// =============================================================================

/**
 * taskRef - links work item to its task definition
 * Domain: WorkItem, Range: Task
 */
export const taskRef = namedNode(YAWL + 'taskRef');

/**
 * caseRef - links work item to its case
 * Domain: WorkItem, Range: WorkflowCase
 */
export const caseRef = namedNode(YAWL + 'caseRef');

/**
 * owner - the resource assigned to this work item
 * Domain: WorkItem, Range: foaf:Person or resource
 */
export const owner = namedNode(YAWL + 'owner');

/**
 * startedBy - the resource who started this work item
 * Domain: WorkItem, Range: foaf:Person or resource
 */
export const startedBy = namedNode(YAWL + 'startedBy');

/**
 * completedBy - the resource who completed this work item
 * Domain: WorkItem, Range: foaf:Person or resource
 */
export const completedBy = namedNode(YAWL + 'completedBy');

/**
 * workItemData - data associated with work item
 * Domain: WorkItem, Range: Literal or Resource
 */
export const workItemData = namedNode(YAWL + 'workItemData');

/**
 * timerExpiry - when a timer task expires
 * Domain: WorkItem, Range: xsd:dateTime
 */
export const timerExpiry = namedNode(YAWL + 'timerExpiry');

// =============================================================================
// YAWL PROPERTIES - Flow/Routing Properties
// =============================================================================

/**
 * flowCondition - SPARQL ASK condition for flow activation
 * Domain: Flow, Range: xsd:string (SPARQL)
 */
export const flowCondition = namedNode(YAWL + 'flowCondition');

/**
 * flowPriority - priority for XOR split evaluation order
 * Domain: Flow, Range: xsd:integer
 */
export const flowPriority = namedNode(YAWL + 'flowPriority');

/**
 * isDefaultFlow - marks default flow for XOR split
 * Domain: Flow, Range: xsd:boolean
 */
export const isDefaultFlow = namedNode(YAWL + 'isDefaultFlow');

/**
 * sourceTask - source of a flow
 * Domain: Flow, Range: Task | Condition
 */
export const sourceTask = namedNode(YAWL + 'sourceTask');

/**
 * targetTask - target of a flow
 * Domain: Flow, Range: Task | Condition
 */
export const targetTask = namedNode(YAWL + 'targetTask');

/**
 * cancellationSet - tasks cancelled when this task completes
 * Domain: Task, Range: Task
 */
export const cancellationSet = namedNode(YAWL + 'cancellationSet');

// =============================================================================
// URI FACTORY FUNCTIONS
// =============================================================================

/**
 * Create a case URI from case ID
 * @param {string} caseId - The case identifier
 * @returns {import('oxigraph').NamedNode} The case URI as a named node
 * @example
 * caseUri('case-123') // => namedNode('http://unrdf.org/yawl/case#case-123')
 */
export const caseUri = caseId => namedNode(YAWL_CASE + caseId);

/**
 * Create a task URI from task ID
 * @param {string} taskId - The task identifier
 * @returns {import('oxigraph').NamedNode} The task URI as a named node
 */
export const taskUri = taskId => namedNode(YAWL_TASK + taskId);

/**
 * Create a work item URI from work item ID
 * @param {string} workItemId - The work item identifier
 * @returns {import('oxigraph').NamedNode} The work item URI as a named node
 */
export const workItemUri = workItemId => namedNode(YAWL_WORK + workItemId);

/**
 * Create a workflow spec URI from spec ID
 * @param {string} specId - The specification identifier
 * @returns {import('oxigraph').NamedNode} The spec URI as a named node
 */
export const specUri = specId => namedNode(YAWL + 'spec-' + specId);

/**
 * Create a named graph URI for a case (for graph isolation)
 * @param {string} caseId - The case identifier
 * @returns {import('oxigraph').NamedNode} The graph URI as a named node
 */
export const caseGraph = caseId => namedNode(YAWL_CASE + caseId + '/graph');

/**
 * Create a flow URI from source and target
 * @param {string} sourceId - Source task/condition ID
 * @param {string} targetId - Target task/condition ID
 * @returns {import('oxigraph').NamedNode} The flow URI as a named node
 */
export const flowUri = (sourceId, targetId) =>
  namedNode(YAWL + 'flow-' + sourceId + '-to-' + targetId);

/**
 * Create a condition URI
 * @param {string} conditionId - The condition identifier
 * @returns {import('oxigraph').NamedNode} The condition URI as a named node
 */
export const conditionUri = conditionId => namedNode(YAWL + 'condition-' + conditionId);

// =============================================================================
// LITERAL FACTORY FUNCTIONS
// =============================================================================

/**
 * Create an xsd:dateTime literal
 * @param {Date|string} date - The date value
 * @returns {import('oxigraph').Literal} The dateTime literal
 */
export const dateTimeLiteral = date => {
  const isoString = date instanceof Date ? date.toISOString() : date;
  return literal(isoString, namedNode(XSD + 'dateTime'));
};

/**
 * Create an xsd:integer literal
 * @param {number} value - The integer value
 * @returns {import('oxigraph').Literal} The integer literal
 */
export const integerLiteral = value =>
  literal(String(value), namedNode(XSD + 'integer'));

/**
 * Create an xsd:boolean literal
 * @param {boolean} value - The boolean value
 * @returns {import('oxigraph').Literal} The boolean literal
 */
export const booleanLiteral = value =>
  literal(String(value), namedNode(XSD + 'boolean'));

/**
 * Create a plain string literal
 * @param {string} value - The string value
 * @returns {import('oxigraph').Literal} The string literal
 */
export const stringLiteral = value => literal(value);

// =============================================================================
// SPARQL QUERY TEMPLATES
// =============================================================================

/**
 * SPARQL query templates for common YAWL operations
 */
export const SPARQL_QUERIES = {
  /**
   * Get all work items for a case
   */
  GET_CASE_WORK_ITEMS: `
${SPARQL_PREFIXES}
SELECT ?workItem ?task ?status ?owner ?createdAt
WHERE {
  ?workItem rdf:type yawl:WorkItem ;
            yawl:caseRef ?case ;
            yawl:taskRef ?task ;
            yawl:status ?status .
  OPTIONAL { ?workItem yawl:owner ?owner }
  OPTIONAL { ?workItem yawl:createdAt ?createdAt }
  FILTER (?case = <CASE_URI>)
}
ORDER BY ?createdAt
`,

  /**
   * Get enabled work items (ready for execution)
   */
  GET_ENABLED_WORK_ITEMS: `
${SPARQL_PREFIXES}
SELECT ?workItem ?task ?taskName
WHERE {
  ?workItem rdf:type yawl:WorkItem ;
            yawl:caseRef ?case ;
            yawl:taskRef ?task ;
            yawl:status yawl:WorkItem_Enabled .
  ?task yawl:taskName ?taskName .
  FILTER (?case = <CASE_URI>)
}
`,

  /**
   * Get case by ID with all properties
   */
  GET_CASE: `
${SPARQL_PREFIXES}
SELECT ?case ?spec ?status ?createdAt ?updatedAt
WHERE {
  ?case rdf:type yawl:WorkflowCase ;
        yawl:specId ?spec ;
        yawl:status ?status .
  OPTIONAL { ?case yawl:createdAt ?createdAt }
  OPTIONAL { ?case yawl:updatedAt ?updatedAt }
  FILTER (?case = <CASE_URI>)
}
`,

  /**
   * Get tasks that can be enabled next (based on completed work items)
   */
  GET_NEXT_ELIGIBLE_TASKS: `
${SPARQL_PREFIXES}
SELECT DISTINCT ?nextTask ?taskName ?joinBehavior
WHERE {
  # Find completed work items in this case
  ?completedWorkItem rdf:type yawl:WorkItem ;
                     yawl:caseRef ?case ;
                     yawl:taskRef ?completedTask ;
                     yawl:status yawl:WorkItem_Completed .

  # Find tasks that follow completed tasks
  ?completedTask yawl:joinsTo ?nextTask .
  ?nextTask yawl:taskName ?taskName .
  OPTIONAL { ?nextTask yawl:joinBehavior ?joinBehavior }

  # Exclude tasks that already have enabled/started work items
  FILTER NOT EXISTS {
    ?existingItem yawl:caseRef ?case ;
                  yawl:taskRef ?nextTask ;
                  yawl:status ?existingStatus .
    FILTER (?existingStatus IN (yawl:WorkItem_Enabled, yawl:WorkItem_Started, yawl:WorkItem_Allocated))
  }

  FILTER (?case = <CASE_URI>)
}
`,

  /**
   * Count active cases by specification
   */
  COUNT_ACTIVE_CASES_BY_SPEC: `
${SPARQL_PREFIXES}
SELECT ?spec (COUNT(?case) AS ?activeCount)
WHERE {
  ?case rdf:type yawl:WorkflowCase ;
        yawl:specId ?spec ;
        yawl:status yawl:Case_Active .
}
GROUP BY ?spec
`,

  /**
   * Get workflow specification structure
   */
  GET_WORKFLOW_STRUCTURE: `
${SPARQL_PREFIXES}
SELECT ?task ?taskName ?kind ?splitBehavior ?joinBehavior ?nextTask
WHERE {
  ?spec yawl:hasTasks ?task .
  ?task yawl:taskName ?taskName ;
        yawl:kind ?kind .
  OPTIONAL { ?task yawl:splitBehavior ?splitBehavior }
  OPTIONAL { ?task yawl:joinBehavior ?joinBehavior }
  OPTIONAL { ?task yawl:joinsTo ?nextTask }
  FILTER (?spec = <SPEC_URI>)
}
ORDER BY ?taskName
`,

  /**
   * Check AND-join satisfaction (all predecessors completed)
   */
  CHECK_AND_JOIN_SATISFIED: `
${SPARQL_PREFIXES}
ASK {
  # Get the task with AND-join
  ?task yawl:joinBehavior yawl:AND_Join .

  # Ensure all predecessor tasks have completed work items
  FILTER NOT EXISTS {
    ?predecessorTask yawl:joinsTo ?task .
    FILTER NOT EXISTS {
      ?workItem yawl:caseRef ?case ;
                yawl:taskRef ?predecessorTask ;
                yawl:status yawl:WorkItem_Completed .
    }
  }

  FILTER (?task = <TASK_URI> && ?case = <CASE_URI>)
}
`,

  /**
   * Evaluate XOR-split condition (returns flows ordered by priority)
   */
  GET_XOR_SPLIT_FLOWS: `
${SPARQL_PREFIXES}
SELECT ?flow ?targetTask ?condition ?priority ?isDefault
WHERE {
  ?flow yawl:sourceTask ?task ;
        yawl:targetTask ?targetTask .
  OPTIONAL { ?flow yawl:flowCondition ?condition }
  OPTIONAL { ?flow yawl:flowPriority ?priority }
  OPTIONAL { ?flow yawl:isDefaultFlow ?isDefault }
  FILTER (?task = <TASK_URI>)
}
ORDER BY DESC(?priority) ?isDefault
`,
};

/**
 * Replace placeholders in SPARQL query
 * @param {string} query - SPARQL query template
 * @param {Object<string, string>} bindings - URI bindings
 * @returns {string} Query with replaced placeholders
 */
export const bindQuery = (query, bindings) => {
  let result = query;
  for (const [key, value] of Object.entries(bindings)) {
    result = result.replace(new RegExp(`<${key}>`, 'g'), `<${value}>`);
  }
  return result;
};

// =============================================================================
// ONTOLOGY METADATA
// =============================================================================

/**
 * YAWL Ontology version
 * @type {string}
 */
export const ONTOLOGY_VERSION = '1.0.0';

/**
 * YAWL Ontology description
 * @type {string}
 */
export const ONTOLOGY_DESCRIPTION =
  'YAWL (Yet Another Workflow Language) RDF Ontology for workflow management';

/**
 * Export all namespace URIs for external use
 */
export const NAMESPACES = {
  YAWL,
  YAWL_CASE,
  YAWL_TASK,
  YAWL_WORK,
  RDF,
  RDFS,
  XSD,
  FOAF,
};

export default {
  // Namespaces
  YAWL,
  YAWL_CASE,
  YAWL_TASK,
  YAWL_WORK,
  NAMESPACES,
  YAWL_PREFIXES,
  SPARQL_PREFIXES,

  // Classes
  WorkflowCase,
  WorkflowSpec,
  Task,
  WorkItem,
  Condition,
  Flow,
  AtomicTask,
  CompositeTask,
  MultipleInstanceTask,
  AutomatedTask,
  ManualTask,

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

  // Split/Join behaviors
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
  caseData,
  parentCase,
  taskName,
  taskId,
  kind,
  joinsTo,
  joinsFrom,
  splitBehavior,
  joinBehavior,
  decomposes,
  inputCondition,
  outputCondition,
  hasTasks,
  taskRef,
  caseRef,
  owner,
  startedBy,
  completedBy,
  workItemData,
  timerExpiry,
  flowCondition,
  flowPriority,
  isDefaultFlow,
  sourceTask,
  targetTask,
  cancellationSet,

  // RDF terms
  rdfType,
  rdfsLabel,
  rdfsComment,

  // URI factories
  caseUri,
  taskUri,
  workItemUri,
  specUri,
  caseGraph,
  flowUri,
  conditionUri,

  // Literal factories
  dateTimeLiteral,
  integerLiteral,
  booleanLiteral,
  stringLiteral,

  // Query utilities
  SPARQL_QUERIES,
  bindQuery,

  // Metadata
  ONTOLOGY_VERSION,
  ONTOLOGY_DESCRIPTION,
};
