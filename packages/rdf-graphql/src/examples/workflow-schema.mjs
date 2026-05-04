/**
 * @file YAWL Workflow GraphQL Example
 * @module @unrdf/rdf-graphql/examples/workflow-schema
 *
 * Demonstrates generating GraphQL schemas from YAWL workflow ontologies
 * and executing type-safe queries against workflow instances.
 */

import { createAdapter } from '../adapter.mjs';

/**
 * YAWL Workflow Ontology (simplified)
 * Defines classes and properties for workflow management
 */
const YAWL_ONTOLOGY = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix yawl: <http://example.org/yawl#> .

# Class Definitions
yawl:Workflow a rdfs:Class ;
  rdfs:label "Workflow" ;
  rdfs:comment "A YAWL workflow specification defining a business process" .

yawl:Case a rdfs:Class ;
  rdfs:label "Case" ;
  rdfs:comment "A workflow instance (case) representing a specific execution" .

yawl:Task a rdfs:Class ;
  rdfs:label "Task" ;
  rdfs:comment "A work item within a workflow" .

yawl:Agent a rdfs:Class ;
  rdfs:label "Agent" ;
  rdfs:comment "An agent that can execute tasks" .

yawl:Receipt a rdfs:Class ;
  rdfs:label "Receipt" ;
  rdfs:comment "A cryptographic proof of task completion" .

# Workflow Properties
yawl:workflowId a rdf:Property ;
  rdfs:domain yawl:Workflow ;
  rdfs:range xsd:string ;
  rdfs:label "Workflow ID" ;
  rdfs:comment "Unique identifier for the workflow" .

yawl:name a rdf:Property ;
  rdfs:domain yawl:Workflow ;
  rdfs:range xsd:string ;
  rdfs:label "Name" ;
  rdfs:comment "Human-readable workflow name" .

yawl:description a rdf:Property ;
  rdfs:domain yawl:Workflow ;
  rdfs:range xsd:string ;
  rdfs:label "Description" ;
  rdfs:comment "Workflow description" .

# Case Properties
yawl:caseId a rdf:Property ;
  rdfs:domain yawl:Case ;
  rdfs:range xsd:string ;
  rdfs:label "Case ID" ;
  rdfs:comment "Unique identifier for the case" .

yawl:workflow a rdf:Property ;
  rdfs:domain yawl:Case ;
  rdfs:range yawl:Workflow ;
  rdfs:label "Workflow" ;
  rdfs:comment "The workflow this case is an instance of" .

yawl:status a rdf:Property ;
  rdfs:domain yawl:Case ;
  rdfs:range xsd:string ;
  rdfs:label "Status" ;
  rdfs:comment "Current execution status" .

yawl:startTime a rdf:Property ;
  rdfs:domain yawl:Case ;
  rdfs:range xsd:dateTime ;
  rdfs:label "Start Time" ;
  rdfs:comment "When the case was initiated" .

# Task Properties
yawl:taskId a rdf:Property ;
  rdfs:domain yawl:Task ;
  rdfs:range xsd:string ;
  rdfs:label "Task ID" ;
  rdfs:comment "Unique identifier for the task" .

yawl:case a rdf:Property ;
  rdfs:domain yawl:Task ;
  rdfs:range yawl:Case ;
  rdfs:label "Case" ;
  rdfs:comment "The case this task belongs to" .

yawl:assignedTo a rdf:Property ;
  rdfs:domain yawl:Task ;
  rdfs:range yawl:Agent ;
  rdfs:label "Assigned To" ;
  rdfs:comment "Agent assigned to execute this task" .

yawl:priority a rdf:Property ;
  rdfs:domain yawl:Task ;
  rdfs:range xsd:integer ;
  rdfs:label "Priority" ;
  rdfs:comment "Task priority (higher = more urgent)" .

# Agent Properties
yawl:agentId a rdf:Property ;
  rdfs:domain yawl:Agent ;
  rdfs:range xsd:string ;
  rdfs:label "Agent ID" ;
  rdfs:comment "Unique identifier for the agent" .

yawl:capabilities a rdf:Property ;
  rdfs:domain yawl:Agent ;
  rdfs:range xsd:string ;
  rdfs:label "Capabilities" ;
  rdfs:comment "Agent capabilities or skills" .

# Receipt Properties
yawl:receiptId a rdf:Property ;
  rdfs:domain yawl:Receipt ;
  rdfs:range xsd:string ;
  rdfs:label "Receipt ID" ;
  rdfs:comment "Unique identifier for the receipt" .

yawl:task a rdf:Property ;
  rdfs:domain yawl:Receipt ;
  rdfs:range yawl:Task ;
  rdfs:label "Task" ;
  rdfs:comment "The task this receipt proves" .

yawl:timestamp a rdf:Property ;
  rdfs:domain yawl:Receipt ;
  rdfs:range xsd:dateTime ;
  rdfs:label "Timestamp" ;
  rdfs:comment "When the receipt was created" .

yawl:proofHash a rdf:Property ;
  rdfs:domain yawl:Receipt ;
  rdfs:range xsd:string ;
  rdfs:label "Proof Hash" ;
  rdfs:comment "Cryptographic hash proving completion" .
`;

/**
 * Sample YAWL Workflow Instance Data
 */
const WORKFLOW_INSTANCES = `
@prefix yawl: <http://example.org/yawl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Workflow Definition
<http://example.org/workflows/order-fulfillment> a yawl:Workflow ;
  yawl:workflowId "wf-001" ;
  yawl:name "Order Fulfillment" ;
  yawl:description "Process customer orders from receipt to delivery" .

# Case Instance
<http://example.org/cases/case-12345> a yawl:Case ;
  yawl:caseId "case-12345" ;
  yawl:workflow <http://example.org/workflows/order-fulfillment> ;
  yawl:status "active" ;
  yawl:startTime "2025-12-25T10:30:00Z"^^xsd:dateTime .

# Tasks
<http://example.org/tasks/task-001> a yawl:Task ;
  yawl:taskId "task-001" ;
  yawl:case <http://example.org/cases/case-12345> ;
  yawl:assignedTo <http://example.org/agents/agent-alice> ;
  yawl:priority 5 .

<http://example.org/tasks/task-002> a yawl:Task ;
  yawl:taskId "task-002" ;
  yawl:case <http://example.org/cases/case-12345> ;
  yawl:assignedTo <http://example.org/agents/agent-bob> ;
  yawl:priority 3 .

# Agents
<http://example.org/agents/agent-alice> a yawl:Agent ;
  yawl:agentId "agent-alice" ;
  yawl:capabilities "order-processing" .

<http://example.org/agents/agent-bob> a yawl:Agent ;
  yawl:agentId "agent-bob" ;
  yawl:capabilities "quality-assurance" .

# Receipts
<http://example.org/receipts/receipt-001> a yawl:Receipt ;
  yawl:receiptId "receipt-001" ;
  yawl:task <http://example.org/tasks/task-001> ;
  yawl:timestamp "2025-12-25T11:00:00Z"^^xsd:dateTime ;
  yawl:proofHash "sha256:abc123def456" .
`;

/**
 * Example GraphQL queries for YAWL workflows
 */
export const EXAMPLE_QUERIES = {
  // Query 1: Get a specific workflow
  getWorkflow: `
    query GetWorkflow($id: ID!) {
      workflow(id: $id) {
        id
        workflowId
        name
        description
      }
    }
  `,

  // Query 2: List all cases
  listCases: `
    query ListCases($limit: Int, $offset: Int) {
      cases(limit: $limit, offset: $offset) {
        id
        caseId
        status
        startTime
      }
    }
  `,

  // Query 3: Get a specific task with details
  getTask: `
    query GetTask($id: ID!) {
      task(id: $id) {
        id
        taskId
        priority
      }
    }
  `,

  // Query 4: List all agents
  listAgents: `
    query ListAgents {
      agents {
        id
        agentId
        capabilities
      }
    }
  `,

  // Query 5: Get receipts
  listReceipts: `
    query ListReceipts {
      receipts {
        id
        receiptId
        timestamp
        proofHash
      }
    }
  `,
};

/**
 * Create and configure YAWL workflow GraphQL adapter
 * @returns {Promise<{adapter: RDFGraphQLAdapter, schema: GraphQLSchema}>}
 */
export async function createWorkflowAdapter() {
  const adapter = createAdapter({
    namespaces: {
      yawl: 'http://example.org/yawl#',
      workflows: 'http://example.org/workflows/',
      cases: 'http://example.org/cases/',
      tasks: 'http://example.org/tasks/',
      agents: 'http://example.org/agents/',
      receipts: 'http://example.org/receipts/',
    },
    enableCache: true,
    typeMapping: {
      Workflow: 'http://example.org/yawl#Workflow',
      Case: 'http://example.org/yawl#Case',
      Task: 'http://example.org/yawl#Task',
      Agent: 'http://example.org/yawl#Agent',
      Receipt: 'http://example.org/yawl#Receipt',
    },
  });

  // Load ontology and data
  await adapter.loadOntology(YAWL_ONTOLOGY);
  await adapter.loadData(WORKFLOW_INSTANCES);

  // Generate schema
  const schema = adapter.generateSchema();

  return { adapter, schema };
}

/**
 * Run example queries and display results
 * @returns {Promise<void>}
 */
export async function runExamples() {
  console.log('=== YAWL Workflow GraphQL Example ===\n');

  const { adapter, schema } = await createWorkflowAdapter();

  // Example 1: Get specific workflow
  console.log('1. Get Workflow:');
  const result1 = await adapter.executeQuery(
    EXAMPLE_QUERIES.getWorkflow,
    { id: 'http://example.org/workflows/order-fulfillment' }
  );
  console.log(JSON.stringify(result1, null, 2));
  console.log('');

  // Example 2: List cases
  console.log('2. List Cases:');
  const result2 = await adapter.executeQuery(
    EXAMPLE_QUERIES.listCases,
    { limit: 10, offset: 0 }
  );
  console.log(JSON.stringify(result2, null, 2));
  console.log('');

  // Example 3: List agents
  console.log('3. List Agents:');
  const result3 = await adapter.executeQuery(EXAMPLE_QUERIES.listAgents);
  console.log(JSON.stringify(result3, null, 2));
  console.log('');

  // Example 4: Get statistics
  console.log('4. Store Statistics:');
  const stats = await adapter.getStatistics();
  console.log(JSON.stringify(stats, null, 2));
  console.log('');

  // Example 5: Cache stats
  console.log('5. Cache Statistics:');
  const cacheStats = adapter.getCacheStats();
  console.log(JSON.stringify(cacheStats, null, 2));
  console.log('');

  return { adapter, schema };
}

// Export ontology and data for testing
export { YAWL_ONTOLOGY, WORKFLOW_INSTANCES };
