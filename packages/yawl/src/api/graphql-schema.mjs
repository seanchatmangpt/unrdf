/**
 * GraphQL Schema for YAWL Workflows
 * Type definitions for workflow operations
 *
 * @module @unrdf/yawl/api/graphql-schema
 */

/**
 * Complete GraphQL schema for YAWL workflows
 * @constant {string}
 */
export const YAWL_GRAPHQL_SCHEMA = `
  # Workflow specification type
  type WorkflowSpec {
    id: ID!
    name: String!
    version: String
    description: String
    tasks: [Task!]!
    flows: [Flow!]!
    createdAt: String!
    updatedAt: String
  }

  # Task definition
  type Task {
    id: ID!
    name: String!
    type: TaskType!
    joinType: JoinType
    splitType: SplitType
    resources: [String!]
    timeout: Int
    properties: JSON
  }

  # Control flow connection
  type Flow {
    from: ID!
    to: ID!
    condition: String
    probability: Float
  }

  # Workflow case (instance)
  type Case {
    id: ID!
    specId: ID!
    status: CaseStatus!
    data: JSON
    startedAt: String!
    completedAt: String
    tasks: [TaskInstance!]!
  }

  # Task instance in a case
  type TaskInstance {
    id: ID!
    taskId: ID!
    caseId: ID!
    status: TaskStatus!
    enabledAt: String
    startedAt: String
    completedAt: String
    result: JSON
    workItems: [WorkItem!]!
  }

  # Work item for task execution
  type WorkItem {
    id: ID!
    taskInstanceId: ID!
    assignedTo: String
    status: WorkItemStatus!
    createdAt: String!
    startedAt: String
    completedAt: String
  }

  # Workflow receipt
  type Receipt {
    hash: String!
    beforeHash: String!
    afterHash: String!
    eventType: String!
    timestamp: String!
    payload: JSON!
    justification: Justification
  }

  # Receipt justification
  type Justification {
    hookValidated: String
    conditionChecked: String
    sparqlQuery: String
    reasoning: String
  }

  # Enumerations
  enum TaskType {
    ATOMIC
    COMPOSITE
    MULTIPLE_INSTANCE
  }

  enum JoinType {
    XOR
    AND
    OR
  }

  enum SplitType {
    XOR
    AND
    OR
  }

  enum CaseStatus {
    RUNNING
    COMPLETED
    CANCELLED
    SUSPENDED
  }

  enum TaskStatus {
    ENABLED
    STARTED
    COMPLETED
    CANCELLED
    FAILED
  }

  enum WorkItemStatus {
    CREATED
    ALLOCATED
    STARTED
    COMPLETED
    CANCELLED
  }

  # Custom scalar for JSON
  scalar JSON

  # Query operations
  type Query {
    workflow(id: ID!): WorkflowSpec
    workflows(limit: Int, offset: Int): [WorkflowSpec!]!
    case(id: ID!): Case
    cases(specId: ID!, status: CaseStatus, limit: Int, offset: Int): [Case!]!
    taskInstance(id: ID!): TaskInstance
    workItem(id: ID!): WorkItem
    receipts(caseId: ID!, limit: Int, offset: Int): [Receipt!]!
    searchWorkflows(query: String!, limit: Int): [WorkflowSpec!]!
    workflowStats(specId: ID!): WorkflowStats!
  }

  # Mutation operations
  type Mutation {
    createWorkflow(input: CreateWorkflowInput!): WorkflowSpec!
    updateWorkflow(id: ID!, input: UpdateWorkflowInput!): WorkflowSpec!
    deleteWorkflow(id: ID!): Boolean!
    startCase(specId: ID!, data: JSON): Case!
    completeTask(taskInstanceId: ID!, result: JSON): TaskInstance!
    cancelTask(taskInstanceId: ID!, reason: String): TaskInstance!
    cancelCase(caseId: ID!, reason: String): Case!
    allocateWorkItem(workItemId: ID!, resourceId: String!): WorkItem!
    startWorkItem(workItemId: ID!): WorkItem!
    completeWorkItem(workItemId: ID!, result: JSON): WorkItem!
  }

  # Subscription operations
  type Subscription {
    caseEvents(caseId: ID!): CaseEvent!
    workflowEvents(specId: ID!): WorkflowEvent!
    allEvents: Event!
  }

  # Event types
  type CaseEvent {
    type: String!
    caseId: ID!
    timestamp: String!
    data: JSON
  }

  type WorkflowEvent {
    type: String!
    specId: ID!
    timestamp: String!
    data: JSON
  }

  type Event {
    type: String!
    timestamp: String!
    data: JSON
  }

  # Statistics
  type WorkflowStats {
    totalCases: Int!
    runningCases: Int!
    completedCases: Int!
    averageCompletionTime: Float
    taskStats: [TaskStats!]!
  }

  type TaskStats {
    taskId: ID!
    taskName: String!
    executionCount: Int!
    averageDuration: Float
    successRate: Float
  }

  # Input types
  input CreateWorkflowInput {
    name: String!
    version: String
    description: String
    tasks: [TaskInput!]!
    flows: [FlowInput!]!
  }

  input UpdateWorkflowInput {
    name: String
    version: String
    description: String
    tasks: [TaskInput!]
    flows: [FlowInput!]
  }

  input TaskInput {
    id: ID!
    name: String!
    type: TaskType!
    joinType: JoinType
    splitType: SplitType
    resources: [String!]
    timeout: Int
    properties: JSON
  }

  input FlowInput {
    from: ID!
    to: ID!
    condition: String
    probability: Float
  }
`;
