/**
 * @file GraphQL Schema
 * @description GraphQL schema definition for workflows
 * @module graphql-gateway/schema
 */

/**
 * GraphQL Schema Definition
 */
export const schema = `
  type Workflow {
    id: ID!
    name: String!
    status: WorkflowStatus!
    tasks: [Task!]!
    createdAt: String!
    completedAt: String
  }

  type Task {
    id: ID!
    name: String!
    status: TaskStatus!
    input: JSON
    output: JSON
  }

  enum WorkflowStatus {
    PENDING
    RUNNING
    COMPLETED
    FAILED
  }

  enum TaskStatus {
    ENABLED
    RUNNING
    COMPLETED
    FAILED
  }

  type Query {
    workflows: [Workflow!]!
    workflow(id: ID!): Workflow
    task(workflowId: ID!, taskId: ID!): Task
  }

  type Mutation {
    createWorkflow(name: String!, definition: JSON!): Workflow!
    executeTask(workflowId: ID!, taskId: ID!, input: JSON): Task!
    cancelWorkflow(id: ID!): Workflow!
  }

  type Subscription {
    workflowUpdated(id: ID!): Workflow!
    taskCompleted(workflowId: ID!): Task!
  }

  scalar JSON
`;

export default schema;
