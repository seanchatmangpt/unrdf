/**
 * GraphQL API for YAWL Workflows Example
 * Demonstrates GraphQL queries and mutations for workflow management
 */

import { createYAWLGraphQLAPI } from '../src/api/graphql-api.mjs';
import { createYawlEngine } from '../src/engine.mjs';

/**
 * Example 1: Basic query execution
 */
async function basicQueryExample() {
  console.log('\n=== Basic GraphQL Query ===\n');

  // Create YAWL engine
  const engine = await createYawlEngine({
    storeUrl: 'memory://',
    enableEvents: true
  });

  // Create GraphQL API
  const api = createYAWLGraphQLAPI({
    engine,
    playground: true,
    introspection: true
  });

  // Execute query
  const query = `
    query {
      workflows {
        id
        name
        version
        tasks {
          id
          name
          type
        }
      }
    }
  `;

  const result = await api.execute(query);

  console.log('Query Result:');
  console.log(JSON.stringify(result, null, 2));

  if (result.errors) {
    console.error('Errors:', result.errors);
  }
}

/**
 * Example 2: Create workflow via mutation
 */
async function createWorkflowExample() {
  console.log('\n=== Create Workflow Mutation ===\n');

  const engine = await createYawlEngine({
    storeUrl: 'memory://',
    enableEvents: true
  });

  const api = createYAWLGraphQLAPI({ engine });

  const mutation = `
    mutation CreateWorkflow($input: CreateWorkflowInput!) {
      createWorkflow(input: $input) {
        id
        name
        version
        tasks {
          id
          name
          type
        }
        flows {
          from
          to
          condition
        }
      }
    }
  `;

  const variables = {
    input: {
      name: 'Loan Approval Process',
      version: '1.0.0',
      description: 'Simple loan approval workflow',
      tasks: [
        {
          id: 'submit',
          name: 'Submit Application',
          type: 'ATOMIC',
          joinType: 'XOR',
          splitType: 'AND'
        },
        {
          id: 'review',
          name: 'Review Application',
          type: 'ATOMIC',
          joinType: 'XOR',
          splitType: 'XOR'
        },
        {
          id: 'approve',
          name: 'Approve Loan',
          type: 'ATOMIC',
          joinType: 'XOR',
          splitType: 'AND'
        },
        {
          id: 'reject',
          name: 'Reject Loan',
          type: 'ATOMIC',
          joinType: 'XOR',
          splitType: 'AND'
        }
      ],
      flows: [
        { from: 'submit', to: 'review' },
        { from: 'review', to: 'approve', condition: 'approved == true' },
        { from: 'review', to: 'reject', condition: 'approved == false' }
      ]
    }
  };

  const result = await api.execute(mutation, variables);

  console.log('Created Workflow:');
  console.log(JSON.stringify(result.data?.createWorkflow, null, 2));

  if (result.errors) {
    console.error('Errors:', result.errors);
  }

  return result.data?.createWorkflow;
}

/**
 * Example 3: Start case and execute tasks
 */
async function executionExample() {
  console.log('\n=== Workflow Execution Example ===\n');

  const engine = await createYawlEngine({
    storeUrl: 'memory://',
    enableEvents: true
  });

  const api = createYAWLGraphQLAPI({ engine });

  // First create workflow
  const createMutation = `
    mutation {
      createWorkflow(input: {
        name: "Order Fulfillment"
        tasks: [
          { id: "receive", name: "Receive Order", type: ATOMIC }
          { id: "process", name: "Process Payment", type: ATOMIC }
          { id: "ship", name: "Ship Order", type: ATOMIC }
        ]
        flows: [
          { from: "receive", to: "process" }
          { from: "process", to: "ship" }
        ]
      }) {
        id
        name
      }
    }
  `;

  const createResult = await api.execute(createMutation);
  const workflowId = createResult.data?.createWorkflow?.id;

  console.log('Workflow created:', workflowId);

  // Start a case
  const startCaseMutation = `
    mutation StartCase($specId: ID!, $data: JSON) {
      startCase(specId: $specId, data: $data) {
        id
        specId
        status
        startedAt
        tasks {
          id
          taskId
          status
        }
      }
    }
  `;

  const startResult = await api.execute(startCaseMutation, {
    specId: workflowId,
    data: {
      orderId: 'ORD-001',
      customerId: 'CUST-123',
      amount: 99.99
    }
  });

  console.log('\nCase started:');
  console.log(JSON.stringify(startResult.data?.startCase, null, 2));

  // Complete a task
  const taskInstanceId = startResult.data?.startCase?.tasks?.[0]?.id;

  if (taskInstanceId) {
    const completeTaskMutation = `
      mutation CompleteTask($taskInstanceId: ID!, $result: JSON) {
        completeTask(taskInstanceId: $taskInstanceId, result: $result) {
          id
          status
          completedAt
          result
        }
      }
    `;

    const completeResult = await api.execute(completeTaskMutation, {
      taskInstanceId,
      result: { status: 'success', message: 'Order received' }
    });

    console.log('\nTask completed:');
    console.log(JSON.stringify(completeResult.data?.completeTask, null, 2));
  }
}

/**
 * Example 4: Query workflow statistics
 */
async function statisticsExample() {
  console.log('\n=== Workflow Statistics Query ===\n');

  const engine = await createYawlEngine({
    storeUrl: 'memory://',
    enableEvents: true
  });

  const api = createYAWLGraphQLAPI({ engine });

  const query = `
    query WorkflowStats($specId: ID!) {
      workflowStats(specId: $specId) {
        totalCases
        runningCases
        completedCases
        averageCompletionTime
        taskStats {
          taskId
          taskName
          executionCount
          averageDuration
          successRate
        }
      }
    }
  `;

  const result = await api.execute(query, {
    specId: 'loan-approval'
  });

  console.log('Workflow Statistics:');
  console.log(JSON.stringify(result.data?.workflowStats, null, 2));

  if (result.errors) {
    console.error('Errors:', result.errors);
  }
}

/**
 * Example 5: Query receipts for audit trail
 */
async function receiptsQueryExample() {
  console.log('\n=== Receipts Query Example ===\n');

  const engine = await createYawlEngine({
    storeUrl: 'memory://',
    enableEvents: true
  });

  const api = createYAWLGraphQLAPI({ engine });

  const query = `
    query CaseReceipts($caseId: ID!, $limit: Int) {
      receipts(caseId: $caseId, limit: $limit) {
        hash
        beforeHash
        afterHash
        eventType
        timestamp
        payload
        justification {
          hookValidated
          conditionChecked
          reasoning
        }
      }
    }
  `;

  const result = await api.execute(query, {
    caseId: 'case-001',
    limit: 10
  });

  console.log('Case Receipts:');
  console.log(JSON.stringify(result.data?.receipts, null, 2));

  if (result.errors) {
    console.error('Errors:', result.errors);
  }
}

/**
 * Example 6: GraphQL subscription (pseudo-code)
 */
function subscriptionExample() {
  console.log('\n=== GraphQL Subscription Example ===\n');

  console.log('Subscription code (requires WebSocket server):');
  console.log(`
    // Client-side subscription
    const subscription = \`
      subscription CaseEvents($caseId: ID!) {
        caseEvents(caseId: $caseId) {
          type
          caseId
          timestamp
          data
        }
      }
    \`;

    // Subscribe to events
    const unsubscribe = await subscribe(
      schema,
      subscription,
      { caseId: 'case-001' }
    );

    // Process events as they arrive
    for await (const event of unsubscribe) {
      console.log('Case event:', event.data.caseEvents);
    }
  `);

  console.log('\nNote: Full subscription support requires WebSocket server');
}

/**
 * Example 7: Complete REST API wrapper
 */
async function restAPIExample() {
  console.log('\n=== REST API Wrapper Example ===\n');

  console.log('Express.js server with GraphQL endpoint:');
  console.log(`
    import express from 'express';
    import { graphqlHTTP } from 'express-graphql';
    import { createYAWLGraphQLAPI } from '@unrdf/yawl/graphql-api';
    import { createYawlEngine } from '@unrdf/yawl';

    const app = express();
    const engine = await createYawlEngine({ storeUrl: 'memory://' });
    const api = createYAWLGraphQLAPI({ engine, playground: true });

    app.use('/graphql', graphqlHTTP({
      schema: api.schema,
      graphiql: true, // Enable GraphiQL interface
    }));

    app.listen(4000, () => {
      console.log('GraphQL API running at http://localhost:4000/graphql');
    });

    // Access via:
    // - Browser: http://localhost:4000/graphql (GraphiQL)
    // - HTTP POST: curl -X POST -H "Content-Type: application/json" \\
    //              -d '{"query": "{ workflows { id name } }"}' \\
    //              http://localhost:4000/graphql
  `);
}

/**
 * Run all examples
 */
async function main() {
  try {
    console.log('GraphQL API for YAWL Workflows Examples');
    console.log('========================================');

    await basicQueryExample();
    await createWorkflowExample();
    await executionExample();
    await statisticsExample();
    await receiptsQueryExample();
    subscriptionExample();
    restAPIExample();

    console.log('\n\n✅ All examples completed successfully');
  } catch (error) {
    console.error('❌ Example failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export {
  basicQueryExample,
  createWorkflowExample,
  executionExample,
  statisticsExample,
  receiptsQueryExample,
  subscriptionExample,
  restAPIExample
};
