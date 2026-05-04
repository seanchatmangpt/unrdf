/**
 * @file API Layer Tests - Comprehensive test suite for unified API layer
 * @module @unrdf/fusion/test/api-layer
 */

import { describe, it, expect, beforeAll } from 'vitest';
import { graphql, subscribe } from 'graphql';
import { createEngine } from '../src/index.mjs';
import {
  createGraphQLSchema,
  createRESTEndpoints,
  createIntrospection,
} from '../src/api-layer.mjs';

// =============================================================================
// Test Setup
// =============================================================================

let engine;
let graphqlSchema;
let restApi;
let introspectionSchema;

beforeAll(async () => {
  // Create engine with all subsystems enabled
  engine = await createEngine({
    enableCaching: true,
    enableBlockchain: true,
    enableGit: false, // Disable Git for faster tests
  });

  // Create API layers
  graphqlSchema = await createGraphQLSchema(engine);
  restApi = await createRESTEndpoints(engine);
  introspectionSchema = await createIntrospection(engine);
});

// =============================================================================
// GraphQL Tests
// =============================================================================

describe('GraphQL API', () => {
  describe('Queries', () => {
    it('should execute status query', async () => {
      const query = `
        query {
          status {
            healthy
            kgc
            cache
            policies
          }
        }
      `;

      const result = await graphql({ schema: graphqlSchema, source: query });

      expect(result.errors).toBeUndefined();
      expect(result.data).toBeDefined();
      expect(result.data.status).toBeDefined();
      expect(result.data.status.healthy).toBe(true);
      expect(result.data.status.kgc).toBeDefined();
      expect(result.data.status.cache).toBeDefined();
      expect(result.data.status.policies).toBeDefined();
    });

    it('should query receipts with pagination', async () => {
      const query = `
        query GetReceipts($limit: Int, $offset: Int) {
          receipts(limit: $limit, offset: $offset) {
            id
            hash
            timestamp
            eventType
            merkleRoot
          }
        }
      `;

      const result = await graphql({
        schema: graphqlSchema,
        source: query,
        variableValues: { limit: 5, offset: 0 },
      });

      expect(result.errors).toBeUndefined();
      expect(result.data).toBeDefined();
      expect(result.data.receipts).toBeDefined();
      expect(Array.isArray(result.data.receipts)).toBe(true);

      if (result.data.receipts.length > 0) {
        const receipt = result.data.receipts[0];
        expect(receipt.id).toBeDefined();
        expect(receipt.hash).toBeDefined();
        expect(receipt.eventType).toBeDefined();
      }
    });

    it('should query policies', async () => {
      const query = `
        query {
          policies {
            id
            trigger
            active
            executionCount
          }
        }
      `;

      const result = await graphql({ schema: graphqlSchema, source: query });

      expect(result.errors).toBeUndefined();
      expect(result.data).toBeDefined();
      expect(result.data.policies).toBeDefined();
      expect(Array.isArray(result.data.policies)).toBe(true);
    });

    it('should query resources with filter', async () => {
      const query = `
        query GetResources($cacheLevel: String) {
          resources(cacheLevel: $cacheLevel) {
            id
            cacheLevel
            size
            allocated
            timestamp
          }
        }
      `;

      const result = await graphql({
        schema: graphqlSchema,
        source: query,
        variableValues: { cacheLevel: 'L1' },
      });

      expect(result.errors).toBeUndefined();
      expect(result.data).toBeDefined();
      expect(result.data.resources).toBeDefined();
      expect(Array.isArray(result.data.resources)).toBe(true);
    });
  });

  describe('Mutations', () => {
    it('should create workflow', async () => {
      const mutation = `
        mutation CreateWorkflow($workflowId: String!, $initialData: String) {
          createWorkflow(workflowId: $workflowId, initialData: $initialData) {
            id
            hash
            timestamp
            eventType
            merkleRoot
          }
        }
      `;

      const result = await graphql({
        schema: graphqlSchema,
        source: mutation,
        variableValues: {
          workflowId: 'test-workflow-1',
          initialData: JSON.stringify({ foo: 'bar' }),
        },
      });

      expect(result.errors).toBeUndefined();
      expect(result.data).toBeDefined();
      expect(result.data.createWorkflow).toBeDefined();
      expect(result.data.createWorkflow.id).toContain('workflow-test-workflow-1');
      expect(result.data.createWorkflow.eventType).toBe('WORKFLOW_CREATED');
      expect(result.data.createWorkflow.hash).toBeDefined();
      expect(result.data.createWorkflow.merkleRoot).toBeDefined();
    });

    it('should execute workflow step', async () => {
      const mutation = `
        mutation ExecuteStep($workflowId: String!, $stepId: String!, $input: String) {
          executeStep(workflowId: $workflowId, stepId: $stepId, input: $input) {
            id
            hash
            eventType
            timestamp
          }
        }
      `;

      const result = await graphql({
        schema: graphqlSchema,
        source: mutation,
        variableValues: {
          workflowId: 'test-workflow-1',
          stepId: 'step-1',
          input: JSON.stringify({ data: 'test' }),
        },
      });

      expect(result.errors).toBeUndefined();
      expect(result.data).toBeDefined();
      expect(result.data.executeStep).toBeDefined();
      expect(result.data.executeStep.id).toContain('step-step-1');
      expect(result.data.executeStep.eventType).toBe('STEP_EXECUTED');
    });

    it('should allocate resource', async () => {
      const mutation = `
        mutation AllocateResource($resourceId: String!, $cacheLevel: String, $size: Int) {
          allocateResource(resourceId: $resourceId, cacheLevel: $cacheLevel, size: $size) {
            id
            cacheLevel
            size
            allocated
            timestamp
          }
        }
      `;

      const result = await graphql({
        schema: graphqlSchema,
        source: mutation,
        variableValues: {
          resourceId: 'test-resource-1',
          cacheLevel: 'L2',
          size: 2097152,
        },
      });

      expect(result.errors).toBeUndefined();
      expect(result.data).toBeDefined();
      expect(result.data.allocateResource).toBeDefined();
      expect(result.data.allocateResource.id).toBe('test-resource-1');
      expect(result.data.allocateResource.cacheLevel).toBe('L2');
      expect(result.data.allocateResource.size).toBe(2097152);
      expect(result.data.allocateResource.allocated).toBe(true);
    });
  });

  describe('Subscriptions', () => {
    it('should subscribe to receipt stream', async () => {
      const subscription = `
        subscription {
          receiptStream {
            id
            hash
            eventType
            timestamp
          }
        }
      `;

      const stream = await subscribe({
        schema: graphqlSchema,
        document: subscription,
      });

      expect(stream).toBeDefined();

      // Get first event from subscription
      if (Symbol.asyncIterator in stream) {
        const iterator = stream[Symbol.asyncIterator]();
        const result = await iterator.next();

        expect(result.done).toBe(false);
        expect(result.value).toBeDefined();
        expect(result.value.data).toBeDefined();
        expect(result.value.data.receiptStream).toBeDefined();
        expect(result.value.data.receiptStream.id).toBeDefined();
        expect(result.value.data.receiptStream.eventType).toBe('STREAM_EVENT');

        // Clean up
        await iterator.return?.();
      }
    });

    it('should subscribe to allocation events', async () => {
      const subscription = `
        subscription {
          allocationEvents {
            id
            cacheLevel
            allocated
            timestamp
          }
        }
      `;

      const stream = await subscribe({
        schema: graphqlSchema,
        document: subscription,
      });

      expect(stream).toBeDefined();

      // Get first event from subscription
      if (Symbol.asyncIterator in stream) {
        const iterator = stream[Symbol.asyncIterator]();
        const result = await iterator.next();

        expect(result.done).toBe(false);
        expect(result.value).toBeDefined();
        expect(result.value.data).toBeDefined();
        expect(result.value.data.allocationEvents).toBeDefined();
        expect(result.value.data.allocationEvents.allocated).toBe(true);

        // Clean up
        await iterator.return?.();
      }
    });
  });
});

// =============================================================================
// REST API Tests
// =============================================================================

describe('REST API', () => {
  describe('GET Endpoints', () => {
    it('should get status', async () => {
      const mockReq = { query: {} };
      const mockRes = {};

      const result = await restApi.handlers.getStatus(mockReq, mockRes);

      expect(result).toBeDefined();
      expect(result.status).toBe('ok');
      expect(result.timestamp).toBeDefined();
      expect(result.engine).toBeDefined();
      expect(result.engine.kgc).toBeDefined();
      expect(result.engine.cache).toBeDefined();
      expect(result.engine.policies).toBeDefined();
      expect(result.health).toBeDefined();
      expect(result.health.store).toBe(true);
      expect(result.health.kgcStore).toBe(true);
    });

    it('should get receipts with pagination', async () => {
      const mockReq = { query: { limit: '5', offset: '0' } };
      const mockRes = {};

      const result = await restApi.handlers.getReceipts(mockReq, mockRes);

      expect(result).toBeDefined();
      expect(result.receipts).toBeDefined();
      expect(Array.isArray(result.receipts)).toBe(true);
      expect(result.total).toBeDefined();
      expect(result.limit).toBe(5);
      expect(result.offset).toBe(0);
    });

    it('should get policies', async () => {
      const mockReq = { query: {} };
      const mockRes = {};

      const result = await restApi.handlers.getPolicies(mockReq, mockRes);

      expect(result).toBeDefined();
      expect(result.policies).toBeDefined();
      expect(Array.isArray(result.policies)).toBe(true);
      expect(result.total).toBeDefined();
    });
  });

  describe('POST Endpoints', () => {
    it('should create workflow', async () => {
      const mockReq = {
        body: {
          workflowId: 'rest-workflow-1',
          initialData: { key: 'value' },
        },
      };
      const mockRes = {};

      const result = await restApi.handlers.createWorkflow(mockReq, mockRes);

      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.workflow).toBeDefined();
      expect(result.workflow.id).toBe('rest-workflow-1');
      expect(result.workflow.createdAt).toBeDefined();
      expect(result.receipt).toBeDefined();
      expect(result.receipt.workflowId).toBe('rest-workflow-1');
      expect(result.receipt.eventType).toBe('WORKFLOW_CREATED');
    });

    it('should allocate resource', async () => {
      const mockReq = {
        body: {
          resourceId: 'rest-resource-1',
          cacheLevel: 'L3',
          size: 4194304,
        },
      };
      const mockRes = {};

      const result = await restApi.handlers.allocateResource(mockReq, mockRes);

      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.allocation).toBeDefined();
      expect(result.allocation.id).toBe('rest-resource-1');
      expect(result.allocation.cacheLevel).toBe('L3');
      expect(result.allocation.size).toBe(4194304);
      expect(result.allocation.allocated).toBe(true);
    });

    it('should register policy', async () => {
      const mockReq = {
        body: {
          id: 'test-policy',
          trigger: 'before-add',
          validate: 'function validate() {}',
        },
      };
      const mockRes = {};

      const result = await restApi.handlers.registerPolicy(mockReq, mockRes);

      expect(result).toBeDefined();
      expect(result.success).toBe(true);
      expect(result.policy).toBeDefined();
      expect(result.policy.id).toBe('test-policy');
      expect(result.policy.registered).toBe(true);
    });
  });

  describe('Error Handling', () => {
    it('should handle invalid workflow request', async () => {
      const mockReq = {
        body: {
          // Missing workflowId
          initialData: { key: 'value' },
        },
      };
      const mockRes = {};

      await expect(
        restApi.handlers.createWorkflow(mockReq, mockRes)
      ).rejects.toThrow();
    });

    it('should handle invalid allocation request', async () => {
      const mockReq = {
        body: {
          // Missing resourceId
          cacheLevel: 'L1',
        },
      };
      const mockRes = {};

      await expect(
        restApi.handlers.allocateResource(mockReq, mockRes)
      ).rejects.toThrow();
    });

    it('should handle invalid cache level', async () => {
      const mockReq = {
        body: {
          resourceId: 'test',
          cacheLevel: 'INVALID', // Not L1/L2/L3
        },
      };
      const mockRes = {};

      await expect(
        restApi.handlers.allocateResource(mockReq, mockRes)
      ).rejects.toThrow();
    });
  });
});

// =============================================================================
// Introspection Tests
// =============================================================================

describe('Introspection API', () => {
  it('should have valid schema structure', () => {
    expect(introspectionSchema).toBeDefined();
    expect(introspectionSchema.version).toBe('1.0.0');
    expect(introspectionSchema.description).toBe('Unified Fusion Engine API');
    expect(introspectionSchema.timestamp).toBeDefined();
  });

  it('should report correct capabilities', () => {
    expect(introspectionSchema.capabilities).toBeDefined();
    expect(introspectionSchema.capabilities.store).toBe(true);
    expect(introspectionSchema.capabilities.kgcStore).toBe(true);
    expect(introspectionSchema.capabilities.receipts).toBe(true);
    expect(introspectionSchema.capabilities.policies).toBe(true);
    expect(introspectionSchema.capabilities.resources).toBe(true);
  });

  it('should document GraphQL queries', () => {
    expect(introspectionSchema.graphql).toBeDefined();
    expect(introspectionSchema.graphql.queries).toBeDefined();
    expect(Array.isArray(introspectionSchema.graphql.queries)).toBe(true);
    expect(introspectionSchema.graphql.queries.length).toBeGreaterThan(0);

    const statusQuery = introspectionSchema.graphql.queries.find(
      (q) => q.name === 'status'
    );
    expect(statusQuery).toBeDefined();
    expect(statusQuery.description).toBe('Get engine status');
    expect(statusQuery.returns).toBe('Status');
  });

  it('should document GraphQL mutations', () => {
    expect(introspectionSchema.graphql.mutations).toBeDefined();
    expect(Array.isArray(introspectionSchema.graphql.mutations)).toBe(true);
    expect(introspectionSchema.graphql.mutations.length).toBeGreaterThan(0);

    const createWorkflowMutation = introspectionSchema.graphql.mutations.find(
      (m) => m.name === 'createWorkflow'
    );
    expect(createWorkflowMutation).toBeDefined();
    expect(createWorkflowMutation.description).toBe('Create new workflow instance');
    expect(createWorkflowMutation.args).toBeDefined();
    expect(createWorkflowMutation.args.workflowId).toBe('String!');
  });

  it('should document GraphQL subscriptions', () => {
    expect(introspectionSchema.graphql.subscriptions).toBeDefined();
    expect(Array.isArray(introspectionSchema.graphql.subscriptions)).toBe(true);
    expect(introspectionSchema.graphql.subscriptions.length).toBeGreaterThan(0);

    const receiptStream = introspectionSchema.graphql.subscriptions.find(
      (s) => s.name === 'receiptStream'
    );
    expect(receiptStream).toBeDefined();
    expect(receiptStream.description).toBe('Subscribe to receipt events');
    expect(receiptStream.returns).toBe('Receipt');
  });

  it('should document REST endpoints', () => {
    expect(introspectionSchema.rest).toBeDefined();
    expect(introspectionSchema.rest.endpoints).toBeDefined();
    expect(Array.isArray(introspectionSchema.rest.endpoints)).toBe(true);
    expect(introspectionSchema.rest.endpoints.length).toBeGreaterThan(0);

    const statusEndpoint = introspectionSchema.rest.endpoints.find(
      (e) => e.path === '/api/v1/status'
    );
    expect(statusEndpoint).toBeDefined();
    expect(statusEndpoint.method).toBe('GET');
    expect(statusEndpoint.handler).toBe('getStatus');
    expect(statusEndpoint.description).toBe('Get engine status');
  });

  it('should include engine stats', () => {
    expect(introspectionSchema.stats).toBeDefined();
    expect(typeof introspectionSchema.stats).toBe('object');
  });

  it('should be deterministic (same engine produces same schema)', async () => {
    const schema1 = await createIntrospection(engine);
    const schema2 = await createIntrospection(engine);

    // Remove timestamps for comparison
    const { timestamp: ts1, ...rest1 } = schema1;
    const { timestamp: ts2, ...rest2 } = schema2;

    expect(rest1).toEqual(rest2);
  });
});

// =============================================================================
// Integration Tests
// =============================================================================

describe('API Layer Integration', () => {
  it('should expose consistent routes between REST and introspection', () => {
    const restRoutes = Object.keys(restApi.routes);
    const introspectionEndpoints = introspectionSchema.rest.endpoints.map(
      (e) => `${e.method} ${e.path}`
    );

    expect(restRoutes).toEqual(expect.arrayContaining(introspectionEndpoints));
  });

  it('should have matching GraphQL schema and introspection', () => {
    const schemaQueryType = graphqlSchema.getQueryType();
    const schemaFields = Object.keys(schemaQueryType.getFields());

    const introspectionQueries = introspectionSchema.graphql.queries.map(
      (q) => q.name
    );

    expect(schemaFields).toEqual(expect.arrayContaining(introspectionQueries));
  });

  it('should handle end-to-end workflow via GraphQL', async () => {
    // 1. Create workflow
    const createMutation = `
      mutation {
        createWorkflow(workflowId: "e2e-test", initialData: "{}") {
          id
          hash
          eventType
        }
      }
    `;

    const createResult = await graphql({
      schema: graphqlSchema,
      source: createMutation,
    });

    expect(createResult.errors).toBeUndefined();
    expect(createResult.data.createWorkflow.eventType).toBe('WORKFLOW_CREATED');

    // 2. Execute step
    const executeMutation = `
      mutation {
        executeStep(workflowId: "e2e-test", stepId: "step-1", input: "{}") {
          id
          eventType
        }
      }
    `;

    const executeResult = await graphql({
      schema: graphqlSchema,
      source: executeMutation,
    });

    expect(executeResult.errors).toBeUndefined();
    expect(executeResult.data.executeStep.eventType).toBe('STEP_EXECUTED');

    // 3. Query receipts
    const receiptQuery = `
      query {
        receipts(limit: 10) {
          id
          eventType
        }
      }
    `;

    const receiptResult = await graphql({
      schema: graphqlSchema,
      source: receiptQuery,
    });

    expect(receiptResult.errors).toBeUndefined();
    expect(receiptResult.data.receipts).toBeDefined();
  });

  it('should handle end-to-end workflow via REST', async () => {
    // 1. Check status
    const statusResult = await restApi.handlers.getStatus({}, {});
    expect(statusResult.status).toBe('ok');

    // 2. Create workflow
    const workflowResult = await restApi.handlers.createWorkflow(
      { body: { workflowId: 'rest-e2e', initialData: {} } },
      {}
    );
    expect(workflowResult.success).toBe(true);

    // 3. Allocate resource
    const allocateResult = await restApi.handlers.allocateResource(
      { body: { resourceId: 'rest-e2e-resource', cacheLevel: 'L1' } },
      {}
    );
    expect(allocateResult.success).toBe(true);

    // 4. Get receipts
    const receiptsResult = await restApi.handlers.getReceipts(
      { query: { limit: '10' } },
      {}
    );
    expect(receiptsResult.receipts).toBeDefined();
  });
});
