/**
 * @file Unified API Layer - GraphQL/REST/Introspection for Fusion Engine
 * @module @unrdf/fusion/api-layer
 *
 * @description
 * Standardizes wrappers around the unified fusion index:
 * - GraphQL: Query, Mutation, Subscription support
 * - REST: HTTP endpoints for workflow, receipts, resources
 * - Introspection: JSON schema for self-documentation
 *
 * @example
 * import { createEngine } from '@unrdf/fusion';
 * import { createGraphQLSchema, createRESTEndpoints } from '@unrdf/fusion/api-layer';
 *
 * const engine = await createEngine();
 * const schema = await createGraphQLSchema(engine);
 * const { routes, handlers } = await createRESTEndpoints(engine);
 */

import {
  GraphQLObjectType,
  GraphQLString,
  GraphQLInt,
  GraphQLBoolean,
  GraphQLList,
  GraphQLNonNull,
  GraphQLSchema,
  GraphQLID,
} from 'graphql';
import { z } from 'zod';

// =============================================================================
// Zod Schemas for Validation
// =============================================================================

const EngineConfigSchema = z.object({
  enableCaching: z.boolean().optional(),
  enableBlockchain: z.boolean().optional(),
  enableGit: z.boolean().optional(),
});

const WorkflowRequestSchema = z.object({
  workflowId: z.string(),
  initialData: z.record(z.any()).optional(),
});

const AllocationRequestSchema = z.object({
  resourceId: z.string(),
  cacheLevel: z.enum(['L1', 'L2', 'L3']).optional(),
  size: z.number().optional(),
});

// =============================================================================
// GraphQL Schema Generation
// =============================================================================

/**
 * Create GraphQL schema for fusion engine
 *
 * @param {Object} engine - Fusion engine instance
 * @returns {Promise<GraphQLSchema>} GraphQL schema with queries, mutations, subscriptions
 *
 * @example
 * const schema = await createGraphQLSchema(engine);
 * // Use with Apollo Server, GraphQL Yoga, etc.
 */
export async function createGraphQLSchema(engine) {
  // Status Type
  const StatusType = new GraphQLObjectType({
    name: 'Status',
    description: 'Engine status information',
    fields: {
      kgc: {
        type: GraphQLString,
        description: 'KGC-4D engine status',
        resolve: () => JSON.stringify(engine.getStats().kgc),
      },
      cache: {
        type: GraphQLString,
        description: 'Cache system status',
        resolve: () => JSON.stringify(engine.getStats().cache),
      },
      policies: {
        type: GraphQLString,
        description: 'Policy hooks status',
        resolve: () => JSON.stringify(engine.getStats().policies),
      },
      healthy: {
        type: GraphQLBoolean,
        description: 'Overall health status',
        resolve: () => {
          const stats = engine.getStats();
          return stats !== null && stats !== undefined;
        },
      },
    },
  });

  // Receipt Type
  const ReceiptType = new GraphQLObjectType({
    name: 'Receipt',
    description: 'Blockchain receipt information',
    fields: {
      id: { type: new GraphQLNonNull(GraphQLID) },
      hash: { type: GraphQLString },
      timestamp: { type: GraphQLString },
      eventType: { type: GraphQLString },
      merkleRoot: { type: GraphQLString },
    },
  });

  // Resource Allocation Type
  const ResourceType = new GraphQLObjectType({
    name: 'Resource',
    description: 'Resource allocation information',
    fields: {
      id: { type: new GraphQLNonNull(GraphQLID) },
      cacheLevel: { type: GraphQLString },
      size: { type: GraphQLInt },
      allocated: { type: GraphQLBoolean },
      timestamp: { type: GraphQLString },
    },
  });

  // Policy Type
  const PolicyType = new GraphQLObjectType({
    name: 'Policy',
    description: 'Policy hook information',
    fields: {
      id: { type: new GraphQLNonNull(GraphQLID) },
      trigger: { type: GraphQLString },
      active: { type: GraphQLBoolean },
      executionCount: { type: GraphQLInt },
    },
  });

  // Query Type
  const QueryType = new GraphQLObjectType({
    name: 'Query',
    description: 'Root query type',
    fields: {
      status: {
        type: StatusType,
        description: 'Get engine status',
        resolve: () => ({}),
      },
      receipts: {
        type: new GraphQLList(ReceiptType),
        description: 'Get all blockchain receipts',
        args: {
          limit: { type: GraphQLInt, defaultValue: 10 },
          offset: { type: GraphQLInt, defaultValue: 0 },
        },
        resolve: async (_, { limit, offset }) => {
          // Mock implementation - actual receipts would come from blockchain anchorer
          if (!engine.receipts) return [];

          // Return deterministic mock data for testing
          return [
            {
              id: 'receipt-1',
              hash: 'abc123',
              timestamp: new Date().toISOString(),
              eventType: 'CREATE',
              merkleRoot: 'merkle-root-1',
            },
          ].slice(offset, offset + limit);
        },
      },
      policies: {
        type: new GraphQLList(PolicyType),
        description: 'List all registered policies',
        resolve: async () => {
          if (!engine.policies) return [];

          const stats = engine.policies.getRegistryStats?.() || {};
          return [
            {
              id: 'policy-validation',
              trigger: 'before-add',
              active: true,
              executionCount: stats.totalExecutions || 0,
            },
          ];
        },
      },
      resources: {
        type: new GraphQLList(ResourceType),
        description: 'List allocated resources',
        args: {
          cacheLevel: { type: GraphQLString },
        },
        resolve: async (_, { cacheLevel }) => {
          if (!engine.resources) return [];

          // Mock resource allocations
          return [
            {
              id: 'resource-1',
              cacheLevel: cacheLevel || 'L1',
              size: 1024 * 1024,
              allocated: true,
              timestamp: new Date().toISOString(),
            },
          ];
        },
      },
    },
  });

  // Mutation Type
  const MutationType = new GraphQLObjectType({
    name: 'Mutation',
    description: 'Root mutation type',
    fields: {
      createWorkflow: {
        type: ReceiptType,
        description: 'Create a new workflow instance',
        args: {
          workflowId: { type: new GraphQLNonNull(GraphQLString) },
          initialData: { type: GraphQLString },
        },
        resolve: async (_, { workflowId, initialData }) => {
          const data = initialData ? JSON.parse(initialData) : {};

          // Create workflow in KGC store
          if (engine.kgcStore) {
            // Mock workflow creation
            return {
              id: `workflow-${workflowId}`,
              hash: 'workflow-hash-123',
              timestamp: new Date().toISOString(),
              eventType: 'WORKFLOW_CREATED',
              merkleRoot: 'workflow-merkle-root',
            };
          }

          throw new Error('KGC store not available');
        },
      },
      executeStep: {
        type: ReceiptType,
        description: 'Execute a workflow step',
        args: {
          workflowId: { type: new GraphQLNonNull(GraphQLString) },
          stepId: { type: new GraphQLNonNull(GraphQLString) },
          input: { type: GraphQLString },
        },
        resolve: async (_, { workflowId, stepId, input }) => {
          const data = input ? JSON.parse(input) : {};

          return {
            id: `step-${stepId}`,
            hash: 'step-hash-456',
            timestamp: new Date().toISOString(),
            eventType: 'STEP_EXECUTED',
            merkleRoot: 'step-merkle-root',
          };
        },
      },
      allocateResource: {
        type: ResourceType,
        description: 'Allocate a cache resource',
        args: {
          resourceId: { type: new GraphQLNonNull(GraphQLString) },
          cacheLevel: { type: GraphQLString, defaultValue: 'L1' },
          size: { type: GraphQLInt, defaultValue: 1048576 },
        },
        resolve: async (_, { resourceId, cacheLevel, size }) => {
          if (!engine.resources) {
            throw new Error('Caching not enabled');
          }

          // Mock resource allocation
          return {
            id: resourceId,
            cacheLevel,
            size,
            allocated: true,
            timestamp: new Date().toISOString(),
          };
        },
      },
    },
  });

  // Subscription Type (for real-time updates)
  const SubscriptionType = new GraphQLObjectType({
    name: 'Subscription',
    description: 'Root subscription type',
    fields: {
      receiptStream: {
        type: ReceiptType,
        description: 'Subscribe to receipt events',
        resolve: (payload) => payload,
        subscribe: async function* () {
          // Mock subscription - in production would use SSE/WebSocket
          yield {
            id: 'receipt-stream-1',
            hash: 'stream-hash-789',
            timestamp: new Date().toISOString(),
            eventType: 'STREAM_EVENT',
            merkleRoot: 'stream-merkle-root',
          };
        },
      },
      allocationEvents: {
        type: ResourceType,
        description: 'Subscribe to resource allocation events',
        resolve: (payload) => payload,
        subscribe: async function* () {
          // Mock subscription
          yield {
            id: 'allocation-stream-1',
            cacheLevel: 'L1',
            size: 1024 * 1024,
            allocated: true,
            timestamp: new Date().toISOString(),
          };
        },
      },
    },
  });

  return new GraphQLSchema({
    query: QueryType,
    mutation: MutationType,
    subscription: SubscriptionType,
  });
}

// =============================================================================
// REST Endpoints
// =============================================================================

/**
 * Create REST endpoints for fusion engine
 *
 * @param {Object} engine - Fusion engine instance
 * @returns {Promise<Object>} Object with routes and handlers
 *
 * @example
 * const { routes, handlers } = await createRESTEndpoints(engine);
 * // Use with Express, Fastify, etc.
 */
export async function createRESTEndpoints(engine) {
  const routes = {
    'GET /api/v1/status': 'getStatus',
    'POST /api/v1/workflow': 'createWorkflow',
    'GET /api/v1/receipts': 'getReceipts',
    'POST /api/v1/allocate': 'allocateResource',
    'GET /api/v1/policies': 'getPolicies',
    'POST /api/v1/policies': 'registerPolicy',
  };

  const handlers = {
    /**
     * GET /api/v1/status
     * Get engine status
     */
    getStatus: async (req, res) => {
      const stats = engine.getStats();

      return {
        status: 'ok',
        timestamp: new Date().toISOString(),
        engine: {
          kgc: stats.kgc || {},
          cache: stats.cache || {},
          policies: stats.policies || {},
        },
        health: {
          store: !!engine.store,
          kgcStore: !!engine.kgcStore,
          receipts: !!engine.receipts,
          policies: !!engine.policies,
          resources: !!engine.resources,
        },
      };
    },

    /**
     * POST /api/v1/workflow
     * Create new workflow instance
     */
    createWorkflow: async (req, res) => {
      const validatedData = WorkflowRequestSchema.parse(req.body);
      const { workflowId, initialData = {} } = validatedData;

      if (!engine.kgcStore) {
        throw new Error('KGC store not available');
      }

      // Mock workflow creation
      const receipt = {
        id: `workflow-${workflowId}`,
        workflowId,
        hash: 'workflow-hash-123',
        timestamp: new Date().toISOString(),
        eventType: 'WORKFLOW_CREATED',
        data: initialData,
      };

      return {
        success: true,
        workflow: {
          id: workflowId,
          createdAt: new Date().toISOString(),
        },
        receipt,
      };
    },

    /**
     * GET /api/v1/receipts
     * Get blockchain receipts
     */
    getReceipts: async (req, res) => {
      const limit = parseInt(req.query?.limit || '10', 10);
      const offset = parseInt(req.query?.offset || '0', 10);

      if (!engine.receipts) {
        return { receipts: [] };
      }

      // Mock receipts
      const allReceipts = [
        {
          id: 'receipt-1',
          hash: 'abc123',
          timestamp: new Date().toISOString(),
          eventType: 'CREATE',
          merkleRoot: 'merkle-root-1',
        },
        {
          id: 'receipt-2',
          hash: 'def456',
          timestamp: new Date().toISOString(),
          eventType: 'UPDATE',
          merkleRoot: 'merkle-root-2',
        },
      ];

      return {
        receipts: allReceipts.slice(offset, offset + limit),
        total: allReceipts.length,
        limit,
        offset,
      };
    },

    /**
     * POST /api/v1/allocate
     * Allocate cache resource
     */
    allocateResource: async (req, res) => {
      const validatedData = AllocationRequestSchema.parse(req.body);
      const { resourceId, cacheLevel = 'L1', size = 1048576 } = validatedData;

      if (!engine.resources) {
        throw new Error('Caching not enabled');
      }

      // Mock resource allocation
      const allocation = {
        id: resourceId,
        cacheLevel,
        size,
        allocated: true,
        timestamp: new Date().toISOString(),
      };

      return {
        success: true,
        allocation,
      };
    },

    /**
     * GET /api/v1/policies
     * List registered policies
     */
    getPolicies: async (req, res) => {
      if (!engine.policies) {
        return { policies: [] };
      }

      const stats = engine.policies.getRegistryStats?.() || {};

      return {
        policies: [
          {
            id: 'policy-validation',
            trigger: 'before-add',
            active: true,
            executionCount: stats.totalExecutions || 0,
          },
        ],
        total: 1,
      };
    },

    /**
     * POST /api/v1/policies
     * Register new policy
     */
    registerPolicy: async (req, res) => {
      const { id, trigger, validate, transform } = req.body;

      if (!engine.policies) {
        throw new Error('Policy registry not available');
      }

      // Mock policy registration
      return {
        success: true,
        policy: {
          id,
          trigger,
          registered: true,
          timestamp: new Date().toISOString(),
        },
      };
    },
  };

  return { routes, handlers };
}

// =============================================================================
// Introspection API
// =============================================================================

/**
 * Create introspection schema for self-documentation
 *
 * @param {Object} engine - Fusion engine instance
 * @returns {Promise<Object>} JSON schema of all available operations
 *
 * @example
 * const schema = await createIntrospection(engine);
 * // Use in tools/prove.mjs for self-documentation
 */
export async function createIntrospection(engine) {
  const schema = {
    version: '1.0.0',
    description: 'Unified Fusion Engine API',
    timestamp: new Date().toISOString(),

    capabilities: {
      store: !!engine.store,
      kgcStore: !!engine.kgcStore,
      receipts: !!engine.receipts,
      policies: !!engine.policies,
      resources: !!engine.resources,
    },

    graphql: {
      queries: [
        {
          name: 'status',
          description: 'Get engine status',
          returns: 'Status',
        },
        {
          name: 'receipts',
          description: 'Get blockchain receipts',
          args: { limit: 'Int', offset: 'Int' },
          returns: '[Receipt]',
        },
        {
          name: 'policies',
          description: 'List registered policies',
          returns: '[Policy]',
        },
        {
          name: 'resources',
          description: 'List allocated resources',
          args: { cacheLevel: 'String' },
          returns: '[Resource]',
        },
      ],
      mutations: [
        {
          name: 'createWorkflow',
          description: 'Create new workflow instance',
          args: { workflowId: 'String!', initialData: 'String' },
          returns: 'Receipt',
        },
        {
          name: 'executeStep',
          description: 'Execute workflow step',
          args: { workflowId: 'String!', stepId: 'String!', input: 'String' },
          returns: 'Receipt',
        },
        {
          name: 'allocateResource',
          description: 'Allocate cache resource',
          args: { resourceId: 'String!', cacheLevel: 'String', size: 'Int' },
          returns: 'Resource',
        },
      ],
      subscriptions: [
        {
          name: 'receiptStream',
          description: 'Subscribe to receipt events',
          returns: 'Receipt',
        },
        {
          name: 'allocationEvents',
          description: 'Subscribe to resource allocation events',
          returns: 'Resource',
        },
      ],
    },

    rest: {
      endpoints: [
        {
          method: 'GET',
          path: '/api/v1/status',
          description: 'Get engine status',
          handler: 'getStatus',
        },
        {
          method: 'POST',
          path: '/api/v1/workflow',
          description: 'Create new workflow instance',
          handler: 'createWorkflow',
          body: WorkflowRequestSchema.shape,
        },
        {
          method: 'GET',
          path: '/api/v1/receipts',
          description: 'Get blockchain receipts',
          handler: 'getReceipts',
          query: { limit: 'number', offset: 'number' },
        },
        {
          method: 'POST',
          path: '/api/v1/allocate',
          description: 'Allocate cache resource',
          handler: 'allocateResource',
          body: AllocationRequestSchema.shape,
        },
        {
          method: 'GET',
          path: '/api/v1/policies',
          description: 'List registered policies',
          handler: 'getPolicies',
        },
        {
          method: 'POST',
          path: '/api/v1/policies',
          description: 'Register new policy',
          handler: 'registerPolicy',
        },
      ],
    },

    stats: engine.getStats(),
  };

  return schema;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  createGraphQLSchema,
  createRESTEndpoints,
  createIntrospection,
};
