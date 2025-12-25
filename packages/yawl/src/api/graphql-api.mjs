/**
 * GraphQL API for YAWL Workflows
 * Complete GraphQL API layer over YAWL workflow engine
 *
 * @module @unrdf/yawl/api/graphql-api
 * @description
 * Provides a full-featured GraphQL API for YAWL workflow management,
 * execution, and querying. Integrates with existing workflow-api and
 * engine for comprehensive workflow operations via GraphQL.
 */

import { z } from 'zod';
import { graphql } from 'graphql';
import { makeExecutableSchema } from '@graphql-tools/schema';
import { YAWL_GRAPHQL_SCHEMA } from './graphql-schema.mjs';
import { createResolvers } from './graphql-resolvers.mjs';

// =============================================================================
// Configuration Schema
// =============================================================================

/**
 * GraphQL API configuration schema
 */
export const GraphQLAPIConfigSchema = z.object({
  /** YAWL engine instance */
  engine: z.any(),
  /** Enable GraphQL playground */
  playground: z.boolean().default(true),
  /** Enable introspection */
  introspection: z.boolean().default(true),
  /** Custom resolvers */
  customResolvers: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// GraphQL API Factory
// =============================================================================

/**
 * Create a GraphQL API for YAWL workflows
 *
 * @param {Object} config - API configuration
 * @returns {Object} GraphQL API instance
 *
 * @example
 * const api = createYAWLGraphQLAPI({
 *   engine: yawlEngine,
 *   playground: true,
 *   introspection: true
 * });
 *
 * const result = await api.execute(`
 *   query {
 *     workflows {
 *       id
 *       name
 *       tasks { id name }
 *     }
 *   }
 * `);
 */
export function createYAWLGraphQLAPI(config) {
  const validated = GraphQLAPIConfigSchema.parse(config);

  // Create executable schema
  const schema = makeExecutableSchema({
    typeDefs: YAWL_GRAPHQL_SCHEMA,
    resolvers: {
      ...createResolvers(validated.engine),
      ...validated.customResolvers,
      // Add JSON scalar resolver
      JSON: {
        __parseValue: (value) => value,
        __serialize: (value) => value,
        __parseLiteral: (ast) => ast.value,
      },
    },
  });

  /**
   * Execute a GraphQL query
   *
   * @param {string} query - GraphQL query string
   * @param {Object} variables - Query variables
   * @param {Object} context - Execution context
   * @returns {Promise<Object>} Query result
   */
  async function execute(query, variables = {}, context = {}) {
    return await graphql({
      schema,
      source: query,
      variableValues: variables,
      contextValue: {
        engine: validated.engine,
        ...context,
      },
    });
  }

  /**
   * Get the GraphQL schema
   *
   * @returns {Object} GraphQL schema
   */
  function getSchema() {
    return schema;
  }

  return {
    execute,
    getSchema,
    schema,
  };
}

// Re-export schema and resolvers for advanced usage
export { YAWL_GRAPHQL_SCHEMA } from './graphql-schema.mjs';
export { createResolvers } from './graphql-resolvers.mjs';
