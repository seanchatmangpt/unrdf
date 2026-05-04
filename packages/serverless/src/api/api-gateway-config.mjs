/**
 * @fileoverview API Gateway Configuration - REST API setup for UNRDF
 *
 * @description
 * Configures API Gateway REST endpoints for UNRDF serverless deployments.
 * Handles routing, authentication, rate limiting, and request/response transformations.
 *
 * @module serverless/api/api-gateway-config
 * @version 1.0.0
 * @license MIT
 */

import { z } from 'zod';

/**
 * API endpoint configuration
 * @typedef {Object} EndpointConfig
 * @property {string} path - API path
 * @property {string} method - HTTP method
 * @property {string} functionName - Lambda function name
 * @property {boolean} authRequired - Require authentication
 * @property {number} rateLimit - Requests per second limit
 */
const EndpointConfigSchema = z.object({
  path: z.string(),
  method: z.enum(['GET', 'POST', 'PUT', 'DELETE', 'PATCH']),
  functionName: z.string(),
  authRequired: z.boolean().default(false),
  rateLimit: z.number().min(1).default(100),
  timeout: z.number().min(1).max(29).default(29),
});

/**
 * API Gateway configuration
 * @typedef {Object} ApiGatewayConfig
 * @property {string} apiName - API name
 * @property {string} stage - Deployment stage
 * @property {EndpointConfig[]} endpoints - API endpoints
 * @property {Object} cors - CORS configuration
 */
const ApiGatewayConfigSchema = z.object({
  apiName: z.string(),
  stage: z.enum(['dev', 'staging', 'prod']).default('dev'),
  endpoints: z.array(EndpointConfigSchema),
  cors: z
    .object({
      allowOrigins: z.array(z.string()).default(['*']),
      allowMethods: z.array(z.string()).default(['GET', 'POST', 'PUT', 'DELETE']),
      allowHeaders: z.array(z.string()).default(['Content-Type', 'Authorization']),
      maxAge: z.number().default(3600),
    })
    .default({}),
  throttling: z
    .object({
      burstLimit: z.number().default(5000),
      rateLimit: z.number().default(10000),
    })
    .default({}),
});

/**
 * API Gateway Configuration Builder
 *
 * @class ApiGatewayConfig
 *
 * @description
 * Fluent builder for API Gateway configurations with:
 * - Endpoint routing and method mapping
 * - CORS policy configuration
 * - Rate limiting and throttling
 * - Request/response validation
 * - Authentication integration
 *
 * @example
 * ```javascript
 * const config = new ApiGatewayConfig('unrdf-api')
 *   .addEndpoint('/query', 'POST', 'queryFunction')
 *   .addEndpoint('/triples', 'GET', 'listFunction')
 *   .enableCors(['https://example.com'])
 *   .setRateLimit(1000)
 *   .build();
 * ```
 */
export class ApiGatewayConfig {
  /**
   * Configuration object
   * @type {Object}
   * @private
   */
  #config;

  /**
   * Create API Gateway configuration
   *
   * @param {string} apiName - API name
   * @param {string} [stage='dev'] - Deployment stage
   */
  constructor(apiName, stage = 'dev') {
    this.#config = {
      apiName,
      stage,
      endpoints: [],
      cors: {
        allowOrigins: ['*'],
        allowMethods: ['GET', 'POST', 'PUT', 'DELETE'],
        allowHeaders: ['Content-Type', 'Authorization'],
        maxAge: 3600,
      },
      throttling: {
        burstLimit: 5000,
        rateLimit: 10000,
      },
    };
  }

  /**
   * Add API endpoint
   *
   * @param {string} path - Endpoint path
   * @param {string} method - HTTP method
   * @param {string} functionName - Lambda function name
   * @param {Object} [options={}] - Additional options
   * @returns {ApiGatewayConfig} This instance for chaining
   *
   * @example
   * ```javascript
   * config.addEndpoint('/query', 'POST', 'queryFunction', {
   *   authRequired: true,
   *   rateLimit: 100
   * });
   * ```
   */
  addEndpoint(path, method, functionName, options = {}) {
    this.#config.endpoints.push({
      path,
      method,
      functionName,
      authRequired: options.authRequired || false,
      rateLimit: options.rateLimit || 100,
      timeout: options.timeout || 29,
    });
    return this;
  }

  /**
   * Enable CORS with custom origins
   *
   * @param {string[]} allowOrigins - Allowed origins
   * @param {Object} [options={}] - CORS options
   * @returns {ApiGatewayConfig} This instance for chaining
   *
   * @example
   * ```javascript
   * config.enableCors(['https://example.com'], {
   *   allowMethods: ['GET', 'POST'],
   *   maxAge: 7200
   * });
   * ```
   */
  enableCors(allowOrigins, options = {}) {
    this.#config.cors = {
      allowOrigins,
      allowMethods: options.allowMethods || this.#config.cors.allowMethods,
      allowHeaders: options.allowHeaders || this.#config.cors.allowHeaders,
      maxAge: options.maxAge || this.#config.cors.maxAge,
    };
    return this;
  }

  /**
   * Set API-wide rate limiting
   *
   * @param {number} rateLimit - Requests per second
   * @param {number} [burstLimit] - Burst capacity
   * @returns {ApiGatewayConfig} This instance for chaining
   *
   * @example
   * ```javascript
   * config.setRateLimit(1000, 5000);
   * ```
   */
  setRateLimit(rateLimit, burstLimit) {
    this.#config.throttling = {
      rateLimit,
      burstLimit: burstLimit || rateLimit * 5,
    };
    return this;
  }

  /**
   * Set deployment stage
   *
   * @param {string} stage - Stage name
   * @returns {ApiGatewayConfig} This instance for chaining
   */
  setStage(stage) {
    this.#config.stage = stage;
    return this;
  }

  /**
   * Build and validate configuration
   *
   * @returns {Object} Validated configuration
   *
   * @throws {Error} If configuration is invalid
   *
   * @example
   * ```javascript
   * const config = builder.build();
   * ```
   */
  build() {
    return ApiGatewayConfigSchema.parse(this.#config);
  }

  /**
   * Export as OpenAPI 3.0 specification
   *
   * @returns {Object} OpenAPI specification
   *
   * @example
   * ```javascript
   * const openapi = config.toOpenAPI();
   * await fs.writeFile('openapi.json', JSON.stringify(openapi, null, 2));
   * ```
   */
  toOpenAPI() {
    const paths = {};

    for (const endpoint of this.#config.endpoints) {
      if (!paths[endpoint.path]) {
        paths[endpoint.path] = {};
      }

      paths[endpoint.path][endpoint.method.toLowerCase()] = {
        summary: `${endpoint.method} ${endpoint.path}`,
        operationId: endpoint.functionName,
        security: endpoint.authRequired ? [{ apiKey: [] }] : [],
        responses: {
          200: {
            description: 'Successful response',
            content: {
              'application/json': {
                schema: { type: 'object' },
              },
            },
          },
          400: { description: 'Bad request' },
          401: { description: 'Unauthorized' },
          429: { description: 'Too many requests' },
          500: { description: 'Internal server error' },
        },
      };
    }

    return {
      openapi: '3.0.0',
      info: {
        title: this.#config.apiName,
        version: '1.0.0',
        description: 'UNRDF Serverless API',
      },
      servers: [
        {
          url: `https://api.example.com/${this.#config.stage}`,
          description: `${this.#config.stage} environment`,
        },
      ],
      paths,
      components: {
        securitySchemes: {
          apiKey: {
            type: 'apiKey',
            in: 'header',
            name: 'X-API-Key',
          },
        },
      },
    };
  }
}

/**
 * Create default UNRDF API configuration
 *
 * @param {string} [stage='dev'] - Deployment stage
 * @returns {Object} API Gateway configuration
 *
 * @example
 * ```javascript
 * const config = createDefaultApiConfig('prod');
 * ```
 */
export function createDefaultApiConfig(stage = 'dev') {
  return new ApiGatewayConfig(`unrdf-api-${stage}`, stage)
    .addEndpoint('/query', 'POST', 'queryFunction', {
      authRequired: stage === 'prod',
      rateLimit: 100,
    })
    .addEndpoint('/triples', 'GET', 'queryFunction', {
      rateLimit: 200,
    })
    .addEndpoint('/triples', 'POST', 'ingestFunction', {
      authRequired: true,
      rateLimit: 50,
    })
    .addEndpoint('/health', 'GET', 'queryFunction', {
      rateLimit: 1000,
    })
    .enableCors(['*'])
    .setRateLimit(10000, 50000)
    .build();
}

/**
 * Validate API Gateway request
 *
 * @param {Object} event - API Gateway event
 * @param {Object} schema - Zod validation schema
 * @returns {Object} Validated request body
 *
 * @throws {Error} If validation fails
 *
 * @example
 * ```javascript
 * export async function handler(event) {
 *   const body = validateApiRequest(event, z.object({
 *     query: z.string(),
 *     bindings: z.record(z.string()).optional()
 *   }));
 *   // ... handle request
 * }
 * ```
 */
export function validateApiRequest(event, schema) {
  try {
    const body = JSON.parse(event.body || '{}');
    return schema.parse(body);
  } catch (error) {
    throw new Error(`Request validation failed: ${error.message}`, { cause: error });
  }
}

/**
 * Create API Gateway response
 *
 * @param {number} statusCode - HTTP status code
 * @param {Object} body - Response body
 * @param {Object} [headers={}] - Additional headers
 * @returns {Object} API Gateway response
 *
 * @example
 * ```javascript
 * return createApiResponse(200, { results: data });
 * ```
 */
export function createApiResponse(statusCode, body, headers = {}) {
  return {
    statusCode,
    headers: {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET,POST,PUT,DELETE',
      ...headers,
    },
    body: JSON.stringify(body),
  };
}

/**
 * Create error response
 *
 * @param {Error} error - Error object
 * @param {number} [statusCode=500] - HTTP status code
 * @returns {Object} Error response
 *
 * @example
 * ```javascript
 * catch (error) {
 *   return createErrorResponse(error, 400);
 * }
 * ```
 */
export function createErrorResponse(error, statusCode = 500) {
  return createApiResponse(statusCode, {
    error: error.message,
    type: error.constructor.name,
  });
}
