/**
 * @fileoverview Message Validator - Zod-based validation for distributed BEAM <-> JS messages
 * @module message-validator
 *
 * @description
 * Provides schema validation for all distributed message types used in:
 * - RPC calls between BEAM and JavaScript
 * - Triple pattern queries
 * - SPARQL query payloads
 * - Distributed system responses
 *
 * Uses Zod for runtime validation with clear error messages suitable for
 * logging and debugging distributed system issues.
 *
 * @example
 * import { validateRPCCall, validateTriplePattern } from './message-validator.mjs';
 *
 * // Validate incoming RPC message
 * const result = validateRPCCall({
 *   target: 'node1',
 *   module: 'graph',
 *   function: 'query',
 *   args: ['SELECT * WHERE { ?s ?p ?o }']
 * });
 *
 * if (!result.success) {
 *   console.error('Invalid RPC call:', result.error);
 * }
 */

import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';

/**
 * OTEL tracer for validation failure logging
 * @type {import('@opentelemetry/api').Tracer}
 */
const tracer = trace.getTracer('unrdf-message-validator');

// ============================================================================
// Message Schemas
// ============================================================================

/**
 * Schema for RDF triple pattern queries
 * All fields are optional to support wildcard patterns (null = any match)
 *
 * @type {z.ZodObject}
 * @example
 * // Valid patterns:
 * { s: 'http://example.org/alice', p: undefined, o: undefined } // All triples with subject alice
 * { s: undefined, p: 'http://xmlns.com/foaf/0.1/name', o: undefined } // All name predicates
 * {} // All triples (wildcard)
 */
const triplePatternSchema = z
  .object({
    s: z.string().optional().nullable(),
    p: z.string().optional().nullable(),
    o: z.string().optional().nullable(),
  })
  .strict();

/**
 * Schema for RPC call messages between distributed nodes
 * Validates the structure of remote procedure calls
 *
 * @type {z.ZodObject}
 * @example
 * {
 *   target: 'node-replica-1',
 *   module: 'knowledge_graph',
 *   function: 'insert_triple',
 *   args: [{ s: '...', p: '...', o: '...' }]
 * }
 */
const rpcCallSchema = z
  .object({
    target: z.string().min(1, 'Target node identifier is required'),
    module: z.string().min(1, 'Module name is required'),
    function: z.string().min(1, 'Function name is required'),
    args: z.array(z.any()).default([]),
  })
  .strict();

/**
 * Schema for RPC result messages
 * Represents the response from a remote procedure call
 *
 * @type {z.ZodObject}
 * @example
 * // Success case:
 * { ok: true, result: { count: 42 } }
 *
 * // Error case:
 * { ok: false, result: null, error: 'Connection timeout to node-2' }
 */
const rpcResultSchema = z
  .object({
    ok: z.boolean(),
    result: z.any(),
    error: z.string().optional().nullable(),
  })
  .strict()
  .refine(
    data => {
      // If not ok, error should be present
      if (!data.ok && !data.error) {
        return false;
      }
      return true;
    },
    {
      message: 'Error message is required when ok is false',
      path: ['error'],
    }
  );

/**
 * Schema for SPARQL query messages
 * Validates SPARQL queries with optional parameter bindings
 *
 * @type {z.ZodObject}
 * @example
 * {
 *   query: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT $limit',
 *   params: { limit: 100 }
 * }
 */
const sparqlQuerySchema = z
  .object({
    query: z.string().min(1, 'SPARQL query string is required'),
    params: z.record(z.string(), z.unknown()).optional().nullable(),
  })
  .strict();

/**
 * Schema for batch triple operations
 * Validates bulk insert/delete operations
 *
 * @type {z.ZodObject}
 */
const batchOperationSchema = z
  .object({
    operation: z.enum(['insert', 'delete', 'update']),
    triples: z.array(triplePatternSchema).min(1, 'At least one triple is required'),
    transactionId: z.string().optional().nullable(),
  })
  .strict();

/**
 * Schema for distributed node health check messages
 *
 * @type {z.ZodObject}
 */
const healthCheckSchema = z
  .object({
    nodeId: z.string().min(1),
    timestamp: z.number().int().positive(),
    status: z.enum(['healthy', 'degraded', 'unhealthy']),
    metrics: z
      .object({
        latency: z.number().nonnegative().optional(),
        errorRate: z.number().min(0).max(1).optional(),
        queueDepth: z.number().int().nonnegative().optional(),
      })
      .optional(),
  })
  .strict();

/**
 * Collection of all message schemas for external access
 * @type {Object}
 */
export const messageSchemas = Object.freeze({
  triplePattern: triplePatternSchema,
  rpcCall: rpcCallSchema,
  rpcResult: rpcResultSchema,
  sparqlQuery: sparqlQuerySchema,
  batchOperation: batchOperationSchema,
  healthCheck: healthCheckSchema,
});

// ============================================================================
// Validation Error Formatting
// ============================================================================

/**
 * Format Zod error for logging and debugging
 *
 * @param {z.ZodError} zodError - Zod validation error
 * @returns {string} Human-readable error message
 * @private
 */
function formatValidationError(zodError) {
  if (!zodError || !zodError.issues) {
    return 'Unknown validation error';
  }

  const issues = zodError.issues.map(issue => {
    const path = issue.path.length > 0 ? `[${issue.path.join('.')}]` : '[root]';
    return `${path}: ${issue.message}`;
  });

  return issues.join('; ');
}

/**
 * Validation result type
 * @typedef {Object} ValidationResult
 * @property {boolean} success - Whether validation passed
 * @property {T} [data] - Validated and typed data (if success)
 * @property {string} [error] - Error message (if failure)
 * @property {z.ZodError} [zodError] - Original Zod error (if failure)
 * @template T
 */

// ============================================================================
// Validation Functions
// ============================================================================

/**
 * Log validation failure to OTEL
 *
 * @param {string} schemaType - Type of schema being validated
 * @param {unknown} data - Data that failed validation
 * @param {string} errorMessage - Formatted error message
 * @private
 */
function logValidationFailure(schemaType, data, errorMessage) {
  tracer.startActiveSpan(`validation.failure.${schemaType}`, span => {
    span.setAttributes({
      'validation.schema_type': schemaType,
      'validation.error': errorMessage,
      'validation.data_type': typeof data,
      'validation.data_preview': JSON.stringify(data).slice(0, 200),
    });
    span.setStatus({
      code: SpanStatusCode.ERROR,
      message: `Validation failed: ${errorMessage}`,
    });
    span.end();
  });
}

/**
 * Validate a triple pattern query
 *
 * @param {unknown} data - Data to validate
 * @returns {ValidationResult<{s?: string|null, p?: string|null, o?: string|null}>}
 *
 * @example
 * const result = validateTriplePattern({ s: 'http://example.org/alice' });
 * if (result.success) {
 *   const { s, p, o } = result.data;
 * }
 */
export function validateTriplePattern(data) {
  const parseResult = triplePatternSchema.safeParse(data);

  if (parseResult.success) {
    return {
      success: true,
      data: parseResult.data,
    };
  }

  const errorMessage = formatValidationError(parseResult.error);
  logValidationFailure('triplePattern', data, errorMessage);

  return {
    success: false,
    error: errorMessage,
    zodError: parseResult.error,
  };
}

/**
 * Validate an RPC call message
 *
 * @param {unknown} data - Data to validate
 * @returns {ValidationResult<{target: string, module: string, function: string, args: any[]}>}
 *
 * @example
 * const result = validateRPCCall({
 *   target: 'node1',
 *   module: 'graph',
 *   function: 'query',
 *   args: []
 * });
 */
export function validateRPCCall(data) {
  const parseResult = rpcCallSchema.safeParse(data);

  if (parseResult.success) {
    return {
      success: true,
      data: parseResult.data,
    };
  }

  const errorMessage = formatValidationError(parseResult.error);
  logValidationFailure('rpcCall', data, errorMessage);

  return {
    success: false,
    error: errorMessage,
    zodError: parseResult.error,
  };
}

/**
 * Validate an RPC result message
 *
 * @param {unknown} data - Data to validate
 * @returns {ValidationResult<{ok: boolean, result: any, error?: string|null}>}
 *
 * @example
 * const result = validateRPCResult({ ok: true, result: { count: 10 } });
 */
export function validateRPCResult(data) {
  const parseResult = rpcResultSchema.safeParse(data);

  if (parseResult.success) {
    return {
      success: true,
      data: parseResult.data,
    };
  }

  const errorMessage = formatValidationError(parseResult.error);
  logValidationFailure('rpcResult', data, errorMessage);

  return {
    success: false,
    error: errorMessage,
    zodError: parseResult.error,
  };
}

/**
 * Validate a SPARQL query message
 *
 * @param {unknown} data - Data to validate
 * @returns {ValidationResult<{query: string, params?: Record<string, any>|null}>}
 *
 * @example
 * const result = validateSPARQLQuery({
 *   query: 'SELECT * WHERE { ?s ?p ?o }',
 *   params: { limit: 100 }
 * });
 */
export function validateSPARQLQuery(data) {
  const parseResult = sparqlQuerySchema.safeParse(data);

  if (parseResult.success) {
    return {
      success: true,
      data: parseResult.data,
    };
  }

  const errorMessage = formatValidationError(parseResult.error);
  logValidationFailure('sparqlQuery', data, errorMessage);

  return {
    success: false,
    error: errorMessage,
    zodError: parseResult.error,
  };
}

/**
 * Validate a batch operation message
 *
 * @param {unknown} data - Data to validate
 * @returns {ValidationResult<{operation: string, triples: Array, transactionId?: string|null}>}
 *
 * @example
 * const result = validateBatchOperation({
 *   operation: 'insert',
 *   triples: [{ s: 'http://example.org/alice', p: 'http://xmlns.com/foaf/0.1/name', o: 'Alice' }]
 * });
 */
export function validateBatchOperation(data) {
  const parseResult = batchOperationSchema.safeParse(data);

  if (parseResult.success) {
    return {
      success: true,
      data: parseResult.data,
    };
  }

  const errorMessage = formatValidationError(parseResult.error);
  logValidationFailure('batchOperation', data, errorMessage);

  return {
    success: false,
    error: errorMessage,
    zodError: parseResult.error,
  };
}

/**
 * Validate a health check message
 *
 * @param {unknown} data - Data to validate
 * @returns {ValidationResult<{nodeId: string, timestamp: number, status: string, metrics?: Object}>}
 *
 * @example
 * const result = validateHealthCheck({
 *   nodeId: 'node-1',
 *   timestamp: Date.now(),
 *   status: 'healthy'
 * });
 */
export function validateHealthCheck(data) {
  const parseResult = healthCheckSchema.safeParse(data);

  if (parseResult.success) {
    return {
      success: true,
      data: parseResult.data,
    };
  }

  const errorMessage = formatValidationError(parseResult.error);
  logValidationFailure('healthCheck', data, errorMessage);

  return {
    success: false,
    error: errorMessage,
    zodError: parseResult.error,
  };
}

// ============================================================================
// Generic Validation
// ============================================================================

/**
 * Validate data against any registered schema
 *
 * @param {string} schemaType - Schema type key from messageSchemas
 * @param {unknown} data - Data to validate
 * @returns {ValidationResult<any>}
 *
 * @example
 * const result = validateMessage('rpcCall', { target: 'node1', ... });
 */
export function validateMessage(schemaType, data) {
  const schema = messageSchemas[schemaType];

  if (!schema) {
    return {
      success: false,
      error: `Unknown schema type: ${schemaType}. Available: ${Object.keys(messageSchemas).join(', ')}`,
    };
  }

  const parseResult = schema.safeParse(data);

  if (parseResult.success) {
    return {
      success: true,
      data: parseResult.data,
    };
  }

  const errorMessage = formatValidationError(parseResult.error);
  logValidationFailure(schemaType, data, errorMessage);

  return {
    success: false,
    error: errorMessage,
    zodError: parseResult.error,
  };
}

// ============================================================================
// Strict Validation (Throws on Error)
// ============================================================================

/**
 * Validate triple pattern strictly (throws on error)
 *
 * @param {unknown} data - Data to validate
 * @returns {{s?: string|null, p?: string|null, o?: string|null}} Validated data
 * @throws {Error} If validation fails
 */
export function parseTriplePattern(data) {
  const result = validateTriplePattern(data);
  if (!result.success) {
    throw new Error(`Invalid triple pattern: ${result.error}`);
  }
  return result.data;
}

/**
 * Validate RPC call strictly (throws on error)
 *
 * @param {unknown} data - Data to validate
 * @returns {{target: string, module: string, function: string, args: any[]}} Validated data
 * @throws {Error} If validation fails
 */
export function parseRPCCall(data) {
  const result = validateRPCCall(data);
  if (!result.success) {
    throw new Error(`Invalid RPC call: ${result.error}`);
  }
  return result.data;
}

/**
 * Validate RPC result strictly (throws on error)
 *
 * @param {unknown} data - Data to validate
 * @returns {{ok: boolean, result: any, error?: string|null}} Validated data
 * @throws {Error} If validation fails
 */
export function parseRPCResult(data) {
  const result = validateRPCResult(data);
  if (!result.success) {
    throw new Error(`Invalid RPC result: ${result.error}`);
  }
  return result.data;
}

/**
 * Validate SPARQL query strictly (throws on error)
 *
 * @param {unknown} data - Data to validate
 * @returns {{query: string, params?: Record<string, any>|null}} Validated data
 * @throws {Error} If validation fails
 */
export function parseSPARQLQuery(data) {
  const result = validateSPARQLQuery(data);
  if (!result.success) {
    throw new Error(`Invalid SPARQL query: ${result.error}`);
  }
  return result.data;
}

// ============================================================================
// Integration Helper for Circuit Breaker
// ============================================================================

/**
 * Create a validation wrapper for circuit breaker integration
 * Validates incoming messages before execution
 *
 * @param {string} schemaType - Schema type to validate against
 * @returns {Function} Validation middleware function
 *
 * @example
 * import { CircuitBreaker } from './orchestration/circuit-breaker.mjs';
 * import { createValidationMiddleware } from './message-validator.mjs';
 *
 * const breaker = new CircuitBreaker({ name: 'rpc-handler' });
 * const validateRPC = createValidationMiddleware('rpcCall');
 *
 * async function handleRPCCall(message) {
 *   const validated = validateRPC(message);
 *   return await breaker.execute(() => processRPC(validated));
 * }
 */
export function createValidationMiddleware(schemaType) {
  const schema = messageSchemas[schemaType];

  if (!schema) {
    throw new Error(
      `Unknown schema type: ${schemaType}. Available: ${Object.keys(messageSchemas).join(', ')}`
    );
  }

  return function validateAndParse(data) {
    const result = validateMessage(schemaType, data);
    if (!result.success) {
      throw new Error(`Validation failed for ${schemaType}: ${result.error}`);
    }
    return result.data;
  };
}

/**
 * Wrap a function with message validation
 * Validates the first argument before calling the wrapped function
 *
 * @param {Function} fn - Function to wrap
 * @param {string} schemaType - Schema type to validate against
 * @returns {Function} Wrapped function with validation
 *
 * @example
 * const processRPC = withValidation(
 *   async (call) => {
 *     // call is guaranteed to be valid
 *     return await executeRPC(call);
 *   },
 *   'rpcCall'
 * );
 */
export function withValidation(fn, schemaType) {
  const validate = createValidationMiddleware(schemaType);

  return async function validatedFunction(data, ...rest) {
    const validated = validate(data);
    return await fn(validated, ...rest);
  };
}
