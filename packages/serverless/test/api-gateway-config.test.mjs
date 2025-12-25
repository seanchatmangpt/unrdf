/**
 * @fileoverview Tests for API Gateway Configuration
 */

import { describe, it, expect } from 'vitest';
import {
  ApiGatewayConfig,
  createDefaultApiConfig,
  validateApiRequest,
  createApiResponse,
  createErrorResponse,
} from '../src/api/api-gateway-config.mjs';
import { z } from 'zod';

describe('ApiGatewayConfig', () => {
  it('creates API config with builder pattern', () => {
    const config = new ApiGatewayConfig('test-api', 'dev')
      .addEndpoint('/query', 'POST', 'queryFunction')
      .addEndpoint('/health', 'GET', 'healthFunction')
      .build();

    expect(config.apiName).toBe('test-api');
    expect(config.stage).toBe('dev');
    expect(config.endpoints).toHaveLength(2);
  });

  it('configures CORS settings', () => {
    const config = new ApiGatewayConfig('test-api')
      .enableCors(['https://example.com'], {
        allowMethods: ['GET', 'POST'],
        maxAge: 7200,
      })
      .build();

    expect(config.cors.allowOrigins).toEqual(['https://example.com']);
    expect(config.cors.maxAge).toBe(7200);
  });

  it('sets rate limiting', () => {
    const config = new ApiGatewayConfig('test-api').setRateLimit(1000, 5000).build();

    expect(config.throttling.rateLimit).toBe(1000);
    expect(config.throttling.burstLimit).toBe(5000);
  });

  it('validates endpoint configuration', () => {
    expect(() => {
      new ApiGatewayConfig('test-api')
        .addEndpoint('/invalid', 'INVALID', 'func') // Invalid method
        .build();
    }).toThrow();
  });

  it('exports OpenAPI specification', () => {
    const config = new ApiGatewayConfig('test-api', 'prod')
      .addEndpoint('/query', 'POST', 'queryFunction', { authRequired: true })
      .addEndpoint('/health', 'GET', 'healthFunction');

    const openapi = config.toOpenAPI();

    expect(openapi.openapi).toBe('3.0.0');
    expect(openapi.info.title).toBe('test-api');
    expect(openapi.paths).toHaveProperty('/query');
    expect(openapi.paths).toHaveProperty('/health');
    expect(openapi.paths['/query'].post.security).toHaveLength(1);
  });
});

describe('createDefaultApiConfig', () => {
  it('creates dev environment config', () => {
    const config = createDefaultApiConfig('dev');

    expect(config.apiName).toBe('unrdf-api-dev');
    expect(config.stage).toBe('dev');
    expect(config.endpoints.length).toBeGreaterThan(0);
  });

  it('creates prod environment config with auth', () => {
    const config = createDefaultApiConfig('prod');

    const queryEndpoint = config.endpoints.find((e) => e.path === '/query');
    expect(queryEndpoint?.authRequired).toBe(true);
  });
});

describe('validateApiRequest', () => {
  it('validates request body against schema', () => {
    const event = {
      body: JSON.stringify({ query: 'SELECT * WHERE { ?s ?p ?o }' }),
    };

    const schema = z.object({
      query: z.string(),
    });

    const validated = validateApiRequest(event, schema);
    expect(validated.query).toBeDefined();
  });

  it('throws on invalid request', () => {
    const event = {
      body: JSON.stringify({ invalid: 'data' }),
    };

    const schema = z.object({
      query: z.string(),
    });

    expect(() => validateApiRequest(event, schema)).toThrow();
  });
});

describe('createApiResponse', () => {
  it('creates successful response', () => {
    const response = createApiResponse(200, { message: 'Success' });

    expect(response.statusCode).toBe(200);
    expect(response.headers['Content-Type']).toBe('application/json');
    expect(JSON.parse(response.body)).toEqual({ message: 'Success' });
  });

  it('includes CORS headers', () => {
    const response = createApiResponse(200, {});

    expect(response.headers['Access-Control-Allow-Origin']).toBe('*');
  });
});

describe('createErrorResponse', () => {
  it('creates error response from Error object', () => {
    const error = new Error('Something went wrong');
    const response = createErrorResponse(error, 400);

    expect(response.statusCode).toBe(400);
    const body = JSON.parse(response.body);
    expect(body.error).toBe('Something went wrong');
    expect(body.type).toBe('Error');
  });

  it('defaults to 500 status code', () => {
    const error = new Error('Internal error');
    const response = createErrorResponse(error);

    expect(response.statusCode).toBe(500);
  });
});
