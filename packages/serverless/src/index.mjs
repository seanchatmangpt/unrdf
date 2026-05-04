/**
 * @fileoverview UNRDF Serverless - One-click AWS deployment for RDF applications
 *
 * @description
 * Serverless deployment toolkit for UNRDF applications with:
 * - AWS CDK infrastructure as code
 * - Lambda function bundling with esbuild
 * - API Gateway REST endpoints
 * - DynamoDB RDF storage
 * - CloudFront CDN integration
 *
 * @module serverless
 * @version 1.0.0
 * @license MIT
 *
 * @example
 * ```javascript
 * import { createUNRDFStack, LambdaBundler, ApiGatewayConfig } from '@unrdf/serverless';
 *
 * // Create CDK stack
 * const stack = createUNRDFStack(app, 'Production', {
 *   environment: 'prod',
 *   memorySizeMb: 2048,
 *   enableCdn: true
 * });
 *
 * // Bundle Lambda functions
 * const bundler = new LambdaBundler({
 *   entryPoint: './src/handler.mjs',
 *   outDir: './dist/lambda'
 * });
 * await bundler.bundle();
 *
 * // Configure API Gateway
 * const apiConfig = new ApiGatewayConfig('my-api')
 *   .addEndpoint('/query', 'POST', 'queryFunction')
 *   .build();
 * ```
 */

// CDK Infrastructure
export { UNRDFStack, createUNRDFStack } from './cdk/unrdf-stack.mjs';

// Lambda Bundling
export {
  LambdaBundler,
  createDefaultBundlerConfig,
  bundleUNRDFFunctions,
} from './deploy/lambda-bundler.mjs';

// API Gateway
export {
  ApiGatewayConfig,
  createDefaultApiConfig,
  validateApiRequest,
  createApiResponse,
  createErrorResponse,
} from './api/api-gateway-config.mjs';

// DynamoDB Storage
export { DynamoDBAdapter, createAdapterFromEnv } from './storage/dynamodb-adapter.mjs';

/**
 * Package version
 * @constant {string}
 */
export const VERSION = '1.0.0';

/**
 * Supported AWS regions
 * @constant {string[]}
 */
export const SUPPORTED_REGIONS = [
  'us-east-1',
  'us-west-2',
  'eu-west-1',
  'eu-central-1',
  'ap-southeast-1',
  'ap-northeast-1',
];

/**
 * Default configuration
 * @constant {Object}
 */
export const DEFAULT_CONFIG = {
  runtime: 'nodejs20.x',
  memorySizeMb: 1024,
  timeoutSeconds: 30,
  environment: 'dev',
};
