/**
 * @fileoverview UNRDF CDK Stack - Infrastructure as Code for RDF Applications
 *
 * @description
 * Defines AWS infrastructure for serverless UNRDF deployments including:
 * - Lambda functions for RDF query execution
 * - DynamoDB tables for RDF triple storage
 * - API Gateway for REST endpoints
 * - CloudFront CDN for global distribution
 *
 * @module serverless/cdk/unrdf-stack
 * @version 1.0.0
 * @license MIT
 */

import { Stack, Duration, RemovalPolicy, CfnOutput } from 'aws-cdk-lib';
import { Runtime, Function as LambdaFunction, Code, LayerVersion } from 'aws-cdk-lib/aws-lambda';
import { RestApi, LambdaIntegration, Cors, EndpointType } from 'aws-cdk-lib/aws-apigateway';
import { Table, AttributeType, BillingMode, StreamViewType } from 'aws-cdk-lib/aws-dynamodb';
import { Distribution, OriginAccessIdentity as _OriginAccessIdentity, ViewerProtocolPolicy } from 'aws-cdk-lib/aws-cloudfront';
import { RestApiOrigin } from 'aws-cdk-lib/aws-cloudfront-origins';
import { Policy as _Policy, PolicyStatement, Effect } from 'aws-cdk-lib/aws-iam';
import { z } from 'zod';

/**
 * Configuration schema for UNRDF stack
 * @typedef {Object} UNRDFStackConfig
 * @property {string} environment - Deployment environment (dev, staging, prod)
 * @property {number} memorySizeMb - Lambda memory allocation in MB
 * @property {number} timeoutSeconds - Lambda timeout in seconds
 * @property {boolean} enableCdn - Enable CloudFront CDN
 * @property {boolean} enableStreaming - Enable DynamoDB streams
 */
const StackConfigSchema = z.object({
  environment: z.enum(['dev', 'staging', 'prod']).default('dev'),
  memorySizeMb: z.number().min(128).max(10240).default(1024),
  timeoutSeconds: z.number().min(3).max(900).default(30),
  enableCdn: z.boolean().default(true),
  enableStreaming: z.boolean().default(false),
  tableName: z.string().optional(),
  apiName: z.string().optional(),
});

/**
 * UNRDF Serverless Stack
 *
 * @class UNRDFStack
 * @extends {Stack}
 *
 * @description
 * Creates a complete serverless infrastructure for UNRDF applications with:
 * - Auto-scaling Lambda functions
 * - DynamoDB for persistent RDF storage
 * - API Gateway with CORS support
 * - Optional CloudFront CDN
 *
 * @example
 * ```javascript
 * import { App } from 'aws-cdk-lib';
 * import { UNRDFStack } from '@unrdf/serverless/cdk';
 *
 * const app = new App();
 * new UNRDFStack(app, 'MyUNRDFApp', {
 *   environment: 'prod',
 *   memorySizeMb: 2048,
 *   enableCdn: true
 * });
 * ```
 */
export class UNRDFStack extends Stack {
  /**
   * DynamoDB table for RDF triples
   * @type {Table}
   */
  triplesTable;

  /**
   * Lambda function for query execution
   * @type {LambdaFunction}
   */
  queryFunction;

  /**
   * API Gateway REST API
   * @type {RestApi}
   */
  api;

  /**
   * CloudFront distribution (optional)
   * @type {Distribution|null}
   */
  distribution = null;

  /**
   * Create UNRDF Stack
   *
   * @param {import('constructs').Construct} scope - CDK scope
   * @param {string} id - Stack ID
   * @param {Object} props - Stack properties
   * @param {Object} [props.config] - UNRDF configuration
   */
  constructor(scope, id, props = {}) {
    super(scope, id, props);

    // Validate configuration
    const config = StackConfigSchema.parse(props.config || {});

    // Create DynamoDB table for RDF triples
    this.triplesTable = this.createTriplesTable(config);

    // Create Lambda layer with dependencies
    const layer = this.createDependencyLayer();

    // Create Lambda functions
    this.queryFunction = this.createQueryFunction(config, layer);
    this.createIngestFunction(config, layer);

    // Grant DynamoDB access to Lambda
    this.triplesTable.grantReadWriteData(this.queryFunction);

    // Create API Gateway
    this.api = this.createApiGateway(config);

    // Create CloudFront CDN if enabled
    if (config.enableCdn) {
      this.distribution = this.createCdnDistribution();
    }

    // Output deployment information
    this.createOutputs(config);
  }

  /**
   * Create DynamoDB table for RDF triple storage
   *
   * @private
   * @param {Object} config - Stack configuration
   * @returns {Table} DynamoDB table
   */
  createTriplesTable(config) {
    const table = new Table(this, 'TriplesTable', {
      tableName: config.tableName || `unrdf-triples-${config.environment}`,
      partitionKey: { name: 'subject', type: AttributeType.STRING },
      sortKey: { name: 'predicate_object', type: AttributeType.STRING },
      billingMode: BillingMode.PAY_PER_REQUEST,
      stream: config.enableStreaming ? StreamViewType.NEW_AND_OLD_IMAGES : undefined,
      removalPolicy: config.environment === 'prod' ? RemovalPolicy.RETAIN : RemovalPolicy.DESTROY,
      pointInTimeRecovery: config.environment === 'prod',
    });

    // Add GSI for predicate queries
    table.addGlobalSecondaryIndex({
      indexName: 'predicate-index',
      partitionKey: { name: 'predicate', type: AttributeType.STRING },
      sortKey: { name: 'subject_object', type: AttributeType.STRING },
    });

    // Add GSI for object queries
    table.addGlobalSecondaryIndex({
      indexName: 'object-index',
      partitionKey: { name: 'object', type: AttributeType.STRING },
      sortKey: { name: 'subject_predicate', type: AttributeType.STRING },
    });

    return table;
  }

  /**
   * Create Lambda layer with UNRDF dependencies
   *
   * @private
   * @returns {LayerVersion} Lambda layer
   */
  createDependencyLayer() {
    return new LayerVersion(this, 'UNRDFLayer', {
      code: Code.fromAsset('dist/layer'),
      compatibleRuntimes: [Runtime.NODEJS_20_X],
      description: 'UNRDF core dependencies and utilities',
    });
  }

  /**
   * Create Lambda function for SPARQL query execution
   *
   * @private
   * @param {Object} config - Stack configuration
   * @param {LayerVersion} layer - Dependency layer
   * @returns {LambdaFunction} Query function
   */
  createQueryFunction(config, layer) {
    const fn = new LambdaFunction(this, 'QueryFunction', {
      functionName: `unrdf-query-${config.environment}`,
      runtime: Runtime.NODEJS_20_X,
      handler: 'index.handler',
      code: Code.fromAsset('dist/lambda/query'),
      layers: [layer],
      memorySize: config.memorySizeMb,
      timeout: Duration.seconds(config.timeoutSeconds),
      environment: {
        TRIPLES_TABLE: this.triplesTable.tableName,
        ENVIRONMENT: config.environment,
        NODE_ENV: 'production',
      },
      reservedConcurrentExecutions: config.environment === 'prod' ? 100 : 10,
    });

    // Add X-Ray tracing
    fn.addToRolePolicy(
      new PolicyStatement({
        effect: Effect.ALLOW,
        actions: ['xray:PutTraceSegments', 'xray:PutTelemetryRecords'],
        resources: ['*'],
      })
    );

    return fn;
  }

  /**
   * Create Lambda function for RDF data ingestion
   *
   * @private
   * @param {Object} config - Stack configuration
   * @param {LayerVersion} layer - Dependency layer
   * @returns {LambdaFunction} Ingest function
   */
  createIngestFunction(config, layer) {
    const fn = new LambdaFunction(this, 'IngestFunction', {
      functionName: `unrdf-ingest-${config.environment}`,
      runtime: Runtime.NODEJS_20_X,
      handler: 'index.handler',
      code: Code.fromAsset('dist/lambda/ingest'),
      layers: [layer],
      memorySize: config.memorySizeMb * 2, // Double memory for batch operations
      timeout: Duration.seconds(Math.min(config.timeoutSeconds * 2, 900)),
      environment: {
        TRIPLES_TABLE: this.triplesTable.tableName,
        ENVIRONMENT: config.environment,
        BATCH_SIZE: '100',
      },
    });

    this.triplesTable.grantReadWriteData(fn);

    return fn;
  }

  /**
   * Create API Gateway REST API
   *
   * @private
   * @param {Object} config - Stack configuration
   * @returns {RestApi} API Gateway
   */
  createApiGateway(config) {
    const api = new RestApi(this, 'UNRDFApi', {
      restApiName: config.apiName || `unrdf-api-${config.environment}`,
      description: 'UNRDF Serverless API for RDF operations',
      endpointTypes: [EndpointType.REGIONAL],
      defaultCorsPreflightOptions: {
        allowOrigins: Cors.ALL_ORIGINS,
        allowMethods: Cors.ALL_METHODS,
        allowHeaders: ['Content-Type', 'Authorization'],
      },
      deployOptions: {
        stageName: config.environment,
        tracingEnabled: true,
        metricsEnabled: true,
      },
    });

    // Add /query endpoint
    const queryResource = api.root.addResource('query');
    queryResource.addMethod('POST', new LambdaIntegration(this.queryFunction), {
      apiKeyRequired: config.environment === 'prod',
    });

    // Add /health endpoint
    const healthResource = api.root.addResource('health');
    healthResource.addMethod('GET', new LambdaIntegration(this.queryFunction));

    return api;
  }

  /**
   * Create CloudFront CDN distribution
   *
   * @private
   * @returns {Distribution} CloudFront distribution
   */
  createCdnDistribution() {
    const distribution = new Distribution(this, 'UNRDFDistribution', {
      defaultBehavior: {
        origin: new RestApiOrigin(this.api),
        viewerProtocolPolicy: ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
        compress: true,
      },
      comment: 'UNRDF API CDN distribution',
    });

    return distribution;
  }

  /**
   * Create CloudFormation outputs
   *
   * @private
   * @param {Object} config - Stack configuration
   */
  createOutputs(config) {
    new CfnOutput(this, 'ApiEndpoint', {
      value: this.api.url,
      description: 'API Gateway endpoint URL',
      exportName: `unrdf-api-endpoint-${config.environment}`,
    });

    new CfnOutput(this, 'TriplesTableName', {
      value: this.triplesTable.tableName,
      description: 'DynamoDB triples table name',
      exportName: `unrdf-triples-table-${config.environment}`,
    });

    new CfnOutput(this, 'QueryFunctionArn', {
      value: this.queryFunction.functionArn,
      description: 'Query Lambda function ARN',
      exportName: `unrdf-query-function-${config.environment}`,
    });

    if (this.distribution) {
      new CfnOutput(this, 'DistributionDomain', {
        value: this.distribution.distributionDomainName,
        description: 'CloudFront distribution domain',
        exportName: `unrdf-cdn-domain-${config.environment}`,
      });
    }
  }
}

/**
 * Create UNRDF stack from configuration
 *
 * @param {import('constructs').Construct} scope - CDK app
 * @param {string} id - Stack identifier
 * @param {Object} config - Stack configuration
 * @returns {UNRDFStack} Initialized stack
 *
 * @example
 * ```javascript
 * const stack = createUNRDFStack(app, 'Production', {
 *   environment: 'prod',
 *   memorySizeMb: 2048,
 *   enableCdn: true
 * });
 * ```
 */
export function createUNRDFStack(scope, id, config) {
  return new UNRDFStack(scope, id, { config });
}
