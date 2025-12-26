#!/usr/bin/env node

/**
 * @fileoverview UNRDF Serverless Deployment Demo
 *
 * @description
 * Demonstrates serverless deployment of UNRDF applications to AWS.
 * Includes dry-run mode for testing without AWS credentials.
 *
 * @example
 * ```bash
 * # Dry-run (no AWS deployment)
 * node examples/deploy-demo.mjs --dry-run
 *
 * # Full deployment (requires AWS credentials)
 * node examples/deploy-demo.mjs --deploy
 * ```
 */

import { App } from 'aws-cdk-lib';
import { createUNRDFStack } from '../src/cdk/unrdf-stack.mjs';
import { LambdaBundler, createDefaultBundlerConfig } from '../src/deploy/lambda-bundler.mjs';
import { ApiGatewayConfig, createDefaultApiConfig } from '../src/api/api-gateway-config.mjs';
import { writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';

/**
 * Deployment configuration
 * @typedef {Object} DeploymentConfig
 * @property {string} environment - Deployment environment
 * @property {boolean} dryRun - Dry-run mode (no actual deployment)
 * @property {boolean} verbose - Verbose logging
 */

/**
 * Parse command line arguments
 *
 * @returns {DeploymentConfig} Configuration
 */
function parseArgs() {
  const args = process.argv.slice(2);
  return {
    environment: args.find((arg) => arg.startsWith('--env='))?.split('=')[1] || 'dev',
    dryRun: args.includes('--dry-run') || !args.includes('--deploy'),
    verbose: args.includes('--verbose'),
  };
}

/**
 * Create demo Lambda handlers
 *
 * @param {string} outputDir - Output directory
 * @returns {Promise<void>}
 */
async function createDemoHandlers(outputDir) {
  // Query handler
  const queryHandler = `
/**
 * SPARQL Query Lambda Handler
 */
export async function handler(event) {
  const body = JSON.parse(event.body || '{}');
  const { query } = body;

  // Simulate query execution
  const results = {
    query,
    bindings: [
      { subject: 'http://example.org/alice', predicate: 'http://xmlns.com/foaf/0.1/name', object: '"Alice"' }
    ],
    count: 1
  };

  return {
    statusCode: 200,
    headers: {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*'
    },
    body: JSON.stringify(results)
  };
}
  `.trim();

  // Ingest handler
  const ingestHandler = `
/**
 * RDF Ingest Lambda Handler
 */
export async function handler(event) {
  const body = JSON.parse(event.body || '{}');
  const { triples } = body;

  // Simulate batch insert
  const inserted = triples?.length || 0;

  return {
    statusCode: 200,
    headers: {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*'
    },
    body: JSON.stringify({ inserted })
  };
}
  `.trim();

  // Create directories
  await mkdir(join(outputDir, 'query'), { recursive: true });
  await mkdir(join(outputDir, 'ingest'), { recursive: true });

  // Write handlers
  await writeFile(join(outputDir, 'query', 'index.mjs'), queryHandler);
  await writeFile(join(outputDir, 'ingest', 'index.mjs'), ingestHandler);
}

/**
 * Bundle Lambda functions
 *
 * @param {boolean} verbose - Verbose logging
 * @returns {Promise<Map<string, Object>>} Bundle results
 */
async function bundleFunctions(verbose) {
  console.log('\n📦 Bundling Lambda functions...');

  const results = new Map();

  for (const fnName of ['query', 'ingest']) {
    try {
      const config = createDefaultBundlerConfig(fnName, { minify: true });
      const bundler = new LambdaBundler(config);
      const metadata = await bundler.bundle();

      results.set(fnName, metadata);

      console.log(`  ✓ ${fnName}: ${metadata.sizeBytes} bytes (gzip: ${metadata.gzipSizeBytes} bytes)`);
      if (verbose) {
        console.log(`    Dependencies: ${metadata.dependencies.join(', ')}`);
        console.log(`    Build time: ${metadata.buildTimeMs}ms`);
      }
    } catch (error) {
      console.error(`  ✗ ${fnName}: ${error.message}`);
    }
  }

  return results;
}

/**
 * Create CDK stack (dry-run)
 *
 * @param {string} environment - Environment name
 * @returns {Object} Stack configuration
 */
function createStackDryRun(environment) {
  console.log(`\n🏗️  Creating CDK stack (${environment})...`);

  const app = new App();

  const stack = createUNRDFStack(app, `UNRDF-${environment}`, {
    environment,
    memorySizeMb: environment === 'prod' ? 2048 : 1024,
    timeoutSeconds: 30,
    enableCdn: environment === 'prod',
    enableStreaming: false,
  });

  console.log('  ✓ Stack created');
  console.log(`    - DynamoDB table: ${stack.triplesTable.tableName}`);
  console.log(`    - Query Lambda: ${stack.queryFunction.functionName}`);
  console.log(`    - API Gateway: ${stack.api.restApiName}`);
  if (stack.distribution) {
    console.log(`    - CloudFront: ${stack.distribution.distributionId}`);
  }

  return {
    tableName: stack.triplesTable.tableName,
    functionName: stack.queryFunction.functionName,
    apiName: stack.api.restApiName,
  };
}

/**
 * Generate API configuration
 *
 * @param {string} environment - Environment name
 * @returns {Object} API configuration
 */
function generateApiConfig(environment) {
  console.log('\n🌐 Generating API Gateway configuration...');

  const config = createDefaultApiConfig(environment);
  console.log(`  ✓ API: ${config.apiName}`);
  console.log(`    - Endpoints: ${config.endpoints.length}`);

  for (const endpoint of config.endpoints) {
    console.log(`      ${endpoint.method} ${endpoint.path} → ${endpoint.functionName}`);
  }

  return config;
}

/**
 * Export OpenAPI specification
 *
 * @param {string} environment - Environment name
 * @returns {Promise<string>} OpenAPI spec path
 */
async function exportOpenAPI(environment) {
  console.log('\n📝 Exporting OpenAPI specification...');

  const apiConfig = new ApiGatewayConfig(`unrdf-api-${environment}`, environment)
    .addEndpoint('/query', 'POST', 'queryFunction', { authRequired: true })
    .addEndpoint('/triples', 'GET', 'queryFunction')
    .addEndpoint('/triples', 'POST', 'ingestFunction', { authRequired: true })
    .addEndpoint('/health', 'GET', 'queryFunction')
    .enableCors(['*']);

  const openapi = apiConfig.toOpenAPI();
  const outputPath = join(process.cwd(), 'dist', 'openapi.json');

  await mkdir(join(process.cwd(), 'dist'), { recursive: true });
  await writeFile(outputPath, JSON.stringify(openapi, null, 2));

  console.log(`  ✓ Exported to: ${outputPath}`);
  return outputPath;
}

/**
 * Generate deployment summary
 *
 * @param {Object} params - Deployment parameters
 * @returns {Promise<void>}
 */
async function generateSummary({ environment, bundleResults, stackConfig, apiConfig }) {
  console.log('\n📊 Deployment Summary');
  console.log('='.repeat(50));
  console.log(`Environment: ${environment}`);
  console.log(`\nInfrastructure:`);
  console.log(`  - DynamoDB: ${stackConfig.tableName}`);
  console.log(`  - Lambda: ${stackConfig.functionName}`);
  console.log(`  - API Gateway: ${stackConfig.apiName}`);

  console.log(`\nLambda Functions:`);
  for (const [name, metadata] of bundleResults) {
    console.log(`  - ${name}:`);
    console.log(`      Size: ${metadata.sizeBytes} bytes`);
    console.log(`      Gzip: ${metadata.gzipSizeBytes} bytes`);
    console.log(`      Build: ${metadata.buildTimeMs}ms`);
  }

  console.log(`\nAPI Endpoints:`);
  for (const endpoint of apiConfig.endpoints) {
    const auth = endpoint.authRequired ? '🔒' : '🔓';
    console.log(`  ${auth} ${endpoint.method} ${endpoint.path}`);
  }

  console.log('='.repeat(50));
}

/**
 * Main deployment function
 */
async function main() {
  const config = parseArgs();

  console.log('🚀 UNRDF Serverless Deployment Demo');
  console.log('='.repeat(50));

  if (config.dryRun) {
    console.log('⚠️  DRY-RUN MODE (no AWS deployment)');
  } else {
    console.log('⚠️  LIVE DEPLOYMENT (requires AWS credentials)');
  }

  try {
    // Step 1: Create demo Lambda handlers
    console.log('\n1️⃣  Creating demo Lambda handlers...');
    await createDemoHandlers('./src/lambda');
    console.log('  ✓ Handlers created');

    // Step 2: Bundle functions
    const bundleResults = await bundleFunctions(config.verbose);

    // Step 3: Create CDK stack
    const stackConfig = createStackDryRun(config.environment);

    // Step 4: Generate API configuration
    const apiConfig = generateApiConfig(config.environment);

    // Step 5: Export OpenAPI spec
    await exportOpenAPI(config.environment);

    // Step 6: Generate summary
    await generateSummary({
      environment: config.environment,
      bundleResults,
      stackConfig,
      apiConfig,
    });

    if (config.dryRun) {
      console.log('\n✅ Dry-run complete! To deploy:');
      console.log('   1. Configure AWS credentials');
      console.log('   2. Run: node examples/deploy-demo.mjs --deploy');
      console.log('   3. Or use: cdk deploy');
    } else {
      console.log('\n✅ Deployment configuration ready!');
      console.log('   Run `cdk deploy` to deploy to AWS');
    }
  } catch (error) {
    console.error('\n❌ Deployment failed:', error.message);
    if (config.verbose) {
      console.error(error);
    }
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { main };
