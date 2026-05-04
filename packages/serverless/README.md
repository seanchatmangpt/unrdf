# @unrdf/serverless

> One-click serverless deployment of UNRDF applications to AWS

## Overview

`@unrdf/serverless` provides infrastructure-as-code and deployment tooling for running UNRDF RDF applications on AWS Lambda with:

- **AWS CDK** - Infrastructure as code with TypeScript/JavaScript
- **Lambda Functions** - Auto-scaling compute for RDF queries
- **DynamoDB** - Persistent triple storage with optimized indexes
- **API Gateway** - REST endpoints with CORS and rate limiting
- **CloudFront CDN** - Global distribution (optional)
- **esbuild** - Optimized bundling for minimal cold starts

## Installation

```bash
pnpm add @unrdf/serverless
```

### Peer Dependencies

```bash
pnpm add @aws-sdk/client-dynamodb @aws-sdk/client-lambda
```

## Quick Start

### 1. Create CDK Stack

```javascript
import { App } from 'aws-cdk-lib';
import { createUNRDFStack } from '@unrdf/serverless';

const app = new App();

const stack = createUNRDFStack(app, 'MyUNRDFApp', {
  environment: 'prod',
  memorySizeMb: 2048,
  timeoutSeconds: 30,
  enableCdn: true,
});

app.synth();
```

### 2. Bundle Lambda Functions

```javascript
import { LambdaBundler } from '@unrdf/serverless';

const bundler = new LambdaBundler({
  entryPoint: './src/handler.mjs',
  outDir: './dist/lambda',
  minify: true,
});

const metadata = await bundler.bundle();
console.log(`Bundle size: ${metadata.sizeBytes} bytes`);
```

### 3. Configure API Gateway

```javascript
import { ApiGatewayConfig } from '@unrdf/serverless';

const config = new ApiGatewayConfig('my-api', 'prod')
  .addEndpoint('/query', 'POST', 'queryFunction', {
    authRequired: true,
    rateLimit: 100,
  })
  .addEndpoint('/triples', 'GET', 'queryFunction')
  .enableCors(['https://example.com'])
  .build();
```

### 4. Deploy

```bash
# Synthesize CloudFormation template
pnpm cdk:synth

# Deploy to AWS
pnpm cdk:deploy

# Destroy infrastructure
pnpm cdk:destroy
```

## Architecture

```
┌─────────────────┐
│   CloudFront    │ (Optional CDN)
│      CDN        │
└────────┬────────┘
         │
┌────────▼────────┐
│  API Gateway    │ (REST API)
└────────┬────────┘
         │
    ┌────┴─────┬─────────┐
    │          │         │
┌───▼──┐   ┌──▼──┐  ┌──▼──┐
│Query │   │List │  │Ingest│  (Lambda Functions)
│Lambda│   │Lambda│ │Lambda│
└───┬──┘   └──┬──┘  └──┬──┘
    │         │        │
    └─────────┼────────┘
              │
       ┌──────▼──────┐
       │  DynamoDB   │ (Triple Store)
       │   Table     │
       └─────────────┘
         (SPO, PSO, OSP indexes)
```

## Features

### CDK Infrastructure

- **Auto-scaling Lambda** - Concurrent execution limits
- **DynamoDB tables** - Optimized for triple patterns
- **Global Secondary Indexes** - Fast predicate/object queries
- **CloudFront CDN** - Global edge caching
- **X-Ray tracing** - Built-in observability

### Lambda Bundling

- **esbuild integration** - Fast, optimized builds
- **Tree-shaking** - Minimal bundle sizes
- **Automatic externals** - AWS SDK excluded by default
- **Source maps** - Optional debugging support
- **Gzip compression** - Deployment optimization

### API Gateway

- **REST endpoints** - Full CRUD operations
- **CORS support** - Configurable origins
- **Rate limiting** - Per-endpoint throttling
- **Authentication** - API key integration
- **OpenAPI export** - Auto-generated specs

### DynamoDB Storage

- **Triple patterns** - SPO, PSO, OSP queries
- **Global indexes** - Optimized access patterns
- **Batch operations** - Bulk import support
- **Pagination** - Large result sets
- **Point-in-time recovery** - Production safety

## Examples

### Deploy Demo (Dry-Run)

```bash
# Run deployment demo without AWS
node examples/deploy-demo.mjs --dry-run

# Deploy to AWS (requires credentials)
node examples/deploy-demo.mjs --deploy --env=prod
```

### Custom Stack Configuration

```javascript
import { UNRDFStack } from '@unrdf/serverless/cdk';

class CustomStack extends UNRDFStack {
  constructor(scope, id, props) {
    super(scope, id, {
      config: {
        environment: 'staging',
        memorySizeMb: 1536,
        timeoutSeconds: 60,
        enableStreaming: true, // Enable DynamoDB streams
      },
      ...props,
    });

    // Add custom resources
    this.addCustomMetrics();
  }

  addCustomMetrics() {
    // Custom CloudWatch metrics, alarms, etc.
  }
}
```

### Lambda Handler Example

```javascript
import { createAdapterFromEnv, createApiResponse, createErrorResponse } from '@unrdf/serverless';

export async function handler(event) {
  try {
    const adapter = createAdapterFromEnv();
    const body = JSON.parse(event.body);

    const results = await adapter.queryTriples({
      subject: body.subject,
      predicate: body.predicate,
    });

    return createApiResponse(200, { results, count: results.length });
  } catch (error) {
    return createErrorResponse(error, 500);
  }
}
```

### Bundle Analysis

```javascript
import { LambdaBundler } from '@unrdf/serverless';

const analysis = await LambdaBundler.analyzeBundleSize('./dist/metafile.json');

console.log('Bundle Analysis:');
console.log(`Total size: ${analysis.totalSizeBytes} bytes`);
console.log('Largest dependencies:');
for (const dep of analysis.largestDeps) {
  console.log(`  ${dep.name}: ${dep.bytes} bytes (${dep.percentage}%)`);
}
```

## Configuration

### Environment Variables (Lambda)

- `TRIPLES_TABLE` - DynamoDB table name (auto-set by CDK)
- `ENVIRONMENT` - Deployment environment (dev, staging, prod)
- `NODE_ENV` - Node environment (production)
- `BATCH_SIZE` - Batch size for bulk operations (default: 100)

### CDK Stack Options

```typescript
interface UNRDFStackConfig {
  environment: 'dev' | 'staging' | 'prod';
  memorySizeMb: number; // 128-10240
  timeoutSeconds: number; // 3-900
  enableCdn: boolean; // CloudFront
  enableStreaming: boolean; // DynamoDB streams
  tableName?: string; // Custom table name
  apiName?: string; // Custom API name
}
```

## Performance

### Cold Start Optimization

- **Minimal bundles** - Tree-shaking reduces size by 70%+
- **No AWS SDK** - Excluded from bundle (provided by Lambda)
- **ES modules** - Faster parsing than CommonJS
- **Provisioned concurrency** - Optional for production

### Query Optimization

- **GSI indexes** - Sub-100ms predicate/object queries
- **Batch operations** - 25 items per DynamoDB batch
- **Pagination** - Cursor-based for large results
- **Caching** - CloudFront edge caching (optional)

### Cost Optimization

- **Pay-per-request** - DynamoDB on-demand billing
- **Auto-scaling Lambda** - No idle costs
- **Minimal memory** - Right-sized allocations
- **CDN caching** - Reduced Lambda invocations

## Deployment Best Practices

1. **Start with dry-run** - Validate configuration before deploying
2. **Use environments** - Separate dev/staging/prod stacks
3. **Enable CDN for production** - Global performance
4. **Monitor with X-Ray** - Built-in tracing
5. **Set concurrency limits** - Prevent runaway costs
6. **Enable point-in-time recovery** - Production data safety

## Testing

```bash
# Run tests
pnpm test

# With coverage
pnpm test:coverage

# Watch mode
pnpm test:watch
```

## Documentation

- [AWS CDK Documentation](https://docs.aws.amazon.com/cdk/)
- [Lambda Best Practices](https://docs.aws.amazon.com/lambda/latest/dg/best-practices.html)
- [DynamoDB Design Patterns](https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/best-practices.html)

## License

MIT
