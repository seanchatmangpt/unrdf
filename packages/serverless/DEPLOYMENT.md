# UNRDF Serverless Deployment Guide

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Initial Setup](#initial-setup)
3. [Deployment Steps](#deployment-steps)
4. [Configuration](#configuration)
5. [Monitoring](#monitoring)
6. [Troubleshooting](#troubleshooting)

## Prerequisites

### AWS Account Setup

1. **AWS Account** - Active AWS account with billing enabled
2. **AWS CLI** - Version 2.x installed and configured
3. **Node.js** - Version 18.x or higher
4. **pnpm** - Version 8.x or higher

### Install AWS CLI

```bash
# macOS
brew install awscli

# Linux
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install

# Verify installation
aws --version
```

### Configure AWS Credentials

```bash
# Configure default profile
aws configure

# Or use environment variables
export AWS_ACCESS_KEY_ID=your_access_key
export AWS_SECRET_ACCESS_KEY=your_secret_key
export AWS_DEFAULT_REGION=us-east-1
```

### Install CDK CLI

```bash
npm install -g aws-cdk

# Verify installation
cdk --version

# Bootstrap CDK (first time only)
cdk bootstrap aws://ACCOUNT-ID/REGION
```

## Initial Setup

### 1. Install Dependencies

```bash
cd packages/serverless
pnpm install
```

### 2. Create Lambda Handlers

Create your Lambda function handlers in `src/lambda/`:

```javascript
// src/lambda/query/index.mjs
import { createAdapterFromEnv, createApiResponse, createErrorResponse } from '@unrdf/serverless';

export async function handler(event) {
  try {
    const adapter = createAdapterFromEnv();
    const body = JSON.parse(event.body || '{}');

    const results = await adapter.queryTriples({
      subject: body.subject,
      predicate: body.predicate,
      object: body.object,
    });

    return createApiResponse(200, {
      results,
      count: results.length,
    });
  } catch (error) {
    return createErrorResponse(error, 500);
  }
}
```

### 3. Create CDK App

Create `app.mjs` in the package root:

```javascript
#!/usr/bin/env node
import { App } from 'aws-cdk-lib';
import { createUNRDFStack } from './src/cdk/unrdf-stack.mjs';

const app = new App();

// Development environment
createUNRDFStack(app, 'UNRDF-Dev', {
  environment: 'dev',
  memorySizeMb: 1024,
  timeoutSeconds: 30,
  enableCdn: false,
});

// Production environment
createUNRDFStack(app, 'UNRDF-Prod', {
  environment: 'prod',
  memorySizeMb: 2048,
  timeoutSeconds: 30,
  enableCdn: true,
});

app.synth();
```

## Deployment Steps

### Step 1: Bundle Lambda Functions

```bash
# Create handlers first
mkdir -p src/lambda/{query,ingest}

# Bundle all functions
node -e "
import { bundleUNRDFFunctions } from './src/deploy/lambda-bundler.mjs';
const results = await bundleUNRDFFunctions({ minify: true });
for (const [name, metadata] of results) {
  console.log(\`\${name}: \${metadata.sizeBytes} bytes\`);
}
"
```

### Step 2: Synthesize CloudFormation

```bash
# Generate CloudFormation template
cdk synth

# Review changes (before first deploy)
cdk diff
```

### Step 3: Deploy to AWS

```bash
# Deploy development environment
cdk deploy UNRDF-Dev

# Deploy production environment
cdk deploy UNRDF-Prod --require-approval never

# Deploy all stacks
cdk deploy --all
```

### Step 4: Verify Deployment

```bash
# Get stack outputs
aws cloudformation describe-stacks \
  --stack-name UNRDF-Dev \
  --query 'Stacks[0].Outputs'

# Test API endpoint
API_URL=$(aws cloudformation describe-stacks \
  --stack-name UNRDF-Dev \
  --query 'Stacks[0].Outputs[?OutputKey==`ApiEndpoint`].OutputValue' \
  --output text)

curl -X POST $API_URL/query \
  -H "Content-Type: application/json" \
  -d '{"subject": "http://example.org/alice"}'
```

## Configuration

### Environment-Specific Settings

#### Development

```javascript
{
  environment: 'dev',
  memorySizeMb: 1024,      // Lower memory for cost savings
  timeoutSeconds: 30,
  enableCdn: false,        // No CDN needed for dev
  enableStreaming: false,
}
```

#### Staging

```javascript
{
  environment: 'staging',
  memorySizeMb: 1536,
  timeoutSeconds: 30,
  enableCdn: false,
  enableStreaming: true,   // Test streaming features
}
```

#### Production

```javascript
{
  environment: 'prod',
  memorySizeMb: 2048,      // Higher memory for performance
  timeoutSeconds: 30,
  enableCdn: true,         // CloudFront for global access
  enableStreaming: true,
}
```

### API Gateway Configuration

```javascript
import { createDefaultApiConfig } from '@unrdf/serverless/api';

const apiConfig = createDefaultApiConfig('prod');

// Customize endpoints
apiConfig.endpoints.push({
  path: '/admin/stats',
  method: 'GET',
  functionName: 'statsFunction',
  authRequired: true,
  rateLimit: 10,
});
```

### DynamoDB Configuration

The CDK stack automatically creates:

- **Primary table** - Subject-Predicate-Object storage
- **GSI: predicate-index** - Fast predicate queries
- **GSI: object-index** - Fast object queries
- **Point-in-time recovery** - Production only
- **DynamoDB Streams** - Optional

## Monitoring

### CloudWatch Metrics

```bash
# Lambda invocations
aws cloudwatch get-metric-statistics \
  --namespace AWS/Lambda \
  --metric-name Invocations \
  --dimensions Name=FunctionName,Value=unrdf-query-prod \
  --start-time 2024-01-01T00:00:00Z \
  --end-time 2024-01-01T23:59:59Z \
  --period 3600 \
  --statistics Sum

# API Gateway requests
aws cloudwatch get-metric-statistics \
  --namespace AWS/ApiGateway \
  --metric-name Count \
  --dimensions Name=ApiName,Value=unrdf-api-prod \
  --start-time 2024-01-01T00:00:00Z \
  --end-time 2024-01-01T23:59:59Z \
  --period 3600 \
  --statistics Sum
```

### X-Ray Tracing

```bash
# View traces in AWS Console
open https://console.aws.amazon.com/xray/home

# Get trace summaries
aws xray get-trace-summaries \
  --start-time 2024-01-01T00:00:00Z \
  --end-time 2024-01-01T23:59:59Z
```

### Logs

```bash
# Stream Lambda logs
aws logs tail /aws/lambda/unrdf-query-prod --follow

# Query logs
aws logs filter-log-events \
  --log-group-name /aws/lambda/unrdf-query-prod \
  --filter-pattern "ERROR"
```

## Troubleshooting

### Common Issues

#### 1. CDK Bootstrap Required

**Error:** `This stack uses assets, so the toolkit stack must be deployed`

**Solution:**

```bash
cdk bootstrap aws://ACCOUNT-ID/REGION
```

#### 2. Lambda Function Timeout

**Error:** `Task timed out after 30.00 seconds`

**Solution:** Increase timeout in stack configuration:

```javascript
createUNRDFStack(app, 'UNRDF-Prod', {
  timeoutSeconds: 60, // Increase from 30
});
```

#### 3. DynamoDB Throttling

**Error:** `ProvisionedThroughputExceededException`

**Solution:** Tables use on-demand billing, but check rate limits:

```javascript
// Increase batch size
const BATCH_SIZE = 50; // Reduce from 100
```

#### 4. API Gateway CORS Issues

**Error:** `No 'Access-Control-Allow-Origin' header`

**Solution:** Verify CORS configuration:

```javascript
const config = new ApiGatewayConfig('my-api')
  .enableCors(['https://example.com'], {
    allowMethods: ['GET', 'POST', 'PUT', 'DELETE'],
    allowHeaders: ['Content-Type', 'Authorization'],
  });
```

#### 5. Large Bundle Size

**Issue:** Cold starts >3 seconds

**Solution:** Analyze and optimize bundle:

```javascript
import { LambdaBundler } from '@unrdf/serverless';

const analysis = await LambdaBundler.analyzeBundleSize('./dist/metafile.json');
console.log('Largest dependencies:', analysis.largestDeps);

// Add to external
const bundler = new LambdaBundler({
  external: ['@aws-sdk/*', 'large-dependency'],
});
```

### Debugging

#### Enable Verbose Logging

```javascript
// In Lambda handler
process.env.LOG_LEVEL = 'debug';

console.log('Event:', JSON.stringify(event, null, 2));
console.log('Context:', JSON.stringify(context, null, 2));
```

#### Local Testing

```bash
# Install SAM CLI
brew install aws-sam-cli

# Create test event
cat > event.json <<EOF
{
  "body": "{\"subject\": \"http://example.org/alice\"}"
}
EOF

# Invoke locally
sam local invoke QueryFunction -e event.json
```

## Rollback

### Rollback Deployment

```bash
# Rollback to previous version
cdk deploy UNRDF-Prod --rollback

# Or manually via CloudFormation
aws cloudformation rollback-stack --stack-name UNRDF-Prod
```

### Destroy Stack

```bash
# Destroy development stack
cdk destroy UNRDF-Dev

# Destroy production (requires confirmation)
cdk destroy UNRDF-Prod

# Destroy all stacks
cdk destroy --all
```

## Best Practices

1. **Use separate AWS accounts** - dev/staging/prod isolation
2. **Enable CloudWatch alarms** - Monitor errors and latency
3. **Set up budgets** - Prevent unexpected costs
4. **Use parameter store** - Store secrets securely
5. **Tag all resources** - Track costs by environment
6. **Enable X-Ray** - Trace requests end-to-end
7. **Implement blue-green deployments** - Zero-downtime updates
8. **Regular backups** - DynamoDB point-in-time recovery

## Cost Optimization

### Estimated Monthly Costs (us-east-1)

**Development (low traffic):**

- Lambda: $0-5/month (1M requests, 1GB memory)
- DynamoDB: $0-10/month (on-demand, <1GB storage)
- API Gateway: $0-5/month (1M requests)
- **Total: ~$20/month**

**Production (moderate traffic):**

- Lambda: $50-100/month (100M requests, 2GB memory)
- DynamoDB: $25-50/month (on-demand, 10GB storage)
- API Gateway: $35-50/month (100M requests)
- CloudFront: $10-30/month (100GB transfer)
- **Total: ~$150-250/month**

### Cost Reduction Tips

1. **Use provisioned concurrency sparingly** - Only for critical functions
2. **Enable CloudFront caching** - Reduce Lambda invocations
3. **Optimize Lambda memory** - Right-size allocations
4. **Use reserved capacity** - For predictable workloads
5. **Clean up old logs** - Set retention policies

## Support

- **AWS Support:** https://console.aws.amazon.com/support
- **CDK Documentation:** https://docs.aws.amazon.com/cdk/
- **UNRDF Issues:** https://github.com/unrdf/unrdf/issues
