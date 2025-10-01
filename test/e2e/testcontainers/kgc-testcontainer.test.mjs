/**
 * @file KGC Sidecar Testcontainer E2E Tests
 * @module test/e2e/testcontainers
 *
 * @description
 * Validates KGC Sidecar + CLI in cleanroom testcontainer environment with
 * OpenTelemetry weaver integration. Implements 80/20 critical scenarios.
 *
 * ⚠️ STATUS: Most tests marked as .todo() pending HTTP API implementation.
 * The core functionality exists in library form but requires an HTTP server
 * wrapper. See docs/testcontainer-strategy.md for implementation plan.
 *
 * @requires testcontainers
 * @requires vitest
 */

import { describe, test, expect, beforeAll, afterAll } from 'vitest';
import { DockerComposeEnvironment, Wait } from 'testcontainers';
import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import http from 'http';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Test configuration
const COMPOSE_FILE = 'docker-compose.yml';
const COMPOSE_DIR = __dirname;
const STARTUP_TIMEOUT = 120000; // 2 minutes

// Service endpoints
let sidecarEndpoint;
let otelCollectorEndpoint;
let jaegerEndpoint;
let prometheusEndpoint;
let giteaEndpoint;

/**
 * Testcontainer environment
 */
let environment;

/**
 * Helper: HTTP GET request
 */
async function httpGet(url) {
  return new Promise((resolve, reject) => {
    const parsedUrl = new URL(url);
    const options = {
      hostname: parsedUrl.hostname,
      port: parsedUrl.port,
      path: parsedUrl.pathname + parsedUrl.search,
      method: 'GET',
      timeout: 5000
    };

    const req = http.request(options, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          resolve({ status: res.statusCode, data, headers: res.headers });
        } catch (e) {
          resolve({ status: res.statusCode, data, headers: res.headers });
        }
      });
    });

    req.on('error', reject);
    req.on('timeout', () => {
      req.destroy();
      reject(new Error('Request timeout'));
    });

    req.end();
  });
}

/**
 * Helper: HTTP POST request
 */
async function httpPost(url, body) {
  return new Promise((resolve, reject) => {
    const parsedUrl = new URL(url);
    const bodyData = JSON.stringify(body);

    const options = {
      hostname: parsedUrl.hostname,
      port: parsedUrl.port,
      path: parsedUrl.pathname + parsedUrl.search,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(bodyData)
      },
      timeout: 5000
    };

    const req = http.request(options, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          resolve({ status: res.statusCode, data: JSON.parse(data), headers: res.headers });
        } catch (e) {
          resolve({ status: res.statusCode, data, headers: res.headers });
        }
      });
    });

    req.on('error', reject);
    req.on('timeout', () => {
      req.destroy();
      reject(new Error('Request timeout'));
    });

    req.write(bodyData);
    req.end();
  });
}

/**
 * Helper: Wait for service health
 */
async function waitForHealth(url, timeout = 30000) {
  const start = Date.now();
  while (Date.now() - start < timeout) {
    try {
      const response = await httpGet(url);
      if (response.status === 200) {
        return true;
      }
    } catch (e) {
      // Retry
    }
    await new Promise(resolve => setTimeout(resolve, 1000));
  }
  throw new Error(`Service not healthy after ${timeout}ms: ${url}`);
}

describe('KGC Sidecar Testcontainer E2E - 80/20 Critical Scenarios', () => {
  beforeAll(async () => {
    console.log('🚀 Starting testcontainer environment...');

    // Start Docker Compose environment
    // NOTE: Skipping kgc-sidecar wait strategy until HTTP API is implemented
    environment = await new DockerComposeEnvironment(COMPOSE_DIR, COMPOSE_FILE)
      // .withWaitStrategy('kgc-sidecar', Wait.forHealthCheck()) // TODO: Enable when HTTP API exists
      .withWaitStrategy('otel-collector', Wait.forHealthCheck())
      .withWaitStrategy('jaeger', Wait.forHealthCheck())
      .withWaitStrategy('prometheus', Wait.forHealthCheck())
      .withWaitStrategy('gitea', Wait.forHealthCheck())
      .withStartupTimeout(STARTUP_TIMEOUT)
      .up();

    // Get container info
    // const sidecarContainer = environment.getContainer('kgc-sidecar'); // TODO: Enable when HTTP API exists
    const otelContainer = environment.getContainer('otel-collector');
    const jaegerContainer = environment.getContainer('jaeger');
    const prometheusContainer = environment.getContainer('prometheus');
    const giteaContainer = environment.getContainer('gitea');

    // Build service endpoints
    // sidecarEndpoint = `http://${sidecarContainer.getHost()}:${sidecarContainer.getMappedPort(3000)}`; // TODO: Enable when HTTP API exists
    otelCollectorEndpoint = `http://${otelContainer.getHost()}:${otelContainer.getMappedPort(4318)}`;
    jaegerEndpoint = `http://${jaegerContainer.getHost()}:${jaegerContainer.getMappedPort(16686)}`;
    prometheusEndpoint = `http://${prometheusContainer.getHost()}:${prometheusContainer.getMappedPort(9090)}`;
    giteaEndpoint = `http://${giteaContainer.getHost()}:${giteaContainer.getMappedPort(3000)}`;

    console.log('✅ Testcontainer environment ready');
    // console.log(`   Sidecar: ${sidecarEndpoint}`); // TODO: Enable when HTTP API exists
    console.log(`   OTel Collector: ${otelCollectorEndpoint}`);
    console.log(`   Jaeger UI: ${jaegerEndpoint}`);
    console.log(`   Prometheus: ${prometheusEndpoint}`);
    console.log(`   Gitea: ${giteaEndpoint}`);

    // Wait for all services to be healthy
    // await waitForHealth(`${sidecarEndpoint}/health`); // TODO: Enable when HTTP API exists
    await waitForHealth(`${otelCollectorEndpoint}/health`);
    await waitForHealth(`${jaegerEndpoint}/`);
    await waitForHealth(`${prometheusEndpoint}/-/healthy`);
    await waitForHealth(`${giteaEndpoint}/api/healthz`);

    console.log('✅ All services healthy and ready');
  }, STARTUP_TIMEOUT);

  afterAll(async () => {
    if (environment) {
      console.log('🛑 Stopping testcontainer environment...');
      await environment.down();
      console.log('✅ Testcontainer environment stopped');
    }
  });

  /**
   * SCENARIO 1: Transaction Lifecycle + OTel Traces (35% Coverage)
   * ⚠️ TODO: Requires HTTP API implementation
   */
  describe('Scenario 1: Transaction Lifecycle + OTel Traces', () => {
    test.todo('should execute transaction with pre/post hooks and create OTel spans', async () => {
      // 1. Register pre-hook
      const preHookResponse = await httpPost(`${sidecarEndpoint}/api/hooks/register`, {
        id: 'pre-hook-ask',
        select: 'SELECT * WHERE { ?s ?p ?o }',
        predicates: [{
          kind: 'ASK',
          query: 'ASK { ?s a :Person }'
        }],
        combine: 'AND',
        phase: 'pre'
      });

      expect(preHookResponse.status).toBe(200);
      expect(preHookResponse.data.success).toBe(true);

      // 2. Register post-hook
      const postHookResponse = await httpPost(`${sidecarEndpoint}/api/hooks/register`, {
        id: 'post-hook-threshold',
        select: 'SELECT (COUNT(?s) as ?count) WHERE { ?s a :Person }',
        predicates: [{
          kind: 'THRESHOLD',
          variable: 'count',
          operator: '>',
          value: 0
        }],
        combine: 'AND',
        phase: 'post'
      });

      expect(postHookResponse.status).toBe(200);

      // 3. Execute transaction
      const transactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':person1', predicate: 'a', object: ':Person' },
          { subject: ':person1', predicate: ':name', object: '"Alice"' }
        ],
        removals: []
      });

      expect(transactionResponse.status).toBe(200);
      expect(transactionResponse.data.committed).toBe(true);

      const transactionId = transactionResponse.data.receipt.transactionId;

      // 4. Wait for telemetry propagation
      await new Promise(resolve => setTimeout(resolve, 2000));

      // 5. Validate Jaeger traces
      const tracesResponse = await httpGet(
        `${jaegerEndpoint}/api/traces?service=kgc-sidecar&operation=kgc.transaction&limit=10`
      );

      expect(tracesResponse.status).toBe(200);
      const traces = JSON.parse(tracesResponse.data);
      expect(traces.data).toBeDefined();
      expect(traces.data.length).toBeGreaterThan(0);

      // Find our transaction trace
      const transactionTrace = traces.data.find(trace =>
        trace.spans.some(span =>
          span.tags?.find(tag => tag.key === 'kgc.transaction.id' && tag.value === transactionId)
        )
      );

      expect(transactionTrace).toBeDefined();
      expect(transactionTrace.spans.length).toBeGreaterThanOrEqual(3); // parent + 2 hooks

      // Validate span hierarchy
      const parentSpan = transactionTrace.spans.find(s => s.references.length === 0);
      expect(parentSpan).toBeDefined();
      expect(parentSpan.operationName).toBe('kgc.transaction');

      const hookSpans = transactionTrace.spans.filter(s => s.operationName === 'kgc.hook');
      expect(hookSpans.length).toBeGreaterThanOrEqual(2);

      // 6. Validate Prometheus metrics
      const metricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_transactions_total`
      );

      expect(metricsResponse.status).toBe(200);
      const metrics = JSON.parse(metricsResponse.data);
      expect(metrics.data.result.length).toBeGreaterThan(0);
    }, 30000);

    test.todo('should record transaction latency metrics within SLO (p99 < 2ms)', async () => {
      // Execute multiple transactions to build histogram
      for (let i = 0; i < 10; i++) {
        await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
          additions: [
            { subject: `:person${i}`, predicate: 'a', object: ':Person' }
          ],
          removals: []
        });
      }

      // Wait for metrics aggregation
      await new Promise(resolve => setTimeout(resolve, 5000));

      // Query p99 latency from Prometheus
      const p99Response = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=histogram_quantile(0.99, rate(kgc_transaction_duration_ms_bucket[1m]))`
      );

      expect(p99Response.status).toBe(200);
      const p99Data = JSON.parse(p99Response.data);

      if (p99Data.data.result.length > 0) {
        const p99Latency = parseFloat(p99Data.data.result[0].value[1]);
        console.log(`   📊 p99 latency: ${p99Latency.toFixed(2)}ms`);
        expect(p99Latency).toBeLessThan(2); // KGC PRD SLO: p99 < 2ms
      }
    }, 20000);
  });

  /**
   * SCENARIO 2: Policy Pack Governance + Metrics (25% Coverage)
   * ⚠️ TODO: Requires HTTP API implementation
   */
  describe('Scenario 2: Policy Pack Governance + Metrics', () => {
    test.todo('should enforce SHACL policy and record compliance metrics', async () => {
      // 1. Create and activate policy pack with SHACL shape
      const policyPackResponse = await httpPost(`${sidecarEndpoint}/api/policy/register`, {
        id: 'person-policy-pack',
        shapes: [
          {
            '@id': ':PersonShape',
            '@type': 'sh:NodeShape',
            'sh:targetClass': ':Person',
            'sh:property': [
              {
                'sh:path': ':name',
                'sh:minCount': 1,
                'sh:datatype': 'xsd:string'
              },
              {
                'sh:path': ':email',
                'sh:minCount': 1,
                'sh:datatype': 'xsd:string',
                'sh:pattern': '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$'
              }
            ]
          }
        ],
        active: true
      });

      expect(policyPackResponse.status).toBe(200);
      expect(policyPackResponse.data.success).toBe(true);

      // 2. Execute valid transaction (complies with SHACL)
      const validTransactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':alice', predicate: 'a', object: ':Person' },
          { subject: ':alice', predicate: ':name', object: '"Alice Smith"' },
          { subject: ':alice', predicate: ':email', object: '"alice@example.com"' }
        ],
        removals: []
      });

      expect(validTransactionResponse.status).toBe(200);
      expect(validTransactionResponse.data.committed).toBe(true);

      // 3. Execute invalid transaction (violates SHACL - missing email)
      const invalidTransactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':bob', predicate: 'a', object: ':Person' },
          { subject: ':bob', predicate: ':name', object: '"Bob Jones"' }
          // Missing required :email property
        ],
        removals: []
      });

      expect(invalidTransactionResponse.status).toBe(200);
      expect(invalidTransactionResponse.data.committed).toBe(false);
      expect(invalidTransactionResponse.data.vetoed).toBe(true);
      expect(invalidTransactionResponse.data.vetoReason).toContain('SHACL');

      // 4. Wait for metrics aggregation
      await new Promise(resolve => setTimeout(resolve, 2000));

      // 5. Validate policy compliance metrics in Prometheus
      const complianceResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_policy_compliance_rate`
      );

      expect(complianceResponse.status).toBe(200);
      const complianceData = JSON.parse(complianceResponse.data);

      if (complianceData.data.result.length > 0) {
        const complianceRate = parseFloat(complianceData.data.result[0].value[1]);
        console.log(`   📊 Policy compliance rate: ${(complianceRate * 100).toFixed(1)}%`);
        expect(complianceRate).toBeGreaterThan(0); // At least some compliance
      }

      // 6. Validate SHACL validation metrics
      const shaclMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_shacl_validations_total`
      );

      expect(shaclMetricsResponse.status).toBe(200);
      const shaclMetrics = JSON.parse(shaclMetricsResponse.data);
      expect(shaclMetrics.data.result.length).toBeGreaterThan(0);

      // 7. Validate traces show policy hook execution
      const policyTracesResponse = await httpGet(
        `${jaegerEndpoint}/api/traces?service=kgc-sidecar&operation=kgc.policy&limit=10`
      );

      expect(policyTracesResponse.status).toBe(200);
      const policyTraces = JSON.parse(policyTracesResponse.data);
      expect(policyTraces.data).toBeDefined();
    }, 30000);

    test.todo('should veto invalid transactions with detailed violation report', async () => {
      // 1. Execute transaction with multiple SHACL violations
      const violationResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':charlie', predicate: 'a', object: ':Person' },
          { subject: ':charlie', predicate: ':email', object: '"invalid-email"' }
          // Missing :name (violation 1)
          // Invalid :email format (violation 2)
        ],
        removals: []
      });

      expect(violationResponse.status).toBe(200);
      expect(violationResponse.data.committed).toBe(false);
      expect(violationResponse.data.vetoed).toBe(true);

      // 2. Validate detailed violation report in receipt
      const receipt = violationResponse.data.receipt;
      expect(receipt).toBeDefined();
      expect(receipt.vetoReason).toBeDefined();
      expect(receipt.violations).toBeDefined();
      expect(receipt.violations.length).toBeGreaterThan(0);

      // Check for specific violation details
      const violations = receipt.violations;
      const hasNameViolation = violations.some(v =>
        v.path && v.path.includes('name')
      );
      const hasEmailViolation = violations.some(v =>
        v.path && v.path.includes('email')
      );

      expect(hasNameViolation || hasEmailViolation).toBe(true);

      // 3. Validate veto metrics in Prometheus
      const vetoMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_transactions_vetoed_total`
      );

      expect(vetoMetricsResponse.status).toBe(200);
      const vetoMetrics = JSON.parse(vetoMetricsResponse.data);

      if (vetoMetrics.data.result.length > 0) {
        const vetoCount = parseInt(vetoMetrics.data.result[0].value[1]);
        console.log(`   🛑 Total vetoed transactions: ${vetoCount}`);
        expect(vetoCount).toBeGreaterThan(0);
      }

      // 4. Validate error span in Jaeger with violation details
      await new Promise(resolve => setTimeout(resolve, 2000));

      const errorTracesResponse = await httpGet(
        `${jaegerEndpoint}/api/traces?service=kgc-sidecar&tag=error:true&limit=10`
      );

      expect(errorTracesResponse.status).toBe(200);
      const errorTraces = JSON.parse(errorTracesResponse.data);

      if (errorTraces.data && errorTraces.data.length > 0) {
        const vetoSpan = errorTraces.data[0].spans.find(s =>
          s.tags && s.tags.some(tag => tag.key === 'kgc.veto' && tag.value === true)
        );

        if (vetoSpan) {
          console.log(`   🔍 Found veto span with ${vetoSpan.tags.length} tags`);
          expect(vetoSpan).toBeDefined();
        }
      }
    }, 30000);
  });

  /**
   * SCENARIO 3: Effect Sandbox Security + Error Isolation (15% Coverage)
   * ⚠️ TODO: Requires HTTP API implementation
   */
  describe('Scenario 3: Effect Sandbox Security + Error Isolation', () => {
    test.todo('should isolate infinite loop effect and timeout', async () => {
      // 1. Register effect with infinite loop
      const effectResponse = await httpPost(`${sidecarEndpoint}/api/effects/register`, {
        id: 'infinite-loop-effect',
        code: `
          // Infinite loop - should timeout
          while (true) {
            Math.random();
          }
        `,
        timeout: 5000 // 5 second timeout
      });

      expect(effectResponse.status).toBe(200);

      // 2. Execute transaction that triggers the effect
      const transactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':trigger', predicate: ':activateEffect', object: ':infinite-loop-effect' }
        ],
        removals: [],
        effects: ['infinite-loop-effect']
      });

      expect(transactionResponse.status).toBe(200);

      // 3. Validate transaction committed but effect timed out
      expect(transactionResponse.data.committed).toBe(true);
      expect(transactionResponse.data.effectResults).toBeDefined();

      const effectResult = transactionResponse.data.effectResults.find(
        r => r.effectId === 'infinite-loop-effect'
      );

      expect(effectResult).toBeDefined();
      expect(effectResult.success).toBe(false);
      expect(effectResult.error).toContain('timeout');

      // 4. Validate timeout metrics in Prometheus
      await new Promise(resolve => setTimeout(resolve, 2000));

      const timeoutMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_effect_timeouts_total`
      );

      expect(timeoutMetricsResponse.status).toBe(200);
      const timeoutMetrics = JSON.parse(timeoutMetricsResponse.data);

      if (timeoutMetrics.data.result.length > 0) {
        const timeoutCount = parseInt(timeoutMetrics.data.result[0].value[1]);
        console.log(`   ⏱️  Effect timeouts: ${timeoutCount}`);
        expect(timeoutCount).toBeGreaterThan(0);
      }

      // 5. Validate error span in Jaeger
      const errorTracesResponse = await httpGet(
        `${jaegerEndpoint}/api/traces?service=kgc-sidecar&operation=kgc.effect&limit=10`
      );

      expect(errorTracesResponse.status).toBe(200);
      const errorTraces = JSON.parse(errorTracesResponse.data);
      expect(errorTraces.data).toBeDefined();
    }, 30000);

    test.todo('should isolate memory bomb effect and enforce limit', async () => {
      // 1. Register effect that tries to allocate excessive memory
      const effectResponse = await httpPost(`${sidecarEndpoint}/api/effects/register`, {
        id: 'memory-bomb-effect',
        code: `
          // Try to allocate 100MB (should exceed 64MB limit)
          const arrays = [];
          for (let i = 0; i < 100; i++) {
            arrays.push(new Array(1024 * 1024).fill(Math.random()));
          }
        `,
        memoryLimit: 64 * 1024 * 1024 // 64MB limit
      });

      expect(effectResponse.status).toBe(200);

      // 2. Execute transaction that triggers the effect
      const transactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':trigger', predicate: ':activateEffect', object: ':memory-bomb-effect' }
        ],
        removals: [],
        effects: ['memory-bomb-effect']
      });

      expect(transactionResponse.status).toBe(200);

      // 3. Validate transaction committed but effect failed due to memory limit
      expect(transactionResponse.data.committed).toBe(true);
      expect(transactionResponse.data.effectResults).toBeDefined();

      const effectResult = transactionResponse.data.effectResults.find(
        r => r.effectId === 'memory-bomb-effect'
      );

      expect(effectResult).toBeDefined();
      expect(effectResult.success).toBe(false);
      expect(effectResult.error).toMatch(/memory|limit|exceeded/i);

      // 4. Validate memory limit metrics
      await new Promise(resolve => setTimeout(resolve, 2000));

      const memoryMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_effect_memory_limit_exceeded_total`
      );

      expect(memoryMetricsResponse.status).toBe(200);
      const memoryMetrics = JSON.parse(memoryMetricsResponse.data);

      if (memoryMetrics.data.result.length > 0) {
        const memoryLimitCount = parseInt(memoryMetrics.data.result[0].value[1]);
        console.log(`   💾 Memory limit exceeded: ${memoryLimitCount}`);
        expect(memoryLimitCount).toBeGreaterThan(0);
      }
    }, 30000);

    test.todo('should record error spans in Jaeger with exception details', async () => {
      // 1. Register effect that throws exception
      const effectResponse = await httpPost(`${sidecarEndpoint}/api/effects/register`, {
        id: 'exception-effect',
        code: `
          throw new Error('Intentional test exception with stack trace');
        `
      });

      expect(effectResponse.status).toBe(200);

      // 2. Execute transaction that triggers the effect
      const transactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':trigger', predicate: ':activateEffect', object: ':exception-effect' }
        ],
        removals: [],
        effects: ['exception-effect']
      });

      expect(transactionResponse.status).toBe(200);
      expect(transactionResponse.data.committed).toBe(true);

      const effectResult = transactionResponse.data.effectResults.find(
        r => r.effectId === 'exception-effect'
      );

      expect(effectResult).toBeDefined();
      expect(effectResult.success).toBe(false);
      expect(effectResult.error).toContain('Intentional test exception');

      // 3. Wait for trace propagation
      await new Promise(resolve => setTimeout(resolve, 3000));

      // 4. Validate error span in Jaeger with exception details
      const errorTracesResponse = await httpGet(
        `${jaegerEndpoint}/api/traces?service=kgc-sidecar&tag=error:true&limit=10`
      );

      expect(errorTracesResponse.status).toBe(200);
      const errorTraces = JSON.parse(errorTracesResponse.data);

      if (errorTraces.data && errorTraces.data.length > 0) {
        const effectSpan = errorTraces.data[0].spans.find(s =>
          s.operationName === 'kgc.effect' &&
          s.tags && s.tags.some(tag => tag.key === 'error' && tag.value === true)
        );

        if (effectSpan) {
          console.log(`   🔴 Found error span: ${effectSpan.operationName}`);

          // Validate error tags
          const errorTag = effectSpan.tags.find(t => t.key === 'error');
          expect(errorTag).toBeDefined();
          expect(errorTag.value).toBe(true);

          // Check for error message in tags or logs
          const hasErrorDetails = effectSpan.tags.some(tag =>
            tag.key === 'error.message' || tag.key === 'exception.message'
          ) || effectSpan.logs && effectSpan.logs.some(log =>
            log.fields && log.fields.some(f => f.key === 'message')
          );

          console.log(`   📋 Error details present: ${hasErrorDetails}`);
        }
      }

      // 5. Validate effect error metrics
      const errorMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_effect_errors_total`
      );

      expect(errorMetricsResponse.status).toBe(200);
      const errorMetrics = JSON.parse(errorMetricsResponse.data);

      if (errorMetrics.data.result.length > 0) {
        const errorCount = parseInt(errorMetrics.data.result[0].value[1]);
        console.log(`   🚨 Total effect errors: ${errorCount}`);
        expect(errorCount).toBeGreaterThan(0);
      }
    }, 35000);
  });

  /**
   * SCENARIO 4: Lockchain Audit Trail + Git Integration (15% Coverage)
   * ⚠️ TODO: Requires HTTP API implementation
   */
  describe('Scenario 4: Lockchain Audit Trail + Git Integration', () => {
    test.todo('should write receipts to Git notes', async () => {
      // 1. Initialize Git repository for lockchain
      const initResponse = await httpPost(`${sidecarEndpoint}/api/lockchain/init`, {
        gitUrl: `http://gitea:3000/kgc-test/audit-trail.git`,
        branch: 'main'
      });

      expect(initResponse.status).toBe(200);
      expect(initResponse.data.success).toBe(true);

      // 2. Execute transaction that should be written to lockchain
      const transactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':doc1', predicate: ':title', object: '"Important Document"' },
          { subject: ':doc1', predicate: ':timestamp', object: '"2024-01-01T00:00:00Z"' }
        ],
        removals: [],
        lockchain: true // Enable lockchain recording
      });

      expect(transactionResponse.status).toBe(200);
      expect(transactionResponse.data.committed).toBe(true);

      const transactionId = transactionResponse.data.receipt.transactionId;
      const receiptHash = transactionResponse.data.receipt.hash;

      // 3. Wait for Git notes to be written
      await new Promise(resolve => setTimeout(resolve, 3000));

      // 4. Verify receipt was written to Git notes via Gitea API
      const notesResponse = await httpGet(
        `${giteaEndpoint}/api/v1/repos/kgc-test/audit-trail/git/notes`
      );

      expect(notesResponse.status).toBe(200);

      // 5. Verify receipt can be retrieved from lockchain
      const receiptResponse = await httpGet(
        `${sidecarEndpoint}/api/lockchain/receipt/${transactionId}`
      );

      expect(receiptResponse.status).toBe(200);
      const receipt = JSON.parse(receiptResponse.data);

      expect(receipt.transactionId).toBe(transactionId);
      expect(receipt.hash).toBe(receiptHash);
      expect(receipt.gitCommit).toBeDefined();
      expect(receipt.merkleProof).toBeDefined();

      console.log(`   📜 Receipt written to Git: ${receipt.gitCommit}`);

      // 6. Validate lockchain metrics
      const lockchainMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_lockchain_receipts_total`
      );

      expect(lockchainMetricsResponse.status).toBe(200);
      const lockchainMetrics = JSON.parse(lockchainMetricsResponse.data);

      if (lockchainMetrics.data.result.length > 0) {
        const receiptCount = parseInt(lockchainMetrics.data.result[0].value[1]);
        console.log(`   📊 Total lockchain receipts: ${receiptCount}`);
        expect(receiptCount).toBeGreaterThan(0);
      }
    }, 40000);

    test.todo('should verify Merkle proof for lockchain integrity', async () => {
      // 1. Execute multiple transactions to build Merkle tree
      const transactionIds = [];

      for (let i = 0; i < 5; i++) {
        const response = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
          additions: [
            { subject: `:item${i}`, predicate: ':index', object: `"${i}"` }
          ],
          removals: [],
          lockchain: true
        });

        expect(response.status).toBe(200);
        transactionIds.push(response.data.receipt.transactionId);
      }

      // 2. Wait for lockchain propagation
      await new Promise(resolve => setTimeout(resolve, 3000));

      // 3. Verify Merkle proof for each transaction
      for (const txId of transactionIds) {
        const proofResponse = await httpGet(
          `${sidecarEndpoint}/api/lockchain/verify/${txId}`
        );

        expect(proofResponse.status).toBe(200);
        const verification = JSON.parse(proofResponse.data);

        expect(verification.valid).toBe(true);
        expect(verification.merkleProof).toBeDefined();
        expect(verification.merkleRoot).toBeDefined();
        expect(verification.path).toBeDefined();
        expect(Array.isArray(verification.path)).toBe(true);

        console.log(`   ✅ Merkle proof verified for transaction ${txId.substring(0, 8)}...`);
      }

      // 4. Validate Merkle tree integrity metrics
      const integrityMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_lockchain_verifications_total`
      );

      expect(integrityMetricsResponse.status).toBe(200);
      const integrityMetrics = JSON.parse(integrityMetricsResponse.data);

      if (integrityMetrics.data.result.length > 0) {
        const verificationCount = parseInt(integrityMetrics.data.result[0].value[1]);
        console.log(`   🔐 Total verifications: ${verificationCount}`);
        expect(verificationCount).toBeGreaterThanOrEqual(5);
      }
    }, 50000);

    test.todo('should detect tampered receipts', async () => {
      // 1. Execute transaction and get receipt
      const transactionResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        additions: [
          { subject: ':original', predicate: ':data', object: '"untampered"' }
        ],
        removals: [],
        lockchain: true
      });

      expect(transactionResponse.status).toBe(200);
      const transactionId = transactionResponse.data.receipt.transactionId;
      const originalHash = transactionResponse.data.receipt.hash;

      // 2. Wait for lockchain write
      await new Promise(resolve => setTimeout(resolve, 3000));

      // 3. Attempt to tamper with receipt (modify hash)
      const tamperedReceipt = {
        transactionId,
        hash: 'tampered-hash-12345',
        timestamp: new Date().toISOString(),
        delta: {
          additions: [
            { subject: ':tampered', predicate: ':data', object: '"modified"' }
          ],
          removals: []
        }
      };

      // 4. Submit tampered receipt for verification
      const tamperResponse = await httpPost(
        `${sidecarEndpoint}/api/lockchain/verify`,
        tamperedReceipt
      );

      expect(tamperResponse.status).toBe(200);
      const verification = JSON.parse(tamperResponse.data);

      // 5. Verify tampering was detected
      expect(verification.valid).toBe(false);
      expect(verification.reason).toMatch(/tampered|invalid|mismatch/i);

      console.log(`   🚨 Tampering detected: ${verification.reason}`);

      // 6. Verify original receipt is still valid
      const originalVerifyResponse = await httpGet(
        `${sidecarEndpoint}/api/lockchain/verify/${transactionId}`
      );

      expect(originalVerifyResponse.status).toBe(200);
      const originalVerification = JSON.parse(originalVerifyResponse.data);

      expect(originalVerification.valid).toBe(true);
      expect(originalVerification.hash).toBe(originalHash);

      console.log(`   ✅ Original receipt still valid`);

      // 7. Validate tampering detection metrics
      const tamperingMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_lockchain_tampering_detected_total`
      );

      expect(tamperingMetricsResponse.status).toBe(200);
      const tamperingMetrics = JSON.parse(tamperingMetricsResponse.data);

      if (tamperingMetrics.data.result.length > 0) {
        const tamperingCount = parseInt(tamperingMetrics.data.result[0].value[1]);
        console.log(`   🛡️  Tampering attempts detected: ${tamperingCount}`);
        expect(tamperingCount).toBeGreaterThan(0);
      }

      // 8. Validate error span in Jaeger for tampering attempt
      const tamperingTracesResponse = await httpGet(
        `${jaegerEndpoint}/api/traces?service=kgc-sidecar&tag=tampering:detected&limit=10`
      );

      expect(tamperingTracesResponse.status).toBe(200);
      const tamperingTraces = JSON.parse(tamperingTracesResponse.data);

      if (tamperingTraces.data && tamperingTraces.data.length > 0) {
        console.log(`   🔍 Found tampering trace in Jaeger`);
      }
    }, 40000);
  });

  /**
   * SCENARIO 5: Multi-Agent Resolution + Distributed Traces (10% Coverage)
   * ⚠️ TODO: Requires HTTP API implementation
   */
  describe('Scenario 5: Multi-Agent Resolution + Distributed Traces', () => {
    test.todo('should detect conflicts and apply resolution strategy', async () => {
      // 1. Register two agents with different priorities
      const agent1Response = await httpPost(`${sidecarEndpoint}/api/agents/register`, {
        id: 'agent-high-priority',
        name: 'High Priority Agent',
        priority: 10
      });

      const agent2Response = await httpPost(`${sidecarEndpoint}/api/agents/register`, {
        id: 'agent-low-priority',
        name: 'Low Priority Agent',
        priority: 5
      });

      expect(agent1Response.status).toBe(200);
      expect(agent2Response.status).toBe(200);

      // 2. Create conflicting transactions from both agents
      const conflict1Response = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        agentId: 'agent-high-priority',
        additions: [
          { subject: ':resource', predicate: ':value', object: '"100"' }
        ],
        removals: []
      });

      const conflict2Response = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        agentId: 'agent-low-priority',
        additions: [
          { subject: ':resource', predicate: ':value', object: '"200"' }
        ],
        removals: []
      });

      expect(conflict1Response.status).toBe(200);
      expect(conflict2Response.status).toBe(200);

      // 3. Wait for conflict resolution
      await new Promise(resolve => setTimeout(resolve, 2000));

      // 4. Query final state - should use high priority agent's value
      const stateResponse = await httpGet(
        `${sidecarEndpoint}/api/query?query=${encodeURIComponent('SELECT ?value WHERE { :resource :value ?value }')}`
      );

      expect(stateResponse.status).toBe(200);
      const state = JSON.parse(stateResponse.data);

      // Verify high-priority agent won
      expect(state.results.bindings).toBeDefined();
      expect(state.results.bindings.length).toBeGreaterThan(0);
      const finalValue = state.results.bindings[0].value.value;
      expect(finalValue).toBe('100'); // High priority agent's value

      console.log(`   ⚖️  Conflict resolved: final value = ${finalValue}`);

      // 5. Validate conflict resolution metrics
      const conflictMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_conflicts_detected_total`
      );

      expect(conflictMetricsResponse.status).toBe(200);
      const conflictMetrics = JSON.parse(conflictMetricsResponse.data);

      if (conflictMetrics.data.result.length > 0) {
        const conflictCount = parseInt(conflictMetrics.data.result[0].value[1]);
        console.log(`   🔀 Conflicts detected: ${conflictCount}`);
        expect(conflictCount).toBeGreaterThan(0);
      }

      // 6. Validate resolution strategy metrics
      const resolutionMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_conflicts_resolved_total`
      );

      expect(resolutionMetricsResponse.status).toBe(200);
      const resolutionMetrics = JSON.parse(resolutionMetricsResponse.data);

      if (resolutionMetrics.data.result.length > 0) {
        const resolutionCount = parseInt(resolutionMetrics.data.result[0].value[1]);
        console.log(`   ✅ Conflicts resolved: ${resolutionCount}`);
        expect(resolutionCount).toBeGreaterThan(0);
      }
    }, 30000);

    test.todo('should create distributed trace across agents', async () => {
      // 1. Register three agents to create distributed workflow
      const agents = ['coordinator', 'validator', 'executor'];
      for (const agentId of agents) {
        const response = await httpPost(`${sidecarEndpoint}/api/agents/register`, {
          id: `agent-${agentId}`,
          name: `${agentId.charAt(0).toUpperCase() + agentId.slice(1)} Agent`,
          role: agentId
        });
        expect(response.status).toBe(200);
      }

      // 2. Execute distributed transaction workflow
      // Coordinator initiates
      const coordinatorResponse = await httpPost(`${sidecarEndpoint}/api/transaction/apply`, {
        agentId: 'agent-coordinator',
        additions: [
          { subject: ':workflow', predicate: ':step', object: '"coordinated"' }
        ],
        removals: [],
        distributed: true,
        nextAgent: 'agent-validator'
      });

      expect(coordinatorResponse.status).toBe(200);
      const workflowId = coordinatorResponse.data.receipt.workflowId;

      // 3. Wait for distributed workflow completion
      await new Promise(resolve => setTimeout(resolve, 5000));

      // 4. Query Jaeger for distributed trace
      const distributedTracesResponse = await httpGet(
        `${jaegerEndpoint}/api/traces?service=kgc-sidecar&operation=kgc.workflow&limit=10`
      );

      expect(distributedTracesResponse.status).toBe(200);
      const distributedTraces = JSON.parse(distributedTracesResponse.data);

      if (distributedTraces.data && distributedTraces.data.length > 0) {
        // Find our workflow trace
        const workflowTrace = distributedTraces.data.find(trace =>
          trace.spans.some(span =>
            span.tags && span.tags.some(tag => tag.key === 'kgc.workflow.id' && tag.value === workflowId)
          )
        );

        if (workflowTrace) {
          console.log(`   🌐 Found distributed trace with ${workflowTrace.spans.length} spans`);

          // Validate span hierarchy across agents
          const coordinatorSpan = workflowTrace.spans.find(s =>
            s.tags && s.tags.some(t => t.key === 'kgc.agent.id' && t.value === 'agent-coordinator')
          );

          const validatorSpan = workflowTrace.spans.find(s =>
            s.tags && s.tags.some(t => t.key === 'kgc.agent.id' && t.value === 'agent-validator')
          );

          const executorSpan = workflowTrace.spans.find(s =>
            s.tags && s.tags.some(t => t.key === 'kgc.agent.id' && t.value === 'agent-executor')
          );

          expect(coordinatorSpan).toBeDefined();
          console.log(`   👤 Coordinator span: ${coordinatorSpan?.operationName}`);

          if (validatorSpan) {
            console.log(`   ✓ Validator span: ${validatorSpan.operationName}`);
          }

          if (executorSpan) {
            console.log(`   ⚡ Executor span: ${executorSpan.operationName}`);
          }

          // Validate parent-child relationships
          const hasDistributedRelationships = workflowTrace.spans.some(span =>
            span.references && span.references.length > 0
          );

          expect(hasDistributedRelationships).toBe(true);
          console.log(`   🔗 Distributed span relationships verified`);
        }
      }

      // 5. Validate distributed workflow metrics
      const workflowMetricsResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=kgc_distributed_workflows_total`
      );

      expect(workflowMetricsResponse.status).toBe(200);
      const workflowMetrics = JSON.parse(workflowMetricsResponse.data);

      if (workflowMetrics.data.result.length > 0) {
        const workflowCount = parseInt(workflowMetrics.data.result[0].value[1]);
        console.log(`   📊 Distributed workflows: ${workflowCount}`);
        expect(workflowCount).toBeGreaterThan(0);
      }

      // 6. Validate agent coordination latency
      const latencyResponse = await httpGet(
        `${prometheusEndpoint}/api/v1/query?query=histogram_quantile(0.99, rate(kgc_agent_coordination_duration_ms_bucket[1m]))`
      );

      expect(latencyResponse.status).toBe(200);
      const latencyData = JSON.parse(latencyResponse.data);

      if (latencyData.data.result.length > 0) {
        const p99Latency = parseFloat(latencyData.data.result[0].value[1]);
        console.log(`   ⚡ Agent coordination p99 latency: ${p99Latency.toFixed(2)}ms`);
        expect(p99Latency).toBeLessThan(100); // Coordination should be fast
      }
    }, 40000);
  });

  /**
   * Infrastructure Validation Tests
   */
  describe('Infrastructure Validation', () => {
    test('OTel Collector should be healthy and accepting OTLP', async () => {
      const healthResponse = await httpGet(`${otelCollectorEndpoint}/health`);
      expect(healthResponse.status).toBe(200);
    });

    test('Jaeger UI should be accessible and show services', async () => {
      const servicesResponse = await httpGet(`${jaegerEndpoint}/api/services`);
      expect(servicesResponse.status).toBe(200);

      const services = JSON.parse(servicesResponse.data);
      expect(services.data).toContain('kgc-sidecar');
    });

    test('Prometheus should be scraping KGC metrics', async () => {
      const targetsResponse = await httpGet(`${prometheusEndpoint}/api/v1/targets`);
      expect(targetsResponse.status).toBe(200);

      const targets = JSON.parse(targetsResponse.data);
      const kgcTarget = targets.data.activeTargets.find(t =>
        t.labels.job === 'kgc-sidecar-metrics'
      );

      expect(kgcTarget).toBeDefined();
      expect(kgcTarget.health).toBe('up');
    });

    test('Gitea should be ready for lockchain repository', async () => {
      const healthResponse = await httpGet(`${giteaEndpoint}/api/healthz`);
      expect(healthResponse.status).toBe(200);
    });
  });
});
