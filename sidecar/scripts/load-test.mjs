#!/usr/bin/env node

/**
 * Load Testing Script for UNRDF Sidecar
 *
 * Uses k6 for comprehensive load testing:
 * - Sustained load: 10,000 tx/sec for 1 hour
 * - Spike test: 50,000 tx/sec burst
 * - Ramp-up test: Progressive load increase
 * - Stress test: Find breaking point
 */

import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Trend, Counter } from 'k6/metrics';

// Custom metrics
const errorRate = new Rate('errors');
const transactionDuration = new Trend('transaction_duration');
const transactionCounter = new Counter('transactions');

// Test configuration
export const options = {
  scenarios: {
    // Scenario 1: Sustained Load Test
    sustained_load: {
      executor: 'constant-arrival-rate',
      rate: 10000, // 10k transactions per second
      timeUnit: '1s',
      duration: '1h',
      preAllocatedVUs: 500,
      maxVUs: 1000,
      exec: 'sustainedLoadTest',
      startTime: '0s'
    },

    // Scenario 2: Spike Test
    spike_test: {
      executor: 'ramping-arrival-rate',
      startRate: 1000,
      timeUnit: '1s',
      preAllocatedVUs: 1000,
      maxVUs: 5000,
      stages: [
        { duration: '30s', target: 1000 },   // Normal load
        { duration: '10s', target: 50000 },  // Spike to 50k
        { duration: '30s', target: 50000 },  // Sustain spike
        { duration: '10s', target: 1000 },   // Back to normal
        { duration: '30s', target: 1000 },   // Recovery
      ],
      exec: 'spikeTest',
      startTime: '1h' // Start after sustained test
    },

    // Scenario 3: Ramp-up Test
    ramp_up: {
      executor: 'ramping-vus',
      startVUs: 0,
      stages: [
        { duration: '5m', target: 100 },
        { duration: '10m', target: 500 },
        { duration: '10m', target: 1000 },
        { duration: '5m', target: 0 },
      ],
      exec: 'rampUpTest',
      startTime: '1h15m'
    },

    // Scenario 4: Stress Test
    stress_test: {
      executor: 'ramping-arrival-rate',
      startRate: 1000,
      timeUnit: '1s',
      preAllocatedVUs: 1000,
      maxVUs: 10000,
      stages: [
        { duration: '2m', target: 5000 },
        { duration: '5m', target: 10000 },
        { duration: '5m', target: 20000 },
        { duration: '5m', target: 30000 },
        { duration: '2m', target: 0 },
      ],
      exec: 'stressTest',
      startTime: '1h45m'
    }
  },

  thresholds: {
    // Performance SLA thresholds
    'http_req_duration': ['p(95)<100', 'p(99)<200'], // 95% under 100ms, 99% under 200ms
    'http_req_failed': ['rate<0.01'], // Error rate under 1%
    'errors': ['rate<0.01'],
    'transaction_duration': ['p(99)<150'],
  },
};

const BASE_URL = __ENV.BASE_URL || 'http://localhost:3456';

/**
 * Sustained Load Test
 * Validates system can handle 10k tx/sec for 1 hour
 */
export function sustainedLoadTest() {
  const transactions = [
    insertTransaction(),
    queryTransaction(),
    updateTransaction()
  ];

  const transaction = transactions[Math.floor(Math.random() * transactions.length)];
  executeTransaction(transaction);
}

/**
 * Spike Test
 * Validates system handles sudden 50k tx/sec burst
 */
export function spikeTest() {
  const transaction = insertTransaction(); // Most demanding operation
  executeTransaction(transaction);
}

/**
 * Ramp-up Test
 * Progressive load increase to validate scaling
 */
export function rampUpTest() {
  const transaction = queryTransaction();
  executeTransaction(transaction);
  sleep(1); // Simulate realistic user behavior
}

/**
 * Stress Test
 * Find breaking point by progressively increasing load
 */
export function stressTest() {
  const transaction = insertTransaction();
  executeTransaction(transaction);
}

/**
 * Execute a transaction and record metrics
 */
function executeTransaction(transaction) {
  const startTime = Date.now();

  const response = http.post(
    `${BASE_URL}/api/transaction`,
    JSON.stringify(transaction),
    {
      headers: {
        'Content-Type': 'application/json',
        'X-Request-ID': generateRequestId(),
      },
      tags: {
        operation: transaction.operation,
        test_type: 'load_test'
      }
    }
  );

  const duration = Date.now() - startTime;

  // Record metrics
  transactionDuration.add(duration);
  transactionCounter.add(1);

  // Check response
  const success = check(response, {
    'status is 200': (r) => r.status === 200,
    'response time < 200ms': (r) => r.timings.duration < 200,
    'no errors': (r) => !r.json('error'),
  });

  if (!success) {
    errorRate.add(1);
  }
}

/**
 * Generate INSERT transaction
 */
function insertTransaction() {
  return {
    operation: 'insert',
    subject: `http://example.org/resource/${generateId()}`,
    predicate: 'http://example.org/property',
    object: {
      '@value': `test_value_${Math.random()}`,
      '@type': 'http://www.w3.org/2001/XMLSchema#string'
    },
    provenance: {
      agent: 'load-test',
      timestamp: new Date().toISOString()
    }
  };
}

/**
 * Generate QUERY transaction
 */
function queryTransaction() {
  return {
    operation: 'query',
    query: `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o
      }
      LIMIT 100
    `
  };
}

/**
 * Generate UPDATE transaction
 */
function updateTransaction() {
  return {
    operation: 'update',
    subject: `http://example.org/resource/${Math.floor(Math.random() * 10000)}`,
    predicate: 'http://example.org/updated',
    object: {
      '@value': new Date().toISOString(),
      '@type': 'http://www.w3.org/2001/XMLSchema#dateTime'
    }
  };
}

/**
 * Generate unique ID
 */
function generateId() {
  return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Generate request correlation ID
 */
function generateRequestId() {
  return `load-test-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
}

/**
 * Setup function - runs once at test start
 */
export function setup() {
  console.log('🚀 Starting UNRDF Load Test');
  console.log(`📍 Target: ${BASE_URL}`);
  console.log('📊 Scenarios:');
  console.log('   1. Sustained Load: 10k tx/sec for 1 hour');
  console.log('   2. Spike Test: 50k tx/sec burst');
  console.log('   3. Ramp-up Test: Progressive scaling');
  console.log('   4. Stress Test: Find breaking point');

  // Warmup request
  const warmup = http.get(`${BASE_URL}/health`);
  check(warmup, {
    'warmup successful': (r) => r.status === 200,
  });

  return { startTime: Date.now() };
}

/**
 * Teardown function - runs once at test end
 */
export function teardown(data) {
  const duration = (Date.now() - data.startTime) / 1000;
  console.log(`\n✅ Load test completed in ${duration}s`);
  console.log('📊 Check Grafana dashboards for detailed metrics');
}

/**
 * Handle summary data
 */
export function handleSummary(data) {
  return {
    'stdout': textSummary(data, { indent: ' ', enableColors: true }),
    '/tmp/unrdf-load-test.json': JSON.stringify(data),
    '/tmp/unrdf-load-test.html': htmlReport(data),
  };
}

/**
 * Generate text summary
 */
function textSummary(data, options) {
  let summary = '\n📊 LOAD TEST SUMMARY\n';
  summary += '='.repeat(50) + '\n\n';

  for (const [scenario, metrics] of Object.entries(data.metrics)) {
    summary += `${scenario}:\n`;

    if (metrics.values) {
      summary += `  Rate: ${metrics.values.rate?.toFixed(2) || 'N/A'}\n`;
      summary += `  P95: ${metrics.values.p95?.toFixed(2) || 'N/A'}ms\n`;
      summary += `  P99: ${metrics.values.p99?.toFixed(2) || 'N/A'}ms\n`;
    }

    summary += '\n';
  }

  return summary;
}

/**
 * Generate HTML report
 */
function htmlReport(data) {
  return `
    <!DOCTYPE html>
    <html>
    <head>
      <title>UNRDF Load Test Report</title>
      <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        h1 { color: #333; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
        tr:nth-child(even) { background-color: #f2f2f2; }
        .pass { color: green; font-weight: bold; }
        .fail { color: red; font-weight: bold; }
      </style>
    </head>
    <body>
      <h1>UNRDF Load Test Report</h1>
      <p>Generated: ${new Date().toISOString()}</p>
      <pre>${JSON.stringify(data, null, 2)}</pre>
    </body>
    </html>
  `;
}
