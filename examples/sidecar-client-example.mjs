/**
 * Sidecar Client Example
 *
 * Demonstrates how to use the KGC Sidecar client for remote knowledge graph operations.
 * Shows connection management, transactions, health checks, and error handling.
 */

import { SidecarClient } from '../src/sidecar/client.mjs';
import { ConnectionPool } from '../src/sidecar/connection-pool.mjs';
import { CircuitBreaker } from '../src/sidecar/circuit-breaker.mjs';
import { RetryStrategy } from '../src/sidecar/retry-strategy.mjs';
import { HealthCheck } from '../src/sidecar/health-check.mjs';

console.log('=== KGC Sidecar Client Examples ===\n');

// Example 1: Basic Sidecar Client Usage
console.log('1. Basic Sidecar Client\n');

const client = new SidecarClient({
  address: process.env.KGC_SIDECAR_ADDRESS || 'localhost:50051',
  timeout: 30000,
  maxRetries: 3,
  tls: {
    enabled: false, // Enable for production
  },
});

try {
  console.log('Connecting to sidecar...');
  await client.connect();
  console.log('✓ Connected successfully\n');

  // Health check
  console.log('Performing health check...');
  const health = await client.healthCheck();
  console.log('Health status:', health.status);
  console.log('Timestamp:', new Date(health.timestamp).toISOString());
  console.log();

  // Apply transaction
  console.log('Applying transaction...');
  const transactionResult = await client.applyTransaction(
    {
      additions: [
        {
          subject: 'http://example.org/Alice',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'http://example.org/Person',
          graph: 'http://example.org/graph/default',
        },
        {
          subject: 'http://example.org/Alice',
          predicate: 'http://example.org/name',
          object: '"Alice Smith"',
          graph: 'http://example.org/graph/default',
        },
      ],
      removals: [],
    },
    {
      validate: true,
      strictMode: true,
    }
  );

  console.log('Transaction committed:', transactionResult.committed);
  console.log('Receipt hash:', transactionResult.receipt.hash);
  console.log('Timestamp:', transactionResult.receipt.timestamp);
  console.log();

  // SPARQL query
  console.log('Executing SPARQL query...');
  const queryResults = await client.query(
    `
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE {
      ?person a ex:Person ;
        ex:name ?name .
    }
  `,
    {
      limit: 10,
      timeout: 5000,
    }
  );

  console.log('Query results:', queryResults);
  console.log();

  await client.disconnect();
  console.log('✓ Disconnected\n');
} catch (error) {
  console.error('Error:', error.message);
}

// Example 2: Connection Pooling
console.log('2. Connection Pooling\n');

const pool = new ConnectionPool({
  minConnections: 2,
  maxConnections: 10,
  idleTimeout: 60000,
  healthCheckInterval: 30000,
});

try {
  console.log('Initializing connection pool...');
  await pool.initialize({
    address: 'localhost:50051',
    timeout: 30000,
  });

  const poolStats = pool.getStats();
  console.log('Pool statistics:');
  console.log(`  Active: ${poolStats.active}`);
  console.log(`  Idle: ${poolStats.idle}`);
  console.log(`  Total: ${poolStats.total}`);
  console.log();

  // Acquire connections
  console.log('Acquiring connections...');
  const conn1 = await pool.acquire();
  const conn2 = await pool.acquire();

  console.log('Performing operations with pooled connections...');
  const [health1, health2] = await Promise.all([conn1.healthCheck(), conn2.healthCheck()]);

  console.log('Connection 1 health:', health1.status);
  console.log('Connection 2 health:', health2.status);
  console.log();

  // Release connections
  pool.release(conn1);
  pool.release(conn2);
  console.log('✓ Released connections\n');

  // Cleanup
  await pool.drain();
  await pool.clear();
  console.log('✓ Pool cleaned up\n');
} catch (error) {
  console.error('Pool error:', error.message);
}

// Example 3: Circuit Breaker Pattern
console.log('3. Circuit Breaker\n');

const breaker = new CircuitBreaker({
  threshold: 5, // Open after 5 failures
  resetTimeout: 30000, // Try again after 30s
  monitoringPeriod: 60000,
});

async function callSidecarWithBreaker() {
  try {
    return await breaker.execute(async () => {
      const client = new SidecarClient({ address: 'localhost:50051' });
      await client.connect();
      const result = await client.healthCheck();
      await client.disconnect();
      return result;
    });
  } catch (error) {
    return { status: 'FAILED', error: error.message };
  }
}

console.log('Circuit breaker state:', breaker.getState());

// Simulate successful calls
for (let i = 0; i < 3; i++) {
  const result = await callSidecarWithBreaker();
  console.log(`Call ${i + 1}:`, result.status);
}

const metrics = breaker.getMetrics();
console.log('\nCircuit breaker metrics:');
console.log(`  Successes: ${metrics.successCount}`);
console.log(`  Failures: ${metrics.failures}`);
console.log(`  Success rate: ${(metrics.successRate * 100).toFixed(2)}%`);
console.log();

// Example 4: Retry Strategy
console.log('4. Retry Strategy\n');

const retry = new RetryStrategy({
  maxRetries: 3,
  initialDelay: 1000,
  maxDelay: 30000,
  multiplier: 2,
  jitter: true,
});

async function unreliableOperation() {
  // Simulate 50% failure rate
  if (Math.random() > 0.5) {
    throw new Error('Simulated failure');
  }
  return { success: true };
}

try {
  console.log('Executing with retry strategy...');
  const result = await retry.execute(unreliableOperation);
  console.log('Result:', result);
  console.log('✓ Operation succeeded (possibly after retries)\n');
} catch (error) {
  console.error('Operation failed after retries:', error.message);
  console.log();
}

// Example 5: Health Monitoring
console.log('5. Health Monitoring\n');

const healthMonitor = new HealthCheck({
  checkInterval: 10000, // Check every 10s
  consecutiveFailures: 3, // Mark unhealthy after 3 failures
  startupGracePeriod: 5000, // Wait 5s before first check
});

// Define health check function
async function checkSidecarHealth() {
  try {
    const client = new SidecarClient({ address: 'localhost:50051' });
    await client.connect();
    const result = await client.healthCheck();
    await client.disconnect();
    return result.status === 'HEALTHY';
  } catch (error) {
    console.error('Health check failed:', error.message);
    return false;
  }
}

// Start health monitoring
console.log('Starting health monitor...');
await healthMonitor.start(checkSidecarHealth);

// Listen for status changes
healthMonitor.on('statusChange', status => {
  console.log(`Health status changed: ${status}`);
});

// Get current status
const currentStatus = healthMonitor.getStatus();
console.log('Current health status:', currentStatus);
console.log();

// Stop monitoring after 5 seconds
setTimeout(async () => {
  await healthMonitor.stop();
  console.log('✓ Health monitor stopped\n');
}, 5000);

// Example 6: Error Handling
console.log('6. Error Handling\n');

async function demonstrateErrorHandling() {
  const client = new SidecarClient({ address: 'localhost:50051' });

  try {
    await client.connect();

    // Simulate invalid transaction
    await client.applyTransaction({
      additions: [
        {
          subject: 'invalid', // Invalid IRI
          predicate: 'invalid',
          object: 'invalid',
        },
      ],
      removals: [],
    });
  } catch (error) {
    console.log('Caught error:', error.constructor.name);
    console.log('Message:', error.message);

    // Handle different error types
    if (error.message.includes('UNAVAILABLE')) {
      console.log('Action: Retry operation');
    } else if (error.message.includes('INVALID')) {
      console.log('Action: Fix input data');
    } else {
      console.log('Action: Log and alert');
    }
  } finally {
    await client.disconnect();
  }
}

await demonstrateErrorHandling();
console.log();

// Example 7: Production Configuration
console.log('7. Production Configuration Example\n');

const productionConfig = {
  address: 'kgc.example.com:443',
  timeout: 30000,
  maxRetries: 3,
  circuitBreaker: {
    enabled: true,
    threshold: 5,
    resetTimeout: 30000,
  },
  tls: {
    enabled: true,
    ca: '/path/to/ca.crt',
    cert: '/path/to/client.crt',
    key: '/path/to/client.key',
  },
  retry: {
    initialDelay: 1000,
    maxDelay: 30000,
    multiplier: 2,
    jitter: true,
  },
  observability: {
    serviceName: 'my-app',
    jaegerEndpoint: 'http://jaeger:14268/api/traces',
    metricsPort: 9464,
  },
};

console.log('Production configuration:');
console.log(JSON.stringify(productionConfig, null, 2));
console.log();

console.log('✓ All examples complete!\n');

console.log('=== Summary ===\n');
console.log('The KGC Sidecar client provides:');
console.log('  ✓ Reliable gRPC communication');
console.log('  ✓ Connection pooling for performance');
console.log('  ✓ Circuit breaker for resilience');
console.log('  ✓ Automatic retry with backoff');
console.log('  ✓ Continuous health monitoring');
console.log('  ✓ Comprehensive error handling');
console.log('  ✓ Production-ready configuration');
console.log();

/**
 * Usage in Production:
 *
 * 1. Set environment variables:
 *    export KGC_SIDECAR_ADDRESS=kgc.example.com:443
 *    export KGC_TLS_ENABLED=true
 *    export KGC_CA_CERT=/path/to/ca.crt
 *
 * 2. Deploy sidecar in Kubernetes:
 *    kubectl apply -f kgc-sidecar.yaml
 *
 * 3. Use connection pooling:
 *    const pool = new ConnectionPool({ maxConnections: 10 });
 *    await pool.initialize(config);
 *
 * 4. Enable observability:
 *    await initTelemetry({ serviceName: 'my-app' });
 *
 * 5. Monitor metrics:
 *    - Prometheus: http://localhost:9464/metrics
 *    - Jaeger: http://localhost:16686
 */
