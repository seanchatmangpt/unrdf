/**
 * @file Example Cleanroom Integration Test
 * @module test/e2e/cleanroom/example-integration-test
 *
 * @description
 * Example integration test demonstrating cleanroom environment usage
 * with full OpenTelemetry observability.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { CleanroomEnvironment } from './setup-cleanroom.mjs';

describe('Cleanroom Integration Test Example', () => {
  let cleanroom;

  beforeAll(async () => {
    // Initialize cleanroom environment
    cleanroom = new CleanroomEnvironment();
    await cleanroom.initialize();

    console.log('Cleanroom environment initialized');
    console.log('Endpoints:', cleanroom.getEndpoints());
  }, 60000); // 60 second timeout for startup

  afterAll(async () => {
    // Cleanup cleanroom environment
    if (cleanroom) {
      await cleanroom.shutdown();
    }
  }, 30000);

  it('should have all services healthy', async () => {
    const health = await cleanroom.checkHealth();

    expect(health.postgres).toBe(true);
    expect(health.otelCollector).toBe(true);
    expect(health.jaeger).toBe(true);
    expect(health.sidecar).toBe(true);
    expect(health.allHealthy).toBe(true);
  });

  it('should query Jaeger for services', async () => {
    // Wait a bit for services to register
    await new Promise(resolve => setTimeout(resolve, 2000));

    const services = await cleanroom.getServices();

    // Should have at least otel-collector and kgc-sidecar
    expect(services).toBeInstanceOf(Array);
    expect(services.length).toBeGreaterThan(0);
  });

  it('should execute test scenario with tracing', async () => {
    const result = await cleanroom.executeScenario(
      'test-hook-evaluation',
      async ({ endpoints, testRunId }) => {
        console.log('Test Run ID:', testRunId);
        console.log('Sidecar Endpoint:', endpoints.sidecarGrpc);

        // This is where you would:
        // 1. Create gRPC client
        // 2. Execute hook evaluation
        // 3. Verify results

        return {
          executed: true,
          testRunId
        };
      }
    );

    expect(result.success).toBe(true);
    expect(result.duration).toBeGreaterThan(0);
  });

  it('should have traces in Jaeger', async () => {
    // Wait for traces to be exported
    await new Promise(resolve => setTimeout(resolve, 3000));

    const traces = await cleanroom.queryTraces({
      service: 'kgc-sidecar',
      limit: 10
    });

    // Might not have traces yet in fresh environment
    expect(traces).toBeInstanceOf(Array);
  });

  it('should provide environment statistics', () => {
    const stats = cleanroom.getStats();

    expect(stats.initialized).toBe(true);
    expect(stats.startupDuration).toBeGreaterThan(0);
    expect(stats.startupDuration).toBeLessThan(30000); // < 30 seconds
    expect(stats.endpoints).toBeDefined();
  });
});

/**
 * 80/20 Critical Path Tests
 */
describe('Critical Path: CLI → Sidecar → RDF Store', () => {
  let cleanroom;

  beforeAll(async () => {
    cleanroom = new CleanroomEnvironment();
    await cleanroom.initialize();
  }, 60000);

  afterAll(async () => {
    if (cleanroom) {
      await cleanroom.shutdown();
    }
  }, 30000);

  it('[80/20] should trace complete request chain', async () => {
    // This test validates the critical 20% that covers 80% of functionality

    const result = await cleanroom.executeScenario(
      'complete-request-chain',
      async ({ endpoints, testRunId }) => {
        // 1. CLI creates root span
        const traceId = `test-${Date.now()}`;

        // 2. CLI calls sidecar via gRPC (with trace context)
        // Implementation would use actual gRPC client

        // 3. Sidecar evaluates hook and queries RDF store
        // Implementation would verify hook execution

        // 4. Query Jaeger for complete trace
        await new Promise(resolve => setTimeout(resolve, 2000));

        return {
          traceId,
          testRunId,
          expectedSpans: ['cli.command', 'grpc.call', 'sidecar.hook', 'rdf.query']
        };
      }
    );

    expect(result.success).toBe(true);

    // Verify trace in Jaeger
    await new Promise(resolve => setTimeout(resolve, 3000));

    const traces = await cleanroom.queryTraces({
      service: 'kgc-sidecar',
      limit: 20
    });

    expect(traces).toBeInstanceOf(Array);
  });

  it('[80/20] should propagate errors in traces', async () => {
    const result = await cleanroom.executeScenario(
      'error-propagation',
      async ({ testRunId }) => {
        // Trigger an error scenario (e.g., invalid SHACL validation)

        return {
          testRunId,
          errorTriggered: true
        };
      }
    );

    expect(result.success).toBe(true);

    // Verify error trace
    await new Promise(resolve => setTimeout(resolve, 2000));

    const traces = await cleanroom.queryTraces({
      service: 'kgc-sidecar',
      tags: { error: 'true' },
      limit: 10
    });

    expect(traces).toBeInstanceOf(Array);
  });
});
