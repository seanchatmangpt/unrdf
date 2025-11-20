/**
 * README Example: OpenTelemetry Observability (lines 291-304)
 * Tests OTEL instrumentation
 */

import { ObservabilityManager } from '../../src/knowledge-engine/index.mjs';
import { trace } from '@opentelemetry/api';

/**
 *
 */
async function testObservability() {
  console.log('🧪 Testing Observability Example...');

  try {
    const obs = new ObservabilityManager();
    console.log('✅ Created Observability instance');

    // Automatic span creation for all operations
    const tracer = trace.getTracer('unrdf');
    console.log('✅ Got tracer');

    if (!tracer) {
      throw new Error('Tracer not available');
    }

    // Access performance metrics
    const metrics = obs.getPerformanceMetrics();
    console.log('✅ Got performance metrics');

    if (!metrics) {
      throw new Error('Metrics not available');
    }

    console.log(`  Latency p95: ${metrics.latency?.p95 || 0}ms`);
    console.log(`  Cache hit rate: ${(metrics.cacheHitRate || 0) * 100}%`);

    console.log('\n✅ Observability Example: PASSED\n');
    return true;
  } catch (error) {
    console.error('❌ Observability Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testObservability();
  process.exit(success ? 0 : 1);
}

export { testObservability };
