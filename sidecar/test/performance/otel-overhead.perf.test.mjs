/**
 * @file OpenTelemetry Overhead Performance Tests
 * @description Measure tracing and instrumentation overhead on system resources
 *
 * SLOs:
 * - OTel CPU overhead: <5% additional CPU usage
 * - OTel memory overhead: <10MB additional memory
 * - Span creation latency: <1ms
 * - Context propagation overhead: <0.5ms
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import { performance } from 'node:perf_hooks'
import { trace, context, SpanStatusCode } from '@opentelemetry/api'
import { Resource } from '@opentelemetry/resources'
import { InMemorySpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base'
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node'
import v8 from 'node:v8'

/**
 * Setup OpenTelemetry for testing
 */
function setupOTel() {
  const exporter = new InMemorySpanExporter()
  const provider = new NodeTracerProvider({
    resource: new Resource({
      'service.name': 'otel-perf-test'
    })
  })

  provider.addSpanProcessor(new SimpleSpanProcessor(exporter))
  provider.register()

  return { provider, exporter }
}

/**
 * Get current memory usage in MB
 */
function getMemoryUsageMB() {
  const usage = process.memoryUsage()
  return {
    rss: usage.rss / 1024 / 1024,
    heapUsed: usage.heapUsed / 1024 / 1024,
    heapTotal: usage.heapTotal / 1024 / 1024,
    external: usage.external / 1024 / 1024
  }
}

/**
 * Estimate CPU usage over a time period
 */
async function measureCPU(durationMs, fn) {
  const startCPU = process.cpuUsage()
  const startTime = performance.now()

  await fn()

  const endCPU = process.cpuUsage(startCPU)
  const elapsedTime = (performance.now() - startTime) * 1000 // Convert to microseconds

  // CPU usage percentage (user + system)
  const cpuPercent = ((endCPU.user + endCPU.system) / elapsedTime) * 100

  return {
    cpuPercent,
    userCPU: endCPU.user,
    systemCPU: endCPU.system,
    elapsedTime: elapsedTime / 1000 // Back to ms
  }
}

describe('OpenTelemetry Overhead Performance', () => {
  let otel
  let tracer

  beforeAll(() => {
    otel = setupOTel()
    tracer = trace.getTracer('perf-test-tracer')
  })

  afterAll(async () => {
    await otel.provider.shutdown()
  })

  describe('Span Creation Overhead (<1ms SLO)', () => {
    it('should create spans in <1ms', () => {
      const iterations = 1000
      const measurements = []

      for (let i = 0; i < iterations; i++) {
        const start = performance.now()
        const span = tracer.startSpan('test-span')
        span.end()
        measurements.push(performance.now() - start)
      }

      const avg = measurements.reduce((sum, m) => sum + m, 0) / iterations
      const sorted = measurements.sort((a, b) => a - b)
      const p95 = sorted[Math.floor(sorted.length * 0.95)]
      const p99 = sorted[Math.floor(sorted.length * 0.99)]

      console.log('\n📊 Span Creation Performance:')
      console.log(`  Iterations: ${iterations}`)
      console.log(`  Avg: ${avg.toFixed(4)}ms`)
      console.log(`  P95: ${p95.toFixed(4)}ms (SLO: <1ms)`)
      console.log(`  P99: ${p99.toFixed(4)}ms`)

      expect(p95).toBeLessThan(1)
    })

    it('should create nested spans efficiently', () => {
      const iterations = 500
      const measurements = []

      for (let i = 0; i < iterations; i++) {
        const start = performance.now()

        const parentSpan = tracer.startSpan('parent')
        const childSpan = tracer.startSpan('child', {}, trace.setSpan(context.active(), parentSpan))
        const grandchildSpan = tracer.startSpan('grandchild', {}, trace.setSpan(context.active(), childSpan))

        grandchildSpan.end()
        childSpan.end()
        parentSpan.end()

        measurements.push(performance.now() - start)
      }

      const avg = measurements.reduce((sum, m) => sum + m, 0) / iterations

      console.log('\n🌳 Nested Span Creation:')
      console.log(`  Iterations: ${iterations}`)
      console.log(`  Avg (3 levels): ${avg.toFixed(4)}ms`)
      console.log(`  Per span: ${(avg / 3).toFixed(4)}ms`)

      expect(avg / 3).toBeLessThan(1) // Each span <1ms
    })
  })

  describe('Span Attribute Overhead', () => {
    it('should add attributes efficiently', () => {
      const iterations = 1000
      const measurements = []

      for (let i = 0; i < iterations; i++) {
        const span = tracer.startSpan('test-span')

        const start = performance.now()
        span.setAttribute('key1', 'value1')
        span.setAttribute('key2', 123)
        span.setAttribute('key3', true)
        span.setAttribute('key4', { nested: 'object' })
        measurements.push(performance.now() - start)

        span.end()
      }

      const avg = measurements.reduce((sum, m) => sum + m, 0) / iterations

      console.log('\n🏷️  Span Attribute Performance:')
      console.log(`  Avg (4 attributes): ${avg.toFixed(4)}ms`)
      console.log(`  Per attribute: ${(avg / 4).toFixed(4)}ms`)

      expect(avg).toBeLessThan(0.5)
    })
  })

  describe('Context Propagation Overhead (<0.5ms SLO)', () => {
    it('should propagate context quickly', () => {
      const iterations = 1000
      const measurements = []

      for (let i = 0; i < iterations; i++) {
        const span = tracer.startSpan('parent')

        const start = performance.now()
        const ctx = trace.setSpan(context.active(), span)
        context.with(ctx, () => {
          // Simulate work in context
          const activeSpan = trace.getSpan(context.active())
          activeSpan?.setAttribute('test', 'value')
        })
        measurements.push(performance.now() - start)

        span.end()
      }

      const avg = measurements.reduce((sum, m) => sum + m, 0) / iterations
      const sorted = measurements.sort((a, b) => a - b)
      const p95 = sorted[Math.floor(sorted.length * 0.95)]

      console.log('\n🔗 Context Propagation Performance:')
      console.log(`  Avg: ${avg.toFixed(4)}ms`)
      console.log(`  P95: ${p95.toFixed(4)}ms (SLO: <0.5ms)`)

      expect(p95).toBeLessThan(0.5)
    })
  })

  describe('Memory Overhead (<10MB SLO)', () => {
    it('should maintain low memory overhead with active tracing', async () => {
      // Force garbage collection if available
      if (global.gc) {
        global.gc()
      }

      await new Promise(resolve => setTimeout(resolve, 100))
      const baselineMemory = getMemoryUsageMB()

      console.log('\n💾 Baseline Memory:')
      console.log(`  RSS: ${baselineMemory.rss.toFixed(2)} MB`)
      console.log(`  Heap Used: ${baselineMemory.heapUsed.toFixed(2)} MB`)

      // Create many spans to measure memory impact
      const spans = []
      for (let i = 0; i < 10000; i++) {
        const span = tracer.startSpan(`span-${i}`)
        span.setAttribute('index', i)
        span.setAttribute('data', { value: i, timestamp: Date.now() })
        span.end()

        // Keep references to prevent GC (simulate active spans)
        if (i % 100 === 0) {
          spans.push(span)
        }
      }

      await new Promise(resolve => setTimeout(resolve, 100))
      const afterTracingMemory = getMemoryUsageMB()

      const memoryOverhead = afterTracingMemory.heapUsed - baselineMemory.heapUsed

      console.log('\n💾 After 10k Spans:')
      console.log(`  RSS: ${afterTracingMemory.rss.toFixed(2)} MB`)
      console.log(`  Heap Used: ${afterTracingMemory.heapUsed.toFixed(2)} MB`)
      console.log(`  Overhead: ${memoryOverhead.toFixed(2)} MB (SLO: <10MB)`)

      expect(memoryOverhead).toBeLessThan(10)
    })
  })

  describe('CPU Overhead (<5% SLO)', () => {
    it('should maintain <5% CPU overhead with active tracing', async () => {
      const iterations = 10000

      // Baseline: work without tracing
      const baselineCPU = await measureCPU(1000, async () => {
        for (let i = 0; i < iterations; i++) {
          // Simulate work
          const data = { index: i, value: Math.sqrt(i) }
          JSON.stringify(data)
        }
      })

      // With tracing: same work with spans
      const tracingCPU = await measureCPU(1000, async () => {
        for (let i = 0; i < iterations; i++) {
          const span = tracer.startSpan(`work-${i}`)
          span.setAttribute('index', i)

          // Same work
          const data = { index: i, value: Math.sqrt(i) }
          JSON.stringify(data)

          span.end()
        }
      })

      const cpuOverhead = tracingCPU.cpuPercent - baselineCPU.cpuPercent
      const overheadPercent = (cpuOverhead / baselineCPU.cpuPercent) * 100

      console.log('\n⚡ CPU Usage:')
      console.log(`  Baseline: ${baselineCPU.cpuPercent.toFixed(2)}%`)
      console.log(`  With tracing: ${tracingCPU.cpuPercent.toFixed(2)}%`)
      console.log(`  Overhead: ${cpuOverhead.toFixed(2)}%`)
      console.log(`  Relative overhead: ${overheadPercent.toFixed(2)}% (SLO: <5%)`)

      expect(overheadPercent).toBeLessThan(5)
    }, 30000)
  })

  describe('Span Export Performance', () => {
    it('should export spans efficiently', async () => {
      const spanCount = 1000

      // Create spans
      for (let i = 0; i < spanCount; i++) {
        const span = tracer.startSpan(`export-test-${i}`)
        span.setAttribute('index', i)
        span.end()
      }

      // Force export
      const start = performance.now()
      await otel.provider.forceFlush()
      const duration = performance.now() - start

      const exportedSpans = otel.exporter.getFinishedSpans()
      const throughput = (exportedSpans.length / duration) * 1000

      console.log('\n📤 Span Export Performance:')
      console.log(`  Spans exported: ${exportedSpans.length}`)
      console.log(`  Duration: ${duration.toFixed(2)}ms`)
      console.log(`  Throughput: ${throughput.toFixed(2)} spans/sec`)

      expect(throughput).toBeGreaterThan(1000) // At least 1000 spans/sec
    })
  })

  describe('Trace Context Extraction', () => {
    it('should extract trace context efficiently', () => {
      const iterations = 1000
      const measurements = []

      const mockHeaders = {
        traceparent: '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'
      }

      for (let i = 0; i < iterations; i++) {
        const start = performance.now()

        // Simulate W3C Trace Context extraction
        const traceparent = mockHeaders.traceparent.split('-')
        const traceId = traceparent[1]
        const spanId = traceparent[2]
        const flags = traceparent[3]

        measurements.push(performance.now() - start)
      }

      const avg = measurements.reduce((sum, m) => sum + m, 0) / iterations

      console.log('\n🔍 Trace Context Extraction:')
      console.log(`  Avg: ${avg.toFixed(4)}ms`)

      expect(avg).toBeLessThan(0.1)
    })
  })
})
