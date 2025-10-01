import { test, expect } from 'vitest'
import { BasicTracerProvider, InMemorySpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base'
import { traced, tracedSync } from '../telemetry/tracer.mjs'

test('tracedSync should create a span and return value', () => {
  const provider = new BasicTracerProvider()
  const exporter = new InMemorySpanExporter()
  provider.addSpanProcessor(new SimpleSpanProcessor(exporter))
  provider.register()

  const fn = (a, b) => a + b
  const tracedFn = tracedSync('sync-add', fn)
  const res = tracedFn(2, 3)
  expect(res).toBe(5)

  const spans = exporter.getFinishedSpans()
  expect(spans.length).toBe(1)
  expect(spans[0].name).toBe('sync-add')
})

test('traced should create a span for async function', async () => {
  const provider = new BasicTracerProvider()
  const exporter = new InMemorySpanExporter()
  provider.addSpanProcessor(new SimpleSpanProcessor(exporter))
  provider.register()

  const fn = async (x) => x * 2
  const tracedFn = traced('async-double', fn)
  const res = await tracedFn(4)
  expect(res).toBe(8)

  const spans = exporter.getFinishedSpans()
  expect(spans.length).toBe(1)
  expect(spans[0].name).toBe('async-double')
})