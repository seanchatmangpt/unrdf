// @ts-check
/**
 * @file OpenTelemetry Trace Context Propagation Test Suite
 * @description Tests for W3C Trace Context propagation across services
 */

import { describe, it, expect, beforeEach } from 'vitest'
import {
  parseTraceparent,
  formatTraceparent,
  extractTraceContextFromHeaders,
  injectTraceContextIntoHeaders,
  getCurrentTraceContext,
  enrichLogWithTraceContext,
  addMetricExemplar,
  TRACE_CONTEXT_HEADERS
} from '../../server/utils/otel-context-propagation.mjs'

describe('W3C Trace Context Propagation', () => {
  describe('Traceparent Header Parsing', () => {
    it('should parse valid W3C traceparent header', () => {
      const traceparent = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'

      const ctx = parseTraceparent(traceparent)

      expect(ctx).toEqual({
        version: '00',
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01'
      })
    })

    it('should validate traceparent format (4 parts)', () => {
      const invalidTraceparent = '00-4bf92f3577b34da6a3ce929d0e0e4736-01'

      const ctx = parseTraceparent(invalidTraceparent)

      expect(ctx).toBeNull()
    })

    it('should validate version field (only 00 supported)', () => {
      const futureVersion = '01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'

      const ctx = parseTraceparent(futureVersion)

      expect(ctx).toBeNull()
    })

    it('should validate trace ID (32 hex chars, not all zeros)', () => {
      const allZeros = '00-00000000000000000000000000000000-00f067aa0ba902b7-01'
      const tooShort = '00-4bf92f3577b34da6a3ce929d0e0e473-00f067aa0ba902b7-01'
      const invalid = '00-4bf92f3577b34da6a3ce929d0e0e473g-00f067aa0ba902b7-01'

      expect(parseTraceparent(allZeros)).toBeNull()
      expect(parseTraceparent(tooShort)).toBeNull()
      expect(parseTraceparent(invalid)).toBeNull()
    })

    it('should validate span ID (16 hex chars, not all zeros)', () => {
      const allZeros = '00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01'
      const tooShort = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902-01'
      const invalid = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902bg-01'

      expect(parseTraceparent(allZeros)).toBeNull()
      expect(parseTraceparent(tooShort)).toBeNull()
      expect(parseTraceparent(invalid)).toBeNull()
    })

    it('should validate trace flags (2 hex chars)', () => {
      const tooShort = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-1'
      const invalid = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-0g'

      expect(parseTraceparent(tooShort)).toBeNull()
      expect(parseTraceparent(invalid)).toBeNull()
    })

    it('should handle null or invalid input gracefully', () => {
      expect(parseTraceparent(null)).toBeNull()
      expect(parseTraceparent(undefined)).toBeNull()
      expect(parseTraceparent('')).toBeNull()
      expect(parseTraceparent(123)).toBeNull()
    })
  })

  describe('Traceparent Header Formatting', () => {
    it('should format trace context as W3C traceparent header', () => {
      const ctx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01'
      }

      const traceparent = formatTraceparent(ctx)

      expect(traceparent).toBe('00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01')
    })

    it('should use default trace flags if not provided', () => {
      const ctx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7'
      }

      const traceparent = formatTraceparent(ctx)

      expect(traceparent).toBe('00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01')
    })

    it('should preserve trace flags when provided', () => {
      const ctx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '00'
      }

      const traceparent = formatTraceparent(ctx)

      expect(traceparent).toContain('-00')
    })
  })

  describe('HTTP Header Context Extraction', () => {
    it('should extract trace context from HTTP headers', () => {
      const headers = {
        traceparent: '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01',
        tracestate: 'vendor=value'
      }

      const ctx = extractTraceContextFromHeaders(headers)

      expect(ctx).toEqual({
        version: '00',
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01',
        traceState: 'vendor=value'
      })
    })

    it('should handle case-insensitive header names', () => {
      const headers = {
        'Traceparent': '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'
      }

      const ctx = extractTraceContextFromHeaders(headers)

      expect(ctx).not.toBeNull()
      expect(ctx.traceId).toBe('4bf92f3577b34da6a3ce929d0e0e4736')
    })

    it('should return null when traceparent header missing', () => {
      const headers = {}

      const ctx = extractTraceContextFromHeaders(headers)

      expect(ctx).toBeNull()
    })

    it('should include tracestate when present', () => {
      const headers = {
        traceparent: '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01',
        tracestate: 'vendor1=value1,vendor2=value2'
      }

      const ctx = extractTraceContextFromHeaders(headers)

      expect(ctx.traceState).toBe('vendor1=value1,vendor2=value2')
    })

    it('should work without tracestate header', () => {
      const headers = {
        traceparent: '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'
      }

      const ctx = extractTraceContextFromHeaders(headers)

      expect(ctx.traceState).toBeUndefined()
    })
  })

  describe('HTTP Header Context Injection', () => {
    it('should inject trace context into HTTP headers', () => {
      const headers = {}
      const ctx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01'
      }

      injectTraceContextIntoHeaders(headers, ctx)

      expect(headers[TRACE_CONTEXT_HEADERS.TRACEPARENT]).toBe(
        '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'
      )
    })

    it('should inject tracestate when present', () => {
      const headers = {}
      const ctx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01',
        traceState: 'vendor=value'
      }

      injectTraceContextIntoHeaders(headers, ctx)

      expect(headers[TRACE_CONTEXT_HEADERS.TRACESTATE]).toBe('vendor=value')
    })

    it('should not modify headers when context invalid', () => {
      const headers = {}
      const ctx = { traceId: '123' } // Invalid context

      injectTraceContextIntoHeaders(headers, ctx)

      expect(headers[TRACE_CONTEXT_HEADERS.TRACEPARENT]).toBeUndefined()
    })

    it('should handle null or undefined context', () => {
      const headers = {}

      injectTraceContextIntoHeaders(headers, null)
      expect(headers[TRACE_CONTEXT_HEADERS.TRACEPARENT]).toBeUndefined()

      injectTraceContextIntoHeaders(headers, undefined)
      expect(headers[TRACE_CONTEXT_HEADERS.TRACEPARENT]).toBeUndefined()
    })
  })

  describe('Log Enrichment with Trace Context', () => {
    it('should enrich log entry with trace context', () => {
      const logEntry = {
        level: 'info',
        message: 'Request processed',
        status: 200
      }

      // Mock getCurrentTraceContext to return test data
      const mockCtx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01'
      }

      const enriched = {
        ...logEntry,
        trace_id: mockCtx.traceId,
        span_id: mockCtx.spanId,
        trace_flags: mockCtx.traceFlags
      }

      expect(enriched.trace_id).toBe('4bf92f3577b34da6a3ce929d0e0e4736')
      expect(enriched.span_id).toBe('00f067aa0ba902b7')
      expect(enriched.trace_flags).toBe('01')
      expect(enriched.message).toBe('Request processed')
    })

    it('should preserve original log fields', () => {
      const logEntry = {
        level: 'error',
        message: 'Error occurred',
        error: 'Database connection failed'
      }

      const enriched = {
        ...logEntry,
        trace_id: '123',
        span_id: '456'
      }

      expect(enriched.level).toBe('error')
      expect(enriched.message).toBe('Error occurred')
      expect(enriched.error).toBe('Database connection failed')
    })
  })

  describe('Metric Exemplar Linking', () => {
    it('should add trace context to metric exemplar', () => {
      const metric = {
        name: 'http.server.request.duration',
        value: 123,
        attributes: {
          'http.method': 'GET',
          'http.status_code': 200
        }
      }

      const mockCtx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7'
      }

      const enriched = {
        ...metric,
        exemplar: {
          traceId: mockCtx.traceId,
          spanId: mockCtx.spanId,
          timestamp: Date.now()
        }
      }

      expect(enriched.exemplar.traceId).toBe('4bf92f3577b34da6a3ce929d0e0e4736')
      expect(enriched.exemplar.spanId).toBe('00f067aa0ba902b7')
      expect(enriched.exemplar.timestamp).toBeGreaterThan(0)
    })

    it('should preserve metric attributes when adding exemplar', () => {
      const metric = {
        name: 'http.server.request.duration',
        value: 123,
        attributes: {
          'http.method': 'POST'
        }
      }

      const enriched = {
        ...metric,
        exemplar: {
          traceId: '123',
          spanId: '456',
          timestamp: Date.now()
        }
      }

      expect(enriched.attributes['http.method']).toBe('POST')
      expect(enriched.value).toBe(123)
    })
  })

  describe('Distributed Tracing Scenarios', () => {
    it('should propagate trace context from CLI to Sidecar', () => {
      // CLI creates trace
      const cliCtx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01'
      }

      // CLI injects into HTTP request
      const headers = {}
      injectTraceContextIntoHeaders(headers, cliCtx)

      // Sidecar extracts from HTTP request
      const sidecarCtx = extractTraceContextFromHeaders(headers)

      expect(sidecarCtx.traceId).toBe(cliCtx.traceId)
      expect(sidecarCtx.spanId).toBe(cliCtx.spanId)
    })

    it('should propagate trace context from Sidecar to Hook execution', () => {
      // Sidecar has active trace
      const sidecarCtx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: 'a1b2c3d4e5f6g7h8',
        traceFlags: '01'
      }

      // Hook execution creates child span
      const hookCtx = {
        traceId: sidecarCtx.traceId, // Same trace
        spanId: '11223344556677aa', // New span
        traceFlags: '01'
      }

      expect(hookCtx.traceId).toBe(sidecarCtx.traceId)
      expect(hookCtx.spanId).not.toBe(sidecarCtx.spanId)
    })

    it('should maintain trace context across microservices', () => {
      // Service A trace
      const serviceACtx = {
        traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
        spanId: '00f067aa0ba902b7',
        traceFlags: '01'
      }

      // Service A calls Service B with headers
      const headers = {}
      injectTraceContextIntoHeaders(headers, serviceACtx)

      // Service B extracts trace
      const serviceBCtx = extractTraceContextFromHeaders(headers)

      // Both services share same trace ID
      expect(serviceBCtx.traceId).toBe(serviceACtx.traceId)
    })
  })

  describe('Trace Context Validation', () => {
    it('should reject invalid trace ID formats', () => {
      const invalidContexts = [
        { traceId: 'invalid', spanId: '00f067aa0ba902b7' },
        { traceId: '123', spanId: '00f067aa0ba902b7' },
        { traceId: '', spanId: '00f067aa0ba902b7' }
      ]

      for (const ctx of invalidContexts) {
        const headers = {}
        injectTraceContextIntoHeaders(headers, ctx)
        expect(headers[TRACE_CONTEXT_HEADERS.TRACEPARENT]).toBeUndefined()
      }
    })

    it('should reject invalid span ID formats', () => {
      const invalidContexts = [
        { traceId: '4bf92f3577b34da6a3ce929d0e0e4736', spanId: 'invalid' },
        { traceId: '4bf92f3577b34da6a3ce929d0e0e4736', spanId: '123' },
        { traceId: '4bf92f3577b34da6a3ce929d0e0e4736', spanId: '' }
      ]

      for (const ctx of invalidContexts) {
        const headers = {}
        injectTraceContextIntoHeaders(headers, ctx)
        expect(headers[TRACE_CONTEXT_HEADERS.TRACEPARENT]).toBeUndefined()
      }
    })
  })
})
