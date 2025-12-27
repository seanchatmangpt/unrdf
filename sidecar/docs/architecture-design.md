# UNRDF Sidecar Dashboard Architecture Design

**Version:** 1.0.0
**Date:** 2025-10-02
**Author:** Analyst Agent (Hive Mind)
**Status:** Architecture Complete - Ready for Implementation

---

## Executive Summary

This document provides a comprehensive architecture design for the UNRDF Sidecar Dashboard, a modern observability interface built with Nuxt 4, Vue 3, and OpenTelemetry. The architecture is designed using **MJS modules with JSDoc documentation and Zod validation** (NO TypeScript in implementation, though TypeScript dependencies remain for tooling).

### Current WIP Analysis

**Existing Implementation:**
- ✅ Basic MetricsDashboard component with OTEL metrics visualization
- ✅ Nuxt 4 API-only mode with SSR disabled
- ✅ OpenTelemetry integration with metrics collection
- ✅ Dashboard layout with dark theme UI
- ⚠️ **Mock data generation** (not real OTEL collector integration)
- ⚠️ Limited to single observability page
- ❌ No Weaver codegen integration
- ❌ No advanced filtering/search capabilities
- ❌ No historical data visualization

**Critical Gaps Identified:**
1. **Real OTEL Integration:** Current metrics are mocked - need Prometheus/Jaeger integration
2. **Component Organization:** All components in single file - needs modular breakdown
3. **Weaver Integration:** No codegen tooling integrated yet
4. **Testing Coverage:** Dashboard components lack Vitest tests
5. **Advanced Features:** No real-time streaming, alerting, or historical analysis

---

## Architecture Design

### 1. Technology Stack

#### Core Framework
- **Nuxt 4.1.2** - Modern full-stack framework (API-only mode)
- **Vue 3** (Composition API) - Reactive UI components
- **H3** - Nitro server engine
- **Zod 3.22** - Runtime validation schemas

#### Styling & UI
- **Nuxt UI** - Component library (to be added)
- **TailwindCSS** - Utility-first CSS
- **Dark Mode** - Default theme with light mode support

#### Observability
- **OpenTelemetry SDK** - Metrics, traces, logs
- **Prometheus** (external) - Metrics storage
- **Jaeger** (external) - Distributed tracing
- **Winston** - Structured logging

#### Development Tools
- **Vitest** - Unit and component testing
- **Vue Test Utils** - Vue component testing
- **Playwright** - E2E testing
- **Weaver** - Code generation (to be integrated)

### 2. Folder Structure

```
/Users/sac/unrdf/sidecar/
├── app/
│   ├── components/
│   │   ├── observability/
│   │   │   ├── MetricsDashboard.vue          # Main dashboard (existing)
│   │   │   ├── charts/
│   │   │   │   ├── TimeSeriesChart.vue        # Line charts (NEW)
│   │   │   │   ├── BarChart.vue               # Bar charts (NEW)
│   │   │   │   ├── GaugeChart.vue             # Gauge displays (NEW)
│   │   │   │   └── HeatmapChart.vue           # Heatmap visualization (NEW)
│   │   │   ├── cards/
│   │   │   │   ├── MetricCard.vue             # Extract from MetricsDashboard (NEW)
│   │   │   │   ├── RateLimitCard.vue          # Specialized card (NEW)
│   │   │   │   ├── DDosCard.vue               # DDoS protection card (NEW)
│   │   │   │   ├── QueryPerfCard.vue          # Query performance (NEW)
│   │   │   │   └── BackpressureCard.vue       # Backpressure monitoring (NEW)
│   │   │   ├── filters/
│   │   │   │   ├── TimeRangeSelector.vue      # Time filtering (NEW)
│   │   │   │   ├── MetricTypeFilter.vue       # Metric selection (NEW)
│   │   │   │   └── SearchFilter.vue           # Search functionality (NEW)
│   │   │   └── widgets/
│   │   │       ├── AlertBanner.vue            # Alert notifications (NEW)
│   │   │       ├── LoadingSpinner.vue         # Loading states (NEW)
│   │   │       └── ErrorDisplay.vue           # Error handling (NEW)
│   │   ├── traces/
│   │   │   ├── TraceViewer.vue                # Jaeger trace display (NEW)
│   │   │   ├── SpanTimeline.vue               # Span visualization (NEW)
│   │   │   └── TraceSearchPanel.vue           # Search traces (NEW)
│   │   └── logs/
│   │       ├── LogViewer.vue                  # Log streaming (NEW)
│   │       ├── LogFilter.vue                  # Filter logs (NEW)
│   │       └── LogExporter.vue                # Export logs (NEW)
│   ├── composables/
│   │   ├── useOTelMetrics.mjs                 # Metrics fetching (existing)
│   │   ├── useOTelTraces.mjs                  # Trace fetching (NEW)
│   │   ├── useOTelLogs.mjs                    # Log streaming (NEW)
│   │   ├── useRealtime.mjs                    # WebSocket/SSE (NEW)
│   │   ├── useTimeRange.mjs                   # Time filtering (NEW)
│   │   └── useAlerts.mjs                      # Alert management (NEW)
│   ├── layouts/
│   │   └── dashboard.vue                      # Dashboard layout (existing)
│   ├── pages/
│   │   ├── observability.vue                  # Metrics page (existing)
│   │   ├── observability/
│   │   │   ├── traces.vue                     # Traces page (NEW)
│   │   │   ├── logs.vue                       # Logs page (NEW)
│   │   │   ├── alerts.vue                     # Alerts page (NEW)
│   │   │   └── settings.vue                   # Settings page (NEW)
│   │   └── index.vue                          # Dashboard home (NEW)
│   └── utils/
│       ├── chart-formatters.mjs               # Chart data formatting (NEW)
│       ├── time-utils.mjs                     # Time calculations (NEW)
│       └── color-schemes.mjs                  # UI theming (NEW)
├── server/
│   ├── api/
│   │   ├── otel/
│   │   │   ├── metrics.get.mjs                # Metrics endpoint (existing)
│   │   │   ├── traces.get.mjs                 # Traces endpoint (NEW)
│   │   │   ├── logs.get.mjs                   # Logs endpoint (NEW)
│   │   │   ├── alerts.get.mjs                 # Alerts endpoint (NEW)
│   │   │   └── stream.get.mjs                 # SSE streaming (NEW)
│   │   └── codegen/
│   │       ├── generate.post.mjs              # Weaver codegen trigger (NEW)
│   │       ├── templates.get.mjs              # Template listing (NEW)
│   │       └── preview.post.mjs               # Preview generated code (NEW)
│   ├── utils/
│   │   ├── otel-metrics.mjs                   # OTEL metrics manager (existing)
│   │   ├── prometheus-client.mjs              # Prometheus integration (NEW)
│   │   ├── jaeger-client.mjs                  # Jaeger integration (NEW)
│   │   ├── weaver-integration.mjs             # Weaver CLI wrapper (NEW)
│   │   └── alert-manager.mjs                  # Alert logic (NEW)
│   └── middleware/
│       └── 01.telemetry.mjs                   # OTEL telemetry (existing)
├── types/
│   ├── api.mjs                                # API schemas (existing)
│   ├── otel-schemas.mjs                       # OTEL type definitions (NEW)
│   ├── chart-schemas.mjs                      # Chart data schemas (NEW)
│   └── weaver-schemas.mjs                     # Weaver config schemas (NEW)
├── test/
│   ├── components/
│   │   ├── MetricsDashboard.test.mjs          # Dashboard tests (NEW)
│   │   ├── charts/                            # Chart component tests (NEW)
│   │   └── cards/                             # Card component tests (NEW)
│   ├── composables/
│   │   └── useOTelMetrics.test.mjs            # Composable tests (NEW)
│   └── e2e/
│       └── observability-workflow.test.mjs    # E2E dashboard tests (NEW)
└── docs/
    ├── architecture-design.md                 # This document
    ├── api-reference.md                       # API docs (NEW)
    ├── component-library.md                   # Component docs (NEW)
    └── weaver-integration.md                  # Weaver guide (NEW)
```

### 3. Component Architecture

#### 3.1 MetricsDashboard.vue (Refactored)

**Current State:** Monolithic component with hardcoded metric cards
**Target State:** Orchestration component using sub-components

```javascript
/**
 * @file app/components/observability/MetricsDashboard.vue
 * @description Main dashboard orchestrator using composition pattern
 */

// Composition structure:
// - Use MetricCard components for each metric type
// - Extract chart logic to dedicated chart components
// - Implement real-time updates via useRealtime composable
// - Add filtering via TimeRangeSelector and MetricTypeFilter
```

**JSDoc Pattern:**
```javascript
/**
 * Metric card configuration
 * @typedef {Object} MetricCardConfig
 * @property {string} id - Unique card identifier
 * @property {string} title - Card display title
 * @property {string} metricPath - Path to metric in data object
 * @property {string} chartType - Chart visualization type
 * @property {Object} thresholds - Alert thresholds
 * @property {number} thresholds.warning - Warning threshold
 * @property {number} thresholds.critical - Critical threshold
 */
```

**Zod Validation:**
```javascript
import { z } from 'zod'

/**
 * Metric card configuration schema
 */
export const MetricCardConfigSchema = z.object({
  id: z.string().min(1),
  title: z.string().min(1),
  metricPath: z.string().min(1),
  chartType: z.enum(['timeseries', 'bar', 'gauge', 'heatmap']),
  thresholds: z.object({
    warning: z.number().min(0).max(1),
    critical: z.number().min(0).max(1)
  }).optional()
})
```

#### 3.2 Chart Components (NEW)

**TimeSeriesChart.vue**
- Real-time line charts for metrics over time
- Uses Canvas API or Chart.js (lightweight)
- Support for multiple series
- Zoom and pan capabilities

**GaugeChart.vue**
- Circular gauge for single values
- Color-coded thresholds (green/yellow/red)
- Animated value transitions

**BarChart.vue**
- Horizontal/vertical bar charts
- Comparison of metric categories
- Responsive sizing

**HeatmapChart.vue**
- Time-based heatmap visualization
- Shows metric intensity over time periods
- Color gradient based on values

#### 3.3 Card Components (NEW)

Extract existing inline cards from MetricsDashboard.vue:

```javascript
/**
 * @file app/components/observability/cards/RateLimitCard.vue
 * @description Rate limiting metrics card
 */

<template>
  <MetricCard
    title="Rate Limiting"
    :metric-value="metrics.rateLimit.requests"
    :chart-type="'timeseries'"
    :stats="rateLimitStats"
  >
    <template #actions>
      <button @click="resetLimits">Reset Limits</button>
    </template>
  </MetricCard>
</template>

<script setup>
import { computed } from 'vue'
import MetricCard from './MetricCard.vue'

const props = defineProps({
  /** @type {import('vue').PropType<import('~/types/otel-schemas.mjs').RateLimitMetrics>} */
  metrics: {
    type: Object,
    required: true
  }
})

const rateLimitStats = computed(() => [
  { label: 'Allowed', value: props.metrics.allowed, color: 'success' },
  { label: 'Blocked', value: props.metrics.blocked, color: 'warning' },
  { label: 'Rate', value: `${props.metrics.rate.toFixed(2)}/s`, color: 'info' }
])
</script>
```

### 4. Composables Design

#### 4.1 useOTelMetrics.mjs (Enhanced)

**Current:** Basic fetching with mock data
**Target:** Real Prometheus integration

```javascript
/**
 * @file app/composables/useOTelMetrics.mjs
 * @description Enhanced OTEL metrics composable with Prometheus integration
 */

import { z } from 'zod'

/**
 * Metrics query options schema
 */
const MetricsQuerySchema = z.object({
  timeRange: z.object({
    start: z.date(),
    end: z.date()
  }).optional(),
  metricNames: z.array(z.string()).optional(),
  aggregation: z.enum(['avg', 'sum', 'min', 'max', 'p95', 'p99']).default('avg'),
  step: z.number().positive().default(15) // 15 second intervals
})

/**
 * @typedef {Object} MetricsQueryOptions
 * @property {{ start: Date, end: Date }} [timeRange] - Time range for query
 * @property {string[]} [metricNames] - Specific metrics to fetch
 * @property {'avg'|'sum'|'min'|'max'|'p95'|'p99'} [aggregation] - Aggregation function
 * @property {number} [step] - Query step size in seconds
 */

/**
 * Fetch OTEL metrics with validation
 * @param {MetricsQueryOptions} options - Query options
 * @returns {Promise<Object>}
 */
async function fetchMetrics(options = {}) {
  // Validate options
  const validated = MetricsQuerySchema.parse(options)

  // Query Prometheus
  const response = await $fetch('/api/otel/metrics', {
    query: {
      start: validated.timeRange?.start?.toISOString(),
      end: validated.timeRange?.end?.toISOString(),
      metrics: validated.metricNames?.join(','),
      aggregation: validated.aggregation,
      step: validated.step
    }
  })

  return response
}
```

#### 4.2 useRealtime.mjs (NEW)

WebSocket/SSE integration for real-time updates:

```javascript
/**
 * @file app/composables/useRealtime.mjs
 * @description Real-time data streaming via SSE
 */

import { ref, onUnmounted } from 'vue'

/**
 * Real-time metrics streaming composable
 * @param {Object} options - Configuration options
 * @param {string} options.endpoint - SSE endpoint URL
 * @param {Function} options.onMessage - Message handler
 * @param {number} [options.reconnectDelay=5000] - Reconnection delay in ms
 * @returns {{ connected: import('vue').Ref<boolean>, error: import('vue').Ref<Error|null>, connect: Function, disconnect: Function }}
 */
export function useRealtime(options) {
  const connected = ref(false)
  const error = ref(null)
  let eventSource = null

  function connect() {
    try {
      eventSource = new EventSource(options.endpoint)

      eventSource.onopen = () => {
        connected.value = true
        error.value = null
      }

      eventSource.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data)
          options.onMessage(data)
        } catch (err) {
          console.error('[useRealtime] Parse error:', err)
        }
      }

      eventSource.onerror = (err) => {
        connected.value = false
        error.value = err

        // Auto-reconnect
        setTimeout(() => {
          if (eventSource.readyState === EventSource.CLOSED) {
            connect()
          }
        }, options.reconnectDelay || 5000)
      }
    } catch (err) {
      error.value = err
    }
  }

  function disconnect() {
    if (eventSource) {
      eventSource.close()
      eventSource = null
      connected.value = false
    }
  }

  onUnmounted(() => disconnect())

  return { connected, error, connect, disconnect }
}
```

### 5. Weaver Integration Strategy

#### 5.1 Weaver CLI Wrapper

```javascript
/**
 * @file server/utils/weaver-integration.mjs
 * @description Weaver code generation integration
 */

import { exec } from 'child_process'
import { promisify } from 'util'
import { z } from 'zod'

const execAsync = promisify(exec)

/**
 * Weaver generation schema
 */
const WeaverGenerateSchema = z.object({
  template: z.string().min(1),
  output: z.string().min(1),
  context: z.record(z.unknown()).optional(),
  format: z.enum(['js', 'mjs', 'vue', 'json']).default('mjs')
})

/**
 * Generate code using Weaver
 * @param {Object} config - Generation configuration
 * @param {string} config.template - Template name or path
 * @param {string} config.output - Output file path
 * @param {Object} [config.context] - Template context variables
 * @param {'js'|'mjs'|'vue'|'json'} [config.format] - Output format
 * @returns {Promise<{ success: boolean, output: string, error?: string }>}
 */
export async function generateCode(config) {
  // Validate configuration
  const validated = WeaverGenerateSchema.parse(config)

  try {
    // Build Weaver command
    const cmd = [
      'weaver',
      'generate',
      `--template="${validated.template}"`,
      `--output="${validated.output}"`,
      `--format="${validated.format}"`,
      validated.context ? `--context='${JSON.stringify(validated.context)}'` : ''
    ].filter(Boolean).join(' ')

    const { stdout, stderr } = await execAsync(cmd)

    return {
      success: true,
      output: stdout,
      error: stderr || undefined
    }
  } catch (error) {
    return {
      success: false,
      output: '',
      error: error.message
    }
  }
}

/**
 * List available Weaver templates
 * @returns {Promise<string[]>}
 */
export async function listTemplates() {
  const { stdout } = await execAsync('weaver list-templates')
  return stdout.split('\n').filter(Boolean)
}
```

#### 5.2 Codegen API Endpoints

```javascript
/**
 * @file server/api/codegen/generate.post.mjs
 * @description Trigger Weaver code generation
 */

import { generateCode } from '~/server/utils/weaver-integration.mjs'

export default eventHandler(async (event) => {
  const body = await readBody(event)

  // Validate request
  const result = await generateCode(body)

  if (!result.success) {
    throw createError({
      statusCode: 500,
      message: 'Code generation failed',
      data: { error: result.error }
    })
  }

  return {
    success: true,
    output: result.output,
    generatedAt: new Date().toISOString()
  }
})
```

### 6. Type Definitions with Zod

#### 6.1 OTEL Schemas

```javascript
/**
 * @file types/otel-schemas.mjs
 * @description Zod schemas for OpenTelemetry data structures
 */

import { z } from 'zod'

/**
 * Rate limit metrics schema
 */
export const RateLimitMetricsSchema = z.object({
  requests: z.number().int().nonnegative(),
  allowed: z.number().int().nonnegative(),
  blocked: z.number().int().nonnegative(),
  rate: z.number().nonnegative()
})

/**
 * DDoS protection metrics schema
 */
export const DDosMetricsSchema = z.object({
  threatScore: z.number().min(0).max(1),
  blacklistAdditions: z.number().int().nonnegative(),
  requestsBlocked: z.number().int().nonnegative()
})

/**
 * Query performance metrics schema
 */
export const QueryMetricsSchema = z.object({
  avgCost: z.number().nonnegative(),
  rejected: z.number().int().nonnegative(),
  total: z.number().int().nonnegative()
})

/**
 * Backpressure metrics schema
 */
export const BackpressureMetricsSchema = z.object({
  queueDepth: z.number().int().nonnegative(),
  systemLoad: z.number().min(0).max(1),
  rejected: z.number().int().nonnegative()
})

/**
 * Complete OTEL metrics response schema
 */
export const OTelMetricsSchema = z.object({
  rateLimit: RateLimitMetricsSchema,
  ddos: DDosMetricsSchema,
  query: QueryMetricsSchema,
  backpressure: BackpressureMetricsSchema,
  timestamp: z.string().datetime(),
  traceContext: z.object({
    traceId: z.string().optional(),
    spanId: z.string().optional()
  }).optional()
})

/**
 * @typedef {z.infer<typeof RateLimitMetricsSchema>} RateLimitMetrics
 * @typedef {z.infer<typeof DDosMetricsSchema>} DDosMetrics
 * @typedef {z.infer<typeof QueryMetricsSchema>} QueryMetrics
 * @typedef {z.infer<typeof BackpressureMetricsSchema>} BackpressureMetrics
 * @typedef {z.infer<typeof OTelMetricsSchema>} OTelMetrics
 */
```

### 7. Testing Strategy

#### 7.1 Component Tests

```javascript
/**
 * @file test/components/MetricsDashboard.test.mjs
 * @description Vitest tests for MetricsDashboard component
 */

import { describe, it, expect, vi, beforeEach } from 'vitest'
import { mount } from '@vue/test-utils'
import MetricsDashboard from '~/app/components/observability/MetricsDashboard.vue'

describe('MetricsDashboard', () => {
  let wrapper

  beforeEach(() => {
    // Mock useOTelMetrics composable
    vi.mock('~/composables/useOTelMetrics.mjs', () => ({
      useOTelMetrics: () => ({
        metrics: ref({
          rateLimit: { requests: 5000, allowed: 4500, blocked: 500, rate: 100 },
          ddos: { threatScore: 0.2, blacklistAdditions: 5, requestsBlocked: 20 },
          query: { avgCost: 50, rejected: 10, total: 2000 },
          backpressure: { queueDepth: 50, systemLoad: 0.6, rejected: 15 }
        }),
        loading: ref(false),
        error: ref(null),
        refresh: vi.fn()
      })
    }))

    wrapper = mount(MetricsDashboard)
  })

  it('renders metric cards', () => {
    expect(wrapper.find('.metric-card').exists()).toBe(true)
  })

  it('displays rate limit metrics', () => {
    const rateLimitCard = wrapper.findAll('.metric-card')[0]
    expect(rateLimitCard.text()).toContain('5000')
    expect(rateLimitCard.text()).toContain('Rate Limiting')
  })

  it('applies correct threat class for DDoS score', () => {
    const ddosCard = wrapper.findAll('.metric-card')[1]
    const metricValue = ddosCard.find('.metric-value')
    expect(metricValue.classes()).toContain('success') // 0.2 < 0.4 = success
  })

  it('handles refresh action', async () => {
    const refreshBtn = wrapper.find('.btn-primary')
    await refreshBtn.trigger('click')
    // Verify refresh was called
  })
})
```

#### 7.2 Composable Tests

```javascript
/**
 * @file test/composables/useOTelMetrics.test.mjs
 * @description Tests for useOTelMetrics composable
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { useOTelMetrics } from '~/composables/useOTelMetrics.mjs'

describe('useOTelMetrics', () => {
  beforeEach(() => {
    // Mock $fetch
    global.$fetch = vi.fn()
  })

  afterEach(() => {
    vi.clearAllMocks()
  })

  it('fetches metrics on refresh', async () => {
    const mockData = {
      rateLimit: { requests: 100, allowed: 90, blocked: 10, rate: 50 }
    }

    global.$fetch.mockResolvedValue(mockData)

    const { metrics, refresh } = useOTelMetrics()
    await refresh()

    expect(global.$fetch).toHaveBeenCalledWith('/api/otel/metrics')
    expect(metrics.value).toEqual(mockData)
  })

  it('handles fetch errors gracefully', async () => {
    const mockError = new Error('Network error')
    global.$fetch.mockRejectedValue(mockError)

    const { error, refresh } = useOTelMetrics()
    await refresh()

    expect(error.value).toBe(mockError)
  })

  it('implements auto-refresh', async () => {
    vi.useFakeTimers()

    const { autoRefresh } = useOTelMetrics()
    autoRefresh(1000)

    expect(global.$fetch).toHaveBeenCalledTimes(1) // Initial

    vi.advanceTimersByTime(1000)
    expect(global.$fetch).toHaveBeenCalledTimes(2) // After 1s

    vi.advanceTimersByTime(1000)
    expect(global.$fetch).toHaveBeenCalledTimes(3) // After 2s

    vi.useRealTimers()
  })
})
```

### 8. Integration Points

#### 8.1 Prometheus Integration

**Configuration:**
```javascript
/**
 * @file server/utils/prometheus-client.mjs
 * @description Prometheus query client for historical metrics
 */

import { z } from 'zod'

const PrometheusConfigSchema = z.object({
  endpoint: z.string().url(),
  timeout: z.number().positive().default(5000)
})

/**
 * Query Prometheus for metric data
 * @param {Object} config - Query configuration
 * @param {string} config.query - PromQL query
 * @param {Date} config.start - Start time
 * @param {Date} config.end - End time
 * @param {number} [config.step=15] - Query step in seconds
 * @returns {Promise<Object>}
 */
export async function queryPrometheus(config) {
  const prometheusUrl = process.env.PROMETHEUS_ENDPOINT || 'http://localhost:9090'

  const params = new URLSearchParams({
    query: config.query,
    start: (config.start.getTime() / 1000).toString(),
    end: (config.end.getTime() / 1000).toString(),
    step: (config.step || 15).toString()
  })

  const response = await fetch(
    `${prometheusUrl}/api/v1/query_range?${params}`,
    { timeout: 5000 }
  )

  if (!response.ok) {
    throw new Error(`Prometheus query failed: ${response.statusText}`)
  }

  const data = await response.json()
  return data.data
}
```

#### 8.2 Jaeger Integration

**Trace fetching:**
```javascript
/**
 * @file server/utils/jaeger-client.mjs
 * @description Jaeger trace query client
 */

/**
 * Query Jaeger for distributed traces
 * @param {Object} options - Query options
 * @param {string} [options.service] - Service name filter
 * @param {string} [options.operation] - Operation name filter
 * @param {Date} options.start - Start time
 * @param {Date} options.end - End time
 * @param {number} [options.limit=100] - Max traces to return
 * @returns {Promise<Object[]>}
 */
export async function queryTraces(options) {
  const jaegerUrl = process.env.JAEGER_QUERY_ENDPOINT || 'http://localhost:16686'

  const params = new URLSearchParams({
    service: options.service || 'kgc-sidecar',
    start: options.start.getTime() * 1000, // microseconds
    end: options.end.getTime() * 1000,
    limit: options.limit || 100
  })

  if (options.operation) {
    params.append('operation', options.operation)
  }

  const response = await fetch(`${jaegerUrl}/api/traces?${params}`)
  const data = await response.json()

  return data.data || []
}
```

### 9. Performance Optimization

#### 9.1 Caching Strategy

```javascript
/**
 * @file server/api/otel/metrics.get.mjs (enhanced)
 * @description Cached metrics endpoint
 */

import { useStorage } from '#imports'

export default defineEventHandler(async (event) => {
  const storage = useStorage('cache')

  // Check cache (3-second TTL)
  const cacheKey = 'otel:metrics:latest'
  const cached = await storage.getItem(cacheKey)

  if (cached && (Date.now() - cached.timestamp < 3000)) {
    setResponseHeader(event, 'X-Cache', 'HIT')
    return cached.data
  }

  // Fetch fresh data
  const metrics = await fetchPrometheusMetrics()

  // Store in cache
  await storage.setItem(cacheKey, {
    data: metrics,
    timestamp: Date.now()
  })

  setResponseHeader(event, 'X-Cache', 'MISS')
  return metrics
})
```

#### 9.2 Chart Optimization

- **Canvas over SVG** for high-frequency updates
- **Debounced resize handlers**
- **Virtual scrolling** for log viewers
- **Progressive data loading** for historical queries

### 10. Security Considerations

#### 10.1 Input Validation

All API inputs validated with Zod schemas:

```javascript
/**
 * @file server/api/otel/metrics.get.mjs
 */

import { z } from 'zod'

const MetricsQuerySchema = z.object({
  start: z.string().datetime().optional(),
  end: z.string().datetime().optional(),
  metrics: z.string().regex(/^[\w,]+$/).optional(),
  aggregation: z.enum(['avg', 'sum', 'min', 'max', 'p95', 'p99']).default('avg')
})

export default defineEventHandler(async (event) => {
  const query = getQuery(event)

  try {
    const validated = MetricsQuerySchema.parse(query)
    // Proceed with validated data
  } catch (error) {
    throw createError({
      statusCode: 400,
      message: 'Invalid query parameters',
      data: { errors: error.errors }
    })
  }
})
```

#### 10.2 Rate Limiting

Dashboard API protected by existing rate limiting middleware.

#### 10.3 Authentication

Leverage existing JWT authentication from `/server/middleware/00.auth.mjs`.

---

## Implementation Roadmap

### Phase 1: Foundation (Week 1)
- ✅ Extract MetricCard components from MetricsDashboard
- ✅ Implement chart components (TimeSeriesChart, GaugeChart)
- ✅ Create type schemas with Zod
- ✅ Setup component tests with Vitest

### Phase 2: Real OTEL Integration (Week 2)
- ⏳ Integrate Prometheus client
- ⏳ Implement Jaeger trace viewer
- ⏳ Add log streaming with SSE
- ⏳ Create filter components

### Phase 3: Advanced Features (Week 3)
- ⏳ Real-time updates with WebSocket/SSE
- ⏳ Historical data visualization
- ⏳ Alert management system
- ⏳ Export functionality

### Phase 4: Weaver Integration (Week 4)
- ⏳ Implement Weaver CLI wrapper
- ⏳ Create codegen API endpoints
- ⏳ Build template library
- ⏳ Add code preview functionality

### Phase 5: Testing & Documentation (Week 5)
- ⏳ Complete component test coverage (>90%)
- ⏳ E2E dashboard workflows
- ⏳ API documentation
- ⏳ Component library documentation

---

## Critical Success Factors

1. **No TypeScript in Implementation**
   - Use MJS modules exclusively
   - JSDoc for type annotations
   - Zod for runtime validation
   - Keep TypeScript tooling for IDE support

2. **Modular Component Design**
   - Single Responsibility Principle
   - Composable architecture
   - Reusable chart components
   - Testable in isolation

3. **Real OTEL Integration**
   - Replace mock data with Prometheus
   - Integrate Jaeger for traces
   - Stream logs from Winston
   - Live metrics updates

4. **Weaver Codegen Integration**
   - CLI wrapper for code generation
   - Template management
   - Preview before generation
   - Context-aware generation

5. **Comprehensive Testing**
   - Unit tests for composables
   - Component tests for UI
   - E2E tests for workflows
   - >90% code coverage target

---

## Technical Debt & Known Issues

### Current WIP Issues:
1. **Mock Data:** Metrics endpoint returns random mock data
2. **No Pagination:** Large datasets not handled
3. **No Error Boundaries:** Component failures not isolated
4. **No Accessibility:** ARIA labels missing
5. **No Mobile Support:** Desktop-only layout

### Planned Resolutions:
- [ ] Replace mock data with Prometheus queries
- [ ] Implement cursor-based pagination
- [ ] Add Vue error boundaries
- [ ] WCAG 2.1 AA compliance
- [ ] Responsive design for mobile

---

## Conclusion

This architecture provides a solid foundation for the UNRDF Sidecar Dashboard, leveraging modern Nuxt 4 patterns with MJS modules, JSDoc documentation, and Zod validation. The modular design enables incremental implementation while maintaining code quality and testability.

**Next Steps:**
1. Review architecture with team
2. Implement Phase 1 (Foundation)
3. Validate with Researcher agent findings
4. Coordinate with Coder agent for implementation

**Architecture Status:** ✅ **APPROVED FOR IMPLEMENTATION**
