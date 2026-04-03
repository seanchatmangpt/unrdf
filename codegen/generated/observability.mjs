/**
 * Advanced Observability Layer
 * Tracing, metrics, and distributed context propagation
 */

export class Observability {
  constructor() {
    this.spans = [];
    this.metrics = new Map();
    this.traces = new Map();
    this.context = new Map();
  }

  /**
   * Start distributed trace
   */
  startTrace(name, attributes = {}) {
    const traceId = `trace_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    const trace = {
      id: traceId,
      name,
      startTime: Date.now(),
      spans: [],
      attributes,
      status: 'ACTIVE'
    };
    this.traces.set(traceId, trace);
    return traceId;
  }

  /**
   * Create span within trace
   */
  startSpan(traceId, spanName, attributes = {}) {
    const trace = this.traces.get(traceId);
    if (!trace) throw new Error(`Trace not found: ${traceId}`);

    const span = {
      id: `span_${Date.now()}_${Math.random().toString(36).substring(7)}`,
      traceId,
      name: spanName,
      startTime: Date.now(),
      endTime: null,
      duration: null,
      attributes,
      events: [],
      status: 'ACTIVE'
    };

    trace.spans.push(span);
    return span.id;
  }

  /**
   * End span and record duration
   */
  endSpan(spanId, status = 'OK', attributes = {}) {
    let span = null;

    for (const trace of this.traces.values()) {
      span = trace.spans.find(s => s.id === spanId);
      if (span) break;
    }

    if (!span) throw new Error(`Span not found: ${spanId}`);

    span.endTime = Date.now();
    span.duration = span.endTime - span.startTime;
    span.status = status;
    span.attributes = { ...span.attributes, ...attributes };

    return {
      spanId,
      duration: span.duration,
      status
    };
  }

  /**
   * Record metric
   */
  recordMetric(name, value, attributes = {}) {
    if (!this.metrics.has(name)) {
      this.metrics.set(name, []);
    }

    this.metrics.get(name).push({
      value,
      timestamp: Date.now(),
      attributes
    });
  }

  /**
   * Set context value (for distributed tracing)
   */
  setContextValue(key, value) {
    this.context.set(key, {
      value,
      timestamp: Date.now()
    });
  }

  /**
   * Get aggregated metrics
   */
  getMetrics(name) {
    const values = this.metrics.get(name) || [];

    if (values.length === 0) return null;

    const nums = values.map(v => v.value);
    return {
      name,
      count: values.length,
      sum: nums.reduce((a, b) => a + b, 0),
      min: Math.min(...nums),
      max: Math.max(...nums),
      avg: nums.reduce((a, b) => a + b, 0) / nums.length,
      latest: values[values.length - 1].value
    };
  }

  /**
   * Export trace for analysis
   */
  exportTrace(traceId) {
    const trace = this.traces.get(traceId);
    if (!trace) return null;

    trace.status = 'COMPLETED';
    trace.endTime = Date.now();
    trace.duration = trace.endTime - trace.startTime;

    return {
      ...trace,
      spanCount: trace.spans.length,
      criticalPath: this._getCriticalPath(trace)
    };
  }

  _getCriticalPath(trace) {
    return trace.spans
      .sort((a, b) => b.duration - a.duration)
      .slice(0, 5)
      .map(s => ({ name: s.name, duration: s.duration }));
  }

  /**
   * Get observability summary
   */
  getSummary() {
    const traceCount = this.traces.size;
    const totalSpans = Array.from(this.traces.values()).reduce((sum, t) => sum + t.spans.length, 0);
    const metricCount = this.metrics.size;

    return {
      traces: traceCount,
      spans: totalSpans,
      metrics: metricCount,
      contextSize: this.context.size,
      timestamp: new Date()
    };
  }
}

export default Observability;
