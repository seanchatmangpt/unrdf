/**
 * @file Jaeger API Client for OTEL Trace Validation
 * @module jaeger-client
 *
 * @description
 * Client for querying Jaeger tracing backend and validating distributed traces.
 */

/**
 * Jaeger API client
 */
export class JaegerClient {
  constructor(jaegerUrl) {
    this.baseUrl = jaegerUrl || 'http://localhost:16686';
    this.apiUrl = `${this.baseUrl}/api`;
  }

  /**
   * Get traces by service name
   */
  async getTraces(query) {
    const params = new URLSearchParams({
      service: query.service || 'unrdf-cli',
      limit: query.limit || 20,
      lookback: query.lookback || '1h',
    });

    if (query.operation) {
      params.append('operation', query.operation);
    }

    if (query.tags) {
      params.append('tags', JSON.stringify(query.tags));
    }

    if (query.minDuration) {
      params.append('minDuration', query.minDuration);
    }

    if (query.maxDuration) {
      params.append('maxDuration', query.maxDuration);
    }

    try {
      const response = await fetch(`${this.apiUrl}/traces?${params.toString()}`);

      if (!response.ok) {
        throw new Error(`Jaeger API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json();
      return data.data || [];
    } catch (error) {
      console.warn(`Failed to fetch traces from Jaeger: ${error.message}`);
      return [];
    }
  }

  /**
   * Get trace by trace ID
   */
  async getTrace(traceId) {
    try {
      const response = await fetch(`${this.apiUrl}/traces/${traceId}`);

      if (!response.ok) {
        throw new Error(`Jaeger API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json();
      return data.data?.[0] || null;
    } catch (error) {
      console.warn(`Failed to fetch trace ${traceId} from Jaeger: ${error.message}`);
      return null;
    }
  }

  /**
   * Get traces by trace ID (handles multiple traces with same ID)
   */
  async getTracesByTraceId(traceId) {
    try {
      const response = await fetch(`${this.apiUrl}/traces/${traceId}`);

      if (!response.ok) {
        throw new Error(`Jaeger API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json();
      return data.data || [];
    } catch (error) {
      console.warn(`Failed to fetch traces for ${traceId} from Jaeger: ${error.message}`);
      return [];
    }
  }

  /**
   * Get services registered in Jaeger
   */
  async getServices() {
    try {
      const response = await fetch(`${this.apiUrl}/services`);

      if (!response.ok) {
        throw new Error(`Jaeger API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json();
      return data.data || [];
    } catch (error) {
      console.warn(`Failed to fetch services from Jaeger: ${error.message}`);
      return [];
    }
  }

  /**
   * Get operations for a service
   */
  async getOperations(serviceName) {
    try {
      const response = await fetch(`${this.apiUrl}/services/${serviceName}/operations`);

      if (!response.ok) {
        throw new Error(`Jaeger API error: ${response.status} ${response.statusText}`);
      }

      const data = await response.json();
      return data.data || [];
    } catch (error) {
      console.warn(`Failed to fetch operations for ${serviceName} from Jaeger: ${error.message}`);
      return [];
    }
  }

  /**
   * Validate trace structure against expected spans
   */
  async validateTraceStructure(traceId, expectedSpans) {
    const trace = await this.getTrace(traceId);

    if (!trace) {
      throw new Error(`Trace not found: ${traceId}`);
    }

    const spans = trace.spans || [];
    const missingSpans = [];

    for (const expectedSpan of expectedSpans) {
      const span = spans.find(s =>
        s.operationName === expectedSpan ||
        s.name === expectedSpan
      );

      if (!span) {
        missingSpans.push(expectedSpan);
      }
    }

    if (missingSpans.length > 0) {
      throw new Error(`Missing expected spans: ${missingSpans.join(', ')}`);
    }

    return true;
  }

  /**
   * Validate span timing against performance SLA
   */
  async validateSpanTiming(traceId, operationName, maxDuration) {
    const trace = await this.getTrace(traceId);

    if (!trace) {
      throw new Error(`Trace not found: ${traceId}`);
    }

    const spans = trace.spans || [];
    const span = spans.find(s =>
      s.operationName === operationName ||
      s.name === operationName
    );

    if (!span) {
      throw new Error(`Span not found: ${operationName}`);
    }

    const duration = span.duration || 0; // in microseconds

    if (duration > maxDuration) {
      throw new Error(
        `Span ${operationName} duration ${duration}µs exceeds max ${maxDuration}µs`
      );
    }

    return { duration, maxDuration, passed: true };
  }

  /**
   * Validate trace context propagation between services
   */
  async validateTraceContextPropagation(traceId, expectedServices) {
    const traces = await this.getTracesByTraceId(traceId);

    if (!traces || traces.length === 0) {
      throw new Error(`No traces found for trace ID: ${traceId}`);
    }

    const servicesFound = new Set();

    for (const trace of traces) {
      const spans = trace.spans || [];
      for (const span of spans) {
        if (span.process?.serviceName) {
          servicesFound.add(span.process.serviceName);
        }
      }
    }

    const missingServices = expectedServices.filter(
      service => !servicesFound.has(service)
    );

    if (missingServices.length > 0) {
      throw new Error(`Missing expected services: ${missingServices.join(', ')}`);
    }

    return { services: Array.from(servicesFound), passed: true };
  }

  /**
   * Get span duration statistics
   */
  async getSpanDurationStats(serviceName, operationName, limit = 100) {
    const traces = await this.getTraces({
      service: serviceName,
      operation: operationName,
      limit,
    });

    const durations = [];

    for (const trace of traces) {
      const spans = trace.spans || [];
      for (const span of spans) {
        if (span.operationName === operationName || span.name === operationName) {
          durations.push(span.duration || 0);
        }
      }
    }

    if (durations.length === 0) {
      return {
        count: 0,
        min: 0,
        max: 0,
        avg: 0,
        p50: 0,
        p95: 0,
        p99: 0,
      };
    }

    const sorted = durations.sort((a, b) => a - b);
    const sum = durations.reduce((acc, d) => acc + d, 0);

    return {
      count: durations.length,
      min: sorted[0],
      max: sorted[sorted.length - 1],
      avg: sum / durations.length,
      p50: sorted[Math.floor(sorted.length * 0.50)],
      p95: sorted[Math.floor(sorted.length * 0.95)],
      p99: sorted[Math.floor(sorted.length * 0.99)],
    };
  }

  /**
   * Find error spans
   */
  async findErrorSpans(serviceName, limit = 50) {
    const traces = await this.getTraces({
      service: serviceName,
      tags: { error: true },
      limit,
    });

    const errorSpans = [];

    for (const trace of traces) {
      const spans = trace.spans || [];
      for (const span of spans) {
        const tags = span.tags || [];
        const hasError = tags.some(tag =>
          tag.key === 'error' && tag.value === true
        );

        if (hasError) {
          errorSpans.push({
            traceId: trace.traceID,
            spanId: span.spanID,
            operationName: span.operationName || span.name,
            duration: span.duration,
            tags: tags,
          });
        }
      }
    }

    return errorSpans;
  }

  /**
   * Check if Jaeger is healthy
   */
  async healthCheck() {
    try {
      const services = await this.getServices();
      return {
        healthy: true,
        servicesCount: services.length,
        services,
      };
    } catch (error) {
      return {
        healthy: false,
        error: error.message,
      };
    }
  }
}

export default JaegerClient;
