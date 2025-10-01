/**
 * OpenTelemetry Instrumentation for KGC Sidecar
 *
 * Automatically instruments Node.js application with OTEL SDK
 * Exports traces, metrics, and logs to OTEL Collector
 *
 * Loaded via NODE_OPTIONS="--require ./otel-instrumentation.mjs"
 */

import { NodeSDK } from '@opentelemetry/sdk-node';
import { getNodeAutoInstrumentations } from '@opentelemetry/auto-instrumentations-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-grpc';
import { OTLPMetricExporter } from '@opentelemetry/exporter-metrics-otlp-grpc';
import { PeriodicExportingMetricReader } from '@opentelemetry/sdk-metrics';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';
import { diag, DiagConsoleLogger, DiagLogLevel } from '@opentelemetry/api';

// Configure diagnostic logging
const logLevel = process.env.OTEL_LOG_LEVEL || 'info';
const logLevelMap = {
  'debug': DiagLogLevel.DEBUG,
  'info': DiagLogLevel.INFO,
  'warn': DiagLogLevel.WARN,
  'error': DiagLogLevel.ERROR
};
diag.setLogger(new DiagConsoleLogger(), logLevelMap[logLevel] || DiagLogLevel.INFO);

// Resource attributes
const resource = new Resource({
  [SemanticResourceAttributes.SERVICE_NAME]: process.env.OTEL_SERVICE_NAME || 'kgc-sidecar',
  [SemanticResourceAttributes.SERVICE_VERSION]: process.env.SERVICE_VERSION || '2.0.0',
  [SemanticResourceAttributes.SERVICE_INSTANCE_ID]: process.env.HOSTNAME || 'sidecar-1',
  [SemanticResourceAttributes.DEPLOYMENT_ENVIRONMENT]: process.env.DEPLOYMENT_ENVIRONMENT || 'cleanroom',
  [SemanticResourceAttributes.PROCESS_PID]: process.pid,
  [SemanticResourceAttributes.PROCESS_RUNTIME_NAME]: 'nodejs',
  [SemanticResourceAttributes.PROCESS_RUNTIME_VERSION]: process.version
});

// OTLP endpoint
const otlpEndpoint = process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'http://otel-collector:4317';

// Trace exporter
const traceExporter = new OTLPTraceExporter({
  url: `${otlpEndpoint}/v1/traces`,
  headers: {}
});

// Metric exporter
const metricExporter = new OTLPMetricExporter({
  url: `${otlpEndpoint}/v1/metrics`,
  headers: {}
});

// Metric reader with periodic export
const metricReader = new PeriodicExportingMetricReader({
  exporter: metricExporter,
  exportIntervalMillis: 10000, // Export every 10 seconds
  exportTimeoutMillis: 5000     // Timeout after 5 seconds
});

// Initialize OpenTelemetry SDK
const sdk = new NodeSDK({
  resource,
  traceExporter,
  metricReader,
  instrumentations: [
    getNodeAutoInstrumentations({
      // gRPC instrumentation
      '@opentelemetry/instrumentation-grpc': {
        enabled: true
      },

      // HTTP instrumentation
      '@opentelemetry/instrumentation-http': {
        enabled: true,
        requestHook: (span, request) => {
          // Add custom attributes to HTTP spans
          span.setAttribute('http.user_agent', request.headers['user-agent']);
        }
      },

      // DNS instrumentation
      '@opentelemetry/instrumentation-dns': {
        enabled: true
      },

      // Net instrumentation
      '@opentelemetry/instrumentation-net': {
        enabled: true
      },

      // Disable unwanted instrumentations
      '@opentelemetry/instrumentation-fs': {
        enabled: false // Too verbose for testing
      }
    })
  ]
});

// Start SDK
sdk.start();

// Graceful shutdown on process termination
process.on('SIGTERM', async () => {
  console.log('SIGTERM received, shutting down OpenTelemetry SDK...');
  try {
    await sdk.shutdown();
    console.log('OpenTelemetry SDK shutdown complete');
  } catch (error) {
    console.error('Error shutting down OpenTelemetry SDK:', error);
  } finally {
    process.exit(0);
  }
});

process.on('SIGINT', async () => {
  console.log('SIGINT received, shutting down OpenTelemetry SDK...');
  try {
    await sdk.shutdown();
    console.log('OpenTelemetry SDK shutdown complete');
  } catch (error) {
    console.error('Error shutting down OpenTelemetry SDK:', error);
  } finally {
    process.exit(0);
  }
});

console.log('OpenTelemetry instrumentation initialized');
console.log(`Service: ${resource.attributes[SemanticResourceAttributes.SERVICE_NAME]}`);
console.log(`Exporting to: ${otlpEndpoint}`);

export default sdk;
