/**
 * @file OpenTelemetry SDK Initialization
 * @module @unrdf/daemon/integrations/otel-sdk
 *
 * @description
 * Initializes OpenTelemetry SDK with Node SDK and configures OTLP exporter for distributed tracing and metrics
 */

import { NodeSDK } from '@opentelemetry/sdk-node';
import Resource from '@opentelemetry/resources';
import { ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION, ATTR_DEPLOYMENT_ENVIRONMENT } from '@opentelemetry/semantic-conventions';
import { BatchSpanProcessor } from '@opentelemetry/sdk-trace-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-grpc';
import { trace, metrics } from '@opentelemetry/api';
import { PeriodicExportingMetricReader } from '@opentelemetry/sdk-metrics';
import { OTLPMetricExporter } from '@opentelemetry/exporter-metrics-otlp-grpc';

let sdk = null;
let tracer = null;
let meter = null;

/**
 * Initialize OpenTelemetry SDK
 * @param {Object} config - Configuration options
 * @param {string} [config.serviceName] - Service name for telemetry
 * @param {string} [config.version] - Service version
 * @param {string} [config.environment] - Deployment environment
 * @param {string} [config.otlpEndpoint] - OTLP gRPC endpoint URL
 * @returns {Promise<NodeSDK>} Initialized NodeSDK instance
 */
export async function initializeOTelSDK(config = {}) {
  if (sdk) {
    return sdk;
  }

  console.log('[OTEL SDK] Initializing OpenTelemetry SDK...');

  // Create resource with service attributes
  const resource = Resource.default().merge(
    new Resource({
      [ATTR_SERVICE_NAME]: config.serviceName || 'unrdf-daemon',
      [ATTR_SERVICE_VERSION]: config.version || '26.4.3',
      [ATTR_DEPLOYMENT_ENVIRONMENT]: config.environment || 'development',
    })
  );

  // Create OTLP trace exporter
  const traceExporter = new OTLPTraceExporter({
    url: config.otlpEndpoint || 'localhost:4317',
  });

  // Create BatchSpanProcessor for optimal performance
  const spanProcessor = new BatchSpanProcessor(traceExporter, {
    scheduledDelayMillis: 0,
    exportTimeoutMillis: 100,
    maxQueueSize: 1000,
  });

  // Create OTLP metrics exporter
  const metricExporter = new OTLPMetricExporter({
    url: config.otlpEndpoint || 'localhost:4317',
  });

  // Create metric reader
  const metricReader = new PeriodicExportingMetricReader({
    exporter: metricExporter,
    exportIntervalMillis: 60000,
  });

  // Initialize NodeSDK with both traces and metrics
  sdk = new NodeSDK({
    resource,
    spanProcessor,
    metricReader,
    autoDetectResources: false,
  });

  // Start the SDK
  await sdk.start();

  // Export tracer and meter for cross-package use
  tracer = trace.getTracer(config.serviceName || 'unrdf-daemon', config.version || '26.4.3');
  meter = metrics.getMeter(config.serviceName || 'unrdf-daemon', config.version || '26.4.3');

  console.log('[OTEL SDK] OpenTelemetry SDK initialized successfully');
  console.log(`[OTEL SDK] Service: ${config.serviceName || 'unrdf-daemon'}`);
  console.log(`[OTEL SDK] Endpoint: ${config.otlpEndpoint || 'localhost:4317'}`);
  console.log('[OTEL SDK] Traces and metrics enabled');

  return sdk;
}

/**
 * Shutdown OpenTelemetry SDK
 * @returns {Promise<void>}
 */
export async function shutdownOTelSDK() {
  if (sdk) {
    console.log('[OTEL SDK] Shutting down OpenTelemetry SDK...');
    await sdk.shutdown();
    sdk = null;
    tracer = null;
    meter = null;
    console.log('[OTEL SDK] OpenTelemetry SDK shutdown complete');
  }
}

/**
 * Get the tracer instance
 * @returns {Tracer} OpenTelemetry tracer
 * @throws {Error} If SDK has not been initialized
 */
export function getTracer() {
  if (!tracer) {
    throw new Error('[OTEL SDK] Tracer not available. Call initializeOTelSDK() first.');
  }
  return tracer;
}

/**
 * Get the meter instance
 * @returns {Meter} OpenTelemetry meter
 * @throws {Error} If SDK has not been initialized
 */
export function getMeter() {
  if (!meter) {
    throw new Error('[OTEL SDK] Meter not available. Call initializeOTelSDK() first.');
  }
  return meter;
}

/**
 * Check if SDK has been initialized
 * @returns {boolean} True if SDK is initialized
 */
export function isSDKInitialized() {
  return sdk !== null;
}
