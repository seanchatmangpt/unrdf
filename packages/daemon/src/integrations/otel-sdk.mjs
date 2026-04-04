/**
 * @file OpenTelemetry SDK Initialization
 * @module @unrdf/daemon/integrations/otel-sdk
 *
 * @description
 * Initializes OpenTelemetry SDK with Node SDK and configures OTLP exporter for distributed tracing
 */

import { NodeSDK } from '@opentelemetry/sdk-node';
import Resource from '@opentelemetry/resources';
import { ATTR_SERVICE_NAME, ATTR_SERVICE_VERSION, ATTR_DEPLOYMENT_ENVIRONMENT } from '@opentelemetry/semantic-conventions';
import { BatchSpanProcessor } from '@opentelemetry/sdk-trace-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-grpc';

let sdk = null;

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

  // Initialize NodeSDK
  sdk = new NodeSDK({
    resource,
    spanProcessor,
    autoDetectResources: false,
  });

  // Start the SDK
  await sdk.start();

  console.log('[OTEL SDK] OpenTelemetry SDK initialized successfully');
  console.log(`[OTEL SDK] Service: ${config.serviceName || 'unrdf-daemon'}`);
  console.log(`[OTEL SDK] Endpoint: ${config.otlpEndpoint || 'localhost:4317'}`);

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
    console.log('[OTEL SDK] OpenTelemetry SDK shutdown complete');
  }
}
