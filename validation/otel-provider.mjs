/**
 * Minimal OpenTelemetry provider initialization for local validation runs.
 * Uses Console exporters to avoid external dependencies/services.
 */
import { NodeSDK } from '@opentelemetry/sdk-node';
import { SimpleSpanProcessor, ConsoleSpanExporter } from '@opentelemetry/sdk-trace-base';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';

let initialized = false;
let sdk;

export async function ensureProviderInitialized() {
  if (initialized) return;

  const resource = new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: 'unrdf',
  });

  sdk = new NodeSDK({
    resource,
    spanProcessor: new SimpleSpanProcessor(new ConsoleSpanExporter()),
  });

  await sdk.start();
  initialized = true;
}

export async function shutdownProvider() {
  if (!initialized) return;
  await sdk.shutdown().catch(() => {});
  initialized = false;
}


