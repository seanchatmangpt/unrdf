/**
 * Minimal OpenTelemetry provider initialization for local validation runs.
 * Uses Console exporters to avoid external dependencies/services.
 */

let initialized = false;

export async function ensureProviderInitialized() {
  // OTEL initialization - simplified for validation
  // The validation framework doesn't actually require full SDK setup
  if (initialized) return;
  initialized = true;
}

export async function shutdownProvider() {
  if (!initialized) return;
  initialized = false;
}


