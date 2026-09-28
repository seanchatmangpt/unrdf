/**
 * @file OpenTelemetry SDK Plugin
 * @description Initialize OpenTelemetry SDK on server startup
 */

import { defineNitroPlugin, useRuntimeConfig } from '#imports'

// Dynamic imports for CommonJS OpenTelemetry modules
let NodeSDK, OTLPTraceExporter, Resource, SemanticResourceAttributes

async function loadOpenTelemetryModules() {
  const sdkNodePkg = await import('@opentelemetry/sdk-node')
  const exporterPkg = await import('@opentelemetry/exporter-otlp-http')
  const resourcesPkg = await import('@opentelemetry/resources')
  const semanticPkg = await import('@opentelemetry/semantic-conventions')

  NodeSDK = sdkNodePkg.NodeSDK || sdkNodePkg.default?.NodeSDK
  OTLPTraceExporter = exporterPkg.OTLPTraceExporter || exporterPkg.default?.OTLPTraceExporter
  Resource = resourcesPkg.Resource || resourcesPkg.default?.Resource
  SemanticResourceAttributes = semanticPkg.SemanticResourceAttributes || semanticPkg.default?.SemanticResourceAttributes
}

/**
 * Initialize OpenTelemetry SDK
 * @param {Object} nitroApp - Nitro app instance
 */
export default defineNitroPlugin(async (nitroApp) => {
  const config = useRuntimeConfig()

  // Skip if telemetry disabled
  if (!config.kgcEnableTelemetry) {
    console.log('[OTel] Telemetry disabled, skipping initialization')
    return
  }

  console.log('[OTel] Initializing SDK...')

  try {
    // Load OpenTelemetry modules
    await loadOpenTelemetryModules()

    const sdk = new NodeSDK({
      resource: new Resource({
        [SemanticResourceAttributes.SERVICE_NAME]: config.otelServiceName,
        [SemanticResourceAttributes.SERVICE_VERSION]: process.env.OTEL_SERVICE_VERSION || '1.0.0'
      }),
      traceExporter: new OTLPTraceExporter({
        url: `${config.otelEndpoint}/v1/traces`
      })
    })

    sdk.start()

    console.log('[OTel] SDK initialized successfully')
    console.log(`[OTel] Exporting to: ${config.otelEndpoint}`)

    // Graceful shutdown
    nitroApp.hooks.hook('close', async () => {
      console.log('[OTel] Shutting down SDK...')
      await sdk.shutdown()
    })
  } catch (error) {
    console.error('[OTel] SDK initialization failed:', error)
    // Don't throw - allow server to start even if OTel fails
  }
})
