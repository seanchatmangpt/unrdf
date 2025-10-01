import { NodeSDK } from '@opentelemetry/sdk-node'
import { getNodeAutoInstrumentations } from '@opentelemetry/auto-instrumentations-node'
import { JaegerExporter } from '@opentelemetry/exporter-jaeger'

const sdk = new NodeSDK({
  traceExporter: new JaegerExporter(),
  instrumentations: [getNodeAutoInstrumentations()]
})

export function startTelemetry() {
  return sdk.start()
}

export function shutdownTelemetry() {
  return sdk.shutdown()
}