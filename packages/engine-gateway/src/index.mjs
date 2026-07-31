/**
 * @file μ(O) Engine Gateway - Main Exports
 * @module @unrdf/engine-gateway
 */

export { EngineGateway } from './gateway.mjs'
export { detectOperationType, N3_ONLY_OPS, OXIGRAPH_OPS } from './operation-detector.mjs'
export { validateN3Usage, validateOxigraphUsage } from './validators.mjs'
