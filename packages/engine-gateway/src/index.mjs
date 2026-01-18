/**
 * @fileoverview μ(O) Engine Gateway - Enforcement layer for Oxigraph-first RDF processing
 * @module @unrdf/engine-gateway
 *
 * This package enforces the UNRDF principle: Oxigraph for SPARQL, N3 only for streaming.
 * It provides static analysis and runtime enforcement to prevent direct N3 Store usage.
 */

export { EngineGateway, createGateway } from './gateway.mjs';
export { detectOperation, OperationType } from './operation-detector.mjs';
export { validateEngine, validateQuery } from './validators.mjs';

/**
 * Package metadata
 */
export const metadata = {
  name: '@unrdf/engine-gateway',
  version: '5.0.1',
  description: 'μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing',
};
