/**
 * @file KGC Probe - Main entry point
 * @module @unrdf/kgc-probe
 *
 * @description
 * KGC Surface Probe provides runtime capability detection and performance
 * benchmarking for Knowledge Graph Computation environments.
 *
 * Available probes:
 * - WASM: WebAssembly capabilities and performance
 *
 * @example
 * import { probeWasm } from '@unrdf/kgc-probe';
 *
 * const observations = await probeWasm({
 *   samples: 100,
 *   timeout: 5000,
 *   maxMemoryMB: 1024
 * });
 *
 * console.log(observations);
 */

export { probeFilesystem, guardPath } from './probes/filesystem.mjs';
export { probeWasm, ObservationSchema, WasmProbeConfigSchema } from './probes/wasm.mjs';

export default {
  probeFilesystem: (await import('./probes/filesystem.mjs')).probeFilesystem,
  probeWasm: (await import('./probes/wasm.mjs')).probeWasm,
};
