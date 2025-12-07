/**
 * AtomVM Library Entry Point
 *
 * Exports all public APIs for library usage.
 * This is the main entry point when importing @unrdf/atomvm as a library.
 *
 * For the browser application, see src/app.mjs
 *
 * @module @unrdf/atomvm
 */

// Re-export all public APIs for library usage
export { AtomVMRuntime } from './atomvm-runtime.mjs';
export { AtomVMNodeRuntime } from './node-runtime.mjs';
export { TerminalUI } from './terminal-ui.mjs';
export {
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus,
  waitForCOI
} from './service-worker-manager.mjs';

// Export App class for browser application usage
export { App } from './app.mjs';
