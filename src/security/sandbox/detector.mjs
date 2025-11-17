/**
 * @file Sandbox Executor Detector
 * @module security/sandbox/detector
 *
 * @description
 * Automatically detects the best sandbox executor based on:
 * - Node.js version and environment
 * - Available dependencies
 * - Platform capabilities (V8 isolates, Worker threads, etc.)
 * - Security requirements
 */

import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('sandbox-detector');

/**
 * Detect runtime environment
 * @returns {Object} Environment info
 */
export function detectEnvironment() {
  return tracer.startActiveSpan('detector.detectEnvironment', (span) => {
    try {
      const env = {
        isNode: typeof process !== 'undefined' && process.versions?.node,
        isBrowser: typeof window !== 'undefined',
        isWorker: typeof WorkerGlobalScope !== 'undefined',
        nodeVersion: process.versions?.node || null,
        v8Version: process.versions?.v8 || null,
        platform: process.platform || 'unknown',
        arch: process.arch || 'unknown'
      };

      span.setAttributes({
        'detector.isNode': env.isNode,
        'detector.isBrowser': env.isBrowser,
        'detector.nodeVersion': env.nodeVersion || 'unknown',
        'detector.platform': env.platform
      });

      span.setStatus({ code: 1 }); // OK
      return env;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Check if isolated-vm is available and working
 * @returns {Promise<boolean>} True if isolated-vm is available
 */
export async function checkIsolatedVm() {
  return tracer.startActiveSpan('detector.checkIsolatedVm', async (span) => {
    try {
      // Try to import isolated-vm
      const ivm = await import('isolated-vm').catch(() => null);
      const available = !!ivm;

      span.setAttribute('detector.isolatedVm.available', available);

      if (available) {
        // Quick smoke test
        try {
          const isolate = new ivm.default.Isolate({ memoryLimit: 8 });
          await isolate.dispose();
          span.setAttribute('detector.isolatedVm.working', true);
          span.setStatus({ code: 1 });
          return true;
        } catch (testError) {
          span.recordException(testError);
          span.setAttribute('detector.isolatedVm.working', false);
          span.setStatus({ code: 1 }); // Not an error, just not working
          return false;
        }
      }

      span.setStatus({ code: 1 });
      return false;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      return false;
    } finally {
      span.end();
    }
  });
}

/**
 * Check if Worker threads are available
 * @returns {Promise<boolean>} True if Worker threads are available
 */
export async function checkWorkerThreads() {
  return tracer.startActiveSpan('detector.checkWorkerThreads', async (span) => {
    try {
      const { Worker } = await import('worker_threads').catch(() => ({}));
      const available = !!Worker;

      span.setAttribute('detector.workerThreads.available', available);
      span.setStatus({ code: 1 });

      return available;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      return false;
    } finally {
      span.end();
    }
  });
}

/**
 * Check if vm2 is available (legacy)
 * @returns {Promise<boolean>} True if vm2 is available
 */
export async function checkVm2() {
  return tracer.startActiveSpan('detector.checkVm2', async (span) => {
    try {
      const vm2Module = await import('vm2').catch(() => null);
      const available = !!vm2Module?.VM;

      span.setAttribute('detector.vm2.available', available);
      span.setAttribute('detector.vm2.deprecated', true);
      span.setStatus({ code: 1 });

      return available;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      return false;
    } finally {
      span.end();
    }
  });
}

/**
 * Detect best available sandbox executor
 * Priority: isolated-vm > worker > vm2 (deprecated) > browser
 *
 * @param {Object} [options] - Detection options
 * @param {boolean} [options.preferIsolatedVm=true] - Prefer isolated-vm if available
 * @param {boolean} [options.allowVm2=false] - Allow deprecated vm2 (not recommended)
 * @param {boolean} [options.allowBrowser=true] - Allow browser executor
 * @returns {Promise<string>} Executor type ('isolated-vm' | 'worker' | 'vm2' | 'browser')
 */
export async function detectBestExecutor(options = {}) {
  return tracer.startActiveSpan('detector.detectBestExecutor', async (span) => {
    try {
      const {
        preferIsolatedVm = true,
        allowVm2 = false,
        allowBrowser = true
      } = options;

      const env = detectEnvironment();
      span.setAttributes({
        'detector.preferIsolatedVm': preferIsolatedVm,
        'detector.allowVm2': allowVm2,
        'detector.allowBrowser': allowBrowser
      });

      // Browser environment
      if (env.isBrowser && allowBrowser) {
        span.setAttribute('detector.executor', 'browser');
        span.setStatus({ code: 1 });
        return 'browser';
      }

      // Node.js environment - check capabilities in priority order
      if (env.isNode) {
        // 1. Try isolated-vm (best security and performance)
        if (preferIsolatedVm && await checkIsolatedVm()) {
          span.setAttribute('detector.executor', 'isolated-vm');
          span.setStatus({ code: 1 });
          return 'isolated-vm';
        }

        // 2. Try Worker threads (good isolation, available in Node 12+)
        if (await checkWorkerThreads()) {
          span.setAttribute('detector.executor', 'worker');
          span.setStatus({ code: 1 });
          return 'worker';
        }

        // 3. Fall back to vm2 if explicitly allowed (deprecated, security issues)
        if (allowVm2 && await checkVm2()) {
          span.setAttribute('detector.executor', 'vm2');
          span.setAttribute('detector.warning', 'Using deprecated vm2 - security issues present');
          span.setStatus({ code: 1 });
          console.warn(
            '[SECURITY WARNING] Using deprecated vm2 sandbox executor. ' +
            'This has known security vulnerabilities. ' +
            'Consider upgrading Node.js or installing isolated-vm.'
          );
          return 'vm2';
        }
      }

      // No suitable executor found
      const error = new Error(
        'No suitable sandbox executor available. ' +
        'Install isolated-vm or upgrade to Node.js 12+ for Worker threads.'
      );
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      throw error;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Create executor instance based on type
 * @param {string} executorType - Type of executor to create
 * @param {Object} [config] - Executor configuration
 * @returns {Promise<Object>} Executor instance
 */
export async function createExecutor(executorType, config = {}) {
  return tracer.startActiveSpan('detector.createExecutor', async (span) => {
    try {
      span.setAttribute('detector.executorType', executorType);

      let ExecutorClass;

      switch (executorType) {
        case 'isolated-vm':
          ExecutorClass = (await import('./isolated-vm-executor.mjs')).IsolatedVmExecutor;
          break;

        case 'worker':
          ExecutorClass = (await import('./worker-executor.mjs')).WorkerExecutor;
          break;

        case 'vm2':
          console.warn('[DEPRECATION] vm2 executor is deprecated and has security vulnerabilities');
          ExecutorClass = (await import('./vm2-executor.mjs')).Vm2Executor;
          break;

        case 'browser':
          ExecutorClass = (await import('./browser-executor.mjs')).BrowserExecutor;
          break;

        default:
          throw new Error(`Unknown executor type: ${executorType}`);
      }

      const executor = new ExecutorClass(config);
      span.setStatus({ code: 1 });
      return executor;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Auto-detect and create best executor
 * @param {Object} [config] - Executor configuration
 * @param {Object} [detectionOptions] - Detection options
 * @returns {Promise<Object>} Executor instance
 */
export async function createBestExecutor(config = {}, detectionOptions = {}) {
  return tracer.startActiveSpan('detector.createBestExecutor', async (span) => {
    try {
      const executorType = await detectBestExecutor(detectionOptions);
      span.setAttribute('detector.selectedExecutor', executorType);

      const executor = await createExecutor(executorType, config);
      span.setStatus({ code: 1 });

      return executor;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Get executor capabilities
 * @param {string} executorType - Executor type
 * @returns {Object} Capabilities object
 */
export function getExecutorCapabilities(executorType) {
  const capabilities = {
    'isolated-vm': {
      memoryIsolation: 'full',
      cpuIsolation: 'full',
      asyncSupport: true,
      wasmSupport: true,
      securityLevel: 'high',
      performance: 'high',
      overhead: 'low'
    },
    'worker': {
      memoryIsolation: 'partial',
      cpuIsolation: 'partial',
      asyncSupport: true,
      wasmSupport: false,
      securityLevel: 'medium',
      performance: 'medium',
      overhead: 'medium'
    },
    'vm2': {
      memoryIsolation: 'weak',
      cpuIsolation: 'none',
      asyncSupport: false,
      wasmSupport: false,
      securityLevel: 'low',
      performance: 'medium',
      overhead: 'low',
      deprecated: true,
      securityIssues: true
    },
    'browser': {
      memoryIsolation: 'partial',
      cpuIsolation: 'partial',
      asyncSupport: true,
      wasmSupport: true,
      securityLevel: 'medium',
      performance: 'low',
      overhead: 'high'
    }
  };

  return capabilities[executorType] || null;
}
