/**
 * @file Secure Sandbox with isolated-vm
 * @description High-security JavaScript sandbox using V8 isolates with WASM support
 */

import ivm from 'isolated-vm'
import { trace, context } from '@opentelemetry/api'
import { SandboxError } from './errors.mjs'

const tracer = trace.getTracer('secure-sandbox')

/**
 * @typedef {Object} SandboxConfig
 * @property {number} memoryLimit - Memory limit in MB (default: 128)
 * @property {number} timeout - Execution timeout in ms (default: 5000)
 * @property {boolean} enableWasm - Enable WASM support (default: true)
 * @property {boolean} enableAsync - Enable async/await (default: false)
 */

/**
 * Secure sandbox using isolated-vm
 */
export class SecureSandbox {
  /**
   * @param {SandboxConfig} config
   */
  constructor(config = {}) {
    this.config = {
      memoryLimit: config.memoryLimit || 128,
      timeout: config.timeout || 5000,
      enableWasm: config.enableWasm !== false,
      enableAsync: config.enableAsync || false
    }

    /** @type {Map<string, ivm.Isolate>} */
    this.isolates = new Map()

    /** @type {Map<string, ivm.Context>} */
    this.contexts = new Map()
  }

  /**
   * Create a new isolate for effect execution
   * @param {string} effectId
   * @returns {Promise<void>}
   */
  async createIsolate(effectId) {
    return tracer.startActiveSpan('createIsolate', async (span) => {
      try {
        span.setAttribute('effectId', effectId)
        span.setAttribute('memoryLimit', this.config.memoryLimit)

        // Create isolated V8 instance
        const isolate = new ivm.Isolate({
          memoryLimit: this.config.memoryLimit,
          inspector: false, // Disable debugging for security
          onCatastrophicError: (msg) => {
            span.recordException(new Error(`Catastrophic error: ${msg}`))
            this.destroyIsolate(effectId)
          }
        })

        // Create context with minimal globals
        const context = await isolate.createContext()

        // Setup safe global environment
        const jail = context.global
        await jail.set('global', jail.derefInto())

        // Add safe console for debugging
        await jail.set('console', new ivm.Reference({
          log: (...args) => console.log('[Sandbox]', ...args),
          error: (...args) => console.error('[Sandbox]', ...args),
          warn: (...args) => console.warn('[Sandbox]', ...args)
        }))

        // Add JSON support
        await jail.set('JSON', new ivm.Reference({
          parse: (str) => JSON.parse(str),
          stringify: (obj) => JSON.stringify(obj)
        }))

        this.isolates.set(effectId, isolate)
        this.contexts.set(effectId, context)

        span.setStatus({ code: 1 }) // OK
      } catch (error) {
        span.recordException(error)
        span.setStatus({ code: 2, message: error.message })
        throw new SandboxError(`Failed to create isolate: ${error.message}`)
      } finally {
        span.end()
      }
    })
  }

  /**
   * Register effect code in the sandbox
   * @param {string} effectId
   * @param {string} code
   * @returns {Promise<void>}
   */
  async registerEffect(effectId, code) {
    return tracer.startActiveSpan('registerEffect', async (span) => {
      try {
        span.setAttribute('effectId', effectId)
        span.setAttribute('codeLength', code.length)

        if (!this.contexts.has(effectId)) {
          await this.createIsolate(effectId)
        }

        const context = this.contexts.get(effectId)

        // Compile and execute effect code
        const script = await this.isolates.get(effectId).compileScript(code)
        await script.run(context, { timeout: this.config.timeout })

        span.setStatus({ code: 1 })
      } catch (error) {
        span.recordException(error)
        span.setStatus({ code: 2, message: error.message })

        if (error.message?.includes('Script execution timed out')) {
          throw new SandboxError('Effect registration timed out', { cause: error })
        }

        throw new SandboxError(`Failed to register effect: ${error.message}`, { cause: error })
      } finally {
        span.end()
      }
    })
  }

  /**
   * Execute effect with input data
   * @param {string} effectId
   * @param {Object} input
   * @returns {Promise<any>}
   */
  async executeEffect(effectId, input) {
    return tracer.startActiveSpan('executeEffect', async (span) => {
      try {
        span.setAttribute('effectId', effectId)

        const context = this.contexts.get(effectId)
        if (!context) {
          throw new SandboxError(`Effect ${effectId} not found`)
        }

        const isolate = this.isolates.get(effectId)

        // Set input data
        const inputRef = await isolate.compileScript(
          `JSON.parse('${JSON.stringify(input)}')`
        )
        const inputData = await inputRef.run(context, { timeout: 1000 })

        // Execute effect function
        const executeScript = await isolate.compileScript(`
          (function() {
            if (typeof effect === 'function') {
              return effect(${JSON.stringify(input)});
            } else if (typeof effect === 'object' && typeof effect.execute === 'function') {
              return effect.execute(${JSON.stringify(input)});
            } else {
              throw new Error('Effect must be a function or object with execute method');
            }
          })()
        `)

        const result = await executeScript.run(context, {
          timeout: this.config.timeout,
          reference: true
        })

        // Copy result back to main isolate
        const output = await result.copy()

        span.setStatus({ code: 1 })
        return output

      } catch (error) {
        span.recordException(error)
        span.setStatus({ code: 2, message: error.message })

        if (error.message?.includes('Script execution timed out')) {
          throw new SandboxError('Effect execution timed out', { cause: error })
        }

        throw new SandboxError(`Failed to execute effect: ${error.message}`, { cause: error })
      } finally {
        span.end()
      }
    })
  }

  /**
   * Execute WASM module in sandbox
   * @param {string} effectId
   * @param {Uint8Array} wasmBytes
   * @param {Object} input
   * @returns {Promise<any>}
   */
  async executeWasm(effectId, wasmBytes, input) {
    return tracer.startActiveSpan('executeWasm', async (span) => {
      try {
        if (!this.config.enableWasm) {
          throw new SandboxError('WASM support is disabled')
        }

        span.setAttribute('effectId', effectId)
        span.setAttribute('wasmSize', wasmBytes.length)

        const context = this.contexts.get(effectId)
        const isolate = this.isolates.get(effectId)

        if (!context || !isolate) {
          throw new SandboxError(`Effect ${effectId} not found`)
        }

        // Create WASM module in isolate
        const wasmArrayBuffer = await isolate.compileScript(
          `new Uint8Array([${Array.from(wasmBytes).join(',')}]).buffer`
        ).run(context)

        // Instantiate WASM module
        const wasmScript = await isolate.compileScript(`
          (async function() {
            const module = await WebAssembly.instantiate(wasmBuffer);
            return module.instance.exports.main(${JSON.stringify(input)});
          })()
        `)

        const result = await wasmScript.run(context, {
          timeout: this.config.timeout,
          promise: true
        })

        span.setStatus({ code: 1 })
        return result

      } catch (error) {
        span.recordException(error)
        span.setStatus({ code: 2, message: error.message })
        throw new SandboxError(`WASM execution failed: ${error.message}`, { cause: error })
      } finally {
        span.end()
      }
    })
  }

  /**
   * Destroy isolate and free resources
   * @param {string} effectId
   */
  async destroyIsolate(effectId) {
    return tracer.startActiveSpan('destroyIsolate', async (span) => {
      try {
        span.setAttribute('effectId', effectId)

        const isolate = this.isolates.get(effectId)
        if (isolate) {
          isolate.dispose()
          this.isolates.delete(effectId)
          this.contexts.delete(effectId)
        }

        span.setStatus({ code: 1 })
      } catch (error) {
        span.recordException(error)
        span.setStatus({ code: 2, message: error.message })
      } finally {
        span.end()
      }
    })
  }

  /**
   * Get memory usage for effect
   * @param {string} effectId
   * @returns {Promise<Object>}
   */
  async getMemoryUsage(effectId) {
    const isolate = this.isolates.get(effectId)
    if (!isolate) {
      throw new SandboxError(`Effect ${effectId} not found`)
    }

    const heapStats = await isolate.getHeapStatistics()
    return {
      used: heapStats.used_heap_size,
      total: heapStats.total_heap_size,
      limit: heapStats.heap_size_limit,
      percentage: (heapStats.used_heap_size / heapStats.heap_size_limit) * 100
    }
  }

  /**
   * Cleanup all isolates
   */
  async cleanup() {
    for (const effectId of this.isolates.keys()) {
      await this.destroyIsolate(effectId)
    }
  }
}
