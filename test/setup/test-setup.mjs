/**
 * @file Test Setup Utilities
 * @description Setup and teardown helpers for UNRDF tests
 */

import { createStore } from '@unrdf/oxigraph'
import { createSampleStore } from '../vitest-helpers.mjs'

/**
 * Setup a fresh RDF store for testing
 * @param {Object} [options={}] - Setup options
 * @returns {import('n3').Store}
 */
export function setupRDFStore(options = {}) {
  const store = options.withData ? createSampleStore(true) : createStore()

  // Add custom initialization if needed
  if (options.initQuads) {
    for (const quad of options.initQuads) {
      store.addQuad(quad)
    }
  }

  return store
}

/**
 * Teardown RDF store
 * @param {import('n3').Store} store - Store to teardown
 */
export function teardownRDFStore(store) {
  if (store && typeof store.removeMatches === 'function') {
    store.removeMatches()
  }
}

/**
 * Setup browser environment for tests
 * @returns {Object} - Browser environment context
 */
export function setupBrowserEnvironment() {
  const context = {
    window: global.window || {},
    document: global.document || {},
    localStorage: new Map(),
    sessionStorage: new Map()
  }

  // Mock localStorage
  if (!global.localStorage) {
    global.localStorage = {
      getItem: (key) => context.localStorage.get(key) || null,
      setItem: (key, value) => context.localStorage.set(key, value),
      removeItem: (key) => context.localStorage.delete(key),
      clear: () => context.localStorage.clear(),
      get length() {
        return context.localStorage.size
      },
      key: (index) => {
        return Array.from(context.localStorage.keys())[index] || null
      }
    }
  }

  return context
}

/**
 * Teardown browser environment
 * @param {Object} context - Browser context to teardown
 */
export function teardownBrowserEnvironment(context) {
  if (context.localStorage) {
    context.localStorage.clear()
  }
  if (context.sessionStorage) {
    context.sessionStorage.clear()
  }
}

/**
 * Setup mock server for HTTP tests
 * @param {Object} [options={}] - Server options
 * @returns {Object} - Mock server instance
 */
export function setupMockServer(options = {}) {
  const handlers = new Map()
  const requests = []

  const server = {
    handlers,
    requests,

    on(method, path, handler) {
      const key = `${method.toUpperCase()}:${path}`
      handlers.set(key, handler)
    },

    get(path, handler) {
      this.on('GET', path, handler)
    },

    post(path, handler) {
      this.on('POST', path, handler)
    },

    async request(method, path, body, headers = {}) {
      const key = `${method.toUpperCase()}:${path}`
      const handler = handlers.get(key)

      const req = { method, path, body, headers, timestamp: Date.now() }
      requests.push(req)

      if (!handler) {
        return { statusCode: 404, body: { error: 'Not Found' } }
      }

      return await handler(req)
    },

    reset() {
      handlers.clear()
      requests.length = 0
    },

    getRequests() {
      return [...requests]
    }
  }

  return server
}

/**
 * Setup mock WebSocket for streaming tests
 * @returns {Object} - Mock WebSocket
 */
export function setupMockWebSocket() {
  const messages = []
  const listeners = {
    open: [],
    message: [],
    close: [],
    error: []
  }

  const ws = {
    readyState: 0, // CONNECTING

    addEventListener(event, handler) {
      if (listeners[event]) {
        listeners[event].push(handler)
      }
    },

    removeEventListener(event, handler) {
      if (listeners[event]) {
        const index = listeners[event].indexOf(handler)
        if (index > -1) {
          listeners[event].splice(index, 1)
        }
      }
    },

    send(data) {
      messages.push({ type: 'send', data, timestamp: Date.now() })
    },

    close(code, reason) {
      this.readyState = 3 // CLOSED
      this._emit('close', { code, reason })
    },

    // Test helpers
    _open() {
      this.readyState = 1 // OPEN
      this._emit('open', {})
    },

    _receive(data) {
      if (this.readyState === 1) {
        messages.push({ type: 'receive', data, timestamp: Date.now() })
        this._emit('message', { data })
      }
    },

    _error(error) {
      this._emit('error', { error })
    },

    _emit(event, data) {
      for (const handler of listeners[event]) {
        handler(data)
      }
    },

    _getMessages() {
      return [...messages]
    },

    _reset() {
      messages.length = 0
      this.readyState = 0
    }
  }

  return ws
}

/**
 * Setup test timeout handler
 * @param {number} timeout - Timeout in ms
 * @returns {Object} - Timeout controller
 */
export function setupTestTimeout(timeout = 5000) {
  let timer = null
  let timedOut = false

  return {
    start() {
      timer = setTimeout(() => {
        timedOut = true
      }, timeout)
    },

    clear() {
      if (timer) {
        clearTimeout(timer)
        timer = null
      }
    },

    isTimedOut() {
      return timedOut
    },

    reset() {
      this.clear()
      timedOut = false
    }
  }
}

/**
 * Create isolated test context
 * @returns {Object} - Test context with cleanup
 */
export function createIsolatedContext() {
  const resources = {
    stores: [],
    subscriptions: [],
    timers: [],
    servers: [],
    webSockets: []
  }

  return {
    addStore(store) {
      resources.stores.push(store)
      return store
    },

    addSubscription(sub) {
      resources.subscriptions.push(sub)
      return sub
    },

    addTimer(timer) {
      resources.timers.push(timer)
      return timer
    },

    addServer(server) {
      resources.servers.push(server)
      return server
    },

    addWebSocket(ws) {
      resources.webSockets.push(ws)
      return ws
    },

    cleanup() {
      // Clean stores
      for (const store of resources.stores) {
        teardownRDFStore(store)
      }

      // Clean subscriptions
      for (const sub of resources.subscriptions) {
        if (typeof sub.unsubscribe === 'function') {
          sub.unsubscribe()
        }
      }

      // Clear timers
      for (const timer of resources.timers) {
        clearTimeout(timer)
      }

      // Reset servers
      for (const server of resources.servers) {
        if (typeof server.reset === 'function') {
          server.reset()
        }
      }

      // Close WebSockets
      for (const ws of resources.webSockets) {
        if (typeof ws.close === 'function') {
          ws.close()
        }
      }

      // Clear arrays
      resources.stores.length = 0
      resources.subscriptions.length = 0
      resources.timers.length = 0
      resources.servers.length = 0
      resources.webSockets.length = 0
    }
  }
}
