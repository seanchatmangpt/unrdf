import { vi, beforeEach } from 'vitest'
import { config } from '@vue/test-utils'

// Mock global fetch
global.fetch = vi.fn()

// Configure Vue Test Utils
config.global.mocks = {
  $route: {},
  $router: {
    push: vi.fn()
  }
}

// Reset mocks before each test
beforeEach(() => {
  vi.clearAllMocks()
  global.fetch.mockReset()

  // Default fetch mock that returns proper response for all endpoints
  global.fetch.mockImplementation((url) => {
    return Promise.resolve({
      ok: true,
      json: async () => {
        if (url.includes('/api/quads')) {
          return { quads: [], count: 0 }
        }
        if (url.includes('/api/stats')) {
          return { totalTriples: 0, uniqueSubjects: 0, uniquePredicates: 0 }
        }
        if (url.includes('/api/query')) {
          return { results: [] }
        }
        return {}
      }
    })
  })
})

// Mock WebSocket
global.WebSocket = class WebSocket {
  constructor(url) {
    this.url = url
    this.readyState = 0
    this.onopen = null
    this.onmessage = null
    this.onerror = null
    this.onclose = null

    setTimeout(() => {
      this.readyState = 1
      if (this.onopen) this.onopen()
    }, 0)
  }

  close() {
    this.readyState = 3
    if (this.onclose) this.onclose()
  }

  send(data) {
    // Mock send
  }
}

WebSocket.CONNECTING = 0
WebSocket.OPEN = 1
WebSocket.CLOSING = 2
WebSocket.CLOSED = 3
