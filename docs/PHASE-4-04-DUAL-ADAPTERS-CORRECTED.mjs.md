# Phase 4.4 (CORRECTED): Dual Adapters - Transparent Runtime Switching

## Overview

JavaScript code should not know or care whether it's talking to a real OTP node or the browser AtomVM simulation. This is achieved through **adapters** that implement a shared interface.

---

## Adapter Interface (JSDoc)

All adapters implement this interface:

```javascript
/**
 * @typedef {Object} ProtocolAdapter
 * @property {() => Promise<void>} connect - Connect to backend
 * @property {() => Promise<void>} disconnect - Disconnect from backend
 * @property {() => boolean} isConnected - Check connection status
 * @property {(command: Command) => Promise<Response>} send - Send command
 * @property {(topic: string, handler: EventHandler) => () => void} subscribe - Subscribe to events
 * @property {() => Promise<HealthStatus>} getHealth - Get system health
 */

/**
 * @typedef {Object} Command
 * @property {'command'} type
 * @property {string | number} id
 * @property {Object} payload
 * @property {string} payload.action
 * @property {Object} [payload.params]
 * @property {Object} [meta]
 * @property {number} [meta.timeout_ms]
 */

/**
 * @typedef {Object} Response
 * @property {'response'} type
 * @property {string | number} id
 * @property {Object} payload
 * @property {boolean} payload.success
 * @property {any} [payload.data]
 * @property {string} [payload.error]
 * @property {Object} [meta]
 * @property {number} [meta.duration_ms]
 */

/**
 * @typedef {Object} Event
 * @property {'event'} type
 * @property {null} id
 * @property {Object} payload
 * @property {string} payload.event_type
 * @property {Object} payload.details
 * @property {Object} [meta]
 */

/**
 * @typedef {Command | Response | Event} ProtocolMessage
 */

/**
 * @typedef {Object} HealthStatus
 * @property {'ok' | 'degraded' | 'down'} status
 * @property {string} [message]
 * @property {Object} [details]
 */

/**
 * @callback EventHandler
 * @param {Event} event
 * @returns {void}
 */

/**
 * @callback Unsubscribe
 * @returns {void}
 */
```

---

## Real Adapter (Production - WebSocket to OTP)

```javascript
// adapters/real-adapter.mjs

/**
 * Adapter for connecting to real Erlang/OTP node via WebSocket
 */
export class RealAdapter {
  /**
   * @param {string} [url] - WebSocket URL
   */
  constructor(url = 'ws://localhost:8080/rdf-protocol') {
    this.url = url;
    /** @type {WebSocket | null} */
    this.ws = null;
    /** @type {Map<string | number, (response: Response) => void>} */
    this.pendingRequests = new Map();
    /** @type {number} */
    this.messageId = 0;
    /** @type {Map<string, Set<EventHandler>>} */
    this.eventListeners = new Map();
    /** @type {number} */
    this.retryCount = 0;
    this.maxRetries = 5;
  }

  /**
   * Connect to WebSocket server
   * @returns {Promise<void>}
   */
  async connect() {
    return new Promise((resolve, reject) => {
      try {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
          console.log('Connected to real Erlang backend');
          this.retryCount = 0;
          resolve();
        };

        this.ws.onmessage = (event) => {
          this.#handleMessage(JSON.parse(event.data));
        };

        this.ws.onerror = (error) => {
          console.error('WebSocket error:', error);
          reject(error);
        };

        this.ws.onclose = () => {
          console.log('Disconnected from real backend');
          this.#attemptReconnect();
        };
      } catch (error) {
        reject(error);
      }
    });
  }

  /**
   * Disconnect from WebSocket
   * @returns {Promise<void>}
   */
  async disconnect() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  /**
   * Check if connected
   * @returns {boolean}
   */
  isConnected() {
    return this.ws !== null && this.ws.readyState === WebSocket.OPEN;
  }

  /**
   * Send command to server
   * @param {Command} command
   * @returns {Promise<Response>}
   */
  async send(command) {
    if (!this.isConnected()) {
      throw new Error('Not connected to server');
    }

    const messageId = ++this.messageId;
    const messageWithId = {
      ...command,
      id: messageId,
      meta: {
        ...command.meta,
        timestamp: new Date().toISOString()
      }
    };

    return new Promise((resolve, reject) => {
      const timeout = command.meta?.timeout_ms ?? 5000;
      const timer = setTimeout(() => {
        this.pendingRequests.delete(messageId);
        reject(new Error(`QUERY_TIMEOUT after ${timeout}ms`));
      }, timeout);

      this.pendingRequests.set(messageId, (response) => {
        clearTimeout(timer);
        resolve(response);
      });

      this.ws.send(JSON.stringify(messageWithId));
    });
  }

  /**
   * Subscribe to events
   * @param {string} topic
   * @param {EventHandler} handler
   * @returns {Unsubscribe}
   */
  subscribe(topic, handler) {
    if (!this.eventListeners.has(topic)) {
      this.eventListeners.set(topic, new Set());
    }
    this.eventListeners.get(topic).add(handler);

    // Return unsubscribe function
    return () => {
      const listeners = this.eventListeners.get(topic);
      if (listeners) {
        listeners.delete(handler);
      }
    };
  }

  /**
   * Get system health
   * @returns {Promise<HealthStatus>}
   */
  async getHealth() {
    if (!this.isConnected()) {
      return { status: 'down', message: 'Not connected' };
    }

    try {
      const response = await this.send({
        type: 'command',
        id: 'health-check',
        payload: { action: 'health' }
      });

      if (response.payload.success) {
        return { status: 'ok' };
      } else {
        return { status: 'degraded', message: response.payload.error };
      }
    } catch (error) {
      return { status: 'down', message: String(error) };
    }
  }

  /**
   * Handle incoming message
   * @private
   * @param {ProtocolMessage} message
   * @returns {void}
   */
  #handleMessage(message) {
    if (message.type === 'response') {
      const handler = this.pendingRequests.get(message.id);
      if (handler) {
        this.pendingRequests.delete(message.id);
        handler(message);
      }
    } else if (message.type === 'event') {
      const listeners = this.eventListeners.get(message.payload.event_type);
      if (listeners) {
        listeners.forEach(handler => handler(message));
      }
    }
  }

  /**
   * Attempt reconnection with exponential backoff
   * @private
   * @returns {Promise<void>}
   */
  async #attemptReconnect() {
    if (this.retryCount >= this.maxRetries) {
      console.error('Max reconnection attempts reached');
      return;
    }

    const delay = Math.pow(2, this.retryCount) * 1000;
    this.retryCount++;

    console.log(`Reconnecting in ${delay}ms...`);
    await new Promise(resolve => setTimeout(resolve, delay));

    try {
      await this.connect();
    } catch (error) {
      console.error('Reconnection failed:', error);
      await this.#attemptReconnect();
    }
  }
}
```

---

## Simulated Adapter (Browser - WebSocket to AtomVM WASM)

```javascript
// adapters/simulated-adapter.mjs

/**
 * Adapter for connecting to AtomVM simulation in browser
 */
export class SimulatedAdapter {
  constructor() {
    this.connected = false;
    /** @type {Map<string | number, (response: Response) => void>} */
    this.pendingRequests = new Map();
    /** @type {Map<string, Set<EventHandler>>} */
    this.eventListeners = new Map();
    this.messageId = 0;
  }

  /**
   * Connect to AtomVM WASM instance
   * @returns {Promise<void>}
   */
  async connect() {
    // AtomVM is already running in WASM
    // Just establish message channel

    if (typeof window !== 'undefined' && window.atomvm_port) {
      window.atomvm_port.onmessage = (message) => {
        this.#handleMessage(JSON.parse(message));
      };
      this.connected = true;
      console.log('Connected to AtomVM simulation');
    } else {
      throw new Error(
        'AtomVM not loaded. Did you forget to load atomvm.js? ' +
        'Or are you in Node.js (not browser)?'
      );
    }
  }

  /**
   * Disconnect from AtomVM
   * @returns {Promise<void>}
   */
  async disconnect() {
    this.connected = false;
  }

  /**
   * Check if connected
   * @returns {boolean}
   */
  isConnected() {
    return this.connected;
  }

  /**
   * Send command to AtomVM
   * @param {Command} command
   * @returns {Promise<Response>}
   */
  async send(command) {
    if (!this.isConnected()) {
      throw new Error('Not connected to AtomVM');
    }

    const messageId = ++this.messageId;
    const messageWithId = {
      ...command,
      id: messageId,
      meta: {
        ...command.meta,
        timestamp: new Date().toISOString()
      }
    };

    return new Promise((resolve, reject) => {
      const timeout = command.meta?.timeout_ms ?? 5000;
      const timer = setTimeout(() => {
        this.pendingRequests.delete(messageId);
        reject(new Error(`QUERY_TIMEOUT after ${timeout}ms`));
      }, timeout);

      this.pendingRequests.set(messageId, (response) => {
        clearTimeout(timer);
        resolve(response);
      });

      window.atomvm_port.send(JSON.stringify(messageWithId));
    });
  }

  /**
   * Subscribe to events
   * @param {string} topic
   * @param {EventHandler} handler
   * @returns {Unsubscribe}
   */
  subscribe(topic, handler) {
    if (!this.eventListeners.has(topic)) {
      this.eventListeners.set(topic, new Set());
    }
    this.eventListeners.get(topic).add(handler);

    return () => {
      const listeners = this.eventListeners.get(topic);
      if (listeners) {
        listeners.delete(handler);
      }
    };
  }

  /**
   * Get system health
   * @returns {Promise<HealthStatus>}
   */
  async getHealth() {
    if (!this.isConnected()) {
      return { status: 'down', message: 'AtomVM not connected' };
    }
    // AtomVM is always healthy if connected
    return { status: 'ok' };
  }

  /**
   * Handle incoming message from AtomVM
   * @private
   * @param {ProtocolMessage} message
   * @returns {void}
   */
  #handleMessage(message) {
    if (message.type === 'response') {
      const handler = this.pendingRequests.get(message.id);
      if (handler) {
        this.pendingRequests.delete(message.id);
        handler(message);
      }
    } else if (message.type === 'event') {
      const listeners = this.eventListeners.get(message.payload.event_type);
      if (listeners) {
        listeners.forEach(handler => handler(message));
      }
    }
  }
}
```

---

## Protocol Client (Adapter Factory)

```javascript
// client.mjs

import { RealAdapter } from './adapters/real-adapter.mjs';
import { SimulatedAdapter } from './adapters/simulated-adapter.mjs';

/**
 * High-level protocol client
 * Automatically selects correct adapter based on environment
 */
export class ProtocolClient {
  /**
   * @param {'production' | 'development'} [mode] - Runtime mode
   */
  constructor(mode = 'production') {
    // Detect environment if not specified
    if (typeof window === 'undefined') {
      // Node.js environment
      mode = 'production';
    }

    if (mode === 'production' || typeof window === 'undefined') {
      // Real OTP node
      this.adapter = new RealAdapter(
        process.env.ERLANG_SERVER_URL || 'ws://localhost:8080/rdf-protocol'
      );
    } else {
      // Browser AtomVM simulation
      this.adapter = new SimulatedAdapter();
    }
  }

  /**
   * Connect to backend
   * @returns {Promise<void>}
   */
  async connect() {
    return this.adapter.connect();
  }

  /**
   * Disconnect from backend
   * @returns {Promise<void>}
   */
  async disconnect() {
    return this.adapter.disconnect();
  }

  /**
   * Check connection status
   * @returns {boolean}
   */
  isConnected() {
    return this.adapter.isConnected();
  }

  /**
   * Execute a SPARQL query
   * @param {string} query - SPARQL query string
   * @param {Object} [options] - Query options
   * @param {number} [options.limit] - Result limit
   * @param {number} [options.offset] - Result offset
   * @returns {Promise<Array>} - Query results
   */
  async executeQuery(query, options = {}) {
    const response = await this.adapter.send({
      type: 'command',
      id: Date.now(),
      payload: {
        action: 'executeQuery',
        params: { query, options }
      }
    });

    if (response.payload.success) {
      return response.payload.data;
    } else {
      throw new Error(response.payload.error);
    }
  }

  /**
   * Add a triple to the store
   * @param {string} subject
   * @param {string} predicate
   * @param {string} object
   * @returns {Promise<void>}
   */
  async addTriple(subject, predicate, object) {
    const response = await this.adapter.send({
      type: 'command',
      id: Date.now(),
      payload: {
        action: 'addTriple',
        params: { subject, predicate, object }
      }
    });

    if (!response.payload.success) {
      throw new Error(response.payload.error);
    }
  }

  /**
   * Subscribe to data changes
   * @param {EventHandler} handler
   * @returns {Unsubscribe}
   */
  onDataChanged(handler) {
    return this.adapter.subscribe('dataChanged', handler);
  }

  /**
   * Get system health
   * @returns {Promise<HealthStatus>}
   */
  async getHealth() {
    return this.adapter.getHealth();
  }
}
```

---

## Usage Example (Application Code)

```javascript
// app.mjs - Same code works with both adapters

import { ProtocolClient } from './client.mjs';

/**
 * Main application
 */
async function main() {
  // Create client - automatically selects correct adapter
  const client = new ProtocolClient();

  // Connect (to either OTP or AtomVM)
  await client.connect();

  try {
    // Execute query - identical API
    const results = await client.executeQuery(
      'SELECT ?s WHERE { ?s ?p ?o }',
      { limit: 100 }
    );
    console.log('Results:', results);

    // Subscribe to changes - identical API
    const unsubscribe = client.onDataChanged((event) => {
      console.log('Data changed:', event.payload.details);
    });

    // Health check
    const health = await client.getHealth();
    console.log('System status:', health.status);

    // Later: unsubscribe
    unsubscribe();
  } finally {
    await client.disconnect();
  }
}

main().catch(console.error);
```

---

## Configuration

### Environment Variables

```bash
# Production (real OTP)
export NODE_ENV=production
export ERLANG_SERVER_URL=wss://rdf.example.com/protocol

# Development (AtomVM in browser)
export NODE_ENV=development
```

### Package.json Scripts

```json
{
  "scripts": {
    "dev": "MODE=development npm run serve",
    "prod": "MODE=production npm run serve",
    "test:otp": "MODE=production npm run test",
    "test:atomvm": "MODE=development npm run test"
  }
}
```

---

## Testing Both Adapters

```javascript
// test/adapters.test.mjs

import { ProtocolClient } from '../client.mjs';

/**
 * Run tests against all adapters
 */
async function testBothAdapters() {
  const modes = ['production', 'development'];

  for (const mode of modes) {
    console.log(`\n=== Testing mode: ${mode} ===`);

    const client = new ProtocolClient(mode);
    await client.connect();

    try {
      // Test basic query
      const results = await client.executeQuery(
        'SELECT * WHERE { ?s ?p ?o }'
      );
      console.log(`✓ Query execution: ${results.length} results`);

      // Test health check
      const health = await client.getHealth();
      console.log(`✓ Health check: ${health.status}`);

      // Test error handling
      try {
        await client.executeQuery('INVALID SPARQL');
      } catch (error) {
        console.log(`✓ Error handling: ${error.message}`);
      }
    } finally {
      await client.disconnect();
    }
  }
}

testBothAdapters().catch(console.error);
```

---

## Key Design Points

### 1. Pure JavaScript (No TypeScript)

All code is standard JavaScript with JSDoc type hints. No TypeScript compilation.

### 2. Single Interface

Both adapters implement identical interface. Application code doesn't know which one is used.

### 3. Environment Detection

Adapter selection is automatic based on runtime environment (Node.js vs browser).

### 4. Error Handling

Errors use simple try-catch, following UNRDF patterns. No defensive code.

### 5. Private Methods

Private methods use `#` prefix (private fields) to hide implementation details.

---

## See Also

- **01-PROTOCOL-DESIGN.md** - Shared protocol layer
- **03-BROWSER-SIMULATION.md** - AtomVM details
- **06-SETUP-JS-SIDE.md** - Full JavaScript setup
