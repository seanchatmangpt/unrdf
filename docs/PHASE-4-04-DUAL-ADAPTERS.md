# Phase 4.4: Dual Adapters - Transparent Runtime Switching

## Overview

JavaScript code should not know or care whether it's talking to a real OTP node or the browser AtomVM simulation. This is achieved through **adapters** that implement a shared interface.

---

## Adapter Interface

All adapters implement this interface:

```typescript
interface ProtocolAdapter {
  // Connection management
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  isConnected(): boolean;

  // Request/response
  send(command: Command): Promise<Response>;

  // Subscriptions (for events)
  subscribe(topic: string, handler: EventHandler): Unsubscribe;

  // Health checks
  getHealth(): Promise<HealthStatus>;
}
```

---

## Architecture

```
┌────────────────────────────┐
│  JavaScript Application     │
│ (Same code for both targets)│
└────────┬───────────────────┘
         │
    ┌────▼─────────────────┐
    │  ProtocolClient       │
    │  (Adapter factory)    │
    └────┬─────────────────┘
         │
         │ Creates correct adapter
         │
    ┌────┴────────────┬──────────────────┐
    │                 │                  │
    ▼                 ▼                  ▼
┌─────────────┐ ┌──────────────┐ ┌─────────────┐
│   Config    │ │   Config     │ │   Config    │
│   Prod      │ │   Dev        │ │   Testing   │
└─────────────┘ └──────────────┘ └─────────────┘
    │                 │                  │
    ▼                 ▼                  ▼
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│  RealAdapter │ │ SimAdapter   │ │ MockAdapter  │
│              │ │ (AtomVM)     │ │ (Testing)    │
│ WebSocket or │ │ WebSocket    │ │ In-memory    │
│ HTTP to OTP  │ │ to WASM      │ │              │
└──────────────┘ └──────────────┘ └──────────────┘
    │                 │                  │
    └─────────────────┼──────────────────┘
                      │
                      ▼
            ┌──────────────────┐
            │  Protocol Layer   │
            │ (Identical on both)
            └──────────────────┘
```

---

## Implementation

### 1. Shared Interface

```typescript
// interfaces/protocol.ts
export interface Command {
  type: 'command';
  id: string | number;
  payload: {
    action: string;
    params?: Record<string, any>;
  };
  meta?: {
    timeout_ms?: number;
  };
}

export interface Response {
  type: 'response';
  id: string | number;
  payload: {
    success: boolean;
    data?: any;
    error?: string;
  };
  meta?: {
    duration_ms: number;
  };
}

export interface Event {
  type: 'event';
  id: null;
  payload: {
    event_type: string;
    details: Record<string, any>;
  };
}

export type ProtocolMessage = Command | Response | Event;

export interface ProtocolAdapter {
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  isConnected(): boolean;
  send(command: Command): Promise<Response>;
  subscribe(topic: string, handler: (event: Event) => void): () => void;
  getHealth(): Promise<HealthStatus>;
}

export interface HealthStatus {
  status: 'ok' | 'degraded' | 'down';
  message?: string;
  details?: Record<string, any>;
}
```

### 2. Real Adapter (Production)

```typescript
// adapters/real-adapter.ts
import { ProtocolAdapter, Command, Response, Event } from '../interfaces';

export class RealAdapter implements ProtocolAdapter {
  private ws: WebSocket | null = null;
  private url: string;
  private pendingRequests = new Map<string | number, (response: Response) => void>();
  private messageId = 0;
  private eventListeners = new Map<string, Set<(event: Event) => void>>();
  private retryCount = 0;
  private maxRetries = 5;

  constructor(url: string = 'ws://localhost:8080/rdf-protocol') {
    this.url = url;
  }

  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
          console.log('Connected to real Erlang backend');
          this.retryCount = 0;
          resolve();
        };

        this.ws.onmessage = (event) => {
          this.handleMessage(JSON.parse(event.data));
        };

        this.ws.onerror = (error) => {
          console.error('WebSocket error:', error);
          reject(error);
        };

        this.ws.onclose = () => {
          console.log('Disconnected from real backend');
          this.attemptReconnect();
        };
      } catch (error) {
        reject(error);
      }
    });
  }

  async disconnect(): Promise<void> {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  isConnected(): boolean {
    return this.ws !== null && this.ws.readyState === WebSocket.OPEN;
  }

  async send(command: Command): Promise<Response> {
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

      this.ws!.send(JSON.stringify(messageWithId));
    });
  }

  subscribe(topic: string, handler: (event: Event) => void): () => void {
    if (!this.eventListeners.has(topic)) {
      this.eventListeners.set(topic, new Set());
    }
    this.eventListeners.get(topic)!.add(handler);

    // Return unsubscribe function
    return () => {
      const listeners = this.eventListeners.get(topic);
      if (listeners) {
        listeners.delete(handler);
      }
    };
  }

  async getHealth(): Promise<HealthStatus> {
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

  private handleMessage(message: ProtocolMessage): void {
    if (message.type === 'response') {
      const handler = this.pendingRequests.get(message.id);
      if (handler) {
        this.pendingRequests.delete(message.id);
        handler(message as Response);
      }
    } else if (message.type === 'event') {
      // Dispatch to subscribers
      const listeners = this.eventListeners.get(message.payload.event_type);
      if (listeners) {
        listeners.forEach(handler => handler(message as Event));
      }
    }
  }

  private async attemptReconnect(): Promise<void> {
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
      this.attemptReconnect();
    }
  }
}
```

### 3. Simulated Adapter (AtomVM + WASM)

```typescript
// adapters/simulated-adapter.ts
import { ProtocolAdapter, Command, Response, Event } from '../interfaces';

declare global {
  interface Window {
    atomvm_port: {
      send(message: any): void;
      onmessage?: (message: any) => void;
    };
  }
}

export class SimulatedAdapter implements ProtocolAdapter {
  private connected = false;
  private pendingRequests = new Map<string | number, (response: Response) => void>();
  private eventListeners = new Map<string, Set<(event: Event) => void>>();
  private messageId = 0;

  async connect(): Promise<void> {
    // AtomVM is already running in WASM
    // Just establish message channel

    if (window.atomvm_port) {
      window.atomvm_port.onmessage = (message) => {
        this.handleMessage(JSON.parse(message));
      };
      this.connected = true;
      console.log('Connected to AtomVM simulation');
    } else {
      throw new Error('AtomVM not loaded. Did you forget to load atomvm.js?');
    }
  }

  async disconnect(): Promise<void> {
    this.connected = false;
  }

  isConnected(): boolean {
    return this.connected;
  }

  async send(command: Command): Promise<Response> {
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

  subscribe(topic: string, handler: (event: Event) => void): () => void {
    if (!this.eventListeners.has(topic)) {
      this.eventListeners.set(topic, new Set());
    }
    this.eventListeners.get(topic)!.add(handler);

    return () => {
      const listeners = this.eventListeners.get(topic);
      if (listeners) {
        listeners.delete(handler);
      }
    };
  }

  async getHealth(): Promise<HealthStatus> {
    if (!this.isConnected()) {
      return { status: 'down', message: 'AtomVM not connected' };
    }
    // AtomVM is always healthy if connected
    return { status: 'ok' };
  }

  private handleMessage(message: ProtocolMessage): void {
    if (message.type === 'response') {
      const handler = this.pendingRequests.get(message.id);
      if (handler) {
        this.pendingRequests.delete(message.id);
        handler(message as Response);
      }
    } else if (message.type === 'event') {
      const listeners = this.eventListeners.get(message.payload.event_type);
      if (listeners) {
        listeners.forEach(handler => handler(message as Event));
      }
    }
  }
}
```

### 4. Adapter Factory (Transparent Selection)

```typescript
// client.ts
import { ProtocolAdapter } from './interfaces';
import { RealAdapter } from './adapters/real-adapter';
import { SimulatedAdapter } from './adapters/simulated-adapter';

export class ProtocolClient {
  private adapter: ProtocolAdapter;

  constructor(mode: 'production' | 'development' = 'production') {
    if (mode === 'production' || (typeof window === 'undefined')) {
      // Real OTP node
      this.adapter = new RealAdapter(
        process.env.ERLANG_SERVER_URL || 'ws://localhost:8080/rdf-protocol'
      );
    } else {
      // Browser AtomVM simulation
      this.adapter = new SimulatedAdapter();
    }
  }

  async connect(): Promise<void> {
    return this.adapter.connect();
  }

  async disconnect(): Promise<void> {
    return this.adapter.disconnect();
  }

  isConnected(): boolean {
    return this.adapter.isConnected();
  }

  async executeQuery(query: string, options: any = {}): Promise<any[]> {
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

  onDataChanged(handler: (event: Event) => void): () => void {
    return this.adapter.subscribe('dataChanged', handler);
  }

  async getHealth(): Promise<HealthStatus> {
    return this.adapter.getHealth();
  }
}
```

### 5. Usage in Application

```typescript
// app.ts - Same code works with both adapters

import { ProtocolClient } from './client';

async function main() {
  // Create client - automatically selects right adapter
  const client = new ProtocolClient();

  // Connect (to either OTP or AtomVM)
  await client.connect();

  // Execute query - identical API
  try {
    const results = await client.executeQuery(
      'SELECT ?s WHERE { ?s ?p ?o }',
      { limit: 100 }
    );
    console.log('Results:', results);
  } catch (error) {
    console.error('Error:', error);
  }

  // Subscribe to changes - identical API
  const unsubscribe = client.onDataChanged((event) => {
    console.log('Data changed:', event.payload.details);
  });

  // Health check
  const health = await client.getHealth();
  console.log('System status:', health.status);

  // Disconnect
  await client.disconnect();
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

# Testing (mock adapter)
export NODE_ENV=test
```

### TypeScript Configuration

```typescript
// config.ts
export const config = {
  production: {
    adapter: 'real',
    url: process.env.ERLANG_SERVER_URL || 'ws://localhost:8080',
    timeout: 5000,
    retries: 3
  },
  development: {
    adapter: 'simulated',
    timeout: 10000,  // More lenient in dev
    retries: 1
  },
  test: {
    adapter: 'mock',
    timeout: 1000,
    retries: 0
  }
};
```

---

## Debugging Adapters

### Enable Verbose Logging

```typescript
// Enable logging in real adapter
const adapter = new RealAdapter('ws://localhost:8080');
(adapter as any).debug = true;  // Enable debug mode

// Then all messages are logged
// → Send: { type: 'command', ... }
// ← Receive: { type: 'response', ... }
```

### Message Interception

```typescript
// Intercept all messages for debugging
class DebugAdapter implements ProtocolAdapter {
  constructor(private delegate: ProtocolAdapter) {}

  async send(command: Command): Promise<Response> {
    console.log('→ Sending:', command);
    const response = await this.delegate.send(command);
    console.log('← Received:', response);
    return response;
  }

  // Implement other methods by delegating...
}

const realAdapter = new RealAdapter();
const debugAdapter = new DebugAdapter(realAdapter);
```

---

## See Also

- **01-PROTOCOL-DESIGN.md** - Shared protocol layer
- **03-BROWSER-SIMULATION.md** - AtomVM details
- **06-SETUP-JS-SIDE.md** - Full JavaScript setup
