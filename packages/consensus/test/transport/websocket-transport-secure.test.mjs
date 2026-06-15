/**
 * @fileoverview Secure WebSocket Transport Tests
 */

import { describe, it, expect, beforeEach, afterEach, beforeAll, afterAll } from 'vitest';
import { createWebSocketTransport } from '../../src/transport/websocket-transport.mjs';
import fs from 'fs';
import path from 'path';
import { execSync } from 'child_process';
import os from 'os';

describe('Secure WebSocket Transport', () => {
  let transport1;
  let transport2;
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'unrdf-test-certs-'));
  const certPath = path.join(tempDir, 'cert.pem');
  const keyPath = path.join(tempDir, 'key.pem');

  beforeAll(() => {
    // Generate self-signed certificate for testing
    try {
      execSync(`openssl req -x509 -newkey rsa:2048 -keyout "${keyPath}" -out "${certPath}" -days 1 -nodes -subj "/CN=localhost"`, { stdio: 'ignore' });
    } catch (error) {
      console.warn('Failed to generate test certificates. TLS tests may fail.', error);
    }
  });

  afterAll(() => {
    try {
      fs.rmSync(tempDir, { recursive: true, force: true });
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  afterEach(async () => {
    if (transport1) await transport1.shutdown();
    if (transport2) await transport2.shutdown();
  });

  it('should authenticate successfully with shared secret', async () => {
    const sharedSecret = 'super-secret';

    transport1 = createWebSocketTransport({
      nodeId: 'node-1',
      port: 11001,
      auth: {
        enabled: true,
        sharedSecret
      }
    });

    transport2 = createWebSocketTransport({
      nodeId: 'node-2',
      port: 11002,
      auth: {
        enabled: true,
        sharedSecret
      }
    });

    await transport1.start();
    await transport2.start();

    transport1.addPeer('node-2', 'localhost', 11002);

    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('Connection timeout')), 5000);
      transport1.on('peer_connected', ({ nodeId }) => {
        if (nodeId === 'node-2') {
          clearTimeout(timeout);
          resolve();
        }
      });
    });

    expect(transport1.isConnected('node-2')).toBe(true);
  });

  it('should fail authentication with wrong shared secret', async () => {
    transport1 = createWebSocketTransport({
      nodeId: 'node-1',
      port: 11003,
      auth: {
        enabled: true,
        sharedSecret: 'secret-1'
      }
    });

    transport2 = createWebSocketTransport({
      nodeId: 'node-2',
      port: 11004,
      auth: {
        enabled: true,
        sharedSecret: 'secret-2'
      }
    });

    await transport1.start();
    await transport2.start();

    transport1.addPeer('node-2', 'localhost', 11004);

    const authFailed = await new Promise((resolve) => {
      const timeout = setTimeout(() => resolve(false), 5000);
      transport1.on('error', (err) => {
        if (err.source === 'auth' || (err.error && err.error.message && err.error.message.includes('Authentication failed')) || (typeof err.error === 'string' && err.error.includes('Authentication failed'))) {
          clearTimeout(timeout);
          resolve(true);
        }
      });
    });

    expect(authFailed).toBe(true);
    expect(transport1.isConnected('node-2')).toBe(false);
  });

  it('should support TLS (WSS)', async () => {
    if (!fs.existsSync(certPath)) {
      console.warn('Skipping TLS test: cert not found');
      return;
    }

    transport1 = createWebSocketTransport({
      nodeId: 'node-1',
      port: 11005,
      tls: {
        enabled: true,
        cert: certPath,
        key: keyPath,
        ca: certPath,
        rejectUnauthorized: false // Self-signed
      }
    });

    transport2 = createWebSocketTransport({
      nodeId: 'node-2',
      port: 11006,
      tls: {
        enabled: true,
        cert: certPath,
        key: keyPath,
        ca: certPath,
        rejectUnauthorized: false // Self-signed
      }
    });

    await transport1.start();
    await transport2.start();

    transport2.on('message', (msg) => {
      transport2.sendResponse(msg, { success: true });
    });

    transport1.addPeer('node-2', 'localhost', 11006);

    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('TLS Connection timeout')), 5000);
      transport1.on('peer_connected', ({ nodeId }) => {
        if (nodeId === 'node-2') {
          clearTimeout(timeout);
          resolve();
        }
      });
    });

    expect(transport1.isConnected('node-2')).toBe(true);
    
    // Send a message over TLS
    const response = await transport1.send('node-2', {
      type: 'heartbeat',
      term: 1,
      data: {}
    });
    
    expect(response.success).toBe(true);
  });
});
