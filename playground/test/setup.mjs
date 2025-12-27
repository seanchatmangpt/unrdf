#!/usr/bin/env node

/**
 * @fileoverview Global setup for integration tests
 *
 * This runs once before all integration tests start
 * Supports both Mocha and Vitest test runners
 */

import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

let serverProcess;

/**
 * Start the playground server for testing
 * @returns {Promise<void>}
 */
async function startTestServer() {
  return new Promise((resolve, reject) => {
    const serverPath = join(__dirname, '..', 'server.mjs');

    console.log('Starting playground server for integration tests...');

    serverProcess = spawn('node', [serverPath], {
      stdio: ['pipe', 'pipe', 'pipe'],
      env: {
        ...process.env,
        NODE_ENV: 'test',
        PORT: '3000',
      },
    });

    // Handle server output
    serverProcess.stdout.on('data', (data) => {
      const output = data.toString();
      console.log('Server:', output.trim());

      // Check if server is ready
      if (output.includes('UNRDF Hooks Runtime Server running on http://localhost:3000')) {
        console.log('Test server started successfully');
        resolve();
      }
    });

    serverProcess.stderr.on('data', (data) => {
      console.error('Server Error:', data.toString());
    });

    serverProcess.on('error', (error) => {
      console.error('Failed to start server:', error);
      reject(error);
    });

    serverProcess.on('close', (code) => {
      if (code !== 0) {
        console.error(`Server exited with code ${code}`);
        reject(new Error(`Server exited with code ${code}`));
      }
    });

    // Timeout after 30 seconds
    setTimeout(() => {
      reject(new Error('Server startup timeout'));
    }, 30000);
  });
}

/**
 * Stop the test server
 * @returns {Promise<void>}
 */
async function stopTestServer() {
  return new Promise((resolve) => {
    if (serverProcess) {
      console.log('Stopping test server...');
      serverProcess.kill('SIGTERM');

      serverProcess.on('close', () => {
        console.log('Test server stopped');
        resolve();
      });
    } else {
      resolve();
    }
  });
}

/**
 * Check if server is responsive
 * @returns {Promise<boolean>}
 */
async function isServerReady() {
  try {
    const response = await fetch('http://localhost:3000/api/runtime/status');
    return response.ok;
  } catch {
    return false;
  }
}

/**
 * Mocha hooks for test lifecycle
 */
export const mochaHooks = {
  beforeAll() {
    console.log('Starting playground-cli test suite...');
  },
  afterAll() {
    console.log('Test suite completed.');
  },
};

/**
 * Vitest global setup function
 * @returns {Promise<void>}
 */
export default async function globalSetup() {
  console.log('Setting up integration test environment...');

  try {
    // Check if server is already running
    if (await isServerReady()) {
      console.log('Server already running, using existing instance');
      return;
    }

    // Start server for testing
    await startTestServer();

    // Wait a bit more for full initialization
    await new Promise(resolve => setTimeout(resolve, 2000));

    console.log('Integration test environment ready');

  } catch (error) {
    console.error('Failed to setup integration test environment:', error);
    process.exit(1);
  }
}

/**
 * Cleanup function for Vitest
 * @returns {Promise<void>}
 */
export async function teardown() {
  console.log('Cleaning up integration test environment...');

  await stopTestServer();

  console.log('Integration test environment cleaned up');
}
