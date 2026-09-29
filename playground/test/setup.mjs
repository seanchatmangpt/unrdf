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

const PORT = process.env.PORT || '3000';
const BASE_URL = `http://localhost:${PORT}`;

/**
 * Start the playground Nitro server for testing.
 *
 * Resolves once /api/runtime/status answers; rejects if the process exits or
 * does not become ready in time.
 *
 * @returns {Promise<void>}
 */
async function startTestServer() {
  const nitroBin = join(__dirname, '..', 'node_modules', '.bin', 'nitropack');

  console.log('Starting playground Nitro server for tests...');

  serverProcess = spawn(nitroBin, ['dev', '--port', PORT], {
    cwd: join(__dirname, '..'),
    // Own process group so the whole nitro worker tree can be killed on teardown
    detached: true,
    stdio: ['ignore', 'pipe', 'pipe'],
    env: {
      ...process.env,
      NODE_ENV: 'test',
    },
  });

  let output = '';
  serverProcess.stdout.on('data', (data) => {
    output += data.toString();
  });
  serverProcess.stderr.on('data', (data) => {
    output += data.toString();
  });

  const exited = new Promise((_, reject) => {
    serverProcess.on('error', reject);
    serverProcess.on('close', (code) => {
      reject(new Error(`Nitro exited with code ${code} before becoming ready:\n${output}`));
    });
  });

  const ready = (async () => {
    const deadline = Date.now() + 30000;
    while (Date.now() < deadline) {
      if (await isServerReady()) return;
      await new Promise((resolve) => setTimeout(resolve, 250));
    }
    throw new Error(`Server startup timeout after 30s:\n${output}`);
  })();

  await Promise.race([ready, exited]);
  console.log('Test server started successfully');
}

/**
 * Stop the test server
 * @returns {Promise<void>}
 */
async function stopTestServer() {
  if (!serverProcess) return;

  console.log('Stopping test server...');
  const closed = new Promise((resolve) => serverProcess.once('close', resolve));
  try {
    process.kill(-serverProcess.pid, 'SIGTERM');
  } catch {
    // Already gone
  }
  await Promise.race([closed, new Promise((resolve) => setTimeout(resolve, 5000))]);
  serverProcess = undefined;
  console.log('Test server stopped');
}

/**
 * Check if server is responsive
 * @returns {Promise<boolean>}
 */
async function isServerReady() {
  try {
    const response = await fetch(`${BASE_URL}/api/runtime/status`);
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
 * Cleanup: stop the server if this process started it
 * @returns {Promise<void>}
 */
async function teardown() {
  console.log('Cleaning up test environment...');

  await stopTestServer();

  console.log('Test environment cleaned up');
}

/**
 * Vitest global setup function; the returned function is vitest's teardown.
 * @returns {Promise<() => Promise<void>>}
 */
export default async function globalSetup() {
  console.log('Setting up test environment...');

  try {
    // Check if server is already running
    if (await isServerReady()) {
      console.log('Server already running, using existing instance');
      return teardown;
    }

    // Start server for testing
    await startTestServer();

    console.log('Test environment ready');
    return teardown;
  } catch (error) {
    // Do not leave a half-started server behind
    await stopTestServer();
    console.error('Failed to set up test environment:', error);
    process.exit(1);
  }
}
