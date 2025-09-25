#!/usr/bin/env node

/**
 * @fileoverview Per-test setup for integration tests
 *
 * This runs before each integration test
 */

import { beforeEach } from 'vitest'

// Global test state
global.testState = {
  createdHooks: new Set(),
  createdDataSources: new Set(),
  hookResults: new Map()
}

/**
 * Clean up test resources
 * @returns {Promise<void>}
 */
async function cleanupTestResources() {
  const BASE_URL = 'http://localhost:3000'

  try {
    // Clear hook results
    await fetch(`${BASE_URL}/api/runtime/status`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ command: 'clear-results' })
    })

    // Delete created hooks
    for (const hookId of global.testState.createdHooks) {
      try {
        await fetch(`${BASE_URL}/api/hooks/${hookId}`, {
          method: 'DELETE'
        })
      } catch (error) {
        // Ignore errors during cleanup
      }
    }

    // Note: Data sources are cleaned up by runtime reset
    global.testState.createdHooks.clear()
    global.testState.createdDataSources.clear()
    global.testState.hookResults.clear()

  } catch (error) {
    // Ignore cleanup errors
  }
}

/**
 * Track created resources for cleanup
 * @param {string} type - Resource type ('hook' or 'data')
 * @param {string} id - Resource ID
 */
global.trackResource = (type, id) => {
  if (type === 'hook') {
    global.testState.createdHooks.add(id)
  } else if (type === 'data') {
    global.testState.createdDataSources.add(id)
  }
}

/**
 * Track hook evaluation results
 * @param {string} hookId - Hook ID
 * @param {Object} result - Evaluation result
 */
global.trackHookResult = (hookId, result) => {
  if (!global.testState.hookResults.has(hookId)) {
    global.testState.hookResults.set(hookId, [])
  }
  global.testState.hookResults.get(hookId).push(result)
}

// Setup before each test
beforeEach(async (context) => {
  // Clean up previous test resources
  await cleanupTestResources()

  // Add test context helpers
  context.trackResource = global.trackResource
  context.trackHookResult = global.trackHookResult
  context.testState = global.testState
})
