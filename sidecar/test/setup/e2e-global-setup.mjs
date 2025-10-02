/**
 * @file E2E Global Setup
 * @description Vitest global setup/teardown for E2E test infrastructure
 *
 * This file runs ONCE before all E2E tests and tears down ONCE after all tests complete.
 * It manages shared Docker Compose infrastructure for all E2E test scenarios.
 */

import { setupDockerCompose, teardownDockerCompose, isDockerAvailable } from './docker-compose-setup.mjs'
import { fileURLToPath } from 'url'
import { dirname, join } from 'path'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)
const projectRoot = join(__dirname, '../..')

/** @type {import('vitest').GlobalSetupContext} */
let globalContext = null

/**
 * Global setup - runs once before all E2E tests
 */
export async function setup() {
  console.log('\n=== E2E Global Setup: Starting Test Infrastructure ===\n')

  // Check Docker availability
  const dockerAvailable = await isDockerAvailable()
  if (!dockerAvailable) {
    throw new Error('Docker daemon is not available. Please start Docker Desktop.')
  }

  // Resolve docker-compose.yml path
  const composeFile = join(projectRoot, 'test/e2e/testcontainers/docker-compose.yml')
  const projectName = 'kgc-e2e-test'

  console.log(`[Global Setup] Using compose file: ${composeFile}`)

  try {
    // Setup Docker Compose with all services
    await setupDockerCompose({
      composeFile,
      projectName,
      healthChecks: [
        { url: 'http://localhost:3000/api/health', timeout: 90000 }, // KGC Sidecar
        { url: 'http://localhost:16686', timeout: 60000 },           // Jaeger UI
        { url: 'http://localhost:9090/-/healthy', timeout: 60000 }   // Prometheus
      ]
    })

    // Store context for teardown
    globalContext = { composeFile, projectName }

    console.log('\n=== E2E Global Setup: Complete ===\n')
  } catch (error) {
    console.error('\n=== E2E Global Setup: FAILED ===\n')
    console.error(error)

    // Attempt cleanup on failure
    try {
      await teardownDockerCompose({ composeFile, projectName })
    } catch (cleanupError) {
      console.error('Cleanup after setup failure also failed:', cleanupError)
    }

    throw error
  }
}

/**
 * Global teardown - runs once after all E2E tests
 */
export async function teardown() {
  console.log('\n=== E2E Global Teardown: Stopping Test Infrastructure ===\n')

  if (!globalContext) {
    console.warn('[Global Teardown] No context found, skipping teardown')
    return
  }

  try {
    await teardownDockerCompose(globalContext)
    console.log('\n=== E2E Global Teardown: Complete ===\n')
  } catch (error) {
    console.error('\n=== E2E Global Teardown: FAILED ===\n')
    console.error(error)
    // Don't throw - allow tests to report results even if teardown fails
  }
}
