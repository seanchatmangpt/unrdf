/**
 * @file Docker Compose Test Infrastructure Setup
 * @description Automated setup/teardown for E2E test infrastructure using dockerode
 *
 * Features:
 * - Automatic Docker Compose orchestration
 * - Health check polling with exponential backoff
 * - Port availability checking
 * - Environment variable injection
 * - CI/CD compatible cleanup
 */

import Docker from 'dockerode'
import { spawn } from 'child_process'
import { promisify } from 'util'
import waitOn from 'wait-on'

const docker = new Docker()

/**
 * @typedef {Object} DockerComposeConfig
 * @property {string} composeFile - Path to docker-compose.yml
 * @property {string} projectName - Docker Compose project name
 * @property {Record<string, string>} env - Environment variables
 * @property {Array<{url: string, timeout: number}>} healthChecks - Services to wait for
 */

/**
 * Execute docker-compose command
 * @param {string[]} args - Command arguments
 * @param {string} cwd - Working directory
 * @returns {Promise<{stdout: string, stderr: string}>}
 */
async function execDockerCompose(args, cwd = process.cwd()) {
  return new Promise((resolve, reject) => {
    const proc = spawn('docker', ['compose', ...args], {
      cwd,
      env: { ...process.env },
      stdio: ['ignore', 'pipe', 'pipe']
    })

    let stdout = ''
    let stderr = ''

    proc.stdout?.on('data', (data) => {
      stdout += data.toString()
      console.log(`[Docker Compose] ${data.toString().trim()}`)
    })

    proc.stderr?.on('data', (data) => {
      stderr += data.toString()
      console.error(`[Docker Compose] ${data.toString().trim()}`)
    })

    proc.on('close', (code) => {
      if (code === 0) {
        resolve({ stdout, stderr })
      } else {
        reject(new Error(`Docker Compose exited with code ${code}: ${stderr}`))
      }
    })

    proc.on('error', reject)
  })
}

/**
 * Check if port is available
 * @param {number} port - Port to check
 * @returns {Promise<boolean>}
 */
async function isPortAvailable(port) {
  try {
    const containers = await docker.listContainers()
    const portsInUse = containers.flatMap(c =>
      Object.keys(c.Ports || {}).map(p => parseInt(p.split('/')[0]))
    )
    return !portsInUse.includes(port)
  } catch (error) {
    console.warn(`[Port Check] Error checking port ${port}:`, error.message)
    return true
  }
}

/**
 * Wait for service health with exponential backoff
 * @param {string} url - Service URL to check
 * @param {number} timeout - Timeout in ms
 * @param {number} interval - Initial interval in ms
 * @returns {Promise<void>}
 */
async function waitForHealth(url, timeout = 60000, interval = 1000) {
  console.log(`[Health Check] Waiting for ${url}...`)

  const startTime = Date.now()
  let currentInterval = interval
  let attempts = 0

  while (Date.now() - startTime < timeout) {
    try {
      attempts++
      const response = await fetch(url, { signal: AbortSignal.timeout(5000) })

      if (response.ok) {
        console.log(`[Health Check] ✓ ${url} is healthy (${attempts} attempts)`)
        return
      }

      console.log(`[Health Check] ${url} returned ${response.status}, retrying...`)
    } catch (error) {
      console.log(`[Health Check] ${url} not ready (attempt ${attempts}): ${error.message}`)
    }

    // Exponential backoff: 1s, 2s, 4s, 8s, max 10s
    await new Promise(resolve => setTimeout(resolve, currentInterval))
    currentInterval = Math.min(currentInterval * 2, 10000)
  }

  throw new Error(`Health check timeout for ${url} after ${timeout}ms`)
}

/**
 * Setup Docker Compose infrastructure
 * @param {DockerComposeConfig} config - Configuration
 * @returns {Promise<void>}
 */
export async function setupDockerCompose(config) {
  const {
    composeFile,
    projectName = 'kgc-e2e-test',
    env = {},
    healthChecks = []
  } = config

  console.log('[E2E Setup] Starting Docker Compose infrastructure...')
  console.log(`[E2E Setup] Compose file: ${composeFile}`)
  console.log(`[E2E Setup] Project name: ${projectName}`)

  // Check if containers are already running
  try {
    const { stdout } = await execDockerCompose(
      ['-f', composeFile, '-p', projectName, 'ps', '--format', 'json'],
      process.cwd()
    )

    if (stdout.trim()) {
      console.log('[E2E Setup] Infrastructure already running, cleaning up first...')
      await teardownDockerCompose({ composeFile, projectName })
    }
  } catch (error) {
    // No existing containers, proceed with setup
  }

  // Pull images first to avoid timeout during up
  console.log('[E2E Setup] Pulling Docker images...')
  try {
    await execDockerCompose(
      ['-f', composeFile, '-p', projectName, 'pull'],
      process.cwd()
    )
  } catch (error) {
    console.warn('[E2E Setup] Image pull failed, will try to use cached images:', error.message)
  }

  // Start services in detached mode
  console.log('[E2E Setup] Starting services...')
  await execDockerCompose(
    ['-f', composeFile, '-p', projectName, 'up', '-d', '--wait'],
    process.cwd()
  )

  // Wait for all health checks
  if (healthChecks.length > 0) {
    console.log(`[E2E Setup] Running ${healthChecks.length} health checks...`)

    for (const check of healthChecks) {
      await waitForHealth(check.url, check.timeout || 60000)
    }
  }

  console.log('[E2E Setup] ✓ Infrastructure ready')
}

/**
 * Teardown Docker Compose infrastructure
 * @param {Pick<DockerComposeConfig, 'composeFile' | 'projectName'>} config - Configuration
 * @returns {Promise<void>}
 */
export async function teardownDockerCompose(config) {
  const {
    composeFile,
    projectName = 'kgc-e2e-test'
  } = config

  console.log('[E2E Teardown] Stopping Docker Compose infrastructure...')

  try {
    // Stop and remove containers, networks, volumes
    await execDockerCompose(
      ['-f', composeFile, '-p', projectName, 'down', '-v', '--remove-orphans'],
      process.cwd()
    )

    console.log('[E2E Teardown] ✓ Infrastructure stopped')
  } catch (error) {
    console.error('[E2E Teardown] Error during teardown:', error.message)

    // Force cleanup with docker system prune
    try {
      console.log('[E2E Teardown] Attempting force cleanup...')
      await execDockerCompose(
        ['-f', composeFile, '-p', projectName, 'rm', '-f', '-s', '-v'],
        process.cwd()
      )
    } catch (forceError) {
      console.error('[E2E Teardown] Force cleanup failed:', forceError.message)
    }
  }
}

/**
 * Get container logs
 * @param {string} composeFile - Path to docker-compose.yml
 * @param {string} projectName - Docker Compose project name
 * @param {string} service - Service name
 * @returns {Promise<string>}
 */
export async function getServiceLogs(composeFile, projectName, service) {
  try {
    const { stdout } = await execDockerCompose(
      ['-f', composeFile, '-p', projectName, 'logs', service],
      process.cwd()
    )
    return stdout
  } catch (error) {
    console.error(`[Logs] Failed to get logs for ${service}:`, error.message)
    return ''
  }
}

/**
 * Wait for multiple URLs to be ready
 * @param {string[]} urls - URLs to wait for
 * @param {number} timeout - Timeout in ms
 * @returns {Promise<void>}
 */
export async function waitForUrls(urls, timeout = 60000) {
  console.log(`[Wait] Waiting for ${urls.length} URLs to be ready...`)

  await Promise.all(
    urls.map(url => waitForHealth(url, timeout))
  )

  console.log('[Wait] ✓ All URLs ready')
}

/**
 * Check Docker daemon availability
 * @returns {Promise<boolean>}
 */
export async function isDockerAvailable() {
  try {
    await docker.ping()
    return true
  } catch (error) {
    console.error('[Docker] Docker daemon not available:', error.message)
    return false
  }
}

/**
 * Get service URL from docker-compose
 * @param {string} composeFile - Path to docker-compose.yml
 * @param {string} projectName - Project name
 * @param {string} service - Service name
 * @param {number} port - Internal port
 * @returns {Promise<string>}
 */
export async function getServiceUrl(composeFile, projectName, service, port) {
  try {
    const { stdout } = await execDockerCompose(
      ['-f', composeFile, '-p', projectName, 'port', service, port.toString()],
      process.cwd()
    )

    const match = stdout.trim().match(/0\.0\.0\.0:(\d+)/)
    if (match) {
      return `http://localhost:${match[1]}`
    }

    // Fallback to default port
    return `http://localhost:${port}`
  } catch (error) {
    console.warn(`[Service URL] Failed to get URL for ${service}:${port}:`, error.message)
    return `http://localhost:${port}`
  }
}
