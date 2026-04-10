/**
 * @file Integration Health Checks
 * @module cli/commands/doctor/checks/integration
 *
 * @description
 * Checks for external integrations including federation peers,
 * OTEL exporter, Docker containers, and optional services.
 */

import { execSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

/**
 * Check federation peer connectivity
 */
async function checkFederationPeers() {
  // This is a placeholder - in a real implementation, this would
  // check actual federation peer status from the federation coordinator
  const federationEnabled = existsSync(join(projectRoot, 'packages/federation'));

  if (!federationEnabled) {
    return {
      status: 'pass',
      actual: 'Federation not enabled',
      expected: 'N/A',
    };
  }

  // TODO: Implement real federation peer check
  // For now, just check if the package exists
  return {
    status: 'pass',
    actual: 'Federation package present (peer check not implemented)',
    expected: 'Federation peers connected',
    fix: 'Implement federation peer status check',
  };
}

/**
 * Check OTEL exporter configuration
 */
async function checkOTELExporter() {
  const endpoint = process.env.OTEL_EXPORTER_JAEGER_ENDPOINT;

  if (!endpoint) {
    return {
      status: 'warn',
      actual: 'OTEL exporter endpoint not configured',
      expected: 'OTEL_EXPORTER_JAEGER_ENDPOINT set',
      fix: 'Set OTEL_EXPORTER_JAEGER_ENDPOINT environment variable',
    };
  }

  // Try to reach the Jaeger endpoint
  try {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 2000);

    const url = endpoint.replace(/\/api\/v2\/spans$/, ''); // Remove spans path for health check
    const response = await fetch(`${url}/api/services`, {
      signal: controller.signal,
    });

    clearTimeout(timeoutId);

    if (response.ok) {
      return {
        status: 'pass',
        actual: `OTEL exporter reachable: ${endpoint}`,
        expected: 'OTEL exporter configured and reachable',
      };
    }

    return {
      status: 'warn',
      actual: `OTEL exporter returned HTTP ${response.status}`,
      expected: 'OTEL exporter reachable',
      fix: 'Check Jaeger is running at configured endpoint',
    };
  } catch (error) {
    if (error.name === 'AbortError') {
      return {
        status: 'warn',
        actual: `OTEL exporter timeout (endpoint: ${endpoint})`,
        expected: 'OTEL exporter reachable',
        fix: 'Start Jaeger or check endpoint configuration',
      };
    }

    return {
      status: 'warn',
      actual: `OTEL exporter unreachable: ${error.message}`,
      expected: 'OTEL exporter configured and reachable',
      fix: 'Check OTEL_EXPORTER_JAEGER_ENDPOINT configuration',
    };
  }
}

/**
 * Check Docker daemon
 */
function checkDocker() {
  try {
    const result = execSync('docker --version', { encoding: 'utf-8', stdio: 'pipe' });

    try {
      // Check if Docker daemon is running
      execSync('docker info', { encoding: 'utf-8', stdio: 'pipe', timeout: 5000 });
      return {
        status: 'pass',
        actual: result.trim(),
        expected: 'Docker daemon running',
      };
    } catch (error) {
      return {
        status: 'warn',
        actual: 'Docker installed but daemon not running',
        expected: 'Docker daemon running',
        fix: 'Start Docker Desktop or Docker daemon',
      };
    }
  } catch (error) {
    return {
      status: 'warn',
      actual: 'Docker not installed',
      expected: 'Docker installed and running',
      fix: 'Install Docker or skip container-based tests',
    };
  }
}

/**
 * Check optional services (Redis, PostgreSQL)
 */
function checkOptionalServices() {
  const services = {
    redis: checkRedis(),
    postgres: checkPostgres(),
  };

  const results = [];
  let allAvailable = true;

  for (const [name, check] of Object.entries(services)) {
    const result = check;
    results.push({ name, ...result });
    if (result.status !== 'pass') {
      allAvailable = false;
    }
  }

  if (allAvailable) {
    return {
      status: 'pass',
      actual: 'All optional services available',
      expected: 'Redis, PostgreSQL (optional)',
      services: results,
    };
  }

  const unavailable = results.filter(r => r.status !== 'pass').map(r => r.name);
  return {
    status: 'warn',
    actual: `Optional services unavailable: ${unavailable.join(', ')}`,
    expected: 'Redis, PostgreSQL (optional)',
    services: results,
    fix: 'Optional services not required - skip features that depend on them',
  };
}

/**
 * Check Redis
 */
function checkRedis() {
  try {
    const host = process.env.REDIS_HOST || 'localhost';
    const port = process.env.REDIS_PORT || 6379;

    // Try to connect to Redis using redis-cli (if available)
    execSync(`redis-cli -h ${host} -p ${port} ping`, {
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 2000,
    });

    return {
      status: 'pass',
      actual: `Redis reachable at ${host}:${port}`,
      expected: 'Redis (optional)',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: 'Redis not running or not installed',
      expected: 'Redis (optional)',
      fix: 'Redis is optional - skip cache features or start Redis server',
    };
  }
}

/**
 * Check PostgreSQL
 */
function checkPostgres() {
  try {
    const host = process.env.PGHOST || 'localhost';
    const port = process.env.PGPORT || 5432;

    // Try to connect to PostgreSQL using psql (if available)
    execSync(`psql -h ${host} -p ${port} -c 'SELECT 1'`, {
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 2000,
    });

    return {
      status: 'pass',
      actual: `PostgreSQL reachable at ${host}:${port}`,
      expected: 'PostgreSQL (optional)',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: 'PostgreSQL not running or not installed',
      expected: 'PostgreSQL (optional)',
      fix: 'PostgreSQL is optional - skip lockchain features or start PostgreSQL server',
    };
  }
}

/**
 * Run all integration checks
 */
export async function checkIntegrations() {
  return {
    category: 'Integrations',
    checks: [
      {
        name: 'Federation peers',
        ...(await checkFederationPeers()),
      },
      {
        name: 'OTEL exporter',
        ...(await checkOTELExporter()),
      },
      {
        name: 'Test containers (Docker)',
        ...checkDocker(),
      },
      {
        name: 'Optional services',
        ...checkOptionalServices(),
      },
    ],
  };
}
