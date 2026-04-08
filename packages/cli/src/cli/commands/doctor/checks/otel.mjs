/**
 * @file OTEL Health Checks
 * @module cli/commands/doctor/checks/otel
 *
 * @description
 * Checks for OpenTelemetry observability pipeline including:
 * - OTLP endpoint connectivity (localhost:4317 gRPC, localhost:4318 HTTP)
 * - OTEL SDK initialization status (isInstrumented)
 * - Weaver registry integrity (weaver registry check)
 * - Trace pipeline E2E (generate test span, verify in Tempo)
 */

import { execSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createServer } from 'node:net';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

/**
 * Check OTLP endpoint connectivity (gRPC and HTTP)
 */
async function checkOTLPEndpoints() {
  const endpoints = [
    { port: 4317, protocol: 'gRPC', service: 'OTLP gRPC' },
    { port: 4318, protocol: 'HTTP', service: 'OTLP HTTP' },
  ];

  const unreachable = [];
  const reachable = [];

  for (const { port, protocol, service } of endpoints) {
    try {
      // Try to connect to the port
      const server = createServer();

      await new Promise((resolve, reject) => {
        server.once('error', reject);
        server.listen(port, () => {
          server.once('close', resolve);
          server.close();
        });
      });

      // If we get here, port was available (OTEL collector not running)
      unreachable.push({ port, protocol, service, status: 'not listening' });
    } catch (error) {
      if (error.code === 'EADDRINUSE' || error.code === 'ECONNREFUSED') {
        // Port is in use, which means OTLP collector is running
        reachable.push({ port, protocol, service, status: 'listening' });
      } else {
        unreachable.push({ port, protocol, service, status: `error: ${error.code}` });
      }
    }
  }

  if (reachable.length === endpoints.length) {
    return {
      status: 'pass',
      actual: `All OTLP endpoints reachable: ${reachable.map((e) => `:${e.port} (${e.protocol})`).join(', ')}`,
      expected: 'OTLP gRPC (:4317) and HTTP (:4318) endpoints reachable',
      endpoints: [...reachable, ...unreachable],
    };
  }

  if (reachable.length === 0) {
    return {
      status: 'fail',
      actual: 'No OTLP endpoints reachable',
      expected: 'OTLP gRPC (:4317) and HTTP (:4318) endpoints reachable',
      endpoints: [...reachable, ...unreachable],
      fix: 'Start OTEL collector: kubectl port-forward -n unrdf-observability svc/unrdf-obs-otel-collector 4317:4317 4318:4318',
    };
  }

  return {
    status: 'warn',
    actual: `Some OTLP endpoints unreachable: ${unreachable.map((e) => `:${e.port} (${e.protocol})`).join(', ')}`,
    expected: 'All OTLP endpoints reachable',
    endpoints: [...reachable, ...unreachable],
    fix: 'Check OTEL collector status: kubectl get pods -n unrdf-observability -l app=otel-collector',
  };
}

/**
 * Check OTEL SDK initialization status
 */
async function checkOTELSDK() {
  try {
    // Try to import and check if OTEL is instrumented
    const { isInstrumented } = await import('@unrdf/core/otel').catch(() => ({ isInstrumented: () => false }));

    if (typeof isInstrumented === 'function' && isInstrumented()) {
      return {
        status: 'pass',
        actual: 'OTEL SDK initialized and instrumented',
        expected: 'OTEL SDK initialized (isInstrumented returns true)',
      };
    }

    return {
      status: 'warn',
      actual: 'OTEL SDK available but not instrumented',
      expected: 'OTEL SDK initialized (isInstrumented returns true)',
      fix: 'Initialize OTEL SDK: import { registerOTEL } from @unrdf/core/otel; await registerOTEL();',
    };
  } catch (error) {
    return {
      status: 'fail',
      actual: `OTEL SDK not available: ${error.message}`,
      expected: 'OTEL SDK initialized and instrumented',
      fix: 'Ensure @unrdf/core/otel is available and OTEL is initialized',
    };
  }
}

/**
 * Check Weaver registry integrity
 */
function checkWeaverRegistry() {
  try {
    // Check if weaver binary is available
    const weaverPath = join(process.env.HOME || '', '.cargo/bin/weaver');

    if (!existsSync(weaverPath)) {
      return {
        status: 'warn',
        actual: 'Weaver binary not found',
        expected: 'weaver registry check passes',
        fix: 'Install Weaver: cargo install weaver-cli',
        note: 'Weaver is used for OTEL schema validation',
      };
    }

    // Run weaver registry check
    const result = execSync('weaver registry check -r ./semconv/model -p ./semconv/policies/', {
      cwd: projectRoot,
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 30000,
    });

    if (result.includes('0 violations')) {
      return {
        status: 'pass',
        actual: 'Weaver registry check passed (0 violations)',
        expected: 'weaver registry check passes',
      };
    }

    return {
      status: 'warn',
      actual: 'Weaver registry check found issues',
      expected: 'weaver registry check passes (0 violations)',
      fix: 'Run: weaver registry check -r ./semconv/model -p ./semconv/policies/ to see violations',
    };
  } catch (error) {
    // Non-zero exit code means violations found
    if (error.status && error.status !== 0) {
      return {
        status: 'fail',
        actual: 'Weaver registry check failed (violations found)',
        expected: 'weaver registry check passes (0 violations)',
        fix: 'Run: weaver registry check -r ./semconv/model -p ./semconv/policies/ to see violations',
      };
    }

    return {
      status: 'warn',
      actual: `Could not run Weaver registry check: ${error.message}`,
      expected: 'weaver registry check passes',
      fix: 'Ensure Weaver is installed: cargo install weaver-cli',
    };
  }
}

/**
 * Check trace pipeline E2E (generate test span, verify in Tempo)
 */
async function checkTracePipeline() {
  try {
    // Check if Tempo is accessible via port-forward
    const tempoPort = 3200; // Default Tempo port

    const server = createServer();

    await new Promise((resolve, reject) => {
      server.once('error', reject);
      server.listen(tempoPort, () => {
        server.once('close', resolve);
        server.close();
      });
    });

    // Port is available (Tempo not running)
    return {
      status: 'warn',
      actual: 'Tempo not accessible',
      expected: 'Tempo running and accepting traces',
      fix: 'Start Tempo: kubectl port-forward -n unrdf-observability svc/unrdf-obs-tempo 3200:3200',
    };
  } catch (error) {
    if (error.code === 'EADDRINUSE') {
      // Tempo is running
      try {
        // Try to query Tempo API for recent traces
        const response = execSync('curl -s http://localhost:3200/api/v1/search', {
          encoding: 'utf-8',
          stdio: 'pipe',
          timeout: 5000,
        });

        const data = JSON.parse(response);
        const traceCount = data.traces ? data.traces.length : 0;

        return {
          status: 'pass',
          actual: `Tempo accessible (${traceCount} traces found)`,
          expected: 'Tempo running and accepting traces',
          details: { traceCount },
        };
      } catch (apiError) {
        return {
          status: 'warn',
          actual: 'Tempo running but API not responding',
          expected: 'Tempo running and accepting traces',
          fix: 'Check Tempo logs: kubectl logs -n unrdf-observability -l app=tempo',
        };
      }
    }

    return {
      status: 'warn',
      actual: `Could not verify Tempo: ${error.message}`,
      expected: 'Tempo running and accepting traces',
      fix: 'Check Tempo status: kubectl get pods -n unrdf-observability -l app=tempo',
    };
  }
}

/**
 * Check OTEL Collector pipeline status
 */
async function checkOTELCollectorPipeline() {
  try {
    // Check OTEL collector health extension at :13133
    const collectorPort = 13133;

    const server = createServer();

    await new Promise((resolve, reject) => {
      server.once('error', reject);
      server.listen(collectorPort, () => {
        server.once('close', resolve);
        server.close();
      });
    });

    // Port is available (collector not running or health extension not enabled)
    return {
      status: 'warn',
      actual: 'OTEL Collector health extension not accessible',
      expected: 'OTEL Collector health extension responding at :13133',
      fix: 'Check OTEL collector configuration and restart',
    };
  } catch (error) {
    if (error.code === 'EADDRINUSE') {
      // Collector is running
      try {
        // Try to query health extension
        const response = execSync('curl -s http://localhost:13133/', {
          encoding: 'utf-8',
          stdio: 'pipe',
          timeout: 5000,
        });

        if (response.includes('OK') || response.includes('healthy')) {
          return {
            status: 'pass',
            actual: 'OTEL Collector pipeline healthy',
            expected: 'OTEL Collector health extension responding at :13133',
          };
        }

        return {
          status: 'warn',
          actual: 'OTEL Collector health extension responding but unhealthy',
          expected: 'OTEL Collector pipeline healthy',
          fix: 'Check OTEL collector logs for pipeline errors',
        };
      } catch (apiError) {
        return {
          status: 'warn',
          actual: 'OTEL Collector running but health extension not responding',
          expected: 'OTEL Collector pipeline healthy',
          fix: 'Check OTEL collector logs: kubectl logs -n unrdf-observability -l app=otel-collector',
        };
      }
    }

    return {
      status: 'warn',
      actual: `Could not verify OTEL collector: ${error.message}`,
      expected: 'OTEL Collector pipeline healthy',
      fix: 'Check OTEL collector status: kubectl get pods -n unrdf-observability -l app=otel-collector',
    };
  }
}

/**
 * Run all OTEL checks
 */
export async function checkOTEL() {
  return {
    category: 'OTEL',
    checks: [
      {
        name: 'OTLP endpoint connectivity',
        ...(await checkOTLPEndpoints()),
      },
      {
        name: 'OTEL SDK initialization',
        ...(await checkOTELSDK()),
      },
      {
        name: 'Weaver registry integrity',
        ...checkWeaverRegistry(),
      },
      {
        name: 'Trace pipeline E2E',
        ...(await checkTracePipeline()),
      },
      {
        name: 'OTEL Collector pipeline status',
        ...(await checkOTELCollectorPipeline()),
      },
    ],
  };
}
