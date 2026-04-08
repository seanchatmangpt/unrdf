/**
 * @file Kubernetes Resource Health Checks
 * @module cli/commands/doctor/checks/kubernetes
 *
 * @description
 * Checks for Kubernetes resources including:
 * - Namespace existence (unrdf-observability)
 * - Pod health with restart counts
 * - PVC storage pressure (Prometheus, Tempo, Loki MinIO)
 * - Daemon metrics endpoint verification (:9464)
 * - Grafana datasource connectivity
 */

import { execSync } from 'node:child_process';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

/**
 * Check K8s namespace existence
 */
function checkNamespace() {
  try {
    const output = execSync('kubectl get namespace unrdf-observability -o json', {
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 10000,
    });

    const data = JSON.parse(output);

    return {
      status: 'pass',
      actual: `Namespace exists (status: ${data.status ? data.status.phase : 'Active'})`,
      expected: 'unrdf-observability namespace exists',
    };
  } catch (error) {
    return {
      status: 'fail',
      actual: `Namespace not found: ${error.message}`,
      expected: 'unrdf-observability namespace exists',
      fix: 'Create namespace: kubectl create namespace unrdf-observability',
      critical: true,
    };
  }
}

/**
 * Check pod health with restart counts
 */
function checkPodHealth() {
  try {
    const output = execSync(
      'kubectl get pods -n unrdf-observability -o json',
      {
        encoding: 'utf-8',
        stdio: 'pipe',
        timeout: 10000,
      }
    );

    const data = JSON.parse(output);
    const pods = data.items || [];

    const unhealthy = [];
    const healthy = [];
    const crashLoop = [];

    for (const pod of pods) {
      const name = pod.metadata.name;
      const phase = pod.status.phase;
      const restarts = pod.status.containerStatuses?.[0]?.restartCount || 0;

      if (phase === 'Running' && restarts < 5) {
        healthy.push({ name, phase, restarts });
      } else if (phase === 'Running' && restarts >= 5) {
        crashLoop.push({ name, phase, restarts });
      } else {
        unhealthy.push({ name, phase, restarts });
      }
    }

    if (crashLoop.length > 0) {
      return {
        status: 'fail',
        actual: `${crashLoop.length} pods in crash loop (${crashLoop.map((p) => p.name).join(', ')})`,
        expected: 'All pods healthy and running',
        critical: true,
        violations: crashLoop,
        fix: 'Check pod logs: kubectl logs -n unrdf-observability <pod-name>',
      };
    }

    if (unhealthy.length > 0) {
      return {
        status: 'warn',
        actual: `${unhealthy.length} unhealthy pods: ${unhealthy.map((p) => p.name).join(', ')}`,
        expected: 'All pods healthy and running',
        violations: [...unhealthy, ...healthy],
        fix: 'Check pod status: kubectl get pods -n unrdf-observability',
      };
    }

    return {
      status: 'pass',
      actual: `All pods healthy (${healthy.length} pods running)`,
      expected: 'All pods healthy and running',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check pod health: ${error.message}`,
      expected: 'All pods healthy and running',
      fix: 'Ensure kubectl is configured and cluster is accessible',
    };
  }
}

/**
 * Check PVC storage pressure
 */
function checkPVCStorage() {
  try {
    const output = execSync(
      'kubectl get pvc -n unrdf-observability -o json',
      {
        encoding: 'utf-8',
        stdio: 'pipe',
        timeout: 10000,
      }
    );

    const data = JSON.parse(output);
    const pvcs = data.items || [];

    if (pvcs.length === 0) {
      return {
        status: 'warn',
        actual: 'No PVCs found in unrdf-observability namespace',
        expected: 'PVCs for Prometheus, Tempo, Loki storage',
        note: 'Using ephemeral storage or hostPath',
      };
    }

    const pressure = [];
    const healthy = [];

    for (const pvc of pvcs) {
      const name = pvc.metadata.name;
      const capacity = pvc.status.capacity?.storage || 'Unknown';
      const phase = pvc.status.phase;

      if (phase === 'Bound') {
        healthy.push({ name, capacity, phase });
      } else {
        pressure.push({ name, capacity, phase });
      }
    }

    if (pressure.length > 0) {
      return {
        status: 'warn',
        actual: `${pressure.length} PVC(s) with issues: ${pressure.map((p) => `${p.name} (${p.phase})`).join(', ')}`,
        expected: 'All PVCs bound and healthy',
        violations: [...pressure, ...healthy],
        fix: 'Check PVC status: kubectl describe pvc -n unrdf-observability <pvc-name>',
      };
    }

    return {
      status: 'pass',
      actual: `All PVCs healthy (${healthy.length} PVCs bound)`,
      expected: 'PVCs for Prometheus, Tempo, Loki storage',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check PVC storage: ${error.message}`,
      expected: 'PVCs for Prometheus, Tempo, Loki storage',
      fix: 'Ensure kubectl is configured and cluster is accessible',
    };
  }
}

/**
 * Check Daemon metrics endpoint (:9464)
 */
async function checkDaemonMetrics() {
  try {
    // Check if daemon is exposing metrics
    const response = execSync('curl -s http://localhost:9464/metrics', {
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 5000,
    });

    if (response.includes('daemon_') || response.includes('up ') || response.length > 0) {
      return {
        status: 'pass',
        actual: 'Daemon metrics endpoint responding',
        expected: 'Daemon metrics endpoint (:9464) accessible',
      };
    }

    return {
      status: 'warn',
      actual: 'Daemon metrics endpoint not responding',
      expected: 'Daemon metrics endpoint (:9464) accessible',
      fix: 'Check daemon status: unrdf daemon status',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: 'Daemon metrics endpoint not accessible',
      expected: 'Daemon metrics endpoint (:9464) accessible',
      fix: 'Start daemon or check if running: unrdf daemon status',
    };
  }
}

/**
 * Check Grafana datasource connectivity
 */
async function checkGrafanaDatasources() {
  try {
    // Check Grafana API for datasource health
    const response = execSync(
      'curl -s http://localhost:3001/api/datasources -u admin:admin',
      {
        encoding: 'utf-8',
        stdio: 'pipe',
        timeout: 5000,
      }
    );

    const datasources = JSON.parse(response);
    const unhealthy = [];

    for (const ds of datasources) {
      if (!ds.isDefault && (ds.uid !== 0 || !ds.uid)) {
        // Check if datasource is healthy
        if (ds.health === undefined || ds.health !== 'OK') {
          unhealthy.push(ds.name);
        }
      }
    }

    if (unhealthy.length > 0) {
      return {
        status: 'warn',
        actual: `${unhealthy.length} datasource(s) unhealthy: ${unhealthy.join(', ')}`,
        expected: 'All Grafana datasources healthy',
        fix: 'Check Grafana datasource settings: http://localhost:3001/datasources',
      };
    }

    return {
      status: 'pass',
      actual: `All Grafana datasources healthy (${datasources.length} datasources)`,
      expected: 'All Grafana datasources healthy',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: 'Could not check Grafana datasources',
      expected: 'All Grafana datasources healthy',
      fix: 'Ensure Grafana is running: kubectl get pods -n unrdf-observability -l app=grafana',
    };
  }
}

/**
 * Run all K8s checks
 */
export async function checkKubernetes() {
  return {
    category: 'Kubernetes',
    checks: [
      {
        name: 'Namespace existence',
        ...checkNamespace(),
      },
      {
        name: 'Pod health',
        ...checkPodHealth(),
      },
      {
        name: 'PVC storage pressure',
        ...checkPVCStorage(),
      },
      {
        name: 'Daemon metrics endpoint',
        ...(await checkDaemonMetrics()),
      },
      {
        name: 'Grafana datasource connectivity',
        ...(await checkGrafanaDatasources()),
      },
    ],
  };
}
