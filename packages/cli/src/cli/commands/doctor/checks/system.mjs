/**
 * @file System Health Checks
 * @module cli/commands/doctor/checks/system
 *
 * @description
 * Checks for system state including build artifacts, daemon status,
 * MCP server, RDF store, port availability, and disk space.
 */

import { execSync } from 'node:child_process';
import { createServer } from 'node:net';
import { readFileSync, existsSync, readdirSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

/**
 * Check build artifacts
 */
function checkBuildArtifacts() {
  const packagesDir = join(projectRoot, 'packages');

  if (!existsSync(packagesDir)) {
    return {
      status: 'fail',
      actual: 'packages directory not found',
      expected: 'packages/ directory exists',
      fix: 'Ensure you are in the correct directory',
    };
  }

  try {
    const packages = readdirSync(packagesDir);
    const missingDist = [];
    const noBuildNeeded = [];

    for (const pkg of packages) {
      const pkgJsonPath = join(packagesDir, pkg, 'package.json');
      if (!existsSync(pkgJsonPath)) {
        continue;
      }

      const pkgJson = JSON.parse(readFileSync(pkgJsonPath, 'utf-8'));
      const hasBuildScript = pkgJson.scripts && pkgJson.scripts.build;

      const distPath = join(packagesDir, pkg, 'dist');
      if (!existsSync(distPath)) {
        if (hasBuildScript) {
          missingDist.push(pkg);
        } else {
          noBuildNeeded.push(pkg);
        }
      }
    }

    if (missingDist.length === 0) {
      const statusMsg =
        noBuildNeeded.length > 0
          ? `All packages OK (${noBuildNeeded.length} packages are source-only)`
          : 'Build artifacts present';
      return {
        status: 'pass',
        actual: statusMsg,
        expected: 'dist/ exists for buildable packages',
      };
    }

    return {
      status: 'warn',
      actual: `Missing dist/ in: ${missingDist.slice(0, 5).join(', ')}${missingDist.length > 5 ? '...' : ''}`,
      expected: 'dist/ exists for buildable packages',
      note: `${noBuildNeeded.length} packages are source-only (no build needed)`,
      fix: 'Run: pnpm build (for packages with build scripts)',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check build artifacts: ${error.message}`,
      expected: 'dist/ exists for buildable packages',
      fix: 'Run: pnpm build (for packages with build scripts)',
    };
  }
}

/**
 * Check daemon operations (CLI command availability)
 */
async function checkDaemonReal() {
  try {
    // Check if daemon command files exist
    const { existsSync } = await import('node:fs');
    const { join } = await import('node:path');

    const daemonListPath = join(projectRoot, 'packages/cli/src/cli/commands/daemon/list.mjs');
    const daemonStatusPath = join(projectRoot, 'packages/cli/src/cli/commands/daemon/status.mjs');
    const daemonRunPath = join(projectRoot, 'packages/cli/src/cli/commands/daemon/run.mjs');

    if (existsSync(daemonListPath) && existsSync(daemonStatusPath) && existsSync(daemonRunPath)) {
      return {
        status: 'pass',
        actual: 'daemon operations available',
        expected: 'Daemon CLI commands accessible',
      };
    }

    return {
      status: 'warn',
      actual: 'daemon commands partially available',
      expected: 'Daemon CLI commands accessible',
      fix: 'Ensure @unrdf/cli is properly installed',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `daemon commands unavailable: ${error.message}`,
      expected: 'Daemon CLI commands accessible',
      fix: 'Ensure @unrdf/cli is properly installed',
    };
  }
}

/**
 * Check MCP server status
 */
async function checkMCPStatus() {
  try {
    const { getMCPServerStatus } = await import('@unrdf/daemon/mcp');
    const status = await getMCPServerStatus();

    if (status.running) {
      return {
        status: 'pass',
        actual: `running (PID: ${status.pid})`,
        expected: 'MCP server running',
        details: status,
      };
    }

    return {
      status: 'fail',
      actual: 'MCP server not running',
      expected: 'MCP server running',
      fix: 'Run: unrdf mcp start',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check MCP status: ${error.message}`,
      expected: 'MCP server running',
      fix: 'Run: unrdf mcp start',
    };
  }
}

/**
 * Check RDF store
 */
function checkRDFStore() {
  const storePath = process.env.UNRDF_STORE_PATH || join(projectRoot, '.unrdf/store');

  if (existsSync(storePath)) {
    try {
      const stats = statSync(storePath);
      return {
        status: 'pass',
        actual: `Store exists (${formatBytes(stats.size)})`,
        expected: 'RDF store accessible',
      };
    } catch (error) {
      return {
        status: 'pass',
        actual: 'Store directory exists',
        expected: 'RDF store accessible',
      };
    }
  }

  return {
    status: 'warn',
    actual: 'RDF store not initialized',
    expected: 'RDF store directory exists',
    fix: 'Initialize RDF store or check UNRDF_STORE_PATH',
  };
}

/**
 * Check port availability
 */
async function checkPorts() {
  const ports = [
    { port: 8089, service: 'Daemon' },
    { port: 9089, service: 'Canopy' },
    { port: 50051, service: 'KGC Sidecar' },
  ];

  const conflicts = [];

  for (const { port, service } of ports) {
    try {
      // Try to bind to the port
      const server = createServer();

      await new Promise((resolve, reject) => {
        server.once('error', reject);
        server.listen(port, () => {
          server.once('close', resolve);
          server.close();
        });
      });
    } catch (error) {
      if (error.code === 'EADDRINUSE') {
        conflicts.push({ port, service });
      }
    }
  }

  if (conflicts.length === 0) {
    return {
      status: 'pass',
      actual: 'All required ports available',
      expected: 'Ports 8089, 9089, 50051 available',
      ports: [
        { port: 8089, service: 'Daemon', status: 'available' },
        { port: 9089, service: 'Canopy', status: 'available' },
        { port: 50051, service: 'KGC Sidecar', status: 'available' },
      ],
    };
  }

  return {
    status: 'warn',
    actual: `Port conflicts: ${conflicts.map(c => `:${c.port} (${c.service})`).join(', ')}`,
    expected: 'All ports available',
    ports: [
      {
        port: 8089,
        service: 'Daemon',
        status: conflicts.find(c => c.port === 8089) ? 'conflict' : 'available',
      },
      {
        port: 9089,
        service: 'Canopy',
        status: conflicts.find(c => c.port === 9089) ? 'conflict' : 'available',
      },
      {
        port: 50051,
        service: 'KGC Sidecar',
        status: conflicts.find(c => c.port === 50051) ? 'conflict' : 'available',
      },
    ],
    fix: 'Stop conflicting services or use different ports',
  };
}

/**
 * Check disk space
 */
async function checkDiskSpace() {
  try {
    const platform = process.platform;

    // Use cross-platform disk space check
    // On Windows, use check-disk-space package
    // On Unix-like systems, use df command
    let availableGB;
    let availableStr;
    let percentUsedStr;

    if (platform === 'win32') {
      try {
        // Use check-disk-space package for cross-platform support
        const checkDiskSpace = (await import('check-disk-space')).default;
        const diskInfo = await checkDiskSpace('.');

        availableGB = diskInfo.free / (1024 * 1024 * 1024); // Convert bytes to GB
        availableStr = `${availableGB.toFixed(1)}G`;
        percentUsedStr = `${((diskInfo.used / diskInfo.total) * 100).toFixed(0)}%`;
      } catch (error) {
        return {
          status: 'warn',
          actual: `Disk space check failed: ${error.message}`,
          expected: '>=1GB free',
          fix: 'Install check-disk-space package: pnpm add check-disk-space',
        };
      }
    } else {
      // On Unix-like systems, use df command
      const output = execSync('df -h .', { encoding: 'utf-8' });
      const lines = output.split('\n');
      if (lines.length < 2) {
        return {
          status: 'warn',
          actual: 'Could not determine disk space',
          expected: '>=1GB free',
          fix: 'Ensure sufficient disk space',
        };
      }

      const parts = lines[1].split(/\s+/);
      const available = parts[3]; // Available column
      percentUsedStr = parts[4]; // Use% column

      // Parse available (e.g., "50G", "500M", "95Gi")
      const availableMatch = available.match(/^(\d+\.?\d*)([MGTP]i?)?$/);
      if (availableMatch) {
        const value = parseFloat(availableMatch[1]);
        const unit = (availableMatch[2] || 'K').toUpperCase();

        // Convert to GB
        availableGB =
          unit === 'T' || unit === 'TI'
            ? value * 1024
            : unit === 'G' || unit === 'GI'
              ? value
              : unit === 'M' || unit === 'MI'
                ? value / 1024
                : unit === 'K' || unit === 'KI'
                  ? value / 1024 / 1024
                  : 0;

        availableStr = available;
      } else {
        return {
          status: 'warn',
          actual: `Could not parse disk space: ${available}`,
          expected: '>=1GB free',
          fix: 'Check disk space manually',
        };
      }
    }

    // Common validation for both platforms
    if (availableGB >= 1) {
      return {
        status: 'pass',
        actual: `${availableStr} available (${percentUsedStr} used)`,
        expected: '>=1GB free',
      };
    }

    return {
      status: 'fail',
      actual: `${availableStr} available (${percentUsedStr} used)`,
      expected: '>=1GB free',
      critical: true,
      fix: 'Free up disk space (remove node_modules, clean caches, etc.)',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: `Could not check disk space: ${error.message}`,
      expected: '>=1GB free',
      fix: 'Check disk space manually',
    };
  }
}

/**
 * Format bytes to human readable
 */
function formatBytes(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${parseFloat((bytes / Math.pow(k, i)).toFixed(1))} ${sizes[i]}`;
}

/**
 * Run all system checks
 */
export async function checkSystem() {
  return {
    category: 'System',
    checks: [
      {
        name: 'Build artifacts',
        ...checkBuildArtifacts(),
      },
      {
        name: 'Daemon status',
        ...(await checkDaemonReal()),
      },
      {
        name: 'MCP server status',
        ...(await checkMCPStatus()),
      },
      {
        name: 'RDF store',
        ...checkRDFStore(),
      },
      {
        name: 'Port availability',
        ...(await checkPorts()),
      },
      {
        name: 'Disk space',
        ...(await checkDiskSpace()),
      },
    ],
  };
}
