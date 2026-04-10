/**
 * @file Environment Health Checks
 * @module cli/commands/doctor/checks/env
 *
 * @description
 * Checks for developer environment including Node.js, pnpm,
 * dependencies, configuration, and required tools.
 */

import { execSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const projectRoot = join(__dirname, '../../../../../../..');

/**
 * Check Node.js version (>=18.0.0)
 */
function checkNodeVersion() {
  const version = process.version;
  const major = parseInt(version.slice(1).split('.')[0], 10);

  if (major >= 18) {
    return {
      status: 'pass',
      actual: version,
      expected: '>=18.0.0',
    };
  }

  return {
    status: 'fail',
    actual: version,
    expected: '>=18.0.0',
    critical: true,
    fix: 'Update Node.js to version 18 or later: https://nodejs.org/',
  };
}

/**
 * Check pnpm version (>=7.0.0)
 */
function checkPnpmVersion() {
  try {
    const version = execSync('pnpm --version', { encoding: 'utf-8' }).trim();
    const major = parseInt(version.split('.')[0], 10);

    if (major >= 7) {
      return {
        status: 'pass',
        actual: `v${version}`,
        expected: '>=7.0.0',
      };
    }

    return {
      status: 'fail',
      actual: `v${version}`,
      expected: '>=7.0.0',
      critical: true,
      fix: 'Update pnpm: npm install -g pnpm@latest',
    };
  } catch (error) {
    return {
      status: 'fail',
      actual: 'pnpm not found',
      expected: 'pnpm >=7.0.0',
      critical: true,
      fix: 'Install pnpm: npm install -g pnpm',
    };
  }
}

/**
 * Check node_modules consistency
 */
function checkNodeModules() {
  const nodeModulesPath = join(projectRoot, 'node_modules');

  if (!existsSync(nodeModulesPath)) {
    return {
      status: 'fail',
      actual: 'node_modules missing',
      expected: 'node_modules present',
      fix: 'Run: pnpm install',
    };
  }

  try {
    // Check for broken symlinks (common in pnpm workspaces)
    const result = execSync('find node_modules -xtype l -! -exec test -e {} \\;', {
      cwd: projectRoot,
      encoding: 'utf-8',
      stdio: 'pipe',
    });

    if (result.trim()) {
      return {
        status: 'fail',
        actual: 'Broken symlinks detected',
        expected: 'No broken symlinks',
        fix: 'Run: pnpm install --frozen-lockfile',
      };
    }

    return {
      status: 'pass',
      actual: 'node_modules consistent',
      expected: 'node_modules present and valid',
    };
  } catch (error) {
    // No broken symlinks found (exit code 1 from find means no matches)
    if (error.status === 1) {
      return {
        status: 'pass',
        actual: 'node_modules consistent',
        expected: 'node_modules present and valid',
      };
    }

    return {
      status: 'warn',
      actual: 'Could not verify symlinks',
      expected: 'No broken symlinks',
      fix: 'Run: pnpm install --frozen-lockfile',
    };
  }
}

/**
 * Check workspace dependencies
 */
function checkWorkspaceDeps() {
  try {
    const pnpmList = execSync('pnpm list --depth 0', {
      cwd: projectRoot,
      encoding: 'utf-8',
      stdio: 'pipe',
    });

    // Check for warning signs in output
    if (pnpmList.includes('No compatible version found')) {
      return {
        status: 'fail',
        actual: 'Dependency version conflicts',
        expected: 'All dependencies resolved',
        fix: 'Run: pnpm install --force',
      };
    }

    return {
      status: 'pass',
      actual: 'Workspace dependencies linked',
      expected: 'All packages linked correctly',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: 'Could not verify workspace deps',
      expected: 'All dependencies resolved',
      fix: 'Run: pnpm install',
    };
  }
}

/**
 * Check environment variables
 */
function checkEnvVars() {
  const envExamplePath = join(projectRoot, '.env.example');

  if (!existsSync(envExamplePath)) {
    return {
      status: 'pass',
      actual: 'No .env.example found',
      expected: 'N/A',
    };
  }

  try {
    const envExample = readFileSync(envExamplePath, 'utf-8');
    const requiredVars = [];

    // Parse .env.example for required variables
    for (const line of envExample.split('\n')) {
      const match = line.match(/^([A-Z_]+)=/);
      if (match) {
        const varName = match[1];
        if (!process.env[varName]) {
          requiredVars.push(varName);
        }
      }
    }

    if (requiredVars.length === 0) {
      return {
        status: 'pass',
        actual: 'All required env vars set',
        expected: 'All required env vars set',
      };
    }

    return {
      status: 'warn',
      actual: `${requiredVars.length} missing env vars`,
      expected: 'All required env vars set',
      missing: requiredVars,
      fix: 'Copy .env.example to .env.local and configure',
    };
  } catch (error) {
    return {
      status: 'warn',
      actual: 'Could not check env vars',
      expected: 'All required env vars set',
      fix: 'Check .env.example and configure environment',
    };
  }
}

/**
 * Check required tools
 */
function checkRequiredTools() {
  const tools = ['git', 'node', 'pnpm'];
  const results = [];
  let allPresent = true;

  for (const tool of tools) {
    try {
      execSync(`${tool} --version`, { stdio: 'pipe' });
      results.push({ tool, present: true });
    } catch {
      results.push({ tool, present: false });
      allPresent = false;
    }
  }

  if (allPresent) {
    return {
      status: 'pass',
      actual: 'All required tools present',
      expected: 'git, node, pnpm',
      tools: results,
    };
  }

  const missing = results.filter(r => !r.present).map(r => r.tool);
  return {
    status: 'fail',
    actual: `Missing: ${missing.join(', ')}`,
    expected: 'git, node, pnpm',
    tools: results,
    fix: `Install missing tools: ${missing.join(', ')}`,
  };
}

/**
 * Run all environment checks
 */
export async function checkEnvironment() {
  return {
    category: 'Environment',
    checks: [
      {
        name: 'Node.js version',
        ...checkNodeVersion(),
      },
      {
        name: 'pnpm version',
        ...checkPnpmVersion(),
      },
      {
        name: 'node_modules consistency',
        ...checkNodeModules(),
      },
      {
        name: 'Workspace dependencies',
        ...checkWorkspaceDeps(),
      },
      {
        name: 'Environment variables',
        ...checkEnvVars(),
      },
      {
        name: 'Required tools',
        ...checkRequiredTools(),
      },
    ],
  };
}
