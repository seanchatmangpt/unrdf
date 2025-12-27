/**
 * @fileoverview Frozen Environment Fixtures
 *
 * Provides deterministic environment mocks for testing:
 * - Frozen system time
 * - Mock file system
 * - Mock environment variables
 * - Mock Node.js version/capabilities
 *
 * @module @unrdf/kgc-probe/test/fixtures/frozen-environment
 */

/**
 * Frozen timestamp for deterministic testing
 * All tests use this as the "current" time
 */
export const FROZEN_TIME = new Date('2025-01-15T10:30:00.000Z');
export const FROZEN_TIMESTAMP = FROZEN_TIME.toISOString();

/**
 * Mock file system structure
 * Represents a typical project directory
 */
export const MOCK_FILESYSTEM = {
  '/project': {
    type: 'directory',
    children: {
      'package.json': {
        type: 'file',
        content: JSON.stringify({
          name: 'test-project',
          version: '1.0.0',
          type: 'module'
        }, null, 2),
        size: 78
      },
      'src': {
        type: 'directory',
        children: {
          'index.mjs': {
            type: 'file',
            content: 'export const main = () => console.log("Hello");',
            size: 48
          },
          'utils.mjs': {
            type: 'file',
            content: 'export const add = (a, b) => a + b;',
            size: 36
          }
        }
      },
      'test': {
        type: 'directory',
        children: {
          'index.test.mjs': {
            type: 'file',
            content: 'import { describe, it, expect } from "vitest";',
            size: 47
          }
        }
      },
      'node_modules': {
        type: 'directory',
        children: {}
      }
    }
  }
};

/**
 * Safe mock environment variables
 * Contains ONLY safe test values - no real secrets
 */
export const MOCK_ENV = {
  NODE_ENV: 'test',
  PATH: '/usr/bin:/bin',
  HOME: '/home/testuser',
  USER: 'testuser',
  LANG: 'en_US.UTF-8',
  TZ: 'UTC',
  // Test-only values (not real secrets)
  TEST_MODE: 'true',
  LOG_LEVEL: 'debug',
  MAX_WORKERS: '4'
};

/**
 * FORBIDDEN environment variable patterns
 * These should NEVER appear in observations
 */
export const FORBIDDEN_ENV_PATTERNS = [
  'API_KEY',
  'SECRET',
  'TOKEN',
  'PASSWORD',
  'PRIVATE',
  'CREDENTIAL',
  'AUTH',
  'AWS_ACCESS',
  'AWS_SECRET',
  'GITHUB_TOKEN',
  'NPM_TOKEN',
  'DATABASE_URL',
  'REDIS_URL',
  'MONGO_URI',
  'JWT_SECRET',
  'ENCRYPTION_KEY',
  'SIGNING_KEY',
  'MASTER_KEY',
  'CLIENT_SECRET',
  'OAUTH_SECRET',
  'SSH_KEY',
  'PGP_KEY',
  'CERT_KEY',
  'PRIVATE_KEY',
  'PASSPHRASE'
];

/**
 * Mock Node.js runtime information
 */
export const MOCK_RUNTIME = {
  nodeVersion: 'v22.12.0',
  platform: 'linux',
  arch: 'x64',
  wasmSupported: true,
  workersSupported: true,
  esModulesSupported: true,
  bigIntSupported: true,
  asyncIteratorsSupported: true,
  memoryUsage: {
    heapTotal: 50 * 1024 * 1024,
    heapUsed: 30 * 1024 * 1024,
    external: 5 * 1024 * 1024,
    arrayBuffers: 2 * 1024 * 1024
  },
  uptime: 3600
};

/**
 * Create frozen Date mock
 * @returns {Object} Mock Date class
 */
export function createFrozenDateMock() {
  const RealDate = Date;
  return class FrozenDate extends RealDate {
    constructor(...args) {
      if (args.length === 0) {
        super(FROZEN_TIME.getTime());
      } else {
        super(...args);
      }
    }

    static now() {
      return FROZEN_TIME.getTime();
    }
  };
}

/**
 * Create mock crypto.randomUUID that returns deterministic UUIDs
 * @param {number} [seed=0] - Starting seed
 * @returns {Function} Mock randomUUID function
 */
export function createDeterministicUUID(seed = 0) {
  let counter = seed;
  return function deterministicUUID() {
    const hex = (counter++).toString(16).padStart(32, '0');
    return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-4${hex.slice(13, 16)}-8${hex.slice(17, 20)}-${hex.slice(20, 32)}`;
  };
}

/**
 * Apply frozen environment for deterministic testing
 * @param {Object} globals - Global object to modify
 * @returns {Function} Cleanup function to restore originals
 */
export function applyFrozenEnvironment(globals = globalThis) {
  const originals = {
    Date: globals.Date,
    randomUUID: globals.crypto?.randomUUID
  };

  // Apply mocks
  globals.Date = createFrozenDateMock();

  if (globals.crypto) {
    globals.crypto.randomUUID = createDeterministicUUID();
  }

  // Return cleanup function
  return function restore() {
    globals.Date = originals.Date;
    if (globals.crypto && originals.randomUUID) {
      globals.crypto.randomUUID = originals.randomUUID;
    }
  };
}

/**
 * Mock filesystem operations
 */
export const mockFs = {
  readFile: async (path) => {
    const file = resolvePath(MOCK_FILESYSTEM, path);
    if (!file || file.type !== 'file') {
      throw new Error(`ENOENT: no such file: ${path}`);
    }
    return file.content;
  },

  writeFile: async (path, content) => {
    // Mock write - doesn't actually write
    return undefined;
  },

  readdir: async (path) => {
    const dir = resolvePath(MOCK_FILESYSTEM, path);
    if (!dir || dir.type !== 'directory') {
      throw new Error(`ENOTDIR: not a directory: ${path}`);
    }
    return Object.keys(dir.children);
  },

  stat: async (path) => {
    const item = resolvePath(MOCK_FILESYSTEM, path);
    if (!item) {
      throw new Error(`ENOENT: no such file or directory: ${path}`);
    }
    return {
      isFile: () => item.type === 'file',
      isDirectory: () => item.type === 'directory',
      size: item.size || 0
    };
  },

  exists: async (path) => {
    return resolvePath(MOCK_FILESYSTEM, path) !== null;
  }
};

/**
 * Resolve path in mock filesystem
 * @param {Object} fs - Filesystem object
 * @param {string} path - Path to resolve
 * @returns {Object|null} Resolved node or null
 */
function resolvePath(fs, path) {
  const parts = path.split('/').filter(Boolean);
  let current = fs[`/${parts[0]}`];

  for (let i = 1; i < parts.length && current; i++) {
    if (current.type === 'directory' && current.children) {
      current = current.children[parts[i]];
    } else {
      return null;
    }
  }

  return current || null;
}

export default {
  FROZEN_TIME,
  FROZEN_TIMESTAMP,
  MOCK_FILESYSTEM,
  MOCK_ENV,
  FORBIDDEN_ENV_PATTERNS,
  MOCK_RUNTIME,
  createFrozenDateMock,
  createDeterministicUUID,
  applyFrozenEnvironment,
  mockFs
};
