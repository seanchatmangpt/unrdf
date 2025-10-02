/**
 * @file CLI Context Wrapper
 * @module cli/utils/context-wrapper
 *
 * @description
 * Wraps CLI commands with store context for dependency injection
 * and proper resource management.
 */

import { setStoreContext } from '../../index.mjs';
import { loadConfig } from './config-loader.mjs';
import { handleError } from './error-handler.mjs';
import { existsSync } from 'node:fs';
import { readFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { Parser } from 'n3';

/**
 * Get path to persistent store file
 * @returns {string} Store file path
 */
function getStoreFilePath() {
  return '.unrdf-store.nq';
}

/**
 * Load persisted store data if exists
 * @returns {Promise<Array>} Array of quads
 */
async function loadPersistedStore() {
  const storePath = getStoreFilePath();
  if (!existsSync(storePath)) {
    return [];
  }

  try {
    const content = await readFile(storePath, 'utf-8');
    const parser = new Parser({ format: 'N-Quads' });
    return parser.parse(content);
  } catch (error) {
    // If store file is corrupted or empty, start fresh
    return [];
  }
}

/**
 * Initialize .unrdf directory structure
 * @returns {Promise<void>}
 */
async function initUnrdfDirectory() {
  const unrdfDir = join(process.cwd(), '.unrdf');

  // Create subdirectories
  const subdirs = ['hooks', 'policies', 'cache'];

  for (const subdir of subdirs) {
    const dirPath = join(unrdfDir, subdir);
    await mkdir(dirPath, { recursive: true });
  }
}

/**
 * Wrap command function with store context
 * @param {Function} commandFn - Command function
 * @param {string} commandName - Command name for error context
 * @returns {Function} Wrapped command function
 */
export function withContext(commandFn, commandName = 'command') {
  return async (ctx) => {
    try {
      // Initialize .unrdf directory structure early
      await initUnrdfDirectory();

      const config = await loadConfig();

      // Load persisted store data for store commands
      const initialQuads = commandName.startsWith('store') ? await loadPersistedStore() : [];

      // Initialize store context with persisted data
      setStoreContext(initialQuads, { baseIRI: config.baseIRI || 'http://example.org/' });

      await commandFn(ctx, config);
    } catch (error) {
      handleError(error, commandName);
    }
  };
}

/**
 * Create a command execution context
 * @param {Object} ctx - CLI context from citty
 * @param {Object} config - Loaded configuration
 * @returns {Object} Execution context
 */
export function createExecutionContext(ctx, config) {
  return {
    args: ctx.args,
    config,
    verbose: ctx.args.verbose || false,
    debug: ctx.args.debug || false
  };
}

/**
 * Validate required arguments
 * @param {Object} args - Command arguments
 * @param {Array<string>} required - Required argument names
 * @throws {Error} If required arguments are missing
 */
export function validateRequiredArgs(args, required) {
  const missing = required.filter(arg => !args[arg]);

  if (missing.length > 0) {
    throw new Error(`Missing required arguments: ${missing.join(', ')}`);
  }
}

/**
 * Get argument value with fallback
 * @param {Object} args - Command arguments
 * @param {string} name - Argument name
 * @param {any} defaultValue - Default value
 * @returns {any} Argument value or default
 */
export function getArg(args, name, defaultValue) {
  return args[name] !== undefined ? args[name] : defaultValue;
}
