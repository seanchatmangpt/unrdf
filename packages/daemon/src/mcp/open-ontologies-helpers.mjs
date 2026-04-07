/**
 * @file Open-Ontologies MCP Helpers
 * @description CLI wrapper functions for open-ontologies command execution
 */

import { spawn } from 'node:child_process';
import { mkdirSync, existsSync } from 'node:fs';
import { ONTO_BINARY, ONTO_DATA_DIR } from './open-ontologies-registry.mjs';

/**
 * Default timeout for CLI commands (5 seconds)
 */
const DEFAULT_TIMEOUT_MS = 5000;

/**
 * Run open-ontologies CLI command with timeout and JSON parsing
 * @param {string[]} args - CLI arguments (e.g., ['validate', 'file.ttl'])
 * @param {object} options - Options
 * @param {number} options.timeoutMs - Timeout in milliseconds (default: 5000)
 * @param {object} options.env - Environment variables to merge
 * @param {string} options.input - Stdin input (for commands like load that accept -)
 * @returns {Promise<object>} Parsed JSON response or stdout
 */
export async function runOntoCommand(args, options = {}) {
  const {
    timeoutMs = DEFAULT_TIMEOUT_MS,
    env = {},
    input = null,
  } = options;

  return new Promise((resolve, reject) => {
    const proc = spawn(ONTO_BINARY, args, {
      stdio: ['pipe', 'pipe', 'pipe'],
      env: { ...process.env, DATA_DIR: ONTO_DATA_DIR, ...env },
    });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    proc.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    // Write input to stdin if provided
    if (input !== null) {
      proc.stdin.write(input);
      proc.stdin.end();
    }

    const timeout = setTimeout(() => {
      proc.kill();
      reject(new Error(`Command timeout after ${timeoutMs}ms: ${args.join(' ')}`));
    }, timeoutMs);

    proc.on('close', (code) => {
      clearTimeout(timeout);

      if (code === 0) {
        try {
          // Try parsing JSON first
          resolve(JSON.parse(stdout));
        } catch {
          // If not JSON, return raw stdout
          resolve(stdout);
        }
      } else {
        const errorMsg = stderr || stdout || 'Unknown error';
        reject(new Error(`Command failed (${code}): ${errorMsg}`));
      }
    });

    proc.on('error', (error) => {
      clearTimeout(timeout);
      reject(new Error(`Failed to spawn open-ontologies binary: ${error.message}`));
    });
  });
}

/**
 * Validate open-ontologies installation
 * @returns {Promise<boolean>} True if binary exists and is executable
 * @throws {Error} If binary not found
 */
export async function validateOntoInstallation() {
  if (!existsSync(ONTO_BINARY)) {
    throw new Error(
      `open-ontologies binary not found at ${ONTO_BINARY}\n` +
      `Install with: cargo install open-ontologies\n` +
      `Or set ONTO_BINARY environment variable`
    );
  }

  // Try running --version to verify it works
  try {
    await runOntoCommand(['--version'], { timeoutMs: 2000 });
    return true;
  } catch (error) {
    throw new Error(`open-ontologies binary exists but failed to run: ${error.message}`);
  }
}

/**
 * Ensure open-ontologies data directory exists
 * @returns {Promise<string>} Path to data directory
 */
export async function ensureDataDir() {
  if (!existsSync(ONTO_DATA_DIR)) {
    mkdirSync(ONTO_DATA_DIR, { recursive: true });
  }
  return ONTO_DATA_DIR;
}

/**
 * Format error for MCP response
 * @param {Error} error - Error object
 * @returns {string} Formatted error message
 */
export function formatOntoError(error) {
  if (error.message.includes('Command timeout')) {
    return `Timeout: open-ontologies command took too long. Try with a smaller dataset or increase timeout.`;
  }
  if (error.message.includes('not found')) {
    return `open-ontologies binary not found. Install with: cargo install open-ontologies`;
  }
  if (error.message.includes('Command failed')) {
    return `open-ontologies error: ${error.message}`;
  }
  return `Error: ${error.message}`;
}

/**
 * Create MCP-formatted response
 * @param {*} result - Command result (object or string)
 * @param {string} _toolName - Tool name for logging (unused, for future logging)
 * @returns {object} MCP response object
 */
export function createOntoResponse(result, _toolName) {
  const text = typeof result === 'string' ? result : JSON.stringify(result, null, 2);

  return {
    content: [
      {
        type: 'text',
        text,
      },
    ],
    isError: false,
  };
}

/**
 * Create MCP-formatted error response
 * @param {Error} error - Error object
 * @param {string} _toolName - Tool name for logging (unused, for future logging)
 * @returns {object} MCP error response object
 */
export function createOntoErrorResponse(error, _toolName) {
  return {
    content: [
      {
        type: 'text',
        text: formatOntoError(error),
      },
    ],
    isError: true,
  };
}
