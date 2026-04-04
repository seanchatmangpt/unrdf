/**
 * @file MCP Server Implementation
 * @module @unrdf/daemon/mcp/server
 * @description Core MCP Server using @modelcontextprotocol/sdk
 */

import {
  Server,
} from '@modelcontextprotocol/sdk/server/index.js';
import { readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { registerTools } from './tools.mjs';
import { registerResources } from './resources.mjs';
import { registerPrompts } from './prompts.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const pkgVersion = JSON.parse(
  readFileSync(resolve(__dirname, '../../package.json'), 'utf-8')
).version;

/**
 * Create and configure the MCP server
 * @param {object} options - Configuration options
 * @param {string} options.name - Server name (default: 'unrdf')
 * @param {string} options.version - Server version (default: from package.json)
 * @returns {Server} Configured MCP server instance
 */
export function createMCPServer(options = {}) {
  const {
    name = 'unrdf',
    version = pkgVersion,
  } = options;

  // Create server instance with capabilities
  const server = new Server(
    {
      name,
      version,
    },
    {
      capabilities: {
        tools: {},
        resources: {},
        prompts: {},
      },
    }
  );

  // Register all tools, resources, and prompts
  registerTools(server, options);
  registerResources(server, options);
  registerPrompts(server, options);

  return server;
}
