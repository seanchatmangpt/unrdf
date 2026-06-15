/**
 * @fileoverview MCP SSE Transport Implementation
 * @module @unrdf/daemon/mcp/sse
 */

import { createServer } from 'node:http';
import { SSEServerTransport } from '@modelcontextprotocol/sdk/server/sse.js';

/**
 * Start MCP server using SSE transport
 * @param {McpServer} server - MCP server instance
 * @param {Object} options - Configuration options
 * @param {number} options.port - Port to listen on
 * @param {string} [options.path='/sse'] - SSE endpoint path
 * @returns {Promise<http.Server>} Started HTTP server
 */
export async function startMCPServerSSE(server, options = {}) {
  const port = options.port || 3001;
  const path = options.path || '/sse';
  
  let transport;

  const httpServer = createServer((req, res) => {
    if (req.url === path) {
      transport = new SSEServerTransport('/messages', res);
      server.connect(transport).catch(err => {
        console.error('MCP Server connection error:', err);
      });
    } else if (req.url === '/messages' && req.method === 'POST') {
      if (!transport) {
        res.writeHead(400);
        res.end('No active SSE connection');
        return;
      }
      transport.handlePostMessage(req, res);
    } else {
      res.writeHead(404);
      res.end();
    }
  });

  return new Promise((resolve, reject) => {
    httpServer.listen(port, (err) => {
      if (err) return reject(err);
      console.log(`MCP SSE Server listening on port ${port}${path}`);
      resolve(httpServer);
    });
  });
}
