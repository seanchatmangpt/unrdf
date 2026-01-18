/**
 * @file Test HTTP server for load testing
 * @module test/load-testing/test-server
 * @description Simple HTTP server exposing UNRDF operations for load testing
 */

import { createServer } from 'node:http';
import { randomUUID } from 'node:crypto';

let requestCount = 0;
let errorCount = 0;
const startTime = Date.now();

// Test data (no external dependencies)
const testTriples = [
  { subject: 'ex:subject1', predicate: 'ex:predicate1', object: 'ex:object1' },
  { subject: 'ex:subject2', predicate: 'ex:predicate2', object: 'ex:object2' },
  { subject: 'ex:subject3', predicate: 'ex:predicate3', object: 'ex:object3' },
];

/**
 * Create a mock receipt (no external dependencies)
 * @param {Object} options - Receipt options
 * @returns {Object} Mock receipt
 */
function createMockReceipt(options) {
  return {
    id: randomUUID(),
    operation: options.operation,
    entityType: options.entityType,
    timestamp: options.timestamp || Date.now(),
    hash: randomUUID()
  };
}

/**
 * Handle HTTP requests
 * @param {import('node:http').IncomingMessage} req - Request
 * @param {import('node:http').ServerResponse} res - Response
 */
function handleRequest(req, res) {
  requestCount++;

  const url = new URL(req.url, `http://${req.headers.host}`);
  const pathname = url.pathname;

  try {
    // CORS headers for testing
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Content-Type', 'application/json');

    switch (pathname) {
      case '/health':
        // Health check endpoint
        res.writeHead(200);
        res.end(JSON.stringify({
          status: 'healthy',
          uptime: Date.now() - startTime,
          requestCount,
          errorCount
        }));
        break;

      case '/api/receipt':
        // Create receipt endpoint
        const receipt = createMockReceipt({
          operation: 'test',
          entityType: 'LoadTest',
          timestamp: Date.now()
        });
        res.writeHead(200);
        res.end(JSON.stringify({ receipt }));
        break;

      case '/api/triples':
        // Return test triples
        res.writeHead(200);
        res.end(JSON.stringify({ triples: testTriples }));
        break;

      case '/api/query':
        // Simulate SPARQL query
        const query = url.searchParams.get('q') || 'SELECT * WHERE { ?s ?p ?o }';
        // Simulate query execution time
        const delay = Math.random() * 10; // 0-10ms random delay
        setTimeout(() => {
          res.writeHead(200);
          res.end(JSON.stringify({
            query,
            results: testTriples,
            count: testTriples.length
          }));
        }, delay);
        break;

      case '/api/insert':
        // Simulate triple insertion
        if (req.method === 'POST') {
          let body = '';
          req.on('data', chunk => { body += chunk; });
          req.on('end', () => {
            try {
              const data = JSON.parse(body);
              res.writeHead(201);
              res.end(JSON.stringify({ success: true, inserted: data }));
            } catch (err) {
              errorCount++;
              res.writeHead(400);
              res.end(JSON.stringify({ error: 'Invalid JSON' }));
            }
          });
        } else {
          res.writeHead(405);
          res.end(JSON.stringify({ error: 'Method not allowed' }));
        }
        break;

      case '/api/stress':
        // CPU-intensive endpoint for stress testing
        let result = 0;
        for (let i = 0; i < 10000; i++) {
          result += Math.sqrt(i);
        }
        res.writeHead(200);
        res.end(JSON.stringify({ result }));
        break;

      case '/metrics':
        // Metrics endpoint
        res.writeHead(200);
        res.end(JSON.stringify({
          requests: requestCount,
          errors: errorCount,
          uptime: Date.now() - startTime,
          requestsPerSecond: requestCount / ((Date.now() - startTime) / 1000),
          errorRate: errorCount / requestCount
        }));
        break;

      default:
        res.writeHead(404);
        res.end(JSON.stringify({ error: 'Not found' }));
    }
  } catch (err) {
    errorCount++;
    console.error('Request error:', err);
    res.writeHead(500);
    res.end(JSON.stringify({ error: err.message }));
  }
}

/**
 * Start the test server
 * @param {number} [port=3000] - Port to listen on
 * @returns {Promise<import('node:http').Server>} Server instance
 */
export function startServer(port = 3000) {
  return new Promise((resolve) => {
    const server = createServer(handleRequest);

    server.listen(port, () => {
      console.log(`Test server listening on port ${port}`);
      console.log(`Health: http://localhost:${port}/health`);
      console.log(`Metrics: http://localhost:${port}/metrics`);
      resolve(server);
    });
  });
}

/**
 * Stop the server
 * @param {import('node:http').Server} server - Server instance
 * @returns {Promise<void>}
 */
export function stopServer(server) {
  return new Promise((resolve) => {
    server.close(() => {
      console.log('Test server stopped');
      resolve();
    });
  });
}

// Start server if run directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const port = process.env.PORT || 3000;
  startServer(port);
}
