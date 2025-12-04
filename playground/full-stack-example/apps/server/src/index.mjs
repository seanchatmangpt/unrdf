/**
 * @fileoverview UNRDF Full-Stack Example - Server
 * Demonstrates all server-side UNRDF packages working together
 */

import { createServer as createHttpServer } from 'node:http';
import { WebSocketServer } from 'ws';
import { Store, Parser } from 'n3';
import { KnowledgeHookManager } from '@unrdf/hooks';
import { createStreamProcessor } from '@unrdf/streaming';
import { createCoordinator } from '@unrdf/federation';
import { executeQuery } from '@unrdf/core';
import { createStore as createOxigraphStore } from '@unrdf/oxigraph';

// ============================================================================
// CONFIGURATION
// ============================================================================

const PORT = process.env.PORT || 3000;
const WS_PORT = process.env.WS_PORT || 3001;
const MAX_BODY_SIZE = 1024 * 1024; // 1MB limit

// ============================================================================
// RDF STORE SETUP
// ============================================================================

/** @type {Store} */
const store = createStore();

// Load sample RDF data
const sampleData = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person1 a foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:knows ex:Person2 ;
  foaf:mbox <mailto:alice@example.org> .

ex:Person2 a foaf:Person ;
  foaf:name "Bob Johnson" ;
  foaf:knows ex:Person1 ;
  foaf:mbox <mailto:bob@example.org> .

ex:Person3 a foaf:Person ;
  foaf:name "Carol White" ;
  foaf:knows ex:Person1 ;
  foaf:mbox <mailto:carol@example.org> .
`;

const parser = new Parser();
const quads = parser.parse(sampleData);
quads.forEach(quad => store.addQuad(quad));

console.log(`📊 Loaded ${quads.length} triples into RDF store`);

// ============================================================================
// KNOWLEDGE HOOKS SETUP
// ============================================================================

const hookManager = new KnowledgeHookManager();

// Define validation hook
hookManager.define({
  name: 'validateEmail',
  trigger: 'before-add',
  validate: quad => {
    if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/mbox') {
      const email = quad.object.value;
      return email.startsWith('mailto:') && email.includes('@');
    }
    return true;
  },
});

// Define enrichment hook
hookManager.define({
  name: 'addTimestamp',
  trigger: 'after-add',
  transform: quad => {
    // Return the quad as-is (transformation happens in the server endpoint)
    return quad;
  },
});

console.log('🔗 Knowledge Hooks configured');

// ============================================================================
// STREAMING SETUP
// ============================================================================

const streamingProcessor = createStreamProcessor({ store });
const changeFeed = createStreamProcessor({ store });

// WebSocket server for real-time updates
const wss = new WebSocketServer({ port: WS_PORT });

// Track WebSocket clients
const wsClients = new Set();

wss.on('connection', ws => {
  console.log('🔌 WebSocket client connected');
  wsClients.add(ws);

  // Send initial data
  const allQuads = store.getQuads();
  ws.send(
    JSON.stringify({
      type: 'initial',
      quads: allQuads.map(q => ({
        subject: q.subject.value,
        predicate: q.predicate.value,
        object: q.object.value,
      })),
    })
  );

  ws.on('close', () => {
    console.log('🔌 WebSocket client disconnected');
    wsClients.delete(ws);
  });
});

// Helper to broadcast changes to all WebSocket clients
function broadcastChange(operation, quad) {
  const message = JSON.stringify({
    type: 'change',
    operation,
    quad: {
      subject: quad.subject.value,
      predicate: quad.predicate.value,
      object: quad.object.value,
    },
  });

  for (const client of wsClients) {
    if (client.readyState === 1) {
      // WebSocket.OPEN
      client.send(message);
    }
  }
}

console.log(`🌊 WebSocket server listening on port ${WS_PORT}`);

// ============================================================================
// FEDERATION SETUP
// ============================================================================

const federation = createCoordinator({
  localStore: store,
});

console.log('🌐 Federation coordinator initialized');

// ============================================================================
// SPARQL QUERY HELPER
// ============================================================================

/**
 * Execute SPARQL query on n3.Store by converting to Oxigraph
 * @param {Store} n3Store - N3 store with quads
 * @param {string} sparql - SPARQL query string
 * @returns {Promise<Object|boolean|Array>} Query results
 */
async function executeSparqlQuery(n3Store, sparql) {
  const quads = n3Store.getQuads();
  const oxStore = createOxigraphStore(quads);
  return executeQuery(oxStore, sparql);
}

// ============================================================================
// HTTP API ENDPOINTS
// ============================================================================

const server = createHttpServer(async (req, res) => {
  // CORS headers
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');

  if (req.method === 'OPTIONS') {
    res.writeHead(204);
    res.end();
    return;
  }

  const url = new URL(req.url, `http://localhost:${PORT}`);

  // GET /api/quads - Get all triples
  if (req.method === 'GET' && url.pathname === '/api/quads') {
    const quads = store.getQuads();
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(
      JSON.stringify({
        count: quads.length,
        quads: quads.map(q => ({
          subject: q.subject.value,
          predicate: q.predicate.value,
          object: q.object.value,
          objectType: q.object.termType,
        })),
      })
    );
    return;
  }

  // GET /api/query - SPARQL query
  if (req.method === 'GET' && url.pathname === '/api/query') {
    const query = url.searchParams.get('q');
    if (!query) {
      res.writeHead(400, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({ error: 'Missing query parameter' }));
      return;
    }

    try {
      const startTime = Date.now();
      const results = await executeSparqlQuery(store, query);
      const executionTime = Date.now() - startTime;

      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(
        JSON.stringify({
          query,
          results,
          executionTime,
        })
      );
    } catch (error) {
      res.writeHead(400, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({ error: error.message }));
    }
    return;
  }

  // POST /api/query - SPARQL query
  if (req.method === 'POST' && url.pathname === '/api/query') {
    let body = '';
    let totalSize = 0;

    req.on('data', chunk => {
      totalSize += chunk.length;
      if (totalSize > MAX_BODY_SIZE) {
        res.writeHead(413, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Payload Too Large' }));
        req.destroy();
        return;
      }
      body += chunk.toString();
    });

    req.on('end', async () => {
      try {
        const { sparql } = JSON.parse(body);
        if (!sparql) {
          res.writeHead(400, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: 'Missing SPARQL query' }));
          return;
        }

        const startTime = Date.now();
        const results = await executeSparqlQuery(store, sparql);
        const executionTime = Date.now() - startTime;

        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(
          JSON.stringify({
            results,
            executionTime,
          })
        );
      } catch (error) {
        res.writeHead(400, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: error.message }));
      }
    });
    return;
  }

  // POST /api/quads - Add new triple
  if (req.method === 'POST' && url.pathname === '/api/quads') {
    let body = '';
    let totalSize = 0;

    req.on('data', chunk => {
      totalSize += chunk.length;
      if (totalSize > MAX_BODY_SIZE) {
        res.writeHead(413, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Payload Too Large' }));
        req.destroy();
        return;
      }
      body += chunk.toString();
    });

    req.on('end', async () => {
      let parsed;
      try {
        parsed = JSON.parse(body);
      } catch (error) {
        res.writeHead(400, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Invalid JSON' }));
        return;
      }

      try {
        const { subject, predicate, object, objectType, validateHooks, quad } = parsed;

        // Support both formats: {quad: {...}} and {subject, predicate, object}
        const s = quad?.subject || subject;
        const p = quad?.predicate || predicate;
        const o = quad?.object || object;
        const oType = quad?.objectType || objectType || 'literal';

        if (!s || !p || !o) {
          res.writeHead(400, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: 'Missing required fields: subject, predicate, object' }));
          return;
        }

        const hooksExecuted = [];

        // Execute before-add hooks
        const beforeHooks = hookManager.getHooksByTrigger('before-add');
        if (beforeHooks.length > 0) {
          const beforeResult = await hookManager.executeByTrigger(
            'before-add',
            { subject: { value: s }, predicate: { value: p }, object: { value: o } },
            { store }
          );

          hooksExecuted.push('pre-add');

          if (
            !beforeResult.success ||
            beforeResult.results.some(r => r.success && r.data?.valid === false)
          ) {
            const failedResult = beforeResult.results.find(r => r.data?.valid === false);
            res.writeHead(400, { 'Content-Type': 'application/json' });
            res.end(JSON.stringify({ error: failedResult?.data?.message || 'Validation failed' }));
            return;
          }
        }

        // Add quad
        const parser = new Parser();
        // Escape quotes in literal values
        const escapedO = oType === 'literal' ? o.replace(/"/g, '\\"') : o;
        const quadStr =
          oType === 'literal' ? `<${s}> <${p}> "${escapedO}" .` : `<${s}> <${p}> <${o}> .`;

        let n3quad;
        try {
          [n3quad] = parser.parse(quadStr);
        } catch (parseError) {
          res.writeHead(400, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: `Invalid quad format: ${parseError.message}` }));
          return;
        }

        store.addQuad(n3quad);

        // Execute after-add hooks
        const afterHooks = hookManager.getHooksByTrigger('after-add');
        if (afterHooks.length > 0) {
          await hookManager.executeByTrigger('after-add', { quad: n3quad }, { store });
          hooksExecuted.push('post-add');
        }

        // Trigger change notification
        broadcastChange('add', n3quad);

        res.writeHead(201, { 'Content-Type': 'application/json' });
        res.end(
          JSON.stringify({
            success: true,
            hooksValidated: validateHooks === true,
            hooksExecuted,
          })
        );
      } catch (error) {
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: error.message }));
      }
    });
    return;
  }

  // DELETE /api/quads - Delete a quad
  if (req.method === 'DELETE' && url.pathname === '/api/quads') {
    let body = '';
    req.on('data', chunk => {
      body += chunk.toString();
    });

    req.on('end', async () => {
      try {
        const { subject, predicate, object } = JSON.parse(body);

        const parser = new Parser();
        const [quad] = parser.parse(`<${subject}> <${predicate}> "${object}" .`);
        store.removeQuad(quad);

        broadcastChange('delete', quad);

        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ success: true }));
      } catch (error) {
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: error.message }));
      }
    });
    return;
  }

  // POST /api/quads/clear - Clear store
  if (req.method === 'POST' && url.pathname === '/api/quads/clear') {
    store.removeQuads(store.getQuads());
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ success: true }));
    return;
  }

  // POST /api/import - Import RDF data
  if (req.method === 'POST' && url.pathname === '/api/import') {
    let body = '';
    req.on('data', chunk => {
      body += chunk.toString();
    });

    req.on('end', async () => {
      try {
        const contentType = req.headers['content-type'];

        if (
          !contentType ||
          (!contentType.includes('text/turtle') && !contentType.includes('application/n-triples'))
        ) {
          res.writeHead(415, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: 'Unsupported Media Type' }));
          return;
        }

        const parser = new Parser();
        const quads = parser.parse(body);
        quads.forEach(quad => store.addQuad(quad));

        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ success: true, imported: quads.length }));
      } catch (error) {
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: error.message }));
      }
    });
    return;
  }

  // GET /api/export - Export RDF data
  if (req.method === 'GET' && url.pathname === '/api/export') {
    const format = url.searchParams.get('format') || 'turtle';
    const { Writer } = await import('n3');
    const writer = new Writer({ format });
    const quads = store.getQuads();

    writer.addQuads(quads);
    writer.end((error, result) => {
      if (error) {
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: error.message }));
        return;
      }

      res.writeHead(200, { 'Content-Type': 'text/turtle' });
      res.end(result);
    });
    return;
  }

  // GET /api/stats - Statistics
  if (req.method === 'GET' && url.pathname === '/api/stats') {
    const quads = store.getQuads();
    const subjects = new Set(quads.map(q => q.subject.value));
    const predicatesSet = new Set(quads.map(q => q.predicate.value));

    const predicateCounts = {};
    quads.forEach(q => {
      const pred = q.predicate.value;
      predicateCounts[pred] = (predicateCounts[pred] || 0) + 1;
    });

    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(
      JSON.stringify({
        quadCount: quads.length,
        subjectCount: subjects.size,
        predicates: predicateCounts,
      })
    );
    return;
  }

  // 404
  res.writeHead(404, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({ error: 'Not found' }));
});

// ============================================================================
// START SERVER
// ============================================================================

/**
 * Create and start the server
 * @param {Object} [options] - Server options
 * @param {number} [options.port] - HTTP port
 * @returns {Promise<Object>} Server instance
 */
export async function createServer(options = {}) {
  const port = options.port ?? PORT;

  return new Promise(resolve => {
    server.listen(port, () => {
      // Attach store and other properties to server for testing
      server.store = store;
      server.hookManager = hookManager;
      server.streamingProcessor = streamingProcessor;
      server.wss = wss;

      console.log(`\n🚀 UNRDF Full-Stack Example Server`);
      console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
      console.log(`📡 HTTP API: http://localhost:${port}`);
      console.log(`🌊 WebSocket: ws://localhost:${WS_PORT}`);
      console.log(`\n📚 Available Endpoints:`);
      console.log(`  GET  /api/quads   - Get all triples`);
      console.log(`  GET  /api/query   - Execute SPARQL query`);
      console.log(`  POST /api/quads   - Add new triple`);
      console.log(`  GET  /api/stats   - Get statistics`);
      console.log(`\n✨ Press Ctrl+C to stop\n`);

      resolve(server);
    });
  });
}

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('\n🛑 Shutting down gracefully...');
  server.close();
  wss.close();
  process.exit(0);
});

// Auto-start if not imported
if (import.meta.url === `file://${process.argv[1]}`) {
  await createServer();
}
