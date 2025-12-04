/**
 * @fileoverview UNRDF Full-Stack Example - Server
 * Demonstrates all server-side UNRDF packages working together
 */

import { createServer } from 'node:http';
import { WebSocketServer } from 'ws';
import { Store, Parser } from 'n3';
import { KnowledgeHookManager } from '@unrdf/hooks';
import { StreamingProcessor } from '@unrdf/streaming';
import { FederationCoordinator } from '@unrdf/federation';

// ============================================================================
// CONFIGURATION
// ============================================================================

const PORT = process.env.PORT || 3000;
const WS_PORT = process.env.WS_PORT || 3001;

// ============================================================================
// RDF STORE SETUP
// ============================================================================

/** @type {Store} */
const store = new Store();

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
quads.forEach((quad) => store.addQuad(quad));

console.log(`📊 Loaded ${quads.length} triples into RDF store`);

// ============================================================================
// KNOWLEDGE HOOKS SETUP
// ============================================================================

const hookManager = new KnowledgeHookManager();

// Define validation hook
hookManager.define({
  name: 'validateEmail',
  type: 'validation',
  trigger: 'before:add',
  handler: async (context) => {
    const { quad } = context;
    if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/mbox') {
      const email = quad.object.value;
      if (!email.startsWith('mailto:') || !email.includes('@')) {
        return { valid: false, message: 'Invalid email format' };
      }
    }
    return { valid: true };
  },
});

// Define enrichment hook
hookManager.define({
  name: 'addTimestamp',
  type: 'transformation',
  trigger: 'after:add',
  handler: async (context) => {
    const { quad, store } = context;
    const timestampQuad = {
      subject: quad.subject,
      predicate: { value: 'http://example.org/addedAt' },
      object: {
        value: new Date().toISOString(),
        termType: 'Literal',
        datatype: {
          value: 'http://www.w3.org/2001/XMLSchema#dateTime',
        },
      },
    };
    store.addQuad(timestampQuad);
    return context;
  },
});

console.log('🔗 Knowledge Hooks configured');

// ============================================================================
// STREAMING SETUP
// ============================================================================

const streamingProcessor = new StreamingProcessor({
  store,
  enableChangeTracking: true,
});

// WebSocket server for real-time updates
const wss = new WebSocketServer({ port: WS_PORT });

wss.on('connection', (ws) => {
  console.log('🔌 WebSocket client connected');

  // Send initial data
  const allQuads = store.getQuads();
  ws.send(
    JSON.stringify({
      type: 'initial',
      quads: allQuads.map((q) => ({
        subject: q.subject.value,
        predicate: q.predicate.value,
        object: q.object.value,
      })),
    })
  );

  // Subscribe to changes
  const unsubscribe = streamingProcessor.subscribe((change) => {
    ws.send(
      JSON.stringify({
        type: 'change',
        operation: change.operation,
        quad: {
          subject: change.quad.subject.value,
          predicate: change.quad.predicate.value,
          object: change.quad.object.value,
        },
      })
    );
  });

  ws.on('close', () => {
    console.log('🔌 WebSocket client disconnected');
    unsubscribe();
  });
});

console.log(`🌊 WebSocket server listening on port ${WS_PORT}`);

// ============================================================================
// FEDERATION SETUP
// ============================================================================

const federation = new FederationCoordinator({
  localStore: store,
  enablePeerDiscovery: true,
});

console.log('🌐 Federation coordinator initialized');

// ============================================================================
// HTTP API ENDPOINTS
// ============================================================================

const server = createServer(async (req, res) => {
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
        quads: quads.map((q) => ({
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
      // Simple pattern matching (replace with full SPARQL in production)
      const results = store.getQuads();
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(
        JSON.stringify({
          query,
          results: results.slice(0, 10).map((q) => ({
            subject: q.subject.value,
            predicate: q.predicate.value,
            object: q.object.value,
          })),
        })
      );
    } catch (error) {
      res.writeHead(500, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({ error: error.message }));
    }
    return;
  }

  // POST /api/quads - Add new triple
  if (req.method === 'POST' && url.pathname === '/api/quads') {
    let body = '';
    req.on('data', (chunk) => {
      body += chunk.toString();
    });

    req.on('end', async () => {
      try {
        const { subject, predicate, object } = JSON.parse(body);

        // Validate via hooks
        const validationResult = await hookManager.executeHook(
          'validateEmail',
          {
            quad: { subject, predicate, object },
            store,
          }
        );

        if (!validationResult.valid) {
          res.writeHead(400, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: validationResult.message }));
          return;
        }

        // Add quad
        const parser = new Parser();
        const [quad] = parser.parse(
          `<${subject}> <${predicate}> <${object}> .`
        );
        store.addQuad(quad);

        // Trigger change notification
        streamingProcessor.notifyChange({
          operation: 'add',
          quad,
        });

        res.writeHead(201, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ success: true }));
      } catch (error) {
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: error.message }));
      }
    });
    return;
  }

  // GET /api/stats - Statistics
  if (req.method === 'GET' && url.pathname === '/api/stats') {
    const quads = store.getQuads();
    const subjects = new Set(quads.map((q) => q.subject.value));
    const predicates = new Set(quads.map((q) => q.predicate.value));

    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(
      JSON.stringify({
        totalTriples: quads.length,
        uniqueSubjects: subjects.size,
        uniquePredicates: predicates.size,
        hooksRegistered: hookManager.getRegisteredHooks().length,
        wsConnections: wss.clients.size,
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

server.listen(PORT, () => {
  console.log(`\n🚀 UNRDF Full-Stack Example Server`);
  console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
  console.log(`📡 HTTP API: http://localhost:${PORT}`);
  console.log(`🌊 WebSocket: ws://localhost:${WS_PORT}`);
  console.log(`\n📚 Available Endpoints:`);
  console.log(`  GET  /api/quads   - Get all triples`);
  console.log(`  GET  /api/query   - Execute SPARQL query`);
  console.log(`  POST /api/quads   - Add new triple`);
  console.log(`  GET  /api/stats   - Get statistics`);
  console.log(`\n✨ Press Ctrl+C to stop\n`);
});

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('\n🛑 Shutting down gracefully...');
  server.close();
  wss.close();
  process.exit(0);
});
