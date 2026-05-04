/**
 * MCP Hyper-Advanced Proof of Concept
 *
 * Demonstrates:
 * 1. Server building with DSL
 * 2. Multi-server federation
 * 3. Protocol bridging (HTTP/WebSocket/STDIO)
 * 4. Tool routing and discovery
 * 5. Capability composition
 */

import {
  createServerBuilder,
  tool,
  resource,
  prompt,
} from '../src/mcp-server-builder.mjs';
import { createFederation } from '../src/mcp-federation.mjs';
import {
  createBridge,
  createHTTPBridge,
  createWebSocketBridge,
  createSTDIOBridge,
} from '../src/mcp-bridge.mjs';

console.log('=== MCP Hyper-Advanced POC ===\n');

// ============================================================================
// PHASE 1: Server Building with DSL
// ============================================================================

console.log('Phase 1: Building MCP Servers with DSL\n');

// Create KGC Tools Server
const kgcBuilder = createServerBuilder();

kgcBuilder.addTool(
  tool('query_graph', 'Query RDF graph with SPARQL')
    .param('query', 'SPARQL query string', 'string', true)
    .param('graph', 'Graph URI', 'string', false)
    .handler(async (params) => {
      console.log(`  [KGC] Executing query: ${params.query.substring(0, 50)}...`);
      return { bindings: [], count: 0 };
    }),
);

kgcBuilder.addTool(
  tool('insert_triple', 'Insert RDF triple')
    .param('subject', 'Subject URI', 'string', true)
    .param('predicate', 'Predicate URI', 'string', true)
    .param('object', 'Object value', 'string', true)
    .handler(async (params) => {
      console.log(`  [KGC] Inserting: ${params.subject} ${params.predicate} ${params.object}`);
      return { success: true };
    }),
);

kgcBuilder.addResource(
  resource('rdf://graphs/default', 'default-graph', 'Default RDF graph')
    .withMimeType('application/n-triples')
    .handler(async () => {
      return '<http://example.org/s> <http://example.org/p> "object" .';
    }),
);

kgcBuilder.addPrompt(
  prompt(
    'sparql_assistant',
    'SPARQL query assistant',
    'Help me write a SPARQL query for: {{task}}',
  )
    .arg('task', 'Query task description', 'string', true)
    .build(),
);

const kgcServer = await kgcBuilder.build('kgc-server', 'KGC RDF Tools', {
  transport: 'http',
  version: '1.0.0',
});

console.log(`✓ Built KGC Server: ${kgcServer.name}`);
console.log(`  - Tools: ${kgcServer.tools.length}`);
console.log(`  - Resources: ${kgcServer.resources.length}`);
console.log(`  - Prompts: ${kgcServer.prompts.length}`);

// Create Analysis Server
const analysisBuilder = createServerBuilder();

analysisBuilder.addTool(
  tool('analyze_code', 'Static code analysis')
    .param('file_path', 'File to analyze', 'string', true)
    .param('rules', 'Linting rules', 'array', false)
    .handler(async (params) => {
      console.log(`  [Analysis] Analyzing: ${params.file_path}`);
      return { issues: [], score: 95 };
    }),
);

analysisBuilder.addTool(
  tool('detect_patterns', 'Pattern detection')
    .param('pattern', 'Pattern to detect', 'string', true)
    .handler(async (params) => {
      console.log(`  [Analysis] Detecting pattern: ${params.pattern}`);
      return { matches: [], count: 0 };
    }),
);

const analysisServer = await analysisBuilder.build('analysis-server', 'Code Analysis Tools', {
  transport: 'websocket',
});

console.log(`✓ Built Analysis Server: ${analysisServer.name}`);
console.log(`  - Tools: ${analysisServer.tools.length}\n`);

// Demonstrate composition (S₁ ⊕ S₂)
const composedBuilder = kgcBuilder.compose(analysisBuilder);
const composedServer = await composedBuilder.build('unified-server', 'Unified Tools', {
  transport: 'stdio',
});

console.log(`✓ Composed Server: ${composedServer.name}`);
console.log(`  - Tools: ${composedServer.tools.length} (combined)\n`);

// ============================================================================
// PHASE 2: Federation and Routing
// ============================================================================

console.log('Phase 2: Multi-Server Federation\n');

const federation = createFederation();

// Register servers
const kgcServerId = federation.registerServer({
  id: 'kgc-01',
  name: 'kgc-server',
  endpoint: 'http://localhost:8001',
  transport: 'http',
  capabilities: {
    tools: ['query_graph', 'insert_triple'],
    resources: ['rdf://graphs/default'],
    prompts: ['sparql_assistant'],
  },
  priority: 10,
});

const analysisServerId = federation.registerServer({
  id: 'analysis-01',
  name: 'analysis-server',
  endpoint: 'ws://localhost:8002',
  transport: 'websocket',
  capabilities: {
    tools: ['analyze_code', 'detect_patterns'],
    resources: [],
    prompts: [],
  },
  priority: 5,
});

console.log(`✓ Registered ${federation.listServers().length} servers`);

// Add routing rules
federation.addRoutingRule({
  pattern: '^query_.*',
  server_id: kgcServerId,
  priority: 100,
});

federation.addRoutingRule({
  pattern: '^analyze_.*',
  server_id: analysisServerId,
  priority: 100,
});

console.log('✓ Configured routing rules\n');

// Discover capabilities
const capabilities = federation.discoverCapabilities();
console.log('Discovered Capabilities:');
console.log(`  - Tools: ${capabilities.tools.join(', ')}`);
console.log(`  - Servers: ${capabilities.servers.length}\n`);

// Invoke tools through federation
console.log('Invoking tools through federation:\n');

const result1 = await federation.invokeTool('query_graph', {
  query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
});
console.log(`✓ Routed to: ${result1.server_id} (${result1.success ? 'success' : 'failed'})`);

const result2 = await federation.invokeTool('analyze_code', {
  file_path: '/src/example.mjs',
});
console.log(`✓ Routed to: ${result2.server_id} (${result2.success ? 'success' : 'failed'})\n`);

// Generate federation receipt
const federationReceipt = await federation.generateReceipt();
console.log('Federation Receipt:');
console.log(`  - Servers: ${federationReceipt.servers_registered}`);
console.log(`  - Tools: ${federationReceipt.tools_available}`);
console.log(`  - Invocations: ${federationReceipt.invocations_total}`);
console.log(`  - Success: ${federationReceipt.invocations_success}`);
console.log(`  - Hash: ${federationReceipt.hash.substring(0, 16)}...\n`);

// ============================================================================
// PHASE 3: Protocol Bridging
// ============================================================================

console.log('Phase 3: Protocol Bridging\n');

// Create bridges for different transports
const httpBridge = createHTTPBridge('http://localhost:8000');
const wsBridge = createWebSocketBridge('ws://localhost:8001');
const stdioBridge = createSTDIOBridge();

console.log('✓ Created 3 transport bridges (HTTP, WebSocket, STDIO)\n');

// Set up message handlers
httpBridge.on('request', async (message) => {
  console.log(`  [HTTP Bridge] Received request: ${message.method}`);
});

wsBridge.on('notification', async (message) => {
  console.log(`  [WS Bridge] Received notification: ${message.method}`);
});

// Send messages through bridges
console.log('Sending messages through bridges:\n');

const request1 = httpBridge.createRequest('tools/list', {});
await httpBridge.send(request1);
console.log(`✓ HTTP Request sent: ${request1.id.substring(0, 8)}...`);

const notification1 = wsBridge.createNotification('status/update', { status: 'active' });
await wsBridge.send(notification1);
console.log(`✓ WebSocket Notification sent: ${notification1.id.substring(0, 8)}...\n`);

// Get bridge statistics
const httpStats = await httpBridge.getStats();
console.log('HTTP Bridge Stats:');
console.log(`  - Messages out: ${httpStats.messages_out}`);
console.log(`  - Bytes out: ${httpStats.bytes_out}`);
console.log(`  - Uptime: ${Math.round(httpStats.uptime_ms)}ms\n`);

const wsStats = await wsBridge.getStats();
console.log('WebSocket Bridge Stats:');
console.log(`  - Messages out: ${wsStats.messages_out}`);
console.log(`  - Bytes out: ${wsStats.bytes_out}`);
console.log(`  - Uptime: ${Math.round(wsStats.uptime_ms)}ms\n`);

// ============================================================================
// PHASE 4: End-to-End Integration
// ============================================================================

console.log('Phase 4: End-to-End Integration\n');

// Create request, route through federation, send via bridge
const toolRequest = httpBridge.createRequest('invoke_tool', {
  tool: 'query_graph',
  params: { query: 'SELECT * WHERE { ?s a ?type }' },
});

await httpBridge.send(toolRequest);
console.log(`✓ Created and sent tool invocation request`);

const routedServer = federation.route('query_graph');
console.log(`✓ Routed to server: ${routedServer}`);

const invocationResult = await federation.invokeTool('query_graph', {
  query: 'SELECT * WHERE { ?s a ?type }',
});

const response = httpBridge.createResponse(toolRequest.id, invocationResult.result);
await httpBridge.send(response);
console.log(`✓ Response sent back through bridge\n`);

// ============================================================================
// SUMMARY
// ============================================================================

console.log('=== Summary ===\n');

const kgcManifest = await kgcBuilder.generateManifest();
console.log('Capability Manifest:');
console.log(`  - Server: ${kgcManifest.server_name} v${kgcManifest.server_version}`);
console.log(`  - Tools: ${kgcManifest.tools_count}`);
console.log(`  - Resources: ${kgcManifest.resources_count}`);
console.log(`  - Prompts: ${kgcManifest.prompts_count}`);
console.log(`  - Transport: ${kgcManifest.transport}`);
console.log(`  - Hash: ${kgcManifest.hash.substring(0, 32)}...\n`);

console.log('Hyper-Advanced Capabilities Demonstrated:');
console.log('  ✓ Server building with fluent DSL');
console.log('  ✓ Tool/Resource/Prompt composition');
console.log('  ✓ Server composition (S₁ ⊕ S₂)');
console.log('  ✓ Multi-server federation');
console.log('  ✓ Dynamic tool routing');
console.log('  ✓ Capability discovery');
console.log('  ✓ Protocol bridging (HTTP/WS/STDIO)');
console.log('  ✓ Message transformation');
console.log('  ✓ Receipt-based verification');
console.log('  ✓ Statistical monitoring\n');

console.log('=== POC Complete ===');
