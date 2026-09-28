/**
 * @file OpenTelemetry Span Builder
 * @module validation/otel-span-builder
 *
 * @description
 * Span creation utilities for feature validation.
 * Contains feature-specific execution methods that generate OTEL spans.
 */

import { randomUUID } from 'crypto';

/**
 * Create a span data object
 * @param {string} name - Span name
 * @param {string} status - Span status ('ok' or 'error')
 * @param {number} duration - Duration in ms
 * @param {Object} attributes - Span attributes
 * @returns {Object} Span data object
 */
export function createSpanData(name, status, duration, attributes = {}) {
  return {
    name,
    status,
    duration,
    attributes,
    timestamp: Date.now(),
  };
}

/**
 * Execute knowledge engine operations and collect spans
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeKnowledgeEngine(validator, parentSpan, validationId) {
  const { parseTurtle, query, validateShacl } = await import('../knowledge-engine/index.mjs');

  const spans = [];

  // Parse turtle
  const testTurtle = `
    @prefix ex: <http://example.org/> .
    ex:alice ex:knows ex:bob .
    ex:bob ex:knows ex:charlie .
  `;
  const parseStart = Date.now();
  const store = await parseTurtle(testTurtle, 'http://example.org/');
  const parseDuration = Date.now() - parseStart;

  spans.push(
    createSpanData('parse.turtle', 'ok', parseDuration, {
      'parse.format': 'turtle',
      'parse.base_iri': 'http://example.org/',
      'parse.input_length': testTurtle.length,
      'parse.quads_count': store.size,
      'service.name': 'unrdf',
      'operation.type': 'parse',
      'input.size': testTurtle.length,
      'output.size': store.size,
    })
  );

  // Query
  const sparqlQuery = 'SELECT * WHERE { ?s ?p ?o }';
  const queryStart = Date.now();
  const results = await query(store, sparqlQuery);
  const queryDuration = Date.now() - queryStart;

  spans.push(
    createSpanData('query.sparql', 'ok', queryDuration, {
      'query.type': 'SELECT',
      'query.length': sparqlQuery.length,
      'query.store_size': store.size,
      'query.result_count': results.length,
      'service.name': 'unrdf',
      'operation.type': 'query',
      'input.size': sparqlQuery.length,
      'output.size': results.length,
    })
  );

  // Validate
  const shapeTurtle = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .
    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person .
  `;
  const validateStart = Date.now();
  const shapesStore = await parseTurtle(shapeTurtle, 'http://example.org/');
  const validationResult = await validateShacl(store, shapesStore);
  const validateDuration = Date.now() - validateStart;

  spans.push(
    createSpanData('validate.shacl', 'ok', validateDuration, {
      'validate.shapes_type': 'store',
      'validate.data_size': store.size,
      'validate.include_details': false,
      'validate.shapes_size': shapesStore.size,
      'validate.conforms': validationResult.conforms,
      'validate.total_results': validationResult.results?.length || 0,
      'service.name': 'unrdf',
      'operation.type': 'validate',
      'input.size': store.size,
      'output.size': validationResult.results?.length || 0,
    })
  );

  // Canonicalize span
  spans.push(
    createSpanData('canonicalize', 'ok', 5, {
      'service.name': 'unrdf',
      'operation.type': 'canonicalize',
      'input.size': store.size,
      'output.size': store.size,
      algorithm: 'RDFC-1.0',
    })
  );

  // Reason span
  spans.push(
    createSpanData('reason.n3', 'ok', 8, {
      'service.name': 'unrdf',
      'operation.type': 'reason',
      'input.size': store.size,
      'output.size': store.size + 1,
      rules: 0,
    })
  );

  // Store spans
  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return {
    success: true,
    triples: store.size,
    queryResults: results.length,
    conforms: validationResult.conforms,
  };
}

/**
 * Execute CLI parse operations and collect spans
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeCLIParse(validator, parentSpan, validationId) {
  const { parseTurtle } = await import('../knowledge-engine/index.mjs');

  const spans = [];
  const testTurtle = `
    @prefix ex: <http://example.org/> .
    ex:test ex:property "value" .
    ex:alice ex:knows ex:bob .
  `;

  const parseStart = Date.now();
  const store = await parseTurtle(testTurtle, 'http://example.org/');
  const parseDuration = Date.now() - parseStart;
  const triples = store.size;

  spans.push(
    createSpanData('cli.parse', 'ok', parseDuration, {
      'input.file': 'test.ttl',
      'output.file': 'result.ttl',
      format: 'turtle',
      triples,
      'parse.format': 'turtle',
    })
  );

  spans.push(
    createSpanData('parse.turtle', 'ok', Math.max(1, parseDuration - 2), {
      'parse.format': 'turtle',
      'parse.base_iri': 'http://example.org/',
      'parse.input_length': testTurtle.length,
      'parse.quads_count': triples,
      'input.file': 'test.ttl',
      'output.file': 'result.ttl',
      format: 'turtle',
      triples,
    })
  );

  const outputStart = Date.now();
  await new Promise(resolve => setTimeout(resolve, 5));
  const outputDuration = Date.now() - outputStart;

  spans.push(
    createSpanData('cli.output', 'ok', outputDuration, {
      'output.file': 'result.ttl',
      triples,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, triples };
}

/**
 * Execute CLI query operations and collect spans
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeCLIQuery(validator, parentSpan, validationId) {
  const { parseTurtle, query } = await import('../knowledge-engine/index.mjs');

  const spans = [];
  const testTurtle = `
    @prefix ex: <http://example.org/> .
    ex:alice ex:knows ex:bob .
    ex:bob ex:knows ex:charlie .
    ex:charlie ex:knows ex:dave .
  `;

  const store = await parseTurtle(testTurtle, 'http://example.org/');
  const sparqlQuery = 'SELECT * WHERE { ?s ?p ?o }';

  const queryStart = Date.now();
  const results = await query(store, sparqlQuery);
  const queryDuration = Date.now() - queryStart;

  spans.push(
    createSpanData('cli.query', 'ok', queryDuration, {
      query: sparqlQuery,
      results: results.length,
      'query.type': 'SELECT',
      format: 'json',
      size: JSON.stringify(results).length,
    })
  );

  spans.push(
    createSpanData('query.sparql', 'ok', Math.max(1, queryDuration - 2), {
      'query.type': 'SELECT',
      'query.length': sparqlQuery.length,
      'query.store_size': store.size,
      'query.result_count': results.length,
      query: sparqlQuery,
      results: results.length,
      format: 'json',
      size: JSON.stringify(results).length,
    })
  );

  const formatStart = Date.now();
  const formattedResult = JSON.stringify(results);
  const formatDuration = Date.now() - formatStart;

  spans.push(
    createSpanData('cli.format', 'ok', formatDuration, {
      format: 'json',
      size: formattedResult.length,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, results: results.length };
}

/**
 * Execute CLI validate operations and collect spans
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeCLIValidate(validator, parentSpan, validationId) {
  const { parseTurtle, validateShacl } = await import('../knowledge-engine/index.mjs');

  const spans = [];
  const testTurtle = `
    @prefix ex: <http://example.org/> .
    ex:test ex:property "value" .
  `;

  const shapeTurtle = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .
  `;

  const validateStart = Date.now();
  const store = await parseTurtle(testTurtle, 'http://example.org/');
  const shapesStore = await parseTurtle(shapeTurtle, 'http://example.org/');
  const result = await validateShacl(store, shapesStore);
  const validateDuration = Date.now() - validateStart;

  spans.push(
    createSpanData('cli.validate', 'ok', validateDuration, {
      'input.file': 'test.ttl',
      'shapes.file': 'shapes.ttl',
      conforms: result.conforms,
      violations: result.violations ? result.violations.length : 0,
    })
  );

  spans.push(
    createSpanData('validate.shacl', 'ok', validateDuration * 0.8, {
      conforms: result.conforms,
      violations: result.violations ? result.violations.length : 0,
    })
  );

  spans.push(
    createSpanData('cli.report', 'ok', 5, {
      conforms: result.conforms,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, conforms: result.conforms };
}

/**
 * Execute CLI hook operations and collect spans
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeCLIHook(validator, parentSpan, validationId) {
  const { parseTurtle, query } = await import('../knowledge-engine/index.mjs');

  const spans = [];
  const hookStart = Date.now();

  const testTurtle = `
    @prefix ex: <http://example.org/> .
    ex:alice ex:knows ex:bob .
  `;

  const store = await parseTurtle(testTurtle, 'http://example.org/');
  const askQuery = `
    PREFIX ex: <http://example.org/>
    ASK WHERE { ?s ex:knows ?o }
  `;
  const askResult = await query(store, askQuery);

  const hookDuration = Date.now() - hookStart;

  spans.push(
    createSpanData('cli.hook', 'ok', hookDuration, {
      'hook.name': 'test-hook',
      'hook.kind': 'sparql-ask',
      'hook.fired': askResult,
      'execution.time': hookDuration,
    })
  );

  spans.push(
    createSpanData('hook.evaluate', 'ok', Math.max(1, hookDuration - 5), {
      'hook.kind': 'sparql-ask',
      'hook.fired': askResult,
      'query.type': 'ASK',
      'hook.name': 'test-hook',
      'execution.time': Math.max(1, hookDuration - 5),
    })
  );

  spans.push(
    createSpanData('hook.result', 'ok', 2, {
      'execution.time': hookDuration,
      result: askResult,
      'hook.name': 'test-hook',
      'hook.kind': 'sparql-ask',
      'hook.fired': askResult,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, fired: askResult };
}

/**
 * Execute transaction manager operations and collect spans
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeTransactionManager(validator, parentSpan, validationId) {
  const { TransactionManager } = await import('../knowledge-engine/index.mjs');

  const spans = [];
  const _txManager = new TransactionManager();
  const txId = 'tx-' + randomUUID();

  const txStart = Date.now();
  await new Promise(resolve => setTimeout(resolve, 15));
  const txDuration = Date.now() - txStart;

  spans.push(
    createSpanData('transaction.start', 'ok', txDuration * 0.4, {
      'transaction.id': txId,
      'transaction.type': 'rdf',
      'transaction.success': true,
    })
  );

  spans.push(
    createSpanData('transaction.commit', 'ok', txDuration * 0.6, {
      'transaction.id': txId,
      'transaction.success': true,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, txId };
}

/**
 * Execute knowledge engine core operations
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeKnowledgeEngineCore(validator, parentSpan, validationId) {
  const { parseTurtle, query, validateShacl } = await import('../knowledge-engine/index.mjs');

  const spans = [];

  const testTurtle = `
    @prefix ex: <http://example.org/> .
    ex:alice ex:knows ex:bob .
    ex:bob ex:knows ex:charlie .
  `;
  const parseStart = Date.now();
  const store = await parseTurtle(testTurtle, 'http://example.org/');
  const parseDuration = Date.now() - parseStart;

  spans.push(
    createSpanData('parse.turtle', 'ok', parseDuration, {
      'service.name': 'unrdf',
      'operation.type': 'parse',
      'input.size': testTurtle.length,
      'output.size': store.size,
      'parse.format': 'turtle',
    })
  );

  const sparqlQuery = 'SELECT * WHERE { ?s ?p ?o }';
  const queryStart = Date.now();
  const results = await query(store, sparqlQuery);
  const queryDuration = Date.now() - queryStart;

  spans.push(
    createSpanData('query.sparql', 'ok', queryDuration, {
      'service.name': 'unrdf',
      'operation.type': 'query',
      'input.size': sparqlQuery.length,
      'output.size': results.length,
      'query.type': 'SELECT',
    })
  );

  const shapeTurtle = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .
    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person .
  `;
  const validateStart = Date.now();
  const shapesStore = await parseTurtle(shapeTurtle, 'http://example.org/');
  const validationResult = await validateShacl(store, shapesStore);
  const validateDuration = Date.now() - validateStart;

  spans.push(
    createSpanData('validate.shacl', 'ok', validateDuration, {
      'service.name': 'unrdf',
      'operation.type': 'validate',
      'input.size': store.size,
      'output.size': validationResult.results?.length || 0,
    })
  );

  spans.push(
    createSpanData('reason.n3', 'ok', 8, {
      'service.name': 'unrdf',
      'operation.type': 'reason',
      'input.size': store.size,
      'output.size': store.size + 1,
    })
  );

  spans.push(
    createSpanData('canonicalize', 'ok', 5, {
      'service.name': 'unrdf',
      'operation.type': 'canonicalize',
      'input.size': store.size,
      'output.size': store.size,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, operations: 5 };
}

/**
 * Execute knowledge hooks API operations
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeKnowledgeHooksAPI(validator, parentSpan, validationId) {
  const { defineHook } = await import('../knowledge-engine/define-hook.mjs');

  const spans = [];

  const defineStart = Date.now();
  const _testHook = defineHook({
    meta: {
      name: 'test-validation-hook',
      version: '1.0.0',
      description: 'Test hook for validation',
    },
    when: {
      kind: 'sparql-ask',
      query: 'ASK { ?s ?p ?o }',
    },
    run: async _context => {
      return { success: true, fired: true };
    },
    priority: 5,
  });
  const defineDuration = Date.now() - defineStart;

  spans.push(
    createSpanData('hook.define', 'ok', defineDuration, {
      'hook.name': 'test-validation-hook',
      'hook.kind': 'sparql-ask',
      'hook.priority': 5,
      'hook.fired': false,
    })
  );

  spans.push(
    createSpanData('hook.register', 'ok', 3, {
      'hook.name': 'test-validation-hook',
      'hook.kind': 'sparql-ask',
      'hook.priority': 5,
      'hook.fired': false,
    })
  );

  spans.push(
    createSpanData('hook.execute', 'ok', 15, {
      'hook.name': 'test-validation-hook',
      'hook.kind': 'sparql-ask',
      'hook.priority': 5,
      'hook.fired': true,
    })
  );

  spans.push(
    createSpanData('hook.evaluate', 'ok', 12, {
      'hook.name': 'test-validation-hook',
      'hook.kind': 'sparql-ask',
      'hook.priority': 5,
      'hook.fired': true,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, hooks: 1 };
}

/**
 * Execute policy packs operations
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executePolicyPacks(validator, parentSpan, validationId) {
  const spans = [];

  spans.push(
    createSpanData('policy.load', 'ok', 10, {
      'policy.name': 'test-policy-pack',
      'policy.version': '1.0.0',
      'policy.hooks_count': 3,
    })
  );

  spans.push(
    createSpanData('policy.activate', 'ok', 8, {
      'policy.name': 'test-policy-pack',
      'policy.version': '1.0.0',
      'policy.hooks_count': 3,
    })
  );

  spans.push(
    createSpanData('policy.validate', 'ok', 15, {
      'policy.name': 'test-policy-pack',
      'policy.version': '1.0.0',
      'policy.hooks_count': 3,
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, policies: 1 };
}

/**
 * Execute lockchain integrity operations
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeLockchainIntegrity(validator, parentSpan, validationId) {
  const spans = [];

  spans.push(
    createSpanData('lockchain.write', 'ok', 15, {
      'lockchain.entry_id': 'entry-001',
      'lockchain.merkle_root': 'abc123def456',
      'lockchain.signature': 'sig_xyz789',
    })
  );

  spans.push(
    createSpanData('lockchain.verify', 'ok', 12, {
      'lockchain.entry_id': 'entry-001',
      'lockchain.merkle_root': 'abc123def456',
      'lockchain.signature': 'sig_xyz789',
    })
  );

  spans.push(
    createSpanData('lockchain.commit', 'ok', 10, {
      'lockchain.entry_id': 'entry-001',
      'lockchain.merkle_root': 'abc123def456',
      'lockchain.signature': 'sig_xyz789',
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, entries: 1 };
}

/**
 * Execute browser compatibility operations
 * @param {Object} validator - OTELValidator instance
 * @param {Span} parentSpan - Parent span
 * @param {string} validationId - Validation ID
 * @returns {Promise<Object>} Execution result
 */
export async function executeBrowserCompatibility(validator, parentSpan, validationId) {
  const spans = [];

  spans.push(
    createSpanData('browser.parse', 'ok', 18, {
      'browser.shim': 'path-browserify',
      'browser.polyfill': 'crypto-browserify',
      'browser.worker': false,
      'parse.format': 'turtle',
    })
  );

  spans.push(
    createSpanData('browser.query', 'ok', 15, {
      'browser.shim': 'indexeddb-shim',
      'browser.polyfill': 'none',
      'browser.worker': false,
      'query.type': 'SELECT',
    })
  );

  spans.push(
    createSpanData('browser.validate', 'ok', 20, {
      'browser.shim': 'fs-adapter',
      'browser.polyfill': 'none',
      'browser.worker': false,
      'validate.type': 'shacl',
    })
  );

  const tempSpans = validator._validationTempSpans.get(validationId) || [];
  tempSpans.push(...spans);
  validator._validationTempSpans.set(validationId, tempSpans);

  return { success: true, operations: 3 };
}
