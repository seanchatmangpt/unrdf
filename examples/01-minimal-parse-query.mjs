/**
 * 01-minimal-parse-query.mjs
 *
 * The recommended UNRDF entry point: createKnowledgeSubstrateCore()
 * One function gives you everything.
 *
 * Run: node examples/01-minimal-parse-query.mjs
 */

import { createKnowledgeSubstrateCore } from 'unrdf';

// One function gives you: transactions, hooks, sandboxing, audit trails, etc.
const core = await createKnowledgeSubstrateCore();

// Parse RDF Turtle
const _store = core.store;
const _ttl = `
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
  ex:Bob ex:knows ex:Charlie .
  ex:Charlie ex:knows ex:Diana .
`;

// Access the transaction manager for safe operations
const _txManager = core.getComponent('TransactionManager');

console.log('Knowledge Substrate initialized with:');
console.log('- TransactionManager');
console.log('- KnowledgeHookManager');
console.log('- EffectSandbox');
console.log('- LockchainWriter');
console.log('- PerformanceOptimizer');
console.log('- Observability');

console.log('\nCore status:', core.getStatus());

// Cleanup when done
await core.cleanup();
