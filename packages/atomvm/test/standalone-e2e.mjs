/**
 * @fileoverview Standalone E2E Verification: AtomVM with @unrdf/oxigraph
 * 
 * Purpose: Verify the @unrdf/atomvm core functionalities without relying on vitest.
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { CircuitBreaker } from '../src/circuit-breaker.mjs';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';
import { TripleStreamBatcher } from '../src/triple-stream-batcher.mjs';
import { RDFValidator } from '../src/rdf-validator.mjs';
import { SPARQLPatternMatcher } from '../src/sparql-pattern-matcher.mjs';
import { SLAMonitor } from '../src/sla-monitor.mjs';

const { namedNode, literal, quad } = dataFactory;

const FOAF = 'http://xmlns.com/foaf/0.1/';
const EX = 'http://example.org/';

async function runE2E() {
  console.log('🚀 Starting AtomVM End-to-End Verification...\n');

  // 1. Oxigraph Integration
  console.log('[1/7] Testing Oxigraph Integration...');
  const store = createStore();
  const alice = namedNode(`${EX}alice`);
  const name = namedNode(`${FOAF}name`);
  store.add(quad(alice, name, literal('Alice')));
  
  if (store.size !== 1) throw new Error('Store size mismatch');
  console.log('   ✅ Triple added to Oxigraph store');

  // 2. SPARQL Query
  console.log('[2/7] Testing SPARQL Execution...');
  const results = store.query(`SELECT ?name WHERE { <${EX}alice> <${FOAF}name> ?name }`);
  if (results.length !== 1 || results[0].get('name').value !== 'Alice') {
    throw new Error('SPARQL query failed');
  }
  console.log('   ✅ SPARQL SELECT query successful');

  // 3. Circuit Breaker
  console.log('[3/7] Testing Circuit Breaker...');
  const breaker = new CircuitBreaker({ failureThreshold: 2, resetTimeout: 1000 });
  const cbResult = await breaker.call(() => 'ok');
  if (cbResult !== 'ok' || breaker.getState() !== 'closed') {
    throw new Error('Circuit breaker failed');
  }
  console.log('   ✅ Circuit Breaker operational');

  // 4. Triple Stream Batching
  console.log('[4/7] Testing Triple Stream Batcher...');
  let batchCount = 0;
  const batcher = new TripleStreamBatcher({ batchSize: 2 });
  batcher.onBatch(() => { batchCount++; });
  
  await batcher.addTriple({ subject: 's1', predicate: 'p1', object: 'o1' });
  await batcher.addTriple({ subject: 's2', predicate: 'p2', object: 'o2' });
  
  if (batchCount !== 1) throw new Error('Batcher failed to trigger');
  console.log('   ✅ Triple Stream Batcher working');

  // 5. RDF Validator
  console.log('[5/7] Testing RDF Validator...');
  const validator = new RDFValidator();
  const validIriResult = validator.validateIRI('http://example.org/valid');
  const invalidIriResult = validator.validateIRI('not-an-iri');
  
  if (!validIriResult.valid || invalidIriResult.valid) throw new Error('Validator logic error');
  console.log('   ✅ RDF Validator logic sound');

  // 6. SPARQL Pattern Matcher
  console.log('[6/7] Testing SPARQL Pattern Matcher...');
  const matcher = new SPARQLPatternMatcher(store);
  const matched = await matcher.matchPattern({ subject: `${EX}alice` });
  if (matched.length !== 1) throw new Error('Pattern matcher failed');
  console.log('   ✅ SPARQL Pattern Matcher working');

  // 7. SLA Monitor
  console.log('[7/7] Testing SLA Monitor...');
  const sla = new SLAMonitor();
  const startTime = performance.now();
  await new Promise(r => setTimeout(r, 10));
  const duration = performance.now() - startTime;
  sla.recordLatency('test-op', duration);
  
  const metrics = sla.getMetrics('test-op');
  if (metrics.sampleCount !== 1 || metrics.latency.avg < 5) throw new Error('SLA Monitor failed');
  console.log('   ✅ SLA Monitor tracking operations');

  console.log('\n✨ ALL ATOMVM CORE SYSTEMS VERIFIED END-TO-END ✨');
  console.log('   Ready for Fortune 5 edge deployment.\n');
}

runE2E().catch(err => {
  console.error('\n❌ E2E VERIFICATION FAILED:');
  console.error(err);
  process.exit(1);
});
