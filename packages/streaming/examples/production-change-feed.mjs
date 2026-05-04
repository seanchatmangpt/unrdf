/**
 * @file Production Change Feed Example
 * @module examples/production-change-feed
 * 
 * @description
 * This example demonstrates a production-ready setup for @unrdf/streaming.
 * It integrates:
 * - Change Feed for real-time monitoring
 * - Real-time SHACL Validation to prevent invalid data
 * - Observability with OpenTelemetry
 * - Delta synchronization patterns
 */

import { createStore } from '@unrdf/oxigraph';
import { 
  createChangeFeed, 
  createRealTimeValidator, 
  createObservabilityManager,
  ValidationMode 
} from '../src/index.mjs';

// 1. Setup Observability
const obs = createObservabilityManager({
  serviceName: 'production-streaming-demo',
  enableMetrics: true,
  enableTracing: true
});

// 2. Setup SHACL Shapes for validation
const shapes = `
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:PersonShape
  a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:minCount 1 ;
    sh:datatype <http://www.w3.org/2001/XMLSchema#string> ;
  ] .
`;

// 3. Create Validator
const validator = createRealTimeValidator({
  shapes,
  mode: ValidationMode.DELTA,
  strict: false // Don't throw, just report
});

// 4. Create Store and Change Feed
const store = createStore();
const feed = createChangeFeed(store);

// 5. Wire up the components

// Listen for all changes
feed.subscribe((change) => {
  console.log(`[Feed] ${change.type.toUpperCase()}:`, 
    `${change.quad.subject.value} ${change.quad.predicate.value} ${change.quad.object.value}`);
  
  // Track metrics
  obs.recordOperation('change_received', { type: change.type });
});

// Perform validation on additions
feed.subscribe(async (change) => {
  if (change.type === 'add') {
    const delta = { additions: [change.quad], removals: [] };
    const result = await validator.validateDelta(delta, store);
    
    if (!result.conforms) {
      console.warn(`[Validator] SHACL Violation:`, result.violations.map(v => v.message).join(', '));
      obs.recordError(new Error('SHACL Violation'), { 
        subject: change.quad.subject.value,
        violations: result.violations.length
      });
    } else {
      console.log(`[Validator] Data conforms to shapes.`);
    }
  }
});

// 6. Simulate Production Activity

async function runDemo() {
  console.log('--- Starting Production Change Feed Demo ---');
  
  // Valid addition
  console.log('\nAdding valid person...');
  store.addQuad({
    subject: { value: 'http://example.org/alice', termType: 'NamedNode' },
    predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', termType: 'NamedNode' },
    object: { value: 'http://xmlns.com/foaf/0.1/Person', termType: 'NamedNode' },
  });
  
  store.addQuad({
    subject: { value: 'http://example.org/alice', termType: 'NamedNode' },
    predicate: { value: 'http://xmlns.com/foaf/0.1/name', termType: 'NamedNode' },
    object: { value: 'Alice', termType: 'Literal' },
  });

  // Wait a bit for async validation
  await new Promise(resolve => setTimeout(resolve, 100));

  // Invalid addition (missing name)
  console.log('\nAdding invalid person (missing name)...');
  store.addQuad({
    subject: { value: 'http://example.org/bob', termType: 'NamedNode' },
    predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', termType: 'NamedNode' },
    object: { value: 'http://xmlns.com/foaf/0.1/Person', termType: 'NamedNode' },
  });

  await new Promise(resolve => setTimeout(resolve, 100));

  // 7. Review Metrics
  console.log('\n--- Demo Metrics ---');
  console.log(JSON.stringify(validator.getMetrics(), null, 2));
  
  // Cleanup
  await validator.cleanup();
  feed.destroy();
  console.log('\nDemo complete.');
}

runDemo().catch(console.error);
