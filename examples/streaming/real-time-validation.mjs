/**
 * @file Real-time Validation Example
 * @description
 * Demonstrates how to use real-time SHACL validation with streaming updates
 * and integrate it with the transaction system.
 */

import { createStore } from '@unrdf/oxigraph';
import { TransactionManager } from '../../src/knowledge-engine/transaction.mjs';
import {
  createRealTimeValidator,
  ValidationMode,
} from '../../src/knowledge-engine/streaming/index.mjs';

const { namedNode, literal, quad } = DataFactory;

async function main() {
  console.log('Real-time SHACL Validation Example\n');

  // Define SHACL shapes
  const shapes = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 1
      ] ;
      sh:property [
        sh:path ex:age ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150
      ] ;
      sh:property [
        sh:path ex:email ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}$"
      ] .
  `;

  // Create validator with delta mode for performance
  const validator = createRealTimeValidator({
    mode: ValidationMode.DELTA,
    shapes,
    strict: false,
    enableCaching: true,
    cacheSize: 100,
    debounceMs: 50,
  });

  console.log('Validator initialized with mode:', ValidationMode.DELTA, '\n');

  // Listen for violations
  validator.on('violation', result => {
    console.error('VALIDATION VIOLATION:');
    console.error('  ID:', result.id);
    console.error('  Timestamp:', new Date(result.timestamp).toISOString());
    console.error('  Violations:', result.violations.length);

    for (const violation of result.violations) {
      console.error(`    - ${violation.message || 'Unknown violation'}`);
      if (violation.focusNode) {
        console.error(`      Focus node: ${violation.focusNode}`);
      }
      if (violation.path) {
        console.error(`      Path: ${violation.path}`);
      }
    }

    console.error('');
  });

  // Listen for successful validations
  validator.on('validated', result => {
    if (result.conforms) {
      console.log('VALIDATION PASSED:', {
        id: result.id,
        duration: result.duration + 'ms',
        mode: result.mode,
      });
    }
  });

  // Create transaction manager with validation hook
  const txManager = new TransactionManager({
    strictMode: false,
  });

  // Add validation hook
  const validationHook = validator.createValidationHook({ strict: false });
  txManager.addHook(validationHook);

  console.log('Validation hook registered\n');

  // Create store
  let store = createStore();

  // Test 1: Valid person
  console.log('Test 1: Adding valid person...');
  const delta1 = {
    additions: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/name'),
        literal('Alice')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/age'),
        literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/email'),
        literal('alice@example.org')
      ),
    ],
    removals: [],
  };

  const result1 = await txManager.apply(store, delta1);
  console.log('Transaction committed:', result1.receipt.committed, '\n');
  store = result1.store;

  // Test 2: Invalid person (missing name)
  console.log('Test 2: Adding person with missing name (should fail)...');
  const delta2 = {
    additions: [
      quad(
        namedNode('http://example.org/bob'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ),
      quad(
        namedNode('http://example.org/bob'),
        namedNode('http://example.org/age'),
        literal('25', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
    removals: [],
  };

  const result2 = await txManager.apply(store, delta2);
  console.log('Transaction committed:', result2.receipt.committed, '\n');
  if (!result2.receipt.committed) {
    console.log('Transaction was vetoed by validation hook\n');
  }

  // Test 3: Invalid age (exceeds maximum)
  console.log('Test 3: Adding person with invalid age (should fail)...');
  const delta3 = {
    additions: [
      quad(
        namedNode('http://example.org/charlie'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ),
      quad(
        namedNode('http://example.org/charlie'),
        namedNode('http://example.org/name'),
        literal('Charlie')
      ),
      quad(
        namedNode('http://example.org/charlie'),
        namedNode('http://example.org/age'),
        literal('200', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
    removals: [],
  };

  const result3 = await txManager.apply(store, delta3);
  console.log('Transaction committed:', result3.receipt.committed, '\n');

  // Test 4: Invalid email format
  console.log('Test 4: Adding person with invalid email (should fail)...');
  const delta4 = {
    additions: [
      quad(
        namedNode('http://example.org/diana'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ),
      quad(
        namedNode('http://example.org/diana'),
        namedNode('http://example.org/name'),
        literal('Diana')
      ),
      quad(
        namedNode('http://example.org/diana'),
        namedNode('http://example.org/email'),
        literal('not-an-email')
      ),
    ],
    removals: [],
  };

  const result4 = await txManager.apply(store, delta4);
  console.log('Transaction committed:', result4.receipt.committed, '\n');

  // Test 5: Valid update
  console.log('Test 5: Updating valid person...');
  const delta5 = {
    additions: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/age'),
        literal('31', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
    removals: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://example.org/age'),
        literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
    ],
  };

  const result5 = await txManager.apply(store, delta5);
  console.log('Transaction committed:', result5.receipt.committed, '\n');

  // Get validator metrics
  console.log('Validator Metrics:');
  const metrics = validator.getMetrics();
  console.log(JSON.stringify(metrics, null, 2));

  // Performance test
  console.log('\nPerformance Test: Validating 100 deltas...');
  const start = Date.now();

  for (let i = 0; i < 100; i++) {
    const delta = {
      additions: [
        quad(
          namedNode(`http://example.org/person${i}`),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/Person')
        ),
        quad(
          namedNode(`http://example.org/person${i}`),
          namedNode('http://example.org/name'),
          literal(`Person ${i}`)
        ),
        quad(
          namedNode(`http://example.org/person${i}`),
          namedNode('http://example.org/age'),
          literal(`${20 + (i % 50)}`, namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        ),
      ],
      removals: [],
    };

    await validator.validateDelta(delta);
  }

  const duration = Date.now() - start;
  console.log(`Completed in ${duration}ms (avg: ${(duration / 100).toFixed(2)}ms per validation)`);

  const finalMetrics = validator.getMetrics();
  console.log(`Cache hit rate: ${(finalMetrics.cacheHitRate * 100).toFixed(1)}%`);
  console.log(`P95 latency: ${finalMetrics.p95Latency.toFixed(2)}ms`);

  // Cleanup
  console.log('\nCleaning up...');
  await validator.cleanup();
  console.log('Cleanup complete');
}

main().catch(console.error);
