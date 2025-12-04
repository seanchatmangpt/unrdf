/**
 * @file Stream Processing Pipeline Example
 * @description
 * Demonstrates how to build a complete streaming pipeline with windowing,
 * aggregation, and real-time validation.
 */

import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import {
  createStreamingPipeline,
  WindowType,
  Aggregators,
  ValidationMode,
} from '../../src/knowledge-engine/streaming/index.mjs';

const { namedNode, literal, quad } = DataFactory;

async function main() {
  console.log('Creating streaming pipeline...\n');

  // Define SHACL shapes for validation
  const shapes = `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass ex:Person ;
      sh:property [
        sh:path ex:name ;
        sh:minCount 1 ;
        sh:datatype <http://www.w3.org/2001/XMLSchema#string>
      ] ;
      sh:property [
        sh:path ex:age ;
        sh:datatype <http://www.w3.org/2001/XMLSchema#integer> ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150
      ] .
  `;

  // Create pipeline with all components
  const pipeline = createStreamingPipeline({
    changeFeed: {
      enableHistory: true,
      historySize: 1000,
      batchMode: false,
    },
    streamProcessor: {
      enableWindowing: true,
      enableAggregation: true,
    },
    validator: {
      mode: ValidationMode.DELTA,
      shapes,
      strict: false,
      enableCaching: true,
    },
  });

  // Configure windowing - tumbling window of 5 events
  pipeline.streamProcessor.configureWindowing({
    type: WindowType.COUNT,
    size: 100,
    count: 5,
  });

  // Register aggregators
  pipeline.streamProcessor.registerAggregator('count', Aggregators.count);
  pipeline.streamProcessor.registerAggregator('avgAge', events => {
    const ages = events
      .filter(e => e.delta?.additions?.length > 0)
      .flatMap(e => e.delta.additions)
      .filter(q => q.predicate.value === 'http://example.org/age')
      .map(q => parseInt(q.object.value) || 0);

    return ages.length > 0 ? ages.reduce((sum, age) => sum + age, 0) / ages.length : 0;
  });

  // Listen for window events
  pipeline.streamProcessor.on('window-created', window => {
    console.log(`Window created: ${window.id}`);
  });

  pipeline.streamProcessor.on('window-closed', window => {
    console.log(`Window closed: ${window.id}, events: ${window.count}`);
  });

  // Listen for validation events
  if (pipeline.validator) {
    pipeline.validator.on('violation', result => {
      console.error('SHACL Violation detected:', {
        id: result.id,
        violations: result.violations.map(v => v.message),
      });
    });
  }

  // Listen for pipeline results
  pipeline.subscriptionManager.on('pipeline-result', result => {
    console.log('Pipeline result:', {
      eventId: result.event?.id,
      windowId: result.windowId,
      windowCount: result.windowCount,
      aggregations: result.aggregations,
      validationConforms: result.validation?.conforms,
    });
  });

  // Listen for pipeline errors
  pipeline.subscriptionManager.on('pipeline-error', error => {
    console.error('Pipeline error:', error.message);
  });

  // Start the pipeline
  pipeline.start();
  console.log('Pipeline started\n');

  // Simulate streaming events
  console.log('Simulating stream events...\n');

  const events = [
    // Valid person 1
    {
      id: 'evt-1',
      type: 'add',
      delta: {
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
        ],
        removals: [],
      },
    },
    // Valid person 2
    {
      id: 'evt-2',
      type: 'add',
      delta: {
        additions: [
          quad(
            namedNode('http://example.org/bob'),
            namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            namedNode('http://example.org/Person')
          ),
          quad(
            namedNode('http://example.org/bob'),
            namedNode('http://example.org/name'),
            literal('Bob')
          ),
          quad(
            namedNode('http://example.org/bob'),
            namedNode('http://example.org/age'),
            literal('25', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
          ),
        ],
        removals: [],
      },
    },
    // Invalid person (missing name) - should trigger violation
    {
      id: 'evt-3',
      type: 'add',
      delta: {
        additions: [
          quad(
            namedNode('http://example.org/charlie'),
            namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            namedNode('http://example.org/Person')
          ),
          quad(
            namedNode('http://example.org/charlie'),
            namedNode('http://example.org/age'),
            literal('35', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
          ),
        ],
        removals: [],
      },
    },
    // Update person age
    {
      id: 'evt-4',
      type: 'update',
      delta: {
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
      },
    },
    // Valid person 3 - will trigger window closure
    {
      id: 'evt-5',
      type: 'add',
      delta: {
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
            namedNode('http://example.org/age'),
            literal('28', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
          ),
        ],
        removals: [],
      },
    },
  ];

  // Process events with delay
  for (const event of events) {
    console.log(`Processing event: ${event.id}`);

    // Record in change feed
    pipeline.changeFeed.recordChange(event.delta, {
      eventId: event.id,
      eventType: event.type,
    });

    // Small delay between events
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  // Wait for processing to complete
  await new Promise(resolve => setTimeout(resolve, 500));

  // Get pipeline metrics
  console.log('\nPipeline Metrics:\n');
  const metrics = pipeline.getMetrics();
  console.log(JSON.stringify(metrics, null, 2));

  // Cleanup
  console.log('\nCleaning up pipeline...');
  await pipeline.cleanup();
  console.log('Cleanup complete');
}

main().catch(console.error);
