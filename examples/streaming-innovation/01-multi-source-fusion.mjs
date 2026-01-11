/**
 * @file Multi-Source Stream Fusion Example
 * @description Demonstrates fusion of RDF changes, daemon events, and YAWL workflows
 *
 * This example shows how to:
 * 1. Set up multiple event sources (RDF store, daemon, YAWL)
 * 2. Configure fusion with priority and correlation
 * 3. Process fused events with business logic
 *
 * Expected output: Correlated events from multiple sources merged intelligently
 */

import { createChangeFeed } from '../../packages/streaming/src/index.mjs';
import { EventEmitter } from 'events';

// ============================================================================
// Stream Fusion Coordinator (from research document)
// ============================================================================

class StreamFusionCoordinator extends EventEmitter {
  constructor(config = {}) {
    super();
    this.sources = new Map();
    this.correlationWindow = config.correlationWindowMs || 1000;
    this.priorityQueue = [];
    this.eventCache = new Map();
    this.correlationRules = config.correlationRules || [];
    this.stats = {
      totalEvents: 0,
      fusedEvents: 0,
      duplicates: 0,
    };
  }

  registerSource(sourceId, source, options = {}) {
    const config = {
      priority: options.priority || 5,
      transform: options.transform || (e => e),
      filter: options.filter || (() => true),
      correlationKey: options.correlationKey || 'id',
    };

    this.sources.set(sourceId, { source, config });

    // Subscribe to source events
    if (typeof source.subscribe === 'function') {
      source.subscribe((change) => {
        this._handleSourceEvent(sourceId, {
          type: 'rdf-change',
          sourceId,
          timestamp: Date.now(),
          data: change,
        });
      });
    }

    if (source instanceof EventEmitter) {
      source.on('event', (event) => {
        this._handleSourceEvent(sourceId, event);
      });
    }
  }

  _handleSourceEvent(sourceId, rawEvent) {
    const sourceConfig = this.sources.get(sourceId).config;

    // Apply filter
    if (!sourceConfig.filter(rawEvent)) {
      return;
    }

    // Transform event
    const event = {
      ...sourceConfig.transform(rawEvent),
      sourceId,
      priority: sourceConfig.priority,
      receivedAt: Date.now(),
    };

    this.stats.totalEvents++;

    // Check for duplicates
    const eventKey = this._getEventKey(event, sourceConfig.correlationKey);
    if (this.eventCache.has(eventKey)) {
      this.stats.duplicates++;
      this.emit('duplicate-event', { event, original: this.eventCache.get(eventKey) });
      return;
    }

    // Add to cache
    this.eventCache.set(eventKey, event);
    setTimeout(() => this.eventCache.delete(eventKey), this.correlationWindow);

    // Check for correlation
    const correlatedEvents = this._findCorrelatedEvents(event);

    if (correlatedEvents.length > 0) {
      // Merge correlated events
      const fusedEvent = this._fuseEvents([event, ...correlatedEvents]);
      this.stats.fusedEvents++;
      this.emit('fused-event', fusedEvent);
    } else {
      // Emit standalone event
      this.emit('event', event);
    }
  }

  _findCorrelatedEvents(event) {
    const correlated = [];

    for (const rule of this.correlationRules) {
      const matches = Array.from(this.eventCache.values()).filter(cached =>
        rule.match(event, cached) && cached !== event
      );
      correlated.push(...matches);
    }

    return correlated;
  }

  _fuseEvents(events) {
    // Sort by priority (highest first)
    events.sort((a, b) => b.priority - a.priority);

    return {
      type: 'fused',
      sources: events.map(e => e.sourceId),
      timestamp: Math.min(...events.map(e => e.receivedAt)),
      data: events.reduce((acc, e) => ({ ...acc, ...e.data }), {}),
      metadata: {
        eventCount: events.length,
        correlationWindow: this.correlationWindow,
        priorities: events.map(e => e.priority),
      },
    };
  }

  _getEventKey(event, keyField) {
    return `${event.sourceId}:${event.data?.[keyField] || event.timestamp}`;
  }

  getMetrics() {
    return {
      sources: this.sources.size,
      cachedEvents: this.eventCache.size,
      correlationRules: this.correlationRules.length,
      stats: this.stats,
    };
  }
}

// ============================================================================
// Correlation Rule
// ============================================================================

class CorrelationRule {
  constructor(config) {
    this.name = config.name;
    this.timeWindowMs = config.timeWindowMs || 1000;
    this.matcher = config.matcher;
  }

  match(event1, event2) {
    // Check time window
    const timeDiff = Math.abs(event1.receivedAt - event2.receivedAt);
    if (timeDiff > this.timeWindowMs) {
      return false;
    }

    // Apply custom matcher
    return this.matcher(event1, event2);
  }
}

// ============================================================================
// Example Usage
// ============================================================================

async function main() {
  console.log('='.repeat(70));
  console.log('Multi-Source Stream Fusion Example');
  console.log('='.repeat(70));
  console.log('');

  // Create event sources
  const rdfSource = createChangeFeed();
  const daemonSource = new EventEmitter();
  const yawlSource = new EventEmitter();

  // Create correlation rule: YAWL task completion + RDF update
  const taskUpdateCorrelation = new CorrelationRule({
    name: 'yawl-rdf-correlation',
    timeWindowMs: 2000,
    matcher: (e1, e2) => {
      const isYawl = e => e.type === 'task-completed';
      const isRdf = e => e.type === 'rdf-change';

      if (!(isYawl(e1) && isRdf(e2)) && !(isYawl(e2) && isRdf(e1))) {
        return false;
      }

      const yawlEvent = isYawl(e1) ? e1 : e2;
      const rdfEvent = isRdf(e1) ? e1 : e2;

      // Match on task ID
      return rdfEvent.data?.quad?.subject?.value?.includes(yawlEvent.data?.taskId);
    },
  });

  // Create fusion coordinator
  const fusion = new StreamFusionCoordinator({
    correlationWindowMs: 2000,
    correlationRules: [taskUpdateCorrelation],
  });

  // Register sources with priorities
  fusion.registerSource('rdf-store', rdfSource, {
    priority: 7, // High priority for RDF changes
    transform: (e) => ({ ...e, source: 'RDF Store' }),
  });

  fusion.registerSource('daemon', daemonSource, {
    priority: 5, // Medium priority for daemon events
    transform: (e) => ({ ...e, source: 'Daemon' }),
  });

  fusion.registerSource('yawl', yawlSource, {
    priority: 8, // Highest priority for workflow events
    transform: (e) => ({ ...e, source: 'YAWL Engine' }),
  });

  // Listen for fused events
  let fusedCount = 0;
  fusion.on('fused-event', (event) => {
    fusedCount++;
    console.log(`\n🔗 FUSED EVENT #${fusedCount}:`);
    console.log(`   Sources: ${event.sources.join(' + ')}`);
    console.log(`   Event Count: ${event.metadata.eventCount}`);
    console.log(`   Data:`, JSON.stringify(event.data, null, 2));
  });

  // Listen for standalone events
  let standaloneCount = 0;
  fusion.on('event', (event) => {
    standaloneCount++;
    console.log(`\n📍 Standalone Event #${standaloneCount}: ${event.sourceId}`);
  });

  // Listen for duplicates
  fusion.on('duplicate-event', (info) => {
    console.log(`\n⚠️  Duplicate detected from ${info.event.sourceId}`);
  });

  console.log('Starting event simulation...\n');

  // Simulate events
  await simulateEvents(rdfSource, daemonSource, yawlSource);

  // Wait for processing
  await new Promise(resolve => setTimeout(resolve, 3000));

  // Display metrics
  console.log('\n' + '='.repeat(70));
  console.log('Fusion Metrics:');
  console.log('='.repeat(70));
  const metrics = fusion.getMetrics();
  console.log(JSON.stringify(metrics, null, 2));

  console.log('\nExample complete!');
}

/**
 * Simulate events from multiple sources
 */
async function simulateEvents(rdfSource, daemonSource, yawlSource) {
  const taskId = 'task-approval-001';

  // Scenario 1: YAWL task completion followed by RDF update (should correlate)
  console.log('Scenario 1: Correlated YAWL + RDF events');

  yawlSource.emit('event', {
    type: 'task-completed',
    data: {
      taskId,
      caseId: 'case-123',
      completedBy: 'user-alice',
      timestamp: Date.now(),
    },
  });

  await delay(500);

  rdfSource.emitChange({
    type: 'add',
    quad: {
      subject: { value: `http://example.org/${taskId}` },
      predicate: { value: 'http://example.org/status' },
      object: { value: 'completed' },
    },
    timestamp: Date.now(),
  });

  await delay(1000);

  // Scenario 2: Standalone daemon event
  console.log('\nScenario 2: Standalone daemon event');

  daemonSource.emit('event', {
    type: 'operation-scheduled',
    data: {
      operationId: 'sync-001',
      scheduledFor: Date.now() + 60000,
    },
  });

  await delay(1000);

  // Scenario 3: Duplicate detection
  console.log('\nScenario 3: Duplicate event');

  const duplicateEvent = {
    type: 'rdf-change',
    data: {
      id: 'unique-event-001',
      change: 'test',
    },
  };

  rdfSource.emitChange(duplicateEvent);

  await delay(100);

  // Try to emit same event again
  rdfSource.emitChange(duplicateEvent);
}

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// Run example
main().catch(console.error);
