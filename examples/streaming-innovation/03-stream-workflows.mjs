/**
 * @file Stream-Driven Workflows Example
 * @description Demonstrates CEP patterns triggering YAWL workflows
 *
 * This example shows how to:
 * 1. Define complex event patterns (sequences, combinations, absence)
 * 2. Detect patterns in real-time event streams
 * 3. Trigger YAWL workflows when patterns match
 * 4. Extract workflow parameters from matched events
 *
 * Expected output: Workflows automatically triggered by event patterns
 */

import { createStreamProcessor } from '../../packages/streaming/src/index.mjs';
import { EventEmitter } from 'events';

// ============================================================================
// Stream-Driven Workflow Engine (from research document)
// ============================================================================

class StreamWorkflowEngine extends EventEmitter {
  constructor(yawlEngine, streamProcessor, config = {}) {
    super();
    this.yawl = yawlEngine;
    this.stream = streamProcessor;
    this.patterns = new Map();
    this.patternStates = new Map();
    this.config = config;
    this.stats = {
      patternsMatched: 0,
      workflowsTriggered: 0,
      errors: 0,
    };
  }

  registerPattern(patternId, pattern) {
    const compiled = this._compilePattern(pattern);
    this.patterns.set(patternId, {
      ...pattern,
      compiled,
    });

    this.patternStates.set(patternId, {
      buffer: [],
      lastMatch: null,
    });

    this.emit('pattern:registered', { patternId, pattern });
  }

  start() {
    this.stream.subscribe((event) => {
      this._processEvent(event);
    });
  }

  _processEvent(event) {
    for (const [patternId, pattern] of this.patterns) {
      const state = this.patternStates.get(patternId);

      state.buffer.push(event);

      if (state.buffer.length > (pattern.bufferSize || 100)) {
        state.buffer.shift();
      }

      const match = this._checkPattern(pattern, state.buffer);

      if (match && match.matched) {
        this._handlePatternMatch(patternId, pattern, match);
        state.lastMatch = Date.now();

        if (pattern.clearOnMatch) {
          state.buffer = [];
        }
      }
    }
  }

  _checkPattern(pattern, buffer) {
    return pattern.compiled(buffer);
  }

  async _handlePatternMatch(patternId, pattern, matchResult) {
    this.stats.patternsMatched++;

    this.emit('pattern:matched', {
      patternId,
      timestamp: Date.now(),
      events: matchResult.events,
    });

    if (!pattern.workflowId) {
      return;
    }

    try {
      const params = this._extractParameters(pattern, matchResult);

      const caseData = await this.yawl.createCase(pattern.workflowId, {
        caseId: `stream-${patternId}-${Date.now()}`,
        inputData: params,
        metadata: {
          triggeredBy: 'stream-pattern',
          patternId,
          matchedEvents: matchResult.events.map(e => e.id || e.timestamp),
        },
      });

      this.stats.workflowsTriggered++;

      this.emit('workflow:triggered', {
        patternId,
        workflowId: pattern.workflowId,
        caseId: caseData.id,
      });
    } catch (error) {
      this.stats.errors++;
      this.emit('workflow:error', {
        patternId,
        error: error.message,
      });
    }
  }

  _extractParameters(pattern, matchResult) {
    if (!pattern.parameterExtractor) {
      return {};
    }

    return pattern.parameterExtractor(matchResult.events);
  }

  _compilePattern(pattern) {
    switch (pattern.type) {
      case 'sequence':
        return this._compileSequencePattern(pattern);
      case 'combination':
        return this._compileCombinationPattern(pattern);
      case 'absence':
        return this._compileAbsencePattern(pattern);
      case 'custom':
        return pattern.matcher;
      default:
        throw new Error(`Unknown pattern type: ${pattern.type}`);
    }
  }

  _compileSequencePattern(pattern) {
    return (buffer) => {
      const { sequence, withinMs } = pattern;

      for (let i = 0; i < buffer.length; i++) {
        const matches = [buffer[i]];
        let lastMatch = buffer[i];

        for (let step = 1; step < sequence.length; step++) {
          const matcher = sequence[step];

          const nextMatch = buffer.slice(i + 1).find(e => {
            const timeDiff = e.timestamp - lastMatch.timestamp;
            return timeDiff <= withinMs && matcher(e);
          });

          if (!nextMatch) {
            break;
          }

          matches.push(nextMatch);
          lastMatch = nextMatch;
        }

        if (matches.length === sequence.length) {
          return { matched: true, events: matches };
        }
      }

      return { matched: false };
    };
  }

  _compileCombinationPattern(pattern) {
    return (buffer) => {
      const { conditions, withinMs } = pattern;
      const baseTime = buffer[buffer.length - 1]?.timestamp || Date.now();

      const matches = conditions.map(condition =>
        buffer.find(e =>
          (baseTime - e.timestamp) <= withinMs && condition(e)
        )
      );

      if (matches.every(m => m !== undefined)) {
        return { matched: true, events: matches };
      }

      return { matched: false };
    };
  }

  _compileAbsencePattern(pattern) {
    return (buffer) => {
      const { presence, absence, durationMs } = pattern;

      const presenceEvent = buffer.find(presence);
      if (!presenceEvent) {
        return { matched: false };
      }

      const absenceInWindow = buffer.some(e =>
        e.timestamp > presenceEvent.timestamp &&
        (e.timestamp - presenceEvent.timestamp) <= durationMs &&
        absence(e)
      );

      if (!absenceInWindow) {
        return { matched: true, events: [presenceEvent] };
      }

      return { matched: false };
    };
  }

  getMetrics() {
    return {
      patternsRegistered: this.patterns.size,
      stats: this.stats,
      patternStates: Array.from(this.patternStates.entries()).map(([id, state]) => ({
        patternId: id,
        bufferSize: state.buffer.length,
        lastMatch: state.lastMatch,
      })),
    };
  }
}

// ============================================================================
// Pattern Builder DSL
// ============================================================================

class PatternBuilder {
  constructor() {
    this.pattern = {};
  }

  sequence(...matchers) {
    this.pattern.type = 'sequence';
    this.pattern.sequence = matchers;
    return this;
  }

  combination(...conditions) {
    this.pattern.type = 'combination';
    this.pattern.conditions = conditions;
    return this;
  }

  within(ms) {
    this.pattern.withinMs = ms;
    return this;
  }

  triggerWorkflow(workflowId) {
    this.pattern.workflowId = workflowId;
    return this;
  }

  extractParams(extractor) {
    this.pattern.parameterExtractor = extractor;
    return this;
  }

  build() {
    return this.pattern;
  }
}

// ============================================================================
// Mock YAWL Engine
// ============================================================================

class MockYAWLEngine {
  constructor() {
    this.cases = [];
  }

  async createCase(workflowId, options) {
    const caseData = {
      id: options.caseId,
      workflowId,
      inputData: options.inputData,
      metadata: options.metadata,
      createdAt: Date.now(),
      status: 'active',
    };

    this.cases.push(caseData);

    return caseData;
  }

  getCases() {
    return this.cases;
  }
}

// ============================================================================
// Mock Stream Processor
// ============================================================================

class MockStreamProcessor extends EventEmitter {
  constructor() {
    super();
    this.subscribers = [];
  }

  subscribe(callback) {
    this.subscribers.push(callback);
    return () => {
      const index = this.subscribers.indexOf(callback);
      if (index > -1) {
        this.subscribers.splice(index, 1);
      }
    };
  }

  emit(event) {
    for (const subscriber of this.subscribers) {
      subscriber(event);
    }
  }
}

// ============================================================================
// Example Usage
// ============================================================================

async function main() {
  console.log('='.repeat(70));
  console.log('Stream-Driven Workflows Example');
  console.log('='.repeat(70));
  console.log('');

  // Create mock YAWL engine and stream processor
  const yawl = new MockYAWLEngine();
  const stream = new MockStreamProcessor();

  // Create workflow engine
  const workflowEngine = new StreamWorkflowEngine(yawl, stream);

  // Listen for pattern matches
  workflowEngine.on('pattern:matched', (info) => {
    console.log(`\n🎯 Pattern Matched: ${info.patternId}`);
    console.log(`   Events: ${info.events.length}`);
    console.log(`   Event IDs: ${info.events.map(e => e.id).join(', ')}`);
  });

  // Listen for workflow triggers
  workflowEngine.on('workflow:triggered', (info) => {
    console.log(`\n🚀 Workflow Triggered:`);
    console.log(`   Pattern: ${info.patternId}`);
    console.log(`   Workflow: ${info.workflowId}`);
    console.log(`   Case ID: ${info.caseId}`);
  });

  // Pattern 1: High-value order followed by payment
  console.log('Registering Pattern 1: High-value order + payment sequence\n');

  const pattern1 = new PatternBuilder()
    .sequence(
      e => e.type === 'order' && e.data.value > 10000,
      e => e.type === 'payment' && e.data.status === 'confirmed'
    )
    .within(5 * 60 * 1000) // 5 minutes
    .triggerWorkflow('premium-order-fulfillment')
    .extractParams(events => ({
      orderId: events[0].data.orderId,
      paymentId: events[1].data.paymentId,
      priority: 'high',
      value: events[0].data.value,
    }))
    .build();

  workflowEngine.registerPattern('high-value-order', pattern1);

  // Pattern 2: Service degradation (combination)
  console.log('Registering Pattern 2: Service degradation detection\n');

  const pattern2 = new PatternBuilder()
    .combination(
      e => e.type === 'error-rate' && e.data.rate > 0.05,
      e => e.type === 'latency' && e.data.p95 > 1000,
      e => e.type === 'cpu-usage' && e.data.usage > 0.8
    )
    .within(60 * 1000) // 1 minute
    .triggerWorkflow('incident-response')
    .extractParams(events => ({
      errorRate: events[0].data.rate,
      latency: events[1].data.p95,
      cpuUsage: events[2].data.usage,
      severity: 'high',
    }))
    .build();

  workflowEngine.registerPattern('service-degradation', pattern2);

  // Start engine
  workflowEngine.start();

  // Simulate events
  console.log('Simulating event stream...\n');

  await simulateEvents(stream);

  // Wait for processing
  await new Promise(resolve => setTimeout(resolve, 1000));

  // Display results
  console.log('\n' + '='.repeat(70));
  console.log('Results:');
  console.log('='.repeat(70));

  const metrics = workflowEngine.getMetrics();
  console.log('\nWorkflow Engine Metrics:');
  console.log(JSON.stringify(metrics, null, 2));

  console.log('\nTriggered Workflow Cases:');
  yawl.getCases().forEach((caseData, index) => {
    console.log(`\nCase ${index + 1}:`);
    console.log(`  ID: ${caseData.id}`);
    console.log(`  Workflow: ${caseData.workflowId}`);
    console.log(`  Input:`, JSON.stringify(caseData.inputData, null, 2));
  });

  console.log('\nExample complete!');
}

/**
 * Simulate event stream
 */
async function simulateEvents(stream) {
  const baseTime = Date.now();

  // Scenario 1: High-value order + payment (should trigger)
  console.log('Scenario 1: High-value order followed by payment');

  stream.emit({
    id: 'event-1',
    type: 'order',
    timestamp: baseTime,
    data: {
      orderId: 'order-12345',
      value: 15000,
      customer: 'ACME Corp',
    },
  });

  await delay(2000);

  stream.emit({
    id: 'event-2',
    type: 'payment',
    timestamp: baseTime + 2000,
    data: {
      paymentId: 'pay-67890',
      orderId: 'order-12345',
      status: 'confirmed',
    },
  });

  await delay(1000);

  // Scenario 2: Service degradation (should trigger)
  console.log('\nScenario 2: Service degradation detected');

  stream.emit({
    id: 'event-3',
    type: 'error-rate',
    timestamp: baseTime + 3000,
    data: {
      rate: 0.08,
      service: 'api-gateway',
    },
  });

  stream.emit({
    id: 'event-4',
    type: 'latency',
    timestamp: baseTime + 3100,
    data: {
      p95: 1500,
      service: 'api-gateway',
    },
  });

  stream.emit({
    id: 'event-5',
    type: 'cpu-usage',
    timestamp: baseTime + 3200,
    data: {
      usage: 0.85,
      host: 'api-server-1',
    },
  });

  await delay(1000);

  // Scenario 3: Incomplete pattern (should NOT trigger)
  console.log('\nScenario 3: Incomplete pattern (order without payment)');

  stream.emit({
    id: 'event-6',
    type: 'order',
    timestamp: baseTime + 4000,
    data: {
      orderId: 'order-99999',
      value: 12000,
      customer: 'Test Corp',
    },
  });

  // No payment event - pattern should not match
}

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// Run example
main().catch(console.error);
