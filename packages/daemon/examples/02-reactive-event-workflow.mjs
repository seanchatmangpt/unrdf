/**
 * @file Reactive Event-Driven Workflow Example
 * @module examples/02-reactive-event-workflow
 * @description Demonstrates event-driven case creation and real-time task processing with streaming inputs.
 *
 * Features:
 * - Event-triggered case creation
 * - Real-time data streaming and processing
 * - Multiple event types (user actions, system events, API callbacks)
 * - Automatic task orchestration on events
 * - Event correlation and deduplication
 *
 * Use Case: E-commerce order processing system with real-time event streaming
 */

import { Daemon } from '../src/daemon.mjs';
import { randomUUID } from 'crypto';
import { EventEmitter } from 'events';

/**
 * Mock event stream provider
 * Simulates incoming events from various sources
 */
class EventStreamProvider extends EventEmitter {
  constructor() {
    super();
    this.eventBuffer = [];
    this.eventId = 0;
  }

  /**
   * Add event to stream
   * @param {string} eventType - Type of event
   * @param {Object} data - Event data
   */
  emit_event(eventType, data) {
    this.eventId += 1;
    const event = {
      eventId: `evt-${this.eventId}`,
      type: eventType,
      timestamp: Date.now(),
      data,
    };

    this.eventBuffer.push(event);
    this.emit('event', event);
  }

  /**
   * Get events by type
   * @param {string} eventType - Event type to filter
   * @returns {Array} Events of matching type
   */
  getEventsByType(eventType) {
    return this.eventBuffer.filter(e => e.type === eventType);
  }
}

/**
 * Mock Workflow Engine for event-driven cases
 */
class EventDrivenWorkflowEngine {
  constructor() {
    this.cases = new Map();
    this.events = new Map();
    this.listeners = new Map();
    this.eventCorrelations = new Map();
  }

  /**
   * Create case triggered by event
   * @param {Object} params - Creation parameters
   * @param {string} params.caseId - Case identifier
   * @param {string} params.eventId - Triggering event ID
   * @param {Object} params.eventData - Event data
   * @returns {Promise<Object>} Created case
   */
  async createCaseFromEvent(params) {
    const { caseId, eventId, eventData } = params;

    const caseObj = {
      caseId,
      status: 'started',
      triggeredByEvent: eventId,
      eventData,
      createdAt: Date.now(),
      tasks: [],
      processedEvents: new Set([eventId]),
    };

    this.cases.set(caseId, caseObj);
    this.events.set(eventId, caseId);
    this._emitEvent('case:created-from-event', {
      caseId,
      eventId,
      eventType: eventData.type,
    });

    return caseObj;
  }

  /**
   * Get or create case based on event correlation
   * Reuses case if event correlates with existing case
   * @param {Object} params - Parameters
   * @param {string} params.eventId - Event identifier
   * @param {Object} params.eventData - Event data
   * @param {string} [params.correlationKey] - Key to correlate events
   * @returns {Promise<Object>} Case object
   */
  async getOrCreateCaseFromEvent(params) {
    const { eventId, eventData, correlationKey } = params;

    // Check if event correlates with existing case
    if (correlationKey && this.eventCorrelations.has(correlationKey)) {
      const caseId = this.eventCorrelations.get(correlationKey);
      const caseObj = this.cases.get(caseId);
      caseObj.processedEvents.add(eventId);
      this._emitEvent('case:correlated-event', {
        caseId,
        eventId,
        correlationKey,
      });
      return caseObj;
    }

    // Create new case
    const caseId = `case-${eventId}`;
    const caseObj = await this.createCaseFromEvent({
      caseId,
      eventId,
      eventData,
    });

    if (correlationKey) {
      this.eventCorrelations.set(correlationKey, caseId);
    }

    return caseObj;
  }

  /**
   * Enable task for real-time processing
   * @param {Object} params - Task parameters
   * @param {string} params.caseId - Case identifier
   * @param {string} params.taskId - Task identifier
   * @param {Object} [params.inputData] - Task input data
   * @returns {Promise<Object>} Enabled task
   */
  async enableTaskForProcessing(params) {
    const { caseId, taskId, inputData = {} } = params;

    const caseObj = this.cases.get(caseId);
    if (!caseObj) {
      throw new Error(`Case not found: ${caseId}`);
    }

    const task = {
      taskId,
      caseId,
      status: 'processing',
      enabledAt: Date.now(),
      inputData,
    };

    caseObj.tasks.push(task);
    this._emitEvent('task:processing-enabled', { caseId, taskId });

    return task;
  }

  /**
   * Process task with streaming data
   * Simulates real-time data transformation
   * @param {Object} params - Processing parameters
   * @param {string} params.caseId - Case identifier
   * @param {string} params.taskId - Task identifier
   * @param {Object} params.streamData - Data to process
   * @returns {Promise<Object>} Processing result
   */
  async processStreamData(params) {
    const { caseId, taskId, streamData } = params;

    const caseObj = this.cases.get(caseId);
    const task = caseObj.tasks.find(t => t.taskId === taskId);
    if (!task) {
      throw new Error(`Task not found: ${taskId}`);
    }

    // Simulate processing
    await new Promise(resolve => setTimeout(resolve, Math.random() * 100));

    const processed = {
      ...streamData,
      processedAt: Date.now(),
      status: 'processed',
    };

    task.result = processed;
    this._emitEvent('stream:data-processed', {
      caseId,
      taskId,
      bytesProcessed: JSON.stringify(streamData).length,
    });

    return processed;
  }

  /**
   * Complete task and emit result event
   * @param {Object} params - Completion parameters
   * @param {string} params.caseId - Case identifier
   * @param {string} params.taskId - Task identifier
   * @param {Object} params.result - Task result
   * @returns {Promise<Object>} Completed task
   */
  async completeTask(params) {
    const { caseId, taskId, result } = params;

    const caseObj = this.cases.get(caseId);
    const task = caseObj.tasks.find(t => t.taskId === taskId);
    if (!task) {
      throw new Error(`Task not found: ${taskId}`);
    }

    task.status = 'completed';
    task.completedAt = Date.now();
    task.result = result;

    this._emitEvent('task:completed', {
      caseId,
      taskId,
      duration: task.completedAt - task.enabledAt,
    });

    return task;
  }

  /**
   * Register event listener
   * @param {string} eventName - Event name
   * @param {Function} handler - Event handler
   * @returns {Function} Unsubscriber
   */
  on(eventName, handler) {
    if (!this.listeners.has(eventName)) {
      this.listeners.set(eventName, []);
    }
    this.listeners.get(eventName).push(handler);

    return () => {
      const handlers = this.listeners.get(eventName);
      const idx = handlers.indexOf(handler);
      if (idx > -1) {
        handlers.splice(idx, 1);
      }
    };
  }

  /**
   * Emit event to listeners
   * @private
   */
  _emitEvent(eventName, data) {
    const handlers = this.listeners.get(eventName) || [];
    for (const handler of handlers) {
      try {
        handler(data);
      } catch (error) {
        console.error(`Event handler error for ${eventName}:`, error);
      }
    }
  }
}

/**
 * Main reactive event workflow example
 * Demonstrates real-time processing of incoming events with automatic case creation
 */
async function reactiveEventWorkflowExample() {
  console.log('=== Reactive Event-Driven Workflow Example ===\n');
  console.log('This example demonstrates event-triggered workflow execution\n');

  // Initialize components
  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'Event-Driven Workflow Daemon',
  });

  const workflowEngine = new EventDrivenWorkflowEngine();
  const eventStream = new EventStreamProvider();

  // =========================================================================
  // Setup event listeners
  // =========================================================================

  daemon.on('operation:started', (event) => {
    console.log(`  ▶ ${event.name}`);
  });

  daemon.on('operation:success', (event) => {
    console.log(`  ✓ ${event.name} completed`);
  });

  workflowEngine.on('case:created-from-event', (event) => {
    console.log(`    📦 Case created: ${event.caseId} (triggered by ${event.eventType})`);
  });

  workflowEngine.on('case:correlated-event', (event) => {
    console.log(`    🔗 Event correlated: ${event.caseId} (key: ${event.correlationKey})`);
  });

  workflowEngine.on('task:processing-enabled', (event) => {
    console.log(`    ⚙️  Task enabled for processing: ${event.taskId}`);
  });

  workflowEngine.on('stream:data-processed', (event) => {
    console.log(`    📊 Stream data processed: ${event.bytesProcessed} bytes`);
  });

  workflowEngine.on('task:completed', (event) => {
    console.log(`    ✓ Task completed: ${event.taskId} (${event.duration}ms)`);
  });

  // =========================================================================
  // Define event processing operations
  // =========================================================================

  let eventCounter = 0;

  /**
   * Operation: Process incoming event stream
   * Monitors events and creates cases or correlates to existing cases
   */
  const eventProcessingOperation = {
    id: 'process-event-stream',
    name: 'Event Stream Processor',
    handler: async () => {
      console.log('\n  📡 Processing event stream...');

      const events = eventStream.eventBuffer;
      const results = [];

      for (const event of events) {
        try {
          // Determine workflow type based on event
          let workflowId = 'order-processing';
          let correlationKey = null;

          if (event.data.orderId) {
            correlationKey = `order:${event.data.orderId}`;
          }

          // Get or create case
          const caseObj = await workflowEngine.getOrCreateCaseFromEvent({
            eventId: event.eventId,
            eventData: event.data,
            correlationKey,
          });

          results.push({
            eventId: event.eventId,
            caseId: caseObj.caseId,
            status: 'case-created',
          });
        } catch (error) {
          console.error(`    ✗ Error processing event ${event.eventId}: ${error.message}`);
        }
      }

      return {
        processedEvents: results.length,
        results,
      };
    },
    metadata: {
      type: 'event-processing',
      realtime: true,
    },
  };

  /**
   * Operation: Process streaming data for a case
   * Simulates real-time data transformation
   */
  const streamProcessingOperation = {
    id: 'stream-processor',
    name: 'Stream Data Processor',
    handler: async () => {
      console.log('\n  🔄 Processing stream data for cases...');

      const results = [];

      // Process each case with streaming data
      for (const [caseId, caseObj] of workflowEngine.cases) {
        try {
          // Enable processing task
          const task = await workflowEngine.enableTaskForProcessing({
            caseId,
            taskId: 'stream-transformation',
            inputData: caseObj.eventData,
          });

          // Process streaming data
          const streamData = {
            ...caseObj.eventData,
            processingId: `proc-${Date.now()}`,
            batchSize: Math.floor(Math.random() * 500) + 50,
          };

          const processed = await workflowEngine.processStreamData({
            caseId,
            taskId: task.taskId,
            streamData,
          });

          // Complete task
          await workflowEngine.completeTask({
            caseId,
            taskId: task.taskId,
            result: processed,
          });

          results.push({
            caseId,
            taskId: task.taskId,
            status: 'completed',
          });
        } catch (error) {
          console.error(`    ✗ Error processing case ${caseId}: ${error.message}`);
        }
      }

      return {
        processedCases: results.length,
        results,
      };
    },
    metadata: {
      type: 'stream-processing',
      realtime: true,
    },
  };

  /**
   * Operation: Generate statistics from processed events
   * Reports on event processing metrics
   */
  const statsOperation = {
    id: 'generate-stats',
    name: 'Statistics Generator',
    handler: async () => {
      const totalEvents = eventStream.eventBuffer.length;
      const totalCases = workflowEngine.cases.size;
      const completedTasks = Array.from(workflowEngine.cases.values()).reduce(
        (sum, c) => sum + c.tasks.filter(t => t.status === 'completed').length,
        0
      );

      return {
        totalEvents,
        totalCases,
        completedTasks,
        averageTasksPerCase: totalCases > 0 ? completedTasks / totalCases : 0,
        timestamp: new Date().toISOString(),
      };
    },
    metadata: {
      type: 'reporting',
    },
  };

  // =========================================================================
  // Start daemon
  // =========================================================================

  await daemon.start();
  console.log('✓ Daemon started\n');

  // =========================================================================
  // Simulate incoming events
  // =========================================================================

  console.log('📨 Simulating incoming events:\n');

  // Order placement events
  eventStream.emit_event('order:placed', {
    orderId: 'ORD-001',
    customerId: 'CUST-123',
    amount: 99.99,
    items: ['item-1', 'item-2'],
  });

  eventStream.emit_event('order:placed', {
    orderId: 'ORD-002',
    customerId: 'CUST-456',
    amount: 149.99,
    items: ['item-3'],
  });

  // Payment events
  eventStream.emit_event('payment:received', {
    orderId: 'ORD-001',
    paymentId: 'PAY-001',
    amount: 99.99,
    method: 'credit_card',
  });

  eventStream.emit_event('inventory:updated', {
    orderId: 'ORD-001',
    itemsReserved: 2,
    warehouseId: 'WH-001',
  });

  eventStream.emit_event('order:placed', {
    orderId: 'ORD-003',
    customerId: 'CUST-789',
    amount: 249.99,
    items: ['item-4', 'item-5', 'item-6'],
  });

  console.log(`\n✓ ${eventStream.eventBuffer.length} events simulated\n`);

  // =========================================================================
  // Schedule operations
  // =========================================================================

  console.log('📋 Scheduling operations:\n');
  daemon.schedule(eventProcessingOperation);
  daemon.schedule(streamProcessingOperation);
  daemon.schedule(statsOperation);

  // =========================================================================
  // Execute operations
  // =========================================================================

  console.log('▶️  Executing operations:\n');

  try {
    // Step 1: Process events and create cases
    console.log('[Step 1] Processing incoming events:');
    const eventResult = await daemon.execute('process-event-stream');
    console.log(`\n    Processed: ${eventResult.processedEvents} events`);

    // Step 2: Process streaming data
    console.log('\n[Step 2] Processing stream data:');
    const streamResult = await daemon.execute('stream-processor');
    console.log(`\n    Processed: ${streamResult.processedCases} cases`);

    // Step 3: Generate statistics
    console.log('\n[Step 3] Generating statistics:');
    const statsResult = await daemon.execute('generate-stats');
    console.log(`\n    Statistics:
      • Total Events: ${statsResult.totalEvents}
      • Total Cases: ${statsResult.totalCases}
      • Completed Tasks: ${statsResult.completedTasks}
      • Avg Tasks/Case: ${statsResult.averageTasksPerCase.toFixed(2)}`);
  } catch (error) {
    console.error(`\n✗ Execution error: ${error.message}`);
  }

  // =========================================================================
  // Display metrics
  // =========================================================================

  console.log('\n\n📊 Final Metrics:');
  const metrics = daemon.getMetrics();
  console.log(`  Executed Operations: ${metrics.totalOperations}`);
  console.log(`  Success Rate: ${metrics.successRate.toFixed(1)}%`);
  console.log(`  Average Duration: ${metrics.averageDuration.toFixed(2)}ms`);

  // =========================================================================
  // Cleanup
  // =========================================================================

  console.log('\n⏹️  Stopping daemon...');
  await daemon.stop();
  console.log('✓ Daemon stopped\n');

  console.log('✅ Example completed successfully!');
}

// Run the example
await reactiveEventWorkflowExample();
