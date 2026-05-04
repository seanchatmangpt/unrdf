/**
 * @file Real-Time Workflow Analytics Pipeline Example
 * @description
 * Demonstrates YAWL-Kafka integration for real-time workflow analytics.
 * Produces YAWL events to Kafka and consumes them for metrics calculation.
 */

import { createWorkflowEngine } from '@unrdf/yawl';
import { YawlWorkflow } from '@unrdf/yawl/workflow';
import { createYAWLKafkaProducer, createYAWLKafkaConsumer } from '../index.mjs';

// =============================================================================
// Configuration
// =============================================================================

const KAFKA_BROKERS = process.env.KAFKA_BROKERS?.split(',') || ['localhost:9092'];
const WORKFLOW_ID = 'expense-approval';

// =============================================================================
// Workflow Definition
// =============================================================================

/**
 * Create expense approval workflow
 * @returns {YawlWorkflow} Configured workflow
 */
function createExpenseWorkflow() {
  const workflow = new YawlWorkflow({
    id: WORKFLOW_ID,
    name: 'Expense Approval Process',
    version: '1.0.0',
  });

  // Define tasks
  workflow.addTask({
    id: 'submit',
    name: 'Submit Expense',
    join: 'xor',
    split: 'and',
  });

  workflow.addTask({
    id: 'review',
    name: 'Manager Review',
    join: 'xor',
    split: 'xor',
  });

  workflow.addTask({
    id: 'approve',
    name: 'Approve Expense',
    join: 'xor',
    split: 'xor',
  });

  workflow.addTask({
    id: 'reject',
    name: 'Reject Expense',
    join: 'xor',
    split: 'xor',
  });

  workflow.addTask({
    id: 'process_payment',
    name: 'Process Payment',
    join: 'xor',
    split: 'xor',
  });

  // Define control flow
  workflow.addFlow('submit', 'review');
  workflow.addFlow('review', 'approve');
  workflow.addFlow('review', 'reject');
  workflow.addFlow('approve', 'process_payment');

  // Set start and end
  workflow.setStart('submit');
  workflow.setEnd(['reject', 'process_payment']);

  return workflow;
}

// =============================================================================
// Analytics Consumer
// =============================================================================

/**
 * Analytics metrics aggregator
 */
class WorkflowAnalytics {
  constructor() {
    this.metrics = {
      casesCreated: 0,
      tasksCompleted: 0,
      tasksEnabled: 0,
      tasksCancelled: 0,
      completionTimes: [],
      taskDurations: new Map(),
    };

    this.taskStartTimes = new Map();
  }

  /**
   * Handle CASE_CREATED event
   */
  onCaseCreated(event) {
    this.metrics.casesCreated++;
    console.log(`📊 [Analytics] Case created: ${event.caseId} (spec: ${event.specId})`);
  }

  /**
   * Handle TASK_ENABLED event
   */
  onTaskEnabled(event) {
    this.metrics.tasksEnabled++;
    console.log(`📊 [Analytics] Task enabled: ${event.taskId} (workItem: ${event.workItemId})`);
  }

  /**
   * Handle TASK_STARTED event
   */
  onTaskStarted(event, metadata) {
    this.taskStartTimes.set(event.workItemId, new Date(event.startedAt));
    console.log(`📊 [Analytics] Task started: ${event.workItemId}`);
    console.log(`   Receipt verified: ${metadata.receiptVerified}`);
  }

  /**
   * Handle TASK_COMPLETED event
   */
  onTaskCompleted(event, metadata) {
    this.metrics.tasksCompleted++;

    const startTime = this.taskStartTimes.get(event.workItemId);
    if (startTime) {
      const endTime = new Date(event.completedAt);
      const duration = endTime - startTime;
      this.metrics.taskDurations.set(event.workItemId, duration);

      console.log(`📊 [Analytics] Task completed: ${event.workItemId}`);
      console.log(`   Duration: ${duration}ms`);
      console.log(`   Receipt hash: ${metadata.headers['yawl.receipt.hash']?.substring(0, 16)}...`);
    }
  }

  /**
   * Handle TASK_CANCELLED event
   */
  onTaskCancelled(event) {
    this.metrics.tasksCancelled++;
    console.log(`📊 [Analytics] Task cancelled: ${event.workItemId} (reason: ${event.reason})`);
  }

  /**
   * Print analytics summary
   */
  printSummary() {
    console.log('\n' + '='.repeat(60));
    console.log('📊 WORKFLOW ANALYTICS SUMMARY');
    console.log('='.repeat(60));
    console.log(`Cases Created:     ${this.metrics.casesCreated}`);
    console.log(`Tasks Enabled:     ${this.metrics.tasksEnabled}`);
    console.log(`Tasks Completed:   ${this.metrics.tasksCompleted}`);
    console.log(`Tasks Cancelled:   ${this.metrics.tasksCancelled}`);

    if (this.metrics.taskDurations.size > 0) {
      const durations = [...this.metrics.taskDurations.values()];
      const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length;
      const maxDuration = Math.max(...durations);
      const minDuration = Math.min(...durations);

      console.log(`\nTask Duration Stats:`);
      console.log(`  Average: ${avgDuration.toFixed(2)}ms`);
      console.log(`  Min:     ${minDuration}ms`);
      console.log(`  Max:     ${maxDuration}ms`);
    }

    console.log('='.repeat(60) + '\n');
  }
}

// =============================================================================
// Main Pipeline
// =============================================================================

/**
 * Run the analytics pipeline demo
 * @returns {Promise<void>}
 */
async function runAnalyticsPipeline() {
  console.log('🚀 Starting YAWL-Kafka Real-Time Analytics Pipeline\n');

  // Create analytics aggregator
  const analytics = new WorkflowAnalytics();

  // Initialize Kafka producer
  console.log('🔧 Initializing Kafka producer...');
  const producer = createYAWLKafkaProducer({
    brokers: KAFKA_BROKERS,
    clientId: 'yawl-producer-example',
  });

  await producer.connect();
  console.log('✅ Producer connected\n');

  // Initialize Kafka consumer
  console.log('🔧 Initializing Kafka consumer...');
  const consumer = createYAWLKafkaConsumer({
    brokers: KAFKA_BROKERS,
    groupId: 'analytics-pipeline',
    clientId: 'yawl-consumer-example',
  });

  // Register event handlers
  consumer.on('YAWL_CASE_CREATED', (event) => analytics.onCaseCreated(event));
  consumer.on('YAWL_TASK_ENABLED', (event) => analytics.onTaskEnabled(event));
  consumer.on('YAWL_TASK_STARTED', (event, meta) => analytics.onTaskStarted(event, meta));
  consumer.on('YAWL_TASK_COMPLETED', (event, meta) => analytics.onTaskCompleted(event, meta));
  consumer.on('YAWL_TASK_CANCELLED', (event) => analytics.onTaskCancelled(event));

  await consumer.connect();
  await consumer.subscribe([
    'YAWL_CASE_CREATED',
    'YAWL_TASK_ENABLED',
    'YAWL_TASK_STARTED',
    'YAWL_TASK_COMPLETED',
    'YAWL_TASK_CANCELLED',
  ]);

  // Start consumer in background
  consumer.run().catch(console.error);
  console.log('✅ Consumer running\n');

  // Create YAWL workflow engine
  console.log('🔧 Initializing YAWL workflow engine...');
  const engine = createWorkflowEngine({
    enableEventLog: false, // We're using Kafka instead
  });

  const workflow = createExpenseWorkflow();
  engine.registerWorkflow(workflow);

  // Attach Kafka producer to engine
  producer.attachToEngine(engine);
  console.log('✅ Producer attached to engine\n');

  // Simulate workflow execution
  console.log('🎬 Executing expense approval workflows...\n');

  // Create and execute 3 expense cases
  for (let i = 1; i <= 3; i++) {
    console.log(`\n--- Case ${i}: Processing Expense Request ---`);

    const { case: expenseCase } = await engine.createCase(WORKFLOW_ID, {
      expenseId: `EXP-${i}`,
      amount: 500 + i * 100,
      submitter: `employee${i}@company.com`,
    });

    console.log(`✅ Case created: ${expenseCase.id}`);

    // Get enabled work items and execute workflow
    const submitItems = expenseCase.getEnabledWorkItems();
    if (submitItems.length > 0) {
      const submitItem = submitItems[0];

      await engine.startTask(expenseCase.id, submitItem.id);
      console.log(`   Task started: submit`);

      await engine.completeTask(expenseCase.id, submitItem.id, {
        submitted: true,
        date: new Date().toISOString(),
      });
      console.log(`   Task completed: submit`);

      // Review task
      const reviewItems = expenseCase.getEnabledWorkItems();
      if (reviewItems.length > 0) {
        const reviewItem = reviewItems[0];

        await engine.startTask(expenseCase.id, reviewItem.id);
        await new Promise(resolve => setTimeout(resolve, 100)); // Simulate processing time

        await engine.completeTask(expenseCase.id, reviewItem.id, {
          reviewed: true,
          decision: i % 2 === 0 ? 'approve' : 'reject',
        });
        console.log(`   Task completed: review (decision: ${i % 2 === 0 ? 'approve' : 'reject'})`);
      }
    }

    // Small delay between cases
    await new Promise(resolve => setTimeout(resolve, 200));
  }

  // Wait for events to be consumed
  console.log('\n⏳ Waiting for events to be processed...');
  await new Promise(resolve => setTimeout(resolve, 2000));

  // Print analytics summary
  analytics.printSummary();

  // Print Kafka producer stats
  console.log('📈 Kafka Producer Stats:');
  console.log(JSON.stringify(producer.getStats(), null, 2));

  console.log('\n📈 Kafka Consumer Stats:');
  console.log(JSON.stringify(consumer.getStats(), null, 2));

  // Cleanup
  console.log('\n🧹 Cleaning up...');
  await producer.disconnect();
  await consumer.disconnect();

  console.log('✅ Pipeline completed successfully!\n');
}

// =============================================================================
// Run Example
// =============================================================================

// Only run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAnalyticsPipeline()
    .then(() => {
      console.log('✨ Example finished');
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Pipeline failed:', error);
      process.exit(1);
    });
}

export { runAnalyticsPipeline };
