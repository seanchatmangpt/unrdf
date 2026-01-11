#!/usr/bin/env node
/**
 * @file YAWL-Nitro Workflow Integration Example
 * @module examples/yawl-nitro-workflow
 * @description Production-ready example demonstrating:
 * - YAWL workflow creation and execution
 * - Nitro task scheduler integration via Daemon
 * - Scheduled workflow case creation
 * - Task timeout enforcement
 * - Automatic retry with exponential backoff
 * - Event monitoring and metrics
 * - Error handling strategies
 * - Health checks and graceful shutdown
 *
 * This example can be run directly:
 * ```bash
 * node examples/yawl-nitro-workflow.mjs
 * ```
 */

import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';
import { Daemon } from '@unrdf/daemon';
import { createNitroTaskExecutor } from '@unrdf/daemon/integrations/nitro-tasks';
import { z } from 'zod';

// ============================================================================
// SCHEMAS AND VALIDATION
// ============================================================================

/**
 * Document submission payload schema
 */
const DocumentPayloadSchema = z.object({
  documentId: z.string().min(1),
  title: z.string().min(1),
  content: z.string().min(10),
  author: z.string().email(),
  category: z.enum(['technical', 'business', 'legal']),
});

/**
 * Review result schema
 */
const ReviewResultSchema = z.object({
  approved: z.boolean(),
  reviewer: z.string(),
  comments: z.string(),
  score: z.number().min(0).max(100),
});

// ============================================================================
// WORKFLOW DEFINITION
// ============================================================================

/**
 * Create document approval workflow
 *
 * Flow: Submit → Review → Approve → Publish
 *
 * @param {Store} store - RDF store instance
 * @returns {Promise<Object>} Workflow creation receipt
 */
async function createDocumentWorkflow(store) {
  console.log('\n📋 Creating workflow definition...');

  const workflowReceipt = await createWorkflow(store, {
    id: 'document-approval-v1',
    name: 'Document Approval Workflow',
    description: 'Four-step document review and publication process',
    version: '1.0.0',
    tasks: [
      {
        id: 'submit',
        name: 'Submit Document',
        kind: 'atomic',
        description: 'Submit document for review',
      },
      {
        id: 'review',
        name: 'Review Document',
        kind: 'atomic',
        description: 'Review document content and quality',
      },
      {
        id: 'approve',
        name: 'Approve Document',
        kind: 'atomic',
        description: 'Management approval decision',
      },
      {
        id: 'publish',
        name: 'Publish Document',
        kind: 'atomic',
        description: 'Publish approved document',
      },
    ],
    flow: [
      { from: 'submit', to: 'review' },
      { from: 'review', to: 'approve' },
      { from: 'approve', to: 'publish' },
    ],
  });

  console.log(`✓ Workflow created: ${workflowReceipt.workflow_id}`);
  console.log(`  Receipt ID: ${workflowReceipt.receipt_id}`);
  console.log(`  Hash: ${workflowReceipt.hash.substring(0, 16)}...`);

  return workflowReceipt;
}

// ============================================================================
// TASK HANDLERS
// ============================================================================

/**
 * Submit document handler
 * Validates document and prepares for review
 *
 * @param {Object} context - Execution context
 * @returns {Promise<Object>} Submission result
 */
async function submitDocumentHandler(context = {}) {
  console.log('\n[SUBMIT] Processing document submission...');

  // Validate payload
  const document = DocumentPayloadSchema.parse(context);
  console.log(`  Document: ${document.title} (${document.documentId})`);
  console.log(`  Author: ${document.author}`);
  console.log(`  Category: ${document.category}`);

  // Simulate document processing
  await new Promise((resolve) => setTimeout(resolve, 150));

  // Content analysis
  const wordCount = document.content.split(/\s+/).length;
  const readingTime = Math.ceil(wordCount / 200); // 200 words/min

  console.log(`  Word count: ${wordCount}`);
  console.log(`  Reading time: ${readingTime} min`);

  return {
    success: true,
    documentId: document.documentId,
    title: document.title,
    author: document.author,
    category: document.category,
    wordCount,
    readingTime,
    submittedAt: new Date(),
    status: 'submitted',
  };
}

/**
 * Review document handler
 * Performs quality review and scoring
 *
 * @param {Object} context - Execution context
 * @returns {Promise<Object>} Review result
 */
async function reviewDocumentHandler(context = {}) {
  console.log('\n[REVIEW] Reviewing document...');
  console.log(`  Document: ${context.title || context.documentId}`);

  // Simulate review process
  await new Promise((resolve) => setTimeout(resolve, 250));

  // Automated scoring (in production, this would be real analysis)
  const baseScore = Math.random() * 30 + 50; // 50-80
  const categoryBonus = {
    technical: 10,
    business: 5,
    legal: 15,
  };

  const finalScore = Math.min(
    100,
    Math.round(baseScore + (categoryBonus[context.category] || 0))
  );

  const approved = finalScore >= 70;

  console.log(`  Score: ${finalScore}/100`);
  console.log(`  Status: ${approved ? 'APPROVED' : 'NEEDS REVISION'}`);

  return {
    success: true,
    documentId: context.documentId,
    approved,
    reviewer: 'automated-reviewer-v1',
    score: finalScore,
    comments: approved
      ? `Document meets quality standards (score: ${finalScore}/100)`
      : `Document needs improvement (score: ${finalScore}/100). Please revise and resubmit.`,
    reviewedAt: new Date(),
    status: 'reviewed',
  };
}

/**
 * Approve document handler
 * Management approval step
 *
 * @param {Object} context - Execution context
 * @returns {Promise<Object>} Approval result
 */
async function approveDocumentHandler(context = {}) {
  console.log('\n[APPROVE] Processing approval...');
  console.log(`  Document: ${context.title || context.documentId}`);

  // Validate previous review
  if (!context.approved) {
    throw new Error('Cannot approve document that failed review');
  }

  // Simulate approval process
  await new Promise((resolve) => setTimeout(resolve, 100));

  // Auto-approve high-scoring documents
  const autoApproved = context.score >= 85;

  console.log(`  Review score: ${context.score}/100`);
  console.log(`  Approval: ${autoApproved ? 'AUTO-APPROVED' : 'MANUAL REVIEW REQUIRED'}`);

  return {
    success: true,
    documentId: context.documentId,
    approved: true,
    approver: autoApproved ? 'auto-approver' : 'pending-manager-review',
    approvalType: autoApproved ? 'automatic' : 'manual',
    approvedAt: new Date(),
    status: 'approved',
  };
}

/**
 * Publish document handler
 * Publish approved document
 *
 * @param {Object} context - Execution context
 * @returns {Promise<Object>} Publication result
 */
async function publishDocumentHandler(context = {}) {
  console.log('\n[PUBLISH] Publishing document...');
  console.log(`  Document: ${context.title || context.documentId}`);

  // Simulate publication
  await new Promise((resolve) => setTimeout(resolve, 200));

  const publishUrl = `https://docs.example.com/${context.category}/${context.documentId}`;

  console.log(`  Published: ${publishUrl}`);
  console.log(`  Status: LIVE`);

  return {
    success: true,
    documentId: context.documentId,
    publishUrl,
    publishedAt: new Date(),
    status: 'published',
  };
}

// ============================================================================
// DAEMON AND EXECUTOR SETUP
// ============================================================================

/**
 * Set up daemon with workflow operations
 *
 * @param {Store} store - RDF store instance
 * @returns {Promise<Object>} Daemon, executor, and operation IDs
 */
async function setupDaemonExecutor(store) {
  console.log('\n⚙️  Setting up daemon and executor...');

  // Create daemon
  const daemon = new Daemon({
    daemonId: crypto.randomUUID(),
    name: 'document-workflow-daemon',
    logLevel: 'info',
    concurrency: 10,
  });

  console.log(`✓ Daemon created: ${daemon.id}`);

  // Generate operation IDs
  const operationIds = {
    submit: crypto.randomUUID(),
    review: crypto.randomUUID(),
    approve: crypto.randomUUID(),
    publish: crypto.randomUUID(),
  };

  // Register workflow operations
  daemon.schedule({
    id: operationIds.submit,
    name: 'workflow:document:submit',
    handler: submitDocumentHandler,
    metadata: { category: 'workflow', priority: 'high', timeout: 5000 },
  });

  daemon.schedule({
    id: operationIds.review,
    name: 'workflow:document:review',
    handler: reviewDocumentHandler,
    metadata: { category: 'workflow', priority: 'normal', timeout: 10000 },
  });

  daemon.schedule({
    id: operationIds.approve,
    name: 'workflow:document:approve',
    handler: approveDocumentHandler,
    metadata: { category: 'workflow', priority: 'high', timeout: 5000 },
  });

  daemon.schedule({
    id: operationIds.publish,
    name: 'workflow:document:publish',
    handler: publishDocumentHandler,
    metadata: { category: 'workflow', priority: 'normal', timeout: 8000 },
  });

  console.log('✓ Registered 4 workflow operations');

  // Create Nitro task executor
  const executor = createNitroTaskExecutor(daemon, {
    autoStart: true,
    timeout: 30000,
    maxRetries: 3,
    enableEventRelay: true,
    enableMetrics: true,
    retryPolicy: {
      backoffMs: 1000,
      backoffMultiplier: 2,
      maxBackoffMs: 30000,
      jitterFactor: 0.1,
    },
  });

  console.log(`✓ Executor created: ${executor.id}`);

  // Register Nitro tasks
  executor.registerOperationAsTask(operationIds.submit, 'document:submit', {
    description: 'Submit document for review',
    cronExpression: '0 9 * * *', // Daily at 9 AM
    priority: 'high',
    tags: ['workflow', 'submit', 'document'],
  });

  executor.registerOperationAsTask(operationIds.review, 'document:review', {
    description: 'Review document quality',
    cronExpression: '0 10 * * *', // Daily at 10 AM
    priority: 'normal',
    tags: ['workflow', 'review', 'document'],
  });

  executor.registerOperationAsTask(operationIds.approve, 'document:approve', {
    description: 'Approve reviewed document',
    cronExpression: '0 11 * * *', // Daily at 11 AM
    priority: 'high',
    tags: ['workflow', 'approve', 'document'],
  });

  executor.registerOperationAsTask(operationIds.publish, 'document:publish', {
    description: 'Publish approved document',
    cronExpression: '0 14 * * *', // Daily at 2 PM
    priority: 'normal',
    tags: ['workflow', 'publish', 'document'],
  });

  console.log('✓ Registered 4 Nitro tasks');

  return { daemon, executor, operationIds };
}

// ============================================================================
// EVENT MONITORING
// ============================================================================

/**
 * Set up event monitoring and logging
 *
 * @param {NitroTaskExecutor} executor - Executor instance
 */
function setupEventMonitoring(executor) {
  console.log('\n📡 Setting up event monitoring...');

  executor.on('task:started', (event) => {
    console.log(`\n▶️  Task started: ${event.nitroTaskId}`);
  });

  executor.on('task:succeeded', (event) => {
    console.log(`✓ Task succeeded: ${event.nitroTaskId} (${event.duration}ms)`);
  });

  executor.on('task:failed', (event) => {
    console.error(`\n✗ Task failed: ${event.nitroTaskId}`);
    console.error(`  Error: ${event.error}`);
    console.error(`  Duration: ${event.duration}ms`);
    if (event.attempt) {
      console.error(`  Attempt: ${event.attempt}`);
      if (event.nextRetryIn) {
        console.error(`  Next retry in: ${event.nextRetryIn}ms`);
      }
    }
  });

  executor.on('task:retry-exhausted', (event) => {
    console.error(`\n❌ Retry exhausted: ${event.nitroTaskId}`);
    console.error(`  Failed after ${event.attempts} attempts`);
    console.error(`  Final error: ${event.error}`);
  });

  console.log('✓ Event monitoring enabled');
}

// ============================================================================
// WORKFLOW EXECUTION
// ============================================================================

/**
 * Execute complete workflow with all tasks
 *
 * @param {Store} store - RDF store instance
 * @param {Object} executor - Nitro task executor
 * @param {Object} operationIds - Operation ID mappings
 * @param {Object} documentData - Document payload
 * @returns {Promise<Object>} Workflow execution result
 */
async function executeCompleteWorkflow(store, executor, operationIds, documentData) {
  console.log('\n🚀 Executing complete workflow...');
  console.log(`  Document: ${documentData.title}`);

  const results = [];
  let context = { ...documentData };

  // Step 1: Submit
  console.log('\n--- Step 1: SUBMIT ---');
  const submitTaskId = executor.daemonToNitroMap.get(operationIds.submit);
  const submitResult = await executor.runTask(submitTaskId, context);
  results.push(submitResult);
  context = { ...context, ...submitResult.result };

  // Step 2: Review
  console.log('\n--- Step 2: REVIEW ---');
  const reviewTaskId = executor.daemonToNitroMap.get(operationIds.review);
  const reviewResult = await executor.runTask(reviewTaskId, context);
  results.push(reviewResult);
  context = { ...context, ...reviewResult.result };

  // Step 3: Approve (only if review passed)
  if (context.approved) {
    console.log('\n--- Step 3: APPROVE ---');
    const approveTaskId = executor.daemonToNitroMap.get(operationIds.approve);
    const approveResult = await executor.runTask(approveTaskId, context);
    results.push(approveResult);
    context = { ...context, ...approveResult.result };

    // Step 4: Publish
    console.log('\n--- Step 4: PUBLISH ---');
    const publishTaskId = executor.daemonToNitroMap.get(operationIds.publish);
    const publishResult = await executor.runTask(publishTaskId, context);
    results.push(publishResult);
    context = { ...context, ...publishResult.result };
  } else {
    console.log('\n⚠️  Workflow stopped: Document failed review');
  }

  return {
    success: true,
    documentId: documentData.documentId,
    finalStatus: context.status,
    stepsCompleted: results.length,
    results,
  };
}

// ============================================================================
// METRICS AND REPORTING
// ============================================================================

/**
 * Generate comprehensive metrics report
 *
 * @param {NitroTaskExecutor} executor - Executor instance
 */
function generateMetricsReport(executor) {
  const metrics = executor.getMetrics();
  const status = executor.getStatus();

  console.log('\n📊 Execution Metrics Report:');
  console.log('─'.repeat(60));

  console.log('\nExecution Statistics:');
  console.log(`  Tasks Executed:  ${metrics.tasksExecuted}`);
  console.log(`  Tasks Succeeded: ${metrics.tasksSucceeded}`);
  console.log(`  Tasks Failed:    ${metrics.tasksFailed}`);
  console.log(
    `  Success Rate:    ${((metrics.tasksSucceeded / metrics.tasksExecuted) * 100 || 0).toFixed(1)}%`
  );

  console.log('\nPerformance:');
  console.log(`  Total Duration:  ${metrics.totalDuration}ms`);
  console.log(`  Avg Duration:    ${metrics.averageDuration.toFixed(0)}ms`);
  console.log(`  Registered:      ${metrics.registeredTasks} tasks`);

  console.log('\nDaemon Health:');
  const health = metrics.daemonHealth;
  console.log(`  Running:         ${health.isRunning}`);
  console.log(`  Uptime:          ${(health.uptime / 1000).toFixed(1)}s`);
  console.log(`  Active Ops:      ${health.activeOperations}`);
  console.log(`  Queued Ops:      ${health.queuedOperations}`);

  console.log('\nExecutor Status:');
  console.log(`  Executor ID:     ${status.executorId}`);
  console.log(`  Running:         ${status.running}`);

  console.log('─'.repeat(60));
}

/**
 * List all registered tasks
 *
 * @param {NitroTaskExecutor} executor - Executor instance
 */
function listRegisteredTasks(executor) {
  console.log('\n📋 Registered Tasks:');
  console.log('─'.repeat(60));

  const tasks = executor.listTasks();
  tasks.forEach((task, index) => {
    console.log(`\n${index + 1}. ${task.nitroTaskId}`);
    console.log(`   Type:        ${task.operationType}`);
    console.log(`   Description: ${task.description || 'N/A'}`);
    console.log(`   Cron:        ${task.cronExpression || 'manual'}`);
    console.log(`   Priority:    ${task.priority}`);
    console.log(`   Tags:        ${task.tags?.join(', ') || 'none'}`);
  });

  console.log('─'.repeat(60));
}

// ============================================================================
// MAIN EXECUTION
// ============================================================================

/**
 * Main application entry point
 */
async function main() {
  console.log('╔═══════════════════════════════════════════════════════════╗');
  console.log('║   YAWL-Nitro Workflow Integration - Production Example   ║');
  console.log('╚═══════════════════════════════════════════════════════════╝');

  try {
    // 1. Create RDF store
    console.log('\n1. Creating RDF store...');
    const store = createStore();
    console.log('✓ RDF store created');

    // 2. Create workflow
    console.log('\n2. Creating workflow definition...');
    const workflowReceipt = await createDocumentWorkflow(store);

    // 3. Set up daemon and executor
    console.log('\n3. Setting up daemon and executor...');
    const { daemon, executor, operationIds } = await setupDaemonExecutor(store);

    // 4. Set up monitoring
    console.log('\n4. Setting up monitoring...');
    setupEventMonitoring(executor);

    // 5. List registered tasks
    console.log('\n5. Listing registered tasks...');
    listRegisteredTasks(executor);

    // 6. Execute workflow with sample document
    console.log('\n6. Executing workflow...');
    const sampleDocument = {
      documentId: `DOC-${Date.now()}`,
      title: 'YAWL-Nitro Integration Guide',
      content: `
        This comprehensive guide explains the integration between YAWL workflow engine
        and Nitro task scheduler. The integration enables scheduled workflow execution,
        timeout enforcement, automatic retry, and unified monitoring. Key features include
        cryptographic receipts, event sourcing, and production-grade error handling.
        The system supports 20 YAWL control flow patterns and scales to thousands of
        concurrent workflow cases. This document demonstrates the complete workflow
        lifecycle from submission through review, approval, and publication.
      `.trim(),
      author: 'workflow@example.com',
      category: 'technical',
    };

    const workflowResult = await executeCompleteWorkflow(
      store,
      executor,
      operationIds,
      sampleDocument
    );

    console.log('\n✅ Workflow execution complete!');
    console.log(`  Document ID: ${workflowResult.documentId}`);
    console.log(`  Final Status: ${workflowResult.finalStatus}`);
    console.log(`  Steps Completed: ${workflowResult.stepsCompleted}/4`);

    // 7. Generate metrics report
    console.log('\n7. Generating metrics report...');
    generateMetricsReport(executor);

    // 8. Get execution history
    console.log('\n8. Execution History:');
    const history = executor.getExecutionHistory();
    history.forEach((entry, index) => {
      const status = entry.success ? '✓' : '✗';
      console.log(
        `  ${index + 1}. ${status} ${entry.taskId} (${entry.duration}ms) at ${entry.timestamp.toISOString()}`
      );
    });

    // 9. Graceful shutdown
    console.log('\n9. Shutting down...');
    await executor.stop();
    console.log('✓ Executor stopped');

    console.log('\n╔═══════════════════════════════════════════════════════════╗');
    console.log('║                  Example Complete!                        ║');
    console.log('╚═══════════════════════════════════════════════════════════╝');

    console.log('\n📚 Next Steps:');
    console.log('  - Add more workflow patterns (parallel, conditional, OR-join)');
    console.log('  - Implement real task handlers with external APIs');
    console.log('  - Add health check endpoints for monitoring');
    console.log('  - Export metrics to Prometheus/Grafana');
    console.log('  - Deploy with Docker/Kubernetes');
    console.log('  - Connect to Nitro dev server for scheduling');
    console.log('\n📖 Documentation:');
    console.log('  - Tutorial: /docs/diataxis/tutorials/yawl-nitro-integration.md');
    console.log('  - How-To: /docs/diataxis/how-to/yawl-nitro-tasks.md');
    console.log('  - API Ref: /docs/diataxis/reference/yawl-nitro-api.md');
    console.log('  - Architecture: /docs/diataxis/explanation/yawl-nitro-architecture.md\n');

    process.exit(0);
  } catch (error) {
    console.error('\n❌ Fatal Error:', error.message);
    console.error('\nStack Trace:');
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

// Export for programmatic use
export {
  createDocumentWorkflow,
  setupDaemonExecutor,
  setupEventMonitoring,
  executeCompleteWorkflow,
  generateMetricsReport,
  listRegisteredTasks,
  submitDocumentHandler,
  reviewDocumentHandler,
  approveDocumentHandler,
  publishDocumentHandler,
};
