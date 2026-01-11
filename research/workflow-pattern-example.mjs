/**
 * @file Complete Workflow Pattern Integration Example
 * @module research/workflow-pattern-example
 * @description Demonstrates all three innovative workflow patterns working together
 *
 * Scenario: E-Commerce Order Processing Pipeline
 * - Self-Healing: Automatic retry on payment failures
 * - AI-Assisted: Intelligent fraud routing
 * - Distributed: Parallel processing across nodes
 */

import { Daemon } from '../packages/daemon/src/daemon.mjs';
import { createChangeFeed } from '../packages/streaming/src/streaming/change-feed.mjs';
import { WorkflowAdapter } from '../packages/v6-core/src/delta/adapters/workflow-adapter.mjs';
import {
  SelfHealingWorkflow,
  AIAssistedRouter,
  DistributedWorkflow,
} from './workflow-pattern-implementations.mjs';

// =============================================================================
// Mock Components Setup
// =============================================================================

/**
 * Mock YAWL Workflow Engine
 */
class MockYAWLEngine {
  constructor() {
    this.cases = new Map();
    this.tasks = new Map();
    this.listeners = new Map();
  }

  on(eventName, handler) {
    if (!this.listeners.has(eventName)) {
      this.listeners.set(eventName, []);
    }
    this.listeners.get(eventName).push(handler);

    return () => {
      const handlers = this.listeners.get(eventName);
      const idx = handlers.indexOf(handler);
      if (idx > -1) handlers.splice(idx, 1);
    };
  }

  emit(eventName, data) {
    const handlers = this.listeners.get(eventName) || [];
    handlers.forEach((h) => h(data));
  }

  async createCase(options) {
    const { caseId, workflowId, inputData } = options;
    this.cases.set(caseId, { caseId, workflowId, inputData, status: 'RUNNING' });
    this.emit('case:created', { caseId, workflowId });
    return { caseId, status: 'RUNNING' };
  }

  async enableTask(options) {
    const { caseId, taskId } = options;
    this.tasks.set(`${caseId}:${taskId}`, { caseId, taskId, status: 'ENABLED' });
    this.emit('task:enabled', { caseId, taskId });
    return { caseId, taskId, status: 'ENABLED' };
  }

  async completeTask(options) {
    const { caseId, taskId, outputData } = options;
    this.tasks.set(`${caseId}:${taskId}`, {
      caseId,
      taskId,
      status: 'COMPLETED',
      outputData,
    });
    this.emit('task:completed', { caseId, taskId, outputData });
    return { caseId, taskId, status: 'COMPLETED' };
  }

  async failTask(options) {
    const { caseId, taskId, error } = options;
    this.emit('task:failed', { caseId, taskId, error });
    return { caseId, taskId, status: 'FAILED', error };
  }
}

/**
 * Mock Federation Manager
 */
class MockFederation {
  constructor(nodeCount = 5) {
    this.nodes = Array.from({ length: nodeCount }, (_, i) => ({
      nodeId: `node-${i + 1}`,
      activeOperations: Math.floor(Math.random() * 10),
      available: true,
    }));
  }

  async getAvailablePeers() {
    return this.nodes.filter((n) => n.available);
  }

  async proposeConsensus(proposal) {
    // Simulate consensus delay
    await new Promise((resolve) => setTimeout(resolve, 100));

    return {
      accepted: true,
      proof: {
        proposalId: Date.now(),
        votesFor: this.nodes.length,
        votesAgainst: 0,
        timestamp: new Date().toISOString(),
      },
    };
  }
}

/**
 * Mock Daemon Cluster
 */
class MockDaemonCluster {
  constructor(nodeIds) {
    this.daemons = new Map();

    nodeIds.forEach((nodeId) => {
      this.daemons.set(nodeId, new Daemon({ daemonId: nodeId }));
    });
  }

  getDaemon(nodeId) {
    return this.daemons.get(nodeId);
  }

  async startAll() {
    for (const daemon of this.daemons.values()) {
      await daemon.start();
    }
  }

  async stopAll() {
    for (const daemon of this.daemons.values()) {
      await daemon.stop();
    }
  }
}

/**
 * Mock Delta Gate
 */
class MockDeltaGate {
  async propose(delta) {
    return {
      receipt: {
        id: `receipt-${Date.now()}`,
        receiptHash: '0'.repeat(64),
        timestamp: new Date().toISOString(),
        operations: delta.operations,
      },
    };
  }
}

/**
 * Mock LLM Client
 */
class MockLLMClient {
  async chat({ messages }) {
    // Simulate LLM latency
    await new Promise((resolve) => setTimeout(resolve, 500));

    // Mock intelligent routing decision
    const userMessage = messages[messages.length - 1].content;

    if (userMessage.includes('fraud') || userMessage.includes('suspicious')) {
      return {
        content: JSON.stringify({
          nextTask: 'manual-review',
          reasoning: 'Suspicious transaction patterns detected requiring manual review',
          confidence: 0.92,
        }),
      };
    } else if (userMessage.includes('approved') || userMessage.includes('verified')) {
      return {
        content: JSON.stringify({
          nextTask: 'process-payment',
          reasoning: 'Transaction verified and approved for processing',
          confidence: 0.95,
        }),
      };
    } else {
      return {
        content: JSON.stringify({
          nextTask: 'automated-review',
          reasoning: 'Standard transaction requiring automated review',
          confidence: 0.88,
        }),
      };
    }
  }
}

// =============================================================================
// Main Example
// =============================================================================

/**
 * E-Commerce Order Processing with All Three Patterns
 */
async function runCompleteExample() {
  console.log('=== Innovative Workflow Patterns - Complete Integration ===\n');
  console.log('Scenario: E-Commerce Order Processing Pipeline\n');

  // -------------------------------------------------------------------------
  // Setup Infrastructure
  // -------------------------------------------------------------------------

  console.log('[Setup] Initializing infrastructure...\n');

  // Create core components
  const yawlEngine = new MockYAWLEngine();
  const changeFeed = createChangeFeed();
  const federation = new MockFederation(5);
  const cluster = new MockDaemonCluster(['node-1', 'node-2', 'node-3', 'node-4', 'node-5']);
  const deltaGate = new MockDeltaGate();
  const llmClient = new MockLLMClient();
  const mainDaemon = new Daemon({ daemonId: 'main-daemon' });

  await mainDaemon.start();
  await cluster.startAll();

  console.log('✓ Infrastructure ready\n');

  // -------------------------------------------------------------------------
  // Initialize Workflow Patterns
  // -------------------------------------------------------------------------

  console.log('[Setup] Initializing workflow patterns...\n');

  // Pattern 1: Self-Healing
  const selfHealing = new SelfHealingWorkflow(mainDaemon, yawlEngine, deltaGate);

  await selfHealing.registerPolicy({
    taskId: 'payment-processing',
    maxRetries: 3,
    backoffMs: 1000,
    backoffMultiplier: 2,
    fallbackTask: 'manual-payment-review',
    validationQuery: 'ASK { ?account :hasBalance ?balance FILTER(?balance > 0) }',
  });

  console.log('  ✓ Self-Healing configured for payment-processing');

  // Pattern 2: AI-Assisted Routing
  const aiRouter = new AIAssistedRouter(yawlEngine, changeFeed, llmClient, deltaGate);

  await aiRouter.registerRouting({
    taskId: 'fraud-check',
    candidateTasks: ['manual-review', 'automated-review', 'process-payment'],
    promptTemplate: `
      Analyze this transaction and route to the appropriate task:
      Available routes: {{CANDIDATES}}
      Transaction data: {{TASK_OUTPUT}}
      Recent activity: {{STATE_HISTORY}}
    `,
    llmConfig: {
      model: 'gpt-4',
      temperature: 0.3,
    },
  });

  console.log('  ✓ AI-Assisted routing configured for fraud-check');

  // Pattern 3: Distributed Workflows
  const distributed = new DistributedWorkflow(yawlEngine, federation, cluster, deltaGate);

  console.log('  ✓ Distributed workflow configured (5 nodes)\n');

  // -------------------------------------------------------------------------
  // Define Order Processing Workflow
  // -------------------------------------------------------------------------

  console.log('[Workflow] Defining order processing workflow...\n');

  const workflowSpec = {
    id: 'order-processing-v1',
    name: 'E-Commerce Order Processing',
    tasks: [
      { id: 'submit-order', name: 'Submit Order' },
      { id: 'validate-inventory', name: 'Validate Inventory' },
      // Parallel payment tasks (distributed)
      { id: 'card-validation', name: 'Card Validation' },
      { id: 'fraud-check', name: 'Fraud Check' }, // AI-assisted
      { id: 'balance-verification', name: 'Balance Verification' },
      { id: 'tax-calculation', name: 'Tax Calculation' },
      { id: 'fee-calculation', name: 'Fee Calculation' },
      // Post-payment
      { id: 'aggregate-results', name: 'Aggregate Payment Results' },
      { id: 'payment-processing', name: 'Process Payment' }, // Self-healing
      { id: 'manual-review', name: 'Manual Review' },
      { id: 'automated-review', name: 'Automated Review' },
      { id: 'manual-payment-review', name: 'Manual Payment Review' },
      { id: 'ship-order', name: 'Ship Order' },
    ],
  };

  console.log(`  Workflow: ${workflowSpec.name}`);
  console.log(`  Total Tasks: ${workflowSpec.tasks.length}\n`);

  // -------------------------------------------------------------------------
  // Execute Order Processing
  // -------------------------------------------------------------------------

  console.log('[Execution] Processing sample order...\n');

  const caseId = `order-${Date.now()}`;
  const orderData = {
    orderId: 'ORD-12345',
    customerId: 'CUST-67890',
    amount: 149.99,
    items: [
      { sku: 'BOOK-001', quantity: 2, price: 49.99 },
      { sku: 'BOOK-002', quantity: 1, price: 50.01 },
    ],
  };

  // Step 1: Create case
  console.log('Step 1: Creating order case...');
  await yawlEngine.createCase({
    caseId,
    workflowId: workflowSpec.id,
    inputData: orderData,
  });
  console.log(`  ✓ Case created: ${caseId}\n`);

  // Step 2: Submit order
  console.log('Step 2: Submitting order...');
  await yawlEngine.enableTask({ caseId, taskId: 'submit-order' });
  await yawlEngine.completeTask({
    caseId,
    taskId: 'submit-order',
    outputData: { submitted: true },
  });
  console.log('  ✓ Order submitted\n');

  // Step 3: Validate inventory
  console.log('Step 3: Validating inventory...');
  await yawlEngine.enableTask({ caseId, taskId: 'validate-inventory' });
  await yawlEngine.completeTask({
    caseId,
    taskId: 'validate-inventory',
    outputData: { available: true },
  });
  console.log('  ✓ Inventory validated\n');

  // Step 4: Distributed parallel payment tasks
  console.log('Step 4: Distributing parallel payment tasks (5 tasks)...');

  const paymentTasks = [
    'card-validation',
    'fraud-check',
    'balance-verification',
    'tax-calculation',
    'fee-calculation',
  ];

  const distributionResult = await distributed.distributeParallelTasks(caseId, paymentTasks, {
    strategy: 'least-loaded',
    consensusRequired: true,
  });

  console.log(
    `  ✓ Distributed to ${distributionResult.results.length} nodes in ${distributionResult.metrics.latency}ms`
  );
  console.log(`  Throughput: ${distributionResult.metrics.throughput.toFixed(1)} tasks/sec`);
  console.log(`  Consensus: ${distributionResult.consensusProof ? 'ACHIEVED' : 'SKIPPED'}\n`);

  // Step 5: Complete payment tasks and trigger AI routing
  console.log('Step 5: Completing payment tasks...');

  // Simulate fraud check completion with suspicious data
  await yawlEngine.completeTask({
    caseId,
    taskId: 'fraud-check',
    outputData: {
      fraudScore: 0.75,
      indicators: ['unusual-location', 'high-velocity'],
      recommendation: 'manual-review',
    },
  });

  // AI router automatically triggers on fraud-check completion
  console.log('  ✓ Fraud check completed (triggering AI routing)');

  // Wait for AI routing decision
  await new Promise((resolve) => setTimeout(resolve, 600));
  console.log('  ✓ AI routing decision completed\n');

  // Step 6: Simulate payment failure (triggers self-healing)
  console.log('Step 6: Processing payment (simulating failure)...');

  await yawlEngine.enableTask({ caseId, taskId: 'payment-processing' });

  // First attempt fails
  await yawlEngine.failTask({
    caseId,
    taskId: 'payment-processing',
    error: 'Payment gateway timeout',
  });

  console.log('  ✗ Payment failed (attempt 1/3)');
  console.log('  → Self-healing: Scheduling retry in 1000ms...\n');

  // Wait for self-healing retry
  await new Promise((resolve) => setTimeout(resolve, 1500));

  console.log('  ✓ Self-healing retry succeeded (attempt 2/3)\n');

  // Step 7: Complete workflow
  console.log('Step 7: Shipping order...');
  await yawlEngine.enableTask({ caseId, taskId: 'ship-order' });
  await yawlEngine.completeTask({
    caseId,
    taskId: 'ship-order',
    outputData: { trackingNumber: 'TRACK-12345' },
  });
  console.log('  ✓ Order shipped\n');

  // -------------------------------------------------------------------------
  // Display Metrics
  // -------------------------------------------------------------------------

  console.log('=== Performance Metrics ===\n');

  const healingMetrics = selfHealing.getMetrics();
  console.log('Self-Healing:');
  console.log(`  Total Retries: ${healingMetrics.totalRetries}`);
  console.log(`  Successful Recoveries: ${healingMetrics.successfulRecoveries}`);
  console.log(`  Success Rate: ${healingMetrics.successRate.toFixed(1)}%`);
  console.log(`  Active Policies: ${healingMetrics.activePolicies}\n`);

  const aiMetrics = aiRouter.getMetrics();
  console.log('AI-Assisted Routing:');
  console.log(`  Total Decisions: ${aiMetrics.totalDecisions}`);
  console.log(`  Successful Routes: ${aiMetrics.successfulRoutes}`);
  console.log(`  Success Rate: ${aiMetrics.successRate.toFixed(1)}%`);
  console.log(`  Avg Confidence: ${aiMetrics.avgConfidence.toFixed(2)}`);
  console.log(`  Avg Latency: ${aiMetrics.avgLatency.toFixed(0)}ms\n`);

  const distMetrics = distributed.getMetrics();
  console.log('Distributed Workflows:');
  console.log(`  Total Distributions: ${distMetrics.totalDistributions}`);
  console.log(`  Successful: ${distMetrics.successfulDistributions}`);
  console.log(`  Success Rate: ${distMetrics.successRate.toFixed(1)}%`);
  console.log(`  Avg Latency: ${distMetrics.avgLatency.toFixed(0)}ms`);
  console.log(`  Avg Throughput: ${distMetrics.avgThroughput.toFixed(1)} tasks/sec\n`);

  // -------------------------------------------------------------------------
  // Cleanup
  // -------------------------------------------------------------------------

  console.log('[Cleanup] Stopping infrastructure...');
  await mainDaemon.stop();
  await cluster.stopAll();
  console.log('✓ Infrastructure stopped\n');

  console.log('=== Example Completed Successfully ===');
}

// =============================================================================
// Run Example
// =============================================================================

// Execute the complete example
runCompleteExample().catch((error) => {
  console.error('Example failed:', error);
  process.exit(1);
});
