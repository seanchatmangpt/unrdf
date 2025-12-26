/**
 * @file Basic Usage Example - YAWL Observability Framework
 *
 * Demonstrates complete setup of metrics, tracing, and SLI monitoring
 * for a YAWL workflow engine.
 */

import { createWorkflowEngine } from '@unrdf/yawl';
import {
  YAWLMetricsCollector,
  YAWLTracer,
  YAWLSLICalculator,
} from '../index.mjs';

// =============================================================================
// 1. Create YAWL Engine
// =============================================================================

const engine = createWorkflowEngine({
  nodeId: 'demo-node-1',
  enableEventLog: false, // Disable KGC-4D for this example
});

// =============================================================================
// 2. Setup Observability
// =============================================================================

// Prometheus metrics collector
const metrics = new YAWLMetricsCollector(engine, {
  prefix: 'demo_yawl',
  defaultLabels: {
    environment: 'development',
    region: 'local',
    version: '1.0.0',
  },
});

// OpenTelemetry distributed tracing
const tracer = new YAWLTracer(engine, {
  tracerName: '@unrdf/yawl-demo',
  includeReceiptHashes: true,
  includeTaskData: true,
  maxTaskDataSize: 500,
});

// SLI calculator
const sli = new YAWLSLICalculator(engine, {
  windowMs: 60000, // 1 minute window
  targetCompletionRate: 0.95,
  targetTaskSuccessRate: 0.99,
  targetP95Latency: 2.0,
  targetResourceUtilization: 0.80,
});

// =============================================================================
// 3. Define a Sample Workflow
// =============================================================================

const approvalWorkflow = {
  id: 'approval-workflow',
  name: 'Purchase Approval',
  startTaskId: 'start',
  tasks: [
    {
      id: 'start',
      name: 'Start',
      kind: 'EmptyTask',
      outputConditions: ['c1'],
      splitType: 'sequence',
    },
    {
      id: 'review',
      name: 'Review Request',
      kind: 'AtomicTask',
      inputConditions: ['c1'],
      outputConditions: ['c2'],
      splitType: 'xor', // XOR split based on approval decision
      timeout: 30000, // 30 seconds
    },
    {
      id: 'approve',
      name: 'Approve',
      kind: 'AtomicTask',
      inputConditions: ['c2'],
      outputConditions: ['c3'],
    },
    {
      id: 'reject',
      name: 'Reject',
      kind: 'AtomicTask',
      inputConditions: ['c2'],
      outputConditions: ['c3'],
    },
    {
      id: 'end',
      name: 'End',
      kind: 'EmptyTask',
      inputConditions: ['c3'],
      joinType: 'xor', // XOR join from approve/reject
    },
  ],
  conditions: [
    { id: 'c1', evaluation: () => true },
    { id: 'c2', evaluation: (data) => data.approved === true },
    { id: 'c3', evaluation: () => true },
  ],
};

engine.registerWorkflow(approvalWorkflow);

// =============================================================================
// 4. Execute Sample Cases
// =============================================================================

async function executeSampleCases() {
  console.log('Executing sample workflow cases...\n');

  // Create and execute 5 cases
  for (let i = 0; i < 5; i++) {
    const { case: yawlCase } = await engine.createCase('approval-workflow', {
      requestId: `REQ-${i + 1}`,
      amount: 1000 * (i + 1),
    });

    console.log(`Case ${yawlCase.id} created`);

    // Get the start work item
    const startWorkItem = Array.from(yawlCase.workItems.values())[0];

    // Start and complete the start task
    await engine.startTask(yawlCase.id, startWorkItem.id);
    const { downstreamEnabled } = await engine.completeTask(
      yawlCase.id,
      startWorkItem.id,
      {}
    );

    // Start review task
    const reviewWorkItem = downstreamEnabled[0];
    await engine.startTask(yawlCase.id, reviewWorkItem.workItemId);

    // Simulate some processing time
    await new Promise((resolve) => setTimeout(resolve, 100 + Math.random() * 200));

    // Complete review (approve 80% of cases)
    const approved = i < 4;
    const reviewResult = await engine.completeTask(
      yawlCase.id,
      reviewWorkItem.workItemId,
      { approved }
    );

    // Start and complete approve/reject task
    const decisionWorkItem = reviewResult.downstreamEnabled[0];
    await engine.startTask(yawlCase.id, decisionWorkItem.workItemId);

    await new Promise((resolve) => setTimeout(resolve, 50 + Math.random() * 100));

    await engine.completeTask(yawlCase.id, decisionWorkItem.workItemId, {
      decision: approved ? 'APPROVED' : 'REJECTED',
    });

    console.log(`Case ${yawlCase.id} ${approved ? 'approved' : 'rejected'}`);
  }

  console.log('\n✅ All cases completed!\n');
}

// =============================================================================
// 5. Display Metrics and SLIs
// =============================================================================

async function displayObservability() {
  console.log('='.repeat(80));
  console.log('PROMETHEUS METRICS');
  console.log('='.repeat(80));
  const promMetrics = await metrics.getMetrics();
  console.log(promMetrics);

  console.log('\n' + '='.repeat(80));
  console.log('SLI SNAPSHOT');
  console.log('='.repeat(80));
  const snapshot = sli.getSnapshot();
  console.log(JSON.stringify(snapshot, null, 2));

  console.log('\n' + '='.repeat(80));
  console.log('SLO COMPLIANCE REPORT');
  console.log('='.repeat(80));
  const sloReport = sli.getSLOReport();
  console.log(JSON.stringify(sloReport, null, 2));

  console.log('\n' + '='.repeat(80));
  console.log('EXAMPLE QUERIES FOR GRAFANA');
  console.log('='.repeat(80));
  console.log(`
# Completion rate by workflow
rate(demo_yawl_workflow_cases_total{status="completed"}[5m])
  / rate(demo_yawl_workflow_cases_total{status="created"}[5m])

# Task p95 latency
histogram_quantile(0.95, rate(demo_yawl_task_duration_seconds_bucket[5m]))

# Pattern usage distribution
sum by (pattern_type, operation) (demo_yawl_pattern_usage_count)

# Resource utilization
avg(demo_yawl_resource_utilization)

# Task error rate by type
rate(demo_yawl_task_errors_total[5m])
  `);
}

// =============================================================================
// 6. Run Example
// =============================================================================

async function main() {
  try {
    await executeSampleCases();
    await displayObservability();

    console.log('\n✅ Example completed successfully!');
    console.log('\nTo visualize metrics in Grafana:');
    console.log('1. Import the dashboard: src/examples/grafana-dashboard.json');
    console.log('2. Configure Prometheus datasource');
    console.log('3. Point to your metrics endpoint');
  } catch (error) {
    console.error('Error:', error);
  } finally {
    // Cleanup
    metrics.destroy();
    tracer.destroy();
    sli.destroy();
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { engine, metrics, tracer, sli };
