/**
 * @file Distributed Data Pipeline Example
 * @description
 * Demonstrates YAWL-Queue with a distributed ETL pipeline across 5 workers.
 *
 * Pipeline stages:
 * 1. Ingest - Load raw data
 * 2. Validate - Check data quality
 * 3. Transform - Apply transformations (parallel 3 workers)
 * 4. Aggregate - Combine results
 * 5. Store - Persist final data
 *
 * @example
 * node src/examples/data-pipeline.mjs
 */

import { YAWLQueueAdapter } from '../adapter.mjs';
import { createWorkflow } from '@unrdf/yawl';

// =============================================================================
// Pipeline Workflow Definition
// =============================================================================

/**
 * Create ETL pipeline workflow
 */
function createETLPipeline() {
  return createWorkflow({
    id: 'etl-pipeline',
    name: 'Distributed ETL Data Pipeline',
    version: '1.0.0',
    tasks: [
      {
        id: 'ingest',
        name: 'Ingest Raw Data',
        kind: 'AtomicTask',
        splitType: 'sequence',
        joinType: 'sequence',
        timeout: 10000,
        priority: 10,
      },
      {
        id: 'validate',
        name: 'Validate Data Quality',
        kind: 'AtomicTask',
        splitType: 'and', // Split to multiple transform workers
        joinType: 'sequence',
        timeout: 5000,
        priority: 8,
      },
      {
        id: 'transform-1',
        name: 'Transform Data Batch 1',
        kind: 'AtomicTask',
        splitType: 'sequence',
        joinType: 'and', // Join at aggregation
        timeout: 15000,
        priority: 5,
        cancellationRegion: 'transform-region',
      },
      {
        id: 'transform-2',
        name: 'Transform Data Batch 2',
        kind: 'AtomicTask',
        splitType: 'sequence',
        joinType: 'and',
        timeout: 15000,
        priority: 5,
        cancellationRegion: 'transform-region',
      },
      {
        id: 'transform-3',
        name: 'Transform Data Batch 3',
        kind: 'AtomicTask',
        splitType: 'sequence',
        joinType: 'and',
        timeout: 15000,
        priority: 5,
        cancellationRegion: 'transform-region',
      },
      {
        id: 'aggregate',
        name: 'Aggregate Transformed Data',
        kind: 'AtomicTask',
        splitType: 'sequence',
        joinType: 'and', // Wait for all transforms
        timeout: 8000,
        priority: 7,
      },
      {
        id: 'store',
        name: 'Store Final Data',
        kind: 'AtomicTask',
        splitType: 'sequence',
        joinType: 'sequence',
        timeout: 5000,
        priority: 9,
      },
    ],
    flows: [
      { from: 'ingest', to: 'validate' },
      { from: 'validate', to: 'transform-1' },
      { from: 'validate', to: 'transform-2' },
      { from: 'validate', to: 'transform-3' },
      { from: 'transform-1', to: 'aggregate' },
      { from: 'transform-2', to: 'aggregate' },
      { from: 'transform-3', to: 'aggregate' },
      { from: 'aggregate', to: 'store' },
    ],
  });
}

// =============================================================================
// Task Handlers
// =============================================================================

/**
 * Custom task execution handlers for each pipeline stage
 */
const taskHandlers = {
  async ingest(job, task) {
    console.log(`\n[Worker ${process.pid}] INGEST: Loading raw data...`);
    await sleep(500);

    const records = [
      { id: 1, value: 100, type: 'A' },
      { id: 2, value: 200, type: 'B' },
      { id: 3, value: 300, type: 'A' },
      { id: 4, value: 400, type: 'C' },
      { id: 5, value: 500, type: 'B' },
      { id: 6, value: 600, type: 'A' },
    ];

    console.log(`[Worker ${process.pid}] INGEST: Loaded ${records.length} records`);
    return { records, timestamp: Date.now() };
  },

  async validate(job, task) {
    console.log(`\n[Worker ${process.pid}] VALIDATE: Checking data quality...`);
    await sleep(300);

    const input = job.data.input || {};
    const records = input.records || [];

    // Filter invalid records
    const validRecords = records.filter(r => r.value > 0 && r.type);

    console.log(`[Worker ${process.pid}] VALIDATE: ${validRecords.length}/${records.length} valid`);

    // Split into batches for parallel processing
    const batchSize = Math.ceil(validRecords.length / 3);
    return {
      batch1: validRecords.slice(0, batchSize),
      batch2: validRecords.slice(batchSize, batchSize * 2),
      batch3: validRecords.slice(batchSize * 2),
      totalRecords: validRecords.length,
    };
  },

  async 'transform-1'(job, task) {
    return transformBatch(job, task, 1);
  },

  async 'transform-2'(job, task) {
    return transformBatch(job, task, 2);
  },

  async 'transform-3'(job, task) {
    return transformBatch(job, task, 3);
  },

  async aggregate(job, task) {
    console.log(`\n[Worker ${process.pid}] AGGREGATE: Combining transformed batches...`);
    await sleep(400);

    // In real implementation, this would fetch outputs from all transform tasks
    // For demo, we simulate aggregated results
    const aggregated = {
      totalRecords: 6,
      sumByType: { A: 1000, B: 700, C: 400 },
      processingTime: Date.now(),
    };

    console.log(`[Worker ${process.pid}] AGGREGATE: Combined ${aggregated.totalRecords} records`);
    return aggregated;
  },

  async store(job, task) {
    console.log(`\n[Worker ${process.pid}] STORE: Persisting final data...`);
    await sleep(300);

    const input = job.data.input || {};
    console.log(`[Worker ${process.pid}] STORE: Saved aggregated data:`, input);

    return {
      success: true,
      stored: true,
      timestamp: Date.now(),
    };
  },
};

/**
 * Transform a batch of records
 */
async function transformBatch(job, task, batchNum) {
  console.log(`\n[Worker ${process.pid}] TRANSFORM-${batchNum}: Processing batch ${batchNum}...`);
  await sleep(800 + Math.random() * 400); // Simulate varying processing time

  const input = job.data.input || {};
  const batch = input[`batch${batchNum}`] || [];

  // Apply transformation: multiply values by 2
  const transformed = batch.map(record => ({
    ...record,
    value: record.value * 2,
    transformed: true,
    batch: batchNum,
  }));

  console.log(`[Worker ${process.pid}] TRANSFORM-${batchNum}: Processed ${transformed.length} records`);
  return { [`batch${batchNum}_result`]: transformed };
}

/**
 * Sleep utility
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// =============================================================================
// Main Example
// =============================================================================

async function main() {
  console.log('='.repeat(80));
  console.log('YAWL-Queue: Distributed ETL Data Pipeline');
  console.log('='.repeat(80));

  // Create adapter
  const adapter = new YAWLQueueAdapter({
    redis: { host: 'localhost', port: 6379 },
    queueName: 'etl-pipeline',
    defaultJobOptions: {
      attempts: 3,
      backoff: {
        type: 'exponential',
        delay: 1000,
      },
    },
  });

  // Create and register workflow
  const workflow = createETLPipeline();
  await adapter.registerWorkflow(workflow);
  console.log(`\nRegistered workflow: ${workflow.id} (${workflow.getTasks().length} tasks)`);

  // Create custom task handler that routes to specific handlers
  const customHandler = async (job, task) => {
    const taskId = task.taskDefId || task.id;
    const handler = taskHandlers[taskId];

    if (handler) {
      return await handler(job, task);
    }

    console.log(`[Worker ${process.pid}] No handler for task: ${taskId}`);
    return { processed: true };
  };

  // Create 5 workers
  console.log('\nCreating 5 workers...');
  const workers = [];
  for (let i = 0; i < 5; i++) {
    const worker = adapter.createWorker({
      concurrency: 1,
      taskHandler: customHandler,
    });
    workers.push(worker);
  }
  console.log(`Created ${workers.length} workers`);

  // Execute pipeline
  console.log('\n' + '='.repeat(80));
  console.log('Executing ETL Pipeline...');
  console.log('='.repeat(80));

  const { caseId, jobId } = await adapter.executeCase('etl-pipeline', {
    source: 'example-database',
    batchSize: 1000,
  });

  console.log(`\nCase created: ${caseId}`);
  console.log(`Initial job queued: ${jobId}\n`);

  // Monitor case status
  const statusInterval = setInterval(async () => {
    try {
      const status = await adapter.getCaseStatus(caseId);
      console.log(`\n[Status] Case: ${status.caseId}`);
      console.log(`  Status: ${status.status}`);
      console.log(`  Enabled: ${status.enabledTasks} | Active: ${status.activeTasks} | Completed: ${status.completedTasks}`);
      console.log(`  Receipts: ${status.receipts}`);

      // Stop when case completes
      if (status.status === 'completed') {
        clearInterval(statusInterval);

        console.log('\n' + '='.repeat(80));
        console.log('Pipeline Completed Successfully!');
        console.log('='.repeat(80));

        // Show final stats
        const stats = await adapter.getStats();
        console.log('\nFinal Statistics:');
        console.log(JSON.stringify(stats, null, 2));

        // Cleanup
        console.log('\nCleaning up...');
        await adapter.close();
        console.log('Done!');
        process.exit(0);
      }
    } catch (error) {
      console.error('Status check error:', error.message);
    }
  }, 2000);

  // Timeout after 60 seconds
  setTimeout(async () => {
    clearInterval(statusInterval);
    console.error('\n\nPipeline timed out after 60 seconds');
    await adapter.close();
    process.exit(1);
  }, 60000);
}

// Run example
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('Pipeline error:', error);
    process.exit(1);
  });
}

export { createETLPipeline, taskHandlers };
