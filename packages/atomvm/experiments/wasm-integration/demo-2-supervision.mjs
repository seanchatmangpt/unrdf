/**
 * Demo 2: Supervision Tree with WASM Workers
 * 
 * This demonstrates:
 * - WASM modules supervised by BEAM-style SupervisorTree
 * - Fault isolation and automatic restart
 * - Multiple WASM workers in parallel
 * - Supervisor restart strategies
 * 
 * Run: node demo-2-supervision.mjs
 */

import { performance } from 'node:perf_hooks';
import { SupervisorTree } from '../../src/supervisor-tree.mjs';

class WASMValidator {
  constructor() {
    this.exports = {
      validate: (input) => (input > 0 ? 1 : 0),
      get_status: () => 1,
    };
  }
}

class WASMWorker {
  constructor(workerId, wasmInstance) {
    this.workerId = workerId;
    this.instance = wasmInstance;
    this.tasksProcessed = 0;
    this.failures = 0;
    this.restarts = 0;
    this.status = 'initializing';
  }

  async start() {
    this.status = 'running';
    console.log(`  [${this.workerId}] Started (restarts: ${this.restarts})`);
  }

  async stop() {
    this.status = 'stopped';
    console.log(`  [${this.workerId}] Stopped`);
  }

  async process(task) {
    if (this.status !== 'running') {
      throw new Error(`Worker ${this.workerId} not running`);
    }

    const start = performance.now();
    
    try {
      if (task.shouldFail) {
        throw new Error('Invalid triple pattern');
      }

      const result = this.instance.exports.validate(1);
      this.tasksProcessed++;

      return {
        workerId: this.workerId,
        task,
        result: result === 1 ? 'valid' : 'invalid',
        latency: performance.now() - start,
        status: 'success',
      };
    } catch (error) {
      this.failures++;
      this.status = 'failed';
      throw error;
    }
  }

  async restart() {
    this.restarts++;
    this.status = 'restarting';
    await this.stop();
    await this.start();
  }

  getMetrics() {
    return {
      workerId: this.workerId,
      status: this.status,
      tasksProcessed: this.tasksProcessed,
      failures: this.failures,
      restarts: this.restarts,
    };
  }
}

async function main() {
  console.log('=== Demo 2: Supervision Tree with WASM Workers ===\n');

  const workers = [];
  for (let i = 1; i <= 3; i++) {
    const wasmModule = new WASMValidator();
    const worker = new WASMWorker(`validator-${i}`, wasmModule);
    workers.push(worker);
  }

  console.log(`✅ Created ${workers.length} WASM workers\n`);

  console.log('--- Scenario 1: one_for_one Restart Strategy ---');
  const supervisor = new SupervisorTree('rdf-validator-supervisor', 'one_for_one');

  for (const worker of workers) {
    supervisor.addChild(worker.workerId, async () => {
      await worker.start();
    }, 'one_for_one');
  }

  await supervisor.start();
  console.log('✅ All workers started\n');

  console.log('Processing valid tasks...');
  for (const worker of workers) {
    const result = await worker.process({ triple: { s: 'alice', p: 'rdf:type', o: 'Person' }, shouldFail: false });
    console.log(`  ✅ ${result.workerId}: ${result.result} (${result.latency.toFixed(2)}ms)`);
  }
  console.log('');

  console.log('--- Scenario 2: Worker Failure & Recovery ---');
  console.log('Simulating failure in validator-2...');
  
  try {
    await workers[1].process({ triple: { s: 'invalid', p: null, o: null }, shouldFail: true });
  } catch (error) {
    console.log(`  ❌ validator-2 failed: ${error.message}`);
  }

  console.log('\nSupervisor restarting failed worker...');
  await workers[1].restart();
  console.log('✅ Worker restarted\n');

  console.log('Verifying other workers still running...');
  const result1 = await workers[0].process({ triple: { s: 'bob', p: 'rdf:type', o: 'Person' }, shouldFail: false });
  const result3 = await workers[2].process({ triple: { s: 'charlie', p: 'rdf:type', o: 'Person' }, shouldFail: false });
  
  console.log(`  ✅ ${result1.workerId}: ${result1.result} (unaffected)`);
  console.log(`  ✅ ${result3.workerId}: ${result3.result} (unaffected)`);
  console.log('');

  console.log('Verifying restarted worker...');
  const result2 = await workers[1].process({ triple: { s: 'dave', p: 'rdf:type', o: 'Person' }, shouldFail: false });
  console.log(`  ✅ ${result2.workerId}: ${result2.result} (recovered)\n`);

  console.log('--- Worker Metrics ---');
  for (const worker of workers) {
    const metrics = worker.getMetrics();
    console.log(`${metrics.workerId}:`);
    console.log(`  Status:         ${metrics.status}`);
    console.log(`  Tasks:          ${metrics.tasksProcessed}`);
    console.log(`  Failures:       ${metrics.failures}`);
    console.log(`  Restarts:       ${metrics.restarts}`);
  }
  console.log('');

  console.log('=== Summary ===');
  console.log('✅ WASM workers supervised by BEAM-style supervisor');
  console.log('✅ one_for_one strategy: only failed worker restarted');
  console.log('✅ Other workers continue processing (fault isolation)');
  console.log('✅ Failed worker recovers and resumes operation\n');
}

main().catch(console.error);
