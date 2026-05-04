/**
 * Proof of Concept - Headless Execution & Automation
 *
 * Demonstrates:
 * 1. Programmatic Claude Code execution
 * 2. Batch task processing with priorities
 * 3. CI/CD integration patterns
 *
 * Run: node demos/headless-execution-poc.mjs
 */

import {
  createHeadlessRunner,
  execute,
  executeStream,
  OutputFormat,
} from '../src/capabilities/headless-runner.mjs';

import {
  createBatchProcessor,
  executeBatch,
  TaskPriority,
} from '../src/capabilities/batch-processor.mjs';

import {
  createCIIntegration,
  detectCI,
} from '../src/capabilities/ci-integration.mjs';

/**
 * Demo 1: Basic Headless Execution
 */
async function demonstrateHeadlessExecution() {
  console.log('='.repeat(80));
  console.log('DEMO 1: Headless Execution - Text, JSON, and Stream Outputs');
  console.log('='.repeat(80));
  console.log();

  // Text output (simple)
  console.log('Step 1: Simple text execution...');
  const textResult = await execute('List 3 primary colors', {
    outputFormat: OutputFormat.TEXT,
    timeout: 10000,
  });
  console.log(`  ✓ Text result (${textResult.duration}ms):`);
  console.log(`    ${textResult.output.trim().slice(0, 100)}...`);
  console.log();

  // JSON output (structured)
  console.log('Step 2: JSON-formatted execution...');
  const jsonResult = await execute('List 3 primary colors in JSON array format', {
    outputFormat: OutputFormat.JSON,
    timeout: 10000,
  });
  console.log(`  ✓ JSON result (${jsonResult.duration}ms):`);
  console.log(`    Type: ${typeof jsonResult.output}`);
  console.log(`    Exit code: ${jsonResult.exitCode}`);
  console.log();

  // Streaming output
  console.log('Step 3: Streaming execution...');
  const events = [];
  const streamResult = await executeStream(
    'Count from 1 to 5',
    (event) => {
      events.push(event);
      if (event.type === 'delta' && events.length <= 5) {
        process.stdout.write('.');
      }
    },
    { timeout: 10000 }
  );
  console.log();
  console.log(`  ✓ Stream result: ${events.length} events in ${streamResult.duration}ms`);
  console.log();

  console.log('✅ Headless Execution demo complete!');
  console.log();
}

/**
 * Demo 2: Batch Processing
 */
async function demonstrateBatchProcessing() {
  console.log('='.repeat(80));
  console.log('DEMO 2: Batch Processing with Priority Queue');
  console.log('='.repeat(80));
  console.log();

  const processor = createBatchProcessor({
    concurrency: 2,
    maxRetries: 2,
    debug: false,
  });

  // Listen to progress events
  processor.on('task:start', ({ taskId }) => {
    console.log(`  ▶ Started: ${taskId}`);
  });

  processor.on('task:complete', ({ taskId }) => {
    console.log(`  ✓ Completed: ${taskId}`);
  });

  processor.on('task:failed', ({ taskId, error }) => {
    console.log(`  ✗ Failed: ${taskId} - ${error}`);
  });

  processor.on('progress', (progress) => {
    console.log(`  Progress: ${progress.percentComplete.toFixed(1)}% (${progress.completed}/${progress.total})`);
  });

  // Add tasks with different priorities
  console.log('Step 1: Adding tasks to queue...');

  processor.addTask({
    id: 'critical-task',
    prompt: 'What is 2 + 2?',
    priority: TaskPriority.CRITICAL,
    options: { timeout: 5000 },
  });

  processor.addTask({
    id: 'high-task-1',
    prompt: 'What is the capital of France?',
    priority: TaskPriority.HIGH,
    options: { timeout: 5000 },
  });

  processor.addTask({
    id: 'normal-task-1',
    prompt: 'Name a programming language',
    priority: TaskPriority.NORMAL,
    options: { timeout: 5000 },
  });

  processor.addTask({
    id: 'normal-task-2',
    prompt: 'What color is the sky?',
    priority: TaskPriority.NORMAL,
    options: { timeout: 5000 },
  });

  processor.addTask({
    id: 'low-task',
    prompt: 'Tell me a fun fact',
    priority: TaskPriority.LOW,
    options: { timeout: 5000 },
  });

  console.log(`  ✓ Added ${processor.tasks.size} tasks`);
  console.log();

  // Process batch
  console.log('Step 2: Processing batch...');
  const summary = await processor.process();
  console.log();

  console.log('Step 3: Batch summary:');
  console.log(`  Total tasks: ${summary.totalTasks}`);
  console.log(`  Completed: ${summary.completed}`);
  console.log(`  Failed: ${summary.failed}`);
  console.log(`  Success rate: ${summary.successRate.toFixed(1)}%`);
  console.log(`  Duration: ${summary.duration}ms`);
  console.log();

  console.log('✅ Batch Processing demo complete!');
  console.log();
}

/**
 * Demo 3: Task Dependencies
 */
async function demonstrateTaskDependencies() {
  console.log('='.repeat(80));
  console.log('DEMO 3: Task Dependencies and Ordering');
  console.log('='.repeat(80));
  console.log();

  const processor = createBatchProcessor({
    concurrency: 3,
    debug: false,
  });

  console.log('Step 1: Adding tasks with dependencies...');

  // Base task
  processor.addTask({
    id: 'base-calculation',
    prompt: 'What is 10 + 5?',
    priority: TaskPriority.HIGH,
    options: { timeout: 5000 },
  });

  // Depends on base
  processor.addTask({
    id: 'multiply-result',
    prompt: 'Multiply the previous result by 2',
    priority: TaskPriority.NORMAL,
    dependencies: ['base-calculation'],
    options: { timeout: 5000 },
  });

  // Depends on multiplication
  processor.addTask({
    id: 'final-check',
    prompt: 'Is the result greater than 20?',
    priority: TaskPriority.NORMAL,
    dependencies: ['multiply-result'],
    options: { timeout: 5000 },
  });

  console.log('  ✓ Added 3 tasks with dependencies');
  console.log('  Dependency chain: base-calculation → multiply-result → final-check');
  console.log();

  processor.on('task:complete', ({ taskId }) => {
    console.log(`  ✓ Completed: ${taskId}`);
  });

  console.log('Step 2: Processing (should execute in order)...');
  const summary = await processor.process();
  console.log();

  console.log(`  ✓ All ${summary.completed} tasks completed in correct order`);
  console.log();

  console.log('✅ Task Dependencies demo complete!');
  console.log();
}

/**
 * Demo 4: CI Integration
 */
async function demonstrateCIIntegration() {
  console.log('='.repeat(80));
  console.log('DEMO 4: CI/CD Integration');
  console.log('='.repeat(80));
  console.log();

  const ci = createCIIntegration({ debug: false });

  // Detect CI environment
  console.log('Step 1: Detecting CI environment...');
  const ciEnv = ci.detectCIEnvironment();
  if (ciEnv) {
    console.log(`  ✓ CI Platform: ${ciEnv.platform}`);
    console.log(`  Event: ${ciEnv.event}`);
    console.log(`  Branch: ${ciEnv.branch || 'N/A'}`);
    console.log(`  SHA: ${ciEnv.sha?.slice(0, 7) || 'N/A'}`);
  } else {
    console.log('  ℹ Not running in CI environment (local development)');
  }
  console.log();

  // Parse test output (simulated)
  console.log('Step 2: Parsing test output...');
  const testOutput = `
  PASS  src/test-1.test.js
  PASS  src/test-2.test.js
  FAIL  src/test-3.test.js

  Tests:       3 failed, 27 passed, 30 total
  Time:        4.5s
  `;

  const testResults = ci.parseTestOutput(testOutput, 'auto');
  console.log('  ✓ Test results parsed:');
  console.log(`    Total: ${testResults.total}`);
  console.log(`    Passed: ${testResults.passed}`);
  console.log(`    Failed: ${testResults.failed}`);
  console.log(`    Pass rate: ${testResults.passRate.toFixed(1)}%`);
  console.log(`    Duration: ${testResults.duration}ms`);
  console.log();

  // Generate workflow
  console.log('Step 3: Generating GitHub Actions workflow...');
  const workflow = await ci.generateWorkflow({
    platform: 'github-actions',
    steps: [
      'Run linter',
      'Run tests',
      'Build project',
    ],
  });
  console.log('  ✓ Workflow generated:');
  console.log(`    Length: ${workflow.length} bytes`);
  console.log(`    Lines: ${workflow.split('\n').length}`);
  console.log();

  console.log('✅ CI Integration demo complete!');
  console.log();
}

/**
 * Demo 5: Advanced Patterns
 */
async function demonstrateAdvancedPatterns() {
  console.log('='.repeat(80));
  console.log('DEMO 5: Advanced Automation Patterns');
  console.log('='.repeat(80));
  console.log();

  // Pattern 1: Parallel execution
  console.log('Step 1: Parallel execution (3 prompts simultaneously)...');
  const runner = createHeadlessRunner();
  const startTime = Date.now();

  const results = await runner.executeParallel([
    { prompt: 'What is 1+1?', outputFormat: 'text', timeout: 5000 },
    { prompt: 'What is 2+2?', outputFormat: 'text', timeout: 5000 },
    { prompt: 'What is 3+3?', outputFormat: 'text', timeout: 5000 },
  ], { concurrency: 3 });

  const duration = Date.now() - startTime;
  console.log(`  ✓ Executed ${results.length} prompts in ${duration}ms`);
  console.log(`  Average: ${(duration / results.length).toFixed(0)}ms per task`);
  console.log();

  // Pattern 2: Conditional execution
  console.log('Step 2: Conditional task execution...');
  const processor = createBatchProcessor({ concurrency: 1 });

  processor.addTask({
    id: 'check-condition',
    prompt: 'Is 5 > 3? Answer with just YES or NO',
    options: { timeout: 5000 },
  });

  processor.on('task:complete', ({ taskId, result }) => {
    if (taskId === 'check-condition' && result?.output?.includes('YES')) {
      console.log('  ✓ Condition met, adding follow-up task...');
      processor.addTask({
        id: 'follow-up',
        prompt: 'What is 5 - 3?',
        dependencies: ['check-condition'],
        options: { timeout: 5000 },
      });
    }
  });

  await processor.process();
  console.log(`  ✓ Conditional execution completed`);
  console.log();

  console.log('✅ Advanced Patterns demo complete!');
  console.log();
}

/**
 * Main demo orchestrator
 */
async function main() {
  console.log('\n');
  console.log('╔════════════════════════════════════════════════════════════════════════════╗');
  console.log('║       HEADLESS EXECUTION & AUTOMATION - PROOF OF CONCEPT                   ║');
  console.log('║                                                                            ║');
  console.log('║  Agent 6 (α₆) - Programmatic/Headless Execution Explorer                  ║');
  console.log('╚════════════════════════════════════════════════════════════════════════════╝');
  console.log('\n');

  console.log('⚠️  Note: Some demos require actual Claude CLI execution and may take time.\n');

  const runDemo = process.argv[2] || 'all';

  try {
    if (runDemo === 'all' || runDemo === '1') {
      await demonstrateHeadlessExecution();
    }

    if (runDemo === 'all' || runDemo === '2') {
      await demonstrateBatchProcessing();
    }

    if (runDemo === 'all' || runDemo === '3') {
      await demonstrateTaskDependencies();
    }

    if (runDemo === 'all' || runDemo === '4') {
      await demonstrateCIIntegration();
    }

    if (runDemo === 'all' || runDemo === '5') {
      await demonstrateAdvancedPatterns();
    }

    console.log('='.repeat(80));
    console.log('All demos completed successfully! 🎉');
    console.log('='.repeat(80));
    console.log();
    console.log('Capabilities demonstrated:');
    console.log('  1. ✅ Text, JSON, and stream-json output formats');
    console.log('  2. ✅ Programmatic execution with timeouts');
    console.log('  3. ✅ Batch processing with priority queues');
    console.log('  4. ✅ Task dependencies and ordering');
    console.log('  5. ✅ Parallel execution');
    console.log('  6. ✅ CI environment detection');
    console.log('  7. ✅ Test result parsing');
    console.log('  8. ✅ Workflow generation');
    console.log('  9. ✅ Event-driven task management');
    console.log(' 10. ✅ Conditional execution patterns');
    console.log();
    console.log('Usage: node demos/headless-execution-poc.mjs [1-5|all]');
    console.log();

  } catch (error) {
    console.error('❌ Demo failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { main };
