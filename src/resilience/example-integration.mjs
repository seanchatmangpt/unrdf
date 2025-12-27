/**
 * @fileoverview Integration Example - Resilience Module Usage
 *
 * **Demonstrates**:
 * - Per-agent circuit breakers with fallback
 * - Retry with exponential backoff
 * - Graceful degradation with QoS levels
 * - Checkpoint-based error recovery
 * - Receipt chain repair
 * - Structured error logging
 *
 * @module resilience/example-integration
 */

import {
  createAgentCircuitBreaker,
  executeWithCircuitBreaker,
  RetryStrategy,
  GracefulDegradationManager,
  QoSLevel,
  FallbackStrategies,
  ErrorRecoveryManager,
  RecoveryStrategy,
  ReceiptChainRepairManager,
  RepairStrategy,
  logError,
  ErrorCategory,
  ErrorSeverity,
} from './index.mjs';

/**
 * Example 1: Agent Circuit Breaker
 * Isolate agent failures to prevent cascading system failures
 */
export async function exampleCircuitBreaker() {
  console.log('\n=== Example 1: Agent Circuit Breaker ===\n');

  // Create circuit breaker for agent
  const breaker = createAgentCircuitBreaker('agent-001', 'coder', {
    failureThreshold: 3,
    resetTimeout: 5000,
  });

  // Simulate agent operations
  for (let i = 0; i < 5; i++) {
    try {
      const result = await breaker.execute(async () => {
        // Simulate 60% failure rate
        if (Math.random() < 0.6) {
          throw new Error('Agent processing failed');
        }
        return { taskCompleted: true, iteration: i };
      });

      console.log(`✅ Iteration ${i}: Success`, result);
    } catch (error) {
      if (error.name === 'CircuitOpenError') {
        console.log(`⚠️ Iteration ${i}: Circuit OPEN - Using fallback`);
      } else {
        console.log(`❌ Iteration ${i}: Failed - ${error.message}`);
      }
    }
  }

  console.log('\nCircuit Status:', breaker.getStatus());
}

/**
 * Example 2: Retry with Exponential Backoff
 * Intelligently retry transient failures
 */
export async function exampleRetryStrategy() {
  console.log('\n=== Example 2: Retry with Exponential Backoff ===\n');

  const retry = new RetryStrategy({
    maxAttempts: 3,
    initialDelayMs: 100,
    backoffMultiplier: 2,
  });

  let attempts = 0;

  try {
    const result = await retry.execute(async () => {
      attempts++;
      console.log(`Attempt ${attempts}...`);

      if (attempts < 3) {
        throw new Error('ETIMEDOUT: Connection timeout');
      }

      return { success: true, attempts };
    });

    console.log('✅ Success after retries:', result);
  } catch (error) {
    console.log('❌ Failed after all retries:', error.message);
  }

  console.log('\nRetry Metrics:', retry.getMetrics());
}

/**
 * Example 3: Graceful Degradation
 * Handle failures with progressive fallback strategies
 */
export async function exampleGracefulDegradation() {
  console.log('\n=== Example 3: Graceful Degradation ===\n');

  const manager = new GracefulDegradationManager();

  // Define fallback strategies (best to worst)
  const fallbacks = [
    {
      name: 'cache',
      qosLevel: QoSLevel.HIGH,
      fallbackFn: async () => {
        console.log('Trying cache fallback...');
        // Simulate cache hit 50% of the time
        if (Math.random() > 0.5) {
          return { data: 'cached-data', quality: 90 };
        }
        throw new Error('Cache miss');
      },
      qualityMetric: 90,
    },
    {
      name: 'simple-algorithm',
      qosLevel: QoSLevel.MEDIUM,
      fallbackFn: async () => {
        console.log('Using simple algorithm fallback...');
        return { data: 'simple-result', quality: 70 };
      },
      qualityMetric: 70,
    },
    FallbackStrategies.defaultValue({ data: 'default', quality: 0 }),
  ];

  try {
    const result = await manager.executeWithDegradation(
      async () => {
        console.log('Trying primary operation...');
        throw new Error('Primary operation failed');
      },
      fallbacks
    );

    console.log('✅ Result:', result);
    console.log(
      `QoS Level: ${result.qosLevel}, Degraded: ${result.degraded}, Quality: ${result.qualityMetric}%`
    );
  } catch (error) {
    console.log('❌ All strategies failed:', error.message);
  }

  console.log('\nDegradation Metrics:', manager.getMetrics());
}

/**
 * Example 4: Error Recovery with Checkpoints
 * Recover from failures using checkpoints
 */
export async function exampleErrorRecovery() {
  console.log('\n=== Example 4: Error Recovery with Checkpoints ===\n');

  const recovery = new ErrorRecoveryManager();

  // Create initial state
  const initialState = {
    processedItems: [],
    currentIndex: 0,
    totalProcessed: 0,
  };

  // Items to process
  const items = ['item1', 'item2', 'item3', 'item4', 'item5'];

  try {
    const result = await recovery.executeWithRecovery(
      'processing_epoch_001',
      initialState,
      async (state) => {
        console.log('Processing items...');

        for (let i = state.currentIndex; i < items.length; i++) {
          console.log(`Processing: ${items[i]}`);

          // Simulate failure on item3
          if (items[i] === 'item3' && state.totalProcessed === 0) {
            throw new Error('Processing failed on item3');
          }

          state.processedItems.push(items[i]);
          state.currentIndex = i + 1;
          state.totalProcessed++;
        }

        return state;
      },
      {
        retryOnFailure: true,
        maxRetries: 2,
        recoveryStrategy: RecoveryStrategy.RETRY,
      }
    );

    console.log('✅ Processing completed:', result.result);
    console.log(`Recovered: ${result.recovered}, Attempts: ${result.attempts}`);
  } catch (error) {
    console.log('❌ Recovery failed:', error.message);
  }

  console.log('\nRecovery Metrics:', recovery.getMetrics());
}

/**
 * Example 5: Receipt Chain Repair
 * Detect and repair broken receipt chains
 */
export async function exampleReceiptChainRepair() {
  console.log('\n=== Example 5: Receipt Chain Repair ===\n');

  const repair = new ReceiptChainRepairManager();

  // Create a receipt chain with issues
  const chain = {
    receipts: [
      {
        id: 'receipt_001',
        epoch: 'epoch_001',
        beforeHash: null,
        receiptHash: 'hash_abc123',
        verify: async () => true,
      },
      {
        id: 'receipt_002',
        epoch: 'epoch_002',
        beforeHash: 'wrong_hash', // Should be 'hash_abc123'
        receiptHash: 'hash_def456',
        verify: async () => true,
      },
      {
        id: 'receipt_003',
        epoch: 'epoch_003',
        beforeHash: 'hash_def456',
        receiptHash: 'hash_ghi789',
        verify: async () => true,
      },
    ],
    getAll: function () {
      return this.receipts;
    },
    get length() {
      return this.receipts.length;
    },
  };

  console.log('Detecting issues in chain...');
  const issues = await repair.detectIssues(chain);

  console.log(`Found ${issues.length} issue(s):`);
  issues.forEach((issue) => {
    console.log(`  - [${issue.type}] ${issue.message}`);
  });

  if (issues.length > 0) {
    console.log('\nRepairing chain...');
    const result = await repair.repairChain(chain, RepairStrategy.RE_HASH);

    console.log('✅ Repair result:', result);
    console.log(`Fixed ${result.issuesFixed} issue(s)`);

    // Verify repair
    const verifyIssues = await repair.detectIssues(chain);
    console.log(`\nVerification: ${verifyIssues.length} remaining issue(s)`);
  }

  console.log('\nRepair Metrics:', repair.getMetrics());
}

/**
 * Example 6: Structured Error Logging
 * Log errors with categorization and tracking
 */
export async function exampleStructuredErrorLogging() {
  console.log('\n=== Example 6: Structured Error Logging ===\n');

  // Simulate various errors
  const errors = [
    { error: new Error('ETIMEDOUT: Connection timeout'), category: ErrorCategory.TRANSIENT },
    { error: new Error('Validation failed: invalid input'), category: ErrorCategory.PERMANENT },
    { error: new Error('401 Unauthorized'), category: ErrorCategory.SECURITY },
    { error: new Error('Database connection refused'), category: ErrorCategory.INFRASTRUCTURE },
  ];

  console.log('Logging errors...\n');

  errors.forEach(({ error, category }) => {
    const logged = logError(error, {
      category,
      context: { agentId: 'agent-001', operation: 'processTask' },
    });

    console.log(
      `[${logged.severity.toUpperCase()}] [${logged.category}] ${logged.message}`
    );
  });

  // Get errors by category
  const transientErrors = logError.__self?.getErrorsByCategory?.(ErrorCategory.TRANSIENT) || [];
  console.log(`\nTransient errors: ${transientErrors.length}`);
}

/**
 * Example 7: Complete Integration
 * Combine all resilience features
 */
export async function exampleCompleteIntegration() {
  console.log('\n=== Example 7: Complete Integration ===\n');

  // Setup resilience stack
  const breaker = createAgentCircuitBreaker('agent-complete', 'system-architect');
  const retry = new RetryStrategy({ maxAttempts: 2, initialDelayMs: 50 });
  const degradation = new GracefulDegradationManager();
  const recovery = new ErrorRecoveryManager();

  // Create checkpoint
  const checkpoint = recovery.createCheckpoint('integration_epoch', {
    step: 0,
    data: [],
  });

  console.log(`Checkpoint created: ${checkpoint.id}`);

  // Execute with full resilience
  try {
    const result = await retry.execute(async () => {
      return await executeWithCircuitBreaker(
        'agent-complete',
        'system-architect',
        async () => {
          return await degradation.executeWithDegradation(
            async () => {
              // Primary operation
              console.log('Executing primary operation...');
              return { success: true, data: 'primary-result' };
            },
            [FallbackStrategies.cache(async () => ({ success: true, data: 'cached-result' }))]
          );
        },
        { fallbackStrategy: 'cache' }
      );
    });

    console.log('✅ Complete integration success:', result);
  } catch (error) {
    logError(error, {
      category: ErrorCategory.UNKNOWN,
      severity: ErrorSeverity.ERROR,
      context: { checkpoint: checkpoint.id },
    });

    // Recover from checkpoint
    const recovered = await recovery.recoverFromCheckpoint(checkpoint.id);
    console.log('Recovered from checkpoint:', recovered.checkpoint.epoch);
  }
}

/**
 * Run all examples
 */
export async function runAllExamples() {
  console.log('\n╔════════════════════════════════════════════╗');
  console.log('║  Resilience Module - Integration Examples ║');
  console.log('╚════════════════════════════════════════════╝\n');

  try {
    await exampleCircuitBreaker();
    await exampleRetryStrategy();
    await exampleGracefulDegradation();
    await exampleErrorRecovery();
    await exampleReceiptChainRepair();
    await exampleStructuredErrorLogging();
    await exampleCompleteIntegration();

    console.log('\n✅ All examples completed!\n');
  } catch (error) {
    console.error('\n❌ Example failed:', error);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllExamples().catch(console.error);
}
