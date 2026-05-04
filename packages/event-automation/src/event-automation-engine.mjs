/**
 * @file Event Automation Engine
 * @module @unrdf/event-automation/engine
 * @description Main engine orchestrating delta processing, receipt generation, and policy enforcement
 */

import { EventEmitter } from 'events';
import { EventAutomationConfigSchema, ProcessDeltaResultSchema, ReplayOptionsSchema } from './schemas.mjs';
import { DeltaProcessor } from './delta-processor.mjs';
import { ReceiptTracker } from './receipt-tracker.mjs';
import { PolicyEnforcer } from './policy-enforcer.mjs';

/**
 * Event automation engine for delta processing with receipts and policies
 */
export class EventAutomationEngine extends EventEmitter {
  /**
   * Creates a new event automation engine
   * @param {Object} [config] - Engine configuration
   * @param {string} [config.id='default-automation'] - Engine ID
   * @param {boolean} [config.autoStart=true] - Auto-start engine
   * @param {number} [config.maxConcurrent=10] - Max concurrent operations
   * @param {boolean} [config.enableReceipts=true] - Enable receipt generation
   * @param {boolean} [config.enablePolicies=true] - Enable policy enforcement
   * @param {boolean} [config.enableReplay=true] - Enable event replay
   * @param {number} [config.replayBufferSize=1000] - Replay buffer size
   * @param {Object} [config.logger] - Logger instance
   */
  constructor(config = {}) {
    super();
    const validated = EventAutomationConfigSchema.parse(config);

    this.config = validated;
    this.logger = validated.logger || console;
    this.isRunning = false;
    this.startTime = null;

    // Initialize components
    this.deltaProcessor = new DeltaProcessor({ logger: this.logger });
    this.receiptTracker = new ReceiptTracker({ logger: this.logger });
    this.policyEnforcer = new PolicyEnforcer({
      logger: this.logger,
      failOnPolicyViolation: false, // Don't fail, just log
    });

    // Event replay buffer
    this.replayBuffer = [];
    this.maxReplayBufferSize = validated.replayBufferSize;

    // Processing queue
    this.processingQueue = [];
    this.activeProcessing = 0;

    // Auto-start if configured
    if (validated.autoStart) {
      this.start();
    }
  }

  /**
   * Start the engine
   */
  start() {
    if (this.isRunning) {
      this.logger.warn('Engine already running');
      return;
    }

    this.isRunning = true;
    this.startTime = Date.now();
    this.emit('started', { timestamp: this.startTime });
    this.logger.info('Event automation engine started', { id: this.config.id });
  }

  /**
   * Stop the engine
   */
  async stop() {
    if (!this.isRunning) {
      this.logger.warn('Engine not running');
      return;
    }

    // Wait for active processing to complete
    while (this.activeProcessing > 0) {
      await new Promise((resolve) => setTimeout(resolve, 100));
    }

    this.isRunning = false;
    this.emit('stopped', {
      timestamp: Date.now(),
      uptime: Date.now() - this.startTime,
    });
    this.logger.info('Event automation engine stopped', { id: this.config.id });
  }

  /**
   * Process a delta with full automation (receipts + policies)
   * @param {Object} delta - Delta to process
   * @param {Object} [_options] - Processing options
   * @returns {Promise<Object>} Processing result
   */
  async processDelta(delta, _options = {}) {
    if (!this.isRunning) {
      throw new Error('Engine not running. Call start() first.');
    }

    const startTime = performance.now();
    const deltaId = delta.id || `delta-${Date.now()}`;

    try {
      // Check max concurrent limit
      while (this.activeProcessing >= this.config.maxConcurrent) {
        await new Promise((resolve) => setTimeout(resolve, 10));
      }

      this.activeProcessing++;
      this.emit('delta:processing', { deltaId });

      // Step 1: Evaluate before:delta policies
      let policyResults = [];
      if (this.config.enablePolicies) {
        try {
          policyResults = await this.policyEnforcer.evaluatePolicies('before:delta', {
            delta,
            engine: this,
          });
        } catch (error) {
          this.emit('policy:violation', {
            deltaId,
            trigger: 'before:delta',
            error: error.message,
          });
          throw error;
        }
      }

      // Step 2: Process delta
      const processResult = await this.deltaProcessor.processDelta(delta);

      if (!processResult.success) {
        throw new Error(processResult.error || 'Delta processing failed');
      }

      // Step 3: Create receipt if enabled
      let receipt = null;
      if (this.config.enableReceipts) {
        receipt = await this.receiptTracker.createReceipt(delta, {
          operation: 'process',
          metadata: {
            processingDuration: processResult.duration,
          },
        });

        this.emit('receipt:created', { deltaId, receiptId: receipt.id });
      }

      // Step 4: Evaluate after:delta policies
      if (this.config.enablePolicies) {
        const afterPolicies = await this.policyEnforcer.evaluatePolicies('after:delta', {
          delta,
          receipt,
          processResult,
          engine: this,
        });
        policyResults = [...policyResults, ...afterPolicies];
      }

      // Step 5: Add to replay buffer if enabled
      if (this.config.enableReplay) {
        this._addToReplayBuffer({
          delta,
          receipt,
          timestamp: Date.now(),
        });
      }

      const duration = performance.now() - startTime;
      const result = {
        success: true,
        deltaId,
        receipts: receipt ? [receipt] : [],
        policyResults,
        duration,
      };

      // Validate result
      ProcessDeltaResultSchema.parse(result);

      this.emit('delta:processed', result);

      return result;
    } catch (error) {
      const duration = performance.now() - startTime;
      const result = {
        success: false,
        deltaId,
        error: error.message,
        duration,
      };

      this.emit('delta:failed', result);

      return result;
    } finally {
      this.activeProcessing--;
    }
  }

  /**
   * Batch process multiple deltas
   * @param {Array<Object>} deltas - Deltas to process
   * @param {Object} [_options] - Processing options
   * @param {boolean} [_options.parallel=false] - Process in parallel
   * @returns {Promise<Array<Object>>} Processing results
   */
  async batchProcess(deltas, _options = {}) {
    if (_options.parallel) {
      return Promise.all(deltas.map((delta) => this.processDelta(delta)));
    }

    const results = [];
    for (const delta of deltas) {
      results.push(await this.processDelta(delta));
    }
    return results;
  }

  /**
   * Replay events from buffer
   * @param {Object} [options] - Replay options
   * @returns {Promise<Array<Object>>} Replay results
   */
  async replay(options = {}) {
    const validated = ReplayOptionsSchema.parse(options);

    let events = [...this.replayBuffer];

    // Filter by timestamp range
    if (validated.fromTimestamp) {
      events = events.filter((e) => e.timestamp >= validated.fromTimestamp);
    }
    if (validated.toTimestamp) {
      events = events.filter((e) => e.timestamp <= validated.toTimestamp);
    }

    // Filter by delta IDs
    if (validated.deltaIds && validated.deltaIds.length > 0) {
      events = events.filter((e) =>
        validated.deltaIds.includes(e.delta.id)
      );
    }

    // Process in batches
    const batchSize = validated.batchSize || 100;
    const results = [];

    for (let i = 0; i < events.length; i += batchSize) {
      const batch = events.slice(i, i + batchSize);
      const batchResults = await this.batchProcess(
        batch.map((e) => e.delta),
        { parallel: validated.parallel }
      );
      results.push(...batchResults);
    }

    this.emit('replay:completed', {
      totalEvents: events.length,
      totalProcessed: results.length,
    });

    return results;
  }

  /**
   * Add event to replay buffer
   * @private
   * @param {Object} event - Event to add
   */
  _addToReplayBuffer(event) {
    this.replayBuffer.push(event);

    // Enforce buffer size limit (FIFO)
    if (this.replayBuffer.length > this.maxReplayBufferSize) {
      this.replayBuffer.shift();
    }
  }

  /**
   * Register a policy
   * @param {Object} policy - Policy to register
   * @returns {string} Policy ID
   */
  registerPolicy(policy) {
    return this.policyEnforcer.registerPolicy(policy);
  }

  /**
   * Unregister a policy
   * @param {string} policyId - Policy ID
   * @returns {boolean} Whether policy was removed
   */
  unregisterPolicy(policyId) {
    return this.policyEnforcer.unregisterPolicy(policyId);
  }

  /**
   * Get engine statistics
   * @returns {Object} Engine statistics
   */
  getStatistics() {
    const deltaMetrics = this.deltaProcessor.getMetrics();
    const receiptMetrics = this.receiptTracker.getMetrics();
    const policyMetrics = this.policyEnforcer.getMetrics();

    return {
      totalProcessed: deltaMetrics.totalProcessed,
      totalSucceeded: deltaMetrics.totalSucceeded,
      totalFailed: deltaMetrics.totalFailed,
      totalReceipts: receiptMetrics.totalReceipts,
      averageDuration: deltaMetrics.averageDuration,
      p95Duration: deltaMetrics.p95Duration,
      p99Duration: deltaMetrics.p99Duration,
      uptimeMs: this.startTime ? Date.now() - this.startTime : 0,
      policies: {
        total: policyMetrics.totalPolicies,
        evaluations: policyMetrics.totalEvaluations,
        passed: policyMetrics.totalPassed,
        failed: policyMetrics.totalFailed,
      },
      replayBufferSize: this.replayBuffer.length,
      activeProcessing: this.activeProcessing,
    };
  }

  /**
   * Reset engine state (for testing)
   */
  reset() {
    this.deltaProcessor.reset();
    this.receiptTracker.reset();
    this.policyEnforcer.reset();
    this.replayBuffer = [];
    this.processingQueue = [];
    this.activeProcessing = 0;
  }
}

/**
 * Create an event automation engine instance
 * @param {Object} [config] - Engine configuration
 * @returns {EventAutomationEngine} Engine instance
 */
export function createEventAutomationEngine(config = {}) {
  return new EventAutomationEngine(config);
}
