/**
 * @file YAWL Daemon Bridge Handler Functions
 * @module @unrdf/daemon/integrations/yawl-handlers
 * @description Handler functions for YAWL daemon bridge scheduled operations
 */

/**
 * Create handler for recurring case creation
 * @param {Object} bridge - YawlDaemonBridge instance
 * @param {string} workflowId - Workflow identifier
 * @param {string} caseIdPrefix - Prefix for case IDs
 * @param {Object} params - Case parameters
 * @returns {Function} Handler function for daemon scheduling
 */
export function createCaseCreationHandler(bridge, workflowId, caseIdPrefix, params) {
  return async () => {
    try {
      const caseId = `${caseIdPrefix}-${Date.now()}`;
      const result = await bridge.yawlEngine.createCase({
        workflowId,
        caseId,
        inputData: params.inputData || {},
      });

      bridge.emit('case:created-by-schedule', {
        bridgeId: bridge.id,
        workflowId,
        caseId,
        result,
        timestamp: new Date(),
      });

      return result;
    } catch (error) {
      bridge.logger.error(
        `[YawlDaemonBridge ${bridge.id}] Case creation failed: ${error.message}`
      );
      throw error;
    }
  };
}

/**
 * Create handler for task timeout monitoring
 * @param {Object} bridge - YawlDaemonBridge instance
 * @param {string} caseId - Case identifier
 * @param {string} taskId - Task identifier
 * @param {number} timeoutMs - Timeout in milliseconds
 * @param {number} startTime - Start timestamp
 * @param {string} operationId - Operation identifier for unscheduling
 * @returns {Function} Handler function for daemon scheduling
 */
export function createTimeoutWatchHandler(bridge, caseId, taskId, timeoutMs, startTime, operationId) {
  return async () => {
    const elapsed = Date.now() - startTime;
    if (elapsed >= timeoutMs) {
      try {
        await bridge.yawlEngine.cancelTask({
          caseId,
          taskId,
          reason: `Timeout after ${timeoutMs}ms`,
        });

        bridge.emit('task:timeout-enforced', {
          bridgeId: bridge.id,
          caseId,
          taskId,
          timeoutMs,
          timestamp: new Date(),
        });

        // Unschedule timeout watch
        bridge.daemon.unschedule(operationId);
        bridge.taskTimeouts.delete(`${caseId}:${taskId}`);
      } catch (error) {
        bridge.logger.error(
          `[YawlDaemonBridge ${bridge.id}] Timeout enforcement failed: ${error.message}`
        );
      }
    }
  };
}

/**
 * Create handler for task retry execution
 * @param {Object} bridge - YawlDaemonBridge instance
 * @param {string} caseId - Case identifier
 * @param {string} taskId - Task identifier
 * @param {string} retryKey - Retry tracking key
 * @param {Object} policy - Retry policy configuration
 * @param {string} operationId - Operation identifier for unscheduling
 * @returns {Function} Handler function for daemon scheduling
 */
export function createRetryHandler(bridge, caseId, taskId, retryKey, policy, operationId) {
  return async () => {
    const state = bridge.taskRetries.get(retryKey);
    if (!state || state.attempts >= state.maxAttempts) {
      return;
    }

    try {
      state.attempts += 1;
      const result = await bridge.yawlEngine.enableTask({
        caseId,
        taskId,
      });

      bridge.emit('task:retry-executed', {
        bridgeId: bridge.id,
        caseId,
        taskId,
        attempt: state.attempts,
        result,
        timestamp: new Date(),
      });

      if (state.attempts >= state.maxAttempts) {
        bridge.daemon.unschedule(operationId);
        bridge.taskRetries.delete(retryKey);
      } else {
        // Schedule next retry with backoff
        const nextBackoff = Math.min(
          policy.backoffMs * Math.pow(policy.backoffMultiplier, state.attempts),
          policy.maxBackoffMs
        );
        const jitter = nextBackoff * policy.jitterFactor * Math.random();
        state.nextRetryTime = Date.now() + nextBackoff + jitter;
      }
    } catch (error) {
      bridge.logger.error(
        `[YawlDaemonBridge ${bridge.id}] Retry execution failed: ${error.message}`
      );

      if (state.attempts >= state.maxAttempts) {
        bridge.daemon.unschedule(operationId);
        bridge.taskRetries.delete(retryKey);

        bridge.emit('task:retry-exhausted', {
          bridgeId: bridge.id,
          caseId,
          taskId,
          attempts: state.attempts,
          error: error.message,
          timestamp: new Date(),
        });
      }
    }
  };
}

/**
 * Create handler for parallel task distribution
 * @param {Object} bridge - YawlDaemonBridge instance
 * @param {string} caseId - Case identifier
 * @param {Array<string>} taskIds - Task identifiers
 * @param {string} strategy - Distribution strategy
 * @param {string} distributionId - Distribution identifier
 * @returns {Function} Handler function for daemon scheduling
 */
export function createDistributionHandler(bridge, caseId, taskIds, strategy, distributionId) {
  return async () => {
    try {
      const results = [];

      // Distribute tasks according to strategy
      for (let i = 0; i < taskIds.length; i += 1) {
        const taskId = taskIds[i];
        const result = await bridge.yawlEngine.enableTask({
          caseId,
          taskId,
        });

        results.push({
          taskId,
          result,
          index: i,
        });
      }

      bridge.emit('tasks:distributed', {
        bridgeId: bridge.id,
        caseId,
        taskIds,
        strategy,
        results,
        timestamp: new Date(),
      });

      bridge.parallelDistributions.set(distributionId, {
        caseId,
        taskIds,
        strategy,
        results,
        timestamp: Date.now(),
      });

      return results;
    } catch (error) {
      bridge.logger.error(
        `[YawlDaemonBridge ${bridge.id}] Task distribution failed: ${error.message}`
      );
      throw error;
    }
  };
}

/**
 * Setup YAWL event listeners for auto-retry and timeout tracking
 * @param {Object} bridge - YawlDaemonBridge instance
 * @returns {Array<Function>} Array of unsubscribe functions
 */
export function setupEventListeners(bridge) {
  const unsubscribers = [];

  if (!bridge.config.enableAutoRetry && !bridge.config.enableTimeoutTracking) {
    return unsubscribers;
  }

  // Listen to task:failed and schedule retry if enabled
  if (bridge.config.enableAutoRetry) {
    const unsubTaskFailed = bridge.yawlEngine.on('task:failed', async (event) => {
      try {
        await bridge.scheduleRetry(event.caseId, event.taskId);
      } catch (error) {
        bridge.logger.error(
          `[YawlDaemonBridge ${bridge.id}] Failed to schedule retry: ${error.message}`
        );
      }
    });
    unsubscribers.push(unsubTaskFailed);
  }

  // Listen to task:enabled and start timeout watch if configured
  if (bridge.config.enableTimeoutTracking) {
    const unsubTaskEnabled = bridge.yawlEngine.on('task:enabled', async (event) => {
      try {
        await bridge.watchTaskTimeout(
          event.caseId,
          event.taskId,
          bridge.config.timeoutDefaults.taskTimeoutMs
        );
      } catch (error) {
        bridge.logger.error(
          `[YawlDaemonBridge ${bridge.id}] Failed to watch timeout: ${error.message}`
        );
      }
    });
    unsubscribers.push(unsubTaskEnabled);
  }

  // Listen for case completion and cleanup
  const unsubCaseCompleted = bridge.yawlEngine.on('case:completed', (event) => {
    // Clean up any resources for this case
    for (const [key] of bridge.taskTimeouts) {
      if (key.startsWith(`${event.caseId}:`)) {
        const timeout = bridge.taskTimeouts.get(key);
        if (timeout?.operationId) {
          bridge.daemon.unschedule(timeout.operationId);
        }
        bridge.taskTimeouts.delete(key);
      }
    }

    for (const [key] of bridge.taskRetries) {
      if (key.startsWith(`${event.caseId}:`)) {
        const retry = bridge.taskRetries.get(key);
        if (retry?.operationId) {
          bridge.daemon.unschedule(retry.operationId);
        }
        bridge.taskRetries.delete(key);
      }
    }
  });
  unsubscribers.push(unsubCaseCompleted);

  return unsubscribers;
}
