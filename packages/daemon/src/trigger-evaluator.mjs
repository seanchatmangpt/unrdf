/**
 * @file Trigger Schedule Evaluator
 * @module @unrdf/daemon/trigger-evaluator
 * @description Pure functions for evaluating trigger execution based on schedules
 */

import cronParser from 'cron-parser';

/**
 * Evaluates whether a trigger should execute based on its type and configuration
 * @param {Object} trigger - Trigger configuration
 * @param {string} trigger.type - Trigger type: 'interval', 'cron', 'idle', 'reactive', 'event'
 * @param {string|number} trigger.value - Interval ms, cron expression, or idle ms
 * @param {number} lastExecuted - Last execution timestamp (ms since epoch)
 * @returns {Object} Evaluation result
 * @returns {boolean} result.shouldExecute - Whether trigger should execute now
 * @returns {number} result.nextExecutionTime - Milliseconds until next execution
 * @example
 * const result = evaluateTrigger(
 *   { type: 'interval', value: 5000 },
 *   Date.now() - 6000
 * );
 * console.log(result); // { shouldExecute: true, nextExecutionTime: 0 }
 */
export function evaluateTrigger(trigger, lastExecuted) {
  const now = Date.now();

  switch (trigger.type) {
    case 'interval':
      return shouldExecuteInterval(trigger.value, lastExecuted, now);

    case 'cron':
      return shouldExecuteCron(trigger.value, lastExecuted, now);

    case 'idle':
      return {
        shouldExecute: false,
        nextExecutionTime: Infinity,
      };

    case 'reactive':
    case 'event':
      return {
        shouldExecute: false,
        nextExecutionTime: Infinity,
      };

    default:
      return {
        shouldExecute: false,
        nextExecutionTime: Infinity,
      };
  }
}

/**
 * Evaluates interval-based trigger execution
 * @param {number} ms - Interval in milliseconds
 * @param {number} lastExecuted - Last execution timestamp
 * @param {number} [now] - Current timestamp (defaults to Date.now())
 * @returns {Object} Evaluation result with shouldExecute and nextExecutionTime
 * @private
 */
function shouldExecuteInterval(ms, lastExecuted, now = Date.now()) {
  if (typeof ms !== 'number' || ms <= 0) {
    return { shouldExecute: false, nextExecutionTime: Infinity };
  }

  const nextExecutionTime = lastExecuted + ms;
  const shouldExecute = now >= nextExecutionTime;
  const delayMs = Math.max(0, nextExecutionTime - now);

  return {
    shouldExecute,
    nextExecutionTime: delayMs,
  };
}

/**
 * Evaluates cron-based trigger execution
 * @param {string} expression - Cron expression (e.g., '0 *\/6 * * *' for every 6 hours)
 * @param {number} lastExecuted - Last execution timestamp
 * @param {number} [now] - Current timestamp (defaults to Date.now())
 * @returns {Object} Evaluation result with shouldExecute and nextExecutionTime
 * @throws {Error} If cron expression is invalid
 * @private
 */
function shouldExecuteCron(expression, lastExecuted, now = Date.now()) {
  if (typeof expression !== 'string') {
    return { shouldExecute: false, nextExecutionTime: Infinity };
  }

  try {
    const interval = cronParser.parseExpression(expression);
    const nextDate = interval.next();
    const nextTime = nextDate.toDate().getTime();

    const shouldExecute = now >= nextTime;
    const delayMs = Math.max(0, nextTime - now);

    return {
      shouldExecute,
      nextExecutionTime: delayMs,
    };
  } catch (error) {
    return { shouldExecute: false, nextExecutionTime: Infinity };
  }
}

/**
 * Evaluates idle-based trigger execution
 * @param {number} ms - Idle threshold in milliseconds
 * @param {number} lastActivityTime - Timestamp of last activity
 * @param {number} [now] - Current timestamp (defaults to Date.now())
 * @returns {Object} Evaluation result with shouldExecute and nextExecutionTime
 * @example
 * const result = shouldExecuteIdle(
 *   30000,
 *   Date.now() - 35000
 * );
 * console.log(result); // { shouldExecute: true, nextExecutionTime: 0 }
 * @internal Used internally by evaluateTrigger
 */
export function shouldExecuteIdle(ms, lastActivityTime, now = Date.now()) {
  if (typeof ms !== 'number' || ms <= 0) {
    return { shouldExecute: false, nextExecutionTime: Infinity };
  }

  const idleThreshold = lastActivityTime + ms;
  const shouldExecute = now >= idleThreshold;
  const delayMs = Math.max(0, idleThreshold - now);

  return {
    shouldExecute,
    nextExecutionTime: delayMs,
  };
}

/**
 * Calculates milliseconds until next execution for a trigger
 * @param {Object} trigger - Trigger configuration
 * @param {string} trigger.type - Trigger type: 'interval', 'cron', 'idle'
 * @param {string|number} trigger.value - Interval/cron/idle value
 * @param {number} lastExecuted - Last execution or activity timestamp
 * @returns {number} Milliseconds until next execution (Infinity if not applicable)
 * @example
 * const delayMs = calculateNextExecutionTime(
 *   { type: 'interval', value: 5000 },
 *   Date.now() - 3000
 * );
 * console.log(delayMs); // ~2000
 */
export function calculateNextExecutionTime(trigger, lastExecuted) {
  const result = evaluateTrigger(trigger, lastExecuted);
  return result.nextExecutionTime;
}

/**
 * Validates trigger configuration structure
 * @param {Object} trigger - Trigger to validate
 * @returns {boolean} Whether trigger has valid structure
 * @example
 * const isValid = isValidTrigger({ type: 'interval', value: 5000 });
 * console.log(isValid); // true
 */
export function isValidTrigger(trigger) {
  if (!trigger || typeof trigger !== 'object') {
    return false;
  }

  const validTypes = ['interval', 'cron', 'idle', 'reactive', 'event'];
  if (!validTypes.includes(trigger.type)) {
    return false;
  }

  if (trigger.type === 'interval' || trigger.type === 'idle') {
    return typeof trigger.value === 'number' && trigger.value > 0;
  }

  if (trigger.type === 'cron') {
    return typeof trigger.value === 'string' && trigger.value.length > 0;
  }

  return true;
}
