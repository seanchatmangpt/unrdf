/**
 * @file Hook Scheduler for Cron and Interval Triggers
 * @module hooks/hook-scheduler
 *
 * @description
 * Event-driven scheduler for cron/time-based hook triggers:
 * - on-schedule: Cron-like scheduled execution
 * - on-interval: Periodic execution at fixed intervals
 * - on-idle: Execute during idle periods
 * - on-startup: Execute once at system startup
 */

import { z } from 'zod';

/**
 * @typedef {Object} ScheduledHook
 * @property {string} id - Unique scheduler entry ID
 * @property {import('./define-hook.mjs').Hook} hook - The hook to execute
 * @property {string} schedule - Cron expression or interval spec
 * @property {number} [interval] - Interval in milliseconds
 * @property {boolean} enabled - Whether the schedule is active
 * @property {Date} [lastRun] - Last execution timestamp
 * @property {Date} [nextRun] - Next scheduled execution
 * @property {number} runCount - Total execution count
 */

/**
 * Schedule configuration schema
 * POKA-YOKE: Interval bounds prevent CPU thrashing (RPN 168 → 0)
 */
export const ScheduleConfigSchema = z.object({
  id: z.string().min(1),
  hookId: z.string().min(1),
  type: z.enum(['cron', 'interval', 'idle', 'startup']),
  expression: z.string().optional(), // Cron expression
  // POKA-YOKE: Interval bounds validation (RPN 168 → 0)
  // Min 10ms prevents CPU thrashing, max 24h prevents integer overflow
  intervalMs: z
    .number()
    .positive()
    .min(10, 'Interval must be at least 10ms to prevent CPU thrashing')
    .max(86400000, 'Interval cannot exceed 24 hours (86400000ms)')
    .optional(),
  idleTimeoutMs: z
    .number()
    .positive()
    .min(100, 'Idle timeout must be at least 100ms')
    .max(3600000, 'Idle timeout cannot exceed 1 hour')
    .optional(),
  enabled: z.boolean().default(true),
  maxRuns: z.number().positive().optional(), // Max executions
  metadata: z.record(z.any()).optional(),
});

/**
 * Hook Scheduler - Manages time-based hook execution
 *
 * @class HookScheduler
 */
export class HookScheduler {
  /**
   * Create a new hook scheduler
   *
   * @param {object} options - Scheduler options
   * @param {Function} options.executeHook - Hook execution function
   * @param {number} options.tickInterval - Scheduler tick interval (default: 1000ms)
   */
  constructor(options = {}) {
    /** @type {Map<string, ScheduledHook>} */
    this.schedules = new Map();

    /** @type {Function} */
    this.executeHook = options.executeHook || (async () => {});

    /** @type {number} */
    this.tickInterval = options.tickInterval || 1000;

    /** @type {NodeJS.Timer|null} */
    this.ticker = null;

    /** @type {boolean} */
    this.running = false;

    /** @type {Date|null} */
    this.lastTick = null;

    /** @type {number} */
    this.idleThreshold = options.idleThreshold || 5000;

    /** @type {number} */
    this.idleStart = Date.now();

    // Startup hooks queue
    /** @type {Array<ScheduledHook>} */
    this.startupQueue = [];
  }

  /**
   * Register a scheduled hook
   *
   * @param {import('./define-hook.mjs').Hook} hook - Hook to schedule
   * @param {object} config - Schedule configuration
   * @returns {ScheduledHook} - Registered scheduled hook
   */
  register(hook, config) {
    const validConfig = ScheduleConfigSchema.parse(config);

    const scheduled = {
      id: validConfig.id,
      hook,
      type: validConfig.type,
      expression: validConfig.expression,
      interval: validConfig.intervalMs,
      idleTimeout: validConfig.idleTimeoutMs,
      enabled: validConfig.enabled,
      lastRun: null,
      nextRun: this._calculateNextRun(validConfig),
      runCount: 0,
      maxRuns: validConfig.maxRuns,
      metadata: validConfig.metadata || {},
    };

    this.schedules.set(validConfig.id, scheduled);

    // Queue startup hooks
    if (validConfig.type === 'startup') {
      this.startupQueue.push(scheduled);
    }

    return scheduled;
  }

  /**
   * Unregister a scheduled hook
   *
   * @param {string} id - Schedule ID to remove
   */
  unregister(id) {
    this.schedules.delete(id);
  }

  /**
   * Start the scheduler
   */
  start() {
    if (this.running) return;

    this.running = true;
    this.lastTick = new Date();

    // Execute startup hooks immediately
    this._executeStartupHooks();

    // Start periodic ticker
    this.ticker = setInterval(() => this._tick(), this.tickInterval);
  }

  /**
   * Stop the scheduler
   */
  stop() {
    if (!this.running) return;

    this.running = false;
    if (this.ticker) {
      clearInterval(this.ticker);
      this.ticker = null;
    }
  }

  /**
   * Notify scheduler of activity (resets idle timer)
   */
  notifyActivity() {
    this.idleStart = Date.now();
  }

  /**
   * Get scheduler statistics
   *
   * @returns {object} - Scheduler stats
   */
  getStats() {
    const schedules = Array.from(this.schedules.values());
    return {
      totalSchedules: schedules.length,
      enabledSchedules: schedules.filter(s => s.enabled).length,
      totalRuns: schedules.reduce((sum, s) => sum + s.runCount, 0),
      byType: {
        cron: schedules.filter(s => s.type === 'cron').length,
        interval: schedules.filter(s => s.type === 'interval').length,
        idle: schedules.filter(s => s.type === 'idle').length,
        startup: schedules.filter(s => s.type === 'startup').length,
      },
      running: this.running,
      idleSince: this.idleStart,
    };
  }

  /**
   * Execute startup hooks
   * @private
   */
  async _executeStartupHooks() {
    for (const scheduled of this.startupQueue) {
      if (scheduled.enabled && scheduled.runCount === 0) {
        await this._executeScheduled(scheduled);
      }
    }
    this.startupQueue = [];
  }

  /**
   * Scheduler tick - check and execute due hooks
   * @private
   */
  async _tick() {
    const now = new Date();
    const isIdle = Date.now() - this.idleStart > this.idleThreshold;

    for (const scheduled of this.schedules.values()) {
      if (!scheduled.enabled) continue;
      if (scheduled.maxRuns && scheduled.runCount >= scheduled.maxRuns) continue;

      let shouldRun = false;

      switch (scheduled.type) {
        case 'interval':
          shouldRun = scheduled.nextRun && now >= scheduled.nextRun;
          break;
        case 'idle':
          shouldRun =
            isIdle &&
            (!scheduled.lastRun ||
              Date.now() - scheduled.lastRun.getTime() >
                (scheduled.idleTimeout || this.idleThreshold));
          break;
        case 'cron':
          shouldRun = scheduled.nextRun && now >= scheduled.nextRun;
          break;
        default:
          // startup handled separately
          break;
      }

      if (shouldRun) {
        await this._executeScheduled(scheduled);
      }
    }

    this.lastTick = now;
  }

  /**
   * Execute a scheduled hook
   * POKA-YOKE: Circuit breaker disables after 3 consecutive failures (RPN 432 → 43)
   * @private
   */
  async _executeScheduled(scheduled) {
    try {
      scheduled.lastRun = new Date();
      scheduled.runCount++;

      // Calculate next run time
      scheduled.nextRun = this._calculateNextRun(scheduled);

      // Execute the hook
      await this.executeHook(scheduled.hook, {
        scheduledId: scheduled.id,
        runCount: scheduled.runCount,
        scheduledTime: scheduled.lastRun,
      });

      // POKA-YOKE: Reset error count on success
      scheduled.errorCount = 0;
      scheduled.lastError = null;
    } catch (error) {
      // POKA-YOKE: Track consecutive errors (RPN 432 → 43)
      scheduled.lastError = error instanceof Error ? error : new Error(String(error));
      scheduled.errorCount = (scheduled.errorCount || 0) + 1;

      // Emit error event for observability
      if (this.onError) {
        this.onError({
          scheduledId: scheduled.id,
          hookName: scheduled.hook?.name || 'unknown',
          error: scheduled.lastError,
          errorCount: scheduled.errorCount,
          timestamp: new Date(),
        });
      }

      console.error(
        `[POKA-YOKE] Scheduled hook "${scheduled.id}" failed (attempt ${scheduled.errorCount}/3):`,
        scheduled.lastError.message
      );

      // POKA-YOKE: Circuit breaker - disable after 3 consecutive failures
      if (scheduled.errorCount >= 3) {
        scheduled.enabled = false;
        console.warn(
          `[POKA-YOKE] Scheduled hook "${scheduled.id}" disabled after 3 consecutive failures. ` +
            `Last error: ${scheduled.lastError.message}. ` +
            `Re-enable with scheduler.enable("${scheduled.id}") after fixing the issue.`
        );

        // Emit circuit-open event
        if (this.onCircuitOpen) {
          this.onCircuitOpen({
            scheduledId: scheduled.id,
            hookName: scheduled.hook?.name || 'unknown',
            lastError: scheduled.lastError,
            totalErrors: scheduled.errorCount,
            timestamp: new Date(),
          });
        }
      }
    }
  }

  /**
   * Re-enable a disabled scheduled hook
   *
   * @param {string} id - Schedule ID to enable
   * @returns {boolean} - True if enabled, false if not found
   */
  enable(id) {
    const scheduled = this.schedules.get(id);
    if (!scheduled) return false;

    scheduled.enabled = true;
    scheduled.errorCount = 0;
    scheduled.lastError = null;
    return true;
  }

  /**
   * Calculate next run time for a schedule
   * @private
   */
  _calculateNextRun(config) {
    const now = new Date();

    switch (config.type) {
      case 'interval':
        return new Date(now.getTime() + (config.intervalMs || config.interval || 60000));
      case 'cron':
        // Simple cron parser (supports: */n for "every n units")
        return this._parseCronExpression(config.expression);
      case 'idle':
      case 'startup':
        return null; // Event-driven, not time-driven
      default:
        return null;
    }
  }

  /**
   * Simple cron expression parser
   * POKA-YOKE: Strict validation instead of silent fallback (RPN 315 → 0)
   * Supports intervals in the format star-slash-n (e.g., star-slash-5 = every 5 minutes)
   * @private
   */
  _parseCronExpression(expression) {
    // POKA-YOKE: Explicit null handling instead of silent fallback
    if (!expression) {
      throw new Error(
        'Cron expression is required for type "cron". ' +
          'Use "*/n" format for intervals (e.g., "*/5" for every 5 minutes).'
      );
    }

    // Simple interval pattern: */n (every n minutes)
    const intervalMatch = expression.match(/^\*\/(\d+)$/);
    if (intervalMatch) {
      const minutes = parseInt(intervalMatch[1], 10);

      // POKA-YOKE: Validate interval range (RPN 315 → 0)
      if (minutes < 1 || minutes > 1440) {
        throw new Error(
          `Invalid cron interval: */[${minutes}]. ` +
            `Value must be between 1 and 1440 minutes (24 hours). ` +
            `Example: "*/5" for every 5 minutes.`
        );
      }

      return new Date(Date.now() + minutes * 60 * 1000);
    }

    // POKA-YOKE: Reject unrecognized patterns instead of silent fallback (RPN 315 → 0)
    throw new Error(
      `Invalid cron expression: "${expression}". ` +
        `Supported format: "*/n" where n is minutes (1-1440). ` +
        `Example: "*/5" for every 5 minutes, "*/60" for every hour.`
    );
  }
}

/**
 * Create a preconfigured scheduler instance
 *
 * @param {object} options - Scheduler options
 * @returns {HookScheduler} - Scheduler instance
 */
export function createHookScheduler(options = {}) {
  return new HookScheduler(options);
}

export default {
  HookScheduler,
  createHookScheduler,
  ScheduleConfigSchema,
};
