/**
 * @file Daemon-HookScheduler Integration
 * @module daemon/integrations/hook-scheduler
 * @description Bidirectional integration between UnrdfDaemon and HookScheduler
 */

import { EventEmitter } from 'events';
import { z } from 'zod';

export const DaemonHookIntegrationConfigSchema = z.object({
  adapterId: z.string().min(1).default(() => `adapter-${Date.now()}`),
  autoStart: z.boolean().default(true),
  timeout: z.number().positive().default(30000),
  retryPolicy: z.object({
    maxAttempts: z.number().int().min(1).default(3),
    backoffMs: z.number().int().min(0).default(1000),
  }).optional(),
});

/**
 * Adapter between Daemon and HookScheduler
 * Manages bidirectional event flow: hook schedules ↔ daemon operations
 * @extends EventEmitter
 */
export class DaemonHookAdapter extends EventEmitter {
  /**
   * Create daemon-hook adapter
   * @param {import('../daemon.mjs').UnrdfDaemon} daemon
   * @param {import('@unrdf/hooks').HookScheduler} hookScheduler
   * @param {object} options - Configuration
   */
  constructor(daemon, hookScheduler, options = {}) {
    super();
    const cfg = DaemonHookIntegrationConfigSchema.parse(options);
    this.id = cfg.adapterId;
    this.daemon = daemon;
    this.hookScheduler = hookScheduler;
    this.config = cfg;
    this.hookToDaemonMap = new Map();
    this.hookMetadata = new Map();
    this._setupListeners();
    if (cfg.autoStart) this.start();
  }

  /**
   *
   */
  _setupListeners() {
    if (this.hookScheduler.onError) {
      const orig = this.hookScheduler.onError;
      this.hookScheduler.onError = (e) => {
        orig(e);
        this.emit('hook:error', { adapterId: this.id, ...e });
      };
    }
    if (this.hookScheduler.onCircuitOpen) {
      const orig = this.hookScheduler.onCircuitOpen;
      this.hookScheduler.onCircuitOpen = (c) => {
        orig(c);
        this.emit('hook:circuit-open', { adapterId: this.id, ...c });
      };
    }
    this.daemon.on('task:completed', (info) => {
      const hid = this._findHookIdByTaskId(info.taskId);
      if (hid) this.emit('hook:executed', { adapterId: this.id, hookId: hid, ...info, timestamp: new Date() });
    });
    this.daemon.on('task:failed', (info) => {
      const hid = this._findHookIdByTaskId(info.taskId);
      if (hid) this.emit('hook:failed', { adapterId: this.id, hookId: hid, ...info, timestamp: new Date() });
    });
  }

  /**
   *
   */
  async start() {
    await this.daemon.start();
    this.hookScheduler.start();
    this.emit('adapter:started', { adapterId: this.id });
  }

  /**
   *
   */
  async stop() {
    this.hookScheduler.stop();
    await this.daemon.stop();
    this.emit('adapter:stopped', { adapterId: this.id });
  }

  /**
   * Register hook with daemon task
   * @param {string} hookId
   * @param {object} hook - Hook definition
   * @param {object} schedule - Schedule config
   * @returns {object}
   */
  registerHookWithDaemon(hookId, hook, schedule) {
    const taskId = `hook-${hookId}`;
    this.hookScheduler.register(hook, schedule);
    this.daemon.registerTask(taskId, (ctx) => this._executeHookViaDaemon(hook, hookId, ctx));
    this.hookToDaemonMap.set(hookId, taskId);
    this.hookMetadata.set(hookId, { hook, schedule });
    this.emit('hook:registered', { adapterId: this.id, hookId, taskId });
    return { hookId, taskId, success: true };
  }

  /**
   * Unregister hook
   * @param {string} hookId
   * @returns {boolean}
   */
  unregisterHook(hookId) {
    const taskId = this.hookToDaemonMap.get(hookId);
    if (!taskId) return false;
    this.daemon.unregisterTask(taskId);
    this.hookScheduler.unregister(hookId);
    this.hookToDaemonMap.delete(hookId);
    this.hookMetadata.delete(hookId);
    this.emit('hook:unregistered', { adapterId: this.id, hookId, taskId });
    return true;
  }

  /**
   * Execute hook via daemon with retry and timeout
   * @param {object} hook
   * @param {string} hookId
   * @param {object} context
   * @returns {Promise<object>}
   * @private
   */
  async _executeHookViaDaemon(hook, hookId, context) {
    const max = this.config.retryPolicy?.maxAttempts || 1;
    const backoff = this.config.retryPolicy?.backoffMs || 0;
    let err;

    for (let i = 0; i < max; i++) {
      try {
        if (i > 0 && backoff > 0) await new Promise((r) => setTimeout(r, backoff * i));
        const result = await Promise.race([
          this.hookScheduler.executeHook(hook, context),
          new Promise((_, rej) =>
            setTimeout(() => rej(new Error(`Timeout: ${this.config.timeout}ms`)), this.config.timeout)
          ),
        ]);
        return {
          success: true,
          hookId,
          result,
          attempt: i + 1,
          receipt: {
            operation: 'hook:execute',
            entityType: 'Hook',
            data: { hookId, success: true },
            timestamp: new Date(),
          },
        };
      } catch (e) {
        err = e instanceof Error ? e : new Error(String(e));
        if (i === max - 1) break;
      }
    }
    return {
      success: false,
      hookId,
      error: err,
      attempts: max,
      receipt: {
        operation: 'hook:execute',
        entityType: 'Hook',
        data: { hookId, success: false, error: err?.message },
        timestamp: new Date(),
      },
    };
  }

  /**
   *
   */
  _findHookIdByTaskId(taskId) {
    for (const [hid, tid] of this.hookToDaemonMap.entries()) {
      if (tid === taskId) return hid;
    }
    return null;
  }

  /**
   *
   */
  getStats() {
    return {
      adapterId: this.id,
      registeredHooks: this.hookToDaemonMap.size,
      daemonStats: {
        running: this.daemon.isRunning,
        activeCount: this.daemon.activeCount,
        maxConcurrent: this.daemon.maxConcurrent,
      },
      schedulerStats: this.hookScheduler.getStats(),
      retryPolicy: this.config.retryPolicy,
    };
  }
}

/**
 * Integrate daemon with hook scheduler
 * @param {import('../daemon.mjs').UnrdfDaemon} daemon
 * @param {import('@unrdf/hooks').HookScheduler} hookScheduler
 * @param {object} options
 * @returns {DaemonHookAdapter}
 */
export function integrateHookScheduler(daemon, hookScheduler, options = {}) {
  return new DaemonHookAdapter(daemon, hookScheduler, options);
}

/**
 * Create daemon initialized with hook scheduler
 * Pre-loads scheduled hooks as daemon operations
 * @param {import('@unrdf/hooks').HookScheduler} hookScheduler
 * @param {object} config - {daemon?: {}, integration?: {}}
 * @returns {Promise<DaemonHookAdapter>}
 */
export async function createDaemonFromHookScheduler(hookScheduler, config = {}) {
  const { UnrdfDaemon } = await import('../daemon.mjs');
  const daemon = new UnrdfDaemon(config.daemon || {});
  const adapter = new DaemonHookAdapter(daemon, hookScheduler, {
    ...(config.integration || {}),
    autoStart: config.integration?.autoStart !== false,
  });
  for (const [id, sched] of hookScheduler.schedules.entries()) {
    if (sched.enabled && sched.hook) {
      adapter.registerHookWithDaemon(id, sched.hook, {
        id,
        hookId: id,
        type: sched.type,
        expression: sched.expression,
        intervalMs: sched.interval,
        idleTimeoutMs: sched.idleTimeout,
        enabled: sched.enabled,
      });
    }
  }
  return adapter;
}

export default {
  DaemonHookAdapter,
  integrateHookScheduler,
  createDaemonFromHookScheduler,
  DaemonHookIntegrationConfigSchema,
};
