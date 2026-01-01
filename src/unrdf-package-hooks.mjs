/**
 * @file src/unrdf-package-hooks.mjs
 * @description Event hook system for package discovery and lifecycle
 */

class PackageHookEmitter {
  constructor() {
    this.hooks = new Map();
    this.globalHooks = [];
    this.eventHistory = [];
    this.maxHistorySize = 1000;
  }

  on(event, callback, priority = 0) {
    if (!this.hooks.has(event)) {
      this.hooks.set(event, []);
    }

    const handler = { callback, priority, id: Math.random() };
    const handlers = this.hooks.get(event);
    handlers.push(handler);
    handlers.sort((a, b) => b.priority - a.priority);

    return () => {
      const idx = handlers.findIndex((h) => h.id === handler.id);
      if (idx !== -1) handlers.splice(idx, 1);
    };
  }

  onAny(callback, priority = 0) {
    this.globalHooks.push({ callback, priority, id: Math.random() });
    this.globalHooks.sort((a, b) => b.priority - a.priority);
  }

  async emit(event, data) {
    const timestamp = new Date().toISOString();
    const record = { event, data, timestamp, handlers: 0 };

    // Execute global hooks first
    for (const hook of this.globalHooks) {
      try {
        await hook.callback({ event, data, timestamp });
        record.handlers++;
      } catch (error) {
        record.error = error.message;
      }
    }

    // Execute event-specific hooks
    const handlers = this.hooks.get(event) || [];
    for (const handler of handlers) {
      try {
        await handler.callback(data);
        record.handlers++;
      } catch (error) {
        record.error = error.message;
      }
    }

    this.eventHistory.push(record);
    if (this.eventHistory.length > this.maxHistorySize) {
      this.eventHistory.shift();
    }

    return record;
  }

  getHistory(event = null) {
    if (event) {
      return this.eventHistory.filter((h) => h.event === event);
    }
    return this.eventHistory;
  }

  clearHistory() {
    this.eventHistory = [];
  }
}

export class PackageLifecycleHooks {
  constructor() {
    this.emitter = new PackageHookEmitter();
  }

  onDiscoveryStart(callback) {
    this.emitter.on('discovery:start', callback);
  }

  onDiscoveryComplete(callback) {
    this.emitter.on('discovery:complete', callback);
  }

  onPackageFound(callback) {
    this.emitter.on('package:found', callback);
  }

  onPackageLoaded(callback) {
    this.emitter.on('package:loaded', callback);
  }

  onPackageError(callback) {
    this.emitter.on('package:error', callback);
  }

  onDependencyResolved(callback) {
    this.emitter.on('dependency:resolved', callback);
  }

  onConflictDetected(callback) {
    this.emitter.on('conflict:detected', callback);
  }

  onTierViolation(callback) {
    this.emitter.on('tier:violation', callback);
  }

  async emitDiscoveryStart(metadata) {
    return this.emitter.emit('discovery:start', metadata);
  }

  async emitDiscoveryComplete(result) {
    return this.emitter.emit('discovery:complete', result);
  }

  async emitPackageFound(pkg) {
    return this.emitter.emit('package:found', pkg);
  }

  async emitPackageLoaded(pkg) {
    return this.emitter.emit('package:loaded', pkg);
  }

  async emitPackageError(error) {
    return this.emitter.emit('package:error', error);
  }

  async emitDependencyResolved(result) {
    return this.emitter.emit('dependency:resolved', result);
  }

  async emitConflictDetected(conflict) {
    return this.emitter.emit('conflict:detected', conflict);
  }

  async emitTierViolation(violation) {
    return this.emitter.emit('tier:violation', violation);
  }

  getEventHistory(event = null) {
    return this.emitter.getHistory(event);
  }

  getEventStats() {
    const stats = {};
    for (const record of this.emitter.eventHistory) {
      if (!stats[record.event]) {
        stats[record.event] = { count: 0, errors: 0 };
      }
      stats[record.event].count++;
      if (record.error) stats[record.event].errors++;
    }
    return stats;
  }
}

export const lifecycleHooks = new PackageLifecycleHooks();

export default lifecycleHooks;
