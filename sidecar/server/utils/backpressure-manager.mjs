/**
 * Backpressure Manager
 *
 * Monitors system load and queue depth to prevent overload:
 * - Queue depth monitoring
 * - Backpressure signaling
 * - Load shedding when system is overloaded
 * - Graceful degradation
 *
 * @module sidecar/utils/backpressure-manager
 */

import { trace, context, SpanStatusCode } from '@opentelemetry/api';
import { metrics } from './otel-metrics.mjs';
import { EventEmitter } from 'events';

const tracer = trace.getTracer('sidecar-backpressure');

// Configuration
const BACKPRESSURE_CONFIG = {
  // Queue depth thresholds
  queueDepth: {
    warning: 100,
    critical: 500,
    maximum: 1000,
  },

  // System load thresholds
  systemLoad: {
    warning: 0.7,   // 70% CPU/memory
    critical: 0.85, // 85%
    maximum: 0.95,  // 95%
  },

  // Load shedding priorities
  priorities: {
    critical: 1,
    high: 2,
    normal: 3,
    low: 4,
    background: 5,
  },

  // Recovery settings
  recovery: {
    cooldownMs: 5000,        // 5 seconds cooldown after load shedding
    checkIntervalMs: 1000,   // Check every second
    memoryCheckIntervalMs: 5000, // Check memory every 5 seconds
  },
};

/**
 * Backpressure Manager Class
 */
class BackpressureManager extends EventEmitter {
  constructor() {
    super();
    this.queues = new Map(); // queueName -> { depth, maxDepth, requests }
    this.systemState = {
      level: 'normal', // normal, warning, critical, overload
      queueDepth: 0,
      systemLoad: 0,
      memoryUsage: 0,
      lastCheck: Date.now(),
      lastLoadShedding: 0,
    };
    this.inCooldown = false;

    // Start monitoring
    this.startMonitoring();
  }

  /**
   * Register a queue for monitoring
   */
  registerQueue(queueName, maxDepth = BACKPRESSURE_CONFIG.queueDepth.maximum) {
    this.queues.set(queueName, {
      depth: 0,
      maxDepth,
      requests: [],
      totalProcessed: 0,
      totalRejected: 0,
    });
  }

  /**
   * Enqueue a request
   */
  enqueue(queueName, request, priority = 'normal') {
    const span = tracer.startSpan('backpressure-enqueue', {
      attributes: {
        'queue.name': queueName,
        'request.priority': priority,
      },
    });

    try {
      // Get or create queue
      if (!this.queues.has(queueName)) {
        this.registerQueue(queueName);
      }

      const queue = this.queues.get(queueName);

      // Check if we should shed load
      const shouldShed = this.shouldShedLoad(priority);

      if (shouldShed) {
        queue.totalRejected++;

        span.setAttributes({
          'backpressure.shed': true,
          'backpressure.level': this.systemState.level,
        });

        // Record metric
        if (metrics?.backpressureRejectedCounter) {
          metrics.backpressureRejectedCounter.add(1, {
            queue: queueName,
            priority,
            reason: 'overload',
          });
        }

        span.setStatus({ code: SpanStatusCode.OK, message: 'Load shed' });

        return {
          queued: false,
          reason: 'System overload - request rejected',
          systemState: this.systemState.level,
          retryAfter: BACKPRESSURE_CONFIG.recovery.cooldownMs / 1000,
        };
      }

      // Check queue depth
      if (queue.depth >= queue.maxDepth) {
        queue.totalRejected++;

        span.setAttributes({
          'backpressure.shed': true,
          'queue.depth': queue.depth,
          'queue.max_depth': queue.maxDepth,
        });

        // Record metric
        if (metrics?.backpressureRejectedCounter) {
          metrics.backpressureRejectedCounter.add(1, {
            queue: queueName,
            priority,
            reason: 'queue_full',
          });
        }

        span.setStatus({ code: SpanStatusCode.OK, message: 'Queue full' });

        return {
          queued: false,
          reason: 'Queue full - maximum depth reached',
          queueDepth: queue.depth,
          maxDepth: queue.maxDepth,
        };
      }

      // Add to queue
      queue.requests.push({
        id: `${queueName}-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
        request,
        priority,
        enqueuedAt: Date.now(),
      });
      queue.depth++;

      // Record metric
      if (metrics?.backpressureQueueDepthGauge) {
        metrics.backpressureQueueDepthGauge.record(queue.depth, { queue: queueName });
      }

      span.setAttributes({
        'backpressure.shed': false,
        'queue.depth': queue.depth,
      });
      span.setStatus({ code: SpanStatusCode.OK });

      return {
        queued: true,
        queueDepth: queue.depth,
        position: queue.depth,
      };

    } catch (err) {
      span.recordException(err);
      span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });
      return {
        queued: false,
        reason: 'Internal error',
        error: err.message,
      };
    } finally {
      span.end();
    }
  }

  /**
   * Dequeue a request
   */
  dequeue(queueName) {
    if (!this.queues.has(queueName)) {
      return null;
    }

    const queue = this.queues.get(queueName);

    if (queue.requests.length === 0) {
      return null;
    }

    // Sort by priority (lower number = higher priority)
    queue.requests.sort((a, b) => {
      const aPriority = BACKPRESSURE_CONFIG.priorities[a.priority] || 3;
      const bPriority = BACKPRESSURE_CONFIG.priorities[b.priority] || 3;
      return aPriority - bPriority;
    });

    const request = queue.requests.shift();
    queue.depth--;
    queue.totalProcessed++;

    // Record metric
    if (metrics?.backpressureQueueDepthGauge) {
      metrics.backpressureQueueDepthGauge.record(queue.depth, { queue: queueName });
    }

    return request;
  }

  /**
   * Get current system load
   */
  getSystemLoad() {
    const cpuUsage = process.cpuUsage();
    const memUsage = process.memoryUsage();

    // Simplified load calculation
    const cpuLoad = (cpuUsage.user + cpuUsage.system) / 1000000 / 100;
    const memLoad = memUsage.heapUsed / memUsage.heapTotal;

    return Math.max(cpuLoad, memLoad);
  }

  /**
   * Calculate total queue depth across all queues
   */
  getTotalQueueDepth() {
    let total = 0;
    for (const queue of this.queues.values()) {
      total += queue.depth;
    }
    return total;
  }

  /**
   * Determine if we should shed load based on priority
   */
  shouldShedLoad(priority) {
    const priorityLevel = BACKPRESSURE_CONFIG.priorities[priority] || 3;

    // In cooldown, only accept critical requests
    if (this.inCooldown && priorityLevel > BACKPRESSURE_CONFIG.priorities.critical) {
      return true;
    }

    switch (this.systemState.level) {
      case 'overload':
        // Only accept critical requests
        return priorityLevel > BACKPRESSURE_CONFIG.priorities.critical;

      case 'critical':
        // Shed low and background requests
        return priorityLevel >= BACKPRESSURE_CONFIG.priorities.low;

      case 'warning':
        // Only shed background requests
        return priorityLevel >= BACKPRESSURE_CONFIG.priorities.background;

      case 'normal':
      default:
        return false;
    }
  }

  /**
   * Update system state based on current metrics
   */
  updateSystemState() {
    const span = tracer.startSpan('backpressure-update-state');

    try {
      const systemLoad = this.getSystemLoad();
      const queueDepth = this.getTotalQueueDepth();
      const memUsage = process.memoryUsage();
      const memoryUsage = memUsage.heapUsed / memUsage.heapTotal;

      let newLevel = 'normal';

      // Determine system level
      if (
        systemLoad >= BACKPRESSURE_CONFIG.systemLoad.maximum ||
        queueDepth >= BACKPRESSURE_CONFIG.queueDepth.maximum ||
        memoryUsage >= 0.95
      ) {
        newLevel = 'overload';
      } else if (
        systemLoad >= BACKPRESSURE_CONFIG.systemLoad.critical ||
        queueDepth >= BACKPRESSURE_CONFIG.queueDepth.critical ||
        memoryUsage >= 0.85
      ) {
        newLevel = 'critical';
      } else if (
        systemLoad >= BACKPRESSURE_CONFIG.systemLoad.warning ||
        queueDepth >= BACKPRESSURE_CONFIG.queueDepth.warning ||
        memoryUsage >= 0.70
      ) {
        newLevel = 'warning';
      }

      const previousLevel = this.systemState.level;
      this.systemState = {
        level: newLevel,
        queueDepth,
        systemLoad,
        memoryUsage,
        lastCheck: Date.now(),
        lastLoadShedding: this.systemState.lastLoadShedding,
      };

      // Emit event on level change
      if (previousLevel !== newLevel) {
        this.emit('level-change', { from: previousLevel, to: newLevel, state: this.systemState });

        // Start cooldown on overload
        if (newLevel === 'overload' || newLevel === 'critical') {
          this.startCooldown();
        }
      }

      // Record metrics
      if (metrics?.backpressureSystemLoadGauge) {
        metrics.backpressureSystemLoadGauge.record(systemLoad, { level: newLevel });
      }

      span.setAttributes({
        'backpressure.level': newLevel,
        'backpressure.queue_depth': queueDepth,
        'backpressure.system_load': systemLoad,
        'backpressure.memory_usage': memoryUsage,
      });
      span.setStatus({ code: SpanStatusCode.OK });

    } catch (err) {
      span.recordException(err);
      span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });
    } finally {
      span.end();
    }
  }

  /**
   * Start cooldown period after load shedding
   */
  startCooldown() {
    if (this.inCooldown) return;

    this.inCooldown = true;
    this.systemState.lastLoadShedding = Date.now();

    console.warn('[Backpressure] Entering cooldown mode -', this.systemState.level);

    setTimeout(() => {
      this.inCooldown = false;
      console.log('[Backpressure] Exiting cooldown mode');
    }, BACKPRESSURE_CONFIG.recovery.cooldownMs);
  }

  /**
   * Start monitoring system state
   */
  startMonitoring() {
    // Regular state updates
    setInterval(() => {
      this.updateSystemState();
    }, BACKPRESSURE_CONFIG.recovery.checkIntervalMs);

    // Log system state changes
    this.on('level-change', ({ from, to, state }) => {
      console.warn(`[Backpressure] System level changed: ${from} → ${to}`, {
        queueDepth: state.queueDepth,
        systemLoad: state.systemLoad.toFixed(2),
        memoryUsage: state.memoryUsage.toFixed(2),
      });
    });
  }

  /**
   * Get current status
   */
  getStatus() {
    const queues = Array.from(this.queues.entries()).map(([name, queue]) => ({
      name,
      depth: queue.depth,
      maxDepth: queue.maxDepth,
      totalProcessed: queue.totalProcessed,
      totalRejected: queue.totalRejected,
      utilizationPct: ((queue.depth / queue.maxDepth) * 100).toFixed(1),
    }));

    return {
      systemState: this.systemState,
      inCooldown: this.inCooldown,
      queues,
      totalQueueDepth: this.getTotalQueueDepth(),
    };
  }
}

// Global backpressure manager instance
const backpressureManager = new BackpressureManager();

/**
 * Backpressure middleware
 */
export function backpressureMiddleware(req, res, next) {
  const priority = req.headers['x-priority'] || 'normal';
  const queueName = req.path.startsWith('/api/admin') ? 'admin' : 'default';

  const result = backpressureManager.enqueue(queueName, req, priority);

  if (!result.queued) {
    return res.status(503).json({
      error: 'Service Unavailable',
      message: result.reason,
      systemState: result.systemState || backpressureManager.systemState.level,
      retryAfter: result.retryAfter || 5,
    });
  }

  // Process request
  res.on('finish', () => {
    backpressureManager.dequeue(queueName);
  });

  next();
}

/**
 * Get backpressure status
 */
export function getBackpressureStatus() {
  return backpressureManager.getStatus();
}

/**
 * Register a custom queue
 */
export function registerQueue(queueName, maxDepth) {
  backpressureManager.registerQueue(queueName, maxDepth);
}

export { backpressureManager };
export default backpressureMiddleware;
