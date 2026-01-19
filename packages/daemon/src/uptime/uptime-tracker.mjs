/**
 * @file Daemon Uptime Tracker
 * @module @unrdf/daemon/uptime/uptime-tracker
 * @description Tracks daemon uptime with precision, calculates availability percentages,
 * supports rolling window calculations, and handles maintenance window exclusions.
 */

import { z } from 'zod';

/**
 * Time period schema for uptime/downtime intervals
 */
export const TimePeriodSchema = z.object({
  start: z.number().int().positive(),
  end: z.number().int().positive().optional(),
  type: z.enum(['up', 'down', 'maintenance']),
  reason: z.string().optional(),
});

/**
 * Maintenance window schema
 */
export const MaintenanceWindowSchema = z.object({
  id: z.string().min(1),
  start: z.number().int().positive(),
  end: z.number().int().positive(),
  description: z.string().optional(),
  excludeFromSLA: z.boolean().default(true),
});

/**
 * Uptime tracker configuration schema
 */
export const UptimeTrackerConfigSchema = z.object({
  serviceName: z.string().default('unrdf-daemon'),
  rollingWindowMs: z.number().int().positive().default(86400000), // 24 hours
  historyRetentionMs: z.number().int().positive().default(2592000000), // 30 days
  precisionMs: z.number().int().positive().default(1000), // 1 second
  checkIntervalMs: z.number().int().positive().default(10000), // 10 seconds
});

/**
 * Rolling window type enum
 */
export const RollingWindowType = {
  HOURLY: 3600000,
  DAILY: 86400000,
  WEEKLY: 604800000,
  MONTHLY: 2592000000,
};

/**
 * Availability result schema
 */
export const AvailabilityResultSchema = z.object({
  availability: z.number().min(0).max(100),
  uptimeMs: z.number().int().min(0),
  downtimeMs: z.number().int().min(0),
  maintenanceMs: z.number().int().min(0),
  totalMs: z.number().int().min(0),
  periodStart: z.number().int().positive(),
  periodEnd: z.number().int().positive(),
  nines: z.string(),
});

/**
 * Calculate availability nines string (e.g., "99.9%", "99.99%")
 * @param {number} availability - Availability percentage
 * @returns {string} Nines representation
 */
function calculateNines(availability) {
  if (availability >= 99.999) return '99.999% (5 nines)';
  if (availability >= 99.99) return '99.99% (4 nines)';
  if (availability >= 99.9) return '99.9% (3 nines)';
  if (availability >= 99) return '99% (2 nines)';
  return `${availability.toFixed(2)}%`;
}

/**
 * Uptime tracker for monitoring daemon availability
 * Tracks uptime/downtime periods with millisecond precision
 */
export class UptimeTracker {
  /**
   * Create uptime tracker instance
   * @param {Object} config - Tracker configuration
   * @param {string} [config.serviceName='unrdf-daemon'] - Service name
   * @param {number} [config.rollingWindowMs=86400000] - Rolling window in ms
   * @param {number} [config.historyRetentionMs=2592000000] - History retention
   * @param {number} [config.precisionMs=1000] - Precision in ms
   * @param {number} [config.checkIntervalMs=10000] - Check interval in ms
   * @example
   * const tracker = new UptimeTracker({ serviceName: 'my-daemon' });
   */
  constructor(config = {}) {
    this.config = UptimeTrackerConfigSchema.parse(config);

    /** @type {Array<z.infer<typeof TimePeriodSchema>>} */
    this.periods = [];

    /** @type {Map<string, z.infer<typeof MaintenanceWindowSchema>>} */
    this.maintenanceWindows = new Map();

    /** @type {z.infer<typeof TimePeriodSchema>|null} */
    this.currentPeriod = null;

    this.startTime = Date.now();
    this.checkInterval = null;
  }

  /**
   * Start tracking uptime
   * @returns {void}
   * @example
   * tracker.start();
   */
  start() {
    if (this.currentPeriod) {
      return;
    }

    const now = Date.now();
    this.currentPeriod = {
      start: now,
      end: undefined,
      type: 'up',
    };
    this.startTime = now;
  }

  /**
   * Stop tracking and mark as down
   * @param {string} [reason] - Reason for stopping
   * @returns {void}
   * @example
   * tracker.stop('Graceful shutdown');
   */
  stop(reason) {
    if (!this.currentPeriod) {
      return;
    }

    const now = Date.now();
    this.currentPeriod.end = now;
    this.periods.push(TimePeriodSchema.parse(this.currentPeriod));
    this.currentPeriod = {
      start: now,
      end: undefined,
      type: 'down',
      reason,
    };
  }

  /**
   * Record a downtime event
   * @param {number} startMs - Downtime start timestamp
   * @param {number} endMs - Downtime end timestamp
   * @param {string} [reason] - Downtime reason
   * @returns {void}
   * @throws {Error} If start >= end
   * @example
   * tracker.recordDowntime(Date.now() - 60000, Date.now(), 'Network issue');
   */
  recordDowntime(startMs, endMs, reason) {
    if (startMs >= endMs) {
      throw new Error('Downtime start must be before end');
    }

    // Close current period if needed
    if (this.currentPeriod && this.currentPeriod.type === 'up') {
      this.currentPeriod.end = startMs;
      this.periods.push(TimePeriodSchema.parse(this.currentPeriod));
    }

    // Record downtime
    this.periods.push(TimePeriodSchema.parse({
      start: startMs,
      end: endMs,
      type: 'down',
      reason,
    }));

    // Resume uptime
    this.currentPeriod = {
      start: endMs,
      end: undefined,
      type: 'up',
    };

    this.pruneHistory();
  }

  /**
   * Schedule a maintenance window
   * @param {string} id - Unique window identifier
   * @param {number} startMs - Window start timestamp
   * @param {number} endMs - Window end timestamp
   * @param {string} [description] - Window description
   * @param {boolean} [excludeFromSLA=true] - Exclude from SLA calculations
   * @returns {z.infer<typeof MaintenanceWindowSchema>}
   * @throws {Error} If start >= end or window overlaps existing
   * @example
   * tracker.scheduleMaintenanceWindow('mw-1', start, end, 'Database upgrade');
   */
  scheduleMaintenanceWindow(id, startMs, endMs, description, excludeFromSLA = true) {
    if (startMs >= endMs) {
      throw new Error('Maintenance window start must be before end');
    }

    const window = MaintenanceWindowSchema.parse({
      id,
      start: startMs,
      end: endMs,
      description,
      excludeFromSLA,
    });

    this.maintenanceWindows.set(id, window);
    return window;
  }

  /**
   * Cancel a scheduled maintenance window
   * @param {string} id - Window identifier
   * @returns {boolean} True if window was found and cancelled
   * @example
   * tracker.cancelMaintenanceWindow('mw-1');
   */
  cancelMaintenanceWindow(id) {
    return this.maintenanceWindows.delete(id);
  }

  /**
   * Get all scheduled maintenance windows
   * @returns {Array<z.infer<typeof MaintenanceWindowSchema>>}
   */
  getMaintenanceWindows() {
    return Array.from(this.maintenanceWindows.values());
  }

  /**
   * Check if a timestamp is within a maintenance window
   * @param {number} timestamp - Timestamp to check
   * @returns {z.infer<typeof MaintenanceWindowSchema>|null}
   */
  isInMaintenanceWindow(timestamp) {
    for (const window of this.maintenanceWindows.values()) {
      if (timestamp >= window.start && timestamp <= window.end) {
        return window;
      }
    }
    return null;
  }

  /**
   * Calculate availability for a time range
   * @param {number} startMs - Period start timestamp
   * @param {number} endMs - Period end timestamp
   * @param {boolean} [excludeMaintenance=true] - Exclude maintenance from downtime
   * @returns {z.infer<typeof AvailabilityResultSchema>}
   * @example
   * const result = tracker.calculateAvailability(
   *   Date.now() - 86400000,
   *   Date.now()
   * );
   */
  calculateAvailability(startMs, endMs, excludeMaintenance = true) {
    let uptimeMs = 0;
    let downtimeMs = 0;
    let maintenanceMs = 0;

    // Include current period in calculations
    const allPeriods = this.currentPeriod
      ? [...this.periods, { ...this.currentPeriod, end: Date.now() }]
      : [...this.periods];

    for (const period of allPeriods) {
      const periodStart = Math.max(period.start, startMs);
      const periodEnd = Math.min(period.end || Date.now(), endMs);

      if (periodStart >= periodEnd) {
        continue;
      }

      const duration = periodEnd - periodStart;

      // Check for maintenance window overlap
      let maintenanceDuration = 0;
      if (excludeMaintenance) {
        for (const window of this.maintenanceWindows.values()) {
          if (window.excludeFromSLA) {
            const overlapStart = Math.max(periodStart, window.start);
            const overlapEnd = Math.min(periodEnd, window.end);
            if (overlapStart < overlapEnd) {
              maintenanceDuration += overlapEnd - overlapStart;
            }
          }
        }
      }

      if (period.type === 'up') {
        uptimeMs += duration - maintenanceDuration;
        maintenanceMs += maintenanceDuration;
      } else if (period.type === 'down') {
        downtimeMs += duration - maintenanceDuration;
        maintenanceMs += maintenanceDuration;
      } else if (period.type === 'maintenance') {
        maintenanceMs += duration;
      }
    }

    const totalMs = endMs - startMs;
    const effectiveTotal = excludeMaintenance ? totalMs - maintenanceMs : totalMs;
    const availability = effectiveTotal > 0 ? (uptimeMs / effectiveTotal) * 100 : 100;

    return AvailabilityResultSchema.parse({
      availability,
      uptimeMs,
      downtimeMs,
      maintenanceMs,
      totalMs,
      periodStart: startMs,
      periodEnd: endMs,
      nines: calculateNines(availability),
    });
  }

  /**
   * Calculate rolling window availability
   * @param {number} windowType - Window type from RollingWindowType
   * @returns {z.infer<typeof AvailabilityResultSchema>}
   * @example
   * const hourly = tracker.calculateRollingAvailability(RollingWindowType.HOURLY);
   */
  calculateRollingAvailability(windowType) {
    const now = Date.now();
    return this.calculateAvailability(now - windowType, now);
  }

  /**
   * Get hourly availability for the last N hours
   * @param {number} [hours=24] - Number of hours
   * @returns {Array<z.infer<typeof AvailabilityResultSchema>>}
   * @example
   * const hourlyStats = tracker.getHourlyAvailability(24);
   */
  getHourlyAvailability(hours = 24) {
    const results = [];
    const now = Date.now();
    const hourMs = 3600000;

    for (let i = 0; i < hours; i++) {
      const endMs = now - (i * hourMs);
      const startMs = endMs - hourMs;
      results.unshift(this.calculateAvailability(startMs, endMs));
    }

    return results;
  }

  /**
   * Get daily availability for the last N days
   * @param {number} [days=30] - Number of days
   * @returns {Array<z.infer<typeof AvailabilityResultSchema>>}
   * @example
   * const dailyStats = tracker.getDailyAvailability(7);
   */
  getDailyAvailability(days = 30) {
    const results = [];
    const now = Date.now();
    const dayMs = 86400000;

    for (let i = 0; i < days; i++) {
      const endMs = now - (i * dayMs);
      const startMs = endMs - dayMs;
      results.unshift(this.calculateAvailability(startMs, endMs));
    }

    return results;
  }

  /**
   * Get monthly availability for the last N months
   * @param {number} [months=12] - Number of months
   * @returns {Array<z.infer<typeof AvailabilityResultSchema>>}
   * @example
   * const monthlyStats = tracker.getMonthlyAvailability(12);
   */
  getMonthlyAvailability(months = 12) {
    const results = [];
    const now = Date.now();
    const monthMs = 2592000000; // 30 days

    for (let i = 0; i < months; i++) {
      const endMs = now - (i * monthMs);
      const startMs = endMs - monthMs;
      results.unshift(this.calculateAvailability(startMs, endMs));
    }

    return results;
  }

  /**
   * Get current uptime in milliseconds
   * @returns {number} Current uptime in ms
   */
  getCurrentUptime() {
    if (!this.currentPeriod || this.currentPeriod.type !== 'up') {
      return 0;
    }
    return Date.now() - this.currentPeriod.start;
  }

  /**
   * Get total tracked uptime since start
   * @returns {number} Total uptime in ms
   */
  getTotalUptime() {
    return Date.now() - this.startTime;
  }

  /**
   * Get all periods within a time range
   * @param {number} startMs - Range start
   * @param {number} endMs - Range end
   * @returns {Array<z.infer<typeof TimePeriodSchema>>}
   */
  getPeriods(startMs, endMs) {
    return this.periods.filter(
      p => (p.end || Date.now()) >= startMs && p.start <= endMs
    );
  }

  /**
   * Get downtime periods within a time range
   * @param {number} startMs - Range start
   * @param {number} endMs - Range end
   * @returns {Array<z.infer<typeof TimePeriodSchema>>}
   */
  getDowntimePeriods(startMs, endMs) {
    return this.getPeriods(startMs, endMs).filter(p => p.type === 'down');
  }

  /**
   * Prune old history beyond retention period
   * @private
   */
  pruneHistory() {
    const cutoff = Date.now() - this.config.historyRetentionMs;
    this.periods = this.periods.filter(p => (p.end || Date.now()) > cutoff);

    // Clean expired maintenance windows
    for (const [id, window] of this.maintenanceWindows) {
      if (window.end < cutoff) {
        this.maintenanceWindows.delete(id);
      }
    }
  }

  /**
   * Export tracker state as JSON
   * @returns {Object} Serializable state
   */
  toJSON() {
    return {
      config: this.config,
      periods: this.periods,
      maintenanceWindows: Array.from(this.maintenanceWindows.entries()),
      currentPeriod: this.currentPeriod,
      startTime: this.startTime,
    };
  }

  /**
   * Restore tracker state from JSON
   * @param {Object} data - Serialized state
   * @returns {UptimeTracker}
   */
  static fromJSON(data) {
    const tracker = new UptimeTracker(data.config);
    tracker.periods = data.periods || [];
    tracker.currentPeriod = data.currentPeriod;
    tracker.startTime = data.startTime;

    if (data.maintenanceWindows) {
      tracker.maintenanceWindows = new Map(data.maintenanceWindows);
    }

    return tracker;
  }
}

/**
 * Create an uptime tracker instance
 * @param {Object} [config] - Tracker configuration
 * @returns {UptimeTracker}
 * @example
 * const tracker = createUptimeTracker({ serviceName: 'api-server' });
 */
export function createUptimeTracker(config = {}) {
  return new UptimeTracker(config);
}
