/**
 * @file Alert Rules and Notifications System
 * @module @unrdf/daemon/monitoring/alerting
 * @description Threshold-based alerts, rate of change alerts, alert suppression/deduplication,
 * and notification channels (webhook, console) for daemon monitoring.
 */

import { z } from 'zod';

/**
 * Alert severity levels
 */
export const AlertSeverity = {
  INFO: 'info',
  WARNING: 'warning',
  ERROR: 'error',
  CRITICAL: 'critical',
};

/**
 * Alert state values
 */
export const AlertState = {
  PENDING: 'pending',
  FIRING: 'firing',
  RESOLVED: 'resolved',
};

/**
 * Threshold comparison operators
 */
export const ComparisonOperator = {
  GREATER_THAN: 'gt',
  GREATER_THAN_OR_EQUAL: 'gte',
  LESS_THAN: 'lt',
  LESS_THAN_OR_EQUAL: 'lte',
  EQUAL: 'eq',
  NOT_EQUAL: 'neq',
};

/**
 * Threshold alert rule schema
 */
export const ThresholdAlertRuleSchema = z.object({
  type: z.literal('threshold'),
  name: z.string().min(1),
  metric: z.string().min(1),
  operator: z.enum(['gt', 'gte', 'lt', 'lte', 'eq', 'neq']),
  threshold: z.number(),
  duration: z.number().int().min(0).default(0),
  severity: z.enum(['info', 'warning', 'error', 'critical']).default('warning'),
  labels: z.record(z.string(), z.string()).optional(),
  annotations: z.record(z.string(), z.string()).optional(),
});

/**
 * Rate of change alert rule schema
 */
export const RateOfChangeAlertRuleSchema = z.object({
  type: z.literal('rate_of_change'),
  name: z.string().min(1),
  metric: z.string().min(1),
  window: z.number().int().min(1000).default(60000),
  threshold: z.number(),
  operator: z.enum(['gt', 'gte', 'lt', 'lte']).default('gt'),
  severity: z.enum(['info', 'warning', 'error', 'critical']).default('warning'),
  labels: z.record(z.string(), z.string()).optional(),
  annotations: z.record(z.string(), z.string()).optional(),
});

/**
 * Combined alert rule schema
 */
export const AlertRuleSchema = z.discriminatedUnion('type', [
  ThresholdAlertRuleSchema,
  RateOfChangeAlertRuleSchema,
]);

/**
 * Alert instance schema
 */
export const AlertSchema = z.object({
  id: z.string().min(1),
  ruleName: z.string().min(1),
  state: z.enum(['pending', 'firing', 'resolved']),
  severity: z.enum(['info', 'warning', 'error', 'critical']),
  value: z.number(),
  threshold: z.number(),
  metric: z.string().min(1),
  message: z.string(),
  labels: z.record(z.string(), z.string()).default({}),
  annotations: z.record(z.string(), z.string()).default({}),
  startsAt: z.date(),
  endsAt: z.date().optional(),
  updatedAt: z.date(),
  fingerprint: z.string().min(1),
});

/**
 * Notification channel configuration schema
 */
export const WebhookChannelSchema = z.object({
  type: z.literal('webhook'),
  name: z.string().min(1),
  url: z.string().url(),
  method: z.enum(['GET', 'POST', 'PUT']).default('POST'),
  headers: z.record(z.string(), z.string()).optional(),
  timeout: z.number().int().min(1000).default(10000),
  retries: z.number().int().min(0).default(3),
});

export const ConsoleChannelSchema = z.object({
  type: z.literal('console'),
  name: z.string().min(1),
  format: z.enum(['json', 'text']).default('text'),
  level: z.enum(['debug', 'info', 'warn', 'error']).default('warn'),
});

export const NotificationChannelSchema = z.discriminatedUnion('type', [
  WebhookChannelSchema,
  ConsoleChannelSchema,
]);

/**
 * Alerting configuration schema
 */
export const AlertingConfigSchema = z.object({
  evaluationInterval: z.number().int().min(1000).default(15000),
  suppressionDuration: z.number().int().min(0).default(300000),
  deduplicationWindow: z.number().int().min(0).default(60000),
  maxAlertsPerRule: z.number().int().min(1).default(100),
  historyRetention: z.number().int().min(60000).default(86400000),
});

/**
 * Alert manager for handling alert rules and notifications
 */
export class AlertManager {
  /** @type {import('zod').infer<typeof AlertingConfigSchema>} */
  #config;

  /** @type {Map<string, import('zod').infer<typeof AlertRuleSchema>>} */
  #rules = new Map();

  /** @type {Map<string, import('zod').infer<typeof NotificationChannelSchema>>} */
  #channels = new Map();

  /** @type {Map<string, import('zod').infer<typeof AlertSchema>>} */
  #activeAlerts = new Map();

  /** @type {Array<import('zod').infer<typeof AlertSchema>>} */
  #alertHistory = [];

  /** @type {Map<string, number>} */
  #suppressedAlerts = new Map();

  /** @type {Map<string, Array<{ timestamp: number, value: number }>>} */
  #metricHistory = new Map();

  /** @type {NodeJS.Timeout | null} */
  #evaluationTimer = null;

  /** @type {Function | null} */
  #metricProvider = null;

  /**
   * Create an alert manager
   * @param {Partial<import('zod').infer<typeof AlertingConfigSchema>>} [config]
   */
  constructor(config = {}) {
    this.#config = AlertingConfigSchema.parse(config);
  }

  /**
   * Set the metric provider function
   * @param {(metricName: string) => number | null} provider - Function that returns metric values
   */
  setMetricProvider(provider) {
    if (typeof provider !== 'function') {
      throw new Error('Metric provider must be a function');
    }
    this.#metricProvider = provider;
  }

  /**
   * Start alert evaluation loop
   */
  start() {
    this.#evaluationTimer = setInterval(() => {
      this.evaluate();
    }, this.#config.evaluationInterval);
  }

  /**
   * Stop alert evaluation loop
   */
  stop() {
    if (this.#evaluationTimer) {
      clearInterval(this.#evaluationTimer);
      this.#evaluationTimer = null;
    }
  }

  /**
   * Register an alert rule
   * @param {import('zod').infer<typeof AlertRuleSchema>} rule
   * @throws {Error} If rule name already exists
   */
  registerRule(rule) {
    const validated = AlertRuleSchema.parse(rule);
    if (this.#rules.has(validated.name)) {
      throw new Error(`Alert rule "${validated.name}" already registered`);
    }
    this.#rules.set(validated.name, validated);
  }

  /**
   * Unregister an alert rule
   * @param {string} name - Rule name
   * @returns {boolean} True if rule was removed
   */
  unregisterRule(name) {
    return this.#rules.delete(name);
  }

  /**
   * Register a notification channel
   * @param {import('zod').infer<typeof NotificationChannelSchema>} channel
   * @throws {Error} If channel name already exists
   */
  registerChannel(channel) {
    const validated = NotificationChannelSchema.parse(channel);
    if (this.#channels.has(validated.name)) {
      throw new Error(`Notification channel "${validated.name}" already registered`);
    }
    this.#channels.set(validated.name, validated);
  }

  /**
   * Unregister a notification channel
   * @param {string} name - Channel name
   * @returns {boolean} True if channel was removed
   */
  unregisterChannel(name) {
    return this.#channels.delete(name);
  }

  /**
   * Evaluate all alert rules
   * @returns {Array<import('zod').infer<typeof AlertSchema>>} Newly fired or resolved alerts
   */
  evaluate() {
    const changedAlerts = [];

    for (const [name, rule] of this.#rules) {
      const alerts = this.#evaluateRule(rule);
      changedAlerts.push(...alerts);
    }

    this.#cleanupHistory();
    return changedAlerts;
  }

  /**
   * Manually push a metric value for evaluation
   * @param {string} metricName - Metric name
   * @param {number} value - Metric value
   */
  pushMetric(metricName, value) {
    if (!this.#metricHistory.has(metricName)) {
      this.#metricHistory.set(metricName, []);
    }

    const history = this.#metricHistory.get(metricName);
    history.push({ timestamp: Date.now(), value });

    // Keep only relevant history
    const cutoff = Date.now() - Math.max(...[...this.#rules.values()]
      .filter((r) => r.type === 'rate_of_change')
      .map((r) => r.window || 60000));

    while (history.length > 0 && history[0].timestamp < cutoff) {
      history.shift();
    }
  }

  /**
   * Get all active alerts
   * @returns {Array<import('zod').infer<typeof AlertSchema>>}
   */
  getActiveAlerts() {
    return [...this.#activeAlerts.values()];
  }

  /**
   * Get alerts by severity
   * @param {string} severity - Alert severity
   * @returns {Array<import('zod').infer<typeof AlertSchema>>}
   */
  getAlertsBySeverity(severity) {
    return [...this.#activeAlerts.values()].filter((a) => a.severity === severity);
  }

  /**
   * Get alert history
   * @param {number} [limit=100] - Maximum entries to return
   * @returns {Array<import('zod').infer<typeof AlertSchema>>}
   */
  getAlertHistory(limit = 100) {
    return this.#alertHistory.slice(-limit);
  }

  /**
   * Suppress an alert by fingerprint
   * @param {string} fingerprint - Alert fingerprint
   * @param {number} [duration] - Suppression duration in ms
   */
  suppressAlert(fingerprint, duration = this.#config.suppressionDuration) {
    this.#suppressedAlerts.set(fingerprint, Date.now() + duration);
  }

  /**
   * Check if an alert is suppressed
   * @param {string} fingerprint - Alert fingerprint
   * @returns {boolean}
   */
  isSuppressed(fingerprint) {
    const expiresAt = this.#suppressedAlerts.get(fingerprint);
    if (!expiresAt) return false;
    if (Date.now() > expiresAt) {
      this.#suppressedAlerts.delete(fingerprint);
      return false;
    }
    return true;
  }

  /**
   * Acknowledge (resolve) an alert
   * @param {string} alertId - Alert ID
   * @returns {boolean} True if alert was acknowledged
   */
  acknowledgeAlert(alertId) {
    const alert = this.#activeAlerts.get(alertId);
    if (!alert) return false;

    const resolved = {
      ...alert,
      state: AlertState.RESOLVED,
      endsAt: new Date(),
      updatedAt: new Date(),
    };

    this.#activeAlerts.delete(alertId);
    this.#alertHistory.push(resolved);
    this.#notify(resolved);

    return true;
  }

  /**
   * Get registered rules
   * @returns {Array<import('zod').infer<typeof AlertRuleSchema>>}
   */
  getRegisteredRules() {
    return [...this.#rules.values()];
  }

  /**
   * Get registered channels
   * @returns {Array<import('zod').infer<typeof NotificationChannelSchema>>}
   */
  getRegisteredChannels() {
    return [...this.#channels.values()];
  }

  // Private methods

  #evaluateRule(rule) {
    const changedAlerts = [];
    const currentValue = this.#getMetricValue(rule.metric);

    if (currentValue === null) return changedAlerts;

    let shouldFire = false;

    if (rule.type === 'threshold') {
      shouldFire = this.#evaluateThreshold(currentValue, rule.operator, rule.threshold);
    } else if (rule.type === 'rate_of_change') {
      const rate = this.#calculateRateOfChange(rule.metric, rule.window);
      if (rate !== null) {
        shouldFire = this.#evaluateThreshold(rate, rule.operator, rule.threshold);
      }
    }

    const fingerprint = this.#generateFingerprint(rule);
    const existingAlert = [...this.#activeAlerts.values()].find(
      (a) => a.fingerprint === fingerprint
    );

    if (shouldFire) {
      if (!existingAlert) {
        if (!this.isSuppressed(fingerprint) && !this.#isDuplicate(fingerprint)) {
          const alert = this.#createAlert(rule, currentValue, fingerprint);
          this.#activeAlerts.set(alert.id, alert);
          this.#alertHistory.push(alert);
          changedAlerts.push(alert);
          this.#notify(alert);
        }
      }
    } else if (existingAlert) {
      const resolved = {
        ...existingAlert,
        state: AlertState.RESOLVED,
        value: currentValue,
        endsAt: new Date(),
        updatedAt: new Date(),
      };
      this.#activeAlerts.delete(existingAlert.id);
      this.#alertHistory.push(resolved);
      changedAlerts.push(resolved);
      this.#notify(resolved);
    }

    return changedAlerts;
  }

  #evaluateThreshold(value, operator, threshold) {
    switch (operator) {
      case ComparisonOperator.GREATER_THAN:
        return value > threshold;
      case ComparisonOperator.GREATER_THAN_OR_EQUAL:
        return value >= threshold;
      case ComparisonOperator.LESS_THAN:
        return value < threshold;
      case ComparisonOperator.LESS_THAN_OR_EQUAL:
        return value <= threshold;
      case ComparisonOperator.EQUAL:
        return value === threshold;
      case ComparisonOperator.NOT_EQUAL:
        return value !== threshold;
      default:
        return false;
    }
  }

  #calculateRateOfChange(metricName, window) {
    const history = this.#metricHistory.get(metricName);
    if (!history || history.length < 2) return null;

    const now = Date.now();
    const windowStart = now - window;
    const relevantPoints = history.filter((p) => p.timestamp >= windowStart);

    if (relevantPoints.length < 2) return null;

    const first = relevantPoints[0];
    const last = relevantPoints[relevantPoints.length - 1];
    const timeDelta = (last.timestamp - first.timestamp) / 1000;

    if (timeDelta === 0) return null;

    return (last.value - first.value) / timeDelta;
  }

  #getMetricValue(metricName) {
    if (this.#metricProvider) {
      return this.#metricProvider(metricName);
    }

    const history = this.#metricHistory.get(metricName);
    if (!history || history.length === 0) return null;

    return history[history.length - 1].value;
  }

  #generateFingerprint(rule) {
    const labelStr = rule.labels ? JSON.stringify(rule.labels) : '';
    return `${rule.name}:${rule.metric}:${labelStr}`;
  }

  #isDuplicate(fingerprint) {
    const cutoff = Date.now() - this.#config.deduplicationWindow;
    return this.#alertHistory.some(
      (a) => a.fingerprint === fingerprint && a.startsAt.getTime() > cutoff
    );
  }

  #createAlert(rule, value, fingerprint) {
    const now = new Date();
    return AlertSchema.parse({
      id: `alert-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`,
      ruleName: rule.name,
      state: AlertState.FIRING,
      severity: rule.severity,
      value,
      threshold: rule.threshold,
      metric: rule.metric,
      message: this.#formatAlertMessage(rule, value),
      labels: rule.labels || {},
      annotations: rule.annotations || {},
      startsAt: now,
      updatedAt: now,
      fingerprint,
    });
  }

  #formatAlertMessage(rule, value) {
    if (rule.type === 'threshold') {
      return `${rule.metric} is ${value} which is ${rule.operator} ${rule.threshold}`;
    }
    return `${rule.metric} rate of change is ${value}/s which exceeds threshold ${rule.threshold}/s`;
  }

  async #notify(alert) {
    for (const [_, channel] of this.#channels) {
      try {
        await this.#sendNotification(channel, alert);
      } catch (error) {
        console.error(`Failed to send notification to ${channel.name}:`, error.message);
      }
    }
  }

  async #sendNotification(channel, alert) {
    if (channel.type === 'console') {
      this.#sendConsoleNotification(channel, alert);
    } else if (channel.type === 'webhook') {
      await this.#sendWebhookNotification(channel, alert);
    }
  }

  #sendConsoleNotification(channel, alert) {
    const logFn = console[channel.level] || console.log;
    if (channel.format === 'json') {
      logFn(JSON.stringify(alert, null, 2));
    } else {
      const stateIcon = alert.state === AlertState.FIRING ? '[FIRING]' : '[RESOLVED]';
      const severityIcon = this.#getSeverityIcon(alert.severity);
      logFn(
        `${stateIcon} ${severityIcon} ${alert.ruleName}: ${alert.message}`
      );
    }
  }

  async #sendWebhookNotification(channel, alert) {
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), channel.timeout);

    try {
      const response = await fetch(channel.url, {
        method: channel.method,
        headers: {
          'Content-Type': 'application/json',
          ...channel.headers,
        },
        body: JSON.stringify({
          version: '1',
          groupKey: alert.fingerprint,
          status: alert.state,
          alerts: [alert],
        }),
        signal: controller.signal,
      });

      if (!response.ok) {
        throw new Error(`Webhook returned ${response.status}: ${response.statusText}`);
      }
    } finally {
      clearTimeout(timeoutId);
    }
  }

  #getSeverityIcon(severity) {
    switch (severity) {
      case AlertSeverity.CRITICAL:
        return '[CRIT]';
      case AlertSeverity.ERROR:
        return '[ERR]';
      case AlertSeverity.WARNING:
        return '[WARN]';
      default:
        return '[INFO]';
    }
  }

  #cleanupHistory() {
    const cutoff = Date.now() - this.#config.historyRetention;
    this.#alertHistory = this.#alertHistory.filter((a) => a.startsAt.getTime() > cutoff);

    // Cleanup expired suppressions
    for (const [fingerprint, expiresAt] of this.#suppressedAlerts) {
      if (Date.now() > expiresAt) {
        this.#suppressedAlerts.delete(fingerprint);
      }
    }
  }
}

/**
 * Create a preconfigured alert manager
 * @param {Partial<import('zod').infer<typeof AlertingConfigSchema>>} [config]
 * @returns {AlertManager}
 */
export function createAlertManager(config = {}) {
  return new AlertManager(config);
}

export default AlertManager;
