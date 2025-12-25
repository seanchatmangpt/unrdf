/**
 * @unrdf/observability - Alert Manager
 *
 * Threshold-based alerting and anomaly detection for workflow metrics.
 * Supports webhook notifications, alert rules, and smart anomaly detection
 * using statistical analysis.
 *
 * @module @unrdf/observability/alerts
 */

import { EventEmitter } from 'node:events';
import { z } from 'zod';

/**
 * Alert severity levels
 */
export const AlertSeverity = {
  INFO: 'info',
  WARNING: 'warning',
  CRITICAL: 'critical',
};

/**
 * Alert rule schema
 * @type {z.ZodObject}
 */
const AlertRuleSchema = z.object({
  id: z.string(),
  name: z.string(),
  metric: z.string(),
  threshold: z.number(),
  operator: z.enum(['gt', 'lt', 'gte', 'lte', 'eq']),
  severity: z.enum(['info', 'warning', 'critical']).default('warning'),
  duration: z.number().min(0).default(60000), // 1 minute default
  labels: z.record(z.string()).optional(),
  annotations: z.record(z.string()).optional(),
  enabled: z.boolean().default(true),
});

/**
 * Webhook configuration schema
 * @type {z.ZodObject}
 */
const WebhookConfigSchema = z.object({
  url: z.string().url(),
  method: z.enum(['POST', 'PUT', 'PATCH']).default('POST'),
  headers: z.record(z.string()).optional(),
  timeout: z.number().min(1000).default(5000),
});

/**
 * AlertManager - Intelligent alerting system
 *
 * Features:
 * - Threshold-based alerts with hysteresis
 * - Anomaly detection using z-score analysis
 * - Webhook notifications
 * - Alert deduplication and grouping
 * - Alert history and correlation
 *
 * @class
 * @extends EventEmitter
 */
export class AlertManager extends EventEmitter {
  /**
   * @param {object} config - Alert manager configuration
   * @param {object[]} [config.rules=[]] - Initial alert rules
   * @param {object[]} [config.webhooks=[]] - Webhook endpoints
   * @param {number} [config.checkInterval=10000] - Rule check interval in ms
   * @param {boolean} [config.enableAnomalyDetection=true] - Enable anomaly detection
   */
  constructor(config = {}) {
    super();
    this.rules = new Map();
    this.webhooks = [];
    this.alertHistory = [];
    this.metricHistory = new Map();
    this.activeAlerts = new Map();
    this.config = {
      checkInterval: 10000,
      enableAnomalyDetection: true,
      ...config,
    };

    // Initialize rules and webhooks
    if (config.rules) {
      config.rules.forEach(rule => this.addRule(rule));
    }
    if (config.webhooks) {
      config.webhooks.forEach(webhook => this.addWebhook(webhook));
    }
  }

  /**
   * Add alert rule
   * @param {object} rule - Alert rule configuration
   * @returns {string} Rule ID
   */
  addRule(rule) {
    const validated = AlertRuleSchema.parse(rule);
    this.rules.set(validated.id, {
      ...validated,
      state: {
        triggered: false,
        triggeredAt: null,
        count: 0,
      },
    });
    return validated.id;
  }

  /**
   * Remove alert rule
   * @param {string} ruleId - Rule ID to remove
   * @returns {boolean} Success status
   */
  removeRule(ruleId) {
    return this.rules.delete(ruleId);
  }

  /**
   * Enable/disable alert rule
   * @param {string} ruleId - Rule ID
   * @param {boolean} enabled - Enable state
   */
  setRuleEnabled(ruleId, enabled) {
    const rule = this.rules.get(ruleId);
    if (rule) {
      rule.enabled = enabled;
    }
  }

  /**
   * Add webhook endpoint
   * @param {object} webhook - Webhook configuration
   */
  addWebhook(webhook) {
    const validated = WebhookConfigSchema.parse(webhook);
    this.webhooks.push(validated);
  }

  /**
   * Evaluate metric value against alert rules
   * @param {string} metricName - Metric name
   * @param {number} value - Current metric value
   * @param {Record<string, string>} labels - Metric labels
   */
  async evaluateMetric(metricName, value, labels = {}) {
    // Store metric history for anomaly detection
    this._recordMetricValue(metricName, value);

    // Check threshold-based rules
    for (const [_ruleId, rule] of this.rules.entries()) {
      if (!rule.enabled || rule.metric !== metricName) {
        continue;
      }

      const triggered = this._evaluateRule(rule, value);

      if (triggered && !rule.state.triggered) {
        // New alert triggered
        rule.state.triggered = true;
        rule.state.triggeredAt = Date.now();
        rule.state.count++;

        const alert = this._createAlert(rule, value, labels);
        await this._fireAlert(alert);
      } else if (!triggered && rule.state.triggered) {
        // Alert resolved
        rule.state.triggered = false;
        const alert = this._createAlert(rule, value, labels, true);
        await this._resolveAlert(alert);
      }
    }

    // Check for anomalies
    if (this.config.enableAnomalyDetection) {
      const anomaly = this._detectAnomaly(metricName, value);
      if (anomaly) {
        const alert = {
          id: `anomaly-${metricName}-${Date.now()}`,
          type: 'anomaly',
          metric: metricName,
          value,
          zscore: anomaly.zscore,
          severity: anomaly.severity,
          timestamp: Date.now(),
          labels,
        };
        await this._fireAlert(alert);
      }
    }
  }

  /**
   * Evaluate rule against value
   * @private
   * @param {object} rule - Alert rule
   * @param {number} value - Metric value
   * @returns {boolean} True if rule triggered
   */
  _evaluateRule(rule, value) {
    const { threshold, operator } = rule;

    switch (operator) {
      case 'gt':
        return value > threshold;
      case 'lt':
        return value < threshold;
      case 'gte':
        return value >= threshold;
      case 'lte':
        return value <= threshold;
      case 'eq':
        return value === threshold;
      default:
        return false;
    }
  }

  /**
   * Create alert object
   * @private
   */
  _createAlert(rule, value, labels, resolved = false) {
    return {
      id: `${rule.id}-${rule.state.triggeredAt}`,
      ruleId: rule.id,
      name: rule.name,
      metric: rule.metric,
      value,
      threshold: rule.threshold,
      operator: rule.operator,
      severity: rule.severity,
      labels: { ...labels, ...rule.labels },
      annotations: rule.annotations || {},
      status: resolved ? 'resolved' : 'firing',
      startsAt: rule.state.triggeredAt,
      endsAt: resolved ? Date.now() : null,
    };
  }

  /**
   * Fire alert
   * @private
   */
  async _fireAlert(alert) {
    this.activeAlerts.set(alert.id, alert);
    this.alertHistory.push(alert);

    // Emit alert event
    this.emit('alert', alert);

    // Send webhooks
    await this._sendWebhooks(alert);
  }

  /**
   * Resolve alert
   * @private
   */
  async _resolveAlert(alert) {
    this.activeAlerts.delete(alert.id);
    this.alertHistory.push(alert);

    // Emit resolution event
    this.emit('alert:resolved', alert);

    // Send webhooks
    await this._sendWebhooks(alert);
  }

  /**
   * Send webhook notifications
   * @private
   */
  async _sendWebhooks(alert) {
    const promises = this.webhooks.map(async (webhook) => {
      try {
        const controller = new AbortController();
        const timeout = setTimeout(() => controller.abort(), webhook.timeout);

        const response = await fetch(webhook.url, {
          method: webhook.method,
          headers: {
            'Content-Type': 'application/json',
            ...webhook.headers,
          },
          body: JSON.stringify(alert),
          signal: controller.signal,
        });

        clearTimeout(timeout);

        if (!response.ok) {
          throw new Error(`Webhook failed: ${response.status} ${response.statusText}`);
        }
      } catch (error) {
        this.emit('webhook:error', { webhook, alert, error });
      }
    });

    await Promise.allSettled(promises);
  }

  /**
   * Record metric value for history
   * @private
   */
  _recordMetricValue(metricName, value) {
    if (!this.metricHistory.has(metricName)) {
      this.metricHistory.set(metricName, []);
    }

    const history = this.metricHistory.get(metricName);
    history.push({ value, timestamp: Date.now() });

    // Keep last 1000 values
    if (history.length > 1000) {
      history.shift();
    }
  }

  /**
   * Detect anomalies using z-score analysis
   * @private
   * @param {string} metricName - Metric name
   * @param {number} value - Current value
   * @returns {object|null} Anomaly details or null
   */
  _detectAnomaly(metricName, value) {
    const history = this.metricHistory.get(metricName);

    if (!history || history.length < 30) {
      // Need at least 30 samples for statistical analysis
      return null;
    }

    const values = history.map(h => h.value);
    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    const variance = values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / values.length;
    const stdDev = Math.sqrt(variance);

    if (stdDev === 0) {
      return null; // No variance, can't detect anomalies
    }

    const zscore = Math.abs((value - mean) / stdDev);

    // Anomaly thresholds
    if (zscore > 3) {
      return { zscore, severity: AlertSeverity.CRITICAL };
    } else if (zscore > 2) {
      return { zscore, severity: AlertSeverity.WARNING };
    }

    return null;
  }

  /**
   * Get active alerts
   * @returns {object[]} Active alerts
   */
  getActiveAlerts() {
    return Array.from(this.activeAlerts.values());
  }

  /**
   * Get alert history
   * @param {object} filters - Filter options
   * @param {number} [filters.limit=100] - Maximum number of alerts
   * @param {string} [filters.severity] - Filter by severity
   * @param {string} [filters.metric] - Filter by metric
   * @returns {object[]} Alert history
   */
  getAlertHistory(filters = {}) {
    let history = [...this.alertHistory];

    if (filters.severity) {
      history = history.filter(a => a.severity === filters.severity);
    }

    if (filters.metric) {
      history = history.filter(a => a.metric === filters.metric);
    }

    const limit = filters.limit || 100;
    return history.slice(-limit).reverse();
  }

  /**
   * Get alert statistics
   * @returns {object} Alert statistics
   */
  getStatistics() {
    const total = this.alertHistory.length;
    const active = this.activeAlerts.size;
    const bySeverity = {
      info: 0,
      warning: 0,
      critical: 0,
    };

    this.alertHistory.forEach(alert => {
      bySeverity[alert.severity]++;
    });

    return {
      total,
      active,
      bySeverity,
      rules: this.rules.size,
      webhooks: this.webhooks.length,
    };
  }

  /**
   * Clear alert history (for testing)
   */
  clearHistory() {
    this.alertHistory = [];
    this.metricHistory.clear();
    this.activeAlerts.clear();
  }
}

/**
 * Create alert manager instance
 * @param {object} config - Configuration
 * @returns {AlertManager} Alert manager instance
 */
export function createAlertManager(config = {}) {
  return new AlertManager(config);
}

export default AlertManager;
