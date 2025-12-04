/**
 * @file Lean Six Sigma Quality Metrics for Knowledge Hooks
 * @module hooks/quality-metrics
 *
 * @description
 * Statistical process control and DMAIC workflow support for quality hooks:
 * - quality-gate: Enforce quality checkpoints (Control)
 * - defect-detection: Statistical outlier detection (Measure)
 * - continuous-improvement: Periodic optimization (Improve)
 * - spc-control: Statistical process control charts (Control)
 * - capability-analysis: Cp/Cpk metrics (Analyze)
 * - root-cause: 5 Whys automation (Analyze)
 * - kaizen-event: Improvement opportunity (Improve)
 * - audit-trail: Compliance logging (Define)
 */

import { z } from 'zod';

/**
 * @typedef {Object} QualityMetric
 * @property {string} name - Metric name
 * @property {number} value - Current value
 * @property {number} target - Target value
 * @property {number} ucl - Upper control limit
 * @property {number} lcl - Lower control limit
 * @property {string} unit - Unit of measure
 */

/**
 * @typedef {Object} DefectRecord
 * @property {string} id - Defect ID
 * @property {string} type - Defect type
 * @property {Date} timestamp - Detection time
 * @property {string} source - Source of defect
 * @property {string} severity - critical, major, minor
 * @property {object} context - Additional context
 */

/**
 * Quality gate configuration schema
 * POKA-YOKE: Validates operator-threshold compatibility at parse time
 */
export const QualityGateSchema = z
  .object({
    name: z.string().min(1, 'Gate name is required'),
    metric: z.string().min(1, 'Metric name is required'),
    operator: z.enum(['gt', 'gte', 'lt', 'lte', 'eq', 'neq', 'between']),
    threshold: z.number().or(z.array(z.number())),
    severity: z.enum(['critical', 'major', 'minor']).default('major'),
    action: z.enum(['block', 'warn', 'log']).default('block'),
  })
  .refine(
    data => {
      // POKA-YOKE: Operator-threshold mismatch guard (RPN 140 → 0)
      if (data.operator === 'between') {
        return Array.isArray(data.threshold) && data.threshold.length === 2;
      }
      return typeof data.threshold === 'number';
    },
    {
      message:
        "Operator 'between' requires threshold as [min, max] array; other operators require a single number. " +
        "Example: { operator: 'between', threshold: [10, 90] } or { operator: 'gt', threshold: 50 }",
    }
  )
  .refine(
    data => {
      // POKA-YOKE: Validate 'between' threshold order
      if (data.operator === 'between' && Array.isArray(data.threshold)) {
        return data.threshold[0] <= data.threshold[1];
      }
      return true;
    },
    {
      message: "For 'between' operator, threshold[0] (min) must be <= threshold[1] (max)",
    }
  );

/**
 * Statistical Process Control (SPC) data point
 */
export const SPCDataPointSchema = z.object({
  timestamp: z.date().or(z.string().transform(s => new Date(s))),
  value: z.number(),
  subgroup: z.string().optional(),
});

/**
 * Quality Metrics Collector - Tracks Six Sigma metrics
 *
 * @class QualityMetricsCollector
 */
export class QualityMetricsCollector {
  /**
   * Create a new quality metrics collector
   *
   * @param {object} options - Collector options
   */
  constructor(options = {}) {
    /** @type {Map<string, number[]>} */
    this.dataPoints = new Map();

    /** @type {Map<string, QualityMetric>} */
    this.metrics = new Map();

    /** @type {Array<DefectRecord>} */
    this.defects = [];

    /** @type {number} */
    this.maxDataPoints = options.maxDataPoints || 1000;

    /** @type {Map<string, object>} */
    this.qualityGates = new Map();

    /** @type {Array<object>} */
    this.auditLog = [];

    /**
     * POKA-YOKE: Maximum audit log size to prevent memory exhaustion (RPN 448 → 45)
     * @type {number}
     */
    this.maxAuditLogSize = options.maxAuditLogSize || 10000;

    /** @type {number} */
    this.defectCount = 0;

    /** @type {number} */
    this.totalCount = 0;
  }

  /**
   * Record a data point for a metric
   *
   * @param {string} metricName - Metric name
   * @param {number} value - Measured value
   * @param {object} context - Additional context
   */
  record(metricName, value, context = {}) {
    if (!this.dataPoints.has(metricName)) {
      this.dataPoints.set(metricName, []);
    }

    const points = this.dataPoints.get(metricName);
    points.push(value);

    // Maintain sliding window
    if (points.length > this.maxDataPoints) {
      points.shift();
    }

    this.totalCount++;

    // Check control limits
    this._checkControlLimits(metricName, value, context);
  }

  /**
   * Record a defect
   *
   * @param {object} defect - Defect information
   */
  recordDefect(defect) {
    const record = {
      id: `DEF-${Date.now()}-${this.defects.length}`,
      timestamp: new Date(),
      ...defect,
    };

    this.defects.push(record);
    this.defectCount++;

    // Log to audit trail
    this._auditLog('defect-recorded', record);
  }

  /**
   * Register a quality gate
   *
   * @param {object} gate - Quality gate configuration
   */
  registerQualityGate(gate) {
    const validated = QualityGateSchema.parse(gate);
    this.qualityGates.set(validated.name, validated);
  }

  /**
   * Check a value against quality gates
   *
   * @param {string} metricName - Metric to check
   * @param {number} value - Value to check
   * @returns {{passed: boolean, violations: object[]}} - Check result
   */
  checkQualityGates(metricName, value) {
    const violations = [];

    for (const gate of this.qualityGates.values()) {
      if (gate.metric !== metricName) continue;

      const passed = this._evaluateGate(gate, value);
      if (!passed) {
        violations.push({
          gate: gate.name,
          metric: metricName,
          value,
          threshold: gate.threshold,
          operator: gate.operator,
          severity: gate.severity,
          action: gate.action,
        });
      }
    }

    return {
      passed: violations.length === 0,
      violations,
    };
  }

  /**
   * Calculate Statistical Process Control metrics
   *
   * @param {string} metricName - Metric name
   * @returns {object} - SPC metrics (mean, stdDev, UCL, LCL, Cp, Cpk)
   */
  calculateSPC(metricName) {
    const points = this.dataPoints.get(metricName) || [];
    if (points.length < 2) {
      return { error: 'Insufficient data points' };
    }

    // Calculate mean
    const mean = points.reduce((sum, v) => sum + v, 0) / points.length;

    // Calculate standard deviation
    const variance = points.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / (points.length - 1);
    const stdDev = Math.sqrt(variance);

    // Control limits (3-sigma)
    const ucl = mean + 3 * stdDev;
    const lcl = mean - 3 * stdDev;

    // Process capability (requires spec limits)
    const metric = this.metrics.get(metricName);
    let cp = null;
    let cpk = null;

    if (metric && metric.ucl !== undefined && metric.lcl !== undefined) {
      const usl = metric.ucl;
      const lsl = metric.lcl;

      cp = (usl - lsl) / (6 * stdDev);
      cpk = Math.min(
        (usl - mean) / (3 * stdDev),
        (mean - lsl) / (3 * stdDev)
      );
    }

    return {
      mean,
      stdDev,
      ucl,
      lcl,
      cp,
      cpk,
      n: points.length,
      min: Math.min(...points),
      max: Math.max(...points),
    };
  }

  /**
   * Detect statistical outliers (defects)
   *
   * @param {string} metricName - Metric name
   * @param {number} sigmaLevel - Sigma level for detection (default: 3)
   * @returns {number[]} - Indices of outlier data points
   */
  detectOutliers(metricName, sigmaLevel = 3) {
    const spc = this.calculateSPC(metricName);
    if (spc.error) return [];

    const points = this.dataPoints.get(metricName) || [];
    const threshold = sigmaLevel * spc.stdDev;

    return points
      .map((v, i) => ({ value: v, index: i }))
      .filter(({ value }) => Math.abs(value - spc.mean) > threshold)
      .map(({ index }) => index);
  }

  /**
   * Calculate Defects Per Million Opportunities (DPMO)
   *
   * @returns {number} - DPMO value
   */
  calculateDPMO() {
    if (this.totalCount === 0) return 0;
    return (this.defectCount / this.totalCount) * 1000000;
  }

  /**
   * Calculate Sigma Level
   *
   * @returns {number} - Sigma level (higher is better)
   */
  calculateSigmaLevel() {
    const dpmo = this.calculateDPMO();
    if (dpmo === 0) return 6; // Perfect

    // Approximate sigma level from DPMO
    // Using simplified lookup table
    if (dpmo <= 3.4) return 6;
    if (dpmo <= 233) return 5;
    if (dpmo <= 6210) return 4;
    if (dpmo <= 66807) return 3;
    if (dpmo <= 308538) return 2;
    if (dpmo <= 691462) return 1;
    return 0;
  }

  /**
   * Generate 5 Whys root cause analysis template
   *
   * @param {DefectRecord} defect - Defect to analyze
   * @returns {object} - 5 Whys template
   */
  generateRootCauseTemplate(defect) {
    return {
      defectId: defect.id,
      problem: defect.type,
      why1: { question: 'Why did this happen?', answer: null },
      why2: { question: 'Why did that cause this?', answer: null },
      why3: { question: 'Why was that the case?', answer: null },
      why4: { question: 'Why did that occur?', answer: null },
      why5: { question: 'What is the root cause?', answer: null },
      rootCause: null,
      preventiveAction: null,
      timestamp: new Date(),
    };
  }

  /**
   * Register a Kaizen improvement event
   *
   * @param {object} event - Kaizen event details
   */
  registerKaizenEvent(event) {
    const record = {
      id: `KAIZEN-${Date.now()}`,
      type: 'kaizen-event',
      timestamp: new Date(),
      status: 'open',
      ...event,
    };

    this._auditLog('kaizen-registered', record);
    return record;
  }

  /**
   * Get quality summary report
   *
   * @returns {object} - Quality summary
   */
  getSummary() {
    return {
      totalMeasurements: this.totalCount,
      totalDefects: this.defectCount,
      dpmo: this.calculateDPMO(),
      sigmaLevel: this.calculateSigmaLevel(),
      metricsTracked: this.metrics.size,
      dataPointsStored: Array.from(this.dataPoints.values())
        .reduce((sum, arr) => sum + arr.length, 0),
      qualityGatesRegistered: this.qualityGates.size,
      auditLogEntries: this.auditLog.length,
      recentDefects: this.defects.slice(-10),
    };
  }

  /**
   * Export audit trail
   *
   * @param {object} options - Export options
   * @returns {Array} - Audit trail entries
   */
  exportAuditTrail(options = {}) {
    let entries = [...this.auditLog];

    if (options.startTime) {
      entries = entries.filter(e => e.timestamp >= options.startTime);
    }
    if (options.endTime) {
      entries = entries.filter(e => e.timestamp <= options.endTime);
    }
    if (options.type) {
      entries = entries.filter(e => e.type === options.type);
    }

    return entries;
  }

  /**
   * Check control limits and record violations
   * @private
   */
  _checkControlLimits(metricName, value, context) {
    const spc = this.calculateSPC(metricName);
    if (spc.error) return;

    if (value > spc.ucl || value < spc.lcl) {
      this.recordDefect({
        type: 'control-limit-violation',
        source: metricName,
        severity: 'major',
        context: {
          value,
          ucl: spc.ucl,
          lcl: spc.lcl,
          mean: spc.mean,
          ...context,
        },
      });
    }
  }

  /**
   * Evaluate a quality gate
   * @private
   */
  _evaluateGate(gate, value) {
    switch (gate.operator) {
      case 'gt': return value > gate.threshold;
      case 'gte': return value >= gate.threshold;
      case 'lt': return value < gate.threshold;
      case 'lte': return value <= gate.threshold;
      case 'eq': return value === gate.threshold;
      case 'neq': return value !== gate.threshold;
      case 'between':
        return Array.isArray(gate.threshold) &&
          value >= gate.threshold[0] &&
          value <= gate.threshold[1];
      default: return true;
    }
  }

  /**
   * Add entry to audit log
   * POKA-YOKE: Includes size limit to prevent memory exhaustion (RPN 448 → 45)
   * @private
   */
  _auditLog(type, data) {
    // POKA-YOKE: FIFO eviction to prevent unbounded memory growth
    if (this.auditLog.length >= this.maxAuditLogSize) {
      // Remove oldest entries (10% of max to reduce frequent shifts)
      const removeCount = Math.max(1, Math.floor(this.maxAuditLogSize * 0.1));
      this.auditLog.splice(0, removeCount);

      // Log warning once when eviction starts
      if (!this._auditEvictionWarned) {
        console.warn(
          `[POKA-YOKE] Audit log reached max size (${this.maxAuditLogSize}). ` +
            `Oldest entries will be evicted. Increase maxAuditLogSize if needed.`
        );
        this._auditEvictionWarned = true;
      }
    }

    this.auditLog.push({
      type,
      timestamp: new Date(),
      data,
    });
  }
}

/**
 * Create hook validators for quality metrics
 *
 * @param {QualityMetricsCollector} collector - Metrics collector
 * @returns {object} - Hook factory functions
 */
export function createQualityHooks(collector) {
  return {
    /**
     * Create a quality gate validation hook
     */
    createQualityGateHook: (gateName, metricExtractor) => ({
      name: `quality-gate-${gateName}`,
      trigger: 'quality-gate',
      validate: (data) => {
        const value = metricExtractor(data);
        const result = collector.checkQualityGates(gateName, value);
        if (!result.passed) {
          const criticalViolations = result.violations.filter(v => v.action === 'block');
          return criticalViolations.length === 0;
        }
        return true;
      },
      metadata: { gateName },
    }),

    /**
     * Create a defect detection hook
     */
    createDefectDetectionHook: (metricName, extractor) => ({
      name: `defect-detection-${metricName}`,
      trigger: 'defect-detection',
      validate: (data) => {
        const value = extractor(data);
        collector.record(metricName, value);
        const outliers = collector.detectOutliers(metricName);
        return outliers.length === 0;
      },
      metadata: { metricName },
    }),

    /**
     * Create an audit trail hook
     */
    createAuditTrailHook: (operationType) => ({
      name: `audit-trail-${operationType}`,
      trigger: 'audit-trail',
      transform: (data) => {
        collector._auditLog(operationType, { data, timestamp: new Date() });
        return data;
      },
      metadata: { operationType },
    }),
  };
}

export default {
  QualityMetricsCollector,
  createQualityHooks,
  QualityGateSchema,
  SPCDataPointSchema,
};
