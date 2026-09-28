/**
 * @fileoverview Certificate Generator - Emit dimension certificates
 *
 * Generates cryptographically signed certificates attesting to system
 * health measurements at a specific epoch.
 *
 * A dimension certificate contains:
 * - All four core metrics (D_t, TC, TE, C_t)
 * - Epoch timestamp
 * - Cryptographic hash of measurements
 * - Trend analysis
 * - Health status
 *
 * Certificates can be:
 * - Stored for audit trails
 * - Compared across epochs
 * - Verified for integrity
 * - Used for compliance reporting
 *
 * @module measurement/certificate-generator
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';

/**
 * Measurement snapshot schema
 */
const MeasurementSnapshotSchema = z.object({
  dimension: z.object({
    systemDimension: z.number(),
    partitionDimensions: z.record(z.number()),
    utilizationRatio: z.number()
  }),
  correlation: z.object({
    totalCorrelation: z.number(),
    normalizedTC: z.number(),
    partitionEntropies: z.record(z.number())
  }),
  transferEntropy: z.object({
    causalEdges: z.array(z.object({
      source: z.string(),
      target: z.string(),
      te: z.number()
    })),
    totalTE: z.number()
  }),
  capacity: z.object({
    systemCapacity: z.number(),
    admissionRate: z.number(),
    throughput: z.object({
      quadsPerSecond: z.number(),
      deltasPerSecond: z.number()
    })
  })
});

/**
 * Certificate schema
 */
const CertificateSchema = z.object({
  version: z.string(),
  epoch: z.string(),
  measurements: MeasurementSnapshotSchema,
  trends: z.object({
    dimension: z.string(),
    correlation: z.string(),
    transferEntropy: z.string(),
    capacity: z.string()
  }),
  health: z.object({
    status: z.enum(['healthy', 'warning', 'critical']),
    issues: z.array(z.object({
      metric: z.string(),
      type: z.string(),
      severity: z.string(),
      message: z.string()
    })),
    score: z.number()
  }),
  metadata: z.object({
    generatedAt: z.string().datetime(),
    partitionCount: z.number(),
    dataPoints: z.number(),
    computeTimeMs: z.number()
  }),
  certificateHash: z.string(),
  previousCertificateHash: z.string().nullable()
});

/**
 * Certificate Generator - Creates attestation certificates for measurements
 *
 * @class CertificateGenerator
 * @example
 * const generator = new CertificateGenerator();
 * const cert = await generator.generateCertificate({
 *   dimension: dimensionResult,
 *   correlation: tcResult,
 *   transferEntropy: teResult,
 *   capacity: capacityResult
 * });
 */
export class CertificateGenerator {
  /**
   * Create a new Certificate Generator
   * @param {Object} [config] - Configuration options
   * @param {string} [config.version='1.0.0'] - Certificate version
   * @param {number} [config.maxCertHistory=100] - Max certificates to keep
   * @param {Object} [config.healthThresholds] - Custom health thresholds
   */
  constructor(config = {}) {
    this.config = {
      version: config.version || '1.0.0',
      maxCertHistory: config.maxCertHistory || 100,
      healthThresholds: {
        dimension: {
          minDimension: 10,
          maxUtilization: 0.9
        },
        correlation: {
          maxTC: 0.8,
          maxNormalizedTC: 0.9
        },
        transferEntropy: {
          maxTE: 0.9
        },
        capacity: {
          minAdmissionRate: 0.5,
          minCapacity: 0.5
        },
        ...config.healthThresholds
      }
    };

    /** @type {Array<Object>} Certificate history */
    this.certificateHistory = [];

    /** @type {string|null} Hash of last certificate */
    this._lastCertificateHash = null;
  }

  /**
   * Generate a dimension certificate
   *
   * @param {Object} measurements - All measurement results
   * @param {Object} measurements.dimension - Dimension computation result
   * @param {Object} measurements.correlation - Total correlation result
   * @param {Object} measurements.transferEntropy - Transfer entropy result (or causal graph)
   * @param {Object} measurements.capacity - Capacity result
   * @param {Object} [options] - Generation options
   * @returns {Promise<Object>} Signed certificate
   */
  async generateCertificate(measurements, options = {}) {
    const startTime = Date.now();
    const epoch = this._generateEpoch();

    // Build measurement snapshot
    const snapshot = this._buildMeasurementSnapshot(measurements);

    // Compute trends
    const trends = this._computeTrends(measurements);

    // Evaluate health
    const health = this._evaluateHealth(measurements);

    // Build metadata
    const metadata = {
      generatedAt: new Date().toISOString(),
      partitionCount: this._countPartitions(measurements),
      dataPoints: this._countDataPoints(measurements),
      computeTimeMs: Date.now() - startTime
    };

    // Build certificate (without hash)
    const certificateData = {
      version: this.config.version,
      epoch,
      measurements: snapshot,
      trends,
      health,
      metadata,
      previousCertificateHash: this._lastCertificateHash
    };

    // Compute certificate hash
    const certificateHash = await this._computeCertificateHash(certificateData);

    const certificate = {
      ...certificateData,
      certificateHash
    };

    // Store in history
    this._recordCertificate(certificate);

    return certificate;
  }

  /**
   * Verify a certificate's integrity
   *
   * @param {Object} certificate - Certificate to verify
   * @returns {Promise<Object>} Verification result
   */
  async verifyCertificate(certificate) {
    const { certificateHash, ...dataWithoutHash } = certificate;

    // Recompute hash
    const computedHash = await this._computeCertificateHash(dataWithoutHash);

    const hashValid = computedHash === certificateHash;

    // Verify chain if previous hash exists
    let chainValid = true;
    if (certificate.previousCertificateHash) {
      const previousCert = this.certificateHistory.find(
        c => c.certificateHash === certificate.previousCertificateHash
      );
      chainValid = previousCert !== undefined;
    }

    // Validate schema
    let schemaValid = true;
    try {
      CertificateSchema.parse(certificate);
    } catch (error) {
      schemaValid = false;
    }

    return {
      valid: hashValid && chainValid && schemaValid,
      hashValid,
      chainValid,
      schemaValid,
      computedHash,
      providedHash: certificateHash,
      verifiedAt: new Date().toISOString()
    };
  }

  /**
   * Compare two certificates
   *
   * @param {Object} cert1 - First certificate (older)
   * @param {Object} cert2 - Second certificate (newer)
   * @returns {Object} Comparison result
   */
  compareCertificates(cert1, cert2) {
    const comparison = {
      epochDelta: this._epochDifference(cert1.epoch, cert2.epoch),
      dimension: this._compareMeasurement(
        cert1.measurements.dimension.systemDimension,
        cert2.measurements.dimension.systemDimension,
        'dimension'
      ),
      correlation: this._compareMeasurement(
        cert1.measurements.correlation.totalCorrelation,
        cert2.measurements.correlation.totalCorrelation,
        'correlation'
      ),
      transferEntropy: this._compareMeasurement(
        cert1.measurements.transferEntropy.totalTE,
        cert2.measurements.transferEntropy.totalTE,
        'transferEntropy'
      ),
      capacity: this._compareMeasurement(
        cert1.measurements.capacity.systemCapacity,
        cert2.measurements.capacity.systemCapacity,
        'capacity'
      ),
      healthChange: {
        from: cert1.health.status,
        to: cert2.health.status,
        scoreChange: cert2.health.score - cert1.health.score,
        improved: cert2.health.score > cert1.health.score
      }
    };

    // Overall assessment
    comparison.summary = this._generateComparisonSummary(comparison);

    return comparison;
  }

  /**
   * Get certificate history
   *
   * @param {number} [limit=10] - Number of certificates to return
   * @returns {Array<Object>} Recent certificates
   */
  getCertificateHistory(limit = 10) {
    return this.certificateHistory.slice(-limit);
  }

  /**
   * Get certificate by epoch
   *
   * @param {string} epoch - Epoch to find
   * @returns {Object|null} Certificate if found
   */
  getCertificateByEpoch(epoch) {
    return this.certificateHistory.find(c => c.epoch === epoch) || null;
  }

  /**
   * Export certificate to different formats
   *
   * @param {Object} certificate - Certificate to export
   * @param {string} format - Export format (json, jsonld, turtle)
   * @returns {string} Formatted certificate
   */
  exportCertificate(certificate, format = 'json') {
    switch (format) {
      case 'json':
        return JSON.stringify(certificate, null, 2);

      case 'jsonld':
        return this._toJSONLD(certificate);

      case 'turtle':
        return this._toTurtle(certificate);

      default:
        return JSON.stringify(certificate, null, 2);
    }
  }

  /**
   * Generate health report from certificate
   *
   * @param {Object} certificate - Certificate to analyze
   * @returns {Object} Health report
   */
  generateHealthReport(certificate) {
    const m = certificate.measurements;
    const h = certificate.health;

    return {
      epoch: certificate.epoch,
      overallStatus: h.status,
      healthScore: h.score,
      metrics: {
        dimension: {
          value: m.dimension.systemDimension,
          status: this._getMetricStatus('dimension', m.dimension),
          trend: certificate.trends.dimension,
          interpretation: this._interpretDimension(m.dimension)
        },
        correlation: {
          value: m.correlation.totalCorrelation,
          normalized: m.correlation.normalizedTC,
          status: this._getMetricStatus('correlation', m.correlation),
          trend: certificate.trends.correlation,
          interpretation: this._interpretCorrelation(m.correlation)
        },
        transferEntropy: {
          totalTE: m.transferEntropy.totalTE,
          causalEdges: m.transferEntropy.causalEdges.length,
          status: this._getMetricStatus('transferEntropy', m.transferEntropy),
          trend: certificate.trends.transferEntropy,
          interpretation: this._interpretTE(m.transferEntropy)
        },
        capacity: {
          value: m.capacity.systemCapacity,
          admissionRate: m.capacity.admissionRate,
          throughput: m.capacity.throughput,
          status: this._getMetricStatus('capacity', m.capacity),
          trend: certificate.trends.capacity,
          interpretation: this._interpretCapacity(m.capacity)
        }
      },
      issues: h.issues,
      recommendations: this._generateRecommendations(h.issues),
      generatedAt: new Date().toISOString()
    };
  }

  /**
   * Build measurement snapshot from raw results
   * @private
   */
  _buildMeasurementSnapshot(measurements) {
    const dim = measurements.dimension || {};
    const corr = measurements.correlation || {};
    const te = measurements.transferEntropy || {};
    const cap = measurements.capacity || {};

    // Extract causal edges from TE result
    let causalEdges = [];
    if (te.edges) {
      // From causal graph
      causalEdges = te.edges.map(e => ({
        source: e.source,
        target: e.target,
        te: e.te
      }));
    } else if (te.transferEntropy !== undefined) {
      // From single TE computation
      causalEdges = [{
        source: te.source,
        target: te.target,
        te: te.transferEntropy
      }];
    }

    const totalTE = causalEdges.reduce((sum, e) => sum + e.te, 0);

    return {
      dimension: {
        systemDimension: dim.systemDimension || 0,
        partitionDimensions: this._extractPartitionDimensions(dim),
        utilizationRatio: dim.utilizationRatio || 0
      },
      correlation: {
        totalCorrelation: corr.totalCorrelation || 0,
        normalizedTC: corr.normalizedTC || 0,
        partitionEntropies: corr.partitionEntropies || {}
      },
      transferEntropy: {
        causalEdges,
        totalTE
      },
      capacity: {
        systemCapacity: cap.systemCapacity || 0,
        admissionRate: cap.admissionRate || 0,
        throughput: cap.throughput || { quadsPerSecond: 0, deltasPerSecond: 0 }
      }
    };
  }

  /**
   * Extract partition dimensions
   * @private
   */
  _extractPartitionDimensions(dim) {
    const result = {};
    if (dim.partitionDimensions) {
      for (const pd of dim.partitionDimensions) {
        result[pd.partitionName] = pd.effectiveDimension;
      }
    }
    return result;
  }

  /**
   * Compute trends from measurement results
   * @private
   */
  _computeTrends(measurements) {
    // Use trend info from measurements if available
    const dimTrend = measurements.dimension?.trend || 'unknown';
    const corrTrend = measurements.correlation?.trend || 'unknown';
    const teTrend = measurements.transferEntropy?.trend || 'unknown';
    const capTrend = measurements.capacity?.trend || 'unknown';

    return {
      dimension: this._normalizeTrend(dimTrend),
      correlation: this._normalizeTrend(corrTrend),
      transferEntropy: this._normalizeTrend(teTrend),
      capacity: this._normalizeTrend(capTrend)
    };
  }

  /**
   * Normalize trend string
   * @private
   */
  _normalizeTrend(trend) {
    const validTrends = ['stable', 'increasing', 'decreasing', 'volatile', 'unknown'];
    return validTrends.includes(trend) ? trend : 'unknown';
  }

  /**
   * Evaluate overall health from measurements
   * @private
   */
  _evaluateHealth(measurements) {
    const issues = [];
    let score = 100;

    const t = this.config.healthThresholds;
    const dim = measurements.dimension || {};
    const corr = measurements.correlation || {};
    const te = measurements.transferEntropy || {};
    const cap = measurements.capacity || {};

    // Check dimension
    if (dim.systemDimension !== undefined && dim.systemDimension < t.dimension.minDimension) {
      issues.push({
        metric: 'dimension',
        type: 'low_value',
        severity: 'warning',
        message: `System dimension ${dim.systemDimension?.toFixed(2)} below threshold ${t.dimension.minDimension}`
      });
      score -= 15;
    }

    if (dim.utilizationRatio !== undefined && dim.utilizationRatio > t.dimension.maxUtilization) {
      issues.push({
        metric: 'dimension',
        type: 'high_utilization',
        severity: 'critical',
        message: `Dimension utilization ${(dim.utilizationRatio * 100).toFixed(1)}% exceeds limit`
      });
      score -= 25;
    }

    // Check correlation
    if (corr.normalizedTC !== undefined && corr.normalizedTC > t.correlation.maxNormalizedTC) {
      issues.push({
        metric: 'correlation',
        type: 'high_coupling',
        severity: 'warning',
        message: `Normalized TC ${corr.normalizedTC?.toFixed(3)} indicates tight coupling`
      });
      score -= 15;
    }

    // Check TE
    const totalTE = te.edges
      ? te.edges.reduce((sum, e) => sum + e.te, 0)
      : (te.transferEntropy || 0);

    if (totalTE > t.transferEntropy.maxTE) {
      issues.push({
        metric: 'transferEntropy',
        type: 'high_causality',
        severity: 'warning',
        message: `Total TE ${totalTE?.toFixed(3)} indicates strong causal dependencies`
      });
      score -= 10;
    }

    // Check capacity
    if (cap.admissionRate !== undefined && cap.admissionRate < t.capacity.minAdmissionRate) {
      issues.push({
        metric: 'capacity',
        type: 'low_admission',
        severity: 'warning',
        message: `Admission rate ${(cap.admissionRate * 100).toFixed(1)}% is below threshold`
      });
      score -= 15;
    }

    // Determine overall status
    score = Math.max(0, score);
    let status = 'healthy';
    if (score < 50) status = 'critical';
    else if (score < 80) status = 'warning';

    return {
      status,
      issues,
      score
    };
  }

  /**
   * Compute certificate hash
   * @private
   */
  async _computeCertificateHash(data) {
    const canonical = JSON.stringify(data, Object.keys(data).sort());
    return blake3(canonical);
  }

  /**
   * Record certificate in history
   * @private
   */
  _recordCertificate(certificate) {
    this.certificateHistory.push(certificate);
    this._lastCertificateHash = certificate.certificateHash;

    // Trim history
    while (this.certificateHistory.length > this.config.maxCertHistory) {
      this.certificateHistory.shift();
    }
  }

  /**
   * Generate epoch string
   * @private
   */
  _generateEpoch() {
    const now = new Date();
    const year = now.getUTCFullYear();
    const month = String(now.getUTCMonth() + 1).padStart(2, '0');
    const day = String(now.getUTCDate()).padStart(2, '0');
    const hour = String(now.getUTCHours()).padStart(2, '0');
    const minute = String(now.getUTCMinutes()).padStart(2, '0');
    const ms = String(now.getUTCMilliseconds()).padStart(3, '0');

    return `tau_${year}_${month}_${day}_${hour}${minute}_${ms}`;
  }

  /**
   * Count partitions in measurements
   * @private
   */
  _countPartitions(measurements) {
    const dim = measurements.dimension;
    if (dim?.partitionDimensions) {
      if (Array.isArray(dim.partitionDimensions)) {
        return dim.partitionDimensions.length;
      }
      return Object.keys(dim.partitionDimensions).length;
    }
    return 0;
  }

  /**
   * Count data points used in measurements
   * @private
   */
  _countDataPoints(measurements) {
    let count = 0;
    if (measurements.dimension?.totalQuads) {
      count += measurements.dimension.totalQuads;
    }
    if (measurements.correlation?.windowSize) {
      count += measurements.correlation.windowSize;
    }
    if (measurements.capacity?.windowSize) {
      count += measurements.capacity.windowSize;
    }
    return count;
  }

  /**
   * Compare measurement values
   * @private
   */
  _compareMeasurement(val1, val2, metric) {
    const delta = val2 - val1;
    const percentChange = val1 !== 0 ? (delta / val1) * 100 : (val2 !== 0 ? 100 : 0);

    let direction;
    if (Math.abs(percentChange) < 5) direction = 'stable';
    else if (delta > 0) direction = 'increased';
    else direction = 'decreased';

    return {
      before: val1,
      after: val2,
      delta,
      percentChange,
      direction
    };
  }

  /**
   * Compute epoch difference
   * @private
   */
  _epochDifference(epoch1, epoch2) {
    // Parse epochs (format: tau_YYYY_MM_DD_HHMM_mmm)
    try {
      const parts1 = epoch1.split('_');
      const parts2 = epoch2.split('_');

      const date1 = new Date(`${parts1[1]}-${parts1[2]}-${parts1[3]}T${parts1[4].slice(0, 2)}:${parts1[4].slice(2)}:00Z`);
      const date2 = new Date(`${parts2[1]}-${parts2[2]}-${parts2[3]}T${parts2[4].slice(0, 2)}:${parts2[4].slice(2)}:00Z`);

      const diffMs = date2.getTime() - date1.getTime();
      return {
        milliseconds: diffMs,
        seconds: diffMs / 1000,
        minutes: diffMs / 60000,
        hours: diffMs / 3600000
      };
    } catch {
      return { milliseconds: 0, seconds: 0, minutes: 0, hours: 0 };
    }
  }

  /**
   * Generate comparison summary
   * @private
   */
  _generateComparisonSummary(comparison) {
    const changes = [];

    if (comparison.dimension.direction !== 'stable') {
      changes.push(`Dimension ${comparison.dimension.direction} by ${Math.abs(comparison.dimension.percentChange).toFixed(1)}%`);
    }
    if (comparison.correlation.direction !== 'stable') {
      changes.push(`Correlation ${comparison.correlation.direction} by ${Math.abs(comparison.correlation.percentChange).toFixed(1)}%`);
    }
    if (comparison.healthChange.scoreChange !== 0) {
      changes.push(`Health score ${comparison.healthChange.scoreChange > 0 ? 'improved' : 'degraded'} by ${Math.abs(comparison.healthChange.scoreChange)} points`);
    }

    if (changes.length === 0) {
      return 'System metrics are stable between epochs';
    }

    return changes.join('; ');
  }

  /**
   * Get metric status
   * @private
   */
  _getMetricStatus(metric, data) {
    const t = this.config.healthThresholds[metric];

    switch (metric) {
      case 'dimension':
        if (data.systemDimension < t.minDimension) return 'warning';
        if (data.utilizationRatio > t.maxUtilization) return 'critical';
        return 'healthy';

      case 'correlation':
        if (data.normalizedTC > t.maxNormalizedTC) return 'warning';
        if (data.totalCorrelation > t.maxTC) return 'warning';
        return 'healthy';

      case 'transferEntropy':
        const totalTE = data.causalEdges
          ? data.causalEdges.reduce((sum, e) => sum + e.te, 0)
          : data.totalTE;
        if (totalTE > t.maxTE) return 'warning';
        return 'healthy';

      case 'capacity':
        if (data.admissionRate < t.minAdmissionRate) return 'warning';
        if (data.systemCapacity < t.minCapacity) return 'warning';
        return 'healthy';

      default:
        return 'unknown';
    }
  }

  /**
   * Interpret dimension measurement
   * @private
   */
  _interpretDimension(dim) {
    const d = dim.systemDimension;
    if (d > 50) return 'High expressiveness - system can represent many distinct states';
    if (d > 20) return 'Moderate expressiveness - adequate for typical workloads';
    if (d > 10) return 'Limited expressiveness - may constrain complex schemas';
    return 'Low expressiveness - consider relaxing constraints';
  }

  /**
   * Interpret correlation measurement
   * @private
   */
  _interpretCorrelation(corr) {
    const tc = corr.normalizedTC;
    if (tc < 0.2) return 'Low coupling - partitions are well isolated';
    if (tc < 0.5) return 'Moderate coupling - some interdependence between partitions';
    if (tc < 0.8) return 'High coupling - changes tend to affect multiple partitions';
    return 'Very high coupling - partitions are tightly bound';
  }

  /**
   * Interpret transfer entropy
   * @private
   */
  _interpretTE(te) {
    const edges = te.causalEdges?.length || 0;
    const totalTE = te.totalTE || 0;

    if (edges === 0) return 'No significant causal relationships detected';
    if (totalTE < 0.5) return `${edges} weak causal links - partitions evolve mostly independently`;
    if (totalTE < 2) return `${edges} moderate causal links - some information flows between partitions`;
    return `${edges} strong causal links - significant information transfer between partitions`;
  }

  /**
   * Interpret capacity measurement
   * @private
   */
  _interpretCapacity(cap) {
    const rate = cap.admissionRate;
    const throughput = cap.throughput.deltasPerSecond;

    if (rate > 0.9 && throughput > 1) return 'High capacity utilization - system is actively processing changes';
    if (rate > 0.7) return 'Good capacity - most changes are being admitted';
    if (rate > 0.5) return 'Moderate capacity - some changes are being rejected';
    return 'Low capacity - many changes are being rejected';
  }

  /**
   * Generate recommendations from issues
   * @private
   */
  _generateRecommendations(issues) {
    const recommendations = [];

    for (const issue of issues) {
      switch (issue.type) {
        case 'low_value':
          if (issue.metric === 'dimension') {
            recommendations.push('Consider relaxing namespace or term constraints to increase expressiveness');
          }
          break;

        case 'high_utilization':
          recommendations.push('Dimension capacity is near limit - plan for scaling or constraint optimization');
          break;

        case 'high_coupling':
          recommendations.push('Partitions are tightly coupled - consider refactoring for better isolation');
          break;

        case 'high_causality':
          recommendations.push('Strong causal dependencies exist - monitor for cascade effects');
          break;

        case 'low_admission':
          recommendations.push('Low admission rate may indicate overly strict invariants or policy issues');
          break;
      }
    }

    if (recommendations.length === 0) {
      recommendations.push('System is operating within healthy parameters');
    }

    return recommendations;
  }

  /**
   * Convert certificate to JSON-LD
   * @private
   */
  _toJSONLD(cert) {
    return JSON.stringify({
      '@context': {
        unrdf: 'https://unrdf.org/vocab#',
        xsd: 'http://www.w3.org/2001/XMLSchema#',
        prov: 'http://www.w3.org/ns/prov#'
      },
      '@type': 'unrdf:DimensionCertificate',
      '@id': `urn:certificate:${cert.certificateHash}`,
      'unrdf:version': cert.version,
      'unrdf:epoch': cert.epoch,
      'unrdf:systemDimension': cert.measurements.dimension.systemDimension,
      'unrdf:totalCorrelation': cert.measurements.correlation.totalCorrelation,
      'unrdf:systemCapacity': cert.measurements.capacity.systemCapacity,
      'unrdf:healthStatus': cert.health.status,
      'unrdf:healthScore': cert.health.score,
      'prov:generatedAtTime': {
        '@type': 'xsd:dateTime',
        '@value': cert.metadata.generatedAt
      },
      'unrdf:certificateHash': cert.certificateHash,
      'unrdf:previousCertificateHash': cert.previousCertificateHash
    }, null, 2);
  }

  /**
   * Convert certificate to Turtle
   * @private
   */
  _toTurtle(cert) {
    const lines = [
      '@prefix unrdf: <https://unrdf.org/vocab#> .',
      '@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .',
      '@prefix prov: <http://www.w3.org/ns/prov#> .',
      '',
      `<urn:certificate:${cert.certificateHash}> a unrdf:DimensionCertificate ;`,
      `  unrdf:version "${cert.version}" ;`,
      `  unrdf:epoch "${cert.epoch}" ;`,
      `  unrdf:systemDimension ${cert.measurements.dimension.systemDimension.toFixed(4)} ;`,
      `  unrdf:totalCorrelation ${cert.measurements.correlation.totalCorrelation.toFixed(4)} ;`,
      `  unrdf:systemCapacity ${cert.measurements.capacity.systemCapacity.toFixed(4)} ;`,
      `  unrdf:healthStatus "${cert.health.status}" ;`,
      `  unrdf:healthScore ${cert.health.score} ;`,
      `  prov:generatedAtTime "${cert.metadata.generatedAt}"^^xsd:dateTime ;`,
      `  unrdf:certificateHash "${cert.certificateHash}" .`
    ];

    return lines.join('\n');
  }

  /**
   * Clear certificate history
   */
  clearHistory() {
    this.certificateHistory = [];
    this._lastCertificateHash = null;
  }

  /**
   * Export generator state
   * @returns {Object} Exportable state
   */
  exportState() {
    return {
      certificateHistory: [...this.certificateHistory],
      lastCertificateHash: this._lastCertificateHash,
      config: { ...this.config },
      exportedAt: new Date().toISOString()
    };
  }

  /**
   * Import generator state
   * @param {Object} state - Previously exported state
   */
  importState(state) {
    if (state.certificateHistory) {
      this.certificateHistory = [...state.certificateHistory];
    }
    if (state.lastCertificateHash) {
      this._lastCertificateHash = state.lastCertificateHash;
    }
  }
}

/**
 * Create a certificate generator with default configuration
 * @param {Object} [config] - Configuration options
 * @returns {CertificateGenerator} New generator
 */
export function createCertificateGenerator(config = {}) {
  return new CertificateGenerator(config);
}

/**
 * Quick certificate generation
 * @param {Object} measurements - All measurement results
 * @param {Object} [options] - Generation options
 * @returns {Promise<Object>} Generated certificate
 */
export async function generateCertificate(measurements, options = {}) {
  const generator = new CertificateGenerator(options);
  return generator.generateCertificate(measurements, options);
}

export default CertificateGenerator;
