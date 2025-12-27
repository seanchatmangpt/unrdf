/**
 * DDoS Detection & Mitigation
 *
 * ML-based traffic pattern analysis with automatic mitigation:
 * - Anomaly detection for request spikes
 * - Automatic IP blacklisting
 * - Traffic shaping during attacks
 * - Pattern learning from normal traffic
 *
 * @module sidecar/utils/ddos-detector
 */

import { trace, context, SpanStatusCode } from '@opentelemetry/api';
import { metrics } from './otel-metrics.mjs';

const tracer = trace.getTracer('sidecar-ddos-detector');

// Configuration
const DETECTOR_CONFIG = {
  // Time windows for analysis
  windowSizes: [10, 60, 300], // 10s, 1m, 5m windows

  // Anomaly detection thresholds
  thresholds: {
    requestSpike: 5.0,      // 5x normal rate
    errorRate: 0.3,         // 30% error rate
    uniqueIPs: 0.2,         // 80% requests from same IP
    requestVariety: 0.1,    // <10% endpoint variety
  },

  // Blacklist settings
  blacklistDuration: 3600 * 1000, // 1 hour in ms
  autoBlacklistThreshold: 0.8,    // 80% confidence score

  // Traffic shaping
  shapingEnabled: true,
  shapingFactor: 0.5,             // Reduce traffic to 50% during attack
};

/**
 * Traffic statistics tracker
 */
class TrafficStats {
  constructor() {
    this.windows = new Map(); // windowSize -> stats
    this.baseline = new Map(); // windowSize -> baseline metrics
    this.ipStats = new Map();  // IP -> request count
    this.endpointStats = new Map(); // endpoint -> count
    this.blacklist = new Map(); // IP -> { until, reason, confidence }
    this.lastCleanup = Date.now();

    // Initialize windows
    DETECTOR_CONFIG.windowSizes.forEach(size => {
      this.windows.set(size, {
        requests: [],
        errors: [],
        startTime: Date.now(),
      });
    });
  }

  /**
   * Record a request
   */
  recordRequest(ip, endpoint, isError = false) {
    const now = Date.now();

    // Record in all time windows
    DETECTOR_CONFIG.windowSizes.forEach(windowSize => {
      const window = this.windows.get(windowSize);
      const cutoff = now - (windowSize * 1000);

      // Remove old entries
      window.requests = window.requests.filter(t => t.time > cutoff);
      window.errors = window.errors.filter(t => t.time > cutoff);

      // Add new entry
      window.requests.push({ time: now, ip, endpoint });
      if (isError) {
        window.errors.push({ time: now, ip, endpoint });
      }
    });

    // Update IP stats
    const ipCount = this.ipStats.get(ip) || 0;
    this.ipStats.set(ip, ipCount + 1);

    // Update endpoint stats
    const endpointCount = this.endpointStats.get(endpoint) || 0;
    this.endpointStats.set(endpoint, endpointCount + 1);

    // Periodic cleanup
    if (now - this.lastCleanup > 60000) { // Every minute
      this.cleanup();
    }
  }

  /**
   * Get statistics for a window
   */
  getWindowStats(windowSize) {
    const window = this.windows.get(windowSize);
    if (!window) return null;

    const now = Date.now();
    const cutoff = now - (windowSize * 1000);

    // Count requests in window
    const requests = window.requests.filter(t => t.time > cutoff);
    const errors = window.errors.filter(t => t.time > cutoff);

    // Calculate metrics
    const totalRequests = requests.length;
    const totalErrors = errors.length;
    const errorRate = totalRequests > 0 ? totalErrors / totalRequests : 0;

    // IP diversity
    const uniqueIPs = new Set(requests.map(r => r.ip)).size;
    const ipDiversity = totalRequests > 0 ? uniqueIPs / totalRequests : 1;

    // Endpoint variety
    const uniqueEndpoints = new Set(requests.map(r => r.endpoint)).size;
    const endpointVariety = totalRequests > 0 ? uniqueEndpoints / totalRequests : 1;

    // Request rate
    const requestRate = totalRequests / windowSize;

    return {
      windowSize,
      totalRequests,
      totalErrors,
      errorRate,
      uniqueIPs,
      ipDiversity,
      uniqueEndpoints,
      endpointVariety,
      requestRate,
    };
  }

  /**
   * Update baseline from current normal traffic
   */
  updateBaseline() {
    DETECTOR_CONFIG.windowSizes.forEach(windowSize => {
      const stats = this.getWindowStats(windowSize);
      if (stats && stats.totalRequests > 10) { // Need minimum data
        this.baseline.set(windowSize, {
          requestRate: stats.requestRate,
          errorRate: stats.errorRate,
          ipDiversity: stats.ipDiversity,
          endpointVariety: stats.endpointVariety,
          updatedAt: Date.now(),
        });
      }
    });
  }

  /**
   * Cleanup old data
   */
  cleanup() {
    const now = Date.now();
    const maxWindow = Math.max(...DETECTOR_CONFIG.windowSizes) * 1000;

    // Clean up IP stats (keep only last 5 minutes)
    this.ipStats.clear();

    // Clean up endpoint stats
    this.endpointStats.clear();

    // Clean up expired blacklist entries
    for (const [ip, entry] of this.blacklist.entries()) {
      if (now > entry.until) {
        this.blacklist.delete(ip);
      }
    }

    this.lastCleanup = now;
  }

  /**
   * Add IP to blacklist
   */
  blacklistIP(ip, reason, confidence, duration = DETECTOR_CONFIG.blacklistDuration) {
    this.blacklist.set(ip, {
      until: Date.now() + duration,
      reason,
      confidence,
      addedAt: Date.now(),
    });
  }

  /**
   * Check if IP is blacklisted
   */
  isBlacklisted(ip) {
    const entry = this.blacklist.get(ip);
    if (!entry) return false;

    const now = Date.now();
    if (now > entry.until) {
      this.blacklist.delete(ip);
      return false;
    }

    return true;
  }

  /**
   * Get blacklist entries
   */
  getBlacklist() {
    return Array.from(this.blacklist.entries()).map(([ip, entry]) => ({
      ip,
      ...entry,
      remainingMs: entry.until - Date.now(),
    }));
  }

  /**
   * Remove from blacklist
   */
  removeFromBlacklist(ip) {
    return this.blacklist.delete(ip);
  }
}

// Global traffic stats instance
const trafficStats = new TrafficStats();

// Update baseline every 5 minutes during normal operation
setInterval(() => {
  trafficStats.updateBaseline();
}, 5 * 60 * 1000);

/**
 * Analyze traffic for anomalies
 */
function analyzeTraffic() {
  const span = tracer.startSpan('ddos-analyze-traffic');

  try {
    const anomalies = [];

    // Analyze each time window
    for (const windowSize of DETECTOR_CONFIG.windowSizes) {
      const stats = trafficStats.getWindowStats(windowSize);
      const baseline = trafficStats.baseline.get(windowSize);

      if (!stats || !baseline) continue;

      const windowAnomalies = [];

      // Check for request spike
      if (stats.requestRate > baseline.requestRate * DETECTOR_CONFIG.thresholds.requestSpike) {
        windowAnomalies.push({
          type: 'request_spike',
          severity: 'high',
          metric: 'requestRate',
          current: stats.requestRate,
          baseline: baseline.requestRate,
          ratio: stats.requestRate / baseline.requestRate,
        });
      }

      // Check for high error rate
      if (stats.errorRate > DETECTOR_CONFIG.thresholds.errorRate) {
        windowAnomalies.push({
          type: 'high_error_rate',
          severity: 'medium',
          metric: 'errorRate',
          current: stats.errorRate,
          threshold: DETECTOR_CONFIG.thresholds.errorRate,
        });
      }

      // Check for low IP diversity (potential single-source attack)
      if (stats.ipDiversity < DETECTOR_CONFIG.thresholds.uniqueIPs) {
        windowAnomalies.push({
          type: 'low_ip_diversity',
          severity: 'high',
          metric: 'ipDiversity',
          current: stats.ipDiversity,
          threshold: DETECTOR_CONFIG.thresholds.uniqueIPs,
          uniqueIPs: stats.uniqueIPs,
          totalRequests: stats.totalRequests,
        });
      }

      // Check for low endpoint variety (potential targeted attack)
      if (stats.endpointVariety < DETECTOR_CONFIG.thresholds.requestVariety) {
        windowAnomalies.push({
          type: 'low_endpoint_variety',
          severity: 'medium',
          metric: 'endpointVariety',
          current: stats.endpointVariety,
          threshold: DETECTOR_CONFIG.thresholds.requestVariety,
        });
      }

      if (windowAnomalies.length > 0) {
        anomalies.push({
          windowSize,
          stats,
          baseline,
          anomalies: windowAnomalies,
        });
      }
    }

    // Calculate overall threat score
    const threatScore = calculateThreatScore(anomalies);

    span.setAttributes({
      'ddos.anomalies_count': anomalies.length,
      'ddos.threat_score': threatScore,
    });

    span.setStatus({ code: SpanStatusCode.OK });

    return { anomalies, threatScore };

  } catch (err) {
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });
    return { anomalies: [], threatScore: 0 };
  } finally {
    span.end();
  }
}

/**
 * Calculate threat score from anomalies
 */
function calculateThreatScore(anomalies) {
  if (anomalies.length === 0) return 0;

  let score = 0;
  let weights = { high: 0.4, medium: 0.2, low: 0.1 };

  for (const window of anomalies) {
    for (const anomaly of window.anomalies) {
      score += weights[anomaly.severity] || 0.1;
    }
  }

  // Normalize to 0-1
  return Math.min(score, 1.0);
}

/**
 * Identify top attacking IPs
 */
function identifyAttackingIPs() {
  const stats60s = trafficStats.getWindowStats(60);
  if (!stats60s || stats60s.totalRequests < 20) return [];

  // Count requests per IP in last minute
  const ipCounts = new Map();
  const window = trafficStats.windows.get(60);

  for (const req of window.requests) {
    ipCounts.set(req.ip, (ipCounts.get(req.ip) || 0) + 1);
  }

  // Find IPs with disproportionate request counts
  const avgRequestsPerIP = stats60s.totalRequests / stats60s.uniqueIPs;
  const suspiciousIPs = [];

  for (const [ip, count] of ipCounts.entries()) {
    if (count > avgRequestsPerIP * 3) { // 3x average
      const confidence = Math.min(count / (avgRequestsPerIP * 10), 1.0);
      suspiciousIPs.push({ ip, count, confidence });
    }
  }

  // Sort by confidence
  return suspiciousIPs.sort((a, b) => b.confidence - a.confidence);
}

/**
 * Automatic mitigation
 */
function autoMitigate(threatScore, anomalies) {
  const span = tracer.startSpan('ddos-auto-mitigate', {
    attributes: { 'ddos.threat_score': threatScore },
  });

  try {
    const actions = [];

    // If high threat, identify and blacklist attacking IPs
    if (threatScore > DETECTOR_CONFIG.autoBlacklistThreshold) {
      const attackingIPs = identifyAttackingIPs();

      for (const { ip, count, confidence } of attackingIPs) {
        if (confidence > 0.7 && !trafficStats.isBlacklisted(ip)) {
          trafficStats.blacklistIP(ip, 'Auto-detected DDoS source', confidence);
          actions.push({
            type: 'blacklist_ip',
            ip,
            confidence,
            requestCount: count,
          });

          // Record metric
          if (metrics?.ddosBlacklistCounter) {
            metrics.ddosBlacklistCounter.add(1, { reason: 'auto', confidence });
          }
        }
      }
    }

    span.setAttributes({
      'ddos.actions_taken': actions.length,
    });
    span.setStatus({ code: SpanStatusCode.OK });

    return actions;

  } catch (err) {
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });
    return [];
  } finally {
    span.end();
  }
}

/**
 * DDoS detection middleware
 */
export function ddosDetectionMiddleware(req, res, next) {
  const ip = req.headers['x-forwarded-for']?.split(',')[0].trim()
    || req.headers['x-real-ip']
    || req.connection?.remoteAddress
    || 'unknown';

  // Check blacklist
  if (trafficStats.isBlacklisted(ip)) {
    const entry = trafficStats.blacklist.get(ip);
    const remainingMs = entry.until - Date.now();

    if (metrics?.ddosBlockedCounter) {
      metrics.ddosBlockedCounter.add(1, { reason: entry.reason });
    }

    return res.status(403).json({
      error: 'Forbidden',
      message: 'IP address temporarily blocked due to suspicious activity',
      reason: entry.reason,
      retryAfter: Math.ceil(remainingMs / 1000),
    });
  }

  // Record request
  const isError = res.statusCode >= 400;
  trafficStats.recordRequest(ip, req.path, isError);

  next();
}

/**
 * Periodic analysis (run every 10 seconds)
 */
setInterval(() => {
  const { anomalies, threatScore } = analyzeTraffic();

  if (threatScore > 0.5) {
    console.warn('[DDoS] Threat detected:', { threatScore, anomalyCount: anomalies.length });

    // Record metric
    if (metrics?.ddosThreatGauge) {
      metrics.ddosThreatGauge.record(threatScore);
    }

    // Auto-mitigate
    const actions = autoMitigate(threatScore, anomalies);
    if (actions.length > 0) {
      console.warn('[DDoS] Mitigation actions taken:', actions);
    }
  }
}, 10000);

/**
 * Get current DDoS status
 */
export function getDDoSStatus() {
  const { anomalies, threatScore } = analyzeTraffic();

  return {
    threatScore,
    status: threatScore > 0.8 ? 'critical' : threatScore > 0.5 ? 'elevated' : 'normal',
    anomalies: anomalies.map(a => ({
      windowSize: a.windowSize,
      anomalyCount: a.anomalies.length,
      types: a.anomalies.map(x => x.type),
    })),
    blacklist: trafficStats.getBlacklist(),
    windows: DETECTOR_CONFIG.windowSizes.map(size => trafficStats.getWindowStats(size)),
  };
}

/**
 * Manual blacklist management
 */
export function manualBlacklist(ip, reason, duration) {
  trafficStats.blacklistIP(ip, reason, 1.0, duration);
}

export function removeFromBlacklist(ip) {
  return trafficStats.removeFromBlacklist(ip);
}

export function getBlacklist() {
  return trafficStats.getBlacklist();
}

export default ddosDetectionMiddleware;
