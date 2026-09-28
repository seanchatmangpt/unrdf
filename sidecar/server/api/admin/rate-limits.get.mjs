/**
 * Admin Rate Limit Status Endpoint
 *
 * Provides visibility into rate limiting status:
 * - Current rate limit consumption per user/IP
 * - Blacklist management
 * - System load metrics
 * - Configuration details
 *
 * @module sidecar/api/admin/rate-limits
 */

import { defineEventHandler, getQuery, setResponseStatus } from '#imports';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import {
  getRateLimitStatus,
  resetRateLimit,
  getRateLimitConfig,
} from '../../middleware/03.rate-limit.mjs';
import {
  getDDoSStatus,
  getBlacklist,
  removeFromBlacklist,
  manualBlacklist,
} from '../../utils/ddos-detector.mjs';

const tracer = trace.getTracer('sidecar-admin-rate-limits');

/**
 * GET /api/admin/rate-limits
 *
 * Get comprehensive rate limiting status
 */
export default defineEventHandler(async (event) => {
  /** @param {number} code @param {Object} body */
  const reply = (code, body) => {
    setResponseStatus(event, code);
    return body;
  };
  const span = tracer.startSpan('admin-get-rate-limits');

  try {
    // Get query parameters
    const { action, key, type, ip, reason, duration } = getQuery(event);

    // Handle different actions
    if (action === 'check' && key) {
      // Check specific rate limit key
      const status = await getRateLimitStatus(key, type || 'unauthenticated');
      return reply(200, { status });
    }

    if (action === 'reset' && key) {
      // Reset rate limit for specific key
      const success = await resetRateLimit(key, type || 'unauthenticated');
      return reply(200, {
        success,
        message: success ? 'Rate limit reset successfully' : 'Failed to reset rate limit',
        key,
        type: type || 'unauthenticated',
      });
    }

    if (action === 'blacklist' && ip) {
      // Add IP to blacklist
      const durationMs = duration ? parseInt(duration, 10) * 1000 : 3600000;
      manualBlacklist(ip, reason || 'Manual blacklist', durationMs);
      return reply(200, {
        success: true,
        message: 'IP blacklisted successfully',
        ip,
        reason: reason || 'Manual blacklist',
        durationMs,
      });
    }

    if (action === 'unblacklist' && ip) {
      // Remove IP from blacklist
      const success = removeFromBlacklist(ip);
      return reply(200, {
        success,
        message: success ? 'IP removed from blacklist' : 'IP not found in blacklist',
        ip,
      });
    }

    // Default: return comprehensive status
    const config = getRateLimitConfig();
    const ddosStatus = getDDoSStatus();
    const blacklist = getBlacklist();

    span.setAttributes({
      'rate_limit.config_count': Object.keys(config).length,
      'ddos.threat_score': ddosStatus.threatScore,
      'ddos.blacklist_count': blacklist.length,
    });

    span.setStatus({ code: SpanStatusCode.OK });

    return reply(200, {
      config,
      ddos: {
        status: ddosStatus.status,
        threatScore: ddosStatus.threatScore,
        anomalies: ddosStatus.anomalies,
        windows: ddosStatus.windows,
      },
      blacklist,
      actions: {
        check: '/api/admin/rate-limits?action=check&key=<key>&type=<type>',
        reset: '/api/admin/rate-limits?action=reset&key=<key>&type=<type>',
        blacklist: '/api/admin/rate-limits?action=blacklist&ip=<ip>&reason=<reason>&duration=<seconds>',
        unblacklist: '/api/admin/rate-limits?action=unblacklist&ip=<ip>',
      },
    });

  } catch (err) {
    console.error('[Admin] Error getting rate limit status:', err);
    span.recordException(err);
    span.setStatus({ code: SpanStatusCode.ERROR, message: err.message });

    return reply(500, {
      error: 'Internal Server Error',
      message: 'Failed to retrieve rate limit status',
      details: err.message,
    });
  } finally {
    span.end();
  }
});
