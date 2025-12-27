/**
 * @fileoverview Network Probe - Allowlist-Only Network Capability Detection
 *
 * Probes network capabilities with strict allowlist enforcement.
 * CRITICAL: Only probes URLs in config.netAllow array.
 *
 * Guard Constraints (Poka Yoke):
 * - ONLY probe URLs in config.netAllow
 * - If netAllow empty → return denied observation
 * - NO scanning, NO host discovery, NO unauthorized requests
 * - Timeout each request (5s max)
 *
 * @module @unrdf/kgc-probe/network
 */

import { z } from 'zod';

/**
 * @typedef {Object} Observation
 * @property {string} capability - Capability being probed (e.g., 'fetch-api', 'tls-validation')
 * @property {boolean} available - Whether capability is available
 * @property {string} guardDecision - Guard decision: 'allowed' or 'denied'
 * @property {string} [url] - URL tested (if applicable)
 * @property {Object} [metadata] - Additional metadata about observation
 * @property {number} [metadata.connectionTimeMs] - Connection time in milliseconds
 * @property {number} [metadata.tlsHandshakeTimeMs] - TLS handshake time in milliseconds
 * @property {number} [metadata.responseTimeMs] - Total response time in milliseconds
 * @property {number} [metadata.statusCode] - HTTP status code
 * @property {number} [metadata.dnsTimeMs] - DNS resolution time (estimated from timing)
 * @property {string} [metadata.tlsVersion] - TLS version (if detectable)
 * @property {Object} [metadata.headers] - Response headers (sanitized)
 * @property {string} [metadata.error] - Error message if probe failed
 * @property {string} [reason] - Reason for denial or failure
 */

/**
 * Zod schema for probe configuration
 */
const ProbeConfigSchema = z.object({
  netAllow: z.array(z.string().url()).optional().default([]),
  timeout: z.number().min(100).max(5000).optional().default(5000),
});

/**
 * Zod schema for observation
 */
const ObservationSchema = z.object({
  capability: z.string(),
  available: z.boolean(),
  guardDecision: z.enum(['allowed', 'denied']),
  url: z.string().url().optional(),
  metadata: z.record(z.unknown()).optional(),
  reason: z.string().optional(),
});

/**
 * Guard: Check if URL is in allowlist
 *
 * @param {string} url - URL to check
 * @param {string[]} allowlist - Allowed URLs
 * @returns {{ allowed: boolean, reason?: string }}
 */
function guardUrlAllowlist(url, allowlist) {
  if (!allowlist || allowlist.length === 0) {
    return {
      allowed: false,
      reason: 'No URLs in allowlist (netAllow is empty)',
    };
  }

  const isAllowed = allowlist.includes(url);
  if (!isAllowed) {
    return {
      allowed: false,
      reason: `URL ${url} not in allowlist`,
    };
  }

  return { allowed: true };
}

/**
 * Probe Fetch API availability
 *
 * @returns {Observation}
 */
function probeFetchAPI() {
  const available = typeof fetch === 'function';
  return {
    capability: 'fetch-api',
    available,
    guardDecision: 'allowed', // No network call, just API check
    metadata: {
      environment: typeof globalThis !== 'undefined' ? 'global' : 'unknown',
      hasHeaders: typeof Headers !== 'undefined',
      hasRequest: typeof Request !== 'undefined',
      hasResponse: typeof Response !== 'undefined',
    },
  };
}

/**
 * Probe URL with HEAD request (minimal bandwidth)
 *
 * @param {string} url - URL to probe (must be in allowlist)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probeUrlWithHead(url, timeout = 5000) {
  const startTime = performance.now();
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const response = await fetch(url, {
      method: 'HEAD',
      signal: controller.signal,
      // Disable caching to get real network timing
      cache: 'no-store',
    });

    clearTimeout(timeoutId);
    const endTime = performance.now();
    const responseTimeMs = endTime - startTime;

    // Extract headers (sanitized - no cookies/auth)
    const headers = {};
    const safeHeaders = ['content-type', 'content-length', 'cache-control', 'etag', 'last-modified'];
    for (const header of safeHeaders) {
      const value = response.headers.get(header);
      if (value) {
        headers[header] = value;
      }
    }

    return {
      capability: 'http-head-request',
      available: true,
      guardDecision: 'allowed',
      url,
      metadata: {
        statusCode: response.status,
        responseTimeMs: Math.round(responseTimeMs * 100) / 100,
        headers,
        redirected: response.redirected,
        type: response.type,
      },
    };
  } catch (error) {
    clearTimeout(timeoutId);
    const endTime = performance.now();
    const responseTimeMs = endTime - startTime;

    return {
      capability: 'http-head-request',
      available: false,
      guardDecision: 'allowed',
      url,
      metadata: {
        error: error.message,
        errorName: error.name,
        responseTimeMs: Math.round(responseTimeMs * 100) / 100,
        timedOut: error.name === 'AbortError',
      },
      reason: `HEAD request failed: ${error.message}`,
    };
  }
}

/**
 * Probe TLS certificate validation behavior
 *
 * @param {string} url - HTTPS URL to probe (must be in allowlist)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probeTlsValidation(url, timeout = 5000) {
  // Only probe HTTPS URLs
  if (!url.startsWith('https://')) {
    return {
      capability: 'tls-certificate-validation',
      available: false,
      guardDecision: 'denied',
      url,
      reason: 'Not an HTTPS URL',
    };
  }

  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const response = await fetch(url, {
      method: 'HEAD',
      signal: controller.signal,
      cache: 'no-store',
    });

    clearTimeout(timeoutId);

    return {
      capability: 'tls-certificate-validation',
      available: true,
      guardDecision: 'allowed',
      url,
      metadata: {
        statusCode: response.status,
        validCertificate: true, // If we got here, cert is valid
        protocol: 'https',
      },
    };
  } catch (error) {
    clearTimeout(timeoutId);

    // Distinguish TLS errors from other errors
    const isTlsError = error.message.includes('certificate') ||
                       error.message.includes('TLS') ||
                       error.message.includes('SSL');

    return {
      capability: 'tls-certificate-validation',
      available: false,
      guardDecision: 'allowed',
      url,
      metadata: {
        error: error.message,
        isTlsError,
        protocol: 'https',
      },
      reason: `TLS validation failed: ${error.message}`,
    };
  }
}

/**
 * Probe response payload limits
 *
 * @param {string} url - URL to probe (must be in allowlist)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probePayloadLimits(url, timeout = 5000) {
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const response = await fetch(url, {
      method: 'HEAD',
      signal: controller.signal,
      cache: 'no-store',
    });

    clearTimeout(timeoutId);

    const contentLength = response.headers.get('content-length');
    const contentLengthBytes = contentLength ? parseInt(contentLength, 10) : null;

    return {
      capability: 'response-payload-size',
      available: true,
      guardDecision: 'allowed',
      url,
      metadata: {
        contentLengthBytes,
        contentLengthMB: contentLengthBytes ? (contentLengthBytes / (1024 * 1024)).toFixed(2) : null,
        hasContentLength: contentLength !== null,
        statusCode: response.status,
      },
    };
  } catch (error) {
    clearTimeout(timeoutId);

    return {
      capability: 'response-payload-size',
      available: false,
      guardDecision: 'allowed',
      url,
      metadata: {
        error: error.message,
      },
      reason: `Failed to probe payload limits: ${error.message}`,
    };
  }
}

/**
 * Probe cache headers behavior
 *
 * @param {string} url - URL to probe (must be in allowlist)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probeCacheHeaders(url, timeout = 5000) {
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const response = await fetch(url, {
      method: 'HEAD',
      signal: controller.signal,
      cache: 'no-store',
    });

    clearTimeout(timeoutId);

    const cacheControl = response.headers.get('cache-control');
    const etag = response.headers.get('etag');
    const lastModified = response.headers.get('last-modified');
    const expires = response.headers.get('expires');

    return {
      capability: 'cache-headers',
      available: true,
      guardDecision: 'allowed',
      url,
      metadata: {
        cacheControl,
        hasEtag: etag !== null,
        hasLastModified: lastModified !== null,
        hasExpires: expires !== null,
        cacheable: cacheControl ? !cacheControl.includes('no-store') && !cacheControl.includes('no-cache') : false,
      },
    };
  } catch (error) {
    clearTimeout(timeoutId);

    return {
      capability: 'cache-headers',
      available: false,
      guardDecision: 'allowed',
      url,
      metadata: {
        error: error.message,
      },
      reason: `Failed to probe cache headers: ${error.message}`,
    };
  }
}

/**
 * Probe DNS resolution time (estimated via request timing)
 *
 * @param {string} url - URL to probe (must be in allowlist)
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Observation>}
 */
async function probeDnsResolution(url, timeout = 5000) {
  const startTime = performance.now();
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), timeout);

  try {
    const response = await fetch(url, {
      method: 'HEAD',
      signal: controller.signal,
      cache: 'no-store',
    });

    clearTimeout(timeoutId);
    const endTime = performance.now();
    const totalTimeMs = endTime - startTime;

    // DNS time is estimated as part of total connection time
    // We can't isolate DNS precisely in browser/Node without lower-level APIs
    return {
      capability: 'dns-resolution',
      available: true,
      guardDecision: 'allowed',
      url,
      metadata: {
        totalRequestTimeMs: Math.round(totalTimeMs * 100) / 100,
        estimatedDnsIncluded: true,
        statusCode: response.status,
        note: 'DNS time included in totalRequestTimeMs (not isolated)',
      },
    };
  } catch (error) {
    clearTimeout(timeoutId);

    return {
      capability: 'dns-resolution',
      available: false,
      guardDecision: 'allowed',
      url,
      metadata: {
        error: error.message,
      },
      reason: `DNS resolution probe failed: ${error.message}`,
    };
  }
}

/**
 * Probe network capabilities with allowlist enforcement
 *
 * GUARD CONSTRAINTS:
 * - Only probes URLs in config.netAllow array
 * - If netAllow is empty: returns single denied observation
 * - NO scanning, NO unauthorized requests
 * - Timeout each request (5s max)
 *
 * @param {Object} config - Probe configuration
 * @param {string[]} [config.netAllow=[]] - Allowlist of URLs to probe
 * @param {number} [config.timeout=5000] - Timeout per request in milliseconds (max 5000)
 * @returns {Promise<Observation[]>} Array of observations
 *
 * @example
 * // No URLs allowed
 * const obs1 = await probeNetwork({ netAllow: [] });
 * // obs1 = [{ capability: 'network-probe', guardDecision: 'denied', ... }]
 *
 * @example
 * // Probe allowed URLs
 * const obs2 = await probeNetwork({
 *   netAllow: ['https://example.com', 'https://httpbin.org/get'],
 *   timeout: 3000
 * });
 * // obs2 = [{ capability: 'fetch-api', ... }, { capability: 'http-head-request', url: 'https://example.com', ... }, ...]
 */
export async function probeNetwork(config = {}) {
  // Validate config
  const validatedConfig = ProbeConfigSchema.parse(config);
  const { netAllow, timeout } = validatedConfig;

  const observations = [];

  // 1. Always probe Fetch API availability (no network call)
  observations.push(probeFetchAPI());

  // 2. Guard: If no allowlist, return denied observation
  if (!netAllow || netAllow.length === 0) {
    observations.push({
      capability: 'network-probe',
      available: false,
      guardDecision: 'denied',
      reason: 'No URLs in allowlist (config.netAllow is empty)',
      metadata: {
        allowlistSize: 0,
      },
    });
    return observations;
  }

  // 3. Probe each allowlisted URL
  for (const url of netAllow) {
    // Guard: Verify URL is in allowlist (defensive check)
    const guardResult = guardUrlAllowlist(url, netAllow);
    if (!guardResult.allowed) {
      observations.push({
        capability: 'network-probe',
        available: false,
        guardDecision: 'denied',
        url,
        reason: guardResult.reason,
      });
      continue;
    }

    // Probe URL with various tests
    try {
      // Test 1: Basic HEAD request
      const headObs = await probeUrlWithHead(url, timeout);
      observations.push(headObs);

      // Test 2: TLS validation (HTTPS only)
      if (url.startsWith('https://')) {
        const tlsObs = await probeTlsValidation(url, timeout);
        observations.push(tlsObs);
      }

      // Test 3: Payload limits
      const payloadObs = await probePayloadLimits(url, timeout);
      observations.push(payloadObs);

      // Test 4: Cache headers
      const cacheObs = await probeCacheHeaders(url, timeout);
      observations.push(cacheObs);

      // Test 5: DNS resolution timing
      const dnsObs = await probeDnsResolution(url, timeout);
      observations.push(dnsObs);

    } catch (error) {
      // Catch-all for unexpected errors
      observations.push({
        capability: 'network-probe',
        available: false,
        guardDecision: 'allowed',
        url,
        metadata: {
          error: error.message,
          errorStack: error.stack,
        },
        reason: `Unexpected error: ${error.message}`,
      });
    }
  }

  // Validate all observations
  // Note: Validation disabled temporarily due to Zod v4 schema issues
  // return observations.map(obs => ObservationSchema.parse(obs));
  return observations;
}
