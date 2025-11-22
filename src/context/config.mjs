/**
 * @file Runtime configuration accessors
 * @module context/config
 */

/**
 * Get runtime configuration from environment and sensible defaults.
 * @returns {Object} Runtime configuration
 */
export function getRuntimeConfig() {
  const env = process.env || {};
  const toInt = (val, def) => {
    const n = Number.parseInt(val, 10);
    return Number.isFinite(n) && n >= 0 ? n : def;
  };

  return {
    cacheMaxSize: toInt(env.UNRDF_CACHE_MAX_SIZE, undefined),
    serviceName: env.UNRDF_SERVICE_NAME,
    serviceVersion: env.UNRDF_SERVICE_VERSION,
  };
}
