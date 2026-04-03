/**
 * @unrdf/docs - UNRDF Nuxt documentation site
 *
 * This package provides the Nuxt-based documentation site
 * with database integration, AI features, and analytics.
 *
 * @module docs
 */

/**
 * Package metadata
 */
export const metadata = {
  name: 'docs',
  version: '5.0.1',
  description: 'UNRDF Nuxt documentation site',
};

/**
 * Get package information
 * @returns {Object} Package metadata
 */
export function getPackageInfo() {
  return { ...metadata };
}

/**
 * Validate package is properly installed
 * @returns {boolean} True if package is valid
 */
export function isInstalled() {
  return true;
}

/**
 * Get server utilities (re-exported from server/)
 * Note: Server utilities are imported dynamically in Nuxt context
 */
export function getServerUtils() {
  return {
    api: 'server/api',
    database: 'server/database',
    routes: 'server/routes',
    utils: 'server/utils',
  };
}
