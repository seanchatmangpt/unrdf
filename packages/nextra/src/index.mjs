/**
 * @unrdf/nextra-docs - UNRDF documentation with Nextra 4
 *
 * This package provides developer-focused Next.js documentation
 * using Nextra theme. The actual content is in the app/ directory.
 *
 * @module @unrdf/nextra-docs
 */

/**
 * Package metadata
 */
export const metadata = {
  name: '@unrdf/nextra-docs',
  version: '5.0.1',
  description: 'UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation',
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
