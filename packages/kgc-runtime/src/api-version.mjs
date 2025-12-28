/**
 * @file API Version - Semantic versioning and deprecation tracking
 * @module @unrdf/kgc-runtime/api-version
 * @description Manages API versioning with deprecation policy and compatibility checking
 */

import { z } from 'zod';

/**
 * Current KGC Runtime API version
 */
export const CURRENT_API_VERSION = '5.0.1';

/**
 * API version status
 */
export const API_STATUS = {
  STABLE: 'stable',
  BETA: 'beta',
  DEPRECATED: 'deprecated',
  REMOVED: 'removed',
};

/**
 * Version metadata schema
 */
const VersionMetadataSchema = z.object({
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  status: z.enum(['stable', 'beta', 'deprecated', 'removed']),
  releaseDate: z.string().optional(),
  deprecationDate: z.string().optional(),
  removalDate: z.string().optional(),
  deprecationReason: z.string().optional(),
  migrationGuide: z.string().optional(),
  breakingChanges: z.array(z.string()).optional(),
});

/**
 * API versions registry
 * Each version tracks its status, deprecation, and compatibility
 */
const API_VERSIONS = [
  {
    version: '5.0.1',
    status: API_STATUS.BETA,
    releaseDate: '2024-12-27',
    breakingChanges: [
      'Plugin system introduced',
      'Enhanced isolation and capability management',
    ],
  },
  {
    version: '5.0.0',
    status: API_STATUS.BETA,
    releaseDate: '2024-12-26',
    breakingChanges: [
      'Work item system refactored',
      'Receipt format updated to include parent hash',
    ],
  },
  {
    version: '4.0.0',
    status: API_STATUS.DEPRECATED,
    releaseDate: '2024-11-01',
    deprecationDate: '2024-12-01',
    removalDate: '2025-03-01',
    deprecationReason: 'Replaced by v5 with enhanced governance features',
    migrationGuide: 'See docs/migration/v4-to-v5.md',
  },
  {
    version: '3.0.0',
    status: API_STATUS.REMOVED,
    releaseDate: '2024-06-01',
    deprecationDate: '2024-09-01',
    removalDate: '2024-12-01',
    deprecationReason: 'Legacy API completely removed',
    migrationGuide: 'See docs/migration/v3-to-v5.md',
  },
];

/**
 * Deprecation policy:
 * - APIs are marked deprecated 2 releases before removal
 * - Deprecated APIs remain functional but emit warnings
 * - Removal happens after minimum 3 months deprecation period
 */
export const DEPRECATION_POLICY = {
  RELEASES_BEFORE_REMOVAL: 2,
  MIN_DEPRECATION_PERIOD_DAYS: 90,
  WARNING_ENABLED: true,
};

/**
 * API Version Manager - Tracks versions, deprecations, and compatibility
 *
 * @example
 * import { APIVersionManager } from '@unrdf/kgc-runtime/api-version';
 * const versionManager = new APIVersionManager();
 * const compatible = versionManager.isCompatible('5.0.0', '5.0.1');
 * console.log(compatible); // true
 */
export class APIVersionManager {
  constructor() {
    this.versions = new Map();
    this.deprecationWarnings = new Set();

    // Load versions
    for (const versionData of API_VERSIONS) {
      this.versions.set(versionData.version, versionData);
    }
  }

  /**
   * Get current API version
   *
   * @returns {string} Current version
   */
  getCurrentVersion() {
    return CURRENT_API_VERSION;
  }

  /**
   * Get version metadata
   *
   * @param {string} version - Version to query
   * @returns {Object|null} Version metadata or null if not found
   *
   * @example
   * const metadata = versionManager.getVersionMetadata('5.0.1');
   * console.log(metadata.status); // 'beta'
   */
  getVersionMetadata(version) {
    return this.versions.get(version) || null;
  }

  /**
   * Check if version is deprecated
   *
   * @param {string} version - Version to check
   * @returns {boolean} True if deprecated
   *
   * @example
   * const deprecated = versionManager.isDeprecated('4.0.0');
   * console.log(deprecated); // true
   */
  isDeprecated(version) {
    const metadata = this.getVersionMetadata(version);
    return metadata?.status === API_STATUS.DEPRECATED;
  }

  /**
   * Check if version is removed
   *
   * @param {string} version - Version to check
   * @returns {boolean} True if removed
   */
  isRemoved(version) {
    const metadata = this.getVersionMetadata(version);
    return metadata?.status === API_STATUS.REMOVED;
  }

  /**
   * Check if version is stable
   *
   * @param {string} version - Version to check
   * @returns {boolean} True if stable
   */
  isStable(version) {
    const metadata = this.getVersionMetadata(version);
    return metadata?.status === API_STATUS.STABLE;
  }

  /**
   * Check version compatibility (semver)
   *
   * @param {string} requiredVersion - Required version
   * @param {string} actualVersion - Actual version
   * @returns {boolean} True if compatible
   *
   * @example
   * const compatible = versionManager.isCompatible('5.0.0', '5.0.1');
   * console.log(compatible); // true (patch compatible)
   */
  isCompatible(requiredVersion, actualVersion) {
    const [reqMajor, reqMinor] = requiredVersion.split('.').map(Number);
    const [actMajor, actMinor] = actualVersion.split('.').map(Number);

    // Major version must match
    if (reqMajor !== actMajor) {
      return false;
    }

    // Minor version must be >= required
    if (actMinor < reqMinor) {
      return false;
    }

    return true;
  }

  /**
   * Emit deprecation warning
   *
   * @param {string} feature - Deprecated feature
   * @param {string} version - Version it was deprecated
   * @param {string} alternative - Recommended alternative
   *
   * @example
   * versionManager.warnDeprecation('oldFunction', '4.0.0', 'Use newFunction instead');
   */
  warnDeprecation(feature, version, alternative) {
    if (!DEPRECATION_POLICY.WARNING_ENABLED) {
      return;
    }

    const warningKey = `${feature}@${version}`;

    // Only warn once per feature
    if (this.deprecationWarnings.has(warningKey)) {
      return;
    }

    console.warn(
      `[DEPRECATION WARNING] ${feature} is deprecated since v${version}. ${alternative}`
    );

    this.deprecationWarnings.add(warningKey);
  }

  /**
   * Get all deprecated versions
   *
   * @returns {Array<Object>} Array of deprecated version metadata
   */
  getDeprecatedVersions() {
    return Array.from(this.versions.values()).filter(
      v => v.status === API_STATUS.DEPRECATED
    );
  }

  /**
   * Get breaking changes for a version
   *
   * @param {string} version - Version to query
   * @returns {Array<string>} Array of breaking changes
   */
  getBreakingChanges(version) {
    const metadata = this.getVersionMetadata(version);
    return metadata?.breakingChanges || [];
  }

  /**
   * Calculate days until removal
   *
   * @param {string} version - Deprecated version
   * @returns {number|null} Days until removal or null if not deprecated
   */
  getDaysUntilRemoval(version) {
    const metadata = this.getVersionMetadata(version);

    if (!metadata || !metadata.removalDate) {
      return null;
    }

    const now = new Date();
    const removalDate = new Date(metadata.removalDate);
    const diffTime = removalDate - now;
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));

    return diffDays;
  }

  /**
   * List all versions
   *
   * @returns {Array<Object>} All version metadata
   */
  listVersions() {
    return Array.from(this.versions.values());
  }

  /**
   * Add new version
   *
   * @param {Object} versionData - Version metadata
   */
  addVersion(versionData) {
    const validated = VersionMetadataSchema.parse(versionData);
    this.versions.set(validated.version, validated);
  }

  /**
   * Mark version as deprecated
   *
   * @param {string} version - Version to deprecate
   * @param {Object} options - Deprecation options
   * @param {string} options.reason - Deprecation reason
   * @param {string} options.migrationGuide - Migration guide URL
   * @param {string} options.removalDate - Planned removal date (ISO)
   */
  deprecateVersion(version, options = {}) {
    const metadata = this.getVersionMetadata(version);

    if (!metadata) {
      throw new Error(`Version not found: ${version}`);
    }

    metadata.status = API_STATUS.DEPRECATED;
    metadata.deprecationDate = new Date().toISOString().split('T')[0];
    metadata.deprecationReason = options.reason;
    metadata.migrationGuide = options.migrationGuide;
    metadata.removalDate = options.removalDate;

    this.versions.set(version, metadata);
  }
}

/**
 * Singleton instance
 */
const versionManager = new APIVersionManager();

/**
 * Get the singleton version manager
 *
 * @returns {APIVersionManager} Version manager instance
 */
export function getVersionManager() {
  return versionManager;
}

/**
 * Check if plugin API version is compatible with runtime
 *
 * @param {string} pluginVersion - Plugin's required API version
 * @returns {boolean} True if compatible
 *
 * @example
 * const compatible = isPluginCompatible('5.0.0');
 * console.log(compatible); // true
 */
export function isPluginCompatible(pluginVersion) {
  return versionManager.isCompatible(pluginVersion, CURRENT_API_VERSION);
}

/**
 * Validate plugin API version
 *
 * @param {string} pluginVersion - Plugin's API version
 * @throws {Error} If version is incompatible or removed
 */
export function validatePluginVersion(pluginVersion) {
  if (versionManager.isRemoved(pluginVersion)) {
    throw new Error(
      `Plugin API version ${pluginVersion} has been removed. Please upgrade plugin.`
    );
  }

  if (!isPluginCompatible(pluginVersion)) {
    throw new Error(
      `Plugin API version ${pluginVersion} is incompatible with runtime ${CURRENT_API_VERSION}`
    );
  }

  if (versionManager.isDeprecated(pluginVersion)) {
    const daysUntilRemoval = versionManager.getDaysUntilRemoval(pluginVersion);
    versionManager.warnDeprecation(
      `Plugin API version ${pluginVersion}`,
      pluginVersion,
      `Upgrade to ${CURRENT_API_VERSION}. Removal in ${daysUntilRemoval} days.`
    );
  }
}
