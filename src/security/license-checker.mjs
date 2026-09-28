/**
 * @fileoverview License Checker - Verify license compatibility across dependencies
 *
 * Implements Q_license_compliance invariant:
 * - Categorizes licenses by permissiveness
 * - Detects incompatible license combinations
 * - Generates compliance reports
 * - Supports custom license policies
 *
 * @module security/license-checker
 */

import { z } from 'zod';
import { readFileSync, existsSync } from 'node:fs';
import { readdir } from 'node:fs/promises';
import { join, dirname, relative, basename } from 'node:path';

/**
 * License finding schema
 */
export const LicenseFindingSchema = z.object({
  package: z.string(),
  version: z.string(),
  license: z.string(),
  category: z.enum(['permissive', 'copyleft', 'proprietary', 'unknown', 'public_domain']),
  compatible: z.boolean(),
  reason: z.string().optional(),
  path: z.string()
});

/**
 * @typedef {z.infer<typeof LicenseFindingSchema>} LicenseFinding
 */

/**
 * License categories and their compatibility
 */
export const LICENSE_CATEGORIES = {
  permissive: {
    licenses: [
      'MIT', 'MIT-0', 'ISC', 'BSD-2-Clause', 'BSD-3-Clause', 'Apache-2.0',
      '0BSD', 'CC0-1.0', 'Unlicense', 'WTFPL', 'Zlib', 'X11', 'BlueOak-1.0.0',
      'CC-BY-3.0', 'CC-BY-4.0', 'OFL-1.1', 'Python-2.0', 'BSL-1.0', 'Artistic-2.0'
    ],
    compatible: true,
    description: 'Permissive licenses allow commercial use with attribution'
  },
  copyleft_weak: {
    licenses: ['LGPL-2.0', 'LGPL-2.1', 'LGPL-3.0', 'MPL-2.0', 'EPL-1.0', 'EPL-2.0', 'CDDL-1.0'],
    compatible: true,
    description: 'Weak copyleft - modifications must be shared, but not entire project'
  },
  copyleft_strong: {
    licenses: ['GPL-2.0', 'GPL-3.0', 'AGPL-3.0', 'GPL-2.0-only', 'GPL-3.0-only', 'AGPL-3.0-only'],
    compatible: false,
    description: 'Strong copyleft - entire project must be released under same license'
  },
  proprietary: {
    licenses: ['Commercial', 'Proprietary', 'UNLICENSED'],
    compatible: false,
    description: 'Proprietary licenses - require explicit permission'
  },
  public_domain: {
    licenses: ['CC0-1.0', 'Unlicense', 'WTFPL', 'Public Domain'],
    compatible: true,
    description: 'Public domain - no restrictions'
  }
};

/**
 * Default allowed licenses (permissive + weak copyleft)
 */
export const DEFAULT_ALLOWED_LICENSES = [
  ...LICENSE_CATEGORIES.permissive.licenses,
  ...LICENSE_CATEGORIES.copyleft_weak.licenses,
  ...LICENSE_CATEGORIES.public_domain.licenses
];

/**
 * Licenses that should always be blocked
 */
export const BLOCKED_LICENSES = [
  'GPL-2.0', 'GPL-3.0', 'AGPL-3.0',
  'GPL-2.0-only', 'GPL-3.0-only', 'AGPL-3.0-only',
  'SSPL-1.0', 'EUPL-1.1', 'EUPL-1.2'
];

/**
 * License aliases - normalize different representations
 */
const LICENSE_ALIASES = {
  'Apache 2.0': 'Apache-2.0',
  'Apache License 2.0': 'Apache-2.0',
  'Apache-2': 'Apache-2.0',
  'BSD': 'BSD-3-Clause',
  'BSD-2': 'BSD-2-Clause',
  'BSD-3': 'BSD-3-Clause',
  'GPL v2': 'GPL-2.0',
  'GPL v3': 'GPL-3.0',
  'LGPL v2': 'LGPL-2.0',
  'LGPL v2.1': 'LGPL-2.1',
  'LGPL v3': 'LGPL-3.0',
  'ISC License': 'ISC',
  'MIT License': 'MIT',
  'Public domain': 'CC0-1.0',
  'Unlicensed': 'Unlicense'
};

/**
 * Normalize license identifier
 *
 * @param {string} license - License to normalize
 * @returns {string} Normalized license
 */
export function normalizeLicense(license) {
  if (!license) return 'UNKNOWN';

  // Clean up the license string
  let cleaned = license.trim();

  // Check aliases
  if (LICENSE_ALIASES[cleaned]) {
    return LICENSE_ALIASES[cleaned];
  }

  // Handle SPDX expressions
  if (cleaned.includes(' OR ')) {
    // Return least restrictive
    const options = cleaned.split(' OR ').map(l => normalizeLicense(l.trim()));
    const permissive = options.find(l => isPermissive(l));
    return permissive || options[0];
  }

  if (cleaned.includes(' AND ')) {
    // Return most restrictive
    const options = cleaned.split(' AND ').map(l => normalizeLicense(l.trim()));
    const copyleft = options.find(l => isCopyleft(l));
    return copyleft || options[0];
  }

  // Handle parentheses
  cleaned = cleaned.replace(/^\(|\)$/g, '');

  // Handle -or-later, -only suffixes
  cleaned = cleaned.replace(/-or-later$/, '');

  return cleaned;
}

/**
 * Check if license is permissive
 *
 * @param {string} license - License to check
 * @returns {boolean} True if permissive
 */
export function isPermissive(license) {
  const normalized = normalizeLicense(license);
  return LICENSE_CATEGORIES.permissive.licenses.includes(normalized) ||
         LICENSE_CATEGORIES.public_domain.licenses.includes(normalized);
}

/**
 * Check if license is copyleft
 *
 * @param {string} license - License to check
 * @returns {boolean} True if copyleft
 */
export function isCopyleft(license) {
  const normalized = normalizeLicense(license);
  return LICENSE_CATEGORIES.copyleft_strong.licenses.includes(normalized) ||
         LICENSE_CATEGORIES.copyleft_weak.licenses.includes(normalized);
}

/**
 * Get license category
 *
 * @param {string} license - License to categorize
 * @returns {string} Category name
 */
export function getLicenseCategory(license) {
  const normalized = normalizeLicense(license);

  for (const [category, info] of Object.entries(LICENSE_CATEGORIES)) {
    if (info.licenses.includes(normalized)) {
      return category;
    }
  }

  return 'unknown';
}

/**
 * Check if license is allowed by policy
 *
 * @param {string} license - License to check
 * @param {Object} [policy] - License policy
 * @returns {{allowed: boolean, reason: string}} Result
 */
export function checkLicensePolicy(license, policy = {}) {
  const normalized = normalizeLicense(license);
  const allowedLicenses = policy.allowed || DEFAULT_ALLOWED_LICENSES;
  const blockedLicenses = policy.blocked || BLOCKED_LICENSES;

  // Check blocked list first
  if (blockedLicenses.includes(normalized)) {
    return {
      allowed: false,
      reason: `License "${normalized}" is explicitly blocked by policy`
    };
  }

  // Check allowed list
  if (allowedLicenses.includes(normalized)) {
    return {
      allowed: true,
      reason: `License "${normalized}" is allowed by policy`
    };
  }

  // Check categories
  const category = getLicenseCategory(normalized);
  if (category === 'unknown') {
    return {
      allowed: false,
      reason: `Unknown license "${license}" - manual review required`
    };
  }

  const categoryInfo = LICENSE_CATEGORIES[category];
  return {
    allowed: categoryInfo?.compatible ?? false,
    reason: categoryInfo?.description || `License "${normalized}" requires review`
  };
}

/**
 * Extract license from package.json
 *
 * @param {Object} pkg - Package.json content
 * @returns {string} License identifier
 */
function extractLicense(pkg) {
  // Direct license field
  if (typeof pkg.license === 'string') {
    return pkg.license;
  }

  // License object with type
  if (pkg.license && typeof pkg.license === 'object') {
    return pkg.license.type || pkg.license.name || 'UNKNOWN';
  }

  // Licenses array (deprecated but still used)
  if (Array.isArray(pkg.licenses) && pkg.licenses.length > 0) {
    const types = pkg.licenses.map(l =>
      typeof l === 'string' ? l : (l.type || l.name)
    ).filter(Boolean);
    return types.join(' OR ') || 'UNKNOWN';
  }

  return 'UNKNOWN';
}

/**
 * Scan node_modules for package licenses
 *
 * @param {string} nodeModulesPath - Path to node_modules
 * @param {Object} [options] - Scan options
 * @returns {Promise<Array<LicenseFinding>>} License findings
 */
async function scanNodeModules(nodeModulesPath, options = {}) {
  const findings = [];

  if (!existsSync(nodeModulesPath)) {
    return findings;
  }

  async function scanDir(dir, depth = 0) {
    if (depth > 5) return; // Limit recursion depth

    let entries;
    try {
      entries = await readdir(dir, { withFileTypes: true });
    } catch {
      return;
    }

    for (const entry of entries) {
      if (!entry.isDirectory()) continue;

      const fullPath = join(dir, entry.name);

      // Handle scoped packages
      if (entry.name.startsWith('@')) {
        await scanDir(fullPath, depth);
        continue;
      }

      // Skip .bin and other hidden directories
      if (entry.name.startsWith('.')) continue;

      // Look for package.json
      const pkgJsonPath = join(fullPath, 'package.json');
      if (existsSync(pkgJsonPath)) {
        try {
          const content = readFileSync(pkgJsonPath, 'utf-8');
          const pkg = JSON.parse(content);

          if (pkg.name && pkg.version) {
            const license = extractLicense(pkg);
            const normalized = normalizeLicense(license);
            const category = getLicenseCategory(normalized);
            const policyResult = checkLicensePolicy(license, options.policy);

            findings.push({
              package: pkg.name,
              version: pkg.version,
              license: normalized,
              category,
              compatible: policyResult.allowed,
              reason: policyResult.reason,
              path: relative(options.basePath || dir, fullPath)
            });
          }
        } catch {
          // Skip packages with invalid package.json
        }
      }

      // Scan nested node_modules
      const nestedNodeModules = join(fullPath, 'node_modules');
      if (existsSync(nestedNodeModules)) {
        await scanDir(nestedNodeModules, depth + 1);
      }
    }
  }

  await scanDir(nodeModulesPath);
  return findings;
}

/**
 * Check licenses for a single package location
 *
 * @param {string} packageJsonPath - Path to package.json
 * @param {Object} [options] - Check options
 * @returns {Promise<Object>} Check results
 */
export async function checkPackage(packageJsonPath, options = {}) {
  const directory = dirname(packageJsonPath);
  const basePath = options.basePath || directory;

  // Read main package.json
  let mainPkg;
  try {
    const content = readFileSync(packageJsonPath, 'utf-8');
    mainPkg = JSON.parse(content);
  } catch {
    return {
      location: relative(basePath, packageJsonPath),
      error: 'Failed to read package.json',
      findings: []
    };
  }

  // Get main project license
  const projectLicense = extractLicense(mainPkg);

  // Scan node_modules
  const nodeModulesPath = join(directory, 'node_modules');
  const findings = await scanNodeModules(nodeModulesPath, {
    ...options,
    basePath: directory
  });

  // Filter production dependencies if requested
  if (options.productionOnly) {
    const prodDeps = new Set(Object.keys(mainPkg.dependencies || {}));
    // Note: This doesn't handle transitive deps accurately
  }

  return {
    location: relative(basePath, packageJsonPath),
    projectLicense: normalizeLicense(projectLicense),
    totalPackages: findings.length,
    findings
  };
}

/**
 * Check licenses for entire directory
 *
 * @param {string} directory - Root directory
 * @param {Object} [options] - Check options
 * @returns {Promise<Object>} Check results
 */
export async function checkDirectory(directory, options = {}) {
  const startTime = Date.now();
  const allFindings = [];
  const packageResults = [];

  // Find all package.json files
  async function findPackageJsons(dir) {
    const results = [];

    async function search(currentDir) {
      let entries;
      try {
        entries = await readdir(currentDir, { withFileTypes: true });
      } catch {
        return;
      }

      for (const entry of entries) {
        const fullPath = join(currentDir, entry.name);

        if (entry.name === 'node_modules' || entry.name.startsWith('.')) {
          continue;
        }

        if (entry.isDirectory()) {
          await search(fullPath);
        } else if (entry.name === 'package.json') {
          results.push(fullPath);
        }
      }
    }

    await search(dir);
    return results;
  }

  const packageJsonFiles = await findPackageJsons(directory);

  for (const pkgPath of packageJsonFiles) {
    const result = await checkPackage(pkgPath, { ...options, basePath: directory });
    packageResults.push(result);
    if (result.findings) {
      allFindings.push(...result.findings);
    }
  }

  // Deduplicate findings by package@version
  const uniqueFindings = new Map();
  for (const finding of allFindings) {
    const key = `${finding.package}@${finding.version}`;
    if (!uniqueFindings.has(key)) {
      uniqueFindings.set(key, finding);
    }
  }
  const findings = Array.from(uniqueFindings.values());

  // Group by license
  const byLicense = {};
  for (const finding of findings) {
    byLicense[finding.license] = (byLicense[finding.license] || 0) + 1;
  }

  // Group by category
  const byCategory = {
    permissive: 0,
    copyleft_weak: 0,
    copyleft_strong: 0,
    proprietary: 0,
    public_domain: 0,
    unknown: 0
  };
  for (const finding of findings) {
    const category = finding.category;
    byCategory[category] = (byCategory[category] || 0) + 1;
  }

  // Find incompatible
  const incompatible = findings.filter(f => !f.compatible);

  // Calculate compliance score
  const complianceScore = findings.length > 0
    ? Math.round(((findings.length - incompatible.length) / findings.length) * 100)
    : 100;

  return {
    passed: incompatible.length === 0,
    findings,
    incompatible,
    packageResults,
    summary: {
      totalPackages: findings.length,
      compatible: findings.length - incompatible.length,
      incompatible: incompatible.length,
      byLicense,
      byCategory,
      complianceScore,
      scanDuration: Date.now() - startTime
    },
    metadata: {
      directory,
      timestamp: new Date().toISOString(),
      policy: options.policy || { type: 'default' }
    }
  };
}

/**
 * Format results as report
 *
 * @param {Object} result - Check result
 * @returns {string} Formatted report
 */
export function formatReport(result) {
  const lines = [];

  lines.push('='.repeat(60));
  lines.push('LICENSE COMPLIANCE REPORT');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Scan Time: ${result.metadata.timestamp}`);
  lines.push(`Directory: ${result.metadata.directory}`);
  lines.push(`Scan Duration: ${result.summary.scanDuration}ms`);
  lines.push('');
  lines.push('-'.repeat(60));
  lines.push('SUMMARY');
  lines.push('-'.repeat(60));
  lines.push(`Total Packages: ${result.summary.totalPackages}`);
  lines.push(`Compatible: ${result.summary.compatible}`);
  lines.push(`Incompatible: ${result.summary.incompatible}`);
  lines.push(`Compliance Score: ${result.summary.complianceScore}%`);
  lines.push(`Status: ${result.passed ? 'PASSED' : 'FAILED'}`);
  lines.push('');
  lines.push('By Category:');
  for (const [category, count] of Object.entries(result.summary.byCategory)) {
    if (count > 0) {
      lines.push(`  ${category}: ${count}`);
    }
  }

  if (Object.keys(result.summary.byLicense).length > 0) {
    lines.push('');
    lines.push('By License:');
    const sorted = Object.entries(result.summary.byLicense)
      .sort(([, a], [, b]) => b - a)
      .slice(0, 15);
    for (const [license, count] of sorted) {
      lines.push(`  ${license}: ${count}`);
    }
    if (Object.keys(result.summary.byLicense).length > 15) {
      lines.push(`  ... and ${Object.keys(result.summary.byLicense).length - 15} more`);
    }
  }

  if (result.incompatible.length > 0) {
    lines.push('');
    lines.push('-'.repeat(60));
    lines.push('INCOMPATIBLE LICENSES');
    lines.push('-'.repeat(60));

    for (const finding of result.incompatible) {
      lines.push('');
      lines.push(`  ${finding.package}@${finding.version}`);
      lines.push(`    License: ${finding.license}`);
      lines.push(`    Category: ${finding.category}`);
      lines.push(`    Reason: ${finding.reason}`);
    }
  }

  lines.push('');
  lines.push('='.repeat(60));

  return lines.join('\n');
}

/**
 * Generate license compatibility matrix
 *
 * @param {Array<LicenseFinding>} findings - License findings
 * @returns {Object} Compatibility matrix
 */
export function generateCompatibilityMatrix(findings) {
  const licenses = [...new Set(findings.map(f => f.license))];
  const matrix = {};

  for (const license1 of licenses) {
    matrix[license1] = {};
    for (const license2 of licenses) {
      matrix[license1][license2] = areLicensesCompatible(license1, license2);
    }
  }

  return matrix;
}

/**
 * Check if two licenses are compatible
 *
 * @param {string} license1 - First license
 * @param {string} license2 - Second license
 * @returns {boolean} True if compatible
 */
function areLicensesCompatible(license1, license2) {
  const cat1 = getLicenseCategory(license1);
  const cat2 = getLicenseCategory(license2);

  // Same license is always compatible
  if (license1 === license2) return true;

  // Unknown requires manual review
  if (cat1 === 'unknown' || cat2 === 'unknown') return false;

  // Permissive with anything permissive
  if (cat1 === 'permissive' && cat2 === 'permissive') return true;
  if (cat1 === 'public_domain' || cat2 === 'public_domain') return true;

  // Strong copyleft only with same family
  if (cat1 === 'copyleft_strong' || cat2 === 'copyleft_strong') {
    return license1 === license2;
  }

  // Weak copyleft generally compatible
  if (cat1 === 'copyleft_weak' || cat2 === 'copyleft_weak') {
    return true;
  }

  return false;
}

export default {
  checkPackage,
  checkDirectory,
  formatReport,
  normalizeLicense,
  getLicenseCategory,
  checkLicensePolicy,
  isPermissive,
  isCopyleft,
  generateCompatibilityMatrix,
  LICENSE_CATEGORIES,
  DEFAULT_ALLOWED_LICENSES,
  BLOCKED_LICENSES
};
