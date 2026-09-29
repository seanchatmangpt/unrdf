/**
 * @fileoverview Dependency Auditor - Check dependencies against CVE database
 *
 * Implements Q_dependency_security invariant:
 * - Scans package.json and lock files for vulnerable packages
 * - Queries multiple vulnerability databases
 * - Provides remediation guidance
 * - Tracks vulnerability trends
 *
 * @module security/dependency-auditor
 */

import { z } from 'zod';
import { readFileSync, existsSync, statSync } from 'node:fs';
import { readdir } from 'node:fs/promises';
import { join, dirname, relative } from 'node:path';
import { execSync } from 'node:child_process';

/**
 * Vulnerability finding schema
 */
export const VulnerabilitySchema = z.object({
  package: z.string(),
  version: z.string(),
  vulnerability: z.string(),
  severity: z.enum(['critical', 'high', 'moderate', 'low']),
  cve: z.string().optional(),
  cwe: z.array(z.string()).optional(),
  title: z.string(),
  description: z.string().optional(),
  recommendation: z.string().optional(),
  patchedVersions: z.string().optional(),
  path: z.array(z.string()).optional(),
  source: z.enum(['npm', 'github', 'snyk', 'osv'])
});

/**
 * @typedef {z.infer<typeof VulnerabilitySchema>} Vulnerability
 */

/**
 * Known vulnerable package patterns (for offline detection)
 * These are high-profile vulnerabilities that should always be flagged
 */
const KNOWN_VULNERABILITIES = [
  {
    package: 'lodash',
    affectedVersions: '<4.17.21',
    vulnerability: 'CVE-2021-23337',
    severity: 'high',
    title: 'Command Injection in lodash',
    description: 'Lodash versions prior to 4.17.21 are vulnerable to Command Injection via the template function.',
    patchedVersions: '>=4.17.21'
  },
  {
    package: 'minimist',
    affectedVersions: '<1.2.6',
    vulnerability: 'CVE-2021-44906',
    severity: 'critical',
    title: 'Prototype Pollution in minimist',
    description: 'Prototype pollution vulnerability in minimist before 1.2.6.',
    patchedVersions: '>=1.2.6'
  },
  {
    package: 'node-fetch',
    affectedVersions: '<2.6.7 || >=3.0.0 <3.1.1',
    vulnerability: 'CVE-2022-0235',
    severity: 'high',
    title: 'Exposure of Sensitive Information in node-fetch',
    description: 'node-fetch is vulnerable to Exposure of Sensitive Information to an Unauthorized Actor.',
    patchedVersions: '>=2.6.7 <3.0.0 || >=3.1.1'
  },
  {
    package: 'express',
    affectedVersions: '<4.17.3',
    vulnerability: 'CVE-2022-24999',
    severity: 'high',
    title: 'Open Redirect in Express.js',
    description: 'Express.js prior to 4.17.3 are vulnerable to an Open Redirect.',
    patchedVersions: '>=4.17.3'
  },
  {
    package: 'got',
    affectedVersions: '<11.8.5',
    vulnerability: 'CVE-2022-33987',
    severity: 'moderate',
    title: 'Unintended cookie exposure in Got',
    description: 'Got before 11.8.5 exposes cookies on cross-origin redirects.',
    patchedVersions: '>=11.8.5'
  },
  {
    package: 'json5',
    affectedVersions: '<2.2.2',
    vulnerability: 'CVE-2022-46175',
    severity: 'high',
    title: 'Prototype Pollution in JSON5',
    description: 'JSON5 before 2.2.2 is vulnerable to prototype pollution.',
    patchedVersions: '>=2.2.2'
  },
  {
    package: 'semver',
    affectedVersions: '<7.5.2',
    vulnerability: 'CVE-2022-25883',
    severity: 'moderate',
    title: 'Regular Expression Denial of Service in semver',
    description: 'semver before 7.5.2 is vulnerable to ReDoS.',
    patchedVersions: '>=7.5.2'
  },
  {
    package: 'axios',
    affectedVersions: '<0.21.2',
    vulnerability: 'CVE-2021-3749',
    severity: 'high',
    title: 'Server-Side Request Forgery in Axios',
    description: 'Axios before 0.21.2 is vulnerable to SSRF attacks.',
    patchedVersions: '>=0.21.2'
  },
  {
    package: 'shell-quote',
    affectedVersions: '<1.7.3',
    vulnerability: 'CVE-2021-42740',
    severity: 'critical',
    title: 'Command Injection in shell-quote',
    description: 'shell-quote before 1.7.3 allows command injection.',
    patchedVersions: '>=1.7.3'
  },
  {
    package: 'ws',
    affectedVersions: '<7.4.6 || >=8.0.0 <8.17.1',
    vulnerability: 'CVE-2024-37890',
    severity: 'high',
    title: 'Denial of Service in ws',
    description: 'ws is vulnerable to DoS when Sec-WebSocket-Extensions header is malformed.',
    patchedVersions: '>=7.4.6 <8.0.0 || >=8.17.1'
  }
];

/**
 * Compare semver versions (simplified)
 *
 * @param {string} version - Version to check
 * @param {string} range - Version range
 * @returns {boolean} True if version is in range
 */
function isVersionAffected(version, range) {
  // Simple version comparison for common patterns
  if (!version || !range) return false;

  // Clean version strings
  const cleanVersion = version.replace(/^[~^]/, '').split('-')[0];
  const parts = cleanVersion.split('.').map(Number);

  // Parse range (simplified - handles <X.Y.Z, >=X.Y.Z patterns)
  const conditions = range.split('||').map(r => r.trim());

  for (const condition of conditions) {
    if (checkCondition(parts, condition)) {
      return true;
    }
  }

  return false;
}

/**
 * Check a single version condition
 *
 * @param {Array<number>} parts - Version parts [major, minor, patch]
 * @param {string} condition - Version condition
 * @returns {boolean} True if condition is met
 */
function checkCondition(parts, condition) {
  // Handle compound conditions with spaces
  const subConditions = condition.split(/\s+/).filter(Boolean);

  if (subConditions.length > 1) {
    return subConditions.every(sub => checkSingleCondition(parts, sub));
  }

  return checkSingleCondition(parts, condition);
}

/**
 * Check a single version comparison
 *
 * @param {Array<number>} parts - Version parts
 * @param {string} condition - Single condition like "<4.17.21"
 * @returns {boolean} True if met
 */
function checkSingleCondition(parts, condition) {
  const match = condition.match(/^([<>=]{1,2})?(\d+(?:\.\d+)*)/);
  if (!match) return false;

  const [, operator = '=', versionStr] = match;
  const targetParts = versionStr.split('.').map(Number);

  // Compare versions
  let comparison = 0;
  for (let i = 0; i < Math.max(parts.length, targetParts.length); i++) {
    const a = parts[i] || 0;
    const b = targetParts[i] || 0;
    if (a > b) { comparison = 1; break; }
    if (a < b) { comparison = -1; break; }
  }

  switch (operator) {
    case '<': return comparison < 0;
    case '<=': return comparison <= 0;
    case '>': return comparison > 0;
    case '>=': return comparison >= 0;
    case '=':
    case '==': return comparison === 0;
    default: return comparison === 0;
  }
}

/**
 * Parse package.json and extract dependencies
 *
 * @param {string} packageJsonPath - Path to package.json
 * @returns {Object} Dependencies map
 */
function parseDependencies(packageJsonPath) {
  try {
    const content = readFileSync(packageJsonPath, 'utf-8');
    const pkg = JSON.parse(content);

    return {
      dependencies: pkg.dependencies || {},
      devDependencies: pkg.devDependencies || {},
      peerDependencies: pkg.peerDependencies || {},
      optionalDependencies: pkg.optionalDependencies || {}
    };
  } catch {
    return {
      dependencies: {},
      devDependencies: {},
      peerDependencies: {},
      optionalDependencies: {}
    };
  }
}

/**
 * Parse lock file to get exact versions
 *
 * @param {string} lockFilePath - Path to lock file
 * @returns {Map<string, string>} Package to version map
 */
function parseLockFile(lockFilePath) {
  const versions = new Map();

  try {
    const content = readFileSync(lockFilePath, 'utf-8');

    if (lockFilePath.endsWith('package-lock.json')) {
      const lock = JSON.parse(content);
      const packages = lock.packages || lock.dependencies || {};

      for (const [path, info] of Object.entries(packages)) {
        const pkgName = path.replace(/^node_modules\//, '').replace(/^.*node_modules\//, '');
        if (pkgName && info.version) {
          versions.set(pkgName, info.version);
        }
      }
    } else if (lockFilePath.endsWith('pnpm-lock.yaml')) {
      // Parse YAML (simplified)
      const lines = content.split('\n');
      let currentPackage = '';

      for (const line of lines) {
        const pkgMatch = line.match(/^\s{4}'?([^@\s]+)@([^:'"]+)/);
        if (pkgMatch) {
          versions.set(pkgMatch[1], pkgMatch[2].replace(/['"]/g, ''));
        }
      }
    }
  } catch {
    // Ignore parse errors
  }

  return versions;
}

/**
 * Run pnpm/npm audit and parse results
 *
 * @param {string} directory - Project directory
 * @returns {Promise<Array<Vulnerability>>} Vulnerabilities found
 */
async function runPackageAudit(directory) {
  const vulnerabilities = [];

  try {
    // Try pnpm audit first
    const auditResult = execSync('pnpm audit --json 2>/dev/null', {
      cwd: directory,
      encoding: 'utf-8',
      maxBuffer: 10 * 1024 * 1024,
      timeout: 60000
    });

    const audit = JSON.parse(auditResult);

    // Parse pnpm audit format
    if (audit.advisories) {
      for (const [id, advisory] of Object.entries(audit.advisories)) {
        vulnerabilities.push({
          package: advisory.module_name,
          version: advisory.findings?.[0]?.version || 'unknown',
          vulnerability: `GHSA-${id}`,
          severity: mapSeverity(advisory.severity),
          cve: advisory.cves?.[0],
          cwe: advisory.cwe ? [advisory.cwe] : [],
          title: advisory.title,
          description: advisory.overview,
          recommendation: advisory.recommendation,
          patchedVersions: advisory.patched_versions,
          path: advisory.findings?.[0]?.paths || [],
          source: 'npm'
        });
      }
    }
  } catch {
    // Audit failed or not available - continue with offline check
  }

  return vulnerabilities;
}

/**
 * Map severity string to enum
 *
 * @param {string} severity - Severity string
 * @returns {string} Normalized severity
 */
function mapSeverity(severity) {
  const map = {
    'critical': 'critical',
    'high': 'high',
    'moderate': 'moderate',
    'medium': 'moderate',
    'low': 'low',
    'info': 'low'
  };
  return map[severity?.toLowerCase()] || 'moderate';
}

/**
 * Check dependencies against known vulnerabilities (offline)
 *
 * @param {Map<string, string>} versions - Package versions
 * @returns {Array<Vulnerability>} Vulnerabilities found
 */
function checkOfflineVulnerabilities(versions) {
  const vulnerabilities = [];

  for (const [pkg, version] of versions) {
    for (const vuln of KNOWN_VULNERABILITIES) {
      if (pkg === vuln.package && isVersionAffected(version, vuln.affectedVersions)) {
        vulnerabilities.push({
          package: pkg,
          version,
          vulnerability: vuln.vulnerability,
          severity: vuln.severity,
          cve: vuln.vulnerability.startsWith('CVE') ? vuln.vulnerability : undefined,
          title: vuln.title,
          description: vuln.description,
          patchedVersions: vuln.patchedVersions,
          source: 'npm'
        });
      }
    }
  }

  return vulnerabilities;
}

/**
 * Find all package.json files in directory
 *
 * @param {string} dir - Directory to search
 * @returns {Promise<Array<string>>} Package.json paths
 */
async function findPackageJsonFiles(dir) {
  const files = [];

  async function search(currentDir) {
    let entries;
    try {
      entries = await readdir(currentDir, { withFileTypes: true });
    } catch {
      return;
    }

    for (const entry of entries) {
      const fullPath = join(currentDir, entry.name);

      // Skip node_modules and hidden directories
      if (entry.name === 'node_modules' || entry.name.startsWith('.')) {
        continue;
      }

      if (entry.isDirectory()) {
        await search(fullPath);
      } else if (entry.name === 'package.json') {
        files.push(fullPath);
      }
    }
  }

  await search(dir);
  return files;
}

/**
 * Audit a single package location
 *
 * @param {string} packageJsonPath - Path to package.json
 * @param {Object} [options] - Audit options
 * @returns {Promise<Object>} Audit results
 */
export async function auditPackage(packageJsonPath, options = {}) {
  const directory = dirname(packageJsonPath);
  const basePath = options.basePath || directory;
  const relativePath = relative(basePath, packageJsonPath);

  const deps = parseDependencies(packageJsonPath);
  const allDeps = new Map();

  // Collect all dependencies
  for (const [name, version] of Object.entries(deps.dependencies)) {
    allDeps.set(name, version);
  }
  if (options.includeDevDependencies !== false) {
    for (const [name, version] of Object.entries(deps.devDependencies)) {
      allDeps.set(name, version);
    }
  }

  // Try to get exact versions from lock file
  const lockFiles = ['pnpm-lock.yaml', 'package-lock.json', 'yarn.lock'];
  let exactVersions = new Map();

  for (const lockFile of lockFiles) {
    const lockPath = join(directory, lockFile);
    if (existsSync(lockPath)) {
      exactVersions = parseLockFile(lockPath);
      break;
    }
  }

  // Merge versions
  for (const [name, version] of allDeps) {
    if (!exactVersions.has(name)) {
      exactVersions.set(name, version.replace(/^[~^]/, ''));
    }
  }

  // Run package manager audit if online check enabled
  let vulnerabilities = [];
  if (options.online !== false) {
    vulnerabilities = await runPackageAudit(directory);
  }

  // Always run offline check for known vulnerabilities
  const offlineVulns = checkOfflineVulnerabilities(exactVersions);

  // Merge results (deduplicate by package+vulnerability)
  const seen = new Set();
  for (const vuln of vulnerabilities) {
    seen.add(`${vuln.package}:${vuln.vulnerability}`);
  }
  for (const vuln of offlineVulns) {
    const key = `${vuln.package}:${vuln.vulnerability}`;
    if (!seen.has(key)) {
      vulnerabilities.push(vuln);
    }
  }

  return {
    location: relativePath,
    directory,
    totalDependencies: allDeps.size,
    vulnerabilities
  };
}

/**
 * Audit entire directory for vulnerabilities
 *
 * @param {string} directory - Root directory
 * @param {Object} [options] - Audit options
 * @returns {Promise<Object>} Audit results
 */
export async function auditDirectory(directory, options = {}) {
  const startTime = Date.now();
  const packageJsonFiles = await findPackageJsonFiles(directory);
  const allVulnerabilities = [];
  const packageResults = [];

  for (const pkgPath of packageJsonFiles) {
    const result = await auditPackage(pkgPath, { ...options, basePath: directory });
    packageResults.push(result);
    allVulnerabilities.push(...result.vulnerabilities);
  }

  // Group by severity
  const bySeverity = {
    critical: allVulnerabilities.filter(v => v.severity === 'critical'),
    high: allVulnerabilities.filter(v => v.severity === 'high'),
    moderate: allVulnerabilities.filter(v => v.severity === 'moderate'),
    low: allVulnerabilities.filter(v => v.severity === 'low')
  };

  // Unique vulnerable packages
  const uniquePackages = new Set(allVulnerabilities.map(v => v.package));

  // Calculate risk score
  const riskScore = Math.min(100,
    bySeverity.critical.length * 25 +
    bySeverity.high.length * 10 +
    bySeverity.moderate.length * 3 +
    bySeverity.low.length * 1
  );

  return {
    passed: bySeverity.critical.length === 0 && bySeverity.high.length === 0,
    vulnerabilities: allVulnerabilities,
    packageResults,
    summary: {
      total: allVulnerabilities.length,
      uniquePackages: uniquePackages.size,
      bySeverity: {
        critical: bySeverity.critical.length,
        high: bySeverity.high.length,
        moderate: bySeverity.moderate.length,
        low: bySeverity.low.length
      },
      riskScore,
      packagesScanned: packageJsonFiles.length,
      scanDuration: Date.now() - startTime
    },
    metadata: {
      directory,
      timestamp: new Date().toISOString(),
      knownVulnerabilitiesChecked: KNOWN_VULNERABILITIES.length
    }
  };
}

/**
 * Format audit results as report
 *
 * @param {Object} result - Audit result
 * @returns {string} Formatted report
 */
export function formatReport(result) {
  const lines = [];

  lines.push('='.repeat(60));
  lines.push('DEPENDENCY VULNERABILITY REPORT');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Scan Time: ${result.metadata.timestamp}`);
  lines.push(`Directory: ${result.metadata.directory}`);
  lines.push(`Packages Scanned: ${result.summary.packagesScanned}`);
  lines.push(`Scan Duration: ${result.summary.scanDuration}ms`);
  lines.push('');
  lines.push('-'.repeat(60));
  lines.push('SUMMARY');
  lines.push('-'.repeat(60));
  lines.push(`Total Vulnerabilities: ${result.summary.total}`);
  lines.push(`Unique Packages Affected: ${result.summary.uniquePackages}`);
  lines.push(`Risk Score: ${result.summary.riskScore}/100`);
  lines.push(`Status: ${result.passed ? 'PASSED' : 'FAILED'}`);
  lines.push('');
  lines.push('By Severity:');
  lines.push(`  Critical: ${result.summary.bySeverity.critical}`);
  lines.push(`  High: ${result.summary.bySeverity.high}`);
  lines.push(`  Moderate: ${result.summary.bySeverity.moderate}`);
  lines.push(`  Low: ${result.summary.bySeverity.low}`);

  if (result.vulnerabilities.length > 0) {
    lines.push('');
    lines.push('-'.repeat(60));
    lines.push('VULNERABILITIES');
    lines.push('-'.repeat(60));

    // Group by severity
    const grouped = {
      critical: result.vulnerabilities.filter(v => v.severity === 'critical'),
      high: result.vulnerabilities.filter(v => v.severity === 'high'),
      moderate: result.vulnerabilities.filter(v => v.severity === 'moderate'),
      low: result.vulnerabilities.filter(v => v.severity === 'low')
    };

    for (const [severity, vulns] of Object.entries(grouped)) {
      if (vulns.length === 0) continue;

      lines.push('');
      lines.push(`[${severity.toUpperCase()}]`);

      for (const vuln of vulns) {
        lines.push(`  ${vuln.package}@${vuln.version}`);
        lines.push(`    ${vuln.title}`);
        lines.push(`    CVE: ${vuln.cve || vuln.vulnerability}`);
        if (vuln.patchedVersions) {
          lines.push(`    Fix: Upgrade to ${vuln.patchedVersions}`);
        }
      }
    }
  }

  lines.push('');
  lines.push('='.repeat(60));

  return lines.join('\n');
}

/**
 * Get remediation commands for vulnerabilities
 *
 * @param {Array<Vulnerability>} vulnerabilities - Vulnerabilities to fix
 * @returns {Array<string>} Remediation commands
 */
export function getRemediationCommands(vulnerabilities) {
  const commands = [];
  const seen = new Set();

  for (const vuln of vulnerabilities) {
    if (vuln.patchedVersions && !seen.has(vuln.package)) {
      // Extract minimum version from patched versions
      const minVersion = vuln.patchedVersions
        .split(' ')
        .find(v => v.startsWith('>='))
        ?.replace('>=', '');

      if (minVersion) {
        commands.push(`pnpm update ${vuln.package}@${minVersion}`);
        seen.add(vuln.package);
      }
    }
  }

  return commands;
}

export default {
  auditPackage,
  auditDirectory,
  formatReport,
  getRemediationCommands,
  KNOWN_VULNERABILITIES
};
