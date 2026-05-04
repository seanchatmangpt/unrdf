/**
 * @file Changelog Generator
 * @module kgc-docs/changelog-generator
 *
 * Generate changelogs from version-tracked KGC Markdown documents
 */

import { readFileSync, existsSync } from 'node:fs';
import { parseFrontmatter } from './parser.mjs';

/**
 * Semver comparison function
 * @param {string} v1 - First version (e.g., "1.2.3")
 * @param {string} v2 - Second version
 * @returns {number} -1 if v1 < v2, 0 if equal, 1 if v1 > v2
 */
export function compareVersions(v1, v2) {
  const parts1 = v1.split('.').map(Number);
  const parts2 = v2.split('.').map(Number);

  for (let i = 0; i < 3; i++) {
    const p1 = parts1[i] || 0;
    const p2 = parts2[i] || 0;
    if (p1 > p2) return 1;
    if (p1 < p2) return -1;
  }

  return 0;
}

/**
 * Parse version from frontmatter
 * @param {string} filePath - Path to KGC Markdown file
 * @returns {object} Version info
 */
export function parseVersionInfo(filePath) {
  if (!existsSync(filePath)) {
    throw new Error(`File not found: ${filePath}`);
  }

  const content = readFileSync(filePath, 'utf-8');
  const frontmatter = parseFrontmatter(content);

  return {
    version: frontmatter.version || '0.0.0',
    author: frontmatter.author || 'Unknown',
    created: frontmatter.created || null,
    updated: frontmatter.updated || null,
    status: frontmatter.status || 'unknown',
  };
}

/**
 * Determine change type from version diff
 * @param {string} oldVersion - Previous version
 * @param {string} newVersion - Current version
 * @returns {string} Change type: 'major', 'minor', 'patch', or 'none'
 */
export function getChangeType(oldVersion, newVersion) {
  const oldParts = oldVersion.split('.').map(Number);
  const newParts = newVersion.split('.').map(Number);

  if (newParts[0] > oldParts[0]) return 'major';
  if (newParts[1] > oldParts[1]) return 'minor';
  if (newParts[2] > oldParts[2]) return 'patch';

  return 'none';
}

/**
 * Generate changelog entry from version info
 * @param {object} versionInfo - Version information object
 * @param {string} changeDescription - Description of changes
 * @returns {string} Changelog entry in markdown format
 */
export function generateChangelogEntry(versionInfo, changeDescription) {
  const date = versionInfo.updated || new Date().toISOString().split('T')[0];

  let entry = `## [${versionInfo.version}] - ${date}\n\n`;

  if (versionInfo.author) {
    entry += `**Author**: ${versionInfo.author}\n\n`;
  }

  entry += `${changeDescription}\n\n`;

  return entry;
}

/**
 * Generate complete changelog from version history
 * @param {Array<object>} versions - Array of version info objects
 * @returns {string} Complete changelog in markdown format
 */
export function generateChangelog(versions) {
  if (!versions || versions.length === 0) {
    return '# Changelog\n\nNo version history available.\n';
  }

  // Sort versions descending (newest first)
  const sorted = [...versions].sort((a, b) =>
    compareVersions(b.version, a.version)
  );

  let changelog = '# Changelog\n\n';
  changelog += `All notable changes to this document are tracked here.\n\n`;
  changelog += `Format based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),\n`;
  changelog += `and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).\n\n`;

  for (const version of sorted) {
    const date = version.updated || version.created || 'Unknown date';
    changelog += `## [${version.version}] - ${date}\n\n`;

    if (version.author) {
      changelog += `**Author**: ${version.author}\n\n`;
    }

    if (version.description) {
      changelog += `${version.description}\n\n`;
    } else {
      changelog += `Version ${version.version} release.\n\n`;
    }
  }

  return changelog;
}

/**
 * Track version changes across document history
 * @param {string} currentPath - Path to current version
 * @param {Array<string>} previousPaths - Paths to previous versions
 * @returns {object} Version tracking summary
 */
export function trackVersionChanges(currentPath, previousPaths = []) {
  const current = parseVersionInfo(currentPath);
  const history = previousPaths.map(parseVersionInfo);

  const changes = [];

  for (let i = 0; i < history.length; i++) {
    const prev = history[i];
    const next = i > 0 ? history[i - 1] : current;

    changes.push({
      from: prev.version,
      to: next.version,
      type: getChangeType(prev.version, next.version),
      date: next.updated || next.created,
    });
  }

  return {
    current_version: current.version,
    total_versions: history.length + 1,
    changes,
    latest_update: current.updated,
  };
}
