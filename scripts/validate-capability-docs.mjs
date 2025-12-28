#!/usr/bin/env node

/**
 * @fileoverview Quality gate validation for capability documentation
 *
 * Validates:
 * - All packages have capability maps
 * - Cross-references are valid
 * - Required sections present
 * - Links are not broken
 * - Metadata consistency
 *
 * @module scripts/validate-capability-docs
 */

import { readFile, readdir, access } from 'node:fs/promises';
import { resolve, join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { existsSync } from 'node:fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = resolve(__dirname, '..');
const PACKAGES_DIR = join(ROOT_DIR, 'packages');
const DOCS_DIR = join(ROOT_DIR, 'docs', 'capabilities');

/**
 * Required sections in capability maps
 * Updated to match actual generated format from generate-capability-maps.mjs
 */
const REQUIRED_SECTIONS = [
  '## Description',
  '## Capability Atoms',
  '## Package Metadata',
  '## Integration Patterns',
  '## Evidence Trail',
];

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  return {
    strict: args.includes('--strict'),
    checkRefs: args.includes('--check-refs'),
    checkMissing: args.includes('--check-missing'),
    summary: args.includes('--summary'),
    verbose: args.includes('--verbose') || args.includes('-v'),
  };
}

/**
 * Get all package names
 * @returns {Promise<string[]>}
 */
async function getAllPackages() {
  const entries = await readdir(PACKAGES_DIR, { withFileTypes: true });
  return entries
    .filter(entry => entry.isDirectory() && !entry.name.startsWith('.'))
    .map(entry => entry.name);
}

/**
 * Check if capability map exists for package
 * @param {string} packageName
 * @returns {Promise<boolean>}
 */
async function capabilityMapExists(packageName) {
  const mapPath = join(DOCS_DIR, packageName, 'capability-map.md');
  return existsSync(mapPath);
}

/**
 * Read capability map
 * @param {string} packageName
 * @returns {Promise<string|null>}
 */
async function readCapabilityMap(packageName) {
  const mapPath = join(DOCS_DIR, packageName, 'capability-map.md');

  try {
    return await readFile(mapPath, 'utf-8');
  } catch {
    return null;
  }
}

/**
 * Validate required sections are present
 * @param {string} content
 * @returns {{ valid: boolean, missing: string[] }}
 */
function validateSections(content) {
  const missing = REQUIRED_SECTIONS.filter(section => !content.includes(section));

  return {
    valid: missing.length === 0,
    missing,
  };
}

/**
 * Extract and validate cross-references
 * @param {string} content
 * @param {string} packageName
 * @param {Set<string>} allPackages
 * @returns {{ valid: boolean, broken: string[] }}
 */
function validateCrossReferences(content, packageName, allPackages) {
  const broken = [];

  // Match markdown links: [text](../package/capability-map.md)
  const linkRegex = /\[([^\]]+)\]\(\.\.\/([^/]+)\/capability-map\.md\)/g;
  const matches = [...content.matchAll(linkRegex)];

  for (const match of matches) {
    const referencedPackage = match[2];
    if (!allPackages.has(referencedPackage)) {
      broken.push(`${packageName} -> ${referencedPackage}`);
    }
  }

  return {
    valid: broken.length === 0,
    broken,
  };
}

/**
 * Validate metadata consistency
 * @param {string} content
 * @param {string} packageName
 * @returns {{ valid: boolean, issues: string[] }}
 */
function validateMetadata(content) {
  const issues = [];

  // Check for required metadata fields
  if (!content.match(/\*\*Version\*\*:/)) {
    issues.push('Missing version metadata');
  }

  if (!content.match(/\*\*Generated\*\*:/)) {
    issues.push('Missing generation timestamp');
  }

  if (!content.match(/\*\*Status\*\*:/)) {
    issues.push('Missing status indicator');
  }

  // Check for package name consistency
  const nameMatch = content.match(/\*\*Name\*\*:\s*`(@unrdf\/[^`]+)`/);
  if (!nameMatch) {
    issues.push('Missing or malformed package name');
  }

  return {
    valid: issues.length === 0,
    issues,
  };
}

/**
 * Validate a single capability map
 * @param {string} packageName
 * @param {Set<string>} allPackages
 * @param {boolean} checkRefs
 * @returns {Promise<{ valid: boolean, errors: string[], warnings: string[] }>}
 */
async function validateCapabilityMap(packageName, allPackages, checkRefs = false) {
  const errors = [];
  const warnings = [];

  const content = await readCapabilityMap(packageName);

  if (!content) {
    errors.push(`Missing capability map for ${packageName}`);
    return { valid: false, errors, warnings };
  }

  // Validate sections
  const sectionsResult = validateSections(content);
  if (!sectionsResult.valid) {
    errors.push(`Missing sections in ${packageName}: ${sectionsResult.missing.join(', ')}`);
  }

  // Validate metadata
  const metadataResult = validateMetadata(content);
  if (!metadataResult.valid) {
    warnings.push(`Metadata issues in ${packageName}: ${metadataResult.issues.join(', ')}`);
  }

  // Validate cross-references if requested
  if (checkRefs) {
    const refsResult = validateCrossReferences(content, packageName, allPackages);
    if (!refsResult.valid) {
      errors.push(`Broken cross-references in ${packageName}: ${refsResult.broken.join(', ')}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

/**
 * Generate validation summary
 * @param {object[]} results
 * @returns {string}
 */
function generateSummary(results) {
  const total = results.length;
  const valid = results.filter(r => r.valid).length;
  const withErrors = results.filter(r => r.errors.length > 0).length;
  const withWarnings = results.filter(r => r.warnings.length > 0).length;

  return `
### Capability Documentation Validation Summary

- **Total Packages**: ${total}
- **Valid**: ${valid} ✅
- **With Errors**: ${withErrors} ❌
- **With Warnings**: ${withWarnings} ⚠️

**Status**: ${withErrors === 0 ? '✅ All validations passed' : `❌ ${withErrors} package(s) with errors`}
`;
}

/**
 * Main execution
 */
async function main() {
  const config = parseArgs();

  console.log('🔍 Validating capability documentation...\n');

  const packages = await getAllPackages();
  const packageSet = new Set(packages);

  console.log(`📋 Found ${packages.length} packages\n`);

  // Check for missing capability maps
  if (config.checkMissing) {
    const missing = [];
    for (const pkg of packages) {
      if (!(await capabilityMapExists(pkg))) {
        missing.push(pkg);
      }
    }

    if (missing.length > 0) {
      console.log(`⚠️  Missing capability maps (${missing.length}):`);
      for (const pkg of missing) {
        console.log(`   - ${pkg}`);
      }
      console.log();
    } else {
      console.log('✅ All packages have capability maps\n');
    }
  }

  // Validate existing capability maps
  const results = [];
  let errorCount = 0;
  let warningCount = 0;

  for (const pkg of packages) {
    if (!(await capabilityMapExists(pkg))) {
      if (config.strict) {
        errorCount++;
        console.log(`❌ ${pkg}: Missing capability map`);
      }
      continue;
    }

    const result = await validateCapabilityMap(pkg, packageSet, config.checkRefs);
    results.push({ package: pkg, ...result });

    if (result.errors.length > 0) {
      errorCount++;
      console.log(`❌ ${pkg}:`);
      for (const error of result.errors) {
        console.log(`   ${error}`);
      }
    } else if (config.verbose || result.warnings.length > 0) {
      if (result.warnings.length > 0) {
        warningCount++;
        console.log(`⚠️  ${pkg}:`);
        for (const warning of result.warnings) {
          console.log(`   ${warning}`);
        }
      } else if (config.verbose) {
        console.log(`✅ ${pkg}`);
      }
    }
  }

  // Print summary
  if (config.summary) {
    console.log(generateSummary(results));
  } else {
    console.log(`\n📊 Summary: ${results.filter(r => r.valid).length}/${results.length} valid`);
    if (errorCount > 0) {
      console.log(`   ❌ ${errorCount} errors`);
    }
    if (warningCount > 0) {
      console.log(`   ⚠️  ${warningCount} warnings`);
    }
  }

  // Exit with error if strict mode and errors found
  if (config.strict && errorCount > 0) {
    console.log('\n❌ Validation failed in strict mode');
    process.exit(1);
  }

  console.log('\n✅ Validation complete');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
}

export { validateCapabilityMap, validateSections, validateCrossReferences, validateMetadata };
