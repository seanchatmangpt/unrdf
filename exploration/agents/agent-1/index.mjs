#!/usr/bin/env node

/**
 * UNRDF Substrate Capability Scanner - Agent 1
 * Scans all @unrdf packages to discover APIs, exports, and maturity levels
 *
 * Usage: node /home/user/unrdf/exploration/agents/agent-1/index.mjs
 * Output: /home/user/unrdf/exploration/capability-map.json
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PACKAGES_DIR = '/home/user/unrdf/packages';
const OUTPUT_PATH = '/home/user/unrdf/exploration/capability-map.json';

/**
 * Role classification based on package name, description, and dependencies
 */
function classifyRole(pkgName, pkgData) {
  const roles = [];
  const desc = (pkgData.description || '').toLowerCase();
  const keywords = (pkgData.keywords || []).join(' ').toLowerCase();

  // Store role
  if (
    desc.includes('store') ||
    desc.includes('oxigraph') ||
    pkgName.includes('oxigraph')
  ) {
    roles.push('store');
  }

  // IO role
  if (
    desc.includes('stream') ||
    desc.includes('io') ||
    desc.includes('serializ') ||
    pkgName.includes('streaming')
  ) {
    roles.push('io');
  }

  // Derivation/reasoning role
  if (
    desc.includes('derive') ||
    desc.includes('infer') ||
    desc.includes('reason') ||
    desc.includes('engine') ||
    desc.includes('knowledge') ||
    pkgName.includes('knowledge-engine') ||
    pkgName.includes('kgc-4d')
  ) {
    roles.push('derive');
  }

  // Enforcement/validation role
  if (
    desc.includes('enforce') ||
    desc.includes('validat') ||
    desc.includes('hook') ||
    desc.includes('policy') ||
    pkgName.includes('hooks') ||
    pkgName.includes('validation')
  ) {
    roles.push('enforce');
  }

  // Rendering role
  if (
    desc.includes('render') ||
    desc.includes('visual') ||
    desc.includes('graph') ||
    pkgName.includes('react') ||
    pkgName.includes('viz')
  ) {
    roles.push('render');
  }

  // Core utilities
  if (pkgName.includes('core') && roles.length === 0) {
    roles.push('core');
  }

  return roles.length > 0 ? roles : ['utility'];
}

/**
 * Extract main exports from package.json
 */
function extractExports(pkgData) {
  if (!pkgData.exports) {
    return [];
  }

  if (typeof pkgData.exports === 'string') {
    return [pkgData.exports];
  }

  if (typeof pkgData.exports === 'object') {
    return Object.values(pkgData.exports)
      .filter((v) => typeof v === 'string')
      .map((v) => v.replace(/^\.\//g, ''));
  }

  return [];
}

/**
 * Assess maturity level
 */
function assessMaturity(pkgName, pkgPath, pkgData) {
  const maturitySignals = {
    hasTests: false,
    hasExamples: false,
    hasReadme: false,
    hasChangeLog: false,
    version: pkgData.version,
  };

  try {
    const files = fs.readdirSync(pkgPath);

    if (files.includes('test') || files.includes('tests')) {
      maturitySignals.hasTests = true;
    }

    if (files.includes('examples') || files.includes('example')) {
      maturitySignals.hasExamples = true;
    }

    if (files.includes('README.md')) {
      maturitySignals.hasReadme = true;
    }

    if (files.includes('CHANGELOG.md')) {
      maturitySignals.hasChangeLog = true;
    }
  } catch (e) {
    // Ignore
  }

  // Determine maturity level
  let maturity = 'experimental';

  if (maturitySignals.hasTests && maturitySignals.hasReadme) {
    maturity = 'mature';
  } else if (maturitySignals.hasTests || maturitySignals.hasExamples) {
    maturity = 'stable';
  } else if (maturitySignals.hasReadme) {
    maturity = 'documented';
  }

  return { maturity, signals: maturitySignals };
}

/**
 * Extract exports from src/index.mjs
 */
function extractSourceExports(pkgPath) {
  const indexFile = path.join(pkgPath, 'src', 'index.mjs');
  try {
    const content = fs.readFileSync(indexFile, 'utf-8');

    // Simple extraction of export statements
    const exportMatches = content.match(
      /export\s+(?:function|const|class|{\s*\w+[^}]*}|default)\s+(\w+)/g
    );
    if (exportMatches) {
      return exportMatches.map((m) => m.replace(/export\s+/, '').split(/\s+/)[0]);
    }
  } catch (e) {
    // File doesn't exist or can't be read
  }

  return [];
}

/**
 * Find example files
 */
function findExamplePath(pkgPath) {
  const exampleDirs = ['examples', 'example', 'demo', 'docs/examples'];

  for (const dir of exampleDirs) {
    const examplePath = path.join(pkgPath, dir);
    if (fs.existsSync(examplePath)) {
      return dir;
    }
  }

  return null;
}

/**
 * Main scanner function
 */
async function scanPackages() {
  console.log('🔍 Scanning UNRDF packages...\n');

  const packages = [];
  const rolesMapping = {};

  try {
    const packageDirs = fs
      .readdirSync(PACKAGES_DIR)
      .filter((f) => {
        const fullPath = path.join(PACKAGES_DIR, f);
        return (
          fs.statSync(fullPath).isDirectory() &&
          fs.existsSync(path.join(fullPath, 'package.json'))
        );
      })
      .sort();

    console.log(
      `Found ${packageDirs.length} packages. Analyzing each...\n`
    );

    for (const pkgDir of packageDirs) {
      const pkgPath = path.join(PACKAGES_DIR, pkgDir);
      const packageJsonPath = path.join(pkgPath, 'package.json');

      try {
        const packageJsonContent = fs.readFileSync(packageJsonPath, 'utf-8');
        const pkgData = JSON.parse(packageJsonContent);

        // Skip private packages
        if (pkgData.private) {
          console.log(`⊘ Skipping private: ${pkgData.name}`);
          continue;
        }

        const pkgName = pkgData.name || `@unrdf/${pkgDir}`;

        // Extract information
        const exportedFromPackageJson = extractExports(pkgData);
        const sourceExports = extractSourceExports(pkgPath);
        const roles = classifyRole(pkgName, pkgData);
        const { maturity, signals } = assessMaturity(pkgName, pkgPath, pkgData);
        const examplePath = findExamplePath(pkgPath);

        const packageInfo = {
          name: pkgName,
          path: `packages/${pkgDir}`,
          version: pkgData.version,
          description: pkgData.description || 'No description',
          role: roles,
          exports: Array.from(
            new Set([
              ...exportedFromPackageJson,
              ...sourceExports,
            ])
          ).slice(0, 10), // Top 10 exports
          mainExport: pkgData.main || 'src/index.mjs',
          maturity,
          signals,
          examples: examplePath,
          keywords: pkgData.keywords || [],
          dependencies: Object.keys(pkgData.dependencies || {}),
          notes: generateNotes(pkgData, signals),
        };

        packages.push(packageInfo);

        // Update roles mapping
        for (const role of roles) {
          if (!rolesMapping[role]) {
            rolesMapping[role] = [];
          }
          rolesMapping[role].push(pkgName);
        }

        console.log(
          `✓ ${pkgName} [${roles.join(',')}] - ${maturity}`
        );
      } catch (err) {
        console.error(`✗ Error processing ${pkgDir}: ${err.message}`);
      }
    }

    // Create capability map
    const capabilityMap = {
      timestamp: new Date().toISOString(),
      scannerVersion: '1.0.0',
      totalPackages: packages.length,
      packages,
      roles_mapping: rolesMapping,
      role_descriptions: {
        store: 'RDF triple/quad storage and querying',
        io: 'Input/Output, serialization, streaming, and synchronization',
        derive:
          'Derivation, inference, reasoning, and knowledge processing',
        enforce:
          'Enforcement, validation, policy execution, and access control',
        render: 'Visualization, rendering, and UI components',
        core: 'Core utilities and foundational infrastructure',
        utility: 'General utility functions',
      },
    };

    // Write output
    fs.writeFileSync(OUTPUT_PATH, JSON.stringify(capabilityMap, null, 2));

    console.log('\n✅ Scan complete!');
    console.log(`📊 Results written to: ${OUTPUT_PATH}`);
    console.log(
      `\n📈 Summary:\n  - Total packages: ${packages.length}\n  - Roles found: ${Object.keys(rolesMapping).length}\n`
    );

    // Print role mapping summary
    console.log('Role Distribution:');
    for (const [role, pkgs] of Object.entries(rolesMapping)) {
      console.log(`  ${role}: ${pkgs.length} packages`);
    }

    return capabilityMap;
  } catch (err) {
    console.error('❌ Error during scan:', err);
    process.exit(1);
  }
}

/**
 * Generate notes about package based on signals
 */
function generateNotes(pkgData, signals) {
  const notes = [];

  if (!signals.hasTests) {
    notes.push('⚠ No test directory found');
  }

  if (!signals.hasExamples) {
    notes.push('⚠ No examples directory found');
  }

  if (!signals.hasReadme) {
    notes.push('⚠ No README.md found');
  }

  const deps = Object.keys(pkgData.dependencies || {});
  if (deps.length === 0) {
    notes.push('ℹ No external dependencies (good isolation)');
  }

  if (
    deps.some((d) =>
      ['@unrdf/', 'workspace:'].some((prefix) => d.startsWith(prefix))
    )
  ) {
    notes.push('ℹ Depends on other @unrdf packages');
  }

  return notes.length > 0 ? notes : ['ℹ Standard package structure'];
}

// Run scanner
scanPackages().catch(console.error);
