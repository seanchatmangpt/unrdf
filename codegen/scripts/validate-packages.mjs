#!/usr/bin/env node

import { REGISTRY, getAll, getPackage } from '../generated/index.mjs';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../..');

// Validate package metadata
async function validatePackages() {
  const all = getAll();
  const results = {
    timestamp: new Date().toISOString(),
    total: all.length,
    valid: 0,
    invalid: 0,
    issues: [],
    packages: {}
  };

  for (const pkg of all) {
    const pkgPath = path.join(projectRoot, 'packages', pkg.path, 'package.json');
    const validation = {
      name: pkg.name,
      version: pkg.version,
      path: pkg.path,
      valid: true,
      checks: {
        fileExists: fs.existsSync(pkgPath),
        hasName: !!pkg.name,
        hasVersion: !!pkg.version,
        hasDescription: !!pkg.description,
        hasMainExport: !!pkg.main,
        hasExports: Object.keys(pkg.exports || {}).length > 0,
        hasDependencies: pkg.dependencies.length > 0,
        isPublishable: pkg.name.startsWith('@unrdf/')
      }
    };

    // Count issues
    Object.entries(validation.checks).forEach(([check, passed]) => {
      if (!passed && ['fileExists', 'hasName', 'hasVersion'].includes(check)) {
        validation.valid = false;
        results.issues.push({
          package: pkg.name,
          check,
          severity: 'error'
        });
      }
    });

    if (validation.valid) {
      results.valid++;
    } else {
      results.invalid++;
    }

    results.packages[pkg.name] = validation;
  }

  return results;
}

// Generate compatibility matrix
function generateCompatibilityMatrix() {
  const all = getAll();
  const matrix = {
    generated: new Date().toISOString(),
    packages: all.length,
    tiers: {},
    versionMatrix: {},
    compatibility: {}
  };

  // By tier
  ['essential', 'extended', 'optional'].forEach(tier => {
    const pkgs = all.filter(p => p.tier === tier);
    matrix.tiers[tier] = {
      count: pkgs.length,
      versions: [...new Set(pkgs.map(p => p.version))],
      allVersionCompatible: pkgs.every(p => p.version.includes('5.') || p.version.includes('6.') || p.version.includes('1.0.0'))
    };
  });

  // Version compatibility
  const versions = new Set();
  all.forEach(p => versions.add(p.version));
  versions.forEach(v => {
    const pkgs = all.filter(p => p.version === v);
    matrix.versionMatrix[v] = {
      packages: pkgs.map(p => p.name),
      count: pkgs.length,
      tiers: {
        essential: pkgs.filter(p => p.tier === 'essential').length,
        extended: pkgs.filter(p => p.tier === 'extended').length,
        optional: pkgs.filter(p => p.tier === 'optional').length
      }
    };
  });

  // Internal dependency compatibility
  all.forEach(pkg => {
    const internalDeps = pkg.dependencies.filter(d => d.startsWith('@unrdf'));
    matrix.compatibility[pkg.name] = {
      totalDependencies: pkg.dependencies.length,
      internalDependencies: internalDeps.length,
      externalDependencies: pkg.dependencies.length - internalDeps.length,
      dependenciesExist: internalDeps.every(dep => all.some(p => p.name === dep))
    };
  });

  return matrix;
}

// Main
async function main() {
  console.log('🔍 Validating packages...');
  const validation = await validatePackages();

  const reportDir = path.join(projectRoot, 'docs', 'generated');
  fs.mkdirSync(reportDir, { recursive: true });

  // Write validation report
  fs.writeFileSync(
    path.join(reportDir, 'validation-report.json'),
    JSON.stringify(validation, null, 2)
  );
  console.log(`   ✓ Validation report: ${validation.valid}/${validation.total} valid`);

  // Generate compatibility matrix
  console.log('🔗 Generating compatibility matrix...');
  const matrix = generateCompatibilityMatrix();
  fs.writeFileSync(
    path.join(reportDir, 'compatibility-matrix.json'),
    JSON.stringify(matrix, null, 2)
  );
  console.log(`   ✓ Compatibility matrix generated`);

  // Summary
  console.log('\n📊 Summary:');
  console.log(`   Total packages: ${validation.total}`);
  console.log(`   Valid: ${validation.valid}`);
  console.log(`   Invalid: ${validation.invalid}`);

  if (validation.issues.length > 0) {
    console.log(`\n⚠️  Issues found: ${validation.issues.length}`);
    validation.issues.forEach(issue => {
      console.log(`   - ${issue.package}: ${issue.check}`);
    });
  } else {
    console.log(`\n✅ All packages valid!`);
  }

  // Version stats
  console.log('\n📦 Version Distribution:');
  Object.entries(matrix.versionMatrix).forEach(([version, data]) => {
    console.log(`   ${version}: ${data.count} packages`);
  });

  console.log('\n✅ Validation complete');
}

main().catch(console.error);
