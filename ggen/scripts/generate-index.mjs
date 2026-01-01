#!/usr/bin/env node

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../..');

// Read all package.json files and create index
function buildPackageIndex() {
  const packagesDir = path.join(projectRoot, 'packages');
  const dirs = fs.readdirSync(packagesDir);
  const packageJsonFiles = [];

  for (const dir of dirs) {
    const pkgPath = path.join(packagesDir, dir, 'package.json');
    if (fs.existsSync(pkgPath)) {
      packageJsonFiles.push(path.join(dir, 'package.json'));
    }
  }

  const packages = {};
  const byTier = { essential: [], extended: [], optional: [] };

  const essentialNames = [
    '@unrdf/core', '@unrdf/oxigraph', '@unrdf/kgc-4d',
    '@unrdf/yawl', '@unrdf/hooks', '@unrdf/streaming', '@unrdf/v6-core'
  ];

  const extendedNames = [
    '@unrdf/federation', '@unrdf/knowledge-engine', '@unrdf/cli',
    '@unrdf/kgc-runtime', '@unrdf/kgc-substrate', '@unrdf/receipts',
    '@unrdf/consensus', '@unrdf/v6-compat'
  ];

  for (const file of packageJsonFiles) {
    const fullPath = path.join(packagesDir, file);
    const pkgJson = JSON.parse(fs.readFileSync(fullPath, 'utf-8'));
    const pkgName = pkgJson.name;

    let tier = 'optional';
    if (essentialNames.includes(pkgName)) tier = 'essential';
    else if (extendedNames.includes(pkgName)) tier = 'extended';

    const pkgData = {
      name: pkgName,
      version: pkgJson.version,
      description: pkgJson.description,
      tier,
      main: pkgJson.main || 'src/index.mjs',
      exports: pkgJson.exports || {},
      path: path.dirname(file),
      dependencies: Object.keys(pkgJson.dependencies || {}),
      devDependencies: Object.keys(pkgJson.devDependencies || {})
    };

    packages[pkgName] = pkgData;
    byTier[tier].push(pkgData);
  }

  return { packages, byTier };
}

// Generate CommonJS index
function generateCommonJS(data) {
  const { packages, byTier } = data;

  const cjs = `/**
 * UNRDF Package Registry
 * Auto-generated from package.json files
 * Generated: ${new Date().toISOString()}
 */

const PACKAGES = ${JSON.stringify(packages, null, 2)};

const REGISTRY = {
  packages: Object.values(PACKAGES),
  essential: ${JSON.stringify(byTier.essential, null, 2)},
  extended: ${JSON.stringify(byTier.extended, null, 2)},
  optional: ${JSON.stringify(byTier.optional, null, 2)},
  total: Object.keys(PACKAGES).length
};

module.exports = {
  PACKAGES,
  REGISTRY,
  getPackage: (name) => PACKAGES[name],
  findByTier: (tier) => REGISTRY[tier] || [],
  getAll: () => Object.values(PACKAGES),
  getTier: (name) => PACKAGES[name]?.tier
};
`;

  return cjs;
}

// Generate ESM index
function generateESM(data) {
  const { packages, byTier } = data;

  const esm = `/**
 * UNRDF Package Registry
 * Auto-generated from package.json files
 * Generated: ${new Date().toISOString()}
 */

export const PACKAGES = ${JSON.stringify(packages, null, 2)};

export const REGISTRY = {
  packages: Object.values(PACKAGES),
  essential: ${JSON.stringify(byTier.essential, null, 2)},
  extended: ${JSON.stringify(byTier.extended, null, 2)},
  optional: ${JSON.stringify(byTier.optional, null, 2)},
  total: Object.keys(PACKAGES).length
};

export function getPackage(name) {
  return PACKAGES[name];
}

export function findByTier(tier) {
  return REGISTRY[tier] || [];
}

export function getAll() {
  return Object.values(PACKAGES);
}

export function getTier(name) {
  return PACKAGES[name]?.tier;
}

export function getEssential() {
  return REGISTRY.essential;
}

export function getExtended() {
  return REGISTRY.extended;
}

export function getOptional() {
  return REGISTRY.optional;
}

export function stats() {
  return {
    total: REGISTRY.total,
    essential: REGISTRY.essential.length,
    extended: REGISTRY.extended.length,
    optional: REGISTRY.optional.length,
    versions: Object.values(PACKAGES).map(p => p.version).join(', ')
  };
}
`;

  return esm;
}

// Main
async function main() {
  console.log('📦 Building package registry from package.json files...');

  const data = buildPackageIndex();
  const { packages, byTier } = data;

  console.log(`   Total: ${Object.keys(packages).length}`);
  console.log(`   Essential: ${byTier.essential.length}`);
  console.log(`   Extended: ${byTier.extended.length}`);
  console.log(`   Optional: ${byTier.optional.length}`);

  // Create output directory
  const outputDir = path.join(projectRoot, 'ggen', 'generated');
  fs.mkdirSync(outputDir, { recursive: true });

  // Generate CommonJS
  console.log('\n📝 Generating CommonJS...');
  const cjs = generateCommonJS(data);
  const cjsPath = path.join(outputDir, 'index.js');
  fs.writeFileSync(cjsPath, cjs);
  console.log(`   ✓ ${cjsPath}`);

  // Generate ESM
  console.log('📝 Generating ESM...');
  const esm = generateESM(data);
  const esmPath = path.join(outputDir, 'index.mjs');
  fs.writeFileSync(esmPath, esm);
  console.log(`   ✓ ${esmPath}`);

  // Generate package.json
  console.log('📝 Generating package.json...');
  const pkgJson = {
    name: '@unrdf/generated',
    version: '6.0.0-rc.1',
    description: 'Auto-generated package registry and utilities',
    type: 'module',
    main: 'index.js',
    module: 'index.mjs',
    exports: {
      '.': './index.mjs',
      './cjs': './index.js',
      './registry': './registry.mjs'
    }
  };
  const pkgPath = path.join(outputDir, 'package.json');
  fs.writeFileSync(pkgPath, JSON.stringify(pkgJson, null, 2));
  console.log(`   ✓ ${pkgPath}`);

  console.log(`\n✅ Generated package registry for ${Object.keys(packages).length} packages`);
  console.log(`\nUsage:`);
  console.log(`  import { REGISTRY, getPackage } from './ggen/generated/index.mjs';`);
  console.log(`  REGISTRY.packages.forEach(p => console.log(p.name));`);
}

main().catch(console.error);
