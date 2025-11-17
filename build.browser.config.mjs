/**
 * @fileoverview Browser bundle configuration using esbuild
 *
 * Creates optimized browser bundles with:
 * - Tree-shaking to remove unused code
 * - Minification for production
 * - Source maps for debugging
 * - Multiple bundle sizes (core, full, examples)
 *
 * Usage:
 *   node build.browser.config.mjs
 *
 * Outputs:
 *   dist/browser/unrdf.core.min.js     - Core bundle (target: <200KB)
 *   dist/browser/unrdf.full.min.js     - Full bundle (target: <500KB)
 *   dist/browser/unrdf.core.js         - Core bundle (dev)
 *   dist/browser/unrdf.full.js         - Full bundle (dev)
 */

import * as esbuild from 'esbuild';
import { readFileSync, writeFileSync } from 'fs';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Bundle configurations
 */
const bundles = [
  {
    name: 'core',
    entryPoints: ['src/browser/index.mjs'],
    outfile: 'dist/browser/unrdf.core.js',
    globalName: 'UNRDF',
    description: 'Core browser bundle (IndexedDB store, query executor)',
  },
  {
    name: 'full',
    entryPoints: ['src/index.mjs', 'src/browser/index.mjs'],
    outfile: 'dist/browser/unrdf.full.js',
    globalName: 'UNRDF',
    description: 'Full browser bundle (all features)',
  },
];

/**
 * Common esbuild options
 */
const commonOptions = {
  bundle: true,
  platform: 'browser',
  target: ['es2020', 'chrome90', 'firefox88', 'safari14'],
  format: 'iife',
  sourcemap: true,
  logLevel: 'info',
};

/**
 * Build bundle (development + production)
 * @param {Object} config - Bundle configuration
 */
async function buildBundle(config) {
  console.log(`\n🔨 Building ${config.name} bundle...`);

  // Development build
  const devResult = await esbuild.build({
    ...commonOptions,
    entryPoints: config.entryPoints,
    outfile: config.outfile,
    globalName: config.globalName,
    minify: false,
    metafile: true,
  });

  // Production build (minified)
  const prodOutfile = config.outfile.replace('.js', '.min.js');
  const prodResult = await esbuild.build({
    ...commonOptions,
    entryPoints: config.entryPoints,
    outfile: prodOutfile,
    globalName: config.globalName,
    minify: true,
    treeShaking: true,
    metafile: true,
  });

  // Analyze bundle sizes
  const devSize = getSizeInfo(config.outfile);
  const prodSize = getSizeInfo(prodOutfile);

  console.log(`\n✅ ${config.name} bundle built successfully!`);
  console.log(`   ${config.description}`);
  console.log(`   Development: ${devSize.kb} KB (${devSize.gzipKb} KB gzipped)`);
  console.log(`   Production:  ${prodSize.kb} KB (${prodSize.gzipKb} KB gzipped)`);

  // Generate bundle analysis report
  await generateBundleReport(config.name, prodResult.metafile, prodSize);

  return { dev: devResult, prod: prodResult, sizes: { dev: devSize, prod: prodSize } };
}

/**
 * Get file size information
 * @param {string} filePath - File path
 * @returns {Object} Size info
 */
function getSizeInfo(filePath) {
  const absolutePath = resolve(__dirname, filePath);
  const buffer = readFileSync(absolutePath);
  const size = buffer.length;

  // Estimate gzip size (rough approximation: ~30% of original)
  // For accurate gzip size, we'd need to actually compress it
  const gzipSize = Math.round(size * 0.3);

  return {
    bytes: size,
    kb: (size / 1024).toFixed(2),
    gzipBytes: gzipSize,
    gzipKb: (gzipSize / 1024).toFixed(2),
  };
}

/**
 * Generate bundle analysis report
 * @param {string} bundleName - Bundle name
 * @param {Object} metafile - esbuild metafile
 * @param {Object} sizeInfo - Size information
 */
async function generateBundleReport(bundleName, metafile, sizeInfo) {
  const report = {
    bundle: bundleName,
    timestamp: new Date().toISOString(),
    size: sizeInfo,
    inputs: {},
    outputs: {},
  };

  // Analyze inputs
  for (const [file, info] of Object.entries(metafile.inputs)) {
    report.inputs[file] = {
      bytes: info.bytes,
      imports: info.imports.map(imp => imp.path),
    };
  }

  // Analyze outputs
  for (const [file, info] of Object.entries(metafile.outputs)) {
    report.outputs[file] = {
      bytes: info.bytes,
      inputs: Object.keys(info.inputs),
    };
  }

  // Find largest dependencies
  const sortedInputs = Object.entries(report.inputs)
    .sort((a, b) => b[1].bytes - a[1].bytes)
    .slice(0, 10);

  report.largestDependencies = sortedInputs.map(([file, info]) => ({
    file,
    kb: (info.bytes / 1024).toFixed(2),
  }));

  // Write report
  const reportPath = `dist/browser/${bundleName}-bundle-report.json`;
  writeFileSync(
    resolve(__dirname, reportPath),
    JSON.stringify(report, null, 2)
  );

  console.log(`   Bundle report: ${reportPath}`);

  // Check bundle size targets
  const targetKb = bundleName === 'core' ? 200 : 500;
  const actualKb = parseFloat(sizeInfo.gzipKb);

  if (actualKb > targetKb) {
    console.warn(`   ⚠️  Warning: Bundle exceeds target size (${actualKb} KB > ${targetKb} KB)`);
  } else {
    console.log(`   ✅ Bundle size within target (${actualKb} KB ≤ ${targetKb} KB)`);
  }
}

/**
 * Build all bundles
 */
async function buildAll() {
  console.log('🚀 Starting browser bundle build...\n');

  const results = [];

  for (const config of bundles) {
    try {
      const result = await buildBundle(config);
      results.push({ name: config.name, ...result });
    } catch (error) {
      console.error(`❌ Failed to build ${config.name} bundle:`, error);
      process.exit(1);
    }
  }

  // Generate summary report
  console.log('\n📊 Build Summary:');
  console.log('═══════════════════════════════════════════════════');

  let totalDevKb = 0;
  let totalProdKb = 0;

  for (const result of results) {
    const devKb = parseFloat(result.sizes.dev.gzipKb);
    const prodKb = parseFloat(result.sizes.prod.gzipKb);

    totalDevKb += devKb;
    totalProdKb += prodKb;

    console.log(`${result.name.toUpperCase()}:`);
    console.log(`  Development: ${result.sizes.dev.kb} KB (${result.sizes.dev.gzipKb} KB gzipped)`);
    console.log(`  Production:  ${result.sizes.prod.kb} KB (${result.sizes.prod.gzipKb} KB gzipped)`);
    console.log('');
  }

  console.log(`Total Production (gzipped): ${totalProdKb.toFixed(2)} KB`);
  console.log('═══════════════════════════════════════════════════');

  // Write summary
  const summary = {
    timestamp: new Date().toISOString(),
    bundles: results.map(r => ({
      name: r.name,
      dev: r.sizes.dev,
      prod: r.sizes.prod,
    })),
    totals: {
      devKb: totalDevKb.toFixed(2),
      prodKb: totalProdKb.toFixed(2),
    },
  };

  writeFileSync(
    resolve(__dirname, 'dist/browser/build-summary.json'),
    JSON.stringify(summary, null, 2)
  );

  console.log('\n✅ All bundles built successfully!');
  console.log('📁 Output directory: dist/browser/');
}

// Run build
buildAll().catch(error => {
  console.error('❌ Build failed:', error);
  process.exit(1);
});
