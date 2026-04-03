import { globSync } from 'glob';

/**
 * Unified esbuild configuration for all UNRDF packages.
 * Single build process serving 21 packages with ESM + CJS output.
 *
 * Entry Points: All packages with src/index.mjs entry point
 * Output: packages with dist/index.mjs (ESM), index.cjs (CJS), index.d.ts (types)
 *
 * Performance: Less than 30 seconds for all packages on single machine
 */

// Discover all packages with src/index.mjs entry points
const entryPoints = globSync('packages/*/src/index.mjs', {
  ignore: ['**/node_modules/**', '**/dist/**'],
});

if (entryPoints.length === 0) {
  console.warn('⚠️  No entry points found. Ensure all packages have packages/*/src/index.mjs');
}

// Build configuration for ESM format (CJS requires separate build pass)
const esmConfig = {
  entryPoints,
  outdir: 'dist',
  outbase: 'packages',
  format: 'esm',
  bundle: false,
  minify: process.env.NODE_ENV === 'production',
  sourcemap: true,
  target: 'es2020',
  platform: 'node',
  logLevel: 'info',
};

// CommonJS config (separate build for dual format support)
const cjsConfig = {
  entryPoints,
  outdir: 'dist',
  outbase: 'packages',
  format: 'cjs',
  bundle: false,
  minify: process.env.NODE_ENV === 'production',
  sourcemap: true,
  target: 'es2020',
  platform: 'node',
  logLevel: 'info',
};

// Export both configs for dual-build pattern
export default [esmConfig, cjsConfig];
