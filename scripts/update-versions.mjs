#!/usr/bin/env node
import { readFileSync, writeFileSync } from 'fs';
import { glob } from 'glob';

const NEW_VERSION = '5.0.1';

// Private packages that should NOT be published
const PRIVATE_PACKAGES = new Set([
  'docs',
  '@unrdf/domain',
  '@unrdf/nextra-docs',
  '@unrdf/test-utils',
  '@unrdf/validation'
]);

async function updatePackages() {
  const packageFiles = await glob('packages/*/package.json');

  console.log(`Found ${packageFiles.length} packages to update\n`);

  for (const file of packageFiles) {
    try {
      const content = readFileSync(file, 'utf8');
      const pkg = JSON.parse(content);

      const isPrivate = PRIVATE_PACKAGES.has(pkg.name) || pkg.private === true;
      const oldVersion = pkg.version || 'none';

      // Update version
      pkg.version = NEW_VERSION;

      // Add publishConfig for public packages
      if (!isPrivate && !pkg.publishConfig) {
        pkg.publishConfig = { access: 'public' };
      }

      // Ensure private flag is set correctly
      if (isPrivate) {
        pkg.private = true;
        // Remove publishConfig from private packages
        delete pkg.publishConfig;
      } else {
        // Ensure publishConfig is correct for public packages
        if (!pkg.publishConfig || pkg.publishConfig.access !== 'public') {
          pkg.publishConfig = { access: 'public' };
        }
      }

      // Write back with pretty formatting
      writeFileSync(file, JSON.stringify(pkg, null, 2) + '\n', 'utf8');

      const status = isPrivate ? 'PRIVATE' : 'PUBLIC';
      console.log(`✓ ${pkg.name}: ${oldVersion} → ${NEW_VERSION} [${status}]`);
    } catch (error) {
      console.error(`✗ Failed to update ${file}:`, error.message);
    }
  }

  console.log(`\n✓ All packages updated to v${NEW_VERSION}`);
}

updatePackages().catch(console.error);
