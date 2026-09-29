#!/usr/bin/env node
import { readFileSync, writeFileSync } from 'fs';
import { glob } from 'glob';

// Target version: first CLI argument, else the workspace root package.json (never hardcoded)
const NEW_VERSION =
  process.argv[2] ||
  JSON.parse(readFileSync(new URL('../package.json', import.meta.url), 'utf8')).version;

async function makeAllPublic() {
  const packageFiles = await glob('packages/*/package.json');

  console.log(`Making ${packageFiles.length} packages PUBLIC\n`);

  for (const file of packageFiles) {
    try {
      const content = readFileSync(file, 'utf8');
      const pkg = JSON.parse(content);

      const oldVersion = pkg.version || 'none';
      const wasPrivate = pkg.private === true;

      // Update version
      pkg.version = NEW_VERSION;

      // Remove private flag - make ALL packages public
      delete pkg.private;

      // Add publishConfig for ALL packages
      pkg.publishConfig = { access: 'public' };

      // Write back with pretty formatting
      writeFileSync(file, JSON.stringify(pkg, null, 2) + '\n', 'utf8');

      const status = wasPrivate ? 'PRIVATE→PUBLIC' : 'PUBLIC';
      console.log(`✓ ${pkg.name}: ${oldVersion} → ${NEW_VERSION} [${status}]`);
    } catch (error) {
      console.error(`✗ Failed to update ${file}:`, error.message);
    }
  }

  console.log(`\n✓ All ${packageFiles.length} packages are now PUBLIC and set to v${NEW_VERSION}`);
}

makeAllPublic().catch(error => {
  console.error(error);
  process.exit(1);
});
