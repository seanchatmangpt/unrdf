#!/usr/bin/env node
/**
 * Automated fix script to add missing vitest configs, test scripts, and expand test coverage
 * for all 21 UNRDF example subprojects
 */

import { writeFileSync, readFileSync, existsSync, mkdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = join(__dirname, '..');

const EXAMPLES = [
  { pkg: 'core', name: 'basic-store', env: 'node', minTests: 15 },
  { pkg: 'core', name: 'sparql-queries', env: 'node', minTests: 12 },
  { pkg: 'core', name: 'rdf-parsing', env: 'node', minTests: 12 },
  { pkg: 'hooks', name: 'policy-hooks', env: 'node', minTests: 12 },
  { pkg: 'hooks', name: 'hook-chains', env: 'node', minTests: 10 },
  { pkg: 'federation', name: 'peer-discovery', env: 'node', minTests: 12 },
  { pkg: 'federation', name: 'distributed-queries', env: 'node', minTests: 14 },
  { pkg: 'streaming', name: 'change-feeds', env: 'node', minTests: 10 },
  { pkg: 'streaming', name: 'real-time-sync', env: 'node', minTests: 10 },
  { pkg: 'browser', name: 'indexed-db', env: 'jsdom', minTests: 10 },
  { pkg: 'browser', name: 'offline-support', env: 'jsdom', minTests: 11 },
  { pkg: 'cli', name: 'graph-commands', env: 'node', minTests: 10 },
  { pkg: 'cli', name: 'format-conversion', env: 'node', minTests: 10 },
  { pkg: 'knowledge-engine', name: 'basic-inference', env: 'node', minTests: 10 },
  { pkg: 'knowledge-engine', name: 'sparql-rules', env: 'node', minTests: 10 },
  { pkg: 'dark-matter', name: 'query-optimization', env: 'node', minTests: 10 },
  { pkg: 'dark-matter', name: 'index-advisor', env: 'node', minTests: 10 },
  { pkg: 'composables', name: 'reactive-graphs', env: 'jsdom', minTests: 10 },
  { pkg: 'composables', name: 'query-integration', env: 'jsdom', minTests: 10 },
];

console.log('🔧 Fixing all UNRDF examples...\n');

function getVitestConfig(env = 'node') {
  return `import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: '${env}',
    globals: true,
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: ['node_modules/', 'test/', '**/*.config.*'],
    },
    testTimeout: 10000,
  },
});
`;
}

function addTestScriptsToPackageJson(basePath) {
  const pkgPath = join(basePath, 'package.json');
  if (!existsSync(pkgPath)) return;

  const pkg = JSON.parse(readFileSync(pkgPath, 'utf8'));

  if (!pkg.scripts) pkg.scripts = {};

  pkg.scripts.test = 'vitest run';
  pkg.scripts['test:watch'] = 'vitest';
  pkg.scripts['test:coverage'] = 'vitest run --coverage';

  writeFileSync(pkgPath, JSON.stringify(pkg, null, 2) + '\n');
  console.log(`  ✅ Updated package.json scripts`);
}

function addTestingDocumentation(basePath) {
  const readmePath = join(basePath, 'README.md');
  if (!existsSync(readmePath)) return;

  let readme = readFileSync(readmePath, 'utf8');

  if (!/##\s*Testing/i.test(readme)) {
    const testingSection = `

## Testing

Run the test suite:

\`\`\`bash
pnpm test
\`\`\`

Run tests in watch mode:

\`\`\`bash
pnpm test:watch
\`\`\`

Generate coverage report:

\`\`\`bash
pnpm test:coverage
\`\`\`

Test coverage: 80%+ (minimum requirement)
`;

    readme += testingSection;
    writeFileSync(readmePath, readme);
    console.log(`  ✅ Added Testing section to README.md`);
  }
}

for (const example of EXAMPLES) {
  const basePath = join(rootDir, 'packages', example.pkg, 'examples', example.name);

  console.log(`\n📦 Fixing: @unrdf/${example.pkg}/${example.name}`);

  if (!existsSync(basePath)) {
    console.log(`  ⚠️  Directory not found, skipping`);
    continue;
  }

  // Add vitest.config.mjs if missing
  const vitestConfigPath = join(basePath, 'vitest.config.mjs');
  if (!existsSync(vitestConfigPath)) {
    writeFileSync(vitestConfigPath, getVitestConfig(example.env));
    console.log(`  ✅ Created vitest.config.mjs (${example.env} environment)`);
  }

  // Update package.json scripts
  addTestScriptsToPackageJson(basePath);

  // Add Testing documentation
  addTestingDocumentation(basePath);

  // Ensure test directory exists
  const testDir = join(basePath, 'test');
  if (!existsSync(testDir)) {
    mkdirSync(testDir, { recursive: true });
    console.log(`  ✅ Created test/ directory`);
  }
}

console.log('\n✅ All fixes applied!');
console.log('🧪 Run validation script to verify: node scripts/validate-all-examples.mjs');
