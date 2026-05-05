#!/usr/bin/env node
/**
 * @file setup-quality.mjs
 * @description One-time setup script for code quality automation
 *
 * Configures:
 * - Husky git hooks
 * - Lint-staged for incremental checks
 * - VSCode settings (if not present)
 * - Git configuration
 *
 * Usage:
 *   node scripts/setup-quality.mjs
 */

import { execSync } from 'child_process';
import { existsSync, mkdirSync, chmodSync, readFileSync, writeFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = join(__dirname, '..');

/**
 * Executes a command and logs output
 * @param {string} cmd - Command to execute
 * @param {string} description - What the command does
 */
function exec(cmd, description) {
  console.log(`\n⏳ ${description}...`);
  try {
    const output = execSync(cmd, { cwd: rootDir, encoding: 'utf8', stdio: 'pipe' });
    console.log(`✅ ${description} complete`);
    if (output.trim()) {
      console.log(`   ${output.trim().split('\n').join('\n   ')}`);
    }
  } catch (error) {
    console.error(`❌ ${description} failed:`);
    console.error(`   ${error.message}`);
    throw error;
  }
}

/**
 * Main setup function
 */
async function setup() {
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('  🛠️  UNRDF Code Quality Setup');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  // Step 1: Verify dependencies
  console.log('📦 Checking dependencies...');
  const packageJson = JSON.parse(readFileSync(join(rootDir, 'package.json'), 'utf8'));
  const hasHusky = packageJson.devDependencies?.husky;
  const hasLintStaged = packageJson.devDependencies?.['lint-staged'];

  if (!hasHusky || !hasLintStaged) {
    console.log('\n⚠️  Missing dependencies. Installing...');
    exec('pnpm add -D -w husky lint-staged', 'Install husky and lint-staged');
  } else {
    console.log('✅ Dependencies already installed');
  }

  // Step 2: Initialize Husky
  console.log('\n🪝 Setting up Git hooks...');
  exec('pnpm exec husky install', 'Initialize Husky');

  // Step 3: Create pre-commit hook
  const huskyDir = join(rootDir, '.husky');
  const preCommitPath = join(huskyDir, 'pre-commit');

  if (!existsSync(huskyDir)) {
    mkdirSync(huskyDir, { recursive: true });
  }

  if (!existsSync(preCommitPath)) {
    const preCommitContent = `#!/usr/bin/env sh
. "$(dirname -- "$0")/_/husky.sh"

# UNRDF Pre-commit Quality Gate
echo "🔍 Running pre-commit quality checks..."
pnpm lint-staged
`;
    writeFileSync(preCommitPath, preCommitContent, 'utf8');
    chmodSync(preCommitPath, 0o755);
    console.log('✅ Pre-commit hook created');
  } else {
    console.log('✅ Pre-commit hook already exists');
  }

  // Step 4: Update package.json with lint-staged config
  if (!packageJson['lint-staged']) {
    console.log('\n⚙️  Configuring lint-staged...');
    packageJson['lint-staged'] = {
      '*.{mjs,js}': [
        'eslint --fix --max-warnings=0',
        'prettier --write'
      ],
      '*.{json,md,yml,yaml}': [
        'prettier --write'
      ]
    };

    // Add prepare script for automatic setup
    if (!packageJson.scripts.prepare) {
      packageJson.scripts.prepare = 'husky install';
    }

    writeFileSync(
      join(rootDir, 'package.json'),
      JSON.stringify(packageJson, null, 2) + '\n',
      'utf8'
    );
    console.log('✅ package.json updated with lint-staged configuration');
  } else {
    console.log('✅ lint-staged configuration already present');
  }

  // Step 5: Configure Git (recommended settings)
  console.log('\n🔧 Configuring Git...');
  try {
    exec('git config core.hooksPath .husky', 'Set Git hooks path');
    console.log('✅ Git configured for Husky hooks');
  } catch {
    console.log('⚠️  Could not configure Git (may not be a git repo)');
  }

  // Step 6: Verify VSCode settings exist
  console.log('\n💻 Checking VSCode settings...');
  const vscodeSettingsPath = join(rootDir, '.vscode', 'settings.json');
  if (existsSync(vscodeSettingsPath)) {
    console.log('✅ VSCode settings already configured');
  } else {
    console.log('⚠️  VSCode settings not found');
    console.log('   Recommended: Copy .vscode/settings.json from repo');
  }

  // Step 7: Test the setup
  console.log('\n🧪 Testing setup...');
  try {
    exec('pnpm lint --help > /dev/null 2>&1', 'Verify lint command');
    exec('pnpm test:fast --help > /dev/null 2>&1', 'Verify test command');
    console.log('✅ Quality commands available');
  } catch {
    console.log('⚠️  Some quality commands may not be available');
  }

  // Summary
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('  ✅ Setup Complete!');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  console.log('📋 Next steps:');
  console.log('   1. Install VSCode extensions (see .vscode/extensions.json)');
  console.log('   2. Run quality report: node scripts/quality-report.mjs');
  console.log('   3. Make a test commit to verify pre-commit hook');
  console.log('\n🎯 Quality automation is now active!\n');

  console.log('💡 Pro tips:');
  console.log('   - Pre-commit hooks run on every commit');
  console.log('   - Use "git commit --no-verify" to bypass (emergencies only)');
  console.log('   - Run "pnpm lint:fix" to auto-fix linting issues');
  console.log('   - Check CODE-QUALITY.md for full documentation\n');
}

// Run setup
setup().catch(error => {
  console.error('\n❌ Setup failed:', error.message);
  process.exit(1);
});
