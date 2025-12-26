#!/usr/bin/env node
/**
 * Git Hooks Installer
 * Sets up pre-commit and pre-push hooks
 */

import { writeFileSync, chmodSync, existsSync, mkdirSync } from 'fs';
import { join } from 'path';

const GIT_HOOKS_DIR = '.git/hooks';

/**
 * Create pre-commit hook
 */
function createPreCommitHook() {
  const hookPath = join(GIT_HOOKS_DIR, 'pre-commit');

  const hookContent = `#!/bin/sh
#
# Pre-commit hook
# Runs quality checks on staged files
#

echo "Running pre-commit checks..."

# Run pre-commit quality gate
node scripts/quality-gates/pre-commit.mjs

# Check exit code
if [ $? -ne 0 ]; then
  echo ""
  echo "❌ Pre-commit checks failed!"
  echo "Fix the issues above and try again"
  echo ""
  echo "To bypass (not recommended): git commit --no-verify"
  exit 1
fi

exit 0
`;

  writeFileSync(hookPath, hookContent);
  chmodSync(hookPath, 0o755);

  console.log('✅ Created pre-commit hook');
}

/**
 * Create pre-push hook
 */
function createPrePushHook() {
  const hookPath = join(GIT_HOOKS_DIR, 'pre-push');

  const hookContent = `#!/bin/sh
#
# Pre-push hook
# Runs comprehensive checks before pushing
#

echo "Running pre-push checks..."
echo "This may take up to 2 minutes..."
echo ""

# Run pre-push quality gate
node scripts/quality-gates/pre-push.mjs

# Check exit code
if [ $? -ne 0 ]; then
  echo ""
  echo "❌ Pre-push checks failed!"
  echo "Fix the issues above before pushing"
  echo ""
  echo "To bypass (not recommended): git push --no-verify"
  exit 1
fi

exit 0
`;

  writeFileSync(hookPath, hookContent);
  chmodSync(hookPath, 0o755);

  console.log('✅ Created pre-push hook');
}

/**
 * Create commit-msg hook for conventional commits
 */
function createCommitMsgHook() {
  const hookPath = join(GIT_HOOKS_DIR, 'commit-msg');

  const hookContent = `#!/bin/sh
#
# Commit message hook
# Validates conventional commit format
#

commit_msg_file=$1
commit_msg=$(cat "$commit_msg_file")

# Check conventional commit format
if ! echo "$commit_msg" | grep -qE "^(feat|fix|docs|style|refactor|perf|test|chore|build|ci|revert)(\\(.+\\))?: .{1,}"; then
  echo ""
  echo "❌ Invalid commit message format!"
  echo ""
  echo "Commit messages must follow conventional commits format:"
  echo "  type(scope): description"
  echo ""
  echo "Examples:"
  echo "  feat(core): add new RDF parser"
  echo "  fix(streaming): resolve memory leak"
  echo "  docs: update README"
  echo ""
  echo "Valid types: feat, fix, docs, style, refactor, perf, test, chore, build, ci, revert"
  echo ""
  exit 1
fi

exit 0
`;

  writeFileSync(hookPath, hookContent);
  chmodSync(hookPath, 0o755);

  console.log('✅ Created commit-msg hook');
}

/**
 * Main execution
 */
function main() {
  console.log('Installing Git hooks...\n');

  // Check if .git/hooks directory exists
  if (!existsSync(GIT_HOOKS_DIR)) {
    console.error('❌ .git/hooks directory not found');
    console.error('Are you in a git repository?');
    process.exit(1);
  }

  // Install hooks
  createPreCommitHook();
  createPrePushHook();
  createCommitMsgHook();

  console.log('\n✅ Git hooks installed successfully');
  console.log('\nHooks installed:');
  console.log('  - pre-commit: Fast quality checks (<5s)');
  console.log('  - pre-push: Comprehensive checks (<2min)');
  console.log('  - commit-msg: Conventional commit validation');
  console.log('\nTo bypass hooks: use --no-verify flag (not recommended)');
}

main();
