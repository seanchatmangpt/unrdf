#!/usr/bin/env node
/**
 * Changelog Generator
 * Generates CHANGELOG.md from conventional commits
 */

import { execSync } from 'child_process';
import { writeFileSync, readFileSync, existsSync } from 'fs';

/**
 * Get commits since last tag
 * @returns {Array} Array of commits
 */
function getCommitsSinceLastTag() {
  try {
    // Get last tag
    const lastTag = execSync('git describe --tags --abbrev=0 2>/dev/null || echo ""', {
      encoding: 'utf8'
    }).trim();

    // Get commits
    const range = lastTag ? `${lastTag}..HEAD` : 'HEAD';
    const output = execSync(`git log ${range} --pretty=format:"%H|%s|%an|%ae|%ad" --date=short`, {
      encoding: 'utf8'
    });

    if (!output.trim()) {
      return [];
    }

    return output
      .trim()
      .split('\n')
      .map(line => {
        const [hash, subject, author, email, date] = line.split('|');
        return { hash, subject, author, email, date };
      });
  } catch (error) {
    console.error('Failed to get commits:', error.message);
    return [];
  }
}

/**
 * Parse conventional commit
 * @param {string} subject - Commit subject
 * @returns {Object} Parsed commit
 */
function parseConventionalCommit(subject) {
  // Match: type(scope): description
  const match = subject.match(/^(\w+)(?:\(([^)]+)\))?: (.+)$/);

  if (match) {
    return {
      type: match[1],
      scope: match[2] || null,
      description: match[3],
      breaking: subject.includes('BREAKING CHANGE')
    };
  }

  return {
    type: 'other',
    scope: null,
    description: subject,
    breaking: false
  };
}

/**
 * Categorize commits
 * @param {Array} commits - Array of commits
 * @returns {Object} Categorized commits
 */
function categorizeCommits(commits) {
  const categories = {
    breaking: [],
    feat: [],
    fix: [],
    perf: [],
    refactor: [],
    docs: [],
    test: [],
    chore: [],
    other: []
  };

  for (const commit of commits) {
    const parsed = parseConventionalCommit(commit.subject);

    if (parsed.breaking) {
      categories.breaking.push({ ...commit, parsed });
    } else if (categories[parsed.type]) {
      categories[parsed.type].push({ ...commit, parsed });
    } else {
      categories.other.push({ ...commit, parsed });
    }
  }

  return categories;
}

/**
 * Generate changelog markdown
 * @param {Object} categories - Categorized commits
 * @param {string} version - Version number
 * @returns {string} Changelog markdown
 */
function generateChangelogMarkdown(categories, version) {
  const date = new Date().toISOString().split('T')[0];
  let md = `## [${version}] - ${date}\n\n`;

  // Breaking Changes
  if (categories.breaking.length > 0) {
    md += '### 💥 BREAKING CHANGES\n\n';
    categories.breaking.forEach(c => {
      md += `- ${c.parsed.description} (${c.hash.substring(0, 7)})\n`;
    });
    md += '\n';
  }

  // Features
  if (categories.feat.length > 0) {
    md += '### ✨ Features\n\n';
    categories.feat.forEach(c => {
      const scope = c.parsed.scope ? `**${c.parsed.scope}**: ` : '';
      md += `- ${scope}${c.parsed.description} (${c.hash.substring(0, 7)})\n`;
    });
    md += '\n';
  }

  // Bug Fixes
  if (categories.fix.length > 0) {
    md += '### 🐛 Bug Fixes\n\n';
    categories.fix.forEach(c => {
      const scope = c.parsed.scope ? `**${c.parsed.scope}**: ` : '';
      md += `- ${scope}${c.parsed.description} (${c.hash.substring(0, 7)})\n`;
    });
    md += '\n';
  }

  // Performance
  if (categories.perf.length > 0) {
    md += '### ⚡ Performance\n\n';
    categories.perf.forEach(c => {
      const scope = c.parsed.scope ? `**${c.parsed.scope}**: ` : '';
      md += `- ${scope}${c.parsed.description} (${c.hash.substring(0, 7)})\n`;
    });
    md += '\n';
  }

  // Refactoring
  if (categories.refactor.length > 0) {
    md += '### ♻️ Refactoring\n\n';
    categories.refactor.forEach(c => {
      const scope = c.parsed.scope ? `**${c.parsed.scope}**: ` : '';
      md += `- ${scope}${c.parsed.description} (${c.hash.substring(0, 7)})\n`;
    });
    md += '\n';
  }

  // Documentation
  if (categories.docs.length > 0) {
    md += '### 📚 Documentation\n\n';
    categories.docs.forEach(c => {
      const scope = c.parsed.scope ? `**${c.parsed.scope}**: ` : '';
      md += `- ${scope}${c.parsed.description} (${c.hash.substring(0, 7)})\n`;
    });
    md += '\n';
  }

  // Tests
  if (categories.test.length > 0) {
    md += '### 🧪 Tests\n\n';
    categories.test.forEach(c => {
      const scope = c.parsed.scope ? `**${c.parsed.scope}**: ` : '';
      md += `- ${scope}${c.parsed.description} (${c.hash.substring(0, 7)})\n`;
    });
    md += '\n';
  }

  return md;
}

/**
 * Update CHANGELOG.md
 * @param {string} newContent - New changelog content
 */
function updateChangelog(newContent) {
  const changelogPath = './CHANGELOG.md';
  let existingContent = '';

  if (existsSync(changelogPath)) {
    existingContent = readFileSync(changelogPath, 'utf8');
    // Remove header if it exists
    existingContent = existingContent.replace(/^# Changelog\n\n/, '');
  }

  const fullChangelog = `# Changelog\n\nAll notable changes to this project will be documented in this file.\n\nThe format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),\nand this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).\n\n${newContent}${existingContent}`;

  writeFileSync(changelogPath, fullChangelog);
}

/**
 * Get next version
 * @param {Object} categories - Categorized commits
 * @returns {string} Next version number
 */
function getNextVersion(categories) {
  try {
    // Get current version from package.json
    const pkg = JSON.parse(readFileSync('./package.json', 'utf8'));
    const [major, minor, patch] = pkg.version.split('.').map(Number);

    // Determine version bump
    if (categories.breaking.length > 0) {
      return `${major + 1}.0.0`;
    } else if (categories.feat.length > 0) {
      return `${major}.${minor + 1}.0`;
    } else {
      return `${major}.${minor}.${patch + 1}`;
    }
  } catch (error) {
    console.error('Failed to determine version:', error.message);
    return '0.0.1';
  }
}

/**
 * Main execution
 */
function main() {
  console.log('Generating changelog...\n');

  const commits = getCommitsSinceLastTag();
  console.log(`Found ${commits.length} commits since last tag`);

  if (commits.length === 0) {
    console.log('No commits to process');
    return;
  }

  const categories = categorizeCommits(commits);

  // Count commits by category
  console.log('\nCommit breakdown:');
  for (const [type, commits] of Object.entries(categories)) {
    if (commits.length > 0) {
      console.log(`  ${type}: ${commits.length}`);
    }
  }

  const version = getNextVersion(categories);
  console.log(`\nNext version: ${version}`);

  const changelog = generateChangelogMarkdown(categories, version);
  updateChangelog(changelog);

  console.log('\n✅ Changelog generated');
  console.log('   File: CHANGELOG.md');
  console.log(`   Version: ${version}`);
}

main();
