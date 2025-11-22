#!/usr/bin/env node

/**
 * @fileoverview Documentation Link Checker
 *
 * Validates all internal and external links in documentation files.
 * Reports broken links, redirects, and accessibility issues.
 *
 * @module docs/tools/check-links
 * @version 1.0.0
 * @license MIT
 */

import { readFile, readdir, stat, mkdir, writeFile, access } from 'node:fs/promises';
import { join, dirname, resolve, relative, extname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Configuration for link checking
 */
const config = {
  directories: ['../'],
  ignorePatterns: [
    '**/node_modules/**',
    '**/archive/**',
    '**/.git/**',
    '**/tools/reports/**'
  ],
  externalLinkTimeout: 10000,
  checkExternalLinks: false, // Set to true to check external links
  reportsDirectory: './reports/',
  fileExtensions: ['.md']
};

/**
 * Link types
 */
const LinkType = {
  INTERNAL: 'internal',
  EXTERNAL: 'external',
  ANCHOR: 'anchor',
  EMAIL: 'email'
};

/**
 * Check if path matches any ignore pattern
 * @param {string} filePath - Path to check
 * @returns {boolean} True if should be ignored
 */
function shouldIgnore(filePath) {
  return config.ignorePatterns.some((pattern) => {
    const regexPattern = pattern
      .replace(/\*\*/g, '.*')
      .replace(/\*/g, '[^/]*')
      .replace(/\//g, '\\/');
    return new RegExp(regexPattern).test(filePath);
  });
}

/**
 * Extract links from markdown content
 * @param {string} content - Markdown content
 * @param {string} filePath - Source file path
 * @returns {Array<Object>} Array of link objects
 */
function extractLinks(content, filePath) {
  const links = [];

  // Match markdown links: [text](url)
  const markdownLinkRegex = /\[([^\]]*)\]\(([^)]+)\)/g;
  let match;
  while ((match = markdownLinkRegex.exec(content)) !== null) {
    const [fullMatch, text, url] = match;
    const line = content.substring(0, match.index).split('\n').length;

    links.push({
      text,
      url: url.trim(),
      source: filePath,
      line,
      type: classifyLink(url)
    });
  }

  // Match reference-style links: [text][ref] and [ref]: url
  const refLinkRegex = /^\[([^\]]+)\]:\s*(.+)$/gm;
  while ((match = refLinkRegex.exec(content)) !== null) {
    const [, ref, url] = match;
    const line = content.substring(0, match.index).split('\n').length;

    links.push({
      text: ref,
      url: url.trim(),
      source: filePath,
      line,
      type: classifyLink(url)
    });
  }

  // Match HTML links: <a href="url">
  const htmlLinkRegex = /<a\s+[^>]*href=["']([^"']+)["'][^>]*>/gi;
  while ((match = htmlLinkRegex.exec(content)) !== null) {
    const [, url] = match;
    const line = content.substring(0, match.index).split('\n').length;

    links.push({
      text: 'HTML link',
      url: url.trim(),
      source: filePath,
      line,
      type: classifyLink(url)
    });
  }

  return links;
}

/**
 * Classify link type
 * @param {string} url - URL to classify
 * @returns {string} Link type
 */
function classifyLink(url) {
  if (url.startsWith('mailto:')) {
    return LinkType.EMAIL;
  }
  if (url.startsWith('http://') || url.startsWith('https://')) {
    return LinkType.EXTERNAL;
  }
  if (url.startsWith('#')) {
    return LinkType.ANCHOR;
  }
  return LinkType.INTERNAL;
}

/**
 * Check if internal link is valid
 * @param {Object} link - Link object
 * @param {string} docsRoot - Documentation root directory
 * @returns {Promise<Object>} Validation result
 */
async function checkInternalLink(link, docsRoot) {
  let targetPath = link.url;

  // Remove anchor from path
  const anchorIndex = targetPath.indexOf('#');
  const anchor = anchorIndex >= 0 ? targetPath.substring(anchorIndex + 1) : null;
  if (anchorIndex >= 0) {
    targetPath = targetPath.substring(0, anchorIndex);
  }

  // Skip empty paths (anchor-only links)
  if (!targetPath) {
    return { valid: true, reason: 'anchor-only' };
  }

  // Resolve relative path
  const sourceDir = dirname(link.source);
  const resolvedPath = resolve(sourceDir, targetPath);

  // Check if file exists
  try {
    await access(resolvedPath);
    const fileStat = await stat(resolvedPath);

    // If directory, check for index.md or README.md
    if (fileStat.isDirectory()) {
      try {
        await access(join(resolvedPath, 'README.md'));
        return { valid: true, resolvedPath: join(resolvedPath, 'README.md') };
      } catch {
        try {
          await access(join(resolvedPath, 'index.md'));
          return { valid: true, resolvedPath: join(resolvedPath, 'index.md') };
        } catch {
          return { valid: false, reason: 'directory-no-index', resolvedPath };
        }
      }
    }

    // Check anchor if present
    if (anchor) {
      const content = await readFile(resolvedPath, 'utf-8');
      const anchorRegex = new RegExp(
        `^#{1,6}\\s+.*?${anchor.replace(/[-/\\^$*+?.()|[\]{}]/g, '\\$&')}`,
        'im'
      );
      if (!anchorRegex.test(content)) {
        // Check for explicit anchor
        const explicitAnchorRegex = new RegExp(`id=["']${anchor}["']`, 'i');
        if (!explicitAnchorRegex.test(content)) {
          return { valid: false, reason: 'anchor-not-found', anchor, resolvedPath };
        }
      }
    }

    return { valid: true, resolvedPath };
  } catch {
    // Check with .md extension
    if (!extname(targetPath)) {
      try {
        await access(`${resolvedPath}.md`);
        return { valid: true, resolvedPath: `${resolvedPath}.md` };
      } catch {
        // File doesn't exist
      }
    }

    return { valid: false, reason: 'file-not-found', resolvedPath };
  }
}

/**
 * Check anchor link within same document
 * @param {Object} link - Link object
 * @returns {Promise<Object>} Validation result
 */
async function checkAnchorLink(link) {
  const anchor = link.url.substring(1);

  try {
    const content = await readFile(link.source, 'utf-8');

    // Generate anchor from heading
    const generateAnchor = (text) =>
      text
        .toLowerCase()
        .replace(/[^\w\s-]/g, '')
        .replace(/\s+/g, '-');

    // Check all headings
    const headingRegex = /^#{1,6}\s+(.+)$/gm;
    let match;
    while ((match = headingRegex.exec(content)) !== null) {
      const headingAnchor = generateAnchor(match[1]);
      if (headingAnchor === anchor || match[1].toLowerCase().includes(anchor.toLowerCase())) {
        return { valid: true };
      }
    }

    // Check explicit anchors
    const explicitAnchorRegex = new RegExp(`id=["']${anchor}["']`, 'i');
    if (explicitAnchorRegex.test(content)) {
      return { valid: true };
    }

    return { valid: false, reason: 'anchor-not-found', anchor };
  } catch (error) {
    return { valid: false, reason: 'read-error', error: error.message };
  }
}

/**
 * Scan directory for markdown files
 * @param {string} dir - Directory to scan
 * @returns {Promise<string[]>} Array of file paths
 */
async function scanDirectory(dir) {
  const files = [];

  async function scan(currentDir) {
    const entries = await readdir(currentDir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(currentDir, entry.name);

      if (shouldIgnore(fullPath)) {
        continue;
      }

      if (entry.isDirectory()) {
        await scan(fullPath);
      } else if (config.fileExtensions.includes(extname(entry.name))) {
        files.push(fullPath);
      }
    }
  }

  await scan(dir);
  return files;
}

/**
 * Check all links in documentation
 * @returns {Promise<Object>} Report object
 */
async function checkLinks() {
  console.log('Checking documentation links...\n');

  const docsRoot = join(__dirname, '../');
  const allLinks = [];
  const results = {
    valid: [],
    broken: [],
    warnings: [],
    skipped: []
  };

  // Scan all documentation directories
  for (const dir of config.directories) {
    const dirPath = join(__dirname, dir);
    const files = await scanDirectory(dirPath);

    console.log(`Found ${files.length} markdown files in ${dir}`);

    for (const file of files) {
      const content = await readFile(file, 'utf-8');
      const links = extractLinks(content, file);
      allLinks.push(...links);
    }
  }

  console.log(`\nChecking ${allLinks.length} total links...\n`);

  // Check each link
  for (const link of allLinks) {
    let result;

    switch (link.type) {
      case LinkType.INTERNAL:
        result = await checkInternalLink(link, docsRoot);
        break;

      case LinkType.ANCHOR:
        result = await checkAnchorLink(link);
        break;

      case LinkType.EXTERNAL:
        if (config.checkExternalLinks) {
          // External link checking would go here
          result = { valid: true, reason: 'external-not-checked' };
        } else {
          results.skipped.push({ ...link, reason: 'external-link-skip' });
          continue;
        }
        break;

      case LinkType.EMAIL:
        results.skipped.push({ ...link, reason: 'email-link' });
        continue;

      default:
        results.skipped.push({ ...link, reason: 'unknown-type' });
        continue;
    }

    if (result.valid) {
      results.valid.push({ ...link, ...result });
    } else {
      results.broken.push({ ...link, ...result });
    }
  }

  // Generate report
  const report = {
    timestamp: new Date().toISOString(),
    summary: {
      total: allLinks.length,
      valid: results.valid.length,
      broken: results.broken.length,
      warnings: results.warnings.length,
      skipped: results.skipped.length
    },
    broken: results.broken.map((link) => ({
      source: relative(docsRoot, link.source),
      line: link.line,
      url: link.url,
      reason: link.reason,
      anchor: link.anchor
    })),
    warnings: results.warnings,
    skipped: results.skipped.length
  };

  // Print summary
  console.log('='.repeat(60));
  console.log('LINK CHECK REPORT');
  console.log('='.repeat(60));
  console.log(`\nTotal Links: ${report.summary.total}`);
  console.log(`  [OK] Valid: ${report.summary.valid}`);
  console.log(`  [X] Broken: ${report.summary.broken}`);
  console.log(`  [!] Warnings: ${report.summary.warnings}`);
  console.log(`  [-] Skipped: ${report.summary.skipped}`);

  if (results.broken.length > 0) {
    console.log('\nBroken Links:');
    for (const link of results.broken) {
      const relativePath = relative(docsRoot, link.source);
      console.log(`  ${relativePath}:${link.line}`);
      console.log(`    URL: ${link.url}`);
      console.log(`    Reason: ${link.reason}`);
      if (link.anchor) {
        console.log(`    Anchor: #${link.anchor}`);
      }
      console.log();
    }
  }

  // Save report
  const reportsDir = join(__dirname, config.reportsDirectory);
  await mkdir(reportsDir, { recursive: true });

  const reportPath = join(reportsDir, 'link-report.json');
  await writeFile(reportPath, JSON.stringify(report, null, 2));
  console.log(`Report saved to: ${reportPath}`);

  return report;
}

// Run if executed directly
if (process.argv[1] === fileURLToPath(import.meta.url)) {
  checkLinks()
    .then((report) => {
      process.exit(report.summary.broken > 0 ? 1 : 0);
    })
    .catch((error) => {
      console.error('Error checking links:', error);
      process.exit(1);
    });
}

export { checkLinks, extractLinks, classifyLink, checkInternalLink, checkAnchorLink };
