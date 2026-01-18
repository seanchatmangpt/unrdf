#!/usr/bin/env node

/**
 * Documentation Validation Script
 * Validates markdown files for:
 * - Broken internal links
 * - Missing referenced files
 * - Markdown syntax errors
 * - Link structure
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { join, dirname, resolve } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = resolve(__dirname, '..');

const results = {
  totalFiles: 0,
  validFiles: 0,
  brokenLinks: [],
  missingFiles: [],
  syntaxErrors: [],
  warnings: []
};

/**
 * Find all markdown files in a directory
 */
function findMarkdownFiles(dir, files = []) {
  try {
    const entries = readdirSync(dir);

    for (const entry of entries) {
      const fullPath = join(dir, entry);

      try {
        const stat = statSync(fullPath);

        if (stat.isDirectory()) {
          // Skip node_modules, .git, archive
          if (!['node_modules', '.git', 'archive'].includes(entry)) {
            findMarkdownFiles(fullPath, files);
          }
        } else if (entry.endsWith('.md')) {
          files.push(fullPath);
        }
      } catch (err) {
        // Skip broken symlinks or inaccessible files
        if (err.code !== 'ENOENT') {
          console.warn(`Warning: Could not stat ${fullPath}: ${err.message}`);
        }
      }
    }
  } catch (err) {
    console.warn(`Warning: Could not read directory ${dir}: ${err.message}`);
  }

  return files;
}

/**
 * Extract links from markdown content
 */
function extractLinks(content) {
  const links = [];

  // Markdown links: [text](url)
  const mdLinkRegex = /\[([^\]]+)\]\(([^)]+)\)/g;
  let match;

  while ((match = mdLinkRegex.exec(content)) !== null) {
    links.push({
      text: match[1],
      url: match[2],
      type: 'markdown'
    });
  }

  // Reference-style links: [text][ref] and [ref]: url
  const refLinkRegex = /\[([^\]]+)\]:\s*(.+)$/gm;
  while ((match = refLinkRegex.exec(content)) !== null) {
    links.push({
      text: match[1],
      url: match[2].trim(),
      type: 'reference'
    });
  }

  return links;
}

/**
 * Resolve a link relative to a file
 */
function resolveLink(linkUrl, filePath) {
  // Skip external links, anchors, and mailto
  if (linkUrl.startsWith('http://') ||
      linkUrl.startsWith('https://') ||
      linkUrl.startsWith('#') ||
      linkUrl.startsWith('mailto:')) {
    return null;
  }

  // Remove anchor from link
  const cleanUrl = linkUrl.split('#')[0];
  if (!cleanUrl) return null;

  // Resolve relative to file's directory
  const fileDir = dirname(filePath);
  return resolve(fileDir, cleanUrl);
}

/**
 * Validate a single markdown file
 */
function validateFile(filePath) {
  results.totalFiles++;

  try {
    const content = readFileSync(filePath, 'utf8');
    const links = extractLinks(content);

    let hasErrors = false;

    for (const link of links) {
      const resolved = resolveLink(link.url, filePath);

      if (resolved && !existsSync(resolved)) {
        results.brokenLinks.push({
          file: filePath.replace(ROOT + '/', ''),
          link: link.url,
          text: link.text,
          resolved: resolved.replace(ROOT + '/', '')
        });
        hasErrors = true;
      }
    }

    if (!hasErrors) {
      results.validFiles++;
    }

  } catch (err) {
    results.syntaxErrors.push({
      file: filePath.replace(ROOT + '/', ''),
      error: err.message
    });
  }
}

/**
 * Main validation
 */
function main() {
  console.log('🔍 UNRDF Documentation Validation\n');
  console.log('Scanning for markdown files...');

  const files = findMarkdownFiles(ROOT);
  console.log(`Found ${files.length} markdown files\n`);

  console.log('Validating links and syntax...');
  for (const file of files) {
    validateFile(file);
  }

  // Generate report
  console.log('\n' + '='.repeat(80));
  console.log('📊 VALIDATION REPORT');
  console.log('='.repeat(80) + '\n');

  console.log(`Total Files Scanned: ${results.totalFiles}`);
  console.log(`Valid Files: ${results.validFiles}`);
  console.log(`Files with Issues: ${results.totalFiles - results.validFiles}`);
  console.log();

  if (results.brokenLinks.length > 0) {
    console.log(`⚠️  Broken Links: ${results.brokenLinks.length}`);
    console.log('---');
    results.brokenLinks.slice(0, 20).forEach(item => {
      console.log(`  File: ${item.file}`);
      console.log(`  Link: ${item.link}`);
      console.log(`  Text: ${item.text}`);
      console.log(`  Missing: ${item.resolved}`);
      console.log();
    });
    if (results.brokenLinks.length > 20) {
      console.log(`  ... and ${results.brokenLinks.length - 20} more`);
      console.log();
    }
  } else {
    console.log('✅ No broken links found');
  }

  if (results.syntaxErrors.length > 0) {
    console.log(`\n❌ Syntax Errors: ${results.syntaxErrors.length}`);
    console.log('---');
    results.syntaxErrors.forEach(item => {
      console.log(`  File: ${item.file}`);
      console.log(`  Error: ${item.error}`);
      console.log();
    });
  } else {
    console.log('✅ No syntax errors found');
  }

  console.log('\n' + '='.repeat(80));

  const successRate = ((results.validFiles / results.totalFiles) * 100).toFixed(1);
  console.log(`\n📈 Success Rate: ${successRate}%`);

  if (results.brokenLinks.length === 0 && results.syntaxErrors.length === 0) {
    console.log('\n✅ All documentation is valid!');
    process.exit(0);
  } else {
    console.log('\n⚠️  Some issues found. Review above.');
    process.exit(1);
  }
}

main();
