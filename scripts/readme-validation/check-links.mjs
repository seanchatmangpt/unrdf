#!/usr/bin/env node

/**
 * Check all links in README.md for validity
 *
 * Validates:
 * - Internal links (files, anchors)
 * - External links (HTTP status)
 * - Relative paths
 */

import { readFile, access } from 'fs/promises';
import { join, dirname, resolve } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = join(__dirname, '../..');

/**
 * Extract all markdown links from content
 * @param {string} content - Markdown content
 * @returns {Array<{text: string, url: string, line: number, type: string}>}
 */
function extractLinks(content) {
  const links = [];
  const lines = content.split('\n');

  // Match [text](url) and ![alt](url)
  const linkRegex = /!?\[([^\]]+)\]\(([^)]+)\)/g;

  lines.forEach((line, lineIndex) => {
    let match;
    while ((match = linkRegex.exec(line)) !== null) {
      const [full, text, url] = match;

      // Determine link type
      let type = 'unknown';
      if (url.startsWith('http://') || url.startsWith('https://')) {
        type = 'external';
      } else if (url.startsWith('#')) {
        type = 'anchor';
      } else if (url.startsWith('/')) {
        type = 'absolute';
      } else {
        type = 'relative';
      }

      links.push({
        text,
        url,
        line: lineIndex + 1,
        type,
        isImage: full.startsWith('!')
      });
    }
  });

  return links;
}

/**
 * Extract all anchor targets from content
 * @param {string} content - Markdown content
 * @returns {Set<string>} - Set of valid anchors
 */
function extractAnchors(content) {
  const anchors = new Set();
  const lines = content.split('\n');

  lines.forEach(line => {
    // Match headers: ## Header Text -> #header-text
    const headerMatch = line.match(/^(#{1,6})\s+(.+)$/);
    if (headerMatch) {
      const headerText = headerMatch[2];
      // GitHub-style anchor: lowercase, spaces to hyphens, remove special chars
      const anchor = headerText
        .toLowerCase()
        .replace(/[^\w\s-]/g, '')
        .replace(/\s+/g, '-');
      anchors.add(`#${anchor}`);
    }

    // Match explicit anchors: <a name="anchor"></a> or <a id="anchor"></a>
    const anchorMatch = line.match(/<a\s+(?:name|id)="([^"]+)"/);
    if (anchorMatch) {
      anchors.add(`#${anchorMatch[1]}`);
    }
  });

  return anchors;
}

/**
 * Check if file exists
 * @param {string} filepath - File path to check
 * @returns {Promise<boolean>}
 */
async function fileExists(filepath) {
  try {
    await access(filepath);
    return true;
  } catch {
    return false;
  }
}

/**
 * Check external URL
 * @param {string} url - URL to check
 * @returns {Promise<{ok: boolean, status?: number, error?: string}>}
 */
async function checkExternalUrl(url) {
  try {
    // Skip localhost and example.com URLs (not checkable in CI)
    if (url.includes('localhost') || url.includes('example.com')) {
      return { ok: true, status: 200, note: 'skipped (local/example)' };
    }

    const response = await fetch(url, {
      method: 'HEAD',
      redirect: 'follow',
      signal: AbortSignal.timeout(5000) // 5 second timeout
    });

    return { ok: response.ok, status: response.status };
  } catch (error) {
    return { ok: false, error: error.message };
  }
}

async function main() {
  try {
    const readmePath = join(projectRoot, 'README.md');
    const content = await readFile(readmePath, 'utf-8');

    console.log('🔗 Checking links in README.md...\n');

    // Extract links and anchors
    const links = extractLinks(content);
    const validAnchors = extractAnchors(content);

    console.log(`📊 Found ${links.length} links to validate`);
    console.log(`   External: ${links.filter(l => l.type === 'external').length}`);
    console.log(`   Internal: ${links.filter(l => l.type === 'relative' || l.type === 'absolute').length}`);
    console.log(`   Anchors: ${links.filter(l => l.type === 'anchor').length}`);
    console.log(`\n🎯 Found ${validAnchors.size} valid anchor targets\n`);

    const results = {
      total: links.length,
      passed: 0,
      failed: 0,
      skipped: 0,
      errors: []
    };

    // Validate each link
    for (const link of links) {
      const { text, url, line, type } = link;

      let valid = false;
      let reason = '';

      if (type === 'anchor') {
        // Check anchor exists in document
        valid = validAnchors.has(url);
        if (!valid) {
          reason = `Anchor not found: ${url}`;
        }
      } else if (type === 'relative' || type === 'absolute') {
        // Check file exists
        const filepath = type === 'absolute'
          ? join(projectRoot, url.replace(/^\//, ''))
          : resolve(dirname(readmePath), url.split('#')[0]); // Remove anchor part

        valid = await fileExists(filepath);
        if (!valid) {
          reason = `File not found: ${filepath}`;
        }

        // If file exists and has anchor, check anchor
        if (valid && url.includes('#')) {
          const anchor = '#' + url.split('#')[1];
          if (filepath.endsWith('.md')) {
            // TODO: Read target file and check anchor
            // For now, skip anchor validation in other files
            results.skipped++;
            continue;
          }
        }
      } else if (type === 'external') {
        // Check external URL
        const check = await checkExternalUrl(url);
        valid = check.ok;
        if (!valid) {
          reason = check.error || `HTTP ${check.status}`;
        } else if (check.note) {
          // Skipped URL
          results.skipped++;
          console.log(`⏭️  ${url} - ${check.note}`);
          continue;
        }
      }

      if (valid) {
        results.passed++;
        console.log(`✅ Line ${line}: ${url}`);
      } else {
        results.failed++;
        results.errors.push({ line, url, text, reason });
        console.log(`❌ Line ${line}: ${url} - ${reason}`);
      }
    }

    // Summary
    console.log('\n' + '='.repeat(60));
    console.log('📊 Link Validation Summary');
    console.log('='.repeat(60));
    console.log(`Total links: ${results.total}`);
    console.log(`✅ Passed: ${results.passed}`);
    console.log(`⏭️  Skipped: ${results.skipped}`);
    console.log(`❌ Failed: ${results.failed}`);

    if (results.failed > 0) {
      console.log('\n❌ Broken Links:');
      results.errors.forEach(err => {
        console.log(`   Line ${err.line}: [${err.text}](${err.url})`);
        console.log(`              ${err.reason}\n`);
      });

      console.error(`\n❌ VALIDATION FAILED: ${results.failed} broken links`);
      process.exit(1);
    } else {
      console.log('\n✅ All links valid!');
    }

  } catch (error) {
    console.error('❌ Error checking links:', error);
    process.exit(1);
  }
}

main();
