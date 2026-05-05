#!/usr/bin/env node
/**
 * @file Migrate /docs/ markdown files to Nextra MDX
 * @module scripts/migrate-docs
 *
 * Converts markdown files from /docs/ to MDX format in packages/nextra/app/
 * with proper frontmatter and navigation structure.
 *
 * Usage: node scripts/migrate-docs.mjs
 */

import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'node:fs';
import { join, dirname, basename } from 'node:path';
import { fileURLToPath } from 'node:url';
import { glob } from 'glob';

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = join(__dirname, '..');
const DOCS_DIR = join(ROOT_DIR, 'docs');
const NEXTRA_APP_DIR = join(ROOT_DIR, 'packages/nextra/app');

/**
 * Mapping of doc files to Nextra structure
 */
const FILE_MAPPING = {
  // Guides
  'QUICKSTART.md': 'guides/quick-start/page.mdx',
  'CLI-USAGE-GUIDE.md': 'guides/cli/page.mdx',
  'GETTING-STARTED.md': 'guides/installation/page.mdx',
  'BASIC-USAGE.md': 'guides/basic-usage/page.mdx',

  // Concepts
  'ARCHITECTURE.md': 'concepts/architecture/page.mdx',
  'RDF-FUNDAMENTALS.md': 'concepts/rdf-fundamentals/page.mdx',
  'VALIDATION.md': 'concepts/validation/page.mdx',
  'STREAMING.md': 'concepts/streaming/page.mdx',
  'FEDERATION.md': 'concepts/federation/page.mdx',

  // API Reference (overview pages)
  'API-REFERENCE.md': 'api/page.mdx',
  'CORE-API.md': 'api/core/page.mdx',
  'HOOKS-API.md': 'api/hooks/page.mdx',

  // Papers
  'KGC-4D-PAPER.md': 'papers/kgc-4d/page.mdx',
  'FORENSIC-UX-PAPER.md': 'papers/forensic-ux/page.mdx',
};

/**
 * Convert markdown to MDX with frontmatter
 * @param {string} content - Markdown content
 * @param {string} title - Page title
 * @returns {string} MDX content
 */
function convertToMDX(content, title) {
  // Extract first heading as title if not provided
  if (!title) {
    const headingMatch = content.match(/^#\s+(.+)$/m);
    title = headingMatch ? headingMatch[1] : 'Untitled';
  }

  // Convert code blocks with language hints
  content = content.replace(/```javascript/g, '```js');
  content = content.replace(/```typescript/g, '```ts');

  // Add frontmatter
  const frontmatter = `---
title: ${title}
description: Auto-migrated from /docs/
---

`;

  return frontmatter + content;
}

/**
 * Migrate a single file
 * @param {string} sourcePath - Source file path
 * @param {string} targetPath - Target file path
 */
function migrateFile(sourcePath, targetPath) {
  const content = readFileSync(sourcePath, 'utf-8');
  const title = basename(sourcePath, '.md').replace(/-/g, ' ');
  const mdx = convertToMDX(content, title);

  // Ensure directory exists
  const targetDir = dirname(targetPath);
  if (!existsSync(targetDir)) {
    mkdirSync(targetDir, { recursive: true });
  }

  writeFileSync(targetPath, mdx);
  console.log(`✅ Migrated: ${sourcePath.replace(ROOT_DIR, '')} → ${targetPath.replace(ROOT_DIR, '')}`);
}

/**
 * Main migration function
 */
async function migrateDocs() {
  let migrated = 0;
  let skipped = 0;

  // Migrate mapped files
  for (const [sourceFile, targetFile] of Object.entries(FILE_MAPPING)) {
    const sourcePath = join(DOCS_DIR, sourceFile);
    const targetPath = join(NEXTRA_APP_DIR, targetFile);

    if (existsSync(sourcePath)) {
      migrateFile(sourcePath, targetPath);
      migrated++;
    } else {
      console.warn(`⚠️  Source not found: ${sourceFile}`);
      skipped++;
    }
  }

  console.log(`\n📊 Migration Summary:`);
  console.log(`   Migrated: ${migrated}`);
  console.log(`   Skipped: ${skipped}`);
  console.log(`   Target: ${NEXTRA_APP_DIR.replace(ROOT_DIR, '')}`);
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  migrateDocs().catch((error) => {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  });
}

export { migrateDocs };
