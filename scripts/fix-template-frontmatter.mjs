#!/usr/bin/env node
/**
 * Fix template frontmatter to comply with catalog schema
 *
 * 1. Add quotes to unquoted YAML values
 * 2. Add missing 'category' field based on template path
 * 3. Validate all templates after fixes
 */

import { readFileSync, writeFileSync, existsSync, readdirSync, statSync } from 'node:fs';
import { resolve, dirname, join } from 'node:path';
import { execSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = resolve(__dirname, '..');
const TEMPLATES_DIR = resolve(ROOT_DIR, 'packages/cli/templates');

console.log('DEBUG: ROOT_DIR =', ROOT_DIR);
console.log('DEBUG: TEMPLATES_DIR =', TEMPLATES_DIR);

// Map template paths to categories
const pathToCategory = {
  'packages/cli/templates/sync/': 'codegen',
  'packages/cli/templates/playground/': 'documentation',
  'packages/cli/templates/scaffolding/': 'scaffolding',
  'packages/cli/templates/config/': 'configuration',
  'packages/cli/templates/api/': 'api',
  'packages/cli/templates/notifications/': 'notifications',
  'packages/cli/templates/cicd/': 'cicd',
  'packages/cli/templates/testing/': 'testing',
  'packages/cli/templates/docs/': 'documentation',
};

// Debug counter (global scope)
let debugCounter = 0;
const DEBUG_MAX = 3; // Debug first 3 templates

/**
 * Determine category from template path
 */
function getCategoryFromPath(templatePath) {
  const relPath = templatePath.replace(ROOT_DIR, '');

  // Debug first 3 templates (unconditional to see if function is called)
  if (debugCounter < DEBUG_MAX) {
    console.log(`\nDEBUG getCategoryFromPath (#${debugCounter + 1}): ${templatePath.split('/').pop()}`);
    console.log(`  relPath: "${relPath}"`);
    console.log(`  Looking for matches in pathToCategory:`);
    for (const [pathPrefix, category] of Object.entries(pathToCategory)) {
      const matches = relPath.includes(pathPrefix);
      console.log(`    "${pathPrefix}" -> ${matches ? 'MATCH!' : 'no match'} (${category})`);
    }
    const result = relPath.includes('packages/cli/templates/api/') ? 'api' :
                   relPath.includes('packages/cli/templates/config/') ? 'configuration' :
                   relPath.includes('packages/cli/templates/sync/') ? 'codegen' :
                   'documentation';
    console.log(`  Result would be: ${result}`);
    debugCounter++;
  }

  for (const [pathPrefix, category] of Object.entries(pathToCategory)) {
    if (relPath.includes(pathPrefix)) {
      return category;
    }
  }
  return 'documentation'; // default
}

/**
 * Recursively find all .njk files
 */
function findNjkFiles(dir, files = []) {
  const entries = readdirSync(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);

    if (entry.isDirectory()) {
      findNjkFiles(fullPath, files);
    } else if (entry.isFile() && entry.name.endsWith('.njk')) {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * Fix frontmatter in a template file using regex
 */
function fixTemplateFrontmatter(templatePath) {
  // Debug first call only
  if (debugCounter === 0) {
    console.log(`\nDEBUG: First call to fixTemplateFrontmatter`);
    console.log(`  templatePath: ${templatePath}`);
  }

  let content = readFileSync(templatePath, 'utf-8');

  // Extract frontmatter between --- markers
  // Handle two formats:
  // 1. Standard: \n---\n (closing --- on its own line)
  // 2. Compact: field value directly followed by --- (no preceding newline)
  const frontmatterMatch = content.match(/^---\n([\s\S]*?)---/);
  if (!frontmatterMatch) {
    return { fixed: false, reason: 'No frontmatter found' };
  }

  let frontmatter = frontmatterMatch[1];
  const body = content.slice(frontmatterMatch[0].length);

  // Add category if missing
  let category;
  if (!frontmatter.includes('category:')) {
    category = getCategoryFromPath(templatePath);
    // Add category as a root-level field (not inside arrays)
    // Find the last line that's not part of a YAML array
    const lines = frontmatter.split('\n');
    let lastRootLine = 0;
    for (let i = lines.length - 1; i >= 0; i--) {
      if (!lines[i].startsWith('  -') && !lines[i].startsWith('    ')) {
        lastRootLine = i;
        break;
      }
    }
    // Insert category after the last root-level field
    lines.splice(lastRootLine + 1, 0, `category: ${category}`);
    frontmatter = lines.join('\n');
  } else {
    // Extract existing category for return value
    const categoryMatch = frontmatter.match(/^category:\s*(.*)$/m);
    category = categoryMatch ? categoryMatch[1].trim() : getCategoryFromPath(templatePath);
  }

  // Debug for graphql-schema template
  if (templatePath.includes('graphql-schema.njk')) {
    console.log(`  DEBUG: Category detected for graphql-schema: "${category}"`);
    console.log(`  DEBUG: categoryMatch result:`, frontmatter.match(/^category:\s*(.*)$/m));
  }

  // Fix description: quote unquoted values
  // Pattern: description: Text without quotes -> description: "Text with quotes"
  frontmatter = frontmatter.replace(
    /^description:\s*([a-zA-Z].*?)$/gm,
    (match, desc) => `description: "${desc}"`
  );

  // Fix to field if it has template variables and isn't quoted
  // Split by lines, check each line starting with "to:"
  const lines = frontmatter.split('\n');
  frontmatter = lines.map(line => {
    if (line.startsWith('to:')) {
      const value = line.replace(/^to:\s*/, '').trim();
      // Check if value contains template variables and isn't already quoted
      if (value.includes('{{') && !value.startsWith('"')) {
        return `to: "${value}"`;
      }
    }
    return line;
  }).join('\n');

  // Write back with proper formatting
  // Ensure closing --- is on its own line with proper newlines around it
  const newContent = `---\n${frontmatter}\n---\n${body}`;
  writeFileSync(templatePath, newContent, 'utf-8');
  return { fixed: true, category };
}

/**
 * Main function
 */
function main() {
  console.log('Scanning for templates to fix...\n');

  // Find all .njk files using native Node.js
  const templates = findNjkFiles(TEMPLATES_DIR);
  console.log(`Found ${templates.length} templates\n`);

  let fixed = 0;
  let skipped = 0;

  for (const templatePath of templates) {
    const relPath = templatePath.replace(ROOT_DIR + '/', '');
    try {
      const result = fixTemplateFrontmatter(templatePath);
      fixed++;
      console.log(`✓ Fixed: ${relPath}`);
      console.log(`  Category: ${result.category}`);

      // Debug for graphql-schema
      if (templatePath.includes('graphql-schema.njk')) {
        console.log(`  DEBUG main: result =`, result);
      }
    } catch (error) {
      skipped++;
      console.error(`✗ Skipped: ${relPath} - ${error.message}`);
    }
  }

  console.log(`\nFixed: ${fixed}, Skipped: ${skipped}`);

  // Now validate all templates
  console.log('\nValidating templates...\n');
  let passed = 0;
  let failed = 0;

  for (const templatePath of templates) {
    const relPath = templatePath.replace(ROOT_DIR + '/', '');
    try {
      const output = execSync(
        `node packages/cli/src/cli/main.mjs template validate --template=${relPath}`,
        { cwd: ROOT_DIR, encoding: 'utf-8', stdio: ['ignore', 'pipe', 'ignore'] }
      );
      if (output.includes('✓')) {
        passed++;
      } else {
        failed++;
      }
    } catch (error) {
      failed++;
    }
  }

  console.log(`\nValidation: ${passed} passed, ${failed} failed`);
}

main();
