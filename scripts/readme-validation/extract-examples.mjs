#!/usr/bin/env node

/**
 * Extract code examples from README.md for validation
 *
 * Extracts JavaScript/TypeScript code blocks and categorizes them by section
 * (Tutorial, How-To, Reference, Explanation)
 */

import { readFile, writeFile, mkdir } from 'fs/promises';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = join(__dirname, '../..');

/**
 * Extract code blocks from markdown content
 * @param {string} content - Markdown content
 * @returns {Array<{section: string, index: number, code: string, language: string}>}
 */
function extractCodeBlocks(content) {
  const blocks = [];
  const lines = content.split('\n');

  let currentSection = 'unknown';
  let inCodeBlock = false;
  let codeBuffer = [];
  let language = '';
  let blockIndex = 0;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Detect section headers
    if (line.match(/^##\s+.*Tutorial/i)) {
      currentSection = 'tutorial';
    } else if (line.match(/^##\s+.*How-?To/i)) {
      currentSection = 'howto';
    } else if (line.match(/^##\s+.*Reference/i)) {
      currentSection = 'reference';
    } else if (line.match(/^##\s+.*Explanation/i)) {
      currentSection = 'explanation';
    }

    // Detect code block start
    if (line.match(/^```(javascript|js|mjs|typescript|ts)/)) {
      inCodeBlock = true;
      language = line.match(/^```(\w+)/)[1];
      codeBuffer = [];
      continue;
    }

    // Detect code block end
    if (inCodeBlock && line.match(/^```$/)) {
      inCodeBlock = false;
      blocks.push({
        section: currentSection,
        index: blockIndex++,
        code: codeBuffer.join('\n'),
        language
      });
      continue;
    }

    // Collect code lines
    if (inCodeBlock) {
      codeBuffer.push(line);
    }
  }

  return blocks;
}

/**
 * Validate code block is syntactically valid
 * @param {string} code - JavaScript code
 * @returns {{valid: boolean, error?: string}}
 */
function validateSyntax(code) {
  try {
    // Basic syntax check using Function constructor
    new Function(code);
    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Categorize code blocks by purpose
 * @param {string} code - Code content
 * @returns {string} - Category: example, snippet, test, config
 */
function categorizeBlock(code) {
  if (code.includes('import') && code.includes('export')) return 'module';
  if (code.includes('describe(') || code.includes('it(')) return 'test';
  if (code.includes('package.json') || code.includes('"scripts"')) return 'config';
  if (code.length < 100) return 'snippet';
  return 'example';
}

async function main() {
  try {
    // Read README
    const readmePath = join(projectRoot, 'README.md');
    const content = await readFile(readmePath, 'utf-8');

    // Extract code blocks
    const blocks = extractCodeBlocks(content);

    console.log(`📝 Extracted ${blocks.length} code blocks from README.md`);

    // Create output directory
    const outputDir = join(projectRoot, 'tmp/readme-examples');
    await mkdir(outputDir, { recursive: true });

    // Statistics
    const stats = {
      total: blocks.length,
      bySection: {},
      byCategory: {},
      syntaxErrors: [],
      valid: 0,
      invalid: 0
    };

    // Process each block
    for (const block of blocks) {
      const { section, index, code, language } = block;

      // Update stats
      stats.bySection[section] = (stats.bySection[section] || 0) + 1;

      const category = categorizeBlock(code);
      stats.byCategory[category] = (stats.byCategory[category] || 0) + 1;

      // Validate syntax (only for JS/TS)
      if (['javascript', 'js', 'mjs', 'typescript', 'ts'].includes(language)) {
        const validation = validateSyntax(code);

        if (validation.valid) {
          stats.valid++;
        } else {
          stats.invalid++;
          stats.syntaxErrors.push({
            section,
            index,
            error: validation.error
          });
        }
      }

      // Write to file
      const ext = language === 'typescript' || language === 'ts' ? 'ts' : 'mjs';
      const filename = `${section}-${index}.${ext}`;
      const filepath = join(outputDir, filename);

      await writeFile(filepath, code, 'utf-8');
    }

    // Generate report
    const report = {
      timestamp: new Date().toISOString(),
      stats,
      blocks: blocks.map(b => ({
        section: b.section,
        index: b.index,
        language: b.language,
        category: categorizeBlock(b.code),
        lines: b.code.split('\n').length
      }))
    };

    // Write report
    const reportPath = join(outputDir, 'extraction-report.json');
    await writeFile(reportPath, JSON.stringify(report, null, 2), 'utf-8');

    // Console output
    console.log('\n📊 Extraction Statistics:');
    console.log(`   Total blocks: ${stats.total}`);
    console.log(`   Valid syntax: ${stats.valid}`);
    console.log(`   Invalid syntax: ${stats.invalid}`);

    console.log('\n📚 By Section:');
    Object.entries(stats.bySection).forEach(([section, count]) => {
      console.log(`   ${section}: ${count}`);
    });

    console.log('\n🏷️  By Category:');
    Object.entries(stats.byCategory).forEach(([category, count]) => {
      console.log(`   ${category}: ${count}`);
    });

    if (stats.syntaxErrors.length > 0) {
      console.log('\n❌ Syntax Errors:');
      stats.syntaxErrors.forEach(err => {
        console.log(`   ${err.section}-${err.index}: ${err.error}`);
      });
    }

    console.log(`\n✅ Examples extracted to: ${outputDir}`);
    console.log(`📄 Report written to: ${reportPath}`);

    // Exit with error if syntax errors found
    if (stats.invalid > 0) {
      console.error(`\n❌ VALIDATION FAILED: ${stats.invalid} syntax errors found`);
      process.exit(1);
    }

  } catch (error) {
    console.error('❌ Error extracting examples:', error);
    process.exit(1);
  }
}

main();
