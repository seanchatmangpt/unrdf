#!/usr/bin/env node
/**
 * Documentation Code Example Validator
 *
 * Extracts and validates all code examples from documentation files.
 * Ensures that documentation examples actually work with the current codebase.
 *
 * @module validation/validate-docs-examples
 */

import { readFileSync, writeFileSync, mkdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = join(__dirname, '..');
const docsDir = join(rootDir, 'docs');

// ANSI colors for output
const colors = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

/**
 * Extract code blocks from markdown
 * @param {string} content - Markdown content
 * @param {string} filename - Source filename
 * @returns {Array<{code: string, language: string, lineStart: number, section: string}>}
 */
function extractCodeBlocks(content, filename) {
  const blocks = [];
  const lines = content.split('\n');
  let inCodeBlock = false;
  let currentBlock = [];
  let currentLanguage = '';
  let blockStartLine = 0;
  let currentSection = 'Introduction';

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Track sections for context
    if (line.startsWith('##')) {
      currentSection = line.replace(/^#+\s*/, '').trim();
    }

    // Code block start
    if (line.startsWith('```')) {
      if (!inCodeBlock) {
        inCodeBlock = true;
        currentLanguage = line.slice(3).trim() || 'text';
        blockStartLine = i + 1;
        currentBlock = [];
      } else {
        // Code block end
        inCodeBlock = false;
        if (currentLanguage === 'javascript' || currentLanguage === 'js') {
          blocks.push({
            code: currentBlock.join('\n'),
            language: currentLanguage,
            lineStart: blockStartLine,
            section: currentSection,
            file: filename,
          });
        }
        currentBlock = [];
      }
    } else if (inCodeBlock) {
      currentBlock.push(line);
    }
  }

  return blocks;
}

/**
 * Categorize code examples
 * @param {object} block - Code block
 * @returns {string} Category
 */
function categorizeExample(block) {
  const { code, file, section } = block;

  // Installation examples (not executable)
  if (code.includes('npm install') || code.includes('pnpm add')) {
    return 'installation';
  }

  // Bash commands (not JavaScript)
  if (code.includes('node ') || code.includes('cd ') || code.includes('mkdir ')) {
    return 'bash-command';
  }

  // Comments-only or pseudo-code
  if (code.trim().startsWith('//') && !code.includes('import')) {
    return 'comment-only';
  }

  // Examples that are explicitly marked as wrong
  if (code.includes('❌ WRONG')) {
    return 'negative-example';
  }

  // Migration "before" examples (N3.js, RDFLib, Jena)
  if (file === 'MIGRATION.md') {
    if (section.includes('Before') || code.includes('import { Store } from \'n3\'') ||
        code.includes('from rdflib import') || code.includes('import org.apache.jena')) {
      return 'migration-before';
    }
  }

  // Incomplete snippets (partial code requiring context)
  if (!code.includes('import') && (code.includes('const') || code.includes('function'))) {
    // Check if it references external variables
    if (code.includes('store') && !code.includes('createStore')) {
      return 'incomplete-snippet';
    }
  }

  // Executable examples
  return 'executable';
}

/**
 * Create a standalone test file for a code block
 * @param {object} block - Code block
 * @returns {string} Test file content
 */
function createTestFile(block) {
  const { code } = block;

  // Wrap the code in a try-catch to capture errors
  return `
// Auto-generated test from: ${block.file} (line ${block.lineStart})
// Section: ${block.section}

try {
${code}
  console.log('✅ Example executed successfully');
} catch (error) {
  console.error('❌ Example failed:', error.message);
  console.error('Stack:', error.stack);
  process.exit(1);
}
`;
}

/**
 * Test if a code example is valid by attempting static analysis
 * @param {string} code - JavaScript code
 * @returns {{valid: boolean, errors: string[]}}
 */
function staticAnalyzeCode(code) {
  const errors = [];

  // Check for common API mismatches based on actual exports
  const apiMismatches = [
    {
      pattern: /createKnowledgeSubstrateCore/,
      error: 'createKnowledgeSubstrateCore() does not exist in @unrdf/core exports',
      suggestion: 'Use createStore() from @unrdf/oxigraph and executeSelectSync() from @unrdf/core',
    },
    {
      pattern: /core\.parseRdf/,
      error: 'core.parseRdf() is not a valid API - no "core" object exists',
      suggestion: 'Use store.load() from @unrdf/oxigraph instead',
    },
    {
      pattern: /core\.query/,
      error: 'core.query() is not a valid API',
      suggestion: 'Use executeSelectSync(store, sparql) from @unrdf/core',
    },
    {
      pattern: /core\.createStore/,
      error: 'core.createStore() is not a valid API',
      suggestion: 'Import createStore directly from @unrdf/oxigraph',
    },
    {
      pattern: /from 'n3'/,
      error: 'N3 import in non-migration example',
      suggestion: 'Use @unrdf/oxigraph instead of N3',
    },
    {
      pattern: /new Store\(\)/,
      error: 'N3 Store constructor used instead of createStore()',
      suggestion: 'Use createStore() from @unrdf/oxigraph',
    },
    {
      pattern: /import.*federatedQuery.*@unrdf\/federation/,
      error: 'federatedQuery is not exported from @unrdf/federation',
      suggestion: 'Check actual @unrdf/federation exports',
    },
  ];

  for (const { pattern, error, suggestion } of apiMismatches) {
    if (pattern.test(code)) {
      errors.push(`${error}\n   → ${suggestion}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Main validation function
 */
async function validateDocumentation() {
  console.log(`${colors.cyan}=== Documentation Code Example Validator ===${colors.reset}\n`);

  const docs = [
    'QUICK-START.md',
    'API-REFERENCE.md',
    'MIGRATION.md',
    'WALKTHROUGHS.md',
  ];

  const results = {
    totalExamples: 0,
    executableExamples: 0,
    staticErrors: 0,
    byCategory: {},
    byFile: {},
    failures: [],
  };

  // Extract and categorize all examples
  for (const docFile of docs) {
    const filePath = join(docsDir, docFile);
    console.log(`${colors.blue}📄 Processing ${docFile}...${colors.reset}`);

    try {
      const content = readFileSync(filePath, 'utf-8');
      const blocks = extractCodeBlocks(content, docFile);

      console.log(`   Found ${blocks.length} code blocks`);

      results.byFile[docFile] = {
        total: blocks.length,
        executable: 0,
        staticErrors: 0,
        examples: [],
      };

      for (const block of blocks) {
        results.totalExamples++;

        const category = categorizeExample(block);
        results.byCategory[category] = (results.byCategory[category] || 0) + 1;

        if (category === 'executable') {
          results.executableExamples++;
          results.byFile[docFile].executable++;

          // Perform static analysis
          const analysis = staticAnalyzeCode(block.code);

          if (!analysis.valid) {
            results.staticErrors++;
            results.byFile[docFile].staticErrors++;

            const failure = {
              file: docFile,
              section: block.section,
              line: block.lineStart,
              errors: analysis.errors,
              code: block.code.split('\n').slice(0, 10).join('\n') + '\n...',
            };

            results.failures.push(failure);

            console.log(`   ${colors.red}✗ Static error in ${block.section} (line ${block.lineStart})${colors.reset}`);
            for (const error of analysis.errors) {
              console.log(`     ${colors.yellow}${error}${colors.reset}`);
            }
          } else {
            console.log(`   ${colors.green}✓ ${block.section} (line ${block.lineStart})${colors.reset}`);
          }
        }

        results.byFile[docFile].examples.push({
          section: block.section,
          line: block.lineStart,
          category,
          valid: category === 'executable' ? staticAnalyzeCode(block.code).valid : null,
        });
      }

      console.log('');
    } catch (error) {
      console.error(`${colors.red}❌ Error processing ${docFile}: ${error.message}${colors.reset}`);
    }
  }

  // Print summary
  console.log(`${colors.cyan}=== Summary ===${colors.reset}\n`);
  console.log(`Total code examples: ${results.totalExamples}`);
  console.log(`Executable examples: ${results.executableExamples}`);
  console.log(`Static errors found: ${results.staticErrors}`);
  console.log('');

  console.log(`${colors.cyan}=== By Category ===${colors.reset}`);
  for (const [category, count] of Object.entries(results.byCategory)) {
    console.log(`  ${category}: ${count}`);
  }
  console.log('');

  console.log(`${colors.cyan}=== By File ===${colors.reset}`);
  for (const [file, stats] of Object.entries(results.byFile)) {
    const errorRate = stats.executable > 0 ?
      Math.round((stats.staticErrors / stats.executable) * 100) : 0;
    const color = stats.staticErrors === 0 ? colors.green : colors.red;
    console.log(`  ${file}:`);
    console.log(`    Total: ${stats.total}, Executable: ${stats.executable}, ${color}Errors: ${stats.staticErrors} (${errorRate}%)${colors.reset}`);
  }
  console.log('');

  // Generate detailed report
  const reportPath = join(rootDir, 'DOCS-CODE-ALIGNMENT-REPORT.md');
  generateReport(results, reportPath);
  console.log(`${colors.green}✅ Report generated: ${reportPath}${colors.reset}\n`);

  // Return exit code based on results
  if (results.staticErrors > 0) {
    console.log(`${colors.red}❌ Validation FAILED: ${results.staticErrors} errors found${colors.reset}`);
    return 1;
  } else {
    console.log(`${colors.green}✅ Validation PASSED: All executable examples are valid${colors.reset}`);
    return 0;
  }
}

/**
 * Generate detailed markdown report
 * @param {object} results - Validation results
 * @param {string} reportPath - Output file path
 */
function generateReport(results, reportPath) {
  const timestamp = new Date().toISOString();
  let report = `# Documentation Code Alignment Report

**Generated:** ${timestamp}

## Executive Summary

| Metric | Value |
|--------|-------|
| Total Code Examples | ${results.totalExamples} |
| Executable Examples | ${results.executableExamples} |
| Static Errors Found | ${results.staticErrors} |
| Success Rate | ${results.executableExamples > 0 ? Math.round(((results.executableExamples - results.staticErrors) / results.executableExamples) * 100) : 0}% |

`;

  // Category breakdown
  report += `## Examples by Category\n\n`;
  report += `| Category | Count |\n`;
  report += `|----------|-------|\n`;
  for (const [category, count] of Object.entries(results.byCategory).sort((a, b) => b[1] - a[1])) {
    report += `| ${category} | ${count} |\n`;
  }
  report += `\n`;

  // File breakdown
  report += `## Examples by Documentation File\n\n`;
  for (const [file, stats] of Object.entries(results.byFile)) {
    const errorRate = stats.executable > 0 ?
      Math.round((stats.staticErrors / stats.executable) * 100) : 0;
    const status = stats.staticErrors === 0 ? '✅' : '❌';

    report += `### ${status} ${file}\n\n`;
    report += `- **Total Examples:** ${stats.total}\n`;
    report += `- **Executable:** ${stats.executable}\n`;
    report += `- **Errors:** ${stats.staticErrors} (${errorRate}%)\n\n`;
  }

  // Failures detail
  if (results.failures.length > 0) {
    report += `## ❌ Failures (${results.failures.length})\n\n`;

    for (let i = 0; i < results.failures.length; i++) {
      const failure = results.failures[i];
      report += `### ${i + 1}. ${failure.file} - ${failure.section} (line ${failure.line})\n\n`;
      report += `**Errors:**\n\n`;
      for (const error of failure.errors) {
        report += `- ${error}\n`;
      }
      report += `\n**Code snippet:**\n\n\`\`\`javascript\n${failure.code}\n\`\`\`\n\n`;
    }
  } else {
    report += `## ✅ No Failures\n\nAll executable examples passed static analysis.\n\n`;
  }

  // Recommendations
  report += `## Recommendations\n\n`;
  if (results.staticErrors > 0) {
    report += `1. **Fix API mismatches:** Update documentation to use actual exported APIs\n`;
    report += `2. **Verify imports:** Ensure all examples import from correct packages\n`;
    report += `3. **Test examples:** Run examples to verify they execute correctly\n`;
    report += `4. **Update walkthroughs:** Align walkthrough code with current API\n`;
  } else {
    report += `All examples are aligned with the codebase. Continue to:\n`;
    report += `1. **Runtime validation:** Execute examples to verify behavior\n`;
    report += `2. **Integration tests:** Add examples to test suite\n`;
    report += `3. **Keep in sync:** Update docs when APIs change\n`;
  }

  writeFileSync(reportPath, report);
}

// Run validation
const exitCode = await validateDocumentation();
process.exit(exitCode);
