#!/usr/bin/env node
/**
 * Cross-Reference Validator
 *
 * Validates all cross-references between documentation and code:
 * 1. Internal documentation links (markdown links)
 * 2. Code references in docs (file:line references)
 * 3. Documentation URLs in code (error messages, comments)
 * 4. Import statements in documentation
 *
 * @module validation/cross-reference-check
 */

import { readFile, access } from 'node:fs/promises';
import { join, dirname, resolve, relative, extname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { glob } from 'glob';
import { constants } from 'node:fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const PROJECT_ROOT = resolve(__dirname, '..');

// Color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
};

/**
 * Check if a file exists
 * @param {string} filePath - Path to check
 * @returns {Promise<boolean>} True if file exists
 */
async function fileExists(filePath) {
  try {
    await access(filePath, constants.F_OK);
    return true;
  } catch {
    return false;
  }
}

/**
 * Extract markdown links from content
 * @param {string} content - Markdown content
 * @param {string} filePath - Source file path
 * @returns {Array<{text: string, link: string, line: number, type: string}>}
 */
function extractMarkdownLinks(content, filePath) {
  const links = [];
  const lines = content.split('\n');

  // Match [text](link) and [text](link "title")
  const markdownLinkRegex = /\[([^\]]+)\]\(([^)]+?)(?:\s+"[^"]*")?\)/g;
  // Match <http://...> or <https://...>
  const autoLinkRegex = /<(https?:\/\/[^>]+)>/g;
  // Match bare URLs
  const bareUrlRegex = /(?:^|\s)(https?:\/\/[^\s<>[\]()]+)/g;

  lines.forEach((line, index) => {
    let match;

    // Markdown links [text](url)
    while ((match = markdownLinkRegex.exec(line)) !== null) {
      const [, text, link] = match;
      links.push({
        text: text.trim(),
        link: link.trim(),
        line: index + 1,
        type: 'markdown',
        source: filePath
      });
    }

    // Auto-links <url>
    while ((match = autoLinkRegex.exec(line)) !== null) {
      const [, link] = match;
      links.push({
        text: link,
        link,
        line: index + 1,
        type: 'autolink',
        source: filePath
      });
    }
  });

  return links;
}

/**
 * Extract code references from documentation (file:line or just file paths)
 * @param {string} content - Document content
 * @param {string} filePath - Source file path
 * @returns {Array<{file: string, line?: number, lineNum: number}>}
 */
function extractCodeReferences(content, filePath) {
  const refs = [];
  const lines = content.split('\n');

  // Match patterns like: src/store.mjs:42, packages/core/src/index.mjs:10
  const fileLineRegex = /([a-z0-9/_-]+(?:\/[a-z0-9_-]+)*\.[a-z]+):(\d+)/gi;
  // Match file paths in backticks or code blocks
  const filePathRegex = /`([a-z0-9/_-]+(?:\/[a-z0-9_-]+)*\.(?:mjs|js|ts|json|md))`/gi;

  lines.forEach((line, index) => {
    let match;

    // File:line references
    while ((match = fileLineRegex.exec(line)) !== null) {
      const [, file, lineNumber] = match;
      refs.push({
        file,
        line: parseInt(lineNumber, 10),
        lineNum: index + 1,
        source: filePath,
        type: 'file:line'
      });
    }

    // File path references
    while ((match = filePathRegex.exec(line)) !== null) {
      const [, file] = match;
      // Skip if already captured as file:line
      if (!refs.some(r => r.file === file && r.lineNum === index + 1)) {
        refs.push({
          file,
          lineNum: index + 1,
          source: filePath,
          type: 'file'
        });
      }
    }
  });

  return refs;
}

/**
 * Extract documentation URLs from code files
 * @param {string} content - Code content
 * @param {string} filePath - Source file path
 * @returns {Array<{url: string, line: number, context: string}>}
 */
function extractDocsUrlsFromCode(content, filePath) {
  const urls = [];
  const lines = content.split('\n');

  // Match URLs in strings/comments pointing to docs
  const docsUrlRegex = /(?:https?:\/\/)?(?:github\.com\/[^/]+\/[^/]+\/(?:blob|tree)\/[^/]+\/)?docs\/[a-zA-Z0-9/_-]+\.md/g;
  const relativeDocsRegex = /['"`](\.\.?\/)*docs\/[a-zA-Z0-9/_-]+\.md['"`]/g;

  lines.forEach((line, index) => {
    let match;

    // Absolute doc URLs
    while ((match = docsUrlRegex.exec(line)) !== null) {
      urls.push({
        url: match[0],
        line: index + 1,
        context: line.trim(),
        source: filePath,
        type: 'absolute'
      });
    }

    // Relative doc paths
    while ((match = relativeDocsRegex.exec(line)) !== null) {
      const url = match[0].replace(/['"`]/g, '');
      urls.push({
        url,
        line: index + 1,
        context: line.trim(),
        source: filePath,
        type: 'relative'
      });
    }
  });

  return urls;
}

/**
 * Extract import examples from documentation
 * @param {string} content - Document content
 * @param {string} filePath - Source file path
 * @returns {Array<{importPath: string, line: number, statement: string}>}
 */
function extractImportExamples(content, filePath) {
  const imports = [];
  const lines = content.split('\n');

  // Match import statements in code blocks
  const importRegex = /import\s+(?:{[^}]+}|\*\s+as\s+\w+|\w+)\s+from\s+['"]([^'"]+)['"]/g;

  let inCodeBlock = false;
  let codeBlockLang = '';

  lines.forEach((line, index) => {
    // Track code blocks
    if (line.trim().startsWith('```')) {
      if (!inCodeBlock) {
        inCodeBlock = true;
        codeBlockLang = line.trim().slice(3).toLowerCase();
      } else {
        inCodeBlock = false;
        codeBlockLang = '';
      }
      return;
    }

    // Only check imports in JS/TS code blocks
    if (inCodeBlock && ['js', 'javascript', 'mjs', 'ts', 'typescript'].includes(codeBlockLang)) {
      let match;
      while ((match = importRegex.exec(line)) !== null) {
        const [statement, importPath] = match;
        imports.push({
          importPath,
          line: index + 1,
          statement: statement.trim(),
          source: filePath
        });
      }
    }
  });

  return imports;
}

/**
 * Resolve a link relative to a source file
 * @param {string} link - Link to resolve
 * @param {string} sourceFile - Source file path
 * @returns {string} Resolved absolute path
 */
function resolveLink(link, sourceFile) {
  // Remove anchor
  const [path] = link.split('#');

  // Skip URLs
  if (path.startsWith('http://') || path.startsWith('https://')) {
    return null;
  }

  // Resolve relative to source file
  const sourceDir = dirname(sourceFile);
  return resolve(sourceDir, path);
}

/**
 * Check if line number exists in file
 * @param {string} filePath - File to check
 * @param {number} lineNumber - Line number to verify
 * @returns {Promise<boolean>} True if line exists
 */
async function lineExists(filePath, lineNumber) {
  try {
    const content = await readFile(filePath, 'utf-8');
    const lines = content.split('\n');
    return lineNumber > 0 && lineNumber <= lines.length;
  } catch {
    return false;
  }
}

/**
 * Validate markdown links
 * @param {Array} allFiles - All markdown files
 * @returns {Promise<Array>} Validation results
 */
async function validateMarkdownLinks(allFiles) {
  const results = [];

  for (const file of allFiles) {
    const content = await readFile(file, 'utf-8');
    const links = extractMarkdownLinks(content, file);

    for (const link of links) {
      // Skip external URLs
      if (link.link.startsWith('http://') || link.link.startsWith('https://')) {
        continue;
      }

      // Skip mailto links
      if (link.link.startsWith('mailto:')) {
        continue;
      }

      // Skip anchor-only links
      if (link.link.startsWith('#')) {
        // TODO: Could validate anchors exist in current file
        continue;
      }

      const resolved = resolveLink(link.link, file);
      if (!resolved) continue;

      const exists = await fileExists(resolved);

      if (!exists) {
        results.push({
          type: 'broken-link',
          severity: 'error',
          file: relative(PROJECT_ROOT, file),
          line: link.line,
          link: link.link,
          resolved: relative(PROJECT_ROOT, resolved),
          message: `Broken link: ${link.link} -> ${relative(PROJECT_ROOT, resolved)}`
        });
      }
    }
  }

  return results;
}

/**
 * Validate code references in documentation
 * @param {Array} docFiles - All documentation files
 * @returns {Promise<Array>} Validation results
 */
async function validateCodeReferences(docFiles) {
  const results = [];

  for (const file of docFiles) {
    const content = await readFile(file, 'utf-8');
    const refs = extractCodeReferences(content, file);

    for (const ref of refs) {
      const filePath = resolve(PROJECT_ROOT, ref.file);
      const exists = await fileExists(filePath);

      if (!exists) {
        results.push({
          type: 'broken-code-ref',
          severity: 'error',
          file: relative(PROJECT_ROOT, file),
          line: ref.lineNum,
          reference: ref.file,
          message: `Code file not found: ${ref.file}`
        });
        continue;
      }

      // If line number specified, verify it exists
      if (ref.line) {
        const lineIsValid = await lineExists(filePath, ref.line);
        if (!lineIsValid) {
          results.push({
            type: 'invalid-line-ref',
            severity: 'warning',
            file: relative(PROJECT_ROOT, file),
            line: ref.lineNum,
            reference: `${ref.file}:${ref.line}`,
            message: `Line ${ref.line} does not exist in ${ref.file}`
          });
        }
      }
    }
  }

  return results;
}

/**
 * Validate documentation URLs in code
 * @param {Array} codeFiles - All code files
 * @returns {Promise<Array>} Validation results
 */
async function validateDocsUrls(codeFiles) {
  const results = [];

  for (const file of codeFiles) {
    const content = await readFile(file, 'utf-8');
    const urls = extractDocsUrlsFromCode(content, file);

    for (const urlRef of urls) {
      let docPath;

      if (urlRef.type === 'relative') {
        // Resolve relative path
        docPath = resolve(dirname(file), urlRef.url);
      } else {
        // Extract path from URL
        const match = urlRef.url.match(/docs\/[a-zA-Z0-9/_-]+\.md/);
        if (match) {
          docPath = resolve(PROJECT_ROOT, match[0]);
        }
      }

      if (docPath) {
        const exists = await fileExists(docPath);
        if (!exists) {
          results.push({
            type: 'broken-docs-url',
            severity: 'error',
            file: relative(PROJECT_ROOT, file),
            line: urlRef.line,
            url: urlRef.url,
            resolved: relative(PROJECT_ROOT, docPath),
            context: urlRef.context,
            message: `Documentation file not found: ${urlRef.url}`
          });
        }
      }
    }
  }

  return results;
}

/**
 * Validate import examples in documentation
 * @param {Array} docFiles - All documentation files
 * @returns {Promise<Array>} Validation results
 */
async function validateImportExamples(docFiles) {
  const results = [];

  for (const file of docFiles) {
    const content = await readFile(file, 'utf-8');
    const imports = extractImportExamples(content, file);

    for (const imp of imports) {
      // Skip external packages (no ./ or ../)
      if (!imp.importPath.startsWith('./') && !imp.importPath.startsWith('../')) {
        continue;
      }

      // Resolve the import path relative to project root or package
      const sourceDir = dirname(file);
      let importFile = resolve(sourceDir, imp.importPath);

      // Try with various extensions
      const extensions = ['', '.mjs', '.js', '.ts', '/index.mjs', '/index.js'];
      let found = false;

      for (const ext of extensions) {
        const testPath = importFile + ext;
        if (await fileExists(testPath)) {
          found = true;
          break;
        }
      }

      if (!found) {
        results.push({
          type: 'invalid-import',
          severity: 'warning',
          file: relative(PROJECT_ROOT, file),
          line: imp.line,
          importPath: imp.importPath,
          statement: imp.statement,
          message: `Import path not found: ${imp.importPath}`
        });
      }
    }
  }

  return results;
}

/**
 * Main validation function
 */
async function main() {
  console.log(`${colors.cyan}Cross-Reference Validation${colors.reset}\n`);
  console.log(`Project root: ${PROJECT_ROOT}\n`);

  // Find all files
  console.log('📁 Scanning files...');
  const [markdownFiles, codeFiles] = await Promise.all([
    glob('**/*.md', {
      cwd: PROJECT_ROOT,
      ignore: ['node_modules/**', '**/node_modules/**', 'dist/**', 'coverage/**'],
      absolute: true
    }),
    glob('**/*.{mjs,js,ts}', {
      cwd: PROJECT_ROOT,
      ignore: ['node_modules/**', '**/node_modules/**', 'dist/**', 'coverage/**'],
      absolute: true
    })
  ]);

  console.log(`Found ${markdownFiles.length} markdown files`);
  console.log(`Found ${codeFiles.length} code files\n`);

  // Run validations
  console.log('🔍 Validating markdown links...');
  const linkResults = await validateMarkdownLinks(markdownFiles);
  console.log(`  Found ${linkResults.length} issues\n`);

  console.log('🔍 Validating code references in docs...');
  const codeRefResults = await validateCodeReferences(markdownFiles);
  console.log(`  Found ${codeRefResults.length} issues\n`);

  console.log('🔍 Validating documentation URLs in code...');
  const docsUrlResults = await validateDocsUrls(codeFiles);
  console.log(`  Found ${docsUrlResults.length} issues\n`);

  console.log('🔍 Validating import examples...');
  const importResults = await validateImportExamples(markdownFiles);
  console.log(`  Found ${importResults.length} issues\n`);

  // Combine all results
  const allResults = [
    ...linkResults,
    ...codeRefResults,
    ...docsUrlResults,
    ...importResults
  ];

  // Sort by severity and file
  allResults.sort((a, b) => {
    if (a.severity !== b.severity) {
      return a.severity === 'error' ? -1 : 1;
    }
    return a.file.localeCompare(b.file);
  });

  // Display results
  console.log(`${colors.cyan}========================================${colors.reset}`);
  console.log(`${colors.cyan}VALIDATION RESULTS${colors.reset}`);
  console.log(`${colors.cyan}========================================${colors.reset}\n`);

  const errors = allResults.filter(r => r.severity === 'error');
  const warnings = allResults.filter(r => r.severity === 'warning');

  if (errors.length > 0) {
    console.log(`${colors.red}ERRORS (${errors.length}):${colors.reset}\n`);
    for (const error of errors) {
      console.log(`${colors.red}✗${colors.reset} ${error.file}:${error.line}`);
      console.log(`  ${error.message}`);
      if (error.context) {
        console.log(`  Context: ${error.context.slice(0, 80)}...`);
      }
      console.log();
    }
  }

  if (warnings.length > 0) {
    console.log(`${colors.yellow}WARNINGS (${warnings.length}):${colors.reset}\n`);
    for (const warning of warnings) {
      console.log(`${colors.yellow}⚠${colors.reset} ${warning.file}:${warning.line}`);
      console.log(`  ${warning.message}`);
      console.log();
    }
  }

  // Summary
  console.log(`${colors.cyan}========================================${colors.reset}`);
  console.log(`${colors.cyan}SUMMARY${colors.reset}`);
  console.log(`${colors.cyan}========================================${colors.reset}\n`);

  const summary = {
    'Broken Links': linkResults.length,
    'Invalid Code References': codeRefResults.length,
    'Broken Docs URLs': docsUrlResults.length,
    'Invalid Imports': importResults.length,
    'Total Errors': errors.length,
    'Total Warnings': warnings.length,
  };

  for (const [key, value] of Object.entries(summary)) {
    const color = value === 0 ? colors.green : (key.includes('Warning') ? colors.yellow : colors.red);
    console.log(`${key}: ${color}${value}${colors.reset}`);
  }

  console.log();

  // Export results for report
  return {
    summary,
    errors,
    warnings,
    allResults,
    stats: {
      markdownFiles: markdownFiles.length,
      codeFiles: codeFiles.length,
    }
  };
}

// Run validation
main()
  .then((results) => {
    // Write results to JSON for report generation
    const output = JSON.stringify(results, null, 2);
    return import('node:fs/promises').then(({ writeFile }) =>
      writeFile(
        join(PROJECT_ROOT, 'validation', 'cross-reference-results.json'),
        output,
        'utf-8'
      )
    );
  })
  .then(() => {
    console.log(`${colors.green}✓ Results saved to validation/cross-reference-results.json${colors.reset}\n`);
    process.exit(0);
  })
  .catch((error) => {
    console.error(`${colors.red}✗ Validation failed:${colors.reset}`, error);
    process.exit(1);
  });
