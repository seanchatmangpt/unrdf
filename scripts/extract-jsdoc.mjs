#!/usr/bin/env node
/**
 * @file Extract JSDoc comments from UNRDF packages
 * @module scripts/extract-jsdoc
 *
 * Extracts JSDoc function documentation from all packages and outputs structured JSON
 * for use in API documentation generation.
 *
 * Usage: node scripts/extract-jsdoc.mjs
 * Output: packages/nextra/data/api-jsdoc.json
 */

import { readFileSync, writeFileSync, readdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { glob } from 'glob';

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = join(__dirname, '..');

/**
 * Parse JSDoc comment block
 * @param {string} jsdoc - Raw JSDoc comment
 * @returns {Object} Parsed JSDoc data
 */
function parseJSDoc(jsdoc) {
  const lines = jsdoc.replace(/\/\*\*|\*\/|\*/g, '').split('\n').map((l) => l.trim());

  const parsed = {
    description: '',
    params: [],
    returns: '',
    examples: [],
  };

  let currentSection = 'description';
  let currentExample = '';

  for (const line of lines) {
    if (line.startsWith('@param')) {
      const match = line.match(/@param\s+\{([^}]+)\}\s+(\w+)\s*-?\s*(.*)/);
      if (match) {
        parsed.params.push({
          type: match[1],
          name: match[2],
          description: match[3],
        });
      }
      currentSection = 'param';
    } else if (line.startsWith('@returns')) {
      const match = line.match(/@returns?\s+\{([^}]+)\}\s*(.*)/);
      if (match) {
        parsed.returns = { type: match[1], description: match[2] };
      }
      currentSection = 'returns';
    } else if (line.startsWith('@example')) {
      currentSection = 'example';
      currentExample = '';
    } else if (currentSection === 'description' && line) {
      parsed.description += (parsed.description ? ' ' : '') + line;
    } else if (currentSection === 'example') {
      if (line.startsWith('@')) {
        if (currentExample) parsed.examples.push(currentExample.trim());
        currentExample = '';
        currentSection = 'other';
      } else {
        currentExample += line + '\n';
      }
    }
  }

  if (currentExample) parsed.examples.push(currentExample.trim());

  return parsed;
}

/**
 * Extract function documentation from source code
 * @param {string} filePath - Path to source file
 * @returns {Array<Object>} Extracted function docs
 */
function extractFunctionsFromFile(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const functions = [];

  // Match: export function functionName(...) or export const functionName = (...)
  const functionRegex = /\/\*\*[\s\S]*?\*\/\s*export\s+(async\s+)?(function|const)\s+(\w+)/g;

  let match;
  while ((match = functionRegex.exec(content)) !== null) {
    const [fullMatch, isAsync, type, funcName] = match;
    const startPos = match.index;

    // Extract JSDoc comment
    const jsdocMatch = fullMatch.match(/\/\*\*[\s\S]*?\*\//);
    const jsdoc = jsdocMatch ? jsdocMatch[0] : '';
    const parsed = parseJSDoc(jsdoc);

    functions.push({
      name: funcName,
      async: Boolean(isAsync),
      type: type === 'const' ? 'arrow' : 'function',
      ...parsed,
      sourceFile: filePath.replace(ROOT_DIR, ''),
      line: content.substring(0, startPos).split('\n').length,
    });
  }

  return functions;
}

/**
 * Main extraction function
 */
async function extractJSDoc() {
  const packages = readdirSync(join(ROOT_DIR, 'packages'), { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => dirent.name)
    .filter((name) => !name.startsWith('.') && name !== 'nextra');

  const allDocs = {};

  for (const pkg of packages) {
    const pkgPath = join(ROOT_DIR, 'packages', pkg);
    const srcPath = join(pkgPath, 'src');

    try {
      // Find all .mjs files in src/
      const files = await glob('**/*.mjs', { cwd: srcPath, absolute: true });

      const functions = [];
      for (const file of files) {
        const fileFunctions = extractFunctionsFromFile(file);
        functions.push(...fileFunctions);
      }

      if (functions.length > 0) {
        allDocs[`@unrdf/${pkg}`] = functions;
      }
    } catch (error) {
      console.warn(`Skipping ${pkg}: ${error.message}`);
    }
  }

  // Write output
  const outputPath = join(ROOT_DIR, 'packages/nextra/data/api-jsdoc.json');
  writeFileSync(outputPath, JSON.stringify(allDocs, null, 2));

  console.log(`✅ Extracted JSDoc from ${Object.keys(allDocs).length} packages`);
  console.log(`📊 Total functions: ${Object.values(allDocs).flat().length}`);
  console.log(`📁 Output: ${outputPath.replace(ROOT_DIR, '')}`);

  return allDocs;
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  extractJSDoc().catch((error) => {
    console.error('❌ Extraction failed:', error);
    process.exit(1);
  });
}

export { extractJSDoc };
