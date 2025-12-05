/**
 * Doctest Extractor - Parse JSDoc @example blocks from source files
 * Uses regex-based extraction for simple and reliable parsing
 */

import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';

/**
 * Extract all @example blocks from a source file using regex
 * @param {string} sourceFile - Path to source file
 * @returns {Array} Array of example objects: {code, functionName, sourceFile, lineNumber}
 */
export function extractExamples(sourceFile) {
  try {
    const source = readFileSync(sourceFile, 'utf-8');
    const examples = [];
    const sourceLines = source.split('\n');

    // Match JSDoc comment blocks with @example tags
    // Pattern: /** ... @example ... code ... */ followed by export/function declaration
    const jsdocPattern = /\/\*\*[\s\S]*?@example\s*([\s\S]*?)\*\//g;
    let match;

    while ((match = jsdocPattern.exec(source)) !== null) {
      const exampleContent = match[1];

      // Split by * that precedes @ sign or just @example to handle multiple @example in one block
      // The pattern looks for either " * @example" or just "@example"
      // We split and then slice(1) to remove everything before the first @example
      const parts = exampleContent.split(/\s*\*?\s*@example\s*/);

      // If split() produced parts after the first @example, use them
      // Otherwise, treat the whole exampleContent as a single example
      const exampleBlocks = parts.length > 1 ? parts.slice(1) : [exampleContent];

      for (const block of exampleBlocks) {
        // Extract code: skip leading * and whitespace, stop at next * / or next @
        const codeLines = block
          .split('\n')
          .map(line => {
            // Remove leading * and up to one space after it
            return line.replace(/^\s*\*\s?/, '').trim();
          })
          .filter(line => line && !line.startsWith('@'))
          .join('\n')
          .trim();

        if (codeLines && !codeLines.startsWith('Guard') && !codeLines.startsWith('Prevent')) {
          // Find line number
          const matchIndex = match.index;
          const lineNumber = source.substring(0, matchIndex).split('\n').length;

          // Try to find function name after this JSDoc
          let functionName = 'unknown';
          const endOfComment = source.indexOf('*/', matchIndex) + 2;
          const afterComment = source.substring(endOfComment, endOfComment + 500);
          const nameMatch = afterComment.match(/(?:export\s+)?(?:function|const|async function)\s+(\w+)|class\s+(\w+)/);
          if (nameMatch) {
            functionName = nameMatch[1] || nameMatch[2];
          }

          examples.push({
            code: codeLines,
            functionName,
            sourceFile,
            lineNumber
          });
        }
      }
    }

    return examples;
  } catch (error) {
    console.error(`Error extracting examples from ${sourceFile}:`, error.message);
    return [];
  }
}

/**
 * Extract examples from all source files in a directory
 * @param {string} srcDir - Source directory (e.g., 'src/')
 * @returns {Object} Map of {fileName: [examples]}
 */
export function extractAllExamples(srcDir = 'src') {
  const result = {};
  const files = readdirSync(srcDir).filter(f => f.endsWith('.mjs') && !f.startsWith('doctest'));

  for (const file of files) {
    const filePath = join(srcDir, file);
    const examples = extractExamples(filePath);
    if (examples.length > 0) {
      result[file] = examples;
    }
  }

  return result;
}

/**
 * CLI interface for testing extractor
 */
if (process.argv[1] === new URL(import.meta.url).pathname) {
  const filePath = process.argv[2] || 'src/time.mjs';
  const examples = extractExamples(filePath);
  console.log(JSON.stringify({ filePath, exampleCount: examples.length, examples }, null, 2));
}
