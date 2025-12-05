/**
 * Doctest Transformer - Convert @example blocks to Vitest test cases
 * Takes extracted examples and generates executable test code
 */

/**
 * Transform examples into Vitest test cases
 * @param {Array} examples - Array of example objects from extractor
 * @param {string} sourceFile - Source file name (for describe block)
 * @returns {string} Vitest test code
 */
export function transformToVitest(examples, sourceFile) {
  if (!examples || examples.length === 0) {
    return generateEmptyTest(sourceFile);
  }

  const imports = new Set();
  const tests = [];

  // Extract and deduplicate imports from examples
  for (const example of examples) {
    const importMatches = example.code.match(/import\s+{[^}]+}\s+from\s+['"][^'"]+['"]/g);
    if (importMatches) {
      importMatches.forEach(imp => {
        // Rewrite relative imports from source files to test file location
        // e.g., './time.mjs' -> '../../src/time.mjs'
        const rewritten = imp.replace(/from\s+['"]\.\/([^'"]+)['"]/g, "from '../../src/$1'");
        imports.add(rewritten);
      });
    }
  }

  // Generate test cases
  for (let i = 0; i < examples.length; i++) {
    const example = examples[i];
    const testCode = generateTestCase(example, i);
    tests.push(testCode);
  }

  return generateTestFile(sourceFile, Array.from(imports), tests);
}

/**
 * Generate a single test case from an example
 * @param {Object} example - Example object {code, functionName, lineNumber}
 * @param {number} index - Test index for unique naming
 * @returns {string} Vitest test code
 */
function generateTestCase(example, index) {
  const { code, functionName, lineNumber } = example;

  // Clean up code: remove imports if present (will be added to file header)
  const cleanCode = code
    .split('\n')
    .filter(line => !line.trim().startsWith('import '))
    .join('\n')
    .trim();

  const testName = `${functionName} example ${index + 1} (line ${lineNumber})`;

  return `  test('${testName}', async () => {
    ${cleanCode}
  });`;
}

/**
 * Generate complete Vitest test file
 * @param {string} sourceFile - Source file name
 * @param {Array} imports - Array of import statements
 * @param {Array} tests - Array of test code strings
 * @returns {string} Complete test file code
 */
function generateTestFile(sourceFile, imports, tests) {
  const header = `import { describe, test, expect } from 'vitest';

${imports.length > 0 ? imports.join('\n') + '\n\n' : ''}`;

  const describe = `describe('Doctests: ${sourceFile}', () => {
${tests.join('\n\n')}
});
`;

  return header + describe;
}

/**
 * Generate empty test file (for source files with no examples)
 * @param {string} sourceFile - Source file name
 * @returns {string} Empty test file
 */
function generateEmptyTest(sourceFile) {
  return `import { describe, test } from 'vitest';

describe('Doctests: ${sourceFile}', () => {
  test('no examples yet', () => {
    // Add @example tags to source file functions
  });
});
`;
}

/**
 * CLI interface for testing transformer
 */
if (process.argv[1] === new URL(import.meta.url).pathname) {
  // Read JSON from stdin
  let input = '';
  process.stdin.on('data', chunk => { input += chunk; });
  process.stdin.on('end', () => {
    try {
      const data = JSON.parse(input);
      const output = transformToVitest(data.examples, data.sourceFile || 'test.mjs');
      console.log(output);
    } catch (error) {
      console.error('Error:', error.message);
      process.exit(1);
    }
  });
}
