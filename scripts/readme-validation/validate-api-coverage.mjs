#!/usr/bin/env node

/**
 * Validate that all exported APIs are documented in README
 *
 * Cross-references:
 * - Public exports from source files
 * - API documentation in README
 */

import { readFile, readdir } from 'fs/promises';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectRoot = join(__dirname, '../..');

/**
 * Extract exports from JavaScript file
 * @param {string} content - File content
 * @param {string} filepath - File path for context
 * @returns {Array<{name: string, type: string, line: number}>}
 */
function extractExports(content, filepath) {
  const exports = [];
  const lines = content.split('\n');

  lines.forEach((line, index) => {
    // Named exports: export function foo()
    const namedMatch = line.match(/^export\s+(async\s+)?(function|class|const|let)\s+(\w+)/);
    if (namedMatch) {
      exports.push({
        name: namedMatch[3],
        type: namedMatch[2],
        line: index + 1,
        file: filepath
      });
    }

    // Export list: export { foo, bar }
    const listMatch = line.match(/^export\s+{([^}]+)}/);
    if (listMatch) {
      const names = listMatch[1].split(',').map(n => n.trim());
      names.forEach(name => {
        const cleanName = name.split(' as ')[0].trim(); // Handle 'export { foo as bar }'
        exports.push({
          name: cleanName,
          type: 'export',
          line: index + 1,
          file: filepath
        });
      });
    }

    // Default export: export default function() or export default class
    if (line.match(/^export\s+default\s+(function|class)/)) {
      exports.push({
        name: 'default',
        type: 'default',
        line: index + 1,
        file: filepath
      });
    }
  });

  return exports;
}

/**
 * Extract documented APIs from README
 * @param {string} content - README content
 * @returns {Set<string>} - Set of documented API names
 */
function extractDocumentedAPIs(content) {
  const documented = new Set();
  const lines = content.split('\n');

  lines.forEach(line => {
    // Match function/class headers in reference section
    // e.g., ### `createDocument(options)`
    const apiMatch = line.match(/^#{2,4}\s+`?(\w+)(?:\(|`)/);
    if (apiMatch) {
      documented.add(apiMatch[1]);
    }

    // Match code examples that show exports
    const exportMatch = line.match(/^\s*(?:export\s+)?(?:function|class|const)\s+(\w+)/);
    if (exportMatch) {
      documented.add(exportMatch[1]);
    }
  });

  return documented;
}

/**
 * Get all source files recursively
 * @param {string} dir - Directory to scan
 * @param {string} ext - File extension filter
 * @returns {Promise<Array<string>>}
 */
async function getSourceFiles(dir, ext = '.mjs') {
  const files = [];

  async function scan(currentDir) {
    const entries = await readdir(currentDir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(currentDir, entry.name);

      if (entry.isDirectory()) {
        // Skip node_modules, test directories
        if (!entry.name.startsWith('.') &&
            entry.name !== 'node_modules' &&
            entry.name !== '__tests__' &&
            !entry.name.endsWith('.test.mjs')) {
          await scan(fullPath);
        }
      } else if (entry.isFile() && entry.name.endsWith(ext)) {
        // Skip test files
        if (!entry.name.includes('.test.') && !entry.name.includes('.spec.')) {
          files.push(fullPath);
        }
      }
    }
  }

  await scan(dir);
  return files;
}

async function main() {
  try {
    console.log('📚 Validating API coverage in README...\n');

    // Read README
    const readmePath = join(projectRoot, 'README.md');
    const readmeContent = await readFile(readmePath, 'utf-8');
    const documentedAPIs = extractDocumentedAPIs(readmeContent);

    console.log(`📖 Found ${documentedAPIs.size} documented APIs in README`);

    // Find all source files (check both src/ and lib/ if they exist)
    const srcDir = join(projectRoot, 'src');
    const sourceFiles = await getSourceFiles(srcDir);

    console.log(`📂 Scanning ${sourceFiles.length} source files...\n`);

    // Extract all exports
    const allExports = [];
    for (const filepath of sourceFiles) {
      const content = await readFile(filepath, 'utf-8');
      const exports = extractExports(content, filepath);
      allExports.push(...exports);
    }

    console.log(`📤 Found ${allExports.length} total exports\n`);

    // Filter out internal/private exports (starting with _ or in internal/ dirs)
    const publicExports = allExports.filter(exp =>
      !exp.name.startsWith('_') &&
      !exp.file.includes('/internal/') &&
      !exp.file.includes('/.') &&
      exp.name !== 'default'  // Skip default exports for now
    );

    console.log(`🔓 ${publicExports.length} public exports to verify\n`);

    // Check coverage
    const results = {
      total: publicExports.length,
      documented: 0,
      undocumented: 0,
      missing: []
    };

    for (const exp of publicExports) {
      if (documentedAPIs.has(exp.name)) {
        results.documented++;
        console.log(`✅ ${exp.name} (${exp.file}:${exp.line})`);
      } else {
        results.undocumented++;
        results.missing.push(exp);
        console.log(`❌ ${exp.name} (${exp.file}:${exp.line}) - NOT DOCUMENTED`);
      }
    }

    // Summary
    console.log('\n' + '='.repeat(60));
    console.log('📊 API Coverage Summary');
    console.log('='.repeat(60));
    console.log(`Total public APIs: ${results.total}`);
    console.log(`✅ Documented: ${results.documented}`);
    console.log(`❌ Undocumented: ${results.undocumented}`);

    if (results.total > 0) {
      const coverage = (results.documented / results.total * 100).toFixed(1);
      console.log(`📈 Coverage: ${coverage}%`);
    }

    if (results.undocumented > 0) {
      console.log('\n❌ Missing API Documentation:');
      results.missing.forEach(exp => {
        console.log(`   ${exp.name} (${exp.type})`);
        console.log(`     File: ${exp.file}:${exp.line}\n`);
      });

      console.error(`\n❌ VALIDATION FAILED: ${results.undocumented} undocumented APIs`);
      process.exit(1);
    } else {
      console.log('\n✅ 100% API coverage!');
    }

  } catch (error) {
    console.error('❌ Error validating API coverage:', error);
    process.exit(1);
  }
}

main();
