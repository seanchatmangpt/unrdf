#!/usr/bin/env node
/**
 * API Documentation Generator
 * Extracts JSDoc and generates markdown documentation
 */

import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'fs';
import { execSync } from 'child_process';
import { join, dirname, basename } from 'path';

/**
 * Find all source files
 * @returns {string[]} Array of file paths
 */
function findSourceFiles() {
  try {
    const output = execSync(
      'find packages/*/src -name "*.js" -o -name "*.mjs" | grep -v node_modules | grep -v test',
      { encoding: 'utf8' }
    );
    return output.trim().split('\n').filter(Boolean);
  } catch (error) {
    console.error('Failed to find source files:', error.message);
    return [];
  }
}

/**
 * Extract JSDoc comments from file
 * @param {string} filepath - File path
 * @returns {Array} Extracted documentation
 */
function extractJSDoc(filepath) {
  try {
    const content = readFileSync(filepath, 'utf8');
    const docs = [];

    // Extract JSDoc blocks with their associated code
    const jsdocPattern = /\/\*\*\s*([\s\S]*?)\*\/\s*(?:export\s+)?(?:async\s+)?(function|class|const)\s+(\w+)/g;
    let match;

    while ((match = jsdocPattern.exec(content)) !== null) {
      const [, comment, type, name] = match;

      // Parse JSDoc tags
      const tags = {};
      const description = [];
      const lines = comment.split('\n').map(line => line.replace(/^\s*\*\s?/, ''));

      let currentTag = null;
      for (const line of lines) {
        const tagMatch = line.match(/@(\w+)\s+(.*)/);
        if (tagMatch) {
          currentTag = tagMatch[1];
          if (!tags[currentTag]) tags[currentTag] = [];
          tags[currentTag].push(tagMatch[2]);
        } else if (currentTag) {
          tags[currentTag][tags[currentTag].length - 1] += ' ' + line.trim();
        } else if (line.trim()) {
          description.push(line.trim());
        }
      }

      docs.push({
        name,
        type,
        description: description.join(' '),
        tags,
        filepath
      });
    }

    return docs;
  } catch (error) {
    console.warn(`Failed to extract JSDoc from ${filepath}:`, error.message);
    return [];
  }
}

/**
 * Generate markdown documentation
 * @param {Array} docs - Documentation entries
 * @param {string} packageName - Package name
 * @returns {string} Markdown content
 */
function generateMarkdown(docs, packageName) {
  let md = `# ${packageName} API Documentation\n\n`;
  md += `Generated: ${new Date().toISOString()}\n\n`;

  // Group by type
  const byType = {
    function: docs.filter(d => d.type === 'function'),
    class: docs.filter(d => d.type === 'class'),
    const: docs.filter(d => d.type === 'const')
  };

  // Functions
  if (byType.function.length > 0) {
    md += '## Functions\n\n';
    for (const doc of byType.function) {
      md += `### ${doc.name}\n\n`;
      if (doc.description) {
        md += `${doc.description}\n\n`;
      }

      if (doc.tags.param) {
        md += '**Parameters:**\n\n';
        doc.tags.param.forEach(param => {
          md += `- ${param}\n`;
        });
        md += '\n';
      }

      if (doc.tags.returns) {
        md += '**Returns:**\n\n';
        md += `${doc.tags.returns.join(', ')}\n\n`;
      }

      if (doc.tags.example) {
        md += '**Example:**\n\n';
        md += '```javascript\n';
        md += doc.tags.example.join('\n');
        md += '\n```\n\n';
      }

      md += '---\n\n';
    }
  }

  // Classes
  if (byType.class.length > 0) {
    md += '## Classes\n\n';
    for (const doc of byType.class) {
      md += `### ${doc.name}\n\n`;
      if (doc.description) {
        md += `${doc.description}\n\n`;
      }
      md += '---\n\n';
    }
  }

  // Constants/Exports
  if (byType.const.length > 0) {
    md += '## Exports\n\n';
    for (const doc of byType.const) {
      md += `### ${doc.name}\n\n`;
      if (doc.description) {
        md += `${doc.description}\n\n`;
      }
      md += '---\n\n';
    }
  }

  return md;
}

/**
 * Main execution
 */
function main() {
  console.log('Generating API documentation...\n');

  const files = findSourceFiles();
  console.log(`Found ${files.length} source files`);

  // Group files by package
  const byPackage = {};
  for (const file of files) {
    const pkgMatch = file.match(/packages\/([^\/]+)/);
    if (pkgMatch) {
      const pkgName = pkgMatch[1];
      if (!byPackage[pkgName]) byPackage[pkgName] = [];
      byPackage[pkgName].push(file);
    }
  }

  // Create docs directory
  const docsDir = './docs/api';
  if (!existsSync(docsDir)) {
    mkdirSync(docsDir, { recursive: true });
  }

  // Generate docs for each package
  let totalDocs = 0;
  for (const [pkgName, pkgFiles] of Object.entries(byPackage)) {
    console.log(`\nProcessing package: ${pkgName}`);

    const allDocs = [];
    for (const file of pkgFiles) {
      const docs = extractJSDoc(file);
      allDocs.push(...docs);
    }

    console.log(`  Extracted ${allDocs.length} documented items`);
    totalDocs += allDocs.length;

    const markdown = generateMarkdown(allDocs, pkgName);
    const outputPath = join(docsDir, `${pkgName}.md`);
    writeFileSync(outputPath, markdown);
    console.log(`  Generated: ${outputPath}`);
  }

  // Generate index
  let indexMd = '# API Documentation Index\n\n';
  indexMd += 'Generated API documentation for all packages.\n\n';
  indexMd += '## Packages\n\n';
  for (const pkgName of Object.keys(byPackage)) {
    indexMd += `- [${pkgName}](./${pkgName}.md)\n`;
  }

  writeFileSync(join(docsDir, 'README.md'), indexMd);

  console.log(`\n✅ API documentation generated`);
  console.log(`   Total: ${totalDocs} documented items`);
  console.log(`   Output: ${docsDir}`);
}

main();
