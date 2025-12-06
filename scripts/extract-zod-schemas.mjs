#!/usr/bin/env node
/**
 * @file Extract Zod schemas from UNRDF packages
 * @module scripts/extract-zod-schemas
 *
 * Extracts Zod schema definitions from all packages and outputs structured JSON
 * for use in API documentation generation.
 *
 * Usage: node scripts/extract-zod-schemas.mjs
 * Output: packages/nextra/data/api-schemas.json
 */

import { readFileSync, writeFileSync, readdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { glob } from 'glob';

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = join(__dirname, '..');

/**
 * Extract schema information from source code
 * @param {string} filePath - Path to source file
 * @returns {Array<Object>} Extracted schema definitions
 */
function extractSchemasFromFile(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const schemas = [];

  // Match: export const XxxSchema = z.object({...})
  const schemaRegex = /export\s+const\s+(\w+Schema)\s*=\s*z\.(object|enum|union|array|string|number|boolean|record)\(/g;

  let match;
  while ((match = schemaRegex.exec(content)) !== null) {
    const [, schemaName, schemaType] = match;
    const startPos = match.index;

    // Find JSDoc comment above schema
    const beforeSchema = content.substring(0, startPos);
    const jsdocMatch = beforeSchema.match(/\/\*\*[\s\S]*?\*\/\s*$/);
    const jsdoc = jsdocMatch ? jsdocMatch[0].replace(/\/\*\*|\*\/|\*/g, '').trim() : '';

    // Extract schema definition (simplified - full parsing would need AST)
    const afterSchema = content.substring(startPos);
    const schemaEndMatch = afterSchema.match(/^[^;]+;/);
    const schemaDef = schemaEndMatch ? schemaEndMatch[0] : '';

    schemas.push({
      name: schemaName,
      type: schemaType,
      jsdoc,
      definition: schemaDef.substring(0, 200), // Truncate for brevity
      sourceFile: filePath.replace(ROOT_DIR, ''),
      line: content.substring(0, startPos).split('\n').length,
    });
  }

  return schemas;
}

/**
 * Main extraction function
 */
async function extractSchemas() {
  const packages = readdirSync(join(ROOT_DIR, 'packages'), { withFileTypes: true })
    .filter((dirent) => dirent.isDirectory())
    .map((dirent) => dirent.name)
    .filter((name) => !name.startsWith('.') && name !== 'nextra');

  const allSchemas = {};

  for (const pkg of packages) {
    const pkgPath = join(ROOT_DIR, 'packages', pkg);
    const srcPath = join(pkgPath, 'src');

    try {
      // Find all .mjs files in src/
      const files = await glob('**/*.mjs', { cwd: srcPath, absolute: true });

      const schemas = [];
      for (const file of files) {
        const fileSchemas = extractSchemasFromFile(file);
        schemas.push(...fileSchemas);
      }

      if (schemas.length > 0) {
        allSchemas[`@unrdf/${pkg}`] = schemas;
      }
    } catch (error) {
      console.warn(`Skipping ${pkg}: ${error.message}`);
    }
  }

  // Write output
  const outputPath = join(ROOT_DIR, 'packages/nextra/data/api-schemas.json');
  writeFileSync(outputPath, JSON.stringify(allSchemas, null, 2));

  console.log(`✅ Extracted schemas from ${Object.keys(allSchemas).length} packages`);
  console.log(`📊 Total schemas: ${Object.values(allSchemas).flat().length}`);
  console.log(`📁 Output: ${outputPath.replace(ROOT_DIR, '')}`);

  return allSchemas;
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  extractSchemas().catch((error) => {
    console.error('❌ Extraction failed:', error);
    process.exit(1);
  });
}

export { extractSchemas };
