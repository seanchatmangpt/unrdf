/**
 * @file TOML Schema Validation
 * @module @unrdf/chatman-equation/validation/validate-toml
 * @description Validates all TOML configuration files against Zod schemas
 */

import { readFile, readdir } from 'fs/promises';
import { join, extname } from 'path';
import TOML from '@iarna/toml';
import { z } from 'zod';

/**
 * TOML Configuration Schema (Zod)
 * Defines the expected structure for 3T methodology TOML files
 */
const TOMLConfigSchema = z.object({
  metadata: z.object({
    name: z.string().min(1),
    version: z.string().regex(/^\d+\.\d+\.\d+$/),
    description: z.string().optional(),
    author: z.string().optional(),
    created: z.string().datetime().optional(),
  }),
  ontology: z.object({
    namespace: z.string().url(),
    prefix: z.string().min(1),
    imports: z.array(z.string().url()).optional(),
  }).optional(),
  templates: z.object({
    engine: z.literal('tera'),
    variables: z.record(z.unknown()).optional(),
  }).optional(),
  validation: z.object({
    strict: z.boolean().default(true),
    shacl: z.string().optional(),
  }).optional(),
});

/**
 * Deployment Manifest Schema
 */
const DeploymentManifestSchema = z.object({
  deployment: z.object({
    name: z.string(),
    version: z.string(),
    timestamp: z.string().datetime(),
    components: z.array(z.string()),
  }),
  validation: z.object({
    toml_files: z.number().int().nonnegative(),
    tera_templates: z.number().int().nonnegative(),
    turtle_files: z.number().int().nonnegative(),
    all_valid: z.boolean(),
  }),
  receipts: z.object({
    package_creation: z.string(),
    documentation: z.string(),
    ontology: z.string(),
    integration: z.string(),
  }).optional(),
});

/**
 * Find all TOML files in a directory recursively
 * @param {string} dir - Directory to search
 * @returns {Promise<string[]>} Array of TOML file paths
 */
async function findTOMLFiles(dir) {
  const files = [];
  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...await findTOMLFiles(fullPath));
    } else if (entry.isFile() && extname(entry.name) === '.toml') {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * Validate a single TOML file
 * @param {string} filePath - Path to TOML file
 * @returns {Promise<{valid: boolean, errors: string[], data: any}>}
 */
async function validateTOMLFile(filePath) {
  try {
    const content = await readFile(filePath, 'utf-8');
    const parsed = TOML.parse(content);

    // Determine schema based on file name
    const schema = filePath.includes('deployment')
      ? DeploymentManifestSchema
      : TOMLConfigSchema;

    const result = schema.safeParse(parsed);

    if (result.success) {
      return {
        valid: true,
        errors: [],
        data: result.data,
      };
    } else {
      return {
        valid: false,
        errors: result.error.errors.map(e => `${e.path.join('.')}: ${e.message}`),
        data: null,
      };
    }
  } catch (error) {
    return {
      valid: false,
      errors: [`Parse error: ${error.message}`],
      data: null,
    };
  }
}

/**
 * Main validation function
 * @returns {Promise<{success: boolean, results: Array}>}
 */
async function validateAllTOML() {
  // Resolve package root - works whether run from root or package dir
  const packageRoot = process.cwd().endsWith('chatman-equation')
    ? process.cwd()
    : join(process.cwd(), 'packages/chatman-equation');
  const tomlFiles = await findTOMLFiles(packageRoot);

  console.log(`\n🔍 TOML Validation Report`);
  console.log(`${'='.repeat(60)}\n`);
  console.log(`Found ${tomlFiles.length} TOML file(s)\n`);

  const results = [];
  let allValid = true;

  for (const file of tomlFiles) {
    const result = await validateTOMLFile(file);
    const relativePath = file.replace(packageRoot + '/', '');

    results.push({ file: relativePath, ...result });

    if (result.valid) {
      console.log(`✅ ${relativePath}`);
    } else {
      console.log(`❌ ${relativePath}`);
      result.errors.forEach(err => console.log(`   - ${err}`));
      allValid = false;
    }
  }

  console.log(`\n${'='.repeat(60)}`);
  console.log(`\n📊 Summary:`);
  console.log(`   Total files: ${tomlFiles.length}`);
  console.log(`   Valid: ${results.filter(r => r.valid).length}`);
  console.log(`   Invalid: ${results.filter(r => !r.valid).length}`);
  console.log(`   Status: ${allValid ? '✅ ALL PASS' : '❌ FAILURES DETECTED'}\n`);

  return {
    success: allValid,
    results,
    stats: {
      total: tomlFiles.length,
      valid: results.filter(r => r.valid).length,
      invalid: results.filter(r => !r.valid).length,
    },
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await validateAllTOML();
  process.exit(result.success ? 0 : 1);
}

export { validateAllTOML, validateTOMLFile, TOMLConfigSchema, DeploymentManifestSchema };
