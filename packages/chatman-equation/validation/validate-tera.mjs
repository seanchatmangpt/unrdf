/**
 * @file Tera Template Validation
 * @module @unrdf/chatman-equation/validation/validate-tera
 * @description Validates all Tera templates can render without errors
 */

import { readFile, readdir } from 'fs/promises';
import { join, extname } from 'path';
import { z } from 'zod';

/**
 * Template Variable Schema
 */
const TemplateVariablesSchema = z.record(z.unknown());

/**
 * Simple Tera-like template renderer for validation
 * NOTE: This is a minimal implementation for validation purposes
 * Production should use actual Tera engine or equivalent
 * @param {string} template - Template string
 * @param {Object} context - Variables to interpolate
 * @returns {string} Rendered template
 */
function renderTemplate(template, context = {}) {
  let result = template;

  // Replace {{ variable }} patterns
  result = result.replace(/\{\{\s*([a-zA-Z0-9_\.]+)\s*\}\}/g, (match, path) => {
    const value = getNestedValue(context, path);
    return value !== undefined ? String(value) : match;
  });

  // Replace {% if condition %} blocks (simple)
  result = result.replace(/\{%\s*if\s+([a-zA-Z0-9_\.]+)\s*%\}([\s\S]*?)\{%\s*endif\s*%\}/g, (match, condition, content) => {
    const value = getNestedValue(context, condition);
    return value ? content : '';
  });

  // Replace {% for item in list %} blocks (simple)
  result = result.replace(/\{%\s*for\s+(\w+)\s+in\s+(\w+)\s*%\}([\s\S]*?)\{%\s*endfor\s*%\}/g, (match, itemName, listName, content) => {
    const list = context[listName];
    if (!Array.isArray(list)) return '';

    return list.map(item => {
      const itemContext = { ...context, [itemName]: item };
      return renderTemplate(content, itemContext);
    }).join('');
  });

  return result;
}

/**
 * Get nested value from object using dot notation
 * @param {Object} obj - Source object
 * @param {string} path - Dot-separated path
 * @returns {any} Value at path
 */
function getNestedValue(obj, path) {
  const parts = path.split('.');
  let current = obj;

  for (const part of parts) {
    if (current === null || current === undefined) {
      return undefined;
    }
    current = current[part];
  }

  return current;
}

/**
 * Find all Tera template files in a directory recursively
 * @param {string} dir - Directory to search
 * @returns {Promise<string[]>} Array of template file paths
 */
async function findTeraFiles(dir) {
  const files = [];
  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...await findTeraFiles(fullPath));
    } else if (entry.isFile() && extname(entry.name) === '.tera') {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * Validate a single Tera template
 * @param {string} filePath - Path to template file
 * @param {Object} [testContext] - Optional test context for rendering
 * @returns {Promise<{valid: boolean, errors: string[], rendered: string}>}
 */
async function validateTeraFile(filePath, testContext = {}) {
  try {
    const template = await readFile(filePath, 'utf-8');

    // Basic syntax validation
    const syntaxErrors = [];

    // Check for unclosed tags
    const openTags = (template.match(/\{[{%]/g) || []).length;
    const closeTags = (template.match(/[}%]\}/g) || []).length;
    if (openTags !== closeTags) {
      syntaxErrors.push('Unbalanced template tags');
    }

    // Check for unclosed blocks
    const ifBlocks = (template.match(/\{%\s*if\s+/g) || []).length;
    const endifBlocks = (template.match(/\{%\s*endif\s*%\}/g) || []).length;
    if (ifBlocks !== endifBlocks) {
      syntaxErrors.push('Unbalanced if/endif blocks');
    }

    const forBlocks = (template.match(/\{%\s*for\s+/g) || []).length;
    const endforBlocks = (template.match(/\{%\s*endfor\s*%\}/g) || []).length;
    if (forBlocks !== endforBlocks) {
      syntaxErrors.push('Unbalanced for/endfor blocks');
    }

    if (syntaxErrors.length > 0) {
      return {
        valid: false,
        errors: syntaxErrors,
        rendered: null,
      };
    }

    // Attempt to render with test context
    const rendered = renderTemplate(template, testContext);

    return {
      valid: true,
      errors: [],
      rendered,
    };
  } catch (error) {
    return {
      valid: false,
      errors: [`Render error: ${error.message}`],
      rendered: null,
    };
  }
}

/**
 * Main validation function
 * @returns {Promise<{success: boolean, results: Array}>}
 */
async function validateAllTera() {
  // Resolve package root - works whether run from root or package dir
  const packageRoot = process.cwd().endsWith('chatman-equation')
    ? process.cwd()
    : join(process.cwd(), 'packages/chatman-equation');
  const teraFiles = await findTeraFiles(packageRoot);

  console.log(`\n🎨 Tera Template Validation Report`);
  console.log(`${'='.repeat(60)}\n`);
  console.log(`Found ${teraFiles.length} Tera template(s)\n`);

  const results = [];
  let allValid = true;

  // Default test context for rendering
  const testContext = {
    name: 'Test',
    version: '1.0.0',
    items: ['item1', 'item2', 'item3'],
    enabled: true,
    metadata: {
      author: 'Test Author',
      description: 'Test Description',
    },
  };

  for (const file of teraFiles) {
    const result = await validateTeraFile(file, testContext);
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
  console.log(`   Total templates: ${teraFiles.length}`);
  console.log(`   Valid: ${results.filter(r => r.valid).length}`);
  console.log(`   Invalid: ${results.filter(r => !r.valid).length}`);
  console.log(`   Status: ${allValid ? '✅ ALL PASS' : '❌ FAILURES DETECTED'}\n`);

  return {
    success: allValid,
    results,
    stats: {
      total: teraFiles.length,
      valid: results.filter(r => r.valid).length,
      invalid: results.filter(r => !r.valid).length,
    },
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await validateAllTera();
  process.exit(result.success ? 0 : 1);
}

export { validateAllTera, validateTeraFile, renderTemplate };
