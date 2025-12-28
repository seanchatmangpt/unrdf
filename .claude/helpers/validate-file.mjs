#!/usr/bin/env node
/**
 * @file File validation helper for UNRDF
 * @module validate-file
 * @description Validates files against UNRDF coding standards
 */

import { readFileSync, statSync } from 'fs';
import { basename, extname } from 'path';

const MAX_LINES = 500;
const FORBIDDEN_IMPORTS = ["from 'n3'", 'from "n3"'];
const FORBIDDEN_PATTERNS = ['it.skip', 'describe.skip', 'TODO:', 'FIXME:'];

/**
 * Validates a file against UNRDF standards
 * @param {string} filePath - Path to file to validate
 * @returns {{ valid: boolean, errors: string[], warnings: string[] }}
 */
export function validateFile(filePath) {
  const errors = [];
  const warnings = [];

  try {
    const stats = statSync(filePath);
    if (!stats.isFile()) {
      return { valid: false, errors: ['Not a file'], warnings: [] };
    }

    const content = readFileSync(filePath, 'utf-8');
    const lines = content.split('\n');
    const ext = extname(filePath);
    const name = basename(filePath);

    // Check line count
    if (lines.length > MAX_LINES) {
      errors.push(`File exceeds ${MAX_LINES} lines (has ${lines.length})`);
    } else if (lines.length > MAX_LINES * 0.8) {
      warnings.push(`File approaching line limit (${lines.length}/${MAX_LINES})`);
    }

    // Check for .mjs files
    if (ext === '.mjs') {
      // Check forbidden imports
      for (const forbidden of FORBIDDEN_IMPORTS) {
        if (content.includes(forbidden)) {
          errors.push(`Forbidden import: ${forbidden} - use @unrdf/oxigraph`);
        }
      }

      // Check for JSDoc on exports
      const exportLines = lines.filter(l => l.startsWith('export '));
      for (const line of exportLines) {
        const lineIndex = lines.indexOf(line);
        if (lineIndex > 0) {
          const prevLine = lines[lineIndex - 1].trim();
          if (!prevLine.endsWith('*/') && !prevLine.startsWith('//')) {
            warnings.push(`Export without JSDoc at line ${lineIndex + 1}`);
          }
        }
      }
    }

    // Check for test files
    if (name.includes('.test.')) {
      for (const pattern of FORBIDDEN_PATTERNS.slice(0, 2)) {
        if (content.includes(pattern)) {
          errors.push(`Forbidden pattern in test: ${pattern}`);
        }
      }

      // Check for empty test bodies
      const emptyTests = content.match(/it\(['"`].*['"`],\s*\(\)\s*=>\s*\{\s*\}\)/g);
      if (emptyTests) {
        errors.push(`Found ${emptyTests.length} empty test bodies`);
      }
    }

    // Check for TODOs in non-test files
    if (!name.includes('.test.')) {
      const todoCount = (content.match(/TODO:/gi) || []).length;
      if (todoCount > 0) {
        errors.push(`Found ${todoCount} TODO comments`);
      }
    }

    // Check naming convention
    if (ext === '.mjs' && name !== name.toLowerCase()) {
      warnings.push(`File name should be kebab-case: ${name}`);
    }

  } catch (error) {
    errors.push(`Error reading file: ${error.message}`);
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings
  };
}

// CLI interface
if (process.argv[1] === import.meta.url.replace('file://', '')) {
  const filePath = process.argv[2];

  if (!filePath) {
    console.error('Usage: node validate-file.mjs <file-path>');
    process.exit(1);
  }

  const result = validateFile(filePath);

  if (result.errors.length > 0) {
    console.error('ERRORS:');
    result.errors.forEach(e => console.error(`  - ${e}`));
  }

  if (result.warnings.length > 0) {
    console.warn('WARNINGS:');
    result.warnings.forEach(w => console.warn(`  - ${w}`));
  }

  if (result.valid) {
    console.log('File is valid');
    process.exit(0);
  } else {
    process.exit(1);
  }
}
