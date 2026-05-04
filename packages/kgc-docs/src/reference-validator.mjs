/**
 * @file Cross-Document Reference Validator
 * @module kgc-docs/reference-validator
 *
 * Validate cross-document references in KGC Markdown
 */

import { readFileSync, existsSync } from 'node:fs';
import { resolve, dirname, join } from 'node:path';
import { parseCrossReferences, buildAST } from './parser.mjs';

/**
 * Validate a single cross-document reference
 * @param {object} reference - Reference object with path and anchor
 * @param {string} sourceFile - Source file path
 * @returns {object} Validation result
 */
export function validateReference(reference, sourceFile) {
  const sourceDir = dirname(sourceFile);
  const targetPath = resolve(sourceDir, reference.path);

  // Check if target file exists
  if (!existsSync(targetPath)) {
    return {
      valid: false,
      reference,
      error: `Target file not found: ${reference.path}`,
    };
  }

  // If anchor specified, check if it exists in target
  if (reference.anchor) {
    try {
      const targetContent = readFileSync(targetPath, 'utf-8');
      const targetAST = buildAST(targetContent);

      // Find heading with matching anchor
      const anchorId = reference.anchor.toLowerCase().replace(/\s+/g, '-');
      const headingExists = targetAST.blocks.some((block) => {
        if (block.type === 'heading') {
          const headingId = block.content.toLowerCase().replace(/\s+/g, '-');
          return headingId === anchorId;
        }
        return false;
      });

      if (!headingExists) {
        return {
          valid: false,
          reference,
          error: `Anchor not found in target: #${reference.anchor}`,
        };
      }
    } catch (err) {
      return {
        valid: false,
        reference,
        error: `Failed to parse target file: ${err.message}`,
      };
    }
  }

  return {
    valid: true,
    reference,
  };
}

/**
 * Validate all cross-references in a document
 * @param {string} filePath - Path to KGC Markdown file
 * @returns {object} Validation results
 */
export function validateAllReferences(filePath) {
  if (!existsSync(filePath)) {
    throw new Error(`File not found: ${filePath}`);
  }

  const content = readFileSync(filePath, 'utf-8');
  const ast = buildAST(content);
  const references = ast.crossReferences || [];

  const results = {
    total: references.length,
    valid: 0,
    invalid: 0,
    errors: [],
  };

  for (const ref of references) {
    const validation = validateReference(ref, filePath);
    if (validation.valid) {
      results.valid++;
    } else {
      results.invalid++;
      results.errors.push(validation);
    }
  }

  return results;
}

/**
 * Build reference map for a document
 * @param {string} filePath - Path to KGC Markdown file
 * @returns {object} Reference map with incoming and outgoing links
 */
export function buildReferenceMap(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const ast = buildAST(content);
  const references = ast.crossReferences || [];

  return {
    file: filePath,
    outgoing: references.map((ref) => ({
      target: ref.path,
      anchor: ref.anchor,
      text: ref.text,
    })),
    metadata: {
      total_outgoing: references.length,
      has_anchors: references.some((ref) => ref.anchor !== null),
    },
  };
}

/**
 * Create manifest with all cross-document references
 * @param {Array<string>} filePaths - Array of KGC Markdown file paths
 * @returns {object} Reference manifest
 */
export function createReferenceManifest(filePaths) {
  const manifest = {
    files: [],
    total_references: 0,
    validation_timestamp: new Date().toISOString(),
  };

  for (const filePath of filePaths) {
    try {
      const map = buildReferenceMap(filePath);
      manifest.files.push(map);
      manifest.total_references += map.metadata.total_outgoing;
    } catch (err) {
      manifest.files.push({
        file: filePath,
        error: err.message,
      });
    }
  }

  return manifest;
}
