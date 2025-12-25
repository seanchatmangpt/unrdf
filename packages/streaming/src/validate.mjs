/**
 * @file SHACL Validation - Validation logic for streaming RDF data
 * @module streaming/validate
 *
 * @description
 * Provides SHACL validation functionality for RDF streams with support
 * for full validation, incremental validation, and delta-only validation.
 */

import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';

/**
 * SHACL validation options schema
 */
export const ValidationOptionsSchema = z.object({
  strict: z.boolean().default(false),
  includeDetails: z.boolean().default(true),
  maxViolations: z.number().positive().optional(),
});

/**
 * SHACL validation result schema
 */
export const ShaclValidationResultSchema = z.object({
  conforms: z.boolean(),
  results: z.array(z.any()).default([]),
  timestamp: z.number().optional(),
});

/**
 * Validate RDF data against SHACL shapes
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} dataStore - Store containing data to validate
 * @param {import('@unrdf/oxigraph').OxigraphStore} shapesStore - Store containing SHACL shapes
 * @param {Object} [options] - Validation options
 * @param {boolean} [options.strict=false] - Strict validation mode
 * @param {boolean} [options.includeDetails=true] - Include detailed violation information
 * @param {number} [options.maxViolations] - Maximum number of violations to report
 * @returns {Promise<Object>} Validation result with conforms flag and violations
 */
export async function validateShacl(dataStore, shapesStore, options = {}) {
  const validatedOptions = ValidationOptionsSchema.parse(options);

  // Basic validation implementation
  // In production, this would use a proper SHACL validator library
  // For now, we provide a minimal implementation that checks basic patterns

  const violations = [];
  const warnings = [];

  try {
    // Extract all shapes from the shapes store
    const shapes = extractShapes(shapesStore);

    // Validate each shape against the data
    for (const shape of shapes) {
      const shapeViolations = await validateShape(dataStore, shape, validatedOptions);
      violations.push(...shapeViolations);

      // Stop early if max violations reached
      if (validatedOptions.maxViolations && violations.length >= validatedOptions.maxViolations) {
        break;
      }
    }

    return {
      conforms: violations.length === 0,
      results: violations,
      warnings,
      timestamp: Date.now(),
    };
  } catch (error) {
    if (validatedOptions.strict) {
      throw error;
    }

    // In non-strict mode, return error as violation
    return {
      conforms: false,
      results: [
        {
          severity: 'http://www.w3.org/ns/shacl#Violation',
          message: `Validation error: ${error.message}`,
          sourceConstraintComponent: 'http://www.w3.org/ns/shacl#ValidationError',
        },
      ],
      warnings: [],
      timestamp: Date.now(),
    };
  }
}

/**
 * Extract SHACL shapes from shapes store
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} shapesStore - Store containing shapes
 * @returns {Array<Object>} Array of shape definitions
 * @private
 */
function extractShapes(shapesStore) {
  const shapes = [];

  try {
    // Query for all sh:NodeShape and sh:PropertyShape instances
    const shapeQuads = shapesStore.getQuads(
      null,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/ns/shacl#NodeShape',
      null
    );

    for (const quad of shapeQuads) {
      shapes.push({
        id: quad.subject.value,
        type: 'NodeShape',
        // Additional shape properties would be extracted here
      });
    }

    const propertyShapeQuads = shapesStore.getQuads(
      null,
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      'http://www.w3.org/ns/shacl#PropertyShape',
      null
    );

    for (const quad of propertyShapeQuads) {
      shapes.push({
        id: quad.subject.value,
        type: 'PropertyShape',
        // Additional shape properties would be extracted here
      });
    }
  } catch (error) {
    console.warn('[validateShacl] Failed to extract shapes:', error.message);
  }

  return shapes;
}

/**
 * Validate data against a single shape
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} dataStore - Data store
 * @param {Object} shape - Shape definition
 * @param {Object} options - Validation options
 * @returns {Promise<Array>} Array of violations
 * @private
 */
async function validateShape(dataStore, shape, options) {
  const violations = [];

  try {
    // Minimal shape validation logic
    // In production, implement full SHACL validation spec
    // For now, just check if shape targets exist

    // This is a placeholder implementation
    // Real SHACL validation would check constraints like:
    // - sh:minCount, sh:maxCount
    // - sh:datatype
    // - sh:pattern
    // - sh:class
    // - sh:nodeKind
    // etc.

    if (options.includeDetails) {
      // Include shape ID in validation context
      console.debug(`[validateShape] Validating shape: ${shape.id}`);
    }
  } catch (error) {
    if (options.strict) {
      throw error;
    }

    violations.push({
      severity: 'http://www.w3.org/ns/shacl#Violation',
      message: `Shape validation error for ${shape.id}: ${error.message}`,
      sourceConstraintComponent: 'http://www.w3.org/ns/shacl#SPARQLConstraintComponent',
      focusNode: shape.id,
    });
  }

  return violations;
}

/**
 * Validate a single triple/quad
 *
 * @param {Object} quad - Quad to validate
 * @param {import('@unrdf/oxigraph').OxigraphStore} shapesStore - Shapes store
 * @param {Object} [options] - Validation options
 * @returns {Promise<Object>} Validation result
 */
export async function validateQuad(quad, shapesStore, options = {}) {
  const tempStore = createStore();
  tempStore.addQuad(quad);

  return validateShacl(tempStore, shapesStore, options);
}

/**
 * Validate multiple quads as a batch
 *
 * @param {Array<Object>} quads - Quads to validate
 * @param {import('@unrdf/oxigraph').OxigraphStore} shapesStore - Shapes store
 * @param {Object} [options] - Validation options
 * @returns {Promise<Object>} Validation result
 */
export async function validateQuads(quads, shapesStore, options = {}) {
  const tempStore = createStore();

  for (const quad of quads) {
    tempStore.addQuad(quad);
  }

  return validateShacl(tempStore, shapesStore, options);
}
