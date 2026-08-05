/**
 * @file SHACL validation for RDF streams.
 *
 * Compiles SHACL Core shapes once, validates complete datasets or affected
 * focus nodes for a delta, and returns deterministic violation structures.
 */

import { z } from 'zod';
import { createStore } from '@unrdf/oxigraph';
import {
  compileShacl,
  validateCompiledShacl,
  validateShaclCore,
  validateShaclDelta,
  affectedFocusNodes,
  evaluatePath,
  readRdfList,
  SH,
  RDF,
  XSD,
} from './shacl-core.mjs';

export const ValidationOptionsSchema = z.object({
  strict: z.boolean().default(false),
  includeDetails: z.boolean().default(true),
  maxViolations: z.number().int().positive().optional(),
  maxDepth: z.number().int().positive().default(64),
  maxNodes: z.number().int().positive().default(100_000),
  timestamp: z.number().optional(),
});

export const ShaclValidationResultSchema = z.object({
  conforms: z.boolean(),
  results: z.array(z.any()).default([]),
  warnings: z.array(z.any()).default([]),
  checkedShapes: z.number().int().nonnegative().default(0),
  checkedFocusNodes: z.number().int().nonnegative().default(0),
  timestamp: z.number(),
});

function normalizeResult(result, options) {
  const normalized = {
    ...result,
    warnings: result.warnings || [],
    timestamp: options.timestamp ?? Date.now(),
  };
  if (!options.includeDetails) {
    normalized.results = normalized.results.map(({ severity, sourceConstraintComponent, focusNode, resultPath }) => ({
      severity,
      sourceConstraintComponent,
      focusNode,
      resultPath,
    }));
  }
  return ShaclValidationResultSchema.parse(normalized);
}

/** Compile a shapes graph into an immutable validation plan. */
export function compileShapes(shapesStore) {
  return compileShacl(shapesStore);
}

/** Validate an RDF dataset against SHACL Core shapes. */
export async function validateShacl(dataStore, shapesStoreOrCompiled, options = {}) {
  const validatedOptions = ValidationOptionsSchema.parse(options);
  try {
    const result = shapesStoreOrCompiled?.shapesById
      ? validateCompiledShacl(dataStore, shapesStoreOrCompiled, validatedOptions)
      : validateShaclCore(dataStore, shapesStoreOrCompiled, validatedOptions);
    return normalizeResult(result, validatedOptions);
  } catch (error) {
    if (validatedOptions.strict) throw error;
    return normalizeResult({
      conforms: false,
      checkedShapes: 0,
      checkedFocusNodes: 0,
      warnings: [{ code: 'SHACL_VALIDATION_ERROR', message: error.message }],
      results: [{
        severity: SH.Violation,
        sourceConstraintComponent: 'http://www.w3.org/ns/shacl#SPARQLConstraintComponent',
        focusNode: null,
        resultPath: null,
        value: null,
        message: error.message,
      }],
    }, validatedOptions);
  }
}

/** Validate only focus nodes affected by additions/deletions in a stream delta. */
export async function validateDelta(dataStore, shapesStoreOrCompiled, delta, options = {}) {
  const validatedOptions = ValidationOptionsSchema.parse(options);
  try {
    const result = validateShaclDelta(dataStore, shapesStoreOrCompiled, delta, validatedOptions);
    return normalizeResult(result, validatedOptions);
  } catch (error) {
    if (validatedOptions.strict) throw error;
    return normalizeResult({
      conforms: false,
      checkedShapes: 0,
      checkedFocusNodes: 0,
      warnings: [{ code: 'SHACL_DELTA_VALIDATION_ERROR', message: error.message }],
      results: [{
        severity: SH.Violation,
        sourceConstraintComponent: 'http://www.w3.org/ns/shacl#SPARQLConstraintComponent',
        focusNode: null,
        resultPath: null,
        value: null,
        message: error.message,
      }],
    }, validatedOptions);
  }
}

/** Validate a single RDF/JS quad. */
export async function validateQuad(quad, shapesStoreOrCompiled, options = {}) {
  if (!quad?.subject || !quad?.predicate || !quad?.object) throw new TypeError('validateQuad requires an RDF/JS quad');
  const tempStore = createStore();
  tempStore.addQuad(quad);
  return validateShacl(tempStore, shapesStoreOrCompiled, options);
}

/** Validate an iterable of RDF/JS quads as one dataset. */
export async function validateQuads(quads, shapesStoreOrCompiled, options = {}) {
  const tempStore = createStore();
  let count = 0;
  for (const quad of quads || []) {
    if (!quad?.subject || !quad?.predicate || !quad?.object) throw new TypeError(`Invalid RDF/JS quad at index ${count}`);
    tempStore.addQuad(quad);
    count += 1;
  }
  return validateShacl(tempStore, shapesStoreOrCompiled, options);
}

export {
  compileShacl,
  validateCompiledShacl,
  validateShaclCore,
  validateShaclDelta,
  affectedFocusNodes,
  evaluatePath,
  readRdfList,
  SH,
  RDF,
  XSD,
};
