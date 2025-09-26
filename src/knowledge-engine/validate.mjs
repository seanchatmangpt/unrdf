/**
 * @file SHACL validation.
 * @module validate
 */

import { Parser, Store } from 'n3';
import rdf from 'rdf-ext';
import SHACLValidator from 'rdf-validate-shacl';

/**
 * Validate a store against SHACL shapes.
 * @param {Store} store - The store containing data to validate
 * @param {Store|string} shapes - The store or Turtle string containing SHACL shapes
 * @param {Object} [options] - Validation options
 * @param {boolean} [options.strict] - Enable strict validation mode
 * @param {boolean} [options.includeDetails] - Include detailed validation results
 * @returns {{conforms: boolean, results: Array<object>}} Validation report
 * 
 * @throws {Error} If validation fails
 * 
 * @example
 * const dataStore = new Store();
 * // ... add data quads to store
 * 
 * const shapesTtl = `
 *   @prefix sh: <http://www.w3.org/ns/shacl#> .
 *   @prefix ex: <http://example.org/> .
 *   ex:PersonShape a sh:NodeShape ;
 *     sh:targetClass ex:Person ;
 *     sh:property [
 *       sh:path ex:name ;
 *       sh:minCount 1 ;
 *       sh:maxCount 1
 *     ] .
 * `;
 * 
 * const report = validateShacl(dataStore, shapesTtl);
 * console.log('Conforms:', report.conforms);
 * console.log('Results:', report.results);
 */
export function validateShacl(store, shapes, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('validateShacl: store must be a valid Store instance');
  }
  if (!shapes) {
    throw new TypeError('validateShacl: shapes must be provided');
  }

  try {
    const shapesStore = typeof shapes === 'string'
      ? new Store(new Parser().parse(shapes))
      : shapes;

    if (!shapesStore || typeof shapesStore.getQuads !== 'function') {
      throw new TypeError('validateShacl: shapes must be a valid Store or Turtle string');
    }

    const validator = new SHACLValidator(rdf.dataset(shapesStore.getQuads()));
    const report = validator.validate(rdf.dataset(store.getQuads()));

    const results = (report.results || []).map(r => ({
      message: r.message?.[0]?.value || null,
      path: r.path?.value || null,
      focusNode: r.focusNode?.value || null,
      severity: r.severity?.value || null,
      sourceConstraint: r.sourceConstraint?.value || null,
      sourceConstraintComponent: r.sourceConstraintComponent?.value || null,
      sourceShape: r.sourceShape?.value || null,
      value: r.value?.value || null,
      ...(options.includeDetails && {
        detail: r.detail || null,
        resultPath: r.resultPath?.value || null,
        resultSeverity: r.resultSeverity?.value || null
      })
    }));

    return {
      conforms: report.conforms,
      results,
      ...(options.includeDetails && {
        totalResults: results.length,
        errorCount: results.filter(r => r.severity === 'http://www.w3.org/ns/shacl#Violation').length,
        warningCount: results.filter(r => r.severity === 'http://www.w3.org/ns/shacl#Warning').length,
        infoCount: results.filter(r => r.severity === 'http://www.w3.org/ns/shacl#Info').length
      })
    };
  } catch (error) {
    throw new Error(`SHACL validation failed: ${error.message}`);
  }
}

/**
 * Validate a store against multiple SHACL shape sets.
 * @param {Store} store - The store containing data to validate
 * @param {Array<Store|string>} shapesList - Array of stores or Turtle strings containing SHACL shapes
 * @param {Object} [options] - Validation options
 * @returns {{conforms: boolean, results: Array<object>, shapeResults: Array<object>}} Combined validation report
 * 
 * @throws {Error} If validation fails
 * 
 * @example
 * const shapesList = [
 *   personShapesTtl,
 *   organizationShapesTtl,
 *   contactShapesTtl
 * ];
 * 
 * const report = validateShaclMultiple(store, shapesList);
 * console.log('Overall conforms:', report.conforms);
 * console.log('Shape-specific results:', report.shapeResults);
 */
export function validateShaclMultiple(store, shapesList, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('validateShaclMultiple: store must be a valid Store instance');
  }
  if (!Array.isArray(shapesList) || shapesList.length === 0) {
    throw new TypeError('validateShaclMultiple: shapesList must be a non-empty array');
  }

  try {
    const shapeResults = [];
    let allResults = [];
    let overallConforms = true;

    for (let i = 0; i < shapesList.length; i++) {
      const shapes = shapesList[i];
      const report = validateShacl(store, shapes, options);
      
      shapeResults.push({
        index: i,
        shapes: typeof shapes === 'string' ? 'Turtle string' : 'Store',
        conforms: report.conforms,
        resultCount: report.results.length,
        results: report.results
      });

      allResults = allResults.concat(report.results);
      if (!report.conforms) {
        overallConforms = false;
      }
    }

    return {
      conforms: overallConforms,
      results: allResults,
      shapeResults,
      totalShapes: shapesList.length,
      totalResults: allResults.length
    };
  } catch (error) {
    throw new Error(`Multiple SHACL validation failed: ${error.message}`);
  }
}

/**
 * Create a SHACL validation report in a structured format.
 * @param {Object} validationResult - Result from validateShacl
 * @param {Object} [options] - Formatting options
 * @param {boolean} [options.includeSummary] - Include summary statistics
 * @param {boolean} [options.groupBySeverity] - Group results by severity
 * @returns {Object} Formatted validation report
 * 
 * @example
 * const report = validateShacl(store, shapes);
 * const formatted = formatValidationReport(report, {
 *   includeSummary: true,
 *   groupBySeverity: true
 * });
 */
export function formatValidationReport(validationResult, options = {}) {
  if (!validationResult || typeof validationResult !== 'object') {
    throw new TypeError('formatValidationReport: validationResult must be an object');
  }

  const { results = [], conforms } = validationResult;
  const { includeSummary = false, groupBySeverity = false } = options;

  let formatted = {
    conforms,
    resultCount: results.length
  };

  if (includeSummary) {
    const severityCounts = results.reduce((acc, result) => {
      const severity = result.severity || 'unknown';
      acc[severity] = (acc[severity] || 0) + 1;
      return acc;
    }, {});

    formatted.summary = {
      totalResults: results.length,
      severityCounts,
      hasErrors: severityCounts['http://www.w3.org/ns/shacl#Violation'] > 0,
      hasWarnings: severityCounts['http://www.w3.org/ns/shacl#Warning'] > 0,
      hasInfo: severityCounts['http://www.w3.org/ns/shacl#Info'] > 0
    };
  }

  if (groupBySeverity) {
    formatted.resultsBySeverity = results.reduce((acc, result) => {
      const severity = result.severity || 'unknown';
      if (!acc[severity]) {
        acc[severity] = [];
      }
      acc[severity].push(result);
      return acc;
    }, {});
  } else {
    formatted.results = results;
  }

  return formatted;
}

/**
 * Check if a validation result contains any errors.
 * @param {Object} validationResult - Result from validateShacl
 * @returns {boolean} True if there are any validation errors
 * 
 * @example
 * const report = validateShacl(store, shapes);
 * if (hasValidationErrors(report)) {
 *   console.log('Validation failed with errors');
 * }
 */
export function hasValidationErrors(validationResult) {
  if (!validationResult || typeof validationResult !== 'object') {
    return false;
  }

  const { results = [] } = validationResult;
  return results.some(result => 
    result.severity === 'http://www.w3.org/ns/shacl#Violation'
  );
}

/**
 * Get validation errors from a validation result.
 * @param {Object} validationResult - Result from validateShacl
 * @returns {Array<Object>} Array of validation error objects
 * 
 * @example
 * const report = validateShacl(store, shapes);
 * const errors = getValidationErrors(report);
 * errors.forEach(error => {
 *   console.log(`Error: ${error.message} at ${error.focusNode}`);
 * });
 */
export function getValidationErrors(validationResult) {
  if (!validationResult || typeof validationResult !== 'object') {
    return [];
  }

  const { results = [] } = validationResult;
  return results.filter(result => 
    result.severity === 'http://www.w3.org/ns/shacl#Violation'
  );
}

/**
 * Get validation warnings from a validation result.
 * @param {Object} validationResult - Result from validateShacl
 * @returns {Array<Object>} Array of validation warning objects
 * 
 * @example
 * const report = validateShacl(store, shapes);
 * const warnings = getValidationWarnings(report);
 * warnings.forEach(warning => {
 *   console.log(`Warning: ${warning.message} at ${warning.focusNode}`);
 * });
 */
export function getValidationWarnings(validationResult) {
  if (!validationResult || typeof validationResult !== 'object') {
    return [];
  }

  const { results = [] } = validationResult;
  return results.filter(result => 
    result.severity === 'http://www.w3.org/ns/shacl#Warning'
  );
}
