/**
 * @fileoverview SHACL validation for knowledge hooks
 * @module hooks/validate
 *
 * @description
 * Full SHACL validation using rdf-validate-shacl against Oxigraph-backed RDF stores.
 * Supports minCount, maxCount, datatype, and other SHACL Core constraints.
 * Caches parsed shape validators for performance.
 */

import SHACLValidator from 'rdf-validate-shacl';
import oxigraph from 'oxigraph';

const SHACL_NS = 'http://www.w3.org/ns/shacl#';

// Severity URI to short string mapping
const SEVERITY_MAP = {
  [`${SHACL_NS}Violation`]: 'violation',
  [`${SHACL_NS}Warning`]: 'warning',
  [`${SHACL_NS}Info`]: 'info',
};

// Cache parsed SHACLValidator instances keyed by shapes string
const validatorCache = new Map();
const CACHE_MAX_SIZE = 50;

/**
 * Get or create a cached SHACLValidator for the given shapes Turtle string.
 *
 * @param {string} shapesString - SHACL shapes as Turtle
 * @returns {SHACLValidator} Cached validator instance
 */
function getValidator(shapesString) {
  let validator = validatorCache.get(shapesString);
  if (validator) {
    return validator;
  }

  const shapesStore = new oxigraph.Store();
  shapesStore.load(shapesString, { format: 'text/turtle' });
  const shapesQuads = shapesStore.match(null, null, null, null);

  validator = new SHACLValidator(shapesQuads);

  // Evict oldest entry if cache is full
  if (validatorCache.size >= CACHE_MAX_SIZE) {
    const firstKey = validatorCache.keys().next().value;
    validatorCache.delete(firstKey);
  }
  validatorCache.set(shapesString, validator);

  return validator;
}

/**
 * Validate RDF data against SHACL shapes.
 *
 * @param {object} dataStore - RDF store containing data to validate (OxigraphStore or raw oxigraph.Store)
 * @param {string} shapesString - SHACL shapes as Turtle string
 * @param {object} options - Validation options
 * @param {boolean} options.strict - Enable strict validation mode
 * @param {boolean} options.includeDetails - Include validation details in report
 * @returns {Promise<object>} SHACL validation report
 */
export async function validateShacl(dataStore, shapesString, options = {}) {
  const { includeDetails = true } = options;

  try {
    if (!dataStore || typeof dataStore.size !== 'number') {
      throw new Error('Invalid data store');
    }
    if (!shapesString || typeof shapesString !== 'string') {
      throw new Error('Invalid shapes string');
    }

    const validator = getValidator(shapesString);

    // Use the raw oxigraph.Store for validation (unwrap OxigraphStore wrapper if needed)
    const rawStore = dataStore.store || dataStore;

    const shaclReport = await validator.validate(rawStore);

    const results = (shaclReport.results || []).map(result => ({
      severity: SEVERITY_MAP[result.severity?.value] || 'violation',
      message: result.message?.map(m => m.value).join('; ') || null,
      focusNode: result.focusNode?.value || null,
      resultPath: result.path?.value || null,
      resultMessage: result.message?.map(m => m.value).join('; ') || null,
      value: result.value?.value || null,
      sourceConstraintComponent: result.sourceConstraintComponent?.value || null,
      sourceShape: result.sourceShape?.value || null,
    }));

    const report = {
      conforms: shaclReport.conforms,
      results,
      timestamp: new Date().toISOString(),
    };

    if (includeDetails) {
      report.details = {
        shapesCount: countShapes(shapesString),
        constraintsChecked: results.length,
        validationTime: 0,
      };
    }

    return report;
  } catch (error) {
    return {
      conforms: false,
      results: [
        {
          severity: 'violation',
          message: `SHACL validation error: ${error.message}`,
          focusNode: null,
          resultPath: null,
          resultMessage: `SHACL validation error: ${error.message}`,
          value: null,
        },
      ],
      error: error.message,
      timestamp: new Date().toISOString(),
    };
  }
}

/**
 * Count the number of sh:NodeShape declarations in a shapes string.
 *
 * @param {string} shapesString - Turtle shapes string
 * @returns {number} Approximate number of shapes
 */
function countShapes(shapesString) {
  const matches = shapesString.match(/sh:NodeShape/g);
  return matches ? matches.length : 0;
}

/**
 * Validate a single node against SHACL shapes
 *
 * @param {object} store - RDF store
 * @param {object} node - Node to validate
 * @param {string} shapesString - SHACL shapes
 * @param {object} options - Validation options
 * @returns {Promise<object>} Validation report for the node
 */
export async function validateNode(store, node, shapesString, options = {}) {
  const report = await validateShacl(store, shapesString, options);

  return {
    ...report,
    node: node?.value || String(node),
  };
}

/**
 * Check if validation report indicates conformance
 *
 * @param {object} report - SHACL validation report
 * @returns {boolean} True if data conforms to shapes
 */
export function isConforming(report) {
  return report && report.conforms === true;
}

/**
 * Get violations from validation report
 *
 * @param {object} report - SHACL validation report
 * @returns {Array} Array of violations
 */
export function getViolations(report) {
  if (!report || !Array.isArray(report.results)) {
    return [];
  }

  return report.results.filter(result => result.severity === 'violation');
}

/**
 * Get warnings from validation report
 *
 * @param {object} report - SHACL validation report
 * @returns {Array} Array of warnings
 */
export function getWarnings(report) {
  if (!report || !Array.isArray(report.results)) {
    return [];
  }

  return report.results.filter(result => result.severity === 'warning');
}

/**
 * Clear the validator cache. Useful in tests or when shapes change frequently.
 */
export function clearValidatorCache() {
  validatorCache.clear();
}
