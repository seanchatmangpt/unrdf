/**
 * @file TOML Configuration Loader
 * @module @unrdf/chatman-equation/loader
 * @description
 * Loads and validates TOML configuration files for Chatman Equation framework.
 * Converts TOML to validated JavaScript objects using Zod schemas.
 */

import { readFileSync } from 'fs';
import TOML from '@iarna/toml';
import {
  ObservationSchema,
  DeltaSchema,
  ClosureOperatorSchema,
  ArtifactSchema,
  UnificationMappingSchema,
  ChatmanExampleSchema,
  safeValidate,
} from './schemas.mjs';

/**
 * Load TOML file and parse
 *
 * @param {string} filePath - Path to TOML file
 * @returns {Object} Parsed TOML object
 * @throws {Error} If file cannot be read or parsed
 */
export function loadToml(filePath) {
  try {
    const content = readFileSync(filePath, 'utf-8');
    return TOML.parse(content);
  } catch (error) {
    throw new Error(`Failed to load TOML file ${filePath}: ${error.message}`);
  }
}

/**
 * Load Equation Schema from TOML
 *
 * @param {string} [filePath] - Path to equation.schema.toml
 * @returns {Object} Parsed schema configuration
 * @throws {Error} If schema file cannot be loaded
 *
 * @example
 * const schema = loadEquationSchema();
 * console.log(schema.metadata.version);
 */
export function loadEquationSchema(filePath = '../config/equation.schema.toml') {
  return loadToml(filePath);
}

/**
 * Load Examples from TOML
 *
 * @param {string} [filePath] - Path to examples.toml
 * @returns {Object} Parsed examples configuration
 * @throws {Error} If examples file cannot be loaded
 *
 * @example
 * const examples = loadExamples();
 * console.log(examples.market_equilibrium);
 */
export function loadExamples(filePath = '../config/examples.toml') {
  return loadToml(filePath);
}

/**
 * Extract and validate observation from example
 *
 * @param {Object} example - Example object from TOML
 * @returns {{ success: boolean, data?: Object, error?: z.ZodError }} Validation result
 *
 * @example
 * const examples = loadExamples();
 * const result = extractObservation(examples.market_equilibrium);
 * if (result.success) {
 *   console.log('Valid observation:', result.data);
 * }
 */
export function extractObservation(example) {
  if (!example.observation) {
    return { success: false, error: new Error('No observation in example') };
  }
  return safeValidate(ObservationSchema, example.observation);
}

/**
 * Extract and validate delta from example
 *
 * @param {Object} example - Example object from TOML
 * @returns {{ success: boolean, data?: Object, error?: z.ZodError }} Validation result
 *
 * @example
 * const examples = loadExamples();
 * const result = extractDelta(examples.market_equilibrium);
 * if (result.success) {
 *   console.log('Valid delta:', result.data);
 * }
 */
export function extractDelta(example) {
  if (!example.delta) {
    return { success: false, error: new Error('No delta in example') };
  }
  return safeValidate(DeltaSchema, example.delta);
}

/**
 * Extract and validate closure operator from example
 *
 * @param {Object} example - Example object from TOML
 * @returns {{ success: boolean, data?: Object, error?: z.ZodError }} Validation result
 *
 * @example
 * const examples = loadExamples();
 * const result = extractClosureOperator(examples.market_equilibrium);
 * if (result.success) {
 *   console.log('Valid operator:', result.data);
 * }
 */
export function extractClosureOperator(example) {
  if (!example.closure_operator) {
    return { success: false, error: new Error('No closure_operator in example') };
  }
  return safeValidate(ClosureOperatorSchema, example.closure_operator);
}

/**
 * Extract and validate expected artifact from example
 *
 * @param {Object} example - Example object from TOML
 * @returns {{ success: boolean, data?: Object, error?: z.ZodError }} Validation result
 *
 * @example
 * const examples = loadExamples();
 * const result = extractArtifact(examples.market_equilibrium);
 * if (result.success) {
 *   console.log('Valid artifact:', result.data);
 * }
 */
export function extractArtifact(example) {
  if (!example.expected_artifact) {
    return { success: false, error: new Error('No expected_artifact in example') };
  }
  return safeValidate(ArtifactSchema, example.expected_artifact);
}

/**
 * Validate complete example
 *
 * @param {Object} example - Example object from TOML
 * @returns {{ success: boolean, data?: Object, errors: Object[] }} Validation result with all component errors
 *
 * @example
 * const examples = loadExamples();
 * const result = validateExample(examples.market_equilibrium);
 * if (result.success) {
 *   console.log('All components valid');
 * } else {
 *   console.error('Validation errors:', result.errors);
 * }
 */
export function validateExample(example) {
  const errors = [];
  const validated = {};

  const obsResult = extractObservation(example);
  if (!obsResult.success) {
    errors.push({ component: 'observation', error: obsResult.error });
  } else {
    validated.observation = obsResult.data;
  }

  const deltaResult = extractDelta(example);
  if (!deltaResult.success) {
    errors.push({ component: 'delta', error: deltaResult.error });
  } else {
    validated.delta = deltaResult.data;
  }

  const operatorResult = extractClosureOperator(example);
  if (!operatorResult.success) {
    errors.push({ component: 'closure_operator', error: operatorResult.error });
  } else {
    validated.closure_operator = operatorResult.data;
  }

  const artifactResult = extractArtifact(example);
  if (!artifactResult.success) {
    errors.push({ component: 'expected_artifact', error: artifactResult.error });
  } else {
    validated.expected_artifact = artifactResult.data;
  }

  return {
    success: errors.length === 0,
    data: errors.length === 0 ? validated : undefined,
    errors,
  };
}

/**
 * Extract unification mapping for domain
 *
 * @param {Object} schema - Schema object from equation.schema.toml
 * @param {string} domain - Domain name (e.g., 'market', 'organization')
 * @returns {{ success: boolean, data?: Object, error?: z.ZodError }} Validation result
 *
 * @example
 * const schema = loadEquationSchema();
 * const result = extractUnificationMapping(schema, 'market');
 * if (result.success) {
 *   console.log('Market mapping:', result.data);
 * }
 */
export function extractUnificationMapping(schema, domain) {
  if (!schema.unification || !schema.unification[domain]) {
    return {
      success: false,
      error: new Error(`No unification mapping for domain: ${domain}`),
    };
  }
  return safeValidate(UnificationMappingSchema, schema.unification[domain]);
}

/**
 * Load all examples and validate
 *
 * @param {string} [filePath] - Path to examples.toml
 * @returns {Object} Map of example name to validation result
 *
 * @example
 * const results = loadAndValidateExamples();
 * for (const [name, result] of Object.entries(results)) {
 *   console.log(`${name}: ${result.success ? 'VALID' : 'INVALID'}`);
 * }
 */
export function loadAndValidateExamples(filePath) {
  const examples = loadExamples(filePath);
  const results = {};

  for (const [name, example] of Object.entries(examples)) {
    if (typeof example === 'object' && example.observation) {
      results[name] = validateExample(example);
    }
  }

  return results;
}

/**
 * Get all unification mappings
 *
 * @param {string} [filePath] - Path to equation.schema.toml
 * @returns {Object} Map of domain to unification mapping
 *
 * @example
 * const mappings = getAllUnificationMappings();
 * console.log(Object.keys(mappings)); // ['market', 'organization', ...]
 */
export function getAllUnificationMappings(filePath) {
  const schema = loadEquationSchema(filePath);
  const mappings = {};

  if (schema.unification) {
    for (const [domain, mapping] of Object.entries(schema.unification)) {
      const result = safeValidate(UnificationMappingSchema, mapping);
      if (result.success) {
        mappings[domain] = result.data;
      }
    }
  }

  return mappings;
}
