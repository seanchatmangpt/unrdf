/**
 * Convention-Preserving Façade Generator
 * @module generator
 */

import { buildFunctionTemplate, buildImport, buildConstant } from './templates.mjs';
import { formatCode } from './formatter.mjs';
import { validateGeneratedCode } from './validator.mjs';

/**
 * Generate façade module matching target conventions
 * @param {CompiledProfile} profile - Target organization conventions
 * @param {CompiledLens} lens - Field mapping transformations
 * @param {ServiceSpec} service - Service to wrap
 * @returns {string} Generated source code (deterministic)
 */
export function generateFacade(profile, lens, service) {
  // STEP 1: Build AST
  const ast = buildFacadeAST(profile, lens, service);

  // STEP 2: Format deterministically
  const code = formatCode(ast, profile);

  // STEP 3: Validate
  const validation = validateGeneratedCode(code, profile);
  if (!validation.ok) {
    throw new Error(`Validation failed: ${validation.violations.join('; ')}`);
  }

  return code;
}

/**
 * Build AST from service spec
 * @param {CompiledProfile} profile - Target conventions
 * @param {CompiledLens} lens - Field transformations
 * @param {ServiceSpec} service - Service specification
 * @returns {Object} Abstract syntax tree
 * @internal
 */
export function buildFacadeAST(profile, lens, service) {
  // Sort operations for determinism
  const sortedOperations = [...service.operations].sort((a, b) =>
    a.name.localeCompare(b.name)
  );

  return {
    header: buildHeader(profile, service),
    imports: buildImports(profile, service, sortedOperations),
    constants: buildConstants(profile, sortedOperations),
    functions: sortedOperations.map(op => buildFunction(profile, lens, op)),
    exports: buildExports(sortedOperations)
  };
}

/**
 * Build file header
 * @param {CompiledProfile} profile - Target conventions
 * @param {ServiceSpec} service - Service specification
 * @returns {Object} Header structure
 */
function buildHeader(profile, service) {
  // Add spaces to camelCase names (e.g., CustomerService -> Customer Service)
  // Skip the first character to avoid leading space
  const formattedName = service.name.replace(/([A-Z])/g, (match, p1, offset) =>
    offset > 0 ? ` ${match}` : match
  ).trim();

  return {
    title: `${formattedName} Façade`,
    description: `Auto-generated from ${formattedName} spec`,
    metadata: `Conventions: ${profile.name || 'target-org'} profile v${profile.version || '1.0.0'}`
  };
}

/**
 * Build import statements
 * @param {CompiledProfile} profile - Target conventions
 * @param {ServiceSpec} service - Service specification
 * @param {Array} operations - Sorted operations
 * @returns {Array} Import structures
 */
export function buildImports(profile, service, operations) {
  const imports = [];

  // Standard imports (sorted)
  imports.push(buildImport('@unrdf/oxigraph', ['createStore', 'dataFactory']));
  imports.push(buildImport('zod', ['z']));

  // Internal imports (sorted alphabetically)
  imports.push(buildImport('./lens.mjs', ['logOperation']));
  imports.push(buildImport('./logging.mjs', ['transformFields']));

  return imports;
}

/**
 * Build constant definitions
 * @param {CompiledProfile} profile - Target conventions
 * @param {Array} operations - Sorted operations
 * @returns {Array} Constant structures
 */
export function buildConstants(profile, operations) {
  const constants = [];

  // Extract unique data types that need schemas
  const dataTypes = new Set();
  for (const op of operations) {
    for (const param of op.params) {
      if (param.type.endsWith('Data')) {
        dataTypes.add(param.type);
      }
    }
  }

  // Build Zod schemas (sorted)
  const sortedTypes = Array.from(dataTypes).sort();
  for (const type of sortedTypes) {
    constants.push({
      name: `${type}Schema`,
      value: buildZodSchema(type)
    });
  }

  return constants;
}

/**
 * Build Zod schema for data type
 * @param {string} type - Data type name
 * @returns {Object} Zod schema definition
 */
function buildZodSchema(type) {
  // For demonstration, return a basic schema
  // In real implementation, this would be derived from the service spec
  if (type === 'CustomerData') {
    return {
      type: 'z.object',
      fields: {
        email: 'z.string().email()',
        name: 'z.string()'
      }
    };
  }

  return {
    type: 'z.object',
    fields: {}
  };
}

/**
 * Build function from operation
 * @param {CompiledProfile} profile - Target conventions
 * @param {CompiledLens} lens - Field transformations
 * @param {Operation} operation - Service operation
 * @returns {Object} Function structure
 */
export function buildFunction(profile, lens, operation) {
  const template = buildFunctionTemplate(operation, profile, lens);

  return {
    jsdoc: template.jsdoc,
    signature: template.signature,
    body: template.body,
    errorHandler: template.errorHandler
  };
}

/**
 * Build export statements
 * @param {Array} operations - Sorted operations
 * @returns {Array} Export structures
 */
export function buildExports(operations) {
  // Functions are exported inline, so no separate exports needed
  return [];
}
