/**
 * Code templates for façade generation
 * @module templates
 */

/**
 * Build JSDoc block for a function
 * @param {Operation} operation - Service operation
 * @param {CompiledProfile} profile - Target conventions
 * @returns {Object} JSDoc structure
 */
export function buildJSDoc(operation, profile) {
  const fields = [];

  // Description field
  fields.push({ tag: 'description', value: operation.description });

  // Parameters (sorted by name for determinism)
  const sortedParams = [...operation.params].sort((a, b) => a.name.localeCompare(b.name));
  for (const param of sortedParams) {
    fields.push({
      tag: 'param',
      value: `{${param.type}} ${param.name} - ${param.desc}`
    });
  }

  // Returns
  fields.push({
    tag: 'returns',
    value: `{${operation.returns.type}} ${operation.returns.desc}`
  });

  // Throws (if profile requires it)
  const conventions = profile.conventions || {};
  const jsdocConv = conventions.jsdoc || {};
  if (jsdocConv.includeThrows !== false) {
    // Determine error type and message based on operation
    const opNameLower = operation.name.toLowerCase();
    let errorClass = conventions.errorClass || 'ValidationError';
    let errorMsg = 'If data is invalid';

    if (opNameLower.startsWith('get') || opNameLower.startsWith('find') || opNameLower.startsWith('retrieve')) {
      errorClass = 'NotFoundError';
      errorMsg = `If ${operation.returns.type.toLowerCase()} does not exist`;
    } else if (opNameLower.startsWith('create')) {
      errorClass = 'ValidationError';
      errorMsg = 'If data is invalid';
    } else if (opNameLower.startsWith('update')) {
      errorClass = 'ValidationError';
      errorMsg = 'If data is invalid';
    } else if (opNameLower.startsWith('delete')) {
      errorClass = 'NotFoundError';
      errorMsg = 'If not found';
    }

    fields.push({
      tag: 'throws',
      value: `{${errorClass}} ${errorMsg}`
    });
  }

  // Sort fields by profile order
  const fieldOrder = jsdocConv.fieldOrder || ['description', 'param', 'returns', 'throws', 'example'];
  fields.sort((a, b) => {
    const aIdx = fieldOrder.indexOf(a.tag);
    const bIdx = fieldOrder.indexOf(b.tag);
    return aIdx - bIdx;
  });

  return { fields };
}

/**
 * Build function signature
 * @param {Operation} operation - Service operation
 * @param {CompiledProfile} profile - Target conventions
 * @returns {Object} Function signature structure
 */
export function buildSignature(operation, profile) {
  // Sort parameters by name for determinism
  const sortedParams = [...operation.params].sort((a, b) => a.name.localeCompare(b.name));
  const paramNames = sortedParams.map(p => p.name);

  return {
    name: operation.name,
    params: paramNames,
    async: operation.async !== false // Default to async
  };
}

/**
 * Build function body
 * @param {Operation} operation - Service operation
 * @param {CompiledProfile} profile - Target conventions
 * @param {CompiledLens} lens - Field transformations
 * @returns {Object} Function body structure
 */
export function buildBody(operation, profile, lens) {
  const conventions = profile.conventions || {};
  const errorStyle = conventions.errorHandling?.style || 'try-catch-zod';

  // Build logging call
  const loggingCall = buildLoggingCall(operation, lens);

  // Build validation (if operation has validation schema)
  const validation = operation.validation ? buildValidation(operation, profile) : null;

  // Build core logic placeholder
  const coreLogic = buildCoreLogic(operation);

  return {
    errorStyle,
    validation,
    loggingCall,
    coreLogic
  };
}

/**
 * Build logging call
 * @param {Operation} operation - Service operation
 * @param {CompiledLens} lens - Field transformations
 * @returns {Object} Logging call structure
 */
function buildLoggingCall(operation, lens) {
  const fieldMap = lens?.getFieldMapping?.() || {};

  // Build fields for logging (sorted for determinism)
  const fields = [];

  // Add operation field first (will be sorted alphabetically later)
  fields.push({ key: 'operation', value: operation.name, isLiteral: true });

  // Add parameters to logging fields
  for (const param of operation.params) {
    // For CustomerData type, expand to email and name fields
    if (param.type === 'CustomerData') {
      // Map canonical fields to target fields
      const emailField = fieldMap.email || 'customer_email';
      const nameField = fieldMap.name || 'customer_name';
      fields.push({ key: emailField, value: 'validated.email', isLiteral: false });
      fields.push({ key: nameField, value: 'validated.name', isLiteral: false });
    } else {
      // Direct parameter mapping
      const targetField = fieldMap[param.name] || param.name;
      fields.push({ key: targetField, value: param.name, isLiteral: false });
    }
  }

  // Sort fields alphabetically by key
  fields.sort((a, b) => a.key.localeCompare(b.key));

  return {
    function: 'logOperation',
    fields
  };
}

/**
 * Build validation logic
 * @param {Operation} operation - Service operation
 * @param {CompiledProfile} profile - Target conventions
 * @returns {Object} Validation structure
 */
function buildValidation(operation, profile) {
  if (!operation.validation) return null;

  // Find the parameter that needs validation
  const validatedParam = operation.params.find(p => p.type.endsWith('Data')) || operation.params[0];

  return {
    schema: operation.validation.schema,
    variable: 'validated',
    paramName: validatedParam?.name || 'data'
  };
}

/**
 * Build core logic placeholder
 * @param {Operation} operation - Service operation
 * @returns {Object} Core logic structure
 */
function buildCoreLogic(operation) {
  // Determine operation type from name
  const opNameLower = operation.name.toLowerCase();

  if (opNameLower.startsWith('create')) {
    return {
      type: 'create',
      calls: operation.calls || [],
      returns: operation.returns
    };
  }

  if (opNameLower.startsWith('get') || opNameLower.startsWith('find') || opNameLower.startsWith('retrieve')) {
    return {
      type: 'get',
      entityType: operation.returns.type,
      calls: operation.calls || [],
      returns: operation.returns
    };
  }

  return {
    type: 'generic',
    calls: operation.calls || [],
    returns: operation.returns
  };
}

/**
 * Build error handler template
 * @param {CompiledProfile} profile - Target conventions
 * @returns {Object} Error handler structure
 */
export function buildErrorHandler(profile) {
  const conventions = profile.conventions || {};
  const errorStyle = conventions.errorHandling?.style || 'try-catch-zod';
  const errorClass = conventions.errorClass || 'Error';

  if (errorStyle === 'try-catch-zod') {
    return {
      wrapper: 'try-catch',
      errorClass,
      rethrowTypes: ['NotFoundError', 'ValidationError']
    };
  }

  if (errorStyle === 'result-monad') {
    return {
      wrapper: 'result',
      errorClass,
      okType: 'Result.ok',
      errType: 'Result.err'
    };
  }

  throw new Error(`Unknown error handling style: ${errorStyle}`);
}

/**
 * Build function template
 * @param {Operation} operation - Service operation
 * @param {CompiledProfile} profile - Target conventions
 * @param {CompiledLens} lens - Field transformations
 * @returns {Object} Complete function template
 */
export function buildFunctionTemplate(operation, profile, lens) {
  return {
    jsdoc: buildJSDoc(operation, profile),
    signature: buildSignature(operation, profile),
    body: buildBody(operation, profile, lens),
    errorHandler: buildErrorHandler(profile)
  };
}

/**
 * Build import statement
 * @param {string} from - Module to import from
 * @param {string[]} names - Names to import
 * @returns {Object} Import structure
 */
export function buildImport(from, names) {
  return {
    from,
    names: [...names].sort() // Sort for determinism
  };
}

/**
 * Build constant definition
 * @param {string} name - Constant name
 * @param {*} value - Constant value
 * @returns {Object} Constant structure
 */
export function buildConstant(name, value) {
  return {
    name,
    value
  };
}
