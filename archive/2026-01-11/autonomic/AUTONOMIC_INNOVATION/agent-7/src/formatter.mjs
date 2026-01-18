/**
 * Deterministic code formatter
 * @module formatter
 */

/**
 * Format AST to deterministic source code
 * @param {Object} ast - Abstract syntax tree
 * @param {CompiledProfile} profile - Formatting rules
 * @returns {string} Formatted code (byte-for-byte stable)
 */
export function formatCode(ast, profile) {
  const config = {
    indent: '  ', // 2 spaces
    lineLength: 100,
    sortImports: true,
    sortExports: true,
    jsdocFieldOrder: profile?.conventions?.jsdoc?.fieldOrder || ['description', 'param', 'returns', 'throws', 'example'],
    trailingComma: profile?.conventions?.style?.trailingComma || 'es5',
    quotes: profile?.conventions?.style?.quotes || 'single',
    semicolons: profile?.conventions?.style?.semicolons !== false
  };

  const sections = [
    formatFileHeader(ast.header, config),
    formatImports(ast.imports, config),
    formatConstants(ast.constants, config),
    formatFunctions(ast.functions, config),
    formatExports(ast.exports, config)
  ];

  return sections.filter(s => s).join('\n\n') + '\n';
}

/**
 * Format file header comment
 * @param {Object} header - Header information
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted header
 */
function formatFileHeader(header, config) {
  if (!header) return '';

  const lines = [
    '/**',
    ` * ${header.title || ''}`,
    ` * ${header.description || ''}`,
    ` * ${header.metadata || ''}`,
    ' */'
  ];

  return lines.filter(l => l.trim() !== '*').join('\n');
}

/**
 * Deterministic import sorting and formatting
 * @param {Array} imports - Import statements
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted imports
 */
export function formatImports(imports, config) {
  if (!imports || !imports.length) return '';

  // Group imports: node built-ins, external, internal
  const groups = {
    builtin: [],
    external: [],
    internal: []
  };

  for (const imp of imports) {
    if (isNodeBuiltin(imp.from)) {
      groups.builtin.push(imp);
    } else if (imp.from.startsWith('.')) {
      groups.internal.push(imp);
    } else {
      groups.external.push(imp);
    }
  }

  // Sort each group alphabetically by 'from'
  for (const group of Object.values(groups)) {
    group.sort((a, b) => a.from.localeCompare(b.from));
  }

  // Format each import
  const formatted = [];

  // Builtin imports
  if (groups.builtin.length > 0) {
    for (const imp of groups.builtin) {
      formatted.push(formatImport(imp, config));
    }
  }

  // Blank line between groups
  if (groups.builtin.length > 0 && groups.external.length > 0) {
    formatted.push('');
  }

  // External imports
  if (groups.external.length > 0) {
    for (const imp of groups.external) {
      formatted.push(formatImport(imp, config));
    }
  }

  // Blank line between groups
  if ((groups.builtin.length > 0 || groups.external.length > 0) && groups.internal.length > 0) {
    formatted.push('');
  }

  // Internal imports
  if (groups.internal.length > 0) {
    for (const imp of groups.internal) {
      formatted.push(formatImport(imp, config));
    }
  }

  return formatted.join('\n');
}

/**
 * Format single import statement
 * @param {Object} imp - Import object
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted import
 */
function formatImport(imp, config) {
  const quote = config.quotes === 'double' ? '"' : "'";
  const semi = config.semicolons ? ';' : '';

  // Sort names for determinism
  const names = [...imp.names].sort();

  if (names.length === 0) {
    return `import ${quote}${imp.from}${quote}${semi}`;
  }

  if (names.length === 1) {
    return `import { ${names[0]} } from ${quote}${imp.from}${quote}${semi}`;
  }

  // Multi-line if >3 names or exceeds line length
  const oneLine = `import { ${names.join(', ')} } from ${quote}${imp.from}${quote}${semi}`;
  if (names.length <= 3 && oneLine.length <= config.lineLength) {
    return oneLine;
  }

  // Multi-line format
  const lines = [
    `import {`,
    ...names.map(n => `  ${n}`),
    `} from ${quote}${imp.from}${quote}${semi}`
  ];
  return lines.join(',\n').replace(',\n}', '\n}');
}

/**
 * Check if module is Node.js built-in
 * @param {string} moduleName - Module name
 * @returns {boolean} True if built-in
 */
function isNodeBuiltin(moduleName) {
  return moduleName.startsWith('node:') ||
    ['fs', 'path', 'http', 'https', 'crypto', 'util', 'stream', 'events'].includes(moduleName);
}

/**
 * Format constants
 * @param {Array} constants - Constant definitions
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted constants
 */
function formatConstants(constants, config) {
  if (!constants || !constants.length) return '';

  // Sort constants by name for determinism
  const sorted = [...constants].sort((a, b) => a.name.localeCompare(b.name));

  return sorted.map(c => formatConstant(c, config)).join('\n\n');
}

/**
 * Format single constant
 * @param {Object} constant - Constant definition
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted constant
 */
function formatConstant(constant, config) {
  const semi = config.semicolons ? ';' : '';

  if (typeof constant.value === 'string') {
    const quote = config.quotes === 'double' ? '"' : "'";
    return `const ${constant.name} = ${quote}${constant.value}${quote}${semi}`;
  }

  if (typeof constant.value === 'object' && constant.value !== null) {
    // Special handling for Zod schemas
    if (constant.value.type === 'z.object') {
      return formatZodSchema(constant.name, constant.value, config);
    }
    return `const ${constant.name} = ${formatObject(constant.value, config, 0)}${semi}`;
  }

  return `const ${constant.name} = ${constant.value}${semi}`;
}

/**
 * Format Zod schema
 * @param {string} name - Schema name
 * @param {Object} schema - Schema definition
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted Zod schema
 */
function formatZodSchema(name, schema, config) {
  const semi = config.semicolons ? ';' : '';
  const fields = schema.fields || {};
  const fieldKeys = Object.keys(fields).sort(); // Sort for determinism

  if (fieldKeys.length === 0) {
    return `const ${name} = z.object({})${semi}`;
  }

  const indent = config.indent;
  const fieldLines = fieldKeys.map(key => {
    const value = fields[key];
    return `${indent}${key}: ${value}`;
  });

  return `const ${name} = z.object({\n${fieldLines.join(',\n')}\n})${semi}`;
}

/**
 * Format object literal
 * @param {Object} obj - Object to format
 * @param {Object} config - Formatting configuration
 * @param {number} depth - Indentation depth
 * @returns {string} Formatted object
 */
function formatObject(obj, config, depth) {
  if (obj === null) return 'null';
  if (typeof obj !== 'object') return JSON.stringify(obj);

  // Sort keys for determinism
  const keys = Object.keys(obj).sort();

  if (keys.length === 0) return '{}';

  const indent = config.indent.repeat(depth + 1);
  const closeIndent = config.indent.repeat(depth);

  const entries = keys.map(key => {
    const value = obj[key];
    let formattedValue;

    if (typeof value === 'object' && value !== null) {
      formattedValue = formatObject(value, config, depth + 1);
    } else if (typeof value === 'string') {
      const quote = config.quotes === 'double' ? '"' : "'";
      formattedValue = `${quote}${value}${quote}`;
    } else {
      formattedValue = String(value);
    }

    return `${indent}${key}: ${formattedValue}`;
  });

  return `{\n${entries.join(',\n')}\n${closeIndent}}`;
}

/**
 * Format functions
 * @param {Array} functions - Function definitions
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted functions
 */
function formatFunctions(functions, config) {
  if (!functions || !functions.length) return '';

  // Sort functions by name for determinism
  const sorted = [...functions].sort((a, b) =>
    a.signature.name.localeCompare(b.signature.name)
  );

  return sorted.map(f => formatFunction(f, config)).join('\n\n');
}

/**
 * Format single function
 * @param {Object} func - Function definition
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted function
 */
function formatFunction(func, config) {
  const parts = [];

  // JSDoc
  if (func.jsdoc) {
    parts.push(formatJSDoc(func.jsdoc, config));
  }

  // Function signature
  const signature = formatSignature(func.signature, config);
  const body = formatFunctionBody(func.body, func.errorHandler, config);

  parts.push(`${signature} {\n${body}\n}`);

  return parts.join('\n');
}

/**
 * Format JSDoc block
 * @param {Object} jsdoc - JSDoc structure
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted JSDoc
 */
export function formatJSDoc(jsdoc, config) {
  const lines = ['/**'];

  // Fields are already sorted by buildJSDoc
  for (const field of jsdoc.fields) {
    if (field.tag === 'description') {
      lines.push(` * ${field.value}`);
    } else {
      lines.push(` * @${field.tag} ${field.value}`);
    }
  }

  lines.push(' */');
  return lines.join('\n');
}

/**
 * Format function signature
 * @param {Object} signature - Signature structure
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted signature
 */
function formatSignature(signature, config) {
  const asyncPrefix = signature.async ? 'async ' : '';
  const params = signature.params.join(', ');
  return `export ${asyncPrefix}function ${signature.name}(${params})`;
}

/**
 * Format function body
 * @param {Object} body - Body structure
 * @param {Object} errorHandler - Error handler structure
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted body
 */
function formatFunctionBody(body, errorHandler, config) {
  const indent = config.indent;
  const lines = [];

  if (errorHandler.wrapper === 'try-catch') {
    lines.push(`${indent}try {`);

    // Validation
    if (body.validation) {
      // Get the parameter name from validation.paramName
      const paramName = body.validation.paramName || 'data';
      lines.push(`${indent}${indent}const ${body.validation.variable} = ${body.validation.schema}.parse(${paramName});`);
      lines.push('');
    }

    // Logging
    if (body.loggingCall) {
      lines.push(`${indent}${indent}${body.loggingCall.function}(${formatLoggingFields(body.loggingCall.fields, config, 2)});`);
      lines.push('');
    }

    // Core logic
    if (body.coreLogic) {
      const logic = body.coreLogic;
      const entityType = logic.entityType || 'Customer';
      const varName = entityType.toLowerCase();

      if (logic.type === 'create') {
        lines.push(`${indent}${indent}const transformed = transformFields(validated);`);
        lines.push(`${indent}${indent}const ${varName} = await store.create('${entityType}', transformed);`);
        lines.push('');
        lines.push(`${indent}${indent}return ${varName};`);
      } else if (logic.type === 'get') {
        // Get operation - assumes 'id' parameter
        lines.push(`${indent}${indent}const ${varName} = await store.get('${entityType}', id);`);
        lines.push('');
        lines.push(`${indent}${indent}if (!${varName}) {`);
        lines.push(`${indent}${indent}${indent}throw new NotFoundError(\`${entityType} \${id} not found\`);`);
        lines.push(`${indent}${indent}}`);
        lines.push('');
        lines.push(`${indent}${indent}return ${varName};`);
      } else {
        // Generic logic
        lines.push(`${indent}${indent}const transformed = transformFields(validated);`);
        lines.push(`${indent}${indent}const ${varName} = await store.create('${entityType}', transformed);`);
        lines.push('');
        lines.push(`${indent}${indent}return ${varName};`);
      }
    }

    // Catch block
    lines.push(`${indent}} catch (err) {`);

    // Rethrow specific errors
    if (errorHandler.rethrowTypes && errorHandler.rethrowTypes.includes('NotFoundError')) {
      lines.push(`${indent}${indent}if (err instanceof NotFoundError) throw err;`);
    }

    // Operation-specific error message
    const logic = body.coreLogic;
    const entityType = logic?.entityType || 'Customer';
    let errorMsg = '';
    let errorClass = errorHandler.errorClass;

    if (logic?.type === 'create') {
      errorMsg = `Failed to create ${entityType.toLowerCase()}`;
      errorClass = 'ValidationError';
    } else if (logic?.type === 'get') {
      errorMsg = `Failed to retrieve ${entityType.toLowerCase()}`;
      errorClass = 'OperationError';
    } else {
      errorMsg = `Failed to create ${entityType.toLowerCase()}`;
    }

    lines.push(`${indent}${indent}throw new ${errorClass}('${errorMsg}', { cause: err });`);
    lines.push(`${indent}}`);
  }

  return lines.join('\n');
}

/**
 * Format logging fields
 * @param {Array<{key: string, value: string, isLiteral: boolean}>} fields - Logging fields
 * @param {Object} config - Formatting configuration
 * @param {number} depth - Indentation depth
 * @returns {string} Formatted logging fields
 */
function formatLoggingFields(fields, config, depth) {
  if (!fields || fields.length === 0) return '{}';

  const indent = config.indent.repeat(depth + 1);
  const closeIndent = config.indent.repeat(depth);

  const entries = fields.map(({ key, value, isLiteral }) => {
    let formattedValue;
    if (isLiteral) {
      const quote = config.quotes === 'double' ? '"' : "'";
      formattedValue = `${quote}${value}${quote}`;
    } else {
      formattedValue = value;
    }
    return `${indent}${key}: ${formattedValue}`;
  });

  return `{\n${entries.join(',\n')}\n${closeIndent}}`;
}

/**
 * Format exports
 * @param {Array} exports - Export definitions
 * @param {Object} config - Formatting configuration
 * @returns {string} Formatted exports
 */
function formatExports(exports, config) {
  if (!exports || !exports.length) return '';

  // Exports are typically part of function declarations
  return '';
}
