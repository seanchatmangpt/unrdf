/**
 * @fileoverview JSDoc Parser with Babel AST Analysis
 * Extracts documentation from JavaScript/TypeScript source files
 */

import { parse as parseComments } from 'comment-parser';
import { parse as parseBabel } from '@babel/parser';
import traverse from '@babel/traverse';
import { readFileSync } from 'fs';
import { relative } from 'path';

/**
 * Parse JSDoc comments and AST from a source file
 * @param {string} filePath - Absolute path to source file
 * @param {string} rootDir - Workspace root directory
 * @returns {Object} Parsed documentation data
 */
export function parseFile(filePath, rootDir = process.cwd()) {
  const code = readFileSync(filePath, 'utf-8');
  const relativePath = relative(rootDir, filePath);

  // Parse JSDoc comments
  const comments = extractJSDocComments(code);

  // Parse AST
  const ast = parseBabel(code, {
    sourceType: 'module',
    plugins: ['jsx', 'typescript'],
  });

  // Extract exports and their metadata
  const exports = extractExports(ast, comments);

  // Extract imports for cross-referencing
  const imports = extractImports(ast);

  return {
    file: filePath,
    relativePath,
    exports,
    imports,
    comments: comments.length,
  };
}

/**
 * Extract JSDoc comments from source code
 * @param {string} code - Source code
 * @returns {Array} Parsed JSDoc blocks
 */
function extractJSDocComments(code) {
  // Extract all comments matching JSDoc pattern
  const jsDocRegex = /\/\*\*[\s\S]*?\*\//g;
  const matches = code.match(jsDocRegex) || [];

  return matches.map(block => {
    const parsed = parseComments(block)[0];
    return {
      description: parsed.description,
      tags: parsed.tags.map(tag => ({
        tag: tag.tag,
        name: tag.name,
        type: tag.type,
        description: tag.description,
        optional: tag.optional,
        default: tag.default,
      })),
      source: block,
    };
  });
}

/**
 * Extract exports from AST
 * @param {Object} ast - Babel AST
 * @param {Array} comments - Parsed JSDoc comments
 * @returns {Array} Export declarations with metadata
 */
function extractExports(ast, comments) {
  const exports = [];
  let commentIndex = 0;

  traverse.default(ast, {
    ExportNamedDeclaration(path) {
      const { declaration } = path.node;

      if (!declaration) return;

      // Find associated JSDoc comment
      const jsdoc = commentIndex < comments.length ? comments[commentIndex++] : null;

      if (declaration.type === 'FunctionDeclaration') {
        exports.push(extractFunctionExport(declaration, jsdoc));
      } else if (declaration.type === 'VariableDeclaration') {
        declaration.declarations.forEach(decl => {
          if (decl.init && (decl.init.type === 'FunctionExpression' || decl.init.type === 'ArrowFunctionExpression')) {
            exports.push(extractFunctionExport(decl, jsdoc));
          } else {
            exports.push(extractVariableExport(decl, jsdoc));
          }
        });
      } else if (declaration.type === 'ClassDeclaration') {
        exports.push(extractClassExport(declaration, jsdoc));
      }
    },

    ExportDefaultDeclaration(path) {
      const { declaration } = path.node;
      const jsdoc = commentIndex < comments.length ? comments[commentIndex++] : null;

      exports.push({
        name: 'default',
        type: declaration.type,
        isDefault: true,
        jsdoc,
      });
    },
  });

  return exports;
}

/**
 * Extract function export metadata
 * @param {Object} node - AST node
 * @param {Object} jsdoc - JSDoc comment
 * @returns {Object} Function metadata
 */
function extractFunctionExport(node, jsdoc) {
  const name = node.id?.name || node.key?.name;
  const params = extractParams(node.params || node.init?.params || []);

  // Extract metadata from JSDoc
  const paramDocs = jsdoc?.tags.filter(t => t.tag === 'param') || [];
  const returnDoc = jsdoc?.tags.find(t => t.tag === 'returns' || t.tag === 'return');
  const examples = jsdoc?.tags.filter(t => t.tag === 'example').map(t => t.description) || [];

  return {
    name,
    type: 'function',
    description: jsdoc?.description || '',
    params: params.map((param, i) => ({
      name: param.name,
      type: paramDocs[i]?.type || 'any',
      description: paramDocs[i]?.description || '',
      optional: param.optional || paramDocs[i]?.optional || false,
      default: param.default || paramDocs[i]?.default,
    })),
    returns: {
      type: returnDoc?.type || 'void',
      description: returnDoc?.description || '',
    },
    examples,
    async: node.async || node.init?.async || false,
  };
}

/**
 * Extract variable export metadata
 * @param {Object} node - AST node
 * @param {Object} jsdoc - JSDoc comment
 * @returns {Object} Variable metadata
 */
function extractVariableExport(node, jsdoc) {
  return {
    name: node.id?.name,
    type: 'variable',
    description: jsdoc?.description || '',
    valueType: node.init?.type || 'unknown',
  };
}

/**
 * Extract class export metadata
 * @param {Object} node - AST node
 * @param {Object} jsdoc - JSDoc comment
 * @returns {Object} Class metadata
 */
function extractClassExport(node, jsdoc) {
  const methods = [];

  node.body.body.forEach(member => {
    if (member.type === 'ClassMethod') {
      methods.push({
        name: member.key.name,
        kind: member.kind, // 'constructor', 'method', 'get', 'set'
        static: member.static,
        async: member.async,
        params: extractParams(member.params),
      });
    }
  });

  return {
    name: node.id.name,
    type: 'class',
    description: jsdoc?.description || '',
    methods,
    extends: node.superClass?.name,
  };
}

/**
 * Extract parameter metadata from AST nodes
 * @param {Array} params - Parameter AST nodes
 * @returns {Array} Parameter metadata
 */
function extractParams(params) {
  return params.map(param => {
    if (param.type === 'Identifier') {
      return { name: param.name, optional: false };
    } else if (param.type === 'AssignmentPattern') {
      return {
        name: param.left.name,
        optional: true,
        default: extractDefaultValue(param.right),
      };
    } else if (param.type === 'RestElement') {
      return { name: param.argument.name, rest: true };
    } else if (param.type === 'ObjectPattern') {
      return { name: 'options', destructured: true };
    }
    return { name: 'unknown' };
  });
}

/**
 * Extract default value from AST node
 * @param {Object} node - AST node
 * @returns {any} Default value
 */
function extractDefaultValue(node) {
  if (node.type === 'Literal') return node.value;
  if (node.type === 'NumericLiteral') return node.value;
  if (node.type === 'StringLiteral') return node.value;
  if (node.type === 'BooleanLiteral') return node.value;
  if (node.type === 'NullLiteral') return null;
  if (node.type === 'ObjectExpression') return {};
  if (node.type === 'ArrayExpression') return [];
  return undefined;
}

/**
 * Extract import statements for cross-referencing
 * @param {Object} ast - Babel AST
 * @returns {Array} Import declarations
 */
function extractImports(ast) {
  const imports = [];

  traverse.default(ast, {
    ImportDeclaration(path) {
      const { source, specifiers } = path.node;

      imports.push({
        source: source.value,
        specifiers: specifiers.map(spec => ({
          imported: spec.imported?.name || spec.local.name,
          local: spec.local.name,
          type: spec.type, // 'ImportSpecifier', 'ImportDefaultSpecifier', 'ImportNamespaceSpecifier'
        })),
      });
    },
  });

  return imports;
}

/**
 * Parse multiple files in batch
 * @param {Array<string>} filePaths - Array of file paths
 * @param {string} rootDir - Workspace root directory
 * @returns {Array} Parsed data for all files
 */
export function parseFiles(filePaths, rootDir = process.cwd()) {
  return filePaths.map(filePath => {
    try {
      return parseFile(filePath, rootDir);
    } catch (error) {
      console.error(`Failed to parse ${filePath}:`, error.message);
      return {
        file: filePath,
        error: error.message,
        exports: [],
        imports: [],
      };
    }
  });
}

export default { parseFile, parseFiles };
