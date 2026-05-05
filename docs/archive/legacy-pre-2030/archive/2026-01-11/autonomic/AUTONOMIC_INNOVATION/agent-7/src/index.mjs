/**
 * Convention-Preserving Generator - Public API
 * @module @unrdf/autonomic-generator
 */

export { generateFacade, buildFacadeAST, buildImports, buildFunction } from './generator.mjs';
export { formatCode, formatImports, formatJSDoc } from './formatter.mjs';
export { validateGeneratedCode, checkImportOrder, checkJSDocFormat, checkLineLength } from './validator.mjs';
export {
  buildFunctionTemplate,
  buildJSDoc,
  buildSignature,
  buildBody,
  buildErrorHandler,
  buildImport,
  buildConstant
} from './templates.mjs';
