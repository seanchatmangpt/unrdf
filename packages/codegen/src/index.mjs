/**
 * @unrdf/codegen - Code Generation & Metaprogramming Tools
 * @module @unrdf/codegen
 *
 * Innovative code generation patterns for RDF-driven development
 */

export {
  generateTypesFromSPARQL,
  generateTSInterface,
  createGenerationReceipt,
} from './sparql-type-generator.mjs';

export {
  MetaTemplateEngine,
  generateCRUDTemplate,
  generateTestTemplate,
} from './meta-template-engine.mjs';

export {
  default as generatePropertyTests,
  generateFromSHACL,
} from './property-test-generator.mjs';
import { createRequire as __createRequire } from 'node:module';
const PKG_VERSION = __createRequire(import.meta.url)('../package.json').version;

/**
 * Package metadata
 */
export const metadata = {
  name: '@unrdf/codegen',
  version: PKG_VERSION,
  description: 'Code generation and metaprogramming tools for UNRDF',
  patterns: [
    'SPARQL Type Generator',
    'Meta-Template Engine',
    'Property-Based Test Generator',
  ],
};

export default {
  generateTypesFromSPARQL,
  MetaTemplateEngine,
  generatePropertyTests,
  metadata,
};
