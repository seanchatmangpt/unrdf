/**
 * @file File role classification
 * @module project-engine/file-roles
 */

import { z } from 'zod';
import { dataFactory } from '@unrdf/oxigraph';
import { FS, PROJECT } from './namespaces.mjs';

const { namedNode, literal } = dataFactory;

const FileRolesOptionsSchema = z.object({
  fsStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'fsStore must be an RDF store with getQuads method',
  }),
  baseIri: z.string().default('http://example.org/unrdf/project#'),
});

const CODE = '(?:tsx?|jsx?|mjs|cjs|mts|cts)';

/**
 * Role patterns, ordered from most to least specific: the first match wins,
 * so `foo.test.ts` is a Test, not a Component.
 */
export const ROLE_PATTERNS = [
  ['Test', [new RegExp(`\\.(?:test|spec)\\.${CODE}$`), /^(?:__tests__|tests?|spec)\//]],
  ['Page', [/^(?:pages|src\/pages|src\/app|app)\//, new RegExp(`(?:^|/)page\\.${CODE}$`)]],
  ['Api', [new RegExp(`(?:api|server|route)\\.${CODE}$`), /^(?:api|server|routes)\//]],
  ['Hook', [new RegExp(`use[A-Z]\\w+\\.${CODE}$`), /^(?:src\/)?hooks?\//]],
  [
    'Service',
    [new RegExp(`(?:service|client|repository)\\.${CODE}$`), /^(?:src\/)?(?:services?|clients?)\//],
  ],
  [
    'Schema',
    [
      new RegExp(`(?:schema|types?|interfaces?)\\.${CODE}$`),
      /(?:schema|types?|interfaces?)\.json$/,
    ],
  ],
  [
    'State',
    [
      new RegExp(`(?:store|state|reducer|context)\\.${CODE}$`),
      /^(?:src\/)?(?:store|state|redux)\//,
    ],
  ],
  ['Doc', [/\.(?:md|mdx)$/, /^(?:docs?|README)/]],
  ['Config', [/package\.json$/, /tsconfig|eslintrc|prettier/, /\.(?:json|ya?ml|toml|conf)$/]],
  ['Build', [/(?:build|webpack|rollup|esbuild|vite|next\.config)/, /^scripts\//]],
  ['Component', [/\.(?:tsx|jsx)$/, new RegExp(`(?:Component|View)\\.${CODE}$`)]],
];

/**
 * Classify a file path to a role, or 'Other' when nothing matches.
 * @param {string} filePath - Project-relative path
 * @returns {string}
 */
export function classifyPath(filePath) {
  for (const [role, patterns] of ROLE_PATTERNS) {
    if (patterns.some(pattern => pattern.test(filePath))) return role;
  }
  return 'Other';
}

/**
 * Add role classifications (`hasRole`, `roleString`) for every scanned file.
 * The store is extended in place and returned. Directories are not classified.
 *
 * @param {Object} options
 * @param {Object} options.fsStore - Store with the project model
 * @param {string} [options.baseIri] - Base IRI for role resources
 * @returns {Object} The same store
 */
export function classifyFiles(options) {
  const { fsStore: store, baseIri } = FileRolesOptionsSchema.parse(options);

  const fileSubjects = new Set(
    store.getQuads(null, namedNode(FS.byteSize), null).map(quad => quad.subject.value)
  );

  for (const quad of store.getQuads(null, namedNode(FS.relativePath), null)) {
    if (!fileSubjects.has(quad.subject.value)) continue;
    const role = classifyPath(quad.object.value);
    if (role === 'Other') continue;
    store.addQuad(quad.subject, namedNode(PROJECT.hasRole), namedNode(`${baseIri}${role}`));
    store.addQuad(quad.subject, namedNode(PROJECT.roleString), literal(role));
  }

  return store;
}
