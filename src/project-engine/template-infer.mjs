/**
 * @file Pattern induction - learns generator templates from existing project code
 * @module project-engine/template-infer
 */

import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { createStore } from '@unrdf/oxigraph'; // TODO: Replace with Oxigraph Store
import { z } from 'zod';

const { namedNode, literal } = DataFactory;

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

const InferOptionsSchema = z.object({
  fsStore: z.any().refine(val => val && typeof val.getQuads === 'function', {
    message: 'fsStore must be an RDF store with getQuads method',
  }),
  domainStore: z.any().nullable().optional(),
  stackProfile: z
    .object({
      uiFramework: z.string().nullable().optional(),
      webFramework: z.string().nullable().optional(),
      testFramework: z.string().nullable().optional(),
      language: z.string().nullable().optional(),
    })
    .optional(),
});

export const TemplateSchema = z.object({
  id: z.string(),
  kind: z.string(),
  outputPattern: z.string(),
  variables: z.array(z.string()),
  invariants: z.array(z.string()),
  variantCount: z.number(),
  examples: z.array(z.string()),
});

export const InferSummarySchema = z.object({
  templateCount: z.number(),
  byKind: z.record(z.string(), z.number()),
});

/* ========================================================================= */
/* Namespaces                                                               */
/* ========================================================================= */

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  gen: 'http://example.org/unrdf/generator#',
  dom: 'http://example.org/unrdf/domain#',
  unproj: 'http://example.org/unrdf/project#',
  fs: 'http://example.org/unrdf/filesystem#',
};

/* ========================================================================= */
/* File Family Patterns                                                     */
/* ========================================================================= */

/**
 * File family detection patterns
 * Each pattern defines how to identify and extract templates from file groups
 */
const FILE_FAMILY_PATTERNS = [
  {
    kind: 'Component',
    patterns: [
      /^src\/features\/([^/]+)\/([A-Z][a-zA-Z]+)(Component|View|Page)?\.(tsx?|jsx?)$/,
      /^src\/components\/([^/]+)\/(index|[A-Z][a-zA-Z]+)\.(tsx?|jsx?)$/,
      /^components\/([^/]+)\/(index|[A-Z][a-zA-Z]+)\.(tsx?|jsx?)$/,
    ],
    outputTemplate: 'src/features/{{entity}}/{{Entity}}{{suffix}}.{{ext}}',
    extractVars: (match, _path) => ({
      entity: match[1],
      Entity: capitalize(match[1]),
      suffix: match[3] || '',
      ext: match[4] || 'tsx',
    }),
  },
  {
    kind: 'Page',
    patterns: [
      /^src\/app\/([^/]+)\/page\.(tsx?|jsx?)$/,
      /^src\/pages\/([^/]+)\/(index|page)\.(tsx?|jsx?)$/,
      /^pages\/([^/]+)\/(index|page)\.(tsx?|jsx?)$/,
      /^app\/([^/]+)\/page\.(tsx?|jsx?)$/,
    ],
    outputTemplate: 'src/app/{{route}}/page.{{ext}}',
    extractVars: (match, _path) => ({
      route: match[1],
      ext: match[2] || 'tsx',
    }),
  },
  {
    kind: 'Route',
    patterns: [
      /^src\/app\/([^/]+)\/route\.(tsx?|ts)$/,
      /^src\/routes\/([^/]+)\.(tsx?|ts)$/,
      /^app\/api\/([^/]+)\/route\.(tsx?|ts)$/,
    ],
    outputTemplate: 'src/app/{{route}}/route.{{ext}}',
    extractVars: (match, _path) => ({
      route: match[1],
      ext: match[2] || 'ts',
    }),
  },
  {
    kind: 'Test',
    patterns: [
      /^src\/([^/]+)\/([^/]+)\.(test|spec)\.(tsx?|jsx?)$/,
      /^test\/([^/]+)\/([^/]+)\.(test|spec)\.(tsx?|jsx?)$/,
      /^__tests__\/([^/]+)\.(test|spec)\.(tsx?|jsx?)$/,
      /^([^/]+)\.(test|spec)\.(tsx?|jsx?)$/,
    ],
    outputTemplate: 'test/{{module}}/{{name}}.{{testType}}.{{ext}}',
    extractVars: (match, _path) => ({
      module: match[1] || 'unit',
      name: match[2] || match[1],
      testType: match[3] || 'test',
      ext: match[4] || 'ts',
    }),
  },
  {
    kind: 'Api',
    patterns: [
      /^src\/api\/([^/]+)\/(route|handler|controller)\.(tsx?|ts)$/,
      /^api\/([^/]+)\.(tsx?|ts)$/,
      /^src\/server\/([^/]+)\.(tsx?|ts)$/,
    ],
    outputTemplate: 'src/api/{{endpoint}}/route.{{ext}}',
    extractVars: (match, _path) => ({
      endpoint: match[1],
      ext: match[3] || match[2] || 'ts',
    }),
  },
  {
    kind: 'Hook',
    patterns: [
      /^src\/hooks\/(use[A-Z][a-zA-Z]+)\.(tsx?|ts)$/,
      /^hooks\/(use[A-Z][a-zA-Z]+)\.(tsx?|ts)$/,
      /^src\/features\/([^/]+)\/hooks\/(use[A-Z][a-zA-Z]+)\.(tsx?|ts)$/,
    ],
    outputTemplate: 'src/hooks/{{hookName}}.{{ext}}',
    extractVars: (match, _path) => ({
      hookName: match[1] || match[2],
      feature: match[1] && match[2] ? match[1] : null,
      ext: match[2] || match[3] || 'ts',
    }),
  },
  {
    kind: 'Service',
    patterns: [
      /^src\/services\/([^/]+)\.(service|client)\.(tsx?|ts)$/,
      /^src\/lib\/([^/]+)\.(tsx?|ts)$/,
      /^lib\/([^/]+)\.(tsx?|ts)$/,
    ],
    outputTemplate: 'src/services/{{serviceName}}.service.{{ext}}',
    extractVars: (match, _path) => ({
      serviceName: match[1],
      ext: match[3] || match[2] || 'ts',
    }),
  },
  {
    kind: 'Schema',
    patterns: [
      /^src\/schemas?\/([^/]+)\.(schema|types?)\.(tsx?|ts)$/,
      /^src\/types\/([^/]+)\.(tsx?|ts)$/,
      /^types\/([^/]+)\.(tsx?|ts)$/,
    ],
    outputTemplate: 'src/schemas/{{schemaName}}.schema.{{ext}}',
    extractVars: (match, _path) => ({
      schemaName: match[1],
      ext: match[3] || match[2] || 'ts',
    }),
  },
  {
    kind: 'Doc',
    patterns: [
      /^docs?\/([^/]+)\.(md|mdx)$/,
      /^src\/docs?\/([^/]+)\.(md|mdx)$/,
      /^([A-Z][A-Z_]+)\.(md|mdx)$/,
    ],
    outputTemplate: 'docs/{{docName}}.{{ext}}',
    extractVars: (match, _path) => ({
      docName: match[1],
      ext: match[2] || 'md',
    }),
  },
  {
    kind: 'Config',
    patterns: [
      /^\.?([a-z]+)rc\.(json|yaml|yml|js|cjs|mjs)$/,
      /^([a-z]+)\.config\.(tsx?|ts|js|cjs|mjs)$/,
      /^config\/([^/]+)\.(json|yaml|yml)$/,
    ],
    outputTemplate: '{{configName}}.config.{{ext}}',
    extractVars: (match, _path) => ({
      configName: match[1],
      ext: match[2] || 'js',
    }),
  },
];

/* ========================================================================= */
/* Helper Functions                                                         */
/* ========================================================================= */

/**
 * Capitalize first letter
 * @param {string} str
 * @returns {string}
 */
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Convert camelCase to kebab-case
 * @param {string} str
 * @returns {string}
 */
function _toKebabCase(str) {
  return str.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase();
}

/**
 * Extract file paths from fs store
 * @param {Store} fsStore
 * @returns {string[]}
 */
function extractFilePathsFromStore(fsStore) {
  const paths = [];

  // Validate fsStore has getQuads method
  if (!fsStore || typeof fsStore.getQuads !== 'function') {
    return paths;
  }

  try {
    const quads = fsStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null);

    for (const quad of quads) {
      paths.push(quad.object.value);
    }
  } catch (e) {
    // Handle stores that don't support getQuads properly
    return paths;
  }

  return paths;
}

/**
 * Group files by family pattern
 * @param {string[]} paths
 * @returns {Map<string, {pattern: Object, matches: Array<{path: string, match: RegExpMatchArray, vars: Object}>}>}
 */
function groupFilesByFamily(paths) {
  const groups = new Map();

  for (const pattern of FILE_FAMILY_PATTERNS) {
    groups.set(pattern.kind, {
      pattern,
      matches: [],
    });
  }

  for (const path of paths) {
    for (const familyPattern of FILE_FAMILY_PATTERNS) {
      for (const regex of familyPattern.patterns) {
        const match = path.match(regex);
        if (match) {
          const vars = familyPattern.extractVars(match, path);
          const group = groups.get(familyPattern.kind);
          group.matches.push({ path, match, vars });
          break;
        }
      }
    }
  }

  return groups;
}

/**
 * Extract invariants from a group of similar files
 * @param {Array<{path: string, match: RegExpMatchArray, vars: Object}>} matches
 * @returns {string[]}
 */
function extractInvariants(matches) {
  if (matches.length < 2) return [];

  const invariants = [];

  // Find common path prefixes
  const prefixes = matches.map(m => {
    const parts = m.path.split('/');
    return parts.slice(0, -1).join('/');
  });
  const commonPrefix = findCommonPrefix(prefixes);
  if (commonPrefix) {
    invariants.push(`path_prefix:${commonPrefix}`);
  }

  // Find common extensions
  const extensions = matches.map(m => {
    const parts = m.path.split('.');
    return parts[parts.length - 1];
  });
  const uniqueExts = [...new Set(extensions)];
  if (uniqueExts.length === 1) {
    invariants.push(`extension:${uniqueExts[0]}`);
  }

  // Find common naming patterns
  const baseNames = matches.map(m => {
    const parts = m.path.split('/');
    const fileName = parts[parts.length - 1];
    return fileName.split('.')[0];
  });
  const namingPattern = detectNamingPattern(baseNames);
  if (namingPattern) {
    invariants.push(`naming:${namingPattern}`);
  }

  return invariants;
}

/**
 * Find common prefix among strings
 * @param {string[]} strings
 * @returns {string}
 */
function findCommonPrefix(strings) {
  if (strings.length === 0) return '';
  if (strings.length === 1) return strings[0];

  let prefix = strings[0];
  for (let i = 1; i < strings.length; i++) {
    while (!strings[i].startsWith(prefix) && prefix.length > 0) {
      prefix = prefix.slice(0, -1);
    }
  }
  return prefix;
}

/**
 * Detect naming pattern from base names
 * @param {string[]} names
 * @returns {string|null}
 */
function detectNamingPattern(names) {
  const patterns = {
    PascalCase: /^[A-Z][a-zA-Z0-9]*$/,
    camelCase: /^[a-z][a-zA-Z0-9]*$/,
    'kebab-case': /^[a-z][a-z0-9-]*$/,
    snake_case: /^[a-z][a-z0-9_]*$/,
    index: /^index$/,
  };

  for (const [name, regex] of Object.entries(patterns)) {
    if (names.every(n => regex.test(n))) {
      return name;
    }
  }

  return null;
}

/**
 * Extract variable names from output pattern
 * @param {string} pattern
 * @returns {string[]}
 */
function extractVariablesFromPattern(pattern) {
  const matches = pattern.match(/\{\{(\w+)\}\}/g) || [];
  return matches.map(m => m.replace(/\{\{|\}\}/g, ''));
}

/**
 * Generate template ID
 * @param {string} kind
 * @param {number} index
 * @returns {string}
 */
function generateTemplateId(kind, index) {
  return `${kind}Template${index > 0 ? index + 1 : ''}`;
}

/* ========================================================================= */
/* RDF Generation                                                           */
/* ========================================================================= */

/**
 * Add template triples to store
 * @param {Store} store
 * @param {Object} template
 */
function addTemplateToStore(store, template) {
  const templateIri = namedNode(`${NS.gen}${template.id}`);

  // rdf:type gen:Template
  store.addQuad(templateIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.gen}Template`));

  // gen:templateKind
  store.addQuad(templateIri, namedNode(`${NS.gen}templateKind`), literal(template.kind));

  // gen:outputPattern
  store.addQuad(templateIri, namedNode(`${NS.gen}outputPattern`), literal(template.outputPattern));

  // gen:variantCount
  store.addQuad(
    templateIri,
    namedNode(`${NS.gen}variantCount`),
    literal(template.variantCount, namedNode(`${NS.xsd}integer`))
  );

  // gen:variable (multiple)
  for (const variable of template.variables) {
    store.addQuad(templateIri, namedNode(`${NS.gen}variable`), literal(variable));
  }

  // gen:invariant (multiple)
  for (const invariant of template.invariants) {
    store.addQuad(templateIri, namedNode(`${NS.gen}invariant`), literal(invariant));
  }

  // gen:example (multiple, max 3)
  for (const example of template.examples.slice(0, 3)) {
    store.addQuad(templateIri, namedNode(`${NS.gen}example`), literal(example));
  }

  // gen:producesRole
  store.addQuad(
    templateIri,
    namedNode(`${NS.gen}producesRole`),
    namedNode(`${NS.unproj}${template.kind}`)
  );
}

/* ========================================================================= */
/* Public API                                                               */
/* ========================================================================= */

/**
 * Infer generator templates from existing project code
 *
 * Scans project files, groups them by family, extracts patterns,
 * and returns RDF triples describing the inferred templates.
 *
 * @param {Object} fsStore - Filesystem store with project files
 * @param {Object} [domainStore] - Optional domain ontology store
 * @param {Object} [stackProfile] - Optional stack detection info
 * @returns {{store: Store, summary: {templateCount: number, byKind: Record<string, number>}}}
 *
 * @example
 * const { store, summary } = inferTemplatesFromProject(fsStore)
 * console.log(`Found ${summary.templateCount} templates`)
 * // store contains RDF triples like:
 * // gen:UserViewTemplate rdf:type gen:Template
 * // gen:UserViewTemplate gen:outputPattern "src/features/{{entity}}/{{Entity}}Page.tsx"
 */
export function inferTemplatesFromProject(fsStore, domainStore, stackProfile) {
  const validated = InferOptionsSchema.parse({
    fsStore,
    domainStore,
    stackProfile,
  });

  const store = createStore();
  const summary = {
    templateCount: 0,
    byKind: {},
  };

  // Extract file paths from fsStore
  const paths = extractFilePathsFromStore(validated.fsStore);

  // Group files by family pattern
  const groups = groupFilesByFamily(paths);

  // Process each family group
  for (const [kind, group] of groups) {
    const { pattern, matches } = group;

    // Skip families with too few matches (need >= 2 for pattern detection)
    if (matches.length < 2) continue;

    // Extract invariants from the matched files
    const invariants = extractInvariants(matches);

    // Extract variables from the output pattern
    const variables = extractVariablesFromPattern(pattern.outputTemplate);

    // Create template
    const templateId = generateTemplateId(kind, 0);
    const template = {
      id: templateId,
      kind,
      outputPattern: pattern.outputTemplate,
      variables,
      invariants,
      variantCount: matches.length,
      examples: matches.map(m => m.path),
    };

    // Add to store
    addTemplateToStore(store, template);

    // Update summary
    summary.templateCount++;
    summary.byKind[kind] = (summary.byKind[kind] || 0) + 1;
  }

  // Validate summary
  InferSummarySchema.parse(summary);

  return { store, summary };
}

/**
 * Infer templates with domain entity binding
 *
 * Enhanced version that also attempts to bind templates to domain entities
 * from the domain ontology store.
 *
 * @param {Object} fsStore - Filesystem store with project files
 * @param {Object} domainStore - Domain ontology store with entities
 * @param {Object} [stackProfile] - Optional stack detection info
 * @returns {{store: Store, summary: {templateCount: number, byKind: Record<string, number>, boundEntities: number}}}
 */
export function inferTemplatesWithDomainBinding(fsStore, domainStore, stackProfile) {
  const { store, summary } = inferTemplatesFromProject(fsStore, domainStore, stackProfile);

  let boundEntities = 0;

  // Try to bind templates to domain entities
  if (domainStore) {
    try {
      // Get domain entities (classes)
      const entityQuads = domainStore.getQuads(
        null,
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.rdfs}Class`)
      );

      const entities = entityQuads.map(q => q.subject.value);

      // Get all templates from store
      const templateQuads = store.getQuads(
        null,
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.gen}Template`)
      );

      // For each template, try to find matching entities
      for (const templateQuad of templateQuads) {
        const templateIri = templateQuad.subject;

        // Get examples for this template
        const exampleQuads = store.getQuads(templateIri, namedNode(`${NS.gen}example`), null);

        for (const exampleQuad of exampleQuads) {
          const examplePath = exampleQuad.object.value;

          // Try to match entity names in the path
          for (const entityIri of entities) {
            const entityName = entityIri.split(/[#/]/).pop();
            if (entityName && examplePath.toLowerCase().includes(entityName.toLowerCase())) {
              // Add binding
              store.addQuad(templateIri, namedNode(`${NS.gen}targetsClass`), namedNode(entityIri));
              boundEntities++;
              break;
            }
          }
        }
      }
    } catch (e) {
      // Ignore binding errors, templates still work
    }
  }

  return {
    store,
    summary: {
      ...summary,
      boundEntities,
    },
  };
}

/**
 * Get templates by kind from store
 *
 * @param {Store} store - Template store
 * @param {string} kind - Template kind (Component, Page, Test, etc.)
 * @returns {Array<{iri: string, outputPattern: string, variantCount: number}>}
 */
export function getTemplatesByKind(store, kind) {
  const templates = [];

  const templateQuads = store.getQuads(null, namedNode(`${NS.gen}templateKind`), literal(kind));

  for (const quad of templateQuads) {
    const templateIri = quad.subject.value;

    // Get output pattern
    const patternQuads = store.getQuads(quad.subject, namedNode(`${NS.gen}outputPattern`), null);
    const outputPattern = patternQuads[0]?.object.value || '';

    // Get variant count
    const countQuads = store.getQuads(quad.subject, namedNode(`${NS.gen}variantCount`), null);
    const variantCount = parseInt(countQuads[0]?.object.value || '0', 10);

    templates.push({
      iri: templateIri,
      outputPattern,
      variantCount,
    });
  }

  return templates;
}

/**
 * Serialize templates to a plain object for debugging/export
 *
 * @param {Store} store - Template store
 * @returns {Object[]}
 */
export function serializeTemplates(store) {
  const templates = [];

  const templateQuads = store.getQuads(
    null,
    namedNode(`${NS.rdf}type`),
    namedNode(`${NS.gen}Template`)
  );

  for (const quad of templateQuads) {
    const iri = quad.subject;

    const template = {
      id: iri.value.split('#').pop(),
      iri: iri.value,
    };

    // Get all properties
    const propQuads = store.getQuads(iri, null, null);

    for (const pq of propQuads) {
      const propName = pq.predicate.value.split('#').pop();
      const value = pq.object.value;

      if (propName === 'type') continue;

      if (['variable', 'invariant', 'example'].includes(propName)) {
        if (!template[propName]) template[propName] = [];
        template[propName].push(value);
      } else {
        template[propName] = value;
      }
    }

    templates.push(template);
  }

  return templates;
}
