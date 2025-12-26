/**
 * @fileoverview Lens schema, validation, and definition
 * @module @unrdf/lens/lens
 */

import { z } from 'zod';

/**
 * @typedef {Object} LensProfile
 * @property {string} namespace - Base IRI namespace (e.g., "https://example.org/")
 * @property {Object<string, string>} prefixes - Prefix mappings { schema: "http://schema.org/" }
 * @property {Object} conventions - Naming conventions { idField: "id", case: "camelCase" }
 */

/**
 * @typedef {Object} PredicateMapping
 * @property {string} iri - Full IRI or prefixed (schema:name)
 * @property {string} [datatype] - XSD datatype (xsd:string, xsd:integer)
 * @property {boolean} [required] - Field required in payload
 * @property {string} [transformId] - Transform function ID (for serialization)
 */

/**
 * @typedef {Object} SubjectRule
 * @property {string} pattern - IRI pattern with placeholders: "{namespace}{type}/{id}"
 * @property {string[]} keys - Fields used for IRI generation ["id"] or ["type", "localId"]
 * @property {string} [skolemPattern] - Optional skolem pattern for blank nodes
 */

/**
 * @typedef {Object} EntityMapping
 * @property {SubjectRule} subject - How to generate subject IRI
 * @property {Object<string, PredicateMapping>} predicates - Property → predicate mappings
 * @property {string} [type] - RDF type IRI (e.g., "schema:Customer")
 */

/**
 * @typedef {Object} Lens
 * @property {string} id - Stable lens identifier
 * @property {string} version - Semantic version
 * @property {LensProfile} profile - Namespace + conventions
 * @property {Object<string, EntityMapping>} mappings - Entity type → mapping rules
 * @property {Object} [metadata] - Optional metadata (author, description)
 */

// Zod schemas for validation
const PredicateMappingSchema = z.object({
  iri: z.string().min(1),
  datatype: z.string().optional(),
  required: z.boolean().optional(),
  transformId: z.string().optional(),
});

const SubjectRuleSchema = z.object({
  pattern: z.string().min(1),
  keys: z.array(z.string()).min(1),
  skolemPattern: z.string().optional(),
});

const EntityMappingSchema = z.object({
  subject: SubjectRuleSchema,
  predicates: z.record(z.string(), PredicateMappingSchema),
  type: z.string().optional(),
});

const LensProfileSchema = z.object({
  namespace: z.string().url(),
  prefixes: z.record(z.string(), z.string().url()),
  conventions: z.object({
    idField: z.string().default('id'),
    case: z.enum(['camelCase', 'snake_case', 'PascalCase']).optional(),
  }),
});

const LensSchema = z.object({
  id: z.string().min(1),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  profile: LensProfileSchema,
  mappings: z.record(z.string(), EntityMappingSchema),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Normalize prefixed IRIs to full URIs
 * @param {string} iri - Potentially prefixed IRI
 * @param {Object<string, string>} prefixes - Prefix mappings
 * @returns {string} Full IRI
 */
function expandPrefixedIRI(iri, prefixes) {
  // Check if it's already a full IRI (contains ://)
  if (iri.includes('://')) {
    return iri;
  }

  // Check for prefix:localName pattern
  const colonIndex = iri.indexOf(':');
  if (colonIndex > 0) {
    const prefix = iri.substring(0, colonIndex);
    const localName = iri.substring(colonIndex + 1);

    if (prefixes[prefix]) {
      return prefixes[prefix] + localName;
    }
  }

  // Return as-is if no expansion possible
  return iri;
}

/**
 * Validate IRI pattern syntax
 * @param {string} pattern - Pattern to validate
 * @throws {Error} If pattern is invalid
 */
function validateIRIPattern(pattern) {
  // Check for valid placeholder syntax: {placeholder}
  const placeholderRegex = /\{([a-zA-Z_][a-zA-Z0-9_]*)\}/g;
  const matches = pattern.match(placeholderRegex);

  if (!matches) {
    throw new Error(`Invalid IRI pattern: ${pattern} - must contain at least one placeholder like {id}`);
  }

  // Validate that pattern will produce valid IRI structure
  if (!pattern.includes('{namespace}') && !pattern.startsWith('http://') && !pattern.startsWith('https://') && !pattern.startsWith('urn:')) {
    throw new Error(`Invalid IRI pattern: ${pattern} - must include {namespace} or start with protocol`);
  }
}

/**
 * Normalize predicate mappings (expand IRIs, sort keys)
 * @param {Object<string, PredicateMapping>} predicates - Predicate mappings
 * @param {Object<string, string>} prefixes - Prefix mappings
 * @returns {Object<string, PredicateMapping>} Normalized predicates
 */
function normalizePredicates(predicates, prefixes) {
  const normalized = {};
  const sortedKeys = Object.keys(predicates).sort();

  for (const key of sortedKeys) {
    const predicate = predicates[key];
    normalized[key] = {
      ...predicate,
      iri: expandPrefixedIRI(predicate.iri, prefixes),
      datatype: predicate.datatype ? expandPrefixedIRI(predicate.datatype, prefixes) : undefined,
    };
  }

  return normalized;
}

/**
 * Define a new lens with validation
 * @param {string} id - Lens identifier
 * @param {LensProfile} profile - Namespace + conventions
 * @param {Object<string, EntityMapping>} mappings - Entity mappings
 * @param {Object} [metadata] - Optional metadata
 * @returns {Lens} Validated and normalized lens
 */
export function defineLens(id, profile, mappings, metadata = {}) {
  // Construct lens object
  const lens = {
    id,
    version: '1.0.0',
    profile,
    mappings,
    metadata,
  };

  // Validate with Zod
  const validated = LensSchema.parse(lens);

  // Additional validation: IRI patterns
  for (const [entityType, mapping] of Object.entries(validated.mappings)) {
    try {
      validateIRIPattern(mapping.subject.pattern);
    } catch (error) {
      throw new Error(`Invalid subject pattern for entity ${entityType}: ${error.message}`);
    }

    // Validate that placeholder keys are sensible
    for (const key of mapping.subject.keys) {
      if (!key.match(/^[a-zA-Z_][a-zA-Z0-9_]*$/)) {
        throw new Error(`Invalid subject key for entity ${entityType}: ${key}`);
      }
    }
  }

  // Normalize: expand prefixed IRIs
  const normalized = {
    ...validated,
    mappings: {},
  };

  const sortedEntityTypes = Object.keys(validated.mappings).sort();

  for (const entityType of sortedEntityTypes) {
    const mapping = validated.mappings[entityType];
    normalized.mappings[entityType] = {
      subject: mapping.subject,
      predicates: normalizePredicates(mapping.predicates, validated.profile.prefixes),
      type: mapping.type ? expandPrefixedIRI(mapping.type, validated.profile.prefixes) : undefined,
    };
  }

  // Return frozen object (immutable)
  return Object.freeze(normalized);
}

/**
 * Export schemas for external validation
 */
export const schemas = {
  Lens: LensSchema,
  LensProfile: LensProfileSchema,
  EntityMapping: EntityMappingSchema,
  SubjectRule: SubjectRuleSchema,
  PredicateMapping: PredicateMappingSchema,
};
