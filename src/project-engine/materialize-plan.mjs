/**
 * @file Materialization planner - convert ontology + templates to file write plans
 * @module project-engine/materialize-plan
 */

import { z } from 'zod';
import { createHash } from 'crypto';
import { DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode } = DataFactory;

/**
 * @typedef {import('n3').Store} Store
 */

/**
 * @typedef {Object} WriteOperation
 * @property {string} path - Output file path
 * @property {string} content - File content to write
 * @property {string} hash - SHA256 hash of content
 * @property {string} templateIri - Source template IRI
 * @property {string} entityIri - Domain entity IRI
 * @property {string} entityType - Type of domain entity
 */

/**
 * @typedef {Object} UpdateOperation
 * @property {string} path - File path to update
 * @property {string} content - New content
 * @property {string} oldHash - Hash of existing content
 * @property {string} newHash - Hash of new content
 * @property {string} templateIri - Source template IRI
 * @property {string} entityIri - Domain entity IRI
 */

/**
 * @typedef {Object} DeleteOperation
 * @property {string} path - File path to delete
 * @property {string} hash - Hash of content being deleted
 * @property {string} reason - Reason for deletion
 */

/**
 * @typedef {Object} MappingRecord
 * @property {string} templateIri - Template that generated this file
 * @property {string} entityIri - Entity used for substitution
 * @property {string} outputPath - Generated file path
 * @property {string} contentHash - Hash of generated content
 */

/**
 * @typedef {Object} MaterializationPlan
 * @property {WriteOperation[]} writes - New files to create
 * @property {UpdateOperation[]} updates - Existing files to update
 * @property {DeleteOperation[]} deletes - Files to remove
 */

/**
 * @typedef {Object} MaterializationReceipt
 * @property {string} ontologyHash - Hash of input ontology
 * @property {string} templateHash - Hash of template graph
 * @property {string} planHash - Hash of the plan
 * @property {MappingRecord[]} mappings - Template-to-output mappings
 * @property {string} timestamp - ISO timestamp
 */

/**
 * @typedef {Object} PlanResult
 * @property {MaterializationPlan} plan
 * @property {MaterializationReceipt} receipt
 */

const _TemplateNodeSchema = z.object({
  iri: z.string(),
  targetsClass: z.string(),
  outputPattern: z.string(),
  content: z.string(),
  extension: z.string().optional(),
});

const PlanOptionsSchema = z.object({
  outputRoot: z.string().default('.'),
  dryRun: z.boolean().default(false),
  existingFiles: z.record(z.string(), z.string()).default({}),
});

/**
 * Variable substitution patterns for templates
 * @type {Record<string, (name: string) => string>}
 */
const VARIABLE_PATTERNS = {
  '{{entity}}': name => name,
  '{{Entity}}': name => name.charAt(0).toUpperCase() + name.slice(1),
  '{{ENTITY}}': name => name.toUpperCase(),
  '{{entity_snake}}': name =>
    name
      .replace(/([A-Z])/g, '_$1')
      .toLowerCase()
      .replace(/^_/, ''),
  '{{entity-kebab}}': name =>
    name
      .replace(/([A-Z])/g, '-$1')
      .toLowerCase()
      .replace(/^-/, ''),
};

/**
 * Extract domain entities from ontology store
 *
 * @param {Store} store - Ontology store
 * @returns {Array<{iri: string, type: string, label: string}>}
 */
function extractDomainEntities(store) {
  const entities = [];
  const RDF_TYPE = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
  const RDFS_LABEL = namedNode('http://www.w3.org/2000/01/rdf-schema#label');

  const typeQuads = store.getQuads(null, RDF_TYPE, null, null);

  for (const quad of typeQuads) {
    const subjectIri = quad.subject.value;
    const typeIri = quad.object.value;

    // Skip blank nodes and non-domain types
    if (subjectIri.startsWith('_:')) continue;
    if (typeIri.includes('rdf-syntax-ns') || typeIri.includes('rdf-schema')) continue;

    // Get label if exists
    const labelQuads = store.getQuads(quad.subject, RDFS_LABEL, null, null);
    const label = labelQuads.length > 0 ? labelQuads[0].object.value : extractLocalName(subjectIri);

    entities.push({
      iri: subjectIri,
      type: typeIri,
      label,
    });
  }

  return entities;
}

/**
 * Extract templates from template graph
 *
 * @param {Store} store - Template graph store
 * @returns {Array<z.infer<typeof TemplateNodeSchema>>}
 */
function extractTemplates(store) {
  const templates = [];
  const TEMPLATE_TYPE = namedNode('http://example.org/unrdf/template#Template');
  const TARGETS_CLASS = namedNode('http://example.org/unrdf/template#targetsClass');
  const OUTPUT_PATTERN = namedNode('http://example.org/unrdf/template#outputPattern');
  const CONTENT = namedNode('http://example.org/unrdf/template#content');
  const EXTENSION = namedNode('http://example.org/unrdf/template#extension');
  const RDF_TYPE = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

  const templateQuads = store.getQuads(null, RDF_TYPE, TEMPLATE_TYPE, null);

  for (const quad of templateQuads) {
    const templateSubject = quad.subject;

    const targetsQuads = store.getQuads(templateSubject, TARGETS_CLASS, null, null);
    const patternQuads = store.getQuads(templateSubject, OUTPUT_PATTERN, null, null);
    const contentQuads = store.getQuads(templateSubject, CONTENT, null, null);
    const extQuads = store.getQuads(templateSubject, EXTENSION, null, null);

    if (targetsQuads.length === 0 || patternQuads.length === 0 || contentQuads.length === 0) {
      continue;
    }

    templates.push({
      iri: templateSubject.value,
      targetsClass: targetsQuads[0].object.value,
      outputPattern: patternQuads[0].object.value,
      content: contentQuads[0].object.value,
      extension: extQuads.length > 0 ? extQuads[0].object.value : undefined,
    });
  }

  return templates;
}

/**
 * Extract local name from IRI
 *
 * @param {string} iri
 * @returns {string}
 */
function extractLocalName(iri) {
  const hashIdx = iri.lastIndexOf('#');
  if (hashIdx !== -1) return iri.slice(hashIdx + 1);
  const slashIdx = iri.lastIndexOf('/');
  if (slashIdx !== -1) return iri.slice(slashIdx + 1);
  return iri;
}

/**
 * Substitute variables in pattern
 *
 * @param {string} pattern - Pattern with {{entity}} placeholders
 * @param {string} entityName - Entity name to substitute
 * @returns {string}
 */
function substituteVariables(pattern, entityName) {
  let result = pattern;

  for (const [placeholder, transform] of Object.entries(VARIABLE_PATTERNS)) {
    result = result.replace(
      new RegExp(placeholder.replace(/[{}]/g, '\\$&'), 'g'),
      transform(entityName)
    );
  }

  return result;
}

/**
 * Compute SHA256 hash of content
 *
 * @param {string} content
 * @returns {string}
 */
function hashContent(content) {
  return createHash('sha256').update(content).digest('hex');
}

/**
 * Hash an N3 store by serializing quad count and sample quads
 *
 * @param {Store} store
 * @returns {string}
 */
function hashStore(store) {
  const hash = createHash('sha256');
  const size = store.size || 0;
  hash.update(`size:${size}`);

  const quads = store.getQuads(null, null, null, null);
  const sample = quads.slice(0, Math.min(10, quads.length));

  for (const q of sample) {
    hash.update(`${q.subject.value}|${q.predicate.value}|${q.object.value}`);
  }

  return hash.digest('hex').substring(0, 16);
}

/**
 * Plan materialization from ontology + templates
 *
 * For each domain entity in ontologyStore:
 *   For each template in templateGraph:
 *     If template.targetsClass matches entity type:
 *       Generate output path by substituting variables
 *       Create write plan with hash + provenance
 *
 * @param {Store} ontologyStore - Domain ontology with entities
 * @param {Store} templateGraph - Template definitions
 * @param {Object} [options] - Planning options
 * @param {string} [options.outputRoot] - Base directory for outputs
 * @param {boolean} [options.dryRun] - If true, don't check existing files
 * @param {Record<string, string>} [options.existingFiles] - Map of path -> content hash
 * @returns {PlanResult}
 */
export function planMaterialization(ontologyStore, templateGraph, options = {}) {
  const opts = PlanOptionsSchema.parse(options);

  const entities = extractDomainEntities(ontologyStore);
  const templates = extractTemplates(templateGraph);

  /** @type {WriteOperation[]} */
  const writes = [];
  /** @type {UpdateOperation[]} */
  const updates = [];
  /** @type {DeleteOperation[]} */
  const deletes = [];
  /** @type {MappingRecord[]} */
  const mappings = [];

  for (const entity of entities) {
    for (const template of templates) {
      // Check if template targets this entity type
      if (template.targetsClass !== entity.type) {
        continue;
      }

      const entityName = entity.label;
      const outputPath = substituteVariables(template.outputPattern, entityName);
      const fullPath = opts.outputRoot === '.' ? outputPath : `${opts.outputRoot}/${outputPath}`;

      // Substitute content variables
      const content = substituteVariables(template.content, entityName);
      const contentHash = hashContent(content);

      // Create mapping record
      const mapping = {
        templateIri: template.iri,
        entityIri: entity.iri,
        outputPath: fullPath,
        contentHash,
      };
      mappings.push(mapping);

      // Check if file exists
      const existingHash = opts.existingFiles[fullPath];

      if (existingHash === undefined) {
        // New file - add to writes
        writes.push({
          path: fullPath,
          content,
          hash: contentHash,
          templateIri: template.iri,
          entityIri: entity.iri,
          entityType: entity.type,
        });
      } else if (existingHash !== contentHash) {
        // File changed - add to updates
        updates.push({
          path: fullPath,
          content,
          oldHash: existingHash,
          newHash: contentHash,
          templateIri: template.iri,
          entityIri: entity.iri,
        });
      }
      // If hashes match, file is unchanged - no action needed
    }
  }

  const plan = { writes, updates, deletes };

  const ontologyHash = hashStore(ontologyStore);
  const templateHash = hashStore(templateGraph);
  const planHash = hashContent(
    JSON.stringify({
      writes: writes.length,
      updates: updates.length,
      deletes: deletes.length,
    })
  );

  const receipt = {
    ontologyHash,
    templateHash,
    planHash: planHash.substring(0, 16),
    mappings,
    timestamp: new Date().toISOString(),
  };

  return { plan, receipt };
}

/**
 * Validate a materialization plan
 *
 * Checks:
 * - No duplicate output paths
 * - All paths are relative (no absolute paths)
 * - No path traversal attacks
 *
 * @param {MaterializationPlan} plan
 * @returns {{valid: boolean, errors: string[]}}
 */
export function validatePlan(plan) {
  const errors = [];
  const seenPaths = new Set();

  const allOps = [...plan.writes, ...plan.updates];

  for (const op of allOps) {
    // Check for duplicates
    if (seenPaths.has(op.path)) {
      errors.push(`Duplicate output path: ${op.path}`);
    }
    seenPaths.add(op.path);

    // Check for absolute paths
    if (op.path.startsWith('/')) {
      errors.push(`Absolute path not allowed: ${op.path}`);
    }

    // Check for path traversal
    if (op.path.includes('..')) {
      errors.push(`Path traversal not allowed: ${op.path}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Create an empty plan
 *
 * @returns {MaterializationPlan}
 */
export function createEmptyPlan() {
  return {
    writes: [],
    updates: [],
    deletes: [],
  };
}

/**
 * Merge two plans together
 *
 * @param {MaterializationPlan} plan1
 * @param {MaterializationPlan} plan2
 * @returns {MaterializationPlan}
 */
export function mergePlans(plan1, plan2) {
  return {
    writes: [...plan1.writes, ...plan2.writes],
    updates: [...plan1.updates, ...plan2.updates],
    deletes: [...plan1.deletes, ...plan2.deletes],
  };
}
