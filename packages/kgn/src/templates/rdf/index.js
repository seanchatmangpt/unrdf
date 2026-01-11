/**
 * @file RDF Templates Index
 * @description Exports all RDF template paths for programmatic use
 */

import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * RDF template definitions
 */
export const RDF_TEMPLATES = {
  ontology: {
    name: 'ontology.njk',
    path: join(__dirname, 'ontology.njk'),
    description: 'OWL ontology template with classes and properties',
    requiredVars: ['ontologyIRI', 'title']
  },
  schema: {
    name: 'schema.njk',
    path: join(__dirname, 'schema.njk'),
    description: 'RDFS vocabulary schema template',
    requiredVars: ['schemaIRI', 'title']
  },
  dataset: {
    name: 'dataset.njk',
    path: join(__dirname, 'dataset.njk'),
    description: 'DCAT dataset metadata template',
    requiredVars: ['datasetIRI', 'title', 'description']
  },
  vocabulary: {
    name: 'vocabulary.njk',
    path: join(__dirname, 'vocabulary.njk'),
    description: 'SKOS concept scheme template',
    requiredVars: ['schemeIRI', 'title']
  },
  shapes: {
    name: 'shapes.njk',
    path: join(__dirname, 'shapes.njk'),
    description: 'SHACL validation shapes template',
    requiredVars: ['shapesGraphIRI', 'shapes']
  },
  sparqlQueries: {
    name: 'sparql-queries.njk',
    path: join(__dirname, 'sparql-queries.njk'),
    description: 'SPARQL query collection template',
    requiredVars: ['title', 'queries']
  },
  jsonldContext: {
    name: 'jsonld-context.njk',
    path: join(__dirname, 'jsonld-context.njk'),
    description: 'JSON-LD context mapping template',
    requiredVars: ['terms']
  }
};

/**
 * Get template path by name
 * @param {string} name - Template name (e.g., 'ontology', 'schema')
 * @returns {string} Full path to template file
 */
export function getTemplatePath(name) {
  const template = RDF_TEMPLATES[name];
  if (!template) {
    throw new Error(`Unknown RDF template: ${name}`);
  }
  return template.path;
}

/**
 * List all available RDF templates
 * @returns {Array<{name: string, description: string, requiredVars: string[]}>}
 */
export function listTemplates() {
  return Object.entries(RDF_TEMPLATES).map(([key, value]) => ({
    key,
    name: value.name,
    description: value.description,
    requiredVars: value.requiredVars
  }));
}

/**
 * Validate data against template requirements
 * @param {string} templateName - Template name
 * @param {Object} data - Data to validate
 * @returns {{valid: boolean, missing: string[]}}
 */
export function validateTemplateData(templateName, data) {
  const template = RDF_TEMPLATES[templateName];
  if (!template) {
    return { valid: false, missing: ['Unknown template'] };
  }

  const missing = template.requiredVars.filter(varName => !data[varName]);

  return {
    valid: missing.length === 0,
    missing
  };
}

export default RDF_TEMPLATES;
