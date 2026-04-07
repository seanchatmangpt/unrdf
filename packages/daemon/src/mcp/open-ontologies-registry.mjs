/**
 * @file Open-Ontologies MCP Tool Registry
 * @description Maps MCP tool names to open-ontologies CLI commands with metadata
 */

import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Open-ontologies binary location (installed via cargo install open-ontologies)
export const ONTO_BINARY = process.env.ONTO_BINARY || join(process.env.HOME || '', '.local', 'bin', 'open-ontologies');

// Open-ontologies data directory (SQLite store)
export const ONTO_DATA_DIR = process.env.ONTO_DATA_DIR || join(process.env.HOME || '', '.open-ontologies');

/**
 * Map MCP tool names to CLI command paths
 * Tool name → CLI command string (e.g., 'validate' → ['validate'])
 */
export const ontoRegistry = {
  // Phase 1: Core Tools
  onto_validate: ['validate'],
  onto_stats: ['stats'],
  onto_query: ['query'],
  onto_load: ['load'],
  onto_marketplace: ['marketplace'],

  // Phase 2: Advanced Features
  onto_reason: ['reason'],
  onto_shacl: ['shacl'],
  onto_save: ['save'],
  onto_clear: ['clear'],
  onto_convert: ['convert'],

  // Phase 3: Expert Features
  onto_align: ['align'],
  onto_drift: ['drift'],
  onto_plan: ['plan'],
  onto_apply: ['apply'],
  onto_version: ['version'],
};

/**
 * Tool metadata for MCP registration
 * Includes description, phase, and examples
 */
export const ontoRegistryMetadata = [
  // Phase 1: Core Tools
  {
    name: 'onto_validate',
    phase: 'core',
    description: 'Validate RDF/OWL syntax and semantics. Checks Turtle, N-Triples, JSON-LD, RDF/XML for correctness.',
    examples: ['onto_validate({file: "data.ttl"})', 'onto_validate({file: "schema.rdf", format: "rdfxml"})'],
  },
  {
    name: 'onto_stats',
    phase: 'core',
    description: 'Show statistics about loaded ontology: triple count, classes, properties, named individuals.',
    examples: ['onto_stats()', 'onto_stats({verbose: true})'],
  },
  {
    name: 'onto_query',
    phase: 'core',
    description: 'Execute SPARQL SELECT/CONSTRUCT/DESCRIBE queries on in-memory ontology store.',
    examples: ['onto_query({query: "SELECT ?s WHERE { ?s a rdf:Class }"})', 'onto_query({query: "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }", format: "ttl"})'],
  },
  {
    name: 'onto_load',
    phase: 'core',
    description: 'Load RDF ontology file into in-memory store. Supports Turtle, N-Triples, JSON-LD, RDF/XML.',
    examples: ['onto_load({file: "ontology.ttl"})', 'onto_load({file: "schema.rdf", format: "rdfxml"})'],
  },
  {
    name: 'onto_marketplace',
    phase: 'core',
    description: 'Browse and install 32 standard ontologies (FOAF, DC, SKOS, Schema.org, etc.) from marketplace.',
    examples: ['onto_marketplace()', 'onto_marketplace({search: "foaf"})', 'onto_marketplace({install: "foaf"})'],
  },

  // Phase 2: Advanced Features
  {
    name: 'onto_reason',
    phase: 'advanced',
    description: 'Perform RDFS/OWL-RL/OWL-DL reasoning to infer new triples from existing ontology.',
    examples: ['onto_reason({engine: "rdfs"})', 'onto_reason({engine: "owl-rl", limit: 1000})'],
  },
  {
    name: 'onto_shacl',
    phase: 'advanced',
    description: 'Validate RDF data against SHACL shapes. Returns constraint violations with details.',
    examples: ['onto_shacl({data: "instances.ttl", shapes: "shapes.ttl"})', 'onto_shacl({data: "-", shapes: "shapes.ttl"}, dataStream)'],
  },
  {
    name: 'onto_save',
    phase: 'advanced',
    description: 'Save in-memory ontology store to file. Supports Turtle, N-Triples, JSON-LD, RDF/XML.',
    examples: ['onto_save({file: "output.ttl"})', 'onto_save({file: "output.rdf", format: "rdfxml"})'],
  },
  {
    name: 'onto_clear',
    phase: 'advanced',
    description: 'Clear all triples from in-memory ontology store. Useful for resetting state.',
    examples: ['onto_clear()', 'onto_clear({confirm: true})'],
  },
  {
    name: 'onto_convert',
    phase: 'advanced',
    description: 'Convert RDF file between formats. Supports Turtle, N-Triples, JSON-LD, RDF/XML, N-Quads.',
    examples: ['onto_convert({input: "data.ttl", output: "data.jsonld", outputFormat: "jsonld"})'],
  },

  // Phase 3: Expert Features
  {
    name: 'onto_align',
    phase: 'expert',
    description: 'Detect ontology alignment candidates between two ontologies using string similarity and structure.',
    examples: ['onto_align({source: "ontology1.ttl", target: "ontology2.ttl", threshold: 0.8})'],
  },
  {
    name: 'onto_drift',
    phase: 'expert',
    description: 'Detect version drift between two ontology versions. Shows added/removed/modified classes and properties.',
    examples: ['onto_drift({old: "v1.ttl", new: "v2.ttl"})', 'onto_drift({old: "v1.ttl", new: "v2.ttl", detailed: true})'],
  },
  {
    name: 'onto_plan',
    phase: 'expert',
    description: 'Plan ontology changes with impact analysis. Shows what will change before applying.',
    examples: ['onto_plan({changes: "changes.ttl"})', 'onto_plan({changes: "changes.ttl", dryRun: true})'],
  },
  {
    name: 'onto_apply',
    phase: 'expert',
    description: 'Apply planned ontology changes. Creates new version with rollback support.',
    examples: ['onto_apply({plan: "plan.json"})', 'onto_apply({plan: "plan.json", backup: true})'],
  },
  {
    name: 'onto_version',
    phase: 'expert',
    description: 'Save ontology snapshots with version tags. Restore previous versions for rollback.',
    examples: ['onto_version({tag: "v1.0.0"})', 'onto_version({restore: "v1.0.0"})', 'onto_version({list: true})'],
  },
];

/**
 * Get CLI command path for tool name
 * @param {string} toolName - MCP tool name
 * @returns {string[]} CLI command arguments
 */
export function getOntoCommand(toolName) {
  return ontoRegistry[toolName] || null;
}

/**
 * Get tool metadata by name
 * @param {string} toolName - MCP tool name
 * @returns {object|null} Tool metadata or null
 */
export function getOntoMetadata(toolName) {
  return ontoRegistryMetadata.find(meta => meta.name === toolName) || null;
}

/**
 * Get tools by implementation phase
 * @param {string} phase - 'core' | 'advanced' | 'expert'
 * @returns {string[]} Tool names for phase
 */
export function getToolsByPhase(phase) {
  return ontoRegistryMetadata
    .filter(meta => meta.phase === phase)
    .map(meta => meta.name);
}
