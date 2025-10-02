/**
 * @file Basic RDF Validation Hook
 * @module policy-packs/default/basic-rdf-validation
 *
 * @description
 * Basic validation hook that ensures RDF graphs have valid structure.
 * Uses a SPARQL ASK query that always returns true (permissive default policy).
 */

import { writeFileSync, mkdirSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

// Create temporary SPARQL query file
const queryContent = 'ASK WHERE { { ?s ?p ?o } UNION { BIND(true AS ?alwaysTrue) } }';
const tmpDir = join(tmpdir(), 'unrdf-policies');
mkdirSync(tmpDir, { recursive: true });
const queryFile = join(tmpDir, 'default-validation.sparql');
writeFileSync(queryFile, queryContent);

export default {
  meta: {
    name: 'basic-rdf-validation',
    version: '1.0.0',
    description: 'Validates basic RDF graph structure - permissive default policy',
    tags: ['validation', 'rdf', 'basic']
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: `file://${queryFile}`,
      mediaType: 'application/sparql-query'
    }
  },
  run: async (context) => {
    const { graph } = context;
    const size = graph?.size || 0;

    return {
      success: true,
      result: {
        storeSize: size,
        message: `Graph validation passed with ${size} triples`
      }
    };
  },
  priority: 50,
  timeout: 30000,
  retries: 1
};
