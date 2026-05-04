/**
 * @file Validate Command
 * @module cli/commands/validate
 * @description Validate RDF files and extract summary statistics
 */
import { defineCommand } from 'citty';
import { readFile } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve } from 'path';
import { createStore } from '@unrdf/core';

const c = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  red: '\x1b[31m',
};
const noColor = !process.stdout.isTTY || process.env.NO_COLOR;
if (noColor) Object.keys(c).forEach(k => (c[k] = ''));

/**
 * Validate an N-Triples file line by line
 * @param {string} filePath - Path to N-Triples file
 * @returns {Promise<Array<{line: number, error: string, content: string}>>} Array of errors
 */
export async function validateNT(filePath) {
  const content = await readFile(filePath, 'utf-8');
  const errors = [];
  const lines = content.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const lineNum = i + 1;
    const raw = lines[i];
    const stripped = raw.trim();

    // Skip empty lines and comments
    if (!stripped || stripped.startsWith('#')) {
      continue;
    }

    // Check for triple structure: subject predicate object .
    const parts = stripped.split(/\s+/);
    if (!stripped.endsWith('.')) {
      errors.push({
        line: lineNum,
        error: 'triple does not end with "."',
        content: stripped.substring(0, 60) + (stripped.length > 60 ? '...' : ''),
      });
    } else if (parts.length < 4) {
      errors.push({
        line: lineNum,
        error: 'incomplete triple (expected 3 terms + dot)',
        content: stripped.substring(0, 60) + (stripped.length > 60 ? '...' : ''),
      });
    }
  }

  return errors;
}

/**
 * Count triples in N-Triples file (fast line-based count)
 * @param {string} filePath - Path to N-Triples file
 * @returns {Promise<number>} Triple count
 */
export async function countTriples(filePath) {
  const content = await readFile(filePath, 'utf-8');
  let count = 0;

  const lines = content.split('\n');
  for (const line of lines) {
    const stripped = line.trim();
    if (stripped && !stripped.startsWith('#')) {
      count++;
    }
  }

  return count;
}

/**
 * Extract summary statistics from ontology
 * @param {Object} store - RDF store
 * @returns {Object} Summary statistics
 */
export async function extractSummary(store) {
  const summary = {
    total_triples: 0,
    classes: 0,
    properties: 0,
    shapes: 0,
    concepts: 0,
    policies: 0,
    namespaces: [],
  };

  // Get total triples
  if (typeof store.size === 'number') {
    summary.total_triples = store.size;
  }

  // Query for different entity types
  const queries = {
    classes: `
      SELECT (COUNT(*) as ?count) WHERE {
        ?s a ?type .
        FILTER (?type = <http://www.w3.org/2002/07/owl#Class> ||
                ?type = <http://www.w3.org/2000/01/rdf-schema#Class>)
      }
    `,
    properties: `
      SELECT (COUNT(*) as ?count) WHERE {
        ?s a ?type .
        FILTER (?type = <http://www.w3.org/2002/07/owl#ObjectProperty> ||
                ?type = <http://www.w3.org/2002/07/owl#DatatypeProperty>)
      }
    `,
    shapes: `
      SELECT (COUNT(*) as ?count) WHERE {
        ?s a <http://www.w3.org/ns/shacl#NodeShape> .
      }
    `,
    concepts: `
      SELECT (COUNT(*) as ?count) WHERE {
        ?s a <http://www.w3.org/2004/02/skos/core#Concept> .
      }
    `,
    policies: `
      SELECT (COUNT(*) as ?count) WHERE {
        ?s a <http://www.w3.org/ns/odrl/2#Policy> .
      }
    `,
  };

  // Execute queries if store supports SPARQL
  if (typeof store.query === 'function') {
    try {
      for (const [key, query] of Object.entries(queries)) {
        const result = await store.query(query);
        if (result && result.length > 0) {
          summary[key] = parseInt(result[0].count || result[0]['?count'] || 0, 10);
        }
      }
    } catch (queryErr) {
      // If SPARQL queries fail, continue with partial summary
      console.log(c.dim + '  Note: SPARQL queries failed, showing basic summary only' + c.reset);
    }
  }

  // Extract namespaces
  const namespaces = new Set();
  if (typeof store.match === 'function') {
    try {
      const subjects = store.match(null, null, null);
      for (const triple of subjects) {
        const subj = triple.subject?.value || triple.subject;
        if (subj && typeof subj === 'string' && subj.includes('#')) {
          const ns = subj.split('#')[0] + '#';
          namespaces.add(ns);
        }
      }
    } catch (matchErr) {
      // If match fails, skip namespace extraction
    }
  }
  summary.namespaces = Array.from(namespaces).sort();

  return summary;
}

/**
 * Validate command
 */
export const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate RDF files and extract summary statistics',
  },
  args: {
    file: {
      type: 'string',
      description: 'RDF file to validate',
      required: true,
    },
    format: {
      type: 'string',
      description: 'RDF format (ntriples, turtle, rdfxml, jsonld)',
      default: 'ntriples',
    },
    summary: {
      type: 'boolean',
      description: 'Extract and display summary statistics',
      default: false,
    },
    json: {
      type: 'boolean',
      description: 'Output results as JSON',
      default: false,
    },
  },
  async run({ args }) {
    const filePath = resolve(args.file);

    if (!existsSync(filePath)) {
      console.error(`${c.red}Error:${c.reset} File not found: ${filePath}`);
      process.exit(1);
    }

    const result = {
      file: filePath,
      format: args.format,
      valid: true,
      errors: [],
      summary: null,
    };

    try {
      console.log(`${c.bold}UNRDF Validate${c.reset}\n`);
      console.log(`${c.cyan}File:${c.reset} ${filePath}`);
      console.log(`${c.cyan}Format:${c.reset} ${args.format}\n`);

      // Phase 1: Line-by-line validation (for N-Triples)
      if (args.format === 'ntriples') {
        console.log(`${c.cyan}Phase 1:${c.reset} Validating N-Triples syntax...`);

        const errors = await validateNT(filePath);
        result.errors = errors;

        if (errors.length > 0) {
          result.valid = false;

          console.log(
            `   ${c.red}✗${c.reset} Found ${c.bold}${errors.length}${c.reset} validation error(s):\n`
          );

          for (const err of errors.slice(0, 10)) {
            // Show first 10 errors
            console.log(
              `   ${c.yellow}Line ${err.line}:${c.reset} ${err.error}\n` +
                `     ${c.dim}${err.content}${c.reset}\n`
            );
          }

          if (errors.length > 10) {
            console.log(
              `   ${c.dim}... and ${errors.length - 10} more errors${c.reset}\n`
            );
          }
        } else {
          console.log(`   ${c.green}✓${c.reset} No syntax errors\n`);
        }

        // Count triples
        const tripleCount = await countTriples(filePath);
        console.log(`${c.cyan}Triples:${c.reset} ${tripleCount}\n`);
      }

      // Phase 2: Load into store and extract summary (if requested)
      if (args.summary || args.format !== 'ntriples') {
        console.log(`${c.cyan}Phase 2:${c.reset} Loading ontology and extracting summary...`);

        try {
          const store = createStore();
          const content = await readFile(filePath, 'utf-8');

          const FORMAT_TO_MIME = {
            ntriples: 'application/n-triples',
            turtle: 'text/turtle',
            rdfxml: 'application/rdf+xml',
            jsonld: 'application/ld+json',
          };

          const mimeType = FORMAT_TO_MIME[args.format] || args.format;

          if (typeof store.load === 'function') {
            await store.load(content, { format: mimeType });
          }

          if (args.summary) {
            const summary = await extractSummary(store);
            result.summary = summary;

            console.log(`   ${c.green}✓${c.reset} Summary extracted:\n`);
            console.log(`   ${c.cyan}Total triples:${c.reset} ${summary.total_triples}`);
            console.log(`   ${c.cyan}Classes:${c.reset} ${summary.classes}`);
            console.log(`   ${c.cyan}Properties:${c.reset} ${summary.properties}`);
            console.log(`   ${c.cyan}Shapes:${c.reset} ${summary.shapes}`);
            console.log(`   ${c.cyan}Concepts:${c.reset} ${summary.concepts}`);
            console.log(`   ${c.cyan}Policies:${c.reset} ${summary.policies}`);

            if (summary.namespaces.length > 0) {
              console.log(`   ${c.cyan}Namespaces:${c.reset}`);
              for (const ns of summary.namespaces) {
                console.log(`     - ${ns}`);
              }
            }
          } else {
            console.log(`   ${c.green}✓${c.reset} Ontology loaded successfully\n`);
          }
        } catch (loadErr) {
          result.valid = false;
          console.log(`   ${c.red}✗${c.reset} Failed to load ontology: ${loadErr.message}\n`);
        }
      }

      // Final result
      if (result.valid) {
        console.log(`${c.green}✓ Valid:${c.reset} ${filePath} is valid\n`);
      } else {
        console.log(`${c.red}✗ Invalid:${c.reset} ${filePath} has errors\n`);
      }

      // JSON output if requested
      if (args.json) {
        console.log(JSON.stringify(result, null, 2));
      }

      process.exit(result.valid ? 0 : 1);
    } catch (err) {
      console.error(`${c.red}Error:${c.reset} ${err.message}`);
      if (args.json) {
        console.log(JSON.stringify({ ...result, valid: false, error: err.message }, null, 2));
      }
      process.exit(1);
    }
  },
});

export default validateCommand;
