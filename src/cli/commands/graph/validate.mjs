/**
 * @fileoverview Graph validate command
 *
 * @description
 * CLI command for validating RDF graphs against SHACL shapes or policies.
 * Instrumented with OpenTelemetry traces and metrics.
 *
 * @module cli/commands/graph/validate
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';

/**
 * Validation schema for validate command arguments
 */
const validateArgsSchema = z.object({
  name: z.string().optional().default(''),
  policy: z.string().optional(),
  output: z.string().optional().default('table'),
});

/**
 * Validate graph command
 */
export const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate a graph against SHACL shapes or policies',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Name of the graph to validate',
      required: true,
    },
    policy: {
      type: 'string',
      description: 'Policy or SHACL shape to validate against',
      alias: 'p',
    },
    output: {
      type: 'string',
      description: 'Output format (table, json, yaml)',
      default: 'table',
      alias: 'o',
    },
  },
  async run(ctx) {
    try {
      // Validate arguments
      const args = validateArgsSchema.parse(ctx.args);

      console.log(`Validating graph: ${args.name}`);

      // Integrate with real SHACL validation from knowledge-engine
      const { validateShacl } = await import('../../../knowledge-engine/validate.mjs');
      const { Store, Parser } = await import('n3');
      const { promises: fs } = await import('fs');
      const path = await import('path');

      // Load graph data - try sidecar first, then fallback to local file
      let store = await createStore();
      try {
        const { createSidecarClient } = await import('../../../sidecar/client.mjs');
        const client = createSidecarClient();
        await client.connect();

        // Query sidecar for graph data using SPARQL
        const query = `CONSTRUCT { ?s ?p ?o } WHERE { GRAPH <${args.name}> { ?s ?p ?o } }`;
        const result = await client.query(query);

        // Convert result to store
        if (result && Array.isArray(result)) {
          for (const quad of result) {
            store.addQuad(quad);
          }
        }
      } catch (sidecarError) {
        // Fallback: try to load from local file
        const graphPath = path.resolve(args.name);
        if (
          await fs
            .access(graphPath)
            .then(() => true)
            .catch(() => false)
        ) {
          const content = await fs.readFile(graphPath, 'utf-8');
          const parser = new Parser();
          store = new Store(parser.parse(content));
        } else {
          throw new Error(`Graph "${args.name}" not found in sidecar or local filesystem`);
        }
      }

      // Load SHACL shapes from policy file
      if (!args.policy) {
        throw new Error('Policy file required: use --policy <path>');
      }

      const shapesPath = path.resolve(args.policy);
      const shapes = await fs.readFile(shapesPath, 'utf-8');

      // Perform real SHACL validation
      const validationResult = validateShacl(store, shapes, {
        strict: true,
        includeDetails: true,
      });

      const results = {
        conforms: validationResult.conforms,
        violations: validationResult.results.filter(
          r => r.severity === 'http://www.w3.org/ns/shacl#Violation'
        ),
        warnings: validationResult.results.filter(
          r => r.severity === 'http://www.w3.org/ns/shacl#Warning'
        ),
        totalResults: validationResult.results.length,
      };

      // Output results based on format
      if (args.output === 'json') {
        console.log(JSON.stringify(results, null, 2));
      } else if (args.output === 'yaml') {
        console.log('conforms:', results.conforms);
        console.log('violations:', results.violations.length);
        console.log('warnings:', results.warnings.length);
        if (results.violations.length > 0) {
          console.log('\nviolations:');
          results.violations.forEach((v, i) => {
            console.log(`  - ${i + 1}: ${v.message || JSON.stringify(v)}`);
          });
        }
      } else {
        // Table format (default)
        if (results.conforms) {
          console.log('✅ Validation passed');
          if (results.warnings.length > 0) {
            console.log(`   Warnings: ${results.warnings.length}`);
          }
        } else {
          console.log('❌ Validation failed');
          console.log(`   Total issues: ${results.totalResults}`);
          console.log(`   Violations: ${results.violations.length}`);
          console.log(`   Warnings: ${results.warnings.length}`);
          if (results.violations.length > 0) {
            console.log('\nViolations:');
            results.violations.forEach((v, i) => {
              const msg = v.message || v.path || JSON.stringify(v);
              console.log(`   ${i + 1}. ${msg}`);
              if (v.focusNode) console.log(`      Focus node: ${v.focusNode}`);
              if (v.path) console.log(`      Path: ${v.path}`);
            });
          }
        }
      }
    } catch (error) {
      console.error('❌ Validation failed:', error.message);
      throw error;
    }
  },
});
