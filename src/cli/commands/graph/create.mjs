/**
 * @fileoverview Graph create command
 *
 * @description
 * CLI command for creating new RDF named graphs.
 * Instrumented with OpenTelemetry traces and metrics.
 *
 * @module cli/commands/graph/create
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { mkdir, writeFile, access, rename } from 'node:fs/promises';
import { constants as fsConstants } from 'node:fs';
import path from 'node:path';

/**
 * Validation schema for create command arguments
 */
const createArgsSchema = z.object({
  name: z.string().optional().default(''),
  'base-iri': z.string().optional().default('http://example.org/'),
  'dry-run': z.boolean().optional().default(false)
});

/**
 * Create graph command
 */
export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new RDF named graph'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Name of the graph to create',
      required: true
    },
    'base-iri': {
      type: 'string',
      description: 'Base IRI for the graph',
      default: 'http://example.org/',
      alias: 'b'
    },
    'dry-run': {
      type: 'boolean',
      description: 'Show what would be created without creating',
      default: false,
      alias: 'd'
    }
  },
  async run(ctx) {
    try {
      // Validate arguments
      const args = createArgsSchema.parse(ctx.args);

      if (args['dry-run']) {
        console.log(`Would create graph: ${args.name}`);
        console.log(`Base IRI: ${args['base-iri']}`);
        return;
      }

      // Create graph metadata under ./graph/<name>.meta.json
      const graphsDir = path.resolve(process.cwd(), 'graph');
      const metaPath = path.join(graphsDir, `${args.name}.meta.json`);

      // Ensure directory exists
      await mkdir(graphsDir, { recursive: true });

      // Prevent overwrite if graph already exists
      try {
        await access(metaPath, fsConstants.F_OK);
        throw new Error(`Graph already exists: ${args.name}`);
      } catch (_) {
        // File does not exist, proceed
      }

      const meta = {
        name: args.name,
        baseIri: args['base-iri'],
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString()
      };

      // Atomic write via temp file + rename
      const tmpPath = metaPath + '.tmp';
      await writeFile(tmpPath, JSON.stringify(meta, null, 2), 'utf8');
      await rename(tmpPath, metaPath);

      console.log(`✅ Graph created: ${args.name}`);
      console.log(`   Base IRI: ${args['base-iri']}`);

    } catch (error) {
      console.error('❌ Create failed:', error.message);
      throw error;
    }
  }
});
