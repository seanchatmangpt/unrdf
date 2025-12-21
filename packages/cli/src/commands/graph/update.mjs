/**
 * @fileoverview Graph update command
 *
 * @description
 * CLI command for updating RDF named graph properties.
 * Instrumented with OpenTelemetry traces and metrics.
 *
 * @module cli/commands/graph/update
 * @version 2.4.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { readFile, writeFile, access, rename } from 'node:fs/promises';
import { constants as fsConstants } from 'node:fs';
import path from 'node:path';

/**
 * Validation schema for graph metadata
 * @type {z.ZodSchema<{baseIri?: string, updatedAt?: string, [key: string]: unknown}>}
 */
const graphMetadataSchema = z.object({
  baseIri: z.string().optional(),
  updatedAt: z.string().optional(),
}).strict().passthrough();

/**
 * Validation schema for update command arguments
 */
const updateArgsSchema = z.object({
  name: z.string().optional().default(''),
  'base-iri': z.string().optional(),
});

/**
 * Update graph command
 */
export const updateCommand = defineCommand({
  meta: {
    name: 'update',
    description: 'Update graph properties',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Name of the graph to update',
      required: true,
    },
    'base-iri': {
      type: 'string',
      description: 'New base IRI for the graph',
      alias: 'b',
    },
  },
  async run(ctx) {
    try {
      // Validate arguments
      const args = updateArgsSchema.parse(ctx.args);

      const graphsDir = path.resolve(process.cwd(), 'graph');
      const metaPath = path.join(graphsDir, `${args.name}.meta.json`);

      // Ensure graph exists
      try {
        await access(metaPath, fsConstants.F_OK);
      } catch {
        throw new Error(`Graph not found: ${args.name}`);
      }

      const rawMetadata = JSON.parse(await readFile(metaPath, 'utf8'));
      const current = graphMetadataSchema.parse(rawMetadata);

      const updated = graphMetadataSchema.parse({
        ...current,
        baseIri: args['base-iri'] ?? current.baseIri,
        updatedAt: new Date().toISOString(),
      });

      const tmpPath = metaPath + '.tmp';
      await writeFile(tmpPath, JSON.stringify(updated, null, 2), 'utf8');
      await rename(tmpPath, metaPath);

      console.log(`✅ Graph updated: ${args.name}`);

      if (args['base-iri']) {
        console.log(`   Base IRI: ${args['base-iri']}`);
      }
    } catch (error) {
      console.error('❌ Update failed:', error.message);
      throw error;
    }
  },
});
