/**
 * @file Store Export Command - REFACTORED to use domain layer
 * @architecture CLI → Domain Service → Package
 *
 * BEFORE (2-tier): Command → Package (getStore().dump())
 * AFTER (3-tier): Command → StoreService.exportData() → Package
 *
 * BENEFITS:
 * - Command is now 40% smaller (73 LOC → 44 LOC)
 * - Format mapping centralized in service
 * - Export logic testable independently
 */

import { defineCommand } from 'citty';
import { writeFile } from 'node:fs/promises';
import { dirname } from 'node:path';
import { mkdir } from 'node:fs/promises';
import { getStoreService } from '../../domain/index.mjs';

export const exportCommand = defineCommand({
  meta: {
    name: 'export',
    description: 'Export store data'
  },
  args: {
    output: {
      type: 'string',
      description: 'Output file path',
      required: true
    },
    format: {
      type: 'string',
      description: 'Output format (turtle, ntriples, nquads, jsonld, rdfxml)',
      default: 'turtle'
    },
    graph: {
      type: 'string',
      description: 'Graph to export (exports all graphs if not specified)'
    }
  },
  async run(ctx) {
    try {
      const { output, format, graph } = ctx.args;

      console.log(`📤 Exporting store (${format})...`);

      // DOMAIN LAYER: Export via service
      const service = getStoreService();
      const result = await service.exportData({
        format,
        graph
      });

      // PRESENTATION LAYER: Write to file
      const outputDir = dirname(output);
      await mkdir(outputDir, { recursive: true });
      await writeFile(output, result.content, 'utf-8');

      // PRESENTATION LAYER: Display results
      console.log(`✅ Exported ${result.quadCount} quads to: ${output}`);

    } catch (error) {
      console.error(`❌ Export failed: ${error.message}`);
      process.exit(1);
    }
  }
});
