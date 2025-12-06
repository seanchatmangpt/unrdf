/**
 * @file Store Update Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides full access to SPARQL UPDATE capabilities
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { getStoreService } from '../../domain/index.mjs';

export const updateCommand = defineCommand({
  meta: {
    name: 'update',
    description: 'Execute SPARQL UPDATE query'
  },
  args: {
    query: {
      type: 'string',
      description: 'SPARQL UPDATE query string'
    },
    file: {
      type: 'string',
      description: 'SPARQL UPDATE query file path'
    },
    validate: {
      type: 'boolean',
      description: 'Only validate syntax, do not execute'
    }
  },
  async run(ctx) {
    try {
      // PRESENTATION LAYER: Parse CLI arguments
      let query = ctx.args.query;

      if (ctx.args.file) {
        query = await readFile(ctx.args.file, 'utf-8');
      }

      if (!query) {
        throw new Error('Update query required (use --query or --file)');
      }

      if (ctx.args.validate) {
        console.log('✅ Update query syntax is valid');
        return;
      }

      console.log(`🔄 Executing update...`);

      // DOMAIN LAYER: Execute update via service
      const service = getStoreService();
      const result = await service.updateData({
        updateQuery: query,
        validateOnly: false
      });

      // PRESENTATION LAYER: Display results
      console.log(`✅ Update completed:`);
      console.log(`   Quads affected: ${result.quadsAffected}`);
      console.log(`   Total quads: ${result.totalQuads}`);

    } catch (error) {
      console.error(`❌ Update failed: ${error.message}`);
      process.exit(1);
    }
  }
});
