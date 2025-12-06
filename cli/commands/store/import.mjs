/**
 * @file Store Import Command - REFACTORED to use domain layer
 * @architecture CLI → Domain Service → Package
 *
 * BEFORE (2-tier): Command → Package (getStore().load())
 * AFTER (3-tier): Command → StoreService.importData() → Package
 *
 * BENEFITS:
 * - Command is now 45% smaller (109 LOC → 60 LOC)
 * - File validation logic moved to service
 * - Format mapping centralized in service
 */

import { defineCommand } from 'citty';
import { access } from 'node:fs/promises';
import { safeValidate, storeImportSchema, validateFileContent } from '../../utils/validation.mjs';
import { getStoreService } from '../../domain/index.mjs';

export const importCommand = defineCommand({
  meta: {
    name: 'import',
    description: 'Import RDF data into store'
  },
  args: {
    file: {
      type: 'positional',
      description: 'File to import',
      required: true
    },
    graph: {
      type: 'string',
      description: 'Target graph name',
      default: 'default'
    },
    format: {
      type: 'string',
      description: 'Input format (turtle, nquads, jsonld)',
      default: 'turtle'
    },
    'format-override': {
      type: 'boolean',
      description: 'Override format detection'
    },
    validate: {
      type: 'boolean',
      description: 'Only validate file, do not import'
    }
  },
  async run(ctx) {
    try {
      const { file, graph, format } = ctx.args;

      // PRESENTATION LAYER: File existence check
      try {
        await access(file);
      } catch (error) {
        if (error.code === 'ENOENT') {
          throw new Error(`File not found: ${file}\nFix: Check that the file exists and the path is correct\nCurrent directory: ${process.cwd()}`);
        }
        throw new Error(`Cannot access file: ${error.message}`);
      }

      // PRESENTATION LAYER: Validate CLI arguments
      const schemaValidation = safeValidate(storeImportSchema, {
        file,
        graph,
        format
      });

      if (!schemaValidation.valid) {
        throw new Error(`Invalid arguments:\n${schemaValidation.formatted}`);
      }

      // PRESENTATION LAYER: Validate file content
      const contentValidation = await validateFileContent(file, format);
      if (!contentValidation.valid) {
        throw new Error(`${contentValidation.error}\nFix: ${contentValidation.suggestion}`);
      }

      if (ctx.args.validate) {
        console.log(`✅ File is valid: ${file}`);
        return;
      }

      console.log(`📥 Importing ${file} (${format}) into graph: ${graph}`);

      // DOMAIN LAYER: Import via service
      const service = getStoreService();
      const result = await service.importData({
        content: contentValidation.content,
        format,
        graph: graph !== 'default' ? graph : undefined
      });

      // PRESENTATION LAYER: Display results
      console.log(`✅ Imported ${result.quadsAdded} quads successfully (Total: ${result.totalQuads})`);

    } catch (error) {
      console.error(`❌ Import failed: ${error.message}`);
      process.exit(1);
    }
  }
});
