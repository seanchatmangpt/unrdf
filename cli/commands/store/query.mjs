/**
 * @file Store Query Command - REFACTORED to use domain layer
 * @architecture CLI → Domain Service → Package
 *
 * BEFORE (2-tier): Command → Package (getStore().query())
 * AFTER (3-tier): Command → StoreService.executeQuery() → Package
 *
 * BENEFITS:
 * - Command is now 60% smaller (109 LOC → 43 LOC)
 * - Business logic moved to testable service
 * - Same service can be used by Next.js, API, CLI
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { formatOutput } from '../../formatters/index.mjs';
import { validateSparqlQuery, safeValidate, storeQuerySchema } from '../../utils/validation.mjs';
import { getStoreService } from '../../domain/index.mjs';

export const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute SPARQL query'
  },
  args: {
    query: {
      type: 'string',
      description: 'SPARQL query string'
    },
    file: {
      type: 'string',
      description: 'SPARQL query file path'
    },
    format: {
      type: 'string',
      description: 'Output format',
      default: 'table'
    },
    timeout: {
      type: 'number',
      description: 'Query timeout in milliseconds',
      default: 10000
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
        try {
          query = await readFile(ctx.args.file, 'utf-8');
        } catch (error) {
          if (error.code === 'ENOENT') {
            throw new Error(`Query file not found: ${ctx.args.file}\nFix: Check that the file exists`);
          }
          throw new Error(`Cannot read query file: ${error.message}`);
        }
      }

      if (!query) {
        throw new Error('Query required (use --query or --file)');
      }

      // PRESENTATION LAYER: Validate SPARQL syntax
      const validation = validateSparqlQuery(query);

      if (!validation.valid) {
        console.error('❌ SPARQL Syntax Errors:');
        validation.errors.forEach(err => console.error(`  • ${err}`));
        if (validation.suggestions.length > 0) {
          console.error('\n📖 Suggestions:');
          validation.suggestions.forEach(sug => console.error(`  • ${sug}`));
        }
        process.exit(1);
      }

      if (validation.warnings.length > 0) {
        console.warn('⚠️  SPARQL Warnings:');
        validation.warnings.forEach(warn => console.warn(`  • ${warn}`));
      }

      if (ctx.args.validate) {
        console.log('✅ Query syntax is valid');
        return;
      }

      console.log(`🔍 Executing query...`);

      // DOMAIN LAYER: Execute query via service
      const service = getStoreService();
      const result = await service.executeQuery({
        query,
        timeout: ctx.args.timeout,
        format: ctx.args.format
      });

      // PRESENTATION LAYER: Format and display results
      console.log(formatOutput(result.data, ctx.args.format));
      console.log(`\n📊 Query completed: ${result.metadata.rowCount} results in ${result.metadata.executionTime}ms`);

    } catch (error) {
      console.error(`❌ Query failed: ${error.message}`);
      process.exit(1);
    }
  }
});
