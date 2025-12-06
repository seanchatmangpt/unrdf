/**
 * @file Store Query Command - with SPARQL validation poka-yoke guard
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { formatOutput } from '../../formatters/index.mjs';
import { validateSparqlQuery, safeValidate, storeQuerySchema } from '../../utils/validation.mjs';

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
      let query = ctx.args.query;

      // FM-CLI-002: Add file existence check
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

      // FM-CLI-001: Add SPARQL validation (poka-yoke guard)
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

      // Validate schema
      const schemaValidation = safeValidate(storeQuerySchema, {
        query,
        format: ctx.args.format,
        timeout: ctx.args.timeout
      });

      if (!schemaValidation.valid) {
        throw new Error(`Invalid query arguments:\n${schemaValidation.formatted}`);
      }

      if (ctx.args.validate) {
        console.log('✅ Query syntax is valid');
        return;
      }

      console.log(`🔍 Executing query...`);

      // Execute SPARQL query using local store
      const { getStore } = await import('../../utils/store-instance.mjs');
      const store = getStore();

      const results = store.query(query, {
        timeout: ctx.args.timeout
      });

      console.log(formatOutput(results, ctx.args.format));
    } catch (error) {
      console.error(`❌ Query failed: ${error.message}`);
      process.exit(1);
    }
  }
});
