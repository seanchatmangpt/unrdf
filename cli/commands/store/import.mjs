/**
 * @file Store Import Command - with file validation poka-yoke guard
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { access } from 'node:fs/promises';
import { safeValidate, storeImportSchema, validateFileContent } from '../../utils/validation.mjs';

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

      // FM-CLI-002: Pre-flight validation BEFORE printing success message
      // Check file exists
      try {
        await access(file);
      } catch (error) {
        if (error.code === 'ENOENT') {
          throw new Error(`File not found: ${file}\nFix: Check that the file exists and the path is correct\nCurrent directory: ${process.cwd()}`);
        }
        throw new Error(`Cannot access file: ${error.message}`);
      }

      // Validate schema
      const schemaValidation = safeValidate(storeImportSchema, {
        file,
        graph,
        format
      });

      if (!schemaValidation.valid) {
        throw new Error(`Invalid arguments:\n${schemaValidation.formatted}`);
      }

      // Validate file content
      const contentValidation = await validateFileContent(file, format);
      if (!contentValidation.valid) {
        throw new Error(`${contentValidation.error}\nFix: ${contentValidation.suggestion}`);
      }

      if (ctx.args.validate) {
        console.log(`✅ File is valid: ${file}`);
        return;
      }

      // NOW we can print the import message - all validation passed
      console.log(`📥 Importing ${file} (${format}) into graph: ${graph}`);

      const content = contentValidation.content;

      // Parse and import using local store
      const { getStore } = await import('../../utils/store-instance.mjs');
      const store = getStore();

      // Map format names to Oxigraph format strings
      const formatMap = {
        'turtle': 'text/turtle',
        'ntriples': 'application/n-triples',
        'nquads': 'application/n-quads',
        'jsonld': 'application/ld+json',
        'rdfxml': 'application/rdf+xml'
      };

      const oxigraphFormat = formatMap[format] || format;

      store.load(content, {
        format: oxigraphFormat,
        to_graph: graph !== 'default' ? { value: graph, termType: 'NamedNode' } : undefined
      });

      console.log(`✅ Imported ${store.size()} quads successfully`);
    } catch (error) {
      console.error(`❌ Import failed: ${error.message}`);
      process.exit(1);
    }
  }
});
