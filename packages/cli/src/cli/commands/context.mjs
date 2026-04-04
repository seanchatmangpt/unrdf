/**
 * Context Command - RDF Context Management
 *
 * Manage JSON-LD contexts and namespace prefixes
 *
 * @module cli/commands/context
 */

import { defineCommand } from 'citty';
import { readFileSync, writeFileSync, existsSync, unlinkSync } from 'node:fs';
import { table } from 'table';

/**
 * Simple file lock using a lock file pattern for concurrent access protection
 */
async function withFileLock(filePath, callback) {
  const lockPath = `${filePath}.lock`;
  const maxRetries = 50;
  let retries = 0;

  while (existsSync(lockPath) && retries < maxRetries) {
    await new Promise(resolve => setTimeout(resolve, 10));
    retries++;
  }

  try {
    // Create lock file
    writeFileSync(lockPath, '');
    // Execute callback
    return await callback();
  } finally {
    // Remove lock file
    try {
      unlinkSync(lockPath);
    } catch {
      // Lock file already removed
    }
  }
}

/**
 * Create a new context
 */
const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new JSON-LD context',
  },
  args: {
    name: {
      type: 'string',
      description: 'Context name',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output file path',
      required: false,
    },
  },
  async run({ args }) {
    const contextName = args.name;
    const outputFile = args.output || `${contextName}-context.jsonld`;

    try {
      const context = {
        '@context': {
          '@vocab': `http://example.org/${contextName}#`,
          rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
          rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
          xsd: 'http://www.w3.org/2001/XMLSchema#',
        },
      };

      writeFileSync(outputFile, JSON.stringify(context, null, 2), 'utf-8');

      console.log(`✅ Created context: ${contextName}`);
      console.log(`📁 File: ${outputFile}`);
    } catch (error) {
      console.error(`❌ Error creating context: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Add prefix to context
 */
const addCommand = defineCommand({
  meta: {
    name: 'add',
    description: 'Add prefix to context',
  },
  args: {
    file: {
      type: 'string',
      description: 'Context file',
      required: true,
    },
    prefix: {
      type: 'string',
      description: 'Prefix name',
      required: true,
    },
    namespace: {
      type: 'string',
      description: 'Namespace IRI',
      required: true,
    },
  },
  async run({ args }) {
    const { file, prefix, namespace } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      await withFileLock(file, async () => {
        const content = readFileSync(file, 'utf-8');
        const context = JSON.parse(content);

        if (!context['@context']) {
          context['@context'] = {};
        }

        context['@context'][prefix] = namespace;

        writeFileSync(file, JSON.stringify(context, null, 2), 'utf-8');
      });

      console.log(`✅ Added prefix: ${prefix} -> ${namespace}`);
      console.log(`📁 Updated: ${file}`);
    } catch (error) {
      console.error(`❌ Error adding prefix: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * List context prefixes
 */
const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List context prefixes',
  },
  args: {
    file: {
      type: 'string',
      description: 'Context file',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (table, json)',
      default: 'table',
    },
  },
  async run({ args }) {
    const { file, format } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      let prefixes;
      await withFileLock(file, async () => {
        const content = readFileSync(file, 'utf-8');
        const context = JSON.parse(content);

        if (!context['@context']) {
          prefixes = null;
        } else {
          prefixes = context['@context'];
        }
      });

      if (!prefixes) {
        console.log('No prefixes found.');
        return;
      }

      if (format === 'json') {
        console.log(JSON.stringify(prefixes, null, 2));
      } else {
        // Table format
        const data = Object.entries(prefixes).map(([prefix, namespace]) => [prefix, namespace]);

        const tableData = [['Prefix', 'Namespace'], ...data];

        console.log(
          table(tableData, {
            header: {
              alignment: 'center',
              content: `Context Prefixes (${data.length})`,
            },
          })
        );
      }
    } catch (error) {
      console.error(`❌ Error listing prefixes: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Remove prefix from context
 */
const removeCommand = defineCommand({
  meta: {
    name: 'remove',
    description: 'Remove prefix from context',
  },
  args: {
    file: {
      type: 'string',
      description: 'Context file',
      required: true,
    },
    prefix: {
      type: 'string',
      description: 'Prefix name to remove',
      required: true,
    },
  },
  async run({ args }) {
    const { file, prefix } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      await withFileLock(file, async () => {
        const content = readFileSync(file, 'utf-8');
        const context = JSON.parse(content);

        if (!context['@context'] || !context['@context'][prefix]) {
          console.error(`❌ Prefix not found: ${prefix}`);
          process.exit(1);
        }

        delete context['@context'][prefix];

        writeFileSync(file, JSON.stringify(context, null, 2), 'utf-8');
      });

      console.log(`✅ Removed prefix: ${prefix}`);
      console.log(`📁 Updated: ${file}`);
    } catch (error) {
      console.error(`❌ Error removing prefix: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Main context command
 */
export const contextCommand = defineCommand({
  meta: {
    name: 'context',
    description: 'Manage JSON-LD contexts',
  },
  subCommands: {
    create: createCommand,
    add: addCommand,
    list: listCommand,
    remove: removeCommand,
  },
});
