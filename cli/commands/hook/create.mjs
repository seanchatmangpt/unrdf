/**
 * @file Hook Create Command
 * @module cli-v2/commands/hook/create
 */

import { defineCommand } from 'citty';
import { writeFile } from 'node:fs/promises';

export const createCommand = defineCommand({
  meta: {
    name: 'create',
    description: 'Create a new knowledge hook'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Hook name',
      required: true
    },
    type: {
      type: 'string',
      description: 'Hook type (sparql-ask, sparql-select, shacl)',
      required: true
    },
    file: {
      type: 'string',
      description: 'SPARQL/SHACL file path'
    },
    output: {
      type: 'string',
      description: 'Output file for hook definition'
    }
  },
  async run(ctx) {
    const { name, type, file, output } = ctx.args;

    const hookDef = {
      meta: {
        name,
        version: '1.0.0'
      },
      when: {
        kind: type,
        ref: file ? {
          uri: `file://${file}`,
          mediaType: type.startsWith('sparql') ? 'application/sparql-query' : 'text/turtle'
        } : {}
      },
      run: async (event) => {
        console.log(`Hook ${name} executed`);
        return { success: true };
      }
    };

    if (output) {
      await writeFile(output, JSON.stringify(hookDef, null, 2));
      console.log(`✅ Hook definition written to: ${output}`);
    } else {
      console.log(JSON.stringify(hookDef, null, 2));
    }
  }
});
