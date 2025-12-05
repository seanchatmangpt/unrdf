/**
 * @file Hook Create Command - with type validation poka-yoke guard
 * @module cli-v2/commands/hook/create
 */

import { defineCommand } from 'citty';
import { writeFile, access } from 'node:fs/promises';
import { validate, hookCreateSchema } from '../../utils/validation.mjs';

// Valid hook types (poka-yoke guard: FM-CLI-003)
const VALID_HOOK_TYPES = ['sparql-ask', 'sparql-select', 'shacl', 'custom'];

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
      description: `Hook type (${VALID_HOOK_TYPES.join(', ')})`,
      required: true
    },
    file: {
      type: 'string',
      description: 'SPARQL/SHACL file path'
    },
    output: {
      type: 'string',
      description: 'Output file for hook definition'
    },
    description: {
      type: 'string',
      description: 'Hook description'
    }
  },
  async run(ctx) {
    try {
      const { name, type, file, output, description } = ctx.args;

      // FM-CLI-003: Validate hook type with enum (poka-yoke guard)
      const validated = validate(hookCreateSchema, {
        name,
        type,
        file,
        description
      }, 'Hook validation');

      // Validate file if provided
      if (file) {
        try {
          await access(file);
        } catch (error) {
          if (error.code === 'ENOENT') {
            throw new Error(`Hook file not found: ${file}\nFix: Create the file or check the path`);
          }
          throw new Error(`Cannot access hook file: ${error.message}`);
        }
      }

      const hookDef = {
        meta: {
          name,
          version: '1.0.0',
          description
        },
        when: {
          kind: validated.type,
          ref: file ? {
            uri: `file://${file}`,
            mediaType: validated.type.startsWith('sparql')
              ? 'application/sparql-query'
              : 'text/turtle'
          } : {}
        },
        run: async (event) => {
          console.log(`Hook ${name} executed`);
          return { success: true };
        }
      };

      if (output) {
        await writeFile(output, JSON.stringify(hookDef, null, 2));
        console.log(`✅ Hook created: ${name} (${validated.type})`);
        console.log(`📄 Definition written to: ${output}`);
      } else {
        console.log(`✅ Hook created: ${name} (${validated.type})`);
        console.log(JSON.stringify(hookDef, null, 2));
      }
    } catch (error) {
      console.error(`❌ Hook creation failed: ${error.message}`);
      process.exit(1);
    }
  }
});
