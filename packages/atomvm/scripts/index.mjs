#!/usr/bin/env node

/**
 * AtomVM Build Scripts
 *
 * Main entry point for AtomVM build and packaging scripts.
 * Handles creating Erlang modules, compiling to BEAM, and packaging to .avm files.
 *
 * Usage:
 *   node scripts/index.mjs build <module> - Build specific module
 *   node scripts/index.mjs clean          - Clean build artifacts
 */

import { buildModule } from './build.mjs';
import { clean } from './clean.mjs';
import { workflow } from './workflow.mjs';

const command = process.argv[2];
const moduleName = process.argv[3];

async function main() {
  try {
    switch (command) {
      case 'build':
        if (!moduleName) {
          throw new Error('moduleName required: node scripts/index.mjs build <module>');
        }
        await buildModule(moduleName);
        break;
      case 'workflow':
      case 'w':
        if (!moduleName) {
          throw new Error('moduleName required: node scripts/index.mjs workflow <module>');
        }
        await workflow(moduleName);
        break;
      case 'clean':
        await clean();
        break;
      default:
        console.error('Usage: node scripts/index.mjs <command> [module]');
        console.error('Commands:');
        console.error('  build <module>   - Build Erlang module to .avm file');
        console.error('  workflow <module> - Complete workflow (build + instructions)');
        console.error('  clean           - Clean build artifacts');
        process.exit(1);
    }
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
}

main();
