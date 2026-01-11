/**
 * @file Sync Command - RDF Ontology to Code Generation
 * @module cli/commands/sync
 * @description CLI command for synchronized code generation from RDF ontology
 */
import { defineCommand } from 'citty';
import { existsSync } from 'fs';

/**
 * Sync command for generating code from RDF ontology
 */
export const syncCommand = defineCommand({
  meta: {
    name: 'sync',
    description: 'Generate synchronized code artifacts from RDF ontology',
  },
  args: {
    config: {
      type: 'string',
      description: 'Path to ggen.toml configuration file',
      default: 'ggen.toml',
    },
    'dry-run': {
      type: 'boolean',
      description: 'Preview changes without writing files',
      default: false,
    },
    verbose: {
      type: 'boolean',
      alias: 'v',
      description: 'Enable verbose output',
      default: false,
    },
    force: {
      type: 'boolean',
      alias: 'f',
      description: 'Overwrite existing files without prompting',
      default: false,
    },
    rule: {
      type: 'string',
      description: 'Run only the specified rule by name',
    },
    output: {
      type: 'string',
      description: 'Output format: text or json',
      default: 'text',
    },
  },
  async run({ args }) {
    // Check config file exists
    if (!existsSync(args.config)) {
      console.error(`Error: Configuration file not found: ${args.config}`);
      console.error('\nCreate a ggen.toml file with:');
      console.error(`
[project]
name = "my-project"
version = "1.0.0"

[ontology]
source = "ontology/schema.ttl"

[generation]
output_dir = "lib"
rules = []
`);
      process.exit(1);
    }
    
    try {
      // Dynamic import to avoid loading dependencies if not needed
      const { runSync } = await import('./sync/orchestrator.mjs');
      
      const result = await runSync({
        config: args.config,
        dryRun: args['dry-run'],
        verbose: args.verbose,
        force: args.force,
        rule: args.rule,
        output: args.output,
      });
      
      if (!result.success) {
        process.exit(1);
      }
    } catch (err) {
      console.error(`Error: ${err.message}`);
      if (args.verbose && err.stack) {
        console.error(err.stack);
      }
      process.exit(1);
    }
  },
});

export default syncCommand;
