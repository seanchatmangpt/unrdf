/**
 * @file Sync Command - RDF Ontology to Code Generation
 * @module cli/commands/sync
 * @description CLI command for synchronized code generation from RDF ontology
 */
import { defineCommand } from 'citty';
import { existsSync, watch as fsWatch } from 'fs';
import { resolve } from 'path';

const DEBOUNCE_MS = 300;

/**
 * Creates a debounced version of a function
 * @param {Function} fn - Function to debounce
 * @param {number} delay - Debounce delay in ms
 * @returns {Function} Debounced function
 */
function debounce(fn, delay) {
  let timeoutId = null;
  return (...args) => {
    if (timeoutId) clearTimeout(timeoutId);
    timeoutId = setTimeout(() => {
      timeoutId = null;
      fn(...args);
    }, delay);
  };
}

/**
 * Extracts watch paths from config
 * @param {string} configPath - Path to config file
 * @returns {Promise<string[]>} Array of absolute paths to watch
 */
async function getWatchPaths(configPath) {
  const { parseConfig } = await import('./sync/config-parser.mjs');
  const config = await parseConfig(configPath);
  const paths = [resolve(configPath)];

  if (config.ontology?.source) {
    paths.push(config.ontology.source);
  }

  if (config.generation?.rules) {
    for (const rule of config.generation.rules) {
      if (rule.template && existsSync(rule.template)) {
        paths.push(rule.template);
      }
    }
  }

  return [...new Set(paths)];
}

/**
 * Starts file watcher for sync command
 * @param {Object} args - Command arguments
 * @param {Function} runSyncFn - The runSync function to call
 */
async function startWatchMode(args, runSyncFn) {
  const c = {
    reset: '\x1b[0m',
    bold: '\x1b[1m',
    dim: '\x1b[2m',
    cyan: '\x1b[36m',
    yellow: '\x1b[33m',
    green: '\x1b[32m',
  };
  const noColor = !process.stdout.isTTY || process.env.NO_COLOR;
  if (noColor) Object.keys(c).forEach(k => c[k] = '');

  let watchPaths = [];
  const watchers = [];
  let isRunning = false;

  const runOnce = async () => {
    if (isRunning) return;
    isRunning = true;

    console.log('\n' + c.dim + '─'.repeat(50) + c.reset + '\n');

    try {
      await runSyncFn({
        config: args.config,
        dryRun: args['dry-run'],
        verbose: args.verbose,
        force: args.force,
        rule: args.rule,
        output: args.output,
      });
    } catch (err) {
      console.error(`Error: ${err.message}`);
      if (args.verbose && err.stack) {
        console.error(err.stack);
      }
    }

    isRunning = false;
    console.log('\n' + c.cyan + 'Watching for changes...' + c.reset + ' (Ctrl+C to stop)');
  };

  const debouncedRun = debounce(runOnce, DEBOUNCE_MS);

  const setupWatchers = async () => {
    // Close existing watchers
    for (const w of watchers) {
      try { w.close(); } catch (e) { /* ignore */ }
    }
    watchers.length = 0;

    try {
      watchPaths = await getWatchPaths(args.config);
    } catch (err) {
      console.error(`${c.yellow}Warning:${c.reset} Could not parse config for watch paths: ${err.message}`);
      watchPaths = [resolve(args.config)];
    }

    if (args.verbose) {
      console.log(c.dim + 'Watching:' + c.reset);
      for (const p of watchPaths) {
        console.log('  ' + p);
      }
    }

    for (const filePath of watchPaths) {
      if (!existsSync(filePath)) continue;

      try {
        const watcher = fsWatch(filePath, { persistent: true }, (eventType, filename) => {
          if (args.verbose) {
            console.log(c.dim + `[${eventType}] ${filename || filePath}` + c.reset);
          }
          debouncedRun();
        });

        watcher.on('error', (err) => {
          if (args.verbose) {
            console.error(`${c.yellow}Watch error:${c.reset} ${err.message}`);
          }
        });

        watchers.push(watcher);
      } catch (err) {
        if (args.verbose) {
          console.error(`${c.yellow}Could not watch:${c.reset} ${filePath}`);
        }
      }
    }
  };

  // Initial run
  await runOnce();

  // Setup watchers
  await setupWatchers();

  // Handle graceful shutdown
  process.on('SIGINT', () => {
    console.log('\n' + c.dim + 'Stopping watch mode...' + c.reset);
    for (const w of watchers) {
      try { w.close(); } catch (e) { /* ignore */ }
    }
    process.exit(0);
  });
}

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
    watch: {
      type: 'boolean',
      alias: 'w',
      description: 'Watch ontology and template files for changes',
      default: false,
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

      if (args.watch) {
        // Watch mode - continuous execution
        await startWatchMode(args, runSync);
      } else {
        // Single run mode
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
