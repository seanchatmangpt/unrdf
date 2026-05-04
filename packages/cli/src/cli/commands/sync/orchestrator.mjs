/**
 * @file Sync Orchestrator
 * @module cli/commands/sync/orchestrator
 * @description Main orchestrator for code generation sync process
 */
import { access, readFile } from 'fs/promises';
import { dirname, resolve, relative } from 'path';
import { constants, existsSync } from 'fs';
import { parseConfig } from './config-parser.mjs';
import { loadOntology } from './ontology-loader.mjs';
import { executeSparqlQuery } from './sparql-executor.mjs';
import { renderWithOptions } from './template-renderer.mjs';
import { SyncArgsSchema } from './schemas.mjs';

const c = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m',
  red: '\x1b[31m',
};
const noColor = !process.stdout.isTTY || process.env.NO_COLOR;
if (noColor) Object.keys(c).forEach(k => (c[k] = ''));

function formatDuration(ms) {
  if (ms < 1) return (ms * 1000).toFixed(0) + 'us';
  if (ms < 1000) return ms.toFixed(1) + 'ms';
  if (ms < 60000) return (ms / 1000).toFixed(2) + 's';
  return (ms / 60000).toFixed(2) + 'm';
}

/**
 *
 */
export async function runSync(options) {
  const startTime = performance.now();
  const args = SyncArgsSchema.parse(options);
  const { config: configPath, dryRun, verbose, force: _force, rule: ruleFilter, output, harden, breed: cliBreed } = args;

  const results = [];
  const metrics = { rulesProcessed: 0, filesGenerated: 0, filesSkipped: 0, errors: 0 };

  try {
    console.log(c.bold + c.blue + 'UNRDF Sync' + c.reset + '\n');
    console.log(c.cyan + 'Phase 1:' + c.reset + ' Loading configuration...');

    const config = await parseConfig(configPath);
    const baseDir = dirname(resolve(configPath));

    // Resolve breed: CLI > Config > Default
    const breed = cliBreed || config.project?.breed || 'dachshund';

    if (verbose) {
      console.log('   Config: ' + configPath);
      console.log('   Project: ' + (config.project?.name || 'unnamed'));
      console.log('   Breed: ' + breed);
    }

    console.log('\n' + c.cyan + 'Phase 2:' + c.reset + ' Loading ontology...');

    if (!config.ontology) {
      throw new Error(
        'Configuration missing required "ontology" section.\n' +
        'Add to unrdf.toml:\n[ontology]\nsource = "path/to/ontology.ttl"\n'
      );
    }

    const { store, tripleCount, prefixes } = await loadOntology(config.ontology, baseDir);
    metrics.totalTriples = tripleCount;
    console.log('   Loaded: ' + c.green + tripleCount + c.reset + ' triples');

    // Load extra sources if configured
    const extraSources = config.ontology?.extra_sources || [];
    if (extraSources.length > 0) {
      console.log('');
      console.log('   ' + c.dim + 'Merging extra sources:' + c.reset);

      for (const extraSource of extraSources) {
        const extraOntology = { ...config.ontology, source: extraSource };
        try {
          const { store: extraStore, tripleCount: _extraCount } = await loadOntology(extraOntology, baseDir);
          const previousCount = store.size || tripleCount;

          // Merge extra store into main store
          if (typeof extraStore.match === 'function') {
            const triples = extraStore.match(null, null, null);
            for (const triple of triples) {
              store.add(triple);
            }
          }

          const newCount = store.size || previousCount;
          const mergedCount = newCount - previousCount;

          console.log(
            '   ' +
              c.green +
              'Merged' +
              c.reset +
              ' ' +
              mergedCount +
              ' triples from ' +
              c.cyan +
              extraSource +
              c.reset
          );

          metrics.totalTriples = newCount;
        } catch (extraErr) {
          console.log(
            '   ' +
              c.yellow +
              'SKIP' +
              c.reset +
              ' ' +
              extraSource +
              ': ' +
              extraErr.message
          );
        }
      }

      console.log('   ' + c.dim + 'Total after merge: ' + metrics.totalTriples + ' triples' + c.reset);
    }

    console.log('\n' + c.cyan + 'Phase 3:' + c.reset + ' Processing rules...');

    const rules = config.generation?.rules || [];
    const enabledRules = ruleFilter
      ? rules.filter(r => r.name === ruleFilter)
      : rules.filter(r => r.enabled !== false);

    if (enabledRules.length === 0) {
      console.log('   ' + c.yellow + 'No rules to process' + c.reset);
    }

    let totalBytes = 0;

    for (const rule of enabledRules) {
      const _ruleStart = performance.now();
      metrics.rulesProcessed++;

      if (verbose) console.log('\n   ' + c.dim + 'Rule: ' + rule.name + c.reset);

      try {
        // Validate template file exists before processing
        if (!rule.template) {
          throw new Error('Rule configuration missing required "template" field');
        }

        const templatePath = resolve(baseDir, rule.template);
        if (!dryRun) {
          try {
            await access(templatePath, constants.R_OK);
          } catch (accessErr) {
            throw new Error(
              `Template file not found or not readable: ${templatePath}\n` +
                `  Rule: ${rule.name}\n` +
                `  Config: ${configPath}\n` +
                `  Fix: Check that the template path is correct and the file exists`
            );
          }
        }

        // Execute SPARQL query
        let sparqlResults;
        try {
          let queryString = rule.query;
          if (rule.query && (rule.query.endsWith(".rq") || rule.query.endsWith(".sparql"))) {
            const queryPath = resolve(baseDir, rule.query);
            if (existsSync(queryPath)) {
              const { readFile } = await import('fs/promises');
              queryString = await readFile(queryPath, 'utf8');
            }
          }
          sparqlResults = await executeSparqlQuery(store, queryString, prefixes);
          if (verbose) console.log('   Query returned ' + sparqlResults.length + ' results');
        } catch (queryErr) {
          throw new Error(
            `SPARQL query execution failed for rule "${rule.name}"\n` +
              `  Template: ${rule.template}\n` +
              `  Error: ${queryErr.message}\n` +
              `  Fix: Check query syntax and ensure ontology contains expected data`
          );
        }

        // Render template
        const outputDir = config.generation?.output_dir || resolve(baseDir, 'lib');
        
        // peek if template exists and check for inject: true to handle per-row injection automatically
        let hasInject = false;
        let hasLoop = false;
        if (existsSync(templatePath)) {
          const content = await readFile(templatePath, 'utf8');
          hasInject = /^\s*inject:\s*true/m.test(content);
          hasLoop = /\{%\s*for\b/.test(content);
        }

        // We only iterate (perRow) if:
        // 1. The output filename is dynamic (contains {{)
        // 2. The template is an injection template (has inject: true) AND does not have its own loop
        const perRow = (rule.output_file || '').includes('{{') || (hasInject && !hasLoop);
        
        // If not perRow, we render exactly once with full context.
        // If perRow, we render once per result row.
        // Special case: if perRow is true but we have 0 results, we render 0 times (skip).
        const rowsToRender = perRow ? sparqlResults : [null];

        for (const row of rowsToRender) {
          try {
            const result = await renderWithOptions(templatePath, sparqlResults, {
              project: config.project,
              breed,
              prefixes,
              outputDir,
              baseDir,
              templates_dir: config.generation?.templates_dir,
              outputPath: rule.output_file,
              dryRun,
              force: _force, // Use CLI force flag
              mode: rule.mode, // Pass rule mode (overwrite, skip_existing, etc.)
              backup_before_overwrite: config.generation?.backup_before_overwrite,
              backup_suffix: config.generation?.backup_suffix,
              harden,
              ...(row || {}),
            });

            if (result.skipped) {
              const displayPath = result.finalPath ? relative(process.cwd(), result.finalPath) : rule.template;
              if (verbose || result.status !== 'skipped') {
                console.log("   " + c.yellow + "SKIP" + c.reset + " " + displayPath + (result.reason ? ` (${result.reason})` : ''));
              }
              results.push({ rule: rule.name, path: result.finalPath, status: "skipped" });
              metrics.filesSkipped++;
              continue;
            }

            const finalPath = result.finalPath;
            const bytes = result.bytes || 0;
            totalBytes += bytes;

            if (result.status === "dry-run") {
              console.log("   " + c.yellow + "[DRY RUN]" + c.reset + " Would write: " + relative(process.cwd(), finalPath));
              results.push({ rule: rule.name, path: finalPath, status: "dry-run", bytes });
              metrics.filesSkipped++;
            } else {
              console.log("   " + c.green + "OK" + c.reset + " " + relative(process.cwd(), finalPath) + " (" + bytes + " bytes)");
              results.push({ rule: rule.name, path: finalPath, status: "success", bytes });
              metrics.filesGenerated++;
            }
          } catch (renderErr) {
            throw new Error(
              `Template rendering failed for rule "${rule.name}"
` +
                `  Template: ${templatePath}
` +
                `  Error: ${renderErr.message}
` +
                `  Fix: Check template syntax and ensure all variables are defined`
            );
          }
        }
      } catch (err) {
        const errorMsg = err.message.includes('\n') ? '\n' + err.message : err.message;
        console.log('   ' + c.red + 'ERR' + c.reset + ' ' + rule.name + ': ' + errorMsg);
        if (verbose && err.stack) {
          console.log('   ' + c.dim + err.stack + c.reset);
        }
        results.push({
          rule: rule.name,
          status: 'error',
          error: err.message,
          template: rule.template,
        });
        metrics.errors++;
        // Continue processing other rules
      }
    }

    metrics.totalBytes = totalBytes;
    const totalDuration = performance.now() - startTime;

    console.log('\n' + c.green + 'Sync complete!' + c.reset);
    console.log('   Rules processed: ' + metrics.rulesProcessed);
    console.log('   Files generated: ' + metrics.filesGenerated);
    if (metrics.errors > 0) console.log('   ' + c.red + 'Errors: ' + metrics.errors + c.reset);
    console.log('   Duration: ' + formatDuration(totalDuration));

    if (output === 'json') {
      console.log(JSON.stringify({ success: true, results, totalDuration, metrics }, null, 2));
    }

    return { success: metrics.errors === 0, results, totalDuration, metrics };
  } catch (err) {
    console.error('\n' + c.red + 'Sync failed:' + c.reset);
    // Format detailed validation errors if ConfigValidationError
    if (err.name === 'ConfigValidationError' && err.format) {
      console.error(err.format());
    } else {
      console.error(' ' + err.message);
    }
    if (verbose && err.stack) console.error(err.stack);
    return {
      success: false,
      results,
      totalDuration: performance.now() - startTime,
      metrics,
      error: err.message,
    };
  }
}

export default { runSync };
