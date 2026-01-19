/**
 * @file Sync Orchestrator
 * @module cli/commands/sync/orchestrator
 * @description Main orchestrator for code generation sync process
 */
import { mkdir, writeFile, access } from 'fs/promises';
import { dirname, resolve, relative } from 'path';
import { constants } from 'fs';
import { parseConfig } from './config-parser.mjs';
import { loadOntology } from './ontology-loader.mjs';
import { executeSparqlQuery } from './sparql-executor.mjs';
import { renderTemplate } from './template-renderer.mjs';
import { SyncArgsSchema } from './schemas.mjs';

const c = { reset: '\x1b[0m', bold: '\x1b[1m', dim: '\x1b[2m', green: '\x1b[32m', yellow: '\x1b[33m', blue: '\x1b[34m', cyan: '\x1b[36m', red: '\x1b[31m' };
const noColor = !process.stdout.isTTY || process.env.NO_COLOR;
if (noColor) Object.keys(c).forEach(k => c[k] = '');

function formatDuration(ms) {
  if (ms < 1) return (ms * 1000).toFixed(0) + 'us';
  if (ms < 1000) return ms.toFixed(1) + 'ms';
  if (ms < 60000) return (ms / 1000).toFixed(2) + 's';
  return (ms / 60000).toFixed(2) + 'm';
}

export async function runSync(options) {
  const startTime = performance.now();
  const args = SyncArgsSchema.parse(options);
  const { config: configPath, dryRun, verbose, force, rule: ruleFilter, output } = args;
  
  const results = [];
  const metrics = { rulesProcessed: 0, filesGenerated: 0, filesSkipped: 0, errors: 0 };
  
  try {
    console.log(c.bold + c.blue + 'UNRDF Sync' + c.reset + '\n');
    console.log(c.cyan + 'Phase 1:' + c.reset + ' Loading configuration...');
    
    const config = await parseConfig(configPath);
    const baseDir = dirname(resolve(configPath));
    
    if (verbose) {
      console.log('   Config: ' + configPath);
      console.log('   Project: ' + (config.project?.name || 'unnamed'));
    }
    
    console.log('\n' + c.cyan + 'Phase 2:' + c.reset + ' Loading ontology...');
    
    const { store, tripleCount, prefixes } = await loadOntology(config.ontology, baseDir);
    metrics.totalTriples = tripleCount;
    console.log('   Loaded: ' + c.green + tripleCount + c.reset + ' triples');
    
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
      const ruleStart = performance.now();
      metrics.rulesProcessed++;

      if (verbose) console.log('\n   ' + c.dim + 'Rule: ' + rule.name + c.reset);

      try {
        // Validate template file exists before processing
        if (!rule.template) {
          throw new Error('Rule configuration missing required "template" field');
        }

        const templatePath = resolve(baseDir, rule.template);
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

        // Execute SPARQL query
        let sparqlResults;
        try {
          sparqlResults = await executeSparqlQuery(store, rule.query, prefixes);
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
        let content, outputPath;
        try {
          const result = await renderTemplate(templatePath, sparqlResults, {
            project: config.project,
            prefixes,
            output_dir: outputDir,
          });
          content = result.content;
          outputPath = result.outputPath;
        } catch (renderErr) {
          throw new Error(
            `Template rendering failed for rule "${rule.name}"\n` +
            `  Template: ${templatePath}\n` +
            `  Error: ${renderErr.message}\n` +
            `  Fix: Check template syntax and ensure all variables are defined`
          );
        }

        const finalPath = resolve(outputDir, outputPath || rule.output_file);
        const bytes = Buffer.byteLength(content, 'utf-8');
        totalBytes += bytes;
        const duration = performance.now() - ruleStart;

        if (dryRun) {
          console.log('   ' + c.yellow + '[DRY RUN]' + c.reset + ' Would write: ' + relative(process.cwd(), finalPath));
          results.push({ rule: rule.name, path: finalPath, status: 'dry-run', duration, bytes });
          metrics.filesSkipped++;
        } else {
          await mkdir(dirname(finalPath), { recursive: true });
          await writeFile(finalPath, content, 'utf-8');
          console.log('   ' + c.green + 'OK' + c.reset + ' ' + relative(process.cwd(), finalPath) + ' (' + bytes + ' bytes)');
          results.push({ rule: rule.name, path: finalPath, status: 'success', duration, bytes });
          metrics.filesGenerated++;
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
          template: rule.template
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
    console.error('\n' + c.red + 'Sync failed:' + c.reset + ' ' + err.message);
    if (verbose && err.stack) console.error(err.stack);
    return { success: false, results, totalDuration: performance.now() - startTime, metrics, error: err.message };
  }
}

export default { runSync };
