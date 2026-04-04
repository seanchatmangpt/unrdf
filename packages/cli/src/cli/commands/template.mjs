/**
 * Template Command - Ontology-driven template generation
 *
 * RDF + SPARQL layer uses {@link RdfTemplateLoader}; render/write uses
 * sync {@link renderWithOptions} / {@link discoverTemplates} only.
 *
 * @module cli/commands/template
 */

import { defineCommand } from 'citty';
import { existsSync } from 'node:fs';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, resolve, isAbsolute } from 'node:path';
import matter from 'gray-matter';
import { table } from 'table';
import { COMMON_PREFIXES } from '@unrdf/core';

import { RdfTemplateLoader, extractPrefixesFromTurtle } from '../../lib/rdf-template-loader.mjs';
import { executeSparqlQuery } from './sync/sparql-executor.mjs';
import { renderWithOptions, discoverTemplates } from './sync/template-renderer.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const DEFAULT_TEMPLATES_DIR = resolve(__dirname, '../../../templates/sync');

/**
 * Flatten first SPARQL row for top-level Nunjucks vars (name, title, …).
 * @param {Array<Record<string, unknown>>} rows
 * @returns {Record<string, unknown>}
 */
function nunjucksFlatFromFirstRow(rows) {
  if (!rows?.length) return {};
  const row = rows[0];
  const out = {};
  for (const [k, v] of Object.entries(row)) {
    if (k.startsWith('_')) continue;
    if (k.startsWith('?')) continue;
    out[k] = v;
  }
  return out;
}

/**
 * Resolve SPARQL string from CLI vs frontmatter (CLI wins when provided).
 * @param {string|undefined} cliSparql
 * @param {unknown} fmSparql
 * @returns {string|undefined}
 */
function resolveSparqlQuery(cliSparql, fmSparql) {
  if (cliSparql !== undefined && String(cliSparql).trim() !== '') {
    return String(cliSparql).trim();
  }
  if (typeof fmSparql === 'string' && fmSparql.trim() !== '') {
    return fmSparql.trim();
  }
  return undefined;
}

/**
 * Generate: render templates with RDF context (sync renderer + SPARQL executor)
 */
const generateCommand = defineCommand({
  meta: {
    name: 'generate',
    description:
      'Generate files from RDF + Nunjucks template (`--template`). ' +
      'RDF path can be positional or `rdf:` in template frontmatter.',
  },
  args: {
    file: {
      type: 'string',
      description:
        'RDF file (Turtle, N-Triples, N-Quads, …). Optional if template sets `rdf:` in frontmatter.',
      required: false,
    },
  },
  options: {
    template: {
      type: 'string',
      description: 'Template .njk path',
      alias: 't',
    },
    outputDir: {
      type: 'string',
      description: 'Output directory for generated files',
      alias: 'o',
      default: './generated',
    },
    subject: {
      type: 'string',
      description: 'Focus subject URI; replaces ?subject in SPARQL when present',
      alias: 's',
    },
    batch: {
      type: 'boolean',
      description: 'One output per instance of `--class-uri`; SPARQL must use ?subject',
      default: false,
    },
    classUri: {
      type: 'string',
      description: 'RDFS/OWL class IRI for batch mode (with `--batch`)',
    },
    sparql: {
      type: 'string',
      description: 'SPARQL SELECT (overrides frontmatter `sparql:`)',
      alias: 'q',
    },
    dryRun: {
      type: 'boolean',
      description: 'Print paths without writing',
      default: false,
    },
    force: {
      type: 'boolean',
      description: 'Overwrite existing files',
      default: false,
    },
  },
  async run(ctx) {
    const templatePath = ctx.args.template;
    if (!templatePath) {
      console.error('Error: --template / -t is required');
      process.exit(1);
    }

    const absTemplate = resolve(templatePath);
    if (!existsSync(absTemplate)) {
      console.error(`Error: Template not found: ${absTemplate}`);
      process.exit(1);
    }

    const templateDir = dirname(absTemplate);
    const rawTemplate = await readFile(absTemplate, 'utf-8');
    const { data: fm } = matter(rawTemplate);

    let rdfPath = ctx.args.file ? resolve(ctx.args.file) : undefined;
    if (!rdfPath && fm.rdf) {
      const r = fm.rdf;
      rdfPath = typeof r === 'string' && isAbsolute(r) ? r : resolve(templateDir, String(r));
    }
    if (!rdfPath) {
      console.error('Error: Provide an RDF file path, or set `rdf:` in the template frontmatter.');
      process.exit(1);
    }

    if (!existsSync(rdfPath)) {
      console.error(`Error: RDF file not found: ${rdfPath}`);
      process.exit(1);
    }

    const sparqlQuery = resolveSparqlQuery(ctx.args.sparql, fm.sparql);
    if (!sparqlQuery) {
      console.error('Error: Provide --sparql / -q or `sparql:` in template frontmatter.');
      process.exit(1);
    }

    const loader = new RdfTemplateLoader();
    const rdfText = await readFile(rdfPath, 'utf8');
    const mergedPrefixes = {
      ...COMMON_PREFIXES,
      ...extractPrefixesFromTurtle(rdfText),
    };

    try {
      const store = await loader.loadFromFile(rdfPath);

      let queryText = sparqlQuery;
      if (ctx.args.subject && queryText.includes('?subject')) {
        queryText = queryText.replace(/\?subject/g, `<${ctx.args.subject}>`);
      }

      const { batch, classUri, outputDir, dryRun, force } = ctx.args;

      if (batch) {
        if (!classUri) {
          console.error('Error: --batch requires --class-uri');
          process.exit(1);
        }
        if (!queryText.includes('?subject')) {
          console.error(
            'Error: batch mode requires SPARQL containing ?subject (replaced per instance).'
          );
          process.exit(1);
        }

        const subjects = loader.findInstancesOfClass(store, classUri);
        if (subjects.length === 0) {
          console.log('No instances found for class; nothing to generate.');
          return;
        }

        for (const subjectUri of subjects) {
          const q = queryText.replace(/\?subject/g, `<${subjectUri}>`);
          const rows = await executeSparqlQuery(store, q, mergedPrefixes);
          const flat = nunjucksFlatFromFirstRow(rows);
          const result = await renderWithOptions(absTemplate, rows, {
            outputDir,
            dryRun,
            force,
            context: {
              ...flat,
              subject: subjectUri,
              prefixes: mergedPrefixes,
            },
          });
          console.log(
            dryRun
              ? `[dry-run] ${result.finalPath}`
              : result.written
                ? `Wrote ${result.finalPath}`
                : `[skipped] ${result.finalPath}`
          );
        }
        return;
      }

      const rows = await executeSparqlQuery(store, queryText, mergedPrefixes);
      const flat = nunjucksFlatFromFirstRow(rows);

      const result = await renderWithOptions(absTemplate, rows, {
        outputDir,
        dryRun,
        force,
        context: {
          ...flat,
          prefixes: mergedPrefixes,
        },
      });

      console.log(
        dryRun
          ? `[dry-run] ${result.finalPath} (${result.bytes ?? 0} bytes)`
          : result.written
            ? `Wrote ${result.finalPath} (${result.bytes ?? 0} bytes)`
            : `[skipped] ${result.finalPath}`
      );
    } catch (error) {
      console.error(`Error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * List: discover templates under packages/cli/templates/sync (or --dir)
 */
const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List discovered .njk templates (default: bundled sync templates)',
  },
  options: {
    format: {
      type: 'string',
      description: 'Output format (table|json)',
      alias: 'f',
      default: 'table',
    },
    dir: {
      type: 'string',
      description: 'Directory to scan for templates',
    },
  },
  async run(ctx) {
    const dir = ctx.args.dir ? resolve(ctx.args.dir) : DEFAULT_TEMPLATES_DIR;
    let templates = [];
    try {
      templates = await discoverTemplates(dir, { recursive: true });
    } catch (e) {
      console.error(`Error: ${e.message}`);
      process.exit(1);
    }

    if (ctx.args.format === 'json') {
      console.log(JSON.stringify(templates, null, 2));
      return;
    }

    const data = [
      ['Name', 'Path', 'to', 'Description'],
      ...templates.map(t => [t.name, t.path, t.hasOutputPath ? 'yes' : 'no', t.description ?? '']),
    ];
    console.log(table(data));
  },
});

/**
 * Query: SELECT + template-oriented context (use `unrdf query` for full SPARQL)
 */
const templateQueryCommand = defineCommand({
  meta: {
    name: 'query',
    description:
      'Run SPARQL SELECT on an RDF file and print template-style context. ' +
      'For CONSTRUCT/ASK/DESCRIBE use `unrdf query`.',
  },
  args: {
    file: {
      type: 'string',
      description: 'RDF data file',
      required: true,
    },
  },
  options: {
    sparql: {
      type: 'string',
      description: 'SPARQL SELECT query',
      alias: 'q',
    },
    predicate: {
      type: 'string',
      description: 'Find all values of predicate',
      alias: 'p',
    },
    subject: {
      type: 'string',
      description: 'Find all predicates for subject',
      alias: 's',
    },
    format: {
      type: 'string',
      description: 'Output format (table|json)',
      alias: 'f',
      default: 'table',
    },
  },
  async run(ctx) {
    if (!existsSync(ctx.args.file)) {
      console.error(`Error: File not found: ${ctx.args.file}`);
      process.exit(1);
    }

    const loader = new RdfTemplateLoader();

    try {
      const store = await loader.loadFromFile(ctx.args.file);

      if (ctx.args.sparql) {
        const results = loader.queryToContext(store, ctx.args.sparql);
        if (ctx.args.format === 'json') {
          console.log(JSON.stringify(results, null, 2));
        } else {
          const rows = results.$rdf?.raw ?? [];
          if (rows.length === 0) {
            console.log('(no results)');
            return;
          }
          const keys = Object.keys(rows[0]);
          const data = [
            ['#', ...keys],
            ...rows.map((row, i) => [i + 1, ...keys.map(k => String(row[k] ?? ''))]),
          ];
          console.log(table(data));
        }
      } else {
        console.log('Provide --sparql / -q (see also `unrdf query` for other query forms).');
      }
    } catch (error) {
      console.error(`Error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Extract: all properties for a subject (RDF context for templates)
 */
const extractCommand = defineCommand({
  meta: {
    name: 'extract',
    description: 'Extract properties for a subject as JSON (template debugging)',
  },
  args: {
    file: {
      type: 'string',
      description: 'RDF data file',
      required: true,
    },
  },
  options: {
    subject: {
      type: 'string',
      description: 'Subject URI',
      alias: 's',
    },
    format: {
      type: 'string',
      description: 'Output format (json|yaml|table)',
      alias: 'f',
      default: 'json',
    },
  },
  async run(ctx) {
    if (!existsSync(ctx.args.file)) {
      console.error(`Error: File not found: ${ctx.args.file}`);
      process.exit(1);
    }

    const loader = new RdfTemplateLoader();

    try {
      const store = await loader.loadFromFile(ctx.args.file);

      if (ctx.args.subject) {
        const context = loader.createInstanceContext(store, ctx.args.subject);
        console.log(JSON.stringify(context, null, 2));
      } else {
        console.log('Provide --subject / -s');
      }
    } catch (error) {
      console.error(`Error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Main template command (parent)
 */
export const templateCommand = defineCommand({
  meta: {
    name: 'template',
    description:
      'Ontology-driven templates (same Nunjucks + Hygen stack as `unrdf sync`). ' +
      'Use `unrdf query` for CONSTRUCT/ASK/DESCRIBE; `template query` is for SELECT + RDF context.',
  },
  subCommands: {
    generate: generateCommand,
    list: listCommand,
    query: templateQueryCommand,
    extract: extractCommand,
  },
});
