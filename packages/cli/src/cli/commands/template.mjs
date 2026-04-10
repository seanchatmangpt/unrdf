/**
 * Template Command - Ontology-driven template generation
 *
 * RDF + SPARQL layer uses {@link RdfTemplateLoader}; render/write uses
 * sync {@link renderWithOptions} / {@link discoverTemplates} only.
 *
 * @module cli/commands/template
 */

import { defineCommand } from 'citty';
import { existsSync, readFileSync } from 'node:fs';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { basename, dirname, resolve, isAbsolute } from 'node:path';
import matter from 'gray-matter';
import { table } from 'table';
import { COMMON_PREFIXES } from '@unrdf/core';
import * as yaml from 'js-yaml';

import { RdfTemplateLoader, extractPrefixesFromTurtle } from '../../lib/rdf-template-loader.mjs';
import { executeSparqlQuery } from './sync/sparql-executor.mjs';
import { renderWithOptions, discoverTemplates } from './sync/template-renderer.mjs';
import { FrontmatterParser } from '../../lib/frontmatter-parser.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const DEFAULT_TEMPLATES_DIR = resolve(__dirname, '../../../templates/sync');
// Resolve catalog path relative to workspace root
// Template command is at: packages/cli/src/cli/commands/template.mjs
// Workspace root is: ../../../../../ (go up from unrdf to chatmangpt)
const TEMPLATE_CATALOG_PATH = resolve(__dirname, '../../../../../../.template-catalog.yaml');

/**
 * Discover local templates from _templates directory (hygen-style)
 * @param {string} cwd - Current working directory
 * @returns {Array} Array of local template objects
 */
async function discoverLocalTemplates(cwd = process.cwd()) {
  const localTemplatesDir = resolve(cwd, '_templates');

  if (!existsSync(localTemplatesDir)) {
    return [];
  }

  try {
    const templates = await discoverTemplates(localTemplatesDir, { recursive: true });

    // Mark templates as local and add metadata
    return templates.map(tmpl => ({
      ...tmpl,
      source: 'local',
      source_type: 'project',
      priority: 'high'
    }));
  } catch (error) {
    console.error(`Warning: Failed to discover local templates: ${error.message}`);
    return [];
  }
}

/**
 * Load local template catalog from _templates/.template-catalog.yaml
 * @param {string} cwd - Current working directory
 * @returns {Object} Local template catalog data
 */
function loadLocalCatalog(cwd = process.cwd()) {
  const localCatalogPath = resolve(cwd, '_templates/.template-catalog.yaml');

  if (!existsSync(localCatalogPath)) {
    return { templates: {}, categories: {} };
  }

  try {
    const catalogContent = readFileSync(localCatalogPath, 'utf-8');
    return yaml.load(catalogContent);
  } catch (error) {
    console.error(`Warning: Failed to load local template catalog: ${error.message}`);
    return { templates: {}, categories: {} };
  }
}

/**
 * Load template catalog from .template-catalog.yaml
 * @returns {Object} Template catalog data
 */
function loadTemplateCatalog() {
  try {
    if (!existsSync(TEMPLATE_CATALOG_PATH)) {
      return { templates: {}, categories: {}, frontmatter_schema: {} };
    }
    const catalogContent = readFileSync(TEMPLATE_CATALOG_PATH, 'utf-8');
    return yaml.load(catalogContent);
  } catch (error) {
    console.error(`Warning: Failed to load template catalog: ${error.message}`);
    return { templates: {}, categories: {}, frontmatter_schema: {} };
  }
}

/**
 * Merge global and local template catalogs (local templates override global)
 * @param {string} cwd - Current working directory
 * @returns {Object} Merged template catalog
 */
async function loadMergedCatalog(cwd = process.cwd()) {
  const globalCatalog = loadTemplateCatalog();
  const localCatalog = loadLocalCatalog(cwd);
  const localDiscovered = await discoverLocalTemplates(cwd);

  const merged = { ...globalCatalog };

  // Merge templates by category (local overrides global)
  for (const [catName, catTemplates] of Object.entries(localCatalog.templates || {})) {
    if (!merged.templates[catName]) {
      merged.templates[catName] = [];
    }

    // Add/override with local catalog templates
    for (const tmpl of catTemplates) {
      const existingIdx = merged.templates[catName].findIndex(t => t.name === tmpl.name);
      if (existingIdx >= 0) {
        merged.templates[catName][existingIdx] = {
          ...tmpl,
          source: 'local',
          priority: 'high'
        };
      } else {
        merged.templates[catName].push({
          ...tmpl,
          source: 'local',
          priority: 'high'
        });
      }
    }
  }

  // Add discovered local templates not in catalog
  for (const tmpl of localDiscovered) {
    const catName = tmpl.category || 'scaffolding';
    if (!merged.templates[catName]) {
      merged.templates[catName] = [];
    }

    const existingIdx = merged.templates[catName].findIndex(t => t.path === tmpl.path);
    if (existingIdx < 0) {
      merged.templates[catName].push({
        name: tmpl.name || basename(tmpl.path),
        ...tmpl,
        source: 'local',
        priority: 'high'
      });
    }
  }

  return merged;
}

/**
 * Filter templates by category
 * @param {Object} catalog - Template catalog
 * @param {string} category - Category to filter by
 * @returns {Array} Filtered templates
 */
function filterByCategory(catalog, category) {
  const filtered = [];
  const categoryLower = category.toLowerCase();

  for (const [catName, templates] of Object.entries(catalog.templates || {})) {
    if (catName.toLowerCase() === categoryLower) {
      for (const tmpl of templates) {
        filtered.push({
          ...tmpl,
          source_category: catName,
          status: tmpl.status || 'existing'
        });
      }
    }
  }

  return filtered;
}

/**
 * Search templates by name/pattern
 * @param {Object} catalog - Template catalog
 * @param {string} pattern - Search pattern
 * @returns {Array} Matching templates
 */
function searchTemplates(catalog, pattern) {
  const results = [];
  const patternLower = pattern.toLowerCase();

  for (const [catName, templates] of Object.entries(catalog.templates || {})) {
    for (const tmpl of templates) {
      const nameMatch = tmpl.name?.toLowerCase().includes(patternLower);
      const descMatch = tmpl.description?.toLowerCase().includes(patternLower);
      const pathMatch = tmpl.path?.toLowerCase().includes(patternLower);

      if (nameMatch || descMatch || pathMatch) {
        results.push({
          ...tmpl,
          source_category: catName,
          status: tmpl.status || 'existing'
        });
      }
    }
  }

  return results;
}

/**
 * Validate template frontmatter against catalog schema
 * @param {string} templatePath - Path to template file
 * @returns {Object} Validation result
 */
async function validateTemplate(templatePath) {
  try {
    const absPath = resolve(templatePath);
    if (!existsSync(absPath)) {
      return { valid: false, errors: [`Template not found: ${absPath}`] };
    }

    const content = await readFile(absPath, 'utf-8');
    const { data: frontmatter } = matter(content);

    const catalog = loadTemplateCatalog();
    const requiredFields = catalog.frontmatter_schema?.required || ['to', 'description'];
    const errors = [];

    for (const field of requiredFields) {
      if (!frontmatter[field]) {
        errors.push(`Missing required field: ${field}`);
      }
    }

    const valid = errors.length === 0;
    return { valid, errors, frontmatter };
  } catch (error) {
    return { valid: false, errors: [`Validation error: ${error.message}`] };
  }
}

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
    template: {
      type: 'string',
      description: 'Template .njk path',
      required: true,
    },
    outputDir: {
      type: 'string',
      description: 'Output directory for generated files',
      default: './src',
    },
    subject: {
      type: 'string',
      description: 'Focus subject URI; replaces ?subject in SPARQL when present',
      required: false,
    },
    batch: {
      type: 'boolean',
      description: 'One output per instance of `--class-uri`; SPARQL must use ?subject',
      default: false,
    },
    classUri: {
      type: 'string',
      description: 'RDFS/OWL class IRI for batch mode (with `--batch`)',
      required: false,
    },
    sparql: {
      type: 'string',
      description: 'SPARQL SELECT (overrides frontmatter `sparql:`)',
      required: false,
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

    // Validate frontmatter
    const fmParser = new FrontmatterParser({ strict: true });
    const fmValidation = fmParser.validate(fm);
    if (!fmValidation.valid) {
      console.error(
        `Error: Template frontmatter validation failed:\n${fmValidation.errors.join('\n')}`
      );
      process.exit(1);
    }

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
 * List: discover templates from catalog or filesystem (or --dir)
 */
const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List templates from catalog or discover .njk files in directory',
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
      description: 'Directory to scan for templates (bypasses catalog)',
    },
    category: {
      type: 'string',
      description: 'Filter by template category',
      alias: 'c',
    },
    pattern: {
      type: 'string',
      description: 'Search by name/pattern',
      alias: 'p',
    },
    all: {
      type: 'boolean',
      description: 'Show all templates including planned (status=planned)',
      default: false,
    },
  },
  async run(ctx) {
    let templates = [];

    // If --dir is specified, scan filesystem instead of using catalog
    if (ctx.args.dir) {
      const dir = resolve(ctx.args.dir);
      try {
        templates = await discoverTemplates(dir, { recursive: true });
      } catch (e) {
        console.error(`Error: ${e.message}`);
        process.exit(1);
      }
    } else {
      // Use merged template catalog (global + local _templates)
      const catalog = await loadMergedCatalog();

      // Apply filters
      if (ctx.args.category) {
        templates = filterByCategory(catalog, ctx.args.category);
      } else if (ctx.args.pattern) {
        templates = searchTemplates(catalog, ctx.args.pattern);
      } else {
        // List all templates from catalog
        for (const [catName, catTemplates] of Object.entries(catalog.templates || {})) {
          for (const tmpl of catTemplates) {
            templates.push({
              ...tmpl,
              source_category: catName,
              status: tmpl.status || 'existing'
            });
          }
        }
      }

      // Filter out planned templates unless --all is specified
      if (!ctx.args.all) {
        templates = templates.filter(t => t.status !== 'planned');
      }
    }

    if (ctx.args.format === 'json') {
      console.log(JSON.stringify(templates, null, 2));
      return;
    }

    if (templates.length === 0) {
      console.log('No templates found.');
      return;
    }

    const data = [
      ['Name', 'Category', 'Description', 'Source', 'Status', 'Path'],
      ...templates.map(t => [
        t.name,
        t.source_category || t.category || 'N/A',
        t.description ?? '',
        t.source === 'local' ? '🏠 Local' : '🌐 Global',
        t.status || 'existing',
        t.path || 'N/A',
      ]),
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
    sparql: {
      type: 'string',
      description: 'SPARQL SELECT query',
      required: false,
    },
    predicate: {
      type: 'string',
      description: 'Find all values of predicate',
      required: false,
    },
    subject: {
      type: 'string',
      description: 'Find all predicates for subject',
      required: false,
    },
    format: {
      type: 'string',
      description: 'Output format (table|json)',
      default: 'table',
      required: false,
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
    subject: {
      type: 'string',
      description: 'Subject URI',
      required: false,
    },
    format: {
      type: 'string',
      description: 'Output format (json|yaml|table)',
      default: 'json',
      required: false,
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
 * Validate: check template frontmatter against catalog schema
 */
const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate template frontmatter against catalog schema',
  },
  args: {
    template: {
      type: 'string',
      description: 'Template file path to validate',
      required: true,
    },
  },
  async run(ctx) {
    const templatePath = ctx.args.template;
    const result = await validateTemplate(templatePath);

    if (result.valid) {
      console.log(`✓ Template validation passed: ${templatePath}`);
      console.log(`  Frontmatter fields: ${Object.keys(result.frontmatter || {}).join(', ')}`);
    } else {
      console.error(`✗ Template validation failed: ${templatePath}`);
      for (const error of result.errors) {
        console.error(`  - ${error}`);
      }
      process.exit(1);
    }
  },
});

/**
 * Catalog: show template catalog information
 */
const catalogCommand = defineCommand({
  meta: {
    name: 'catalog',
    description: 'Show template catalog information',
  },
  options: {
    format: {
      type: 'string',
      description: 'Output format (table|json)',
      alias: 'f',
      default: 'table',
    },
  },
  async run(ctx) {
    const catalog = await loadMergedCatalog();
    const globalCatalog = loadTemplateCatalog();
    const localCatalog = loadLocalCatalog();
    const localTemplates = await discoverLocalTemplates();

    if (ctx.args.format === 'json') {
      console.log(JSON.stringify(catalog, null, 2));
      return;
    }

    console.log('Template Catalog Information:\n');
    console.log(`Global Catalog Path: ${TEMPLATE_CATALOG_PATH}`);
    console.log(`Local Templates Dir: ${resolve(process.cwd(), '_templates')}`);
    console.log(`\nCategories: ${Object.keys(catalog.categories || {}).length}`);
    console.log(`Global Templates: ${Object.values(globalCatalog.templates || {}).flat().length}`);
    console.log(`Local Templates: ${localTemplates.length}`);
    console.log(`Total Templates: ${Object.values(catalog.templates || {}).flat().length}\n`);

    // Show templates by category with source indicators
    const data = [
      ['Category', 'Source', 'Template Count', 'Templates'],
      ...Object.entries(catalog.templates || {}).flatMap(([cat, templates]) =>
        templates.map(t => [
          cat,
          t.source === 'local' ? '🏠 Local' : '🌐 Global',
          '1',
          t.name
        ])
      )
    ];
    console.log(table(data));

    // Show frontmatter schema
    console.log('\nRequired Frontmatter Fields:');
    for (const field of catalog.frontmatter_schema?.required || []) {
      console.log(`  - ${field}`);
    }

    console.log('\nOptional Frontmatter Fields:');
    for (const field of catalog.frontmatter_schema?.optional || []) {
      console.log(`  - ${field}`);
    }

    // Show local template info if any
    if (localTemplates.length > 0) {
      console.log('\n🏠 Local Templates (_templates/):');
      for (const tmpl of localTemplates) {
        console.log(`  - ${tmpl.name || basename(tmpl.path)} (${tmpl.path})`);
      }
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
    validate: validateCommand,
    catalog: catalogCommand,
  },
});
