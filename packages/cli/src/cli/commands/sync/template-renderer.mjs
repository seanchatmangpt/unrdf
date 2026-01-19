/**
 * @file Template Renderer
 * @module cli/commands/sync/template-renderer
 * @description Renders code generation templates using Nunjucks
 */
import { readFile, writeFile, mkdir, readdir, stat } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, dirname, basename, extname, join, isAbsolute } from 'path';
import matter from 'gray-matter';
import nunjucks from 'nunjucks';
import { COMMON_PREFIXES } from '@unrdf/core';

export const TEMPLATE_EXTENSIONS = ['.njk', '.nunjucks', '.jinja', '.jinja2', '.j2', '.tera'];
export const DEFAULT_PREFIXES = { ...COMMON_PREFIXES };

/**
 * Render template with SPARQL results
 * @param {string} templatePath - Path to template file
 * @param {Array} sparqlResults - SPARQL query results
 * @param {Object} [context] - Additional context variables
 * @returns {Promise<Object>} Render result with content, outputPath, etc.
 */
export async function renderTemplate(templatePath, sparqlResults, context = {}) {
  const absPath = resolve(templatePath);
  if (!existsSync(absPath)) throw new Error(`Template not found: ${absPath}`);

  const templateContent = await readFile(absPath, 'utf-8');
  const { data: frontmatter, content: template } = matter(templateContent);
  const env = createNunjucksEnvironment(dirname(absPath));

  const renderContext = {
    sparql_results: sparqlResults,
    results: sparqlResults,
    prefixes: { ...DEFAULT_PREFIXES, ...(context.prefixes || {}) },
    now: new Date(),
    ...frontmatter.variables,
    ...context,
  };

  let rendered;
  try {
    rendered = env.renderString(template, renderContext);
  } catch (err) {
    // Extract line/column info from Nunjucks error
    const lineInfo = extractNunjucksLineInfo(err);
    let errorMsg = `Template rendering failed: ${err.message}`;

    if (lineInfo.line !== null) {
      errorMsg += `\n  Line ${lineInfo.line}${lineInfo.column !== null ? `, Column ${lineInfo.column}` : ''}`;
    }

    // Add context-specific suggestions
    const suggestions = getTemplateSuggestions(err.message, renderContext);
    if (suggestions.length > 0) {
      errorMsg += '\n\nPossible fixes:\n  ' + suggestions.join('\n  ');
    }

    throw new Error(errorMsg);
  }

  let outputPath = frontmatter.to || context.outputPath;
  if (outputPath?.includes('{{')) outputPath = env.renderString(outputPath, renderContext);

  return {
    content: rendered,
    outputPath,
    description: frontmatter.description,
    mode: frontmatter.mode || 'overwrite',
    frontmatter,
    templatePath: absPath,
  };
}

/**
 * Create Nunjucks environment with custom filters
 * @param {string} [templatesDir] - Templates directory
 * @returns {nunjucks.Environment} Nunjucks environment
 */
export function createNunjucksEnvironment(templatesDir) {
  const loader = templatesDir ? new nunjucks.FileSystemLoader(templatesDir) : null;
  const env = new nunjucks.Environment(loader, { autoescape: false, trimBlocks: true, lstripBlocks: true });

  // Case filters
  env.addFilter('camelCase', s => s?.replace(/[-_\s]+(.)?/g, (_, c) => c?.toUpperCase() || '') || '');
  env.addFilter('pascalCase', s => { const c = s?.replace(/[-_\s]+(.)?/g, (_, c) => c?.toUpperCase() || '') || ''; return c.charAt(0).toUpperCase() + c.slice(1); });
  env.addFilter('snakeCase', s => s?.replace(/([A-Z])/g, '_$1').toLowerCase().replace(/^_/, '').replace(/[-\s]+/g, '_') || '');
  env.addFilter('kebabCase', s => s?.replace(/([A-Z])/g, '-$1').toLowerCase().replace(/^-/, '').replace(/[_\s]+/g, '-') || '');

  // RDF filters
  env.addFilter('localName', uri => uri?.split(/[#/]/).pop() || '');
  env.addFilter('namespace', uri => { const i = Math.max(uri?.lastIndexOf('#') ?? -1, uri?.lastIndexOf('/') ?? -1); return i >= 0 ? uri.substring(0, i + 1) : ''; });
  env.addFilter('expand', (prefixedName, prefixes = DEFAULT_PREFIXES) => {
    if (!prefixedName?.includes(':')) return prefixedName;
    const [prefix, local] = prefixedName.split(':', 2);
    const ns = prefixes[prefix] || DEFAULT_PREFIXES[prefix];
    return ns ? ns + local : prefixedName;
  });
  env.addFilter('toTurtle', triples => {
    if (!Array.isArray(triples)) return '';
    return triples.map(t => `${t.s} ${t.p} ${t.o} .`).join('\n');
  });

  // Type filters
  env.addFilter('zodType', t => ({ string: 'z.string()', integer: 'z.number().int()', int: 'z.number().int()', float: 'z.number()', boolean: 'z.boolean()', date: 'z.string().date()', anyURI: 'z.string().url()' }[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'z.string()'));
  env.addFilter('jsdocType', t => ({ string: 'string', integer: 'number', int: 'number', float: 'number', boolean: 'boolean' }[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'string'));

  // Data filters
  env.addFilter('groupBy', (arr, key) => { const g = {}; for (const i of arr || []) { const k = i[key] || i[`?${key}`] || 'undefined'; (g[k] = g[k] || []).push(i); } return g; });
  env.addFilter('distinctValues', (arr, key) => [...new Set((arr || []).map(i => i[key] || i[`?${key}`]).filter(v => v != null))]);
  env.addFilter('sortBy', (arr, key, dir = 'asc') => { const s = [...(arr || [])].sort((a, b) => { const av = a[key] || a[`?${key}`] || '', bv = b[key] || b[`?${key}`] || ''; return av < bv ? -1 : av > bv ? 1 : 0; }); return dir === 'desc' ? s.reverse() : s; });
  env.addFilter('keys', obj => obj ? Object.keys(obj) : []);
  env.addFilter('values', obj => obj ? Object.values(obj) : []);
  env.addFilter('items', obj => obj ? Object.entries(obj) : []);

  // String filters
  env.addFilter('indent', (s, n = 2) => s?.split('\n').map(l => ' '.repeat(n) + l).join('\n') || '');
  env.addFilter('quote', (s, c = '"') => `${c}${String(s ?? '').replace(new RegExp(c, 'g'), '\\' + c)}${c}`);
  env.addFilter('date', (d, f = 'YYYY-MM-DD') => { const dt = d instanceof Date ? d : new Date(); const p = n => String(n).padStart(2, '0'); return f.replace('YYYY', dt.getFullYear()).replace('MM', p(dt.getMonth() + 1)).replace('DD', p(dt.getDate())).replace('HH', p(dt.getHours())).replace('mm', p(dt.getMinutes())).replace('ss', p(dt.getSeconds())); });

  return env;
}

/**
 * Create a template engine with API matching test expectations
 * @param {Object} [options] - Engine options
 * @param {string} [options.templateDir] - Templates directory
 * @param {Object} [options.prefixes] - Custom prefixes
 * @returns {Promise<Object>} Template engine with render, renderFile, addFilter, addGlobal, prefixes
 */
export async function createTemplateEngine(options = {}) {
  const { templateDir, prefixes = {} } = options;
  const env = createNunjucksEnvironment(templateDir);
  const enginePrefixes = { ...DEFAULT_PREFIXES, ...prefixes };

  // Add expand filter with merged prefixes
  env.addFilter('expand', (prefixedName) => {
    if (!prefixedName?.includes(':')) return prefixedName;
    const [prefix, local] = prefixedName.split(':', 2);
    const ns = enginePrefixes[prefix];
    return ns ? ns + local : prefixedName;
  });

  // Add global functions
  env.addGlobal('uri', (prefixedName) => {
    if (!prefixedName?.includes(':')) return prefixedName;
    const [prefix, local] = prefixedName.split(':', 2);
    const ns = enginePrefixes[prefix];
    return ns ? ns + local : prefixedName;
  });

  env.addGlobal('literal', (value, langOrDatatype) => {
    if (langOrDatatype && langOrDatatype.startsWith('http')) {
      return `"${value}"^^<${langOrDatatype}>`;
    }
    if (langOrDatatype) {
      return `"${value}"@${langOrDatatype}`;
    }
    return `"${value}"`;
  });

  env.addGlobal('blankNode', (id) => `_:${id}`);

  return {
    render: (template, context) => env.renderString(template, context),
    renderFile: (filename, context) => env.render(filename, context),
    addFilter: (name, fn) => env.addFilter(name, fn),
    addGlobal: (name, value) => env.addGlobal(name, value),
    prefixes: enginePrefixes,
  };
}

/**
 * Render with options and write to disk
 * @param {string} templatePath - Path to template
 * @param {Array} sparqlResults - SPARQL results
 * @param {Object} [options] - Render options including context, dryRun, outputDir, force
 * @returns {Promise<Object>} Render result
 */
export async function renderWithOptions(templatePath, sparqlResults, options = {}) {
  const { dryRun = false, outputDir, force = false, outputPath: overridePath, context = {} } = options;
  const result = await renderTemplate(templatePath, sparqlResults, { ...context, output_dir: outputDir });

  // Allow outputPath override from options
  let finalOutputPath = overridePath || result.outputPath;
  if (!finalOutputPath) throw new Error(`Template ${templatePath} does not specify output path`);

  // Determine final path based on whether outputDir is provided and if path is absolute
  let finalPath;
  if (isAbsolute(finalOutputPath)) {
    finalPath = finalOutputPath;
  } else if (outputDir) {
    finalPath = resolve(outputDir, finalOutputPath);
  } else {
    finalPath = resolve(dirname(templatePath), finalOutputPath);
  }

  if (dryRun) {
    return {
      ...result,
      finalPath,
      status: 'dry-run',
      bytes: Buffer.byteLength(result.content, 'utf-8'),
      written: false,
      dryRun: true,
    };
  }

  if (existsSync(finalPath) && !force && result.mode === 'skip_existing') {
    return {
      ...result,
      finalPath,
      status: 'skipped',
      written: false,
      skipped: true,
      dryRun: false,
    };
  }

  await mkdir(dirname(finalPath), { recursive: true });
  if (result.mode === 'append' && existsSync(finalPath)) {
    const existing = await readFile(finalPath, 'utf-8');
    await writeFile(finalPath, existing + '\n' + result.content, 'utf-8');
  } else {
    await writeFile(finalPath, result.content, 'utf-8');
  }

  return {
    ...result,
    finalPath,
    status: 'success',
    bytes: Buffer.byteLength(result.content, 'utf-8'),
    written: true,
    dryRun: false,
  };
}

/**
 * Batch render multiple templates
 * @param {Array<{path: string, sparqlResults?: Array, context?: Object}>} templates - Templates to render
 * @param {Object} [sharedContext] - Context shared across all templates
 * @param {Object} [options] - Render options
 * @returns {Promise<Array>} Array of render results
 */
export async function batchRender(templates, sharedContext = {}, options = {}) {
  const results = [];
  for (const template of templates) {
    const { path: templatePath, sparqlResults = [], context = {} } = template;
    const mergedContext = { ...sharedContext, ...context };

    // For batch render, we render without frontmatter requirements
    const absPath = resolve(templatePath);
    if (!existsSync(absPath)) {
      results.push({ error: `Template not found: ${absPath}`, templatePath: absPath });
      continue;
    }

    const templateContent = await readFile(absPath, 'utf-8');
    const { data: frontmatter, content: templateBody } = matter(templateContent);
    const env = createNunjucksEnvironment(dirname(absPath));

    const renderContext = {
      sparql_results: sparqlResults,
      results: sparqlResults,
      prefixes: { ...DEFAULT_PREFIXES, ...(mergedContext.prefixes || {}) },
      now: new Date(),
      ...frontmatter.variables,
      ...mergedContext,
    };

    try {
      const rendered = env.renderString(templateBody, renderContext);
      results.push({
        content: rendered,
        outputPath: frontmatter.to,
        templatePath: absPath,
        frontmatter,
      });
    } catch (err) {
      const lineInfo = extractNunjucksLineInfo(err);
      const errorDetails = lineInfo.line !== null
        ? `${err.message} (Line ${lineInfo.line}${lineInfo.column !== null ? `, Column ${lineInfo.column}` : ''})`
        : err.message;

      results.push({ error: errorDetails, templatePath: absPath });
    }
  }
  return results;
}

/**
 * Discover templates in a directory
 * @param {string} dir - Directory to search
 * @param {Object} [options] - Discovery options
 * @param {boolean} [options.recursive=false] - Search recursively
 * @returns {Promise<Array>} Array of template metadata
 */
export async function discoverTemplates(dir, options = {}) {
  const { recursive = false } = options;
  const absDir = resolve(dir);

  if (!existsSync(absDir)) {
    throw new Error(`Template directory not found: ${absDir}`);
  }

  const templates = [];

  async function scanDir(currentDir) {
    const entries = await readdir(currentDir);
    for (const entry of entries) {
      const fullPath = join(currentDir, entry);
      const entryStat = await stat(fullPath);

      if (entryStat.isDirectory() && recursive) {
        await scanDir(fullPath);
      } else if (entryStat.isFile()) {
        const ext = extname(entry);
        if (TEMPLATE_EXTENSIONS.includes(ext)) {
          const content = await readFile(fullPath, 'utf-8');
          const { data: frontmatter } = matter(content);

          templates.push({
            name: basename(entry, ext),
            path: fullPath,
            extension: ext,
            hasOutputPath: !!frontmatter.to,
            description: frontmatter.description,
            frontmatter,
          });
        }
      }
    }
  }

  await scanDir(absDir);
  return templates;
}

/**
 * Extract line and column information from Nunjucks error
 * @param {Error} err - Nunjucks error
 * @returns {Object} Object with line and column (or null if not found)
 */
function extractNunjucksLineInfo(err) {
  // Nunjucks errors include line/column in message or properties
  const msg = err.message || String(err);

  // Try to extract from error properties first (most reliable)
  if (err.lineno !== undefined) {
    return {
      line: err.lineno,
      column: err.colno !== undefined ? err.colno : null,
    };
  }

  // Try to extract from error message
  const patterns = [
    /\(line (\d+), column (\d+)\)/,
    /at line (\d+),?\s*column (\d+)/i,
    /\[Line (\d+), Column (\d+)\]/,
  ];

  for (const pattern of patterns) {
    const match = msg.match(pattern);
    if (match) {
      return {
        line: parseInt(match[1], 10),
        column: parseInt(match[2], 10),
      };
    }
  }

  return { line: null, column: null };
}

/**
 * Get template fix suggestions based on error message and context
 * @param {string} errorMsg - Error message
 * @param {Object} context - Render context
 * @returns {Array<string>} Array of suggestion strings
 */
function getTemplateSuggestions(errorMsg, context = {}) {
  const suggestions = [];
  const msg = errorMsg.toLowerCase();

  if (msg.includes('undefined') || msg.includes('is not defined')) {
    suggestions.push('Check that all variables used in template are defined in context');

    // Try to extract variable name
    const varMatch = errorMsg.match(/['"]?(\w+)['"]?\s+is\s+(not\s+)?defined/i);
    if (varMatch) {
      const varName = varMatch[1];
      suggestions.push(`Variable '${varName}' is not available. Available: ${Object.keys(context).join(', ')}`);
    } else {
      suggestions.push(`Available context variables: ${Object.keys(context).join(', ')}`);
    }
  }

  if (msg.includes('filter') || msg.includes('unknown filter')) {
    suggestions.push('Check that custom filters are registered with the template engine');
    suggestions.push('Available filters: camelCase, pascalCase, snakeCase, kebabCase, localName, namespace, expand, etc.');
  }

  if (msg.includes('unexpected token') || msg.includes('unexpected end of')) {
    suggestions.push('Check for unmatched {% ... %} or {{ ... }} tags');
    suggestions.push('Verify all blocks have matching {% endfor %}, {% endif %}, etc.');
  }

  if (msg.includes('expected') || msg.includes('unexpected')) {
    suggestions.push('Check Nunjucks syntax - ensure proper tag structure');
    suggestions.push('Example: {% for item in items %}...{% endfor %}');
  }

  if (msg.includes('cannot read') || msg.includes('cannot access')) {
    suggestions.push('Check for null/undefined values before accessing properties');
    suggestions.push('Use safe navigation: {{ variable.property if variable }}');
  }

  // Generic suggestions if none matched
  if (suggestions.length === 0) {
    suggestions.push('Verify Nunjucks template syntax is valid');
    suggestions.push('Check that all loops and conditionals are properly closed');
  }

  return suggestions;
}

export default { renderTemplate, createNunjucksEnvironment, createTemplateEngine, renderWithOptions, batchRender, discoverTemplates, TEMPLATE_EXTENSIONS, DEFAULT_PREFIXES };
