/**
 * @file Template Renderer
 * @module cli/commands/sync/template-renderer
 * @description Renders code generation templates using Nunjucks
 */
import { readFile, writeFile, mkdir } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, dirname } from 'path';
import matter from 'gray-matter';
import nunjucks from 'nunjucks';
import { COMMON_PREFIXES } from '@unrdf/core';

export const TEMPLATE_EXTENSIONS = ['.njk', '.nunjucks', '.jinja', '.jinja2', '.j2', '.tera'];
export const DEFAULT_PREFIXES = { ...COMMON_PREFIXES };

/**
 * Render template with SPARQL results
 */
export async function renderTemplate(templatePath, sparqlResults, context = {}) {
  if (!existsSync(templatePath)) throw new Error(`Template not found: ${templatePath}`);
  
  const templateContent = await readFile(templatePath, 'utf-8');
  const { data: frontmatter, content: template } = matter(templateContent);
  const env = createNunjucksEnvironment(dirname(templatePath));
  
  const renderContext = {
    sparql_results: sparqlResults,
    results: sparqlResults,
    prefixes: { ...DEFAULT_PREFIXES, ...(context.prefixes || {}) },
    now: new Date(),
    ...frontmatter.variables,
    ...context,
  };
  
  let rendered;
  try { rendered = env.renderString(template, renderContext); }
  catch (err) { throw new Error(`Template rendering failed: ${err.message}`); }
  
  let outputPath = frontmatter.to || context.outputPath;
  if (outputPath?.includes('{{')) outputPath = env.renderString(outputPath, renderContext);
  
  return { content: rendered, outputPath, description: frontmatter.description, mode: frontmatter.mode || 'overwrite', frontmatter };
}

/**
 * Create Nunjucks environment with custom filters
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
 * Render with options and write to disk
 */
export async function renderWithOptions(templatePath, sparqlResults, options = {}) {
  const { dryRun = false, outputDir, force = false, context = {} } = options;
  const result = await renderTemplate(templatePath, sparqlResults, { ...context, output_dir: outputDir });
  
  if (!result.outputPath) throw new Error(`Template ${templatePath} does not specify output path`);
  const finalPath = outputDir ? resolve(outputDir, result.outputPath) : resolve(dirname(templatePath), result.outputPath);
  
  if (dryRun) return { ...result, finalPath, status: 'dry-run', bytes: Buffer.byteLength(result.content, 'utf-8') };
  if (existsSync(finalPath) && !force && result.mode === 'skip_existing') return { ...result, finalPath, status: 'skipped' };
  
  await mkdir(dirname(finalPath), { recursive: true });
  if (result.mode === 'append' && existsSync(finalPath)) {
    const existing = await readFile(finalPath, 'utf-8');
    await writeFile(finalPath, existing + '\n' + result.content, 'utf-8');
  } else await writeFile(finalPath, result.content, 'utf-8');
  
  return { ...result, finalPath, status: 'success', bytes: Buffer.byteLength(result.content, 'utf-8') };
}

export default { renderTemplate, createNunjucksEnvironment, renderWithOptions, TEMPLATE_EXTENSIONS, DEFAULT_PREFIXES };
