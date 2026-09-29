/**
 * @fileoverview Template rendering and output helpers for the playground CLI
 *
 * The .tex.njk templates in ../templates use a `texescape` filter and named
 * content variables (introduction, method, ...). This module provides the
 * Nunjucks environment that supplies them and writes rendered output to disk.
 *
 * @module playground/render
 */

import { mkdirSync, writeFileSync } from 'node:fs';
import { dirname, isAbsolute, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import nunjucks from 'nunjucks';

/** Directory holding the bundled templates. */
export const TEMPLATE_DIR = fileURLToPath(new URL('../templates/', import.meta.url));

const TEX_ESCAPES = {
  '\\': '\\textbackslash{}',
  '&': '\\&',
  '%': '\\%',
  $: '\\$',
  '#': '\\#',
  _: '\\_',
  '{': '\\{',
  '}': '\\}',
  '~': '\\textasciitilde{}',
  '^': '\\textasciicircum{}'
};

/**
 * Escape LaTeX special characters.
 *
 * @param {unknown} value - Value to escape (null/undefined become '')
 * @returns {string} Escaped text
 */
export function texEscape(value) {
  if (value === null || value === undefined) {
return '';
}
  return String(value).replace(/[\\&%$#_{}~^]/g, ch => TEX_ESCAPES[ch]);
}

/**
 * Create the Nunjucks environment used for all playground templates.
 *
 * @param {string} [searchPath] - Template directory
 * @returns {import('nunjucks').Environment} Environment
 */
export function createEnvironment(searchPath = TEMPLATE_DIR) {
  const env = new nunjucks.Environment(new nunjucks.FileSystemLoader(searchPath), {
    autoescape: false,
    throwOnUndefined: false
  });
  env.addFilter('texescape', texEscape);
  return env;
}

/**
 * Turn a section heading into template variable names ("Methods" gives
 * "methods" and "method"; "Design & Development" gives "design").
 *
 * @param {string} heading - Section heading
 * @returns {string[]} Candidate variable names
 */
export function headingVariables(heading) {
  const first = heading
    .toLowerCase()
    .split(/[^a-z0-9]+/)
    .find(Boolean);
  if (!first) {
return [];
}
  return first.endsWith('s') && first.length > 3 ? [first, first.slice(0, -1)] : [first];
}

/**
 * Render a template file.
 *
 * @param {string} template - Template file name in TEMPLATE_DIR, or a path to a custom template
 * @param {{title: string, author: string, abstract?: string, sections?: Array<{heading: string, content?: string}>}} data - Document data
 * @param {Record<string, unknown>} [extra] - Extra template variables
 * @returns {string} Rendered document
 */
export function renderTemplate(template, data, extra = {}) {
  const sections = (data.sections ?? []).map(s => ({ title: s.heading, content: s.content ?? '' }));
  const sectionVars = {};
  for (const section of data.sections ?? []) {
    if (!section.content) {
continue;
}
    for (const name of headingVariables(section.heading)) {
      sectionVars[name] = section.content;
    }
  }
  const context = { ...data, ...sectionVars, ...extra, sections };

  if (isAbsolute(template) || template.includes('/') || template.includes('\\')) {
    const file = resolve(template);
    return createEnvironment(dirname(file)).render(file.slice(dirname(file).length + 1), context);
  }
  return createEnvironment().render(template, context);
}

/**
 * Render a document as Markdown (no template required).
 *
 * @param {{title: string, abstract?: string, sections?: Array<{heading: string, content?: string}>}} data - Document data
 * @param {string} byline - Author line
 * @returns {string} Markdown
 */
export function renderMarkdown(data, byline) {
  const parts = [`# ${data.title}`, '', byline, ''];
  if (data.abstract) {
parts.push('## Abstract', '', data.abstract, '');
}
  for (const section of data.sections ?? []) {
    parts.push(`## ${section.heading}`, '', section.content ?? '', '');
  }
  return parts.join('\n');
}

/**
 * Write output, creating parent directories.
 *
 * @param {string} outputPath - Destination path
 * @param {string} content - File content
 * @returns {string} Absolute path written
 */
export function writeOutput(outputPath, content) {
  const file = resolve(outputPath);
  mkdirSync(dirname(file), { recursive: true });
  writeFileSync(file, content);
  return file;
}
