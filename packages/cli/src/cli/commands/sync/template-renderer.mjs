/**
 * @file Template Renderer
 * @module cli/commands/sync/template-renderer
 * @description Renders code generation templates using Nunjucks
 */
import { readFile, writeFile, mkdir, readdir, stat, chmod, copyFile } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, dirname, basename, extname, join, isAbsolute } from 'path';
import matter from 'gray-matter';
import nunjucks from 'nunjucks';
import { COMMON_PREFIXES } from '@unrdf/core';
import {
  FrontmatterParser,
  getOperationMode,
  shouldSkip,
} from '../../../lib/frontmatter-parser.mjs';

export const TEMPLATE_EXTENSIONS = ['.njk', '.nunjucks', '.jinja', '.jinja2', '.j2', '.tera'];
export const DEFAULT_PREFIXES = { ...COMMON_PREFIXES };

/**
 * Recursively strip leading/trailing double quotes from string values that
 * contain {{ }} template syntax. This is a safety net for values that were
 * auto-quoted by preprocessFrontmatter() but where js-yaml did not strip
 * the quotes during parsing (e.g., nested values, edge cases).
 *
 * @param {*} value - Any value from parsed frontmatter
 * @returns {*} Value with quotes stripped from template-syntax strings
 */
function stripQuotesFromTemplateVars(value) {
  if (typeof value === 'string') {
    // Only strip if the value contains template syntax AND is wrapped in matching quotes
    if ((value.includes('{{') || value.includes('}}')) &&
        ((value.startsWith('"') && value.endsWith('"')) ||
         (value.startsWith("'") && value.endsWith("'")))) {
      return value.slice(1, -1);
    }
    return value;
  }
  if (Array.isArray(value)) {
    return value.map(stripQuotesFromTemplateVars);
  }
  if (value !== null && typeof value === 'object') {
    const result = {};
    for (const [k, v] of Object.entries(value)) {
      result[k] = stripQuotesFromTemplateVars(v);
    }
    return result;
  }
  return value;
}

/**
 * Preprocess YAML frontmatter to auto-quote values containing {{ }} template syntax
 * This fixes js-yaml 3.x's strict parsing that treats unquoted {{ as starting a multi-line key
 *
 * IMPORTANT: Only quotes TOP-LEVEL key-value pairs (no nested children). Nested YAML
 * structures (objects, lists) are left untouched because quoting them breaks js-yaml parsing.
 *
 * @param {string} content - Full template content with frontmatter
 * @returns {string} Preprocessed content with quoted {{ }} values
 */
export function preprocessFrontmatter(content) {
  // Match the frontmatter section between --- markers
  const frontmatterMatch = content.match(/^---\n([\s\S]+?)\n---/);
  if (!frontmatterMatch) return content;

  const lines = frontmatterMatch[1].split('\n');
  const result = [];

  for (const line of lines) {
    const trimmed = line.trimStart();

    // Skip empty lines and comments
    if (!trimmed || trimmed.startsWith('#')) {
      result.push(line);
      continue;
    }

    // Match top-level key: value (line must NOT start with whitespace — no nested entries)
    const topLevelMatch = line.match(/^([a-zA-Z_][a-zA-Z0-9_-]*):\s+(.+)$/);
    if (topLevelMatch) {
      const [, key, rawValue] = topLevelMatch;
      const value = rawValue.trimStart();

      // Skip if value starts a block (object or list indicator)
      if (value === '|' || value === '>' || value === '{' || value === '[') {
        result.push(line);
        continue;
      }
      // Skip if already quoted
      if (value.startsWith('"') || value.startsWith("'")) {
        result.push(line);
        continue;
      }
      // Skip booleans, null, numbers
      if (/^(true|false|null|[0-9]+(\.[0-9]+)?)$/.test(value)) {
        result.push(line);
        continue;
      }
      // If value contains {{ or }}, quote it
      if (value.includes('{{') || value.includes('}}')) {
        result.push(`${key}: "${value}"`);
        continue;
      }
    }

    // For nested lines (indented) or non-matching lines, pass through unchanged
    result.push(line);
  }

  // Replace the frontmatter in the original content
  return content.replace(/^---\n[\s\S]+?\n---/, `---\n${result.join('\n')}\n---`);
}

/**
 * Render template with SPARQL results
 * @param {string} templatePath - Path to template file
 * @param {Array} sparqlResults - SPARQL query results
 * @param {Object} [context] - Additional context variables
 * @returns {Promise<Object>} Render result with content, outputPath, etc.
 */
export async function renderTemplate(templatePath, sparqlResults, context = {}) {
  const env = createNunjucksEnvironment(context.templates_dir || dirname(resolve(templatePath)), context.prefixes || {});
  
  // Inject hardening globals
  const harden_enabled = !!context.harden;
  const renderContext = { 
    ...context, 
    harden: harden_enabled,
    harden_enabled,
    sparql_results: sparqlResults, 
    results: sparqlResults, 
    now: new Date() 
  };
  const absPath = resolve(templatePath);
  if (!existsSync(absPath)) throw new Error(`Template not found: ${absPath}`);

  const templateContent = await readFile(absPath, 'utf-8');
  const preprocessedContent = preprocessFrontmatter(templateContent);
  let frontmatter, rawTemplate;
  try {
    const parsed = matter(preprocessedContent);
    frontmatter = stripQuotesFromTemplateVars(parsed.data);
    rawTemplate = parsed.content;
  } catch (error) {
    throw new Error(
      `Failed to parse template frontmatter from ${absPath}:\n${error.message}\n\n` +
      `Note: Template syntax {{ }} in frontmatter values is auto-quoted.\n` +
      `If the error persists, check for:\n` +
      `  - Malformed YAML (indentation, colons in unquoted strings)\n` +
      `  - Complex template expressions that need manual quoting\n` +
      `  - Multi-line values with template syntax\n`
    );
  }

  // Validate frontmatter against schema
  const hadExplicitTo = 'to' in frontmatter;
  const synthesizedTo = frontmatter.to || context.outputPath;
  const validationFrontmatter = { ...frontmatter };
  if (synthesizedTo && !hadExplicitTo) {
    validationFrontmatter.to = synthesizedTo;
  }


  

  // Interpolate frontmatter strings
  for (const [key, value] of Object.entries(frontmatter)) {
    if (typeof value === "string" && value.includes("{{")) {
      try {
        frontmatter[key] = env.renderString(value, renderContext);
      } catch (e) { /* ignore interpolation errors in frontmatter for now */ }
    }
  }

  const parser = new FrontmatterParser({ strict: true });
  const validation = parser.validate(validationFrontmatter);
  if (!validation.valid) {
    throw new Error(
      `Template frontmatter validation failed for ${absPath}:\n${validation.errors.join('\n')}`
    );
  }

  // Check skip conditions
  if (shouldSkip(frontmatter, context)) {
    return {
      content: '',
      outputPath: frontmatter.to || context.outputPath,
      description: frontmatter.description,
      mode: frontmatter.mode || 'overwrite',
      frontmatter,
      templatePath: absPath,
      skipped: true,
    };
  }

  // from directive: load body from another template file
  let template = rawTemplate;
  if (frontmatter.from) {
    const fromPath = isAbsolute(frontmatter.from)
      ? frontmatter.from
      : resolve(dirname(absPath), frontmatter.from);
    if (!existsSync(fromPath)) throw new Error(`from: template not found: ${fromPath}`);
    template = await readFile(fromPath, 'utf-8');
  }

  

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

  let outputPath = context.outputPath || frontmatter.to;
  if (outputPath?.includes('{{')) outputPath = env.renderString(outputPath, renderContext);

  if (!outputPath) {
    throw new Error(
      'Output path not specified. Set "to:" in template frontmatter or "output_file" in rule configuration.\n' +
        `  Template: ${absPath}`
    );
  }

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
 * @param {string|Array<string>} [searchPaths] - Templates directory or array of search paths
 * @param {Object} [prefixes={}] - Custom namespace prefixes for the expand filter
 * @returns {nunjucks.Environment} Nunjucks environment
 */
export function createNunjucksEnvironment(searchPaths, prefixes = {}) {
  const paths = Array.isArray(searchPaths) ? searchPaths : searchPaths ? [searchPaths] : [];
  const validPaths = paths.filter(Boolean);
  const loader = validPaths.length > 0 ? new nunjucks.FileSystemLoader(validPaths) : null;
  const env = new nunjucks.Environment(loader, {
    autoescape: false,
    trimBlocks: true,
    lstripBlocks: true,
  });

  // Case filters
  env.addFilter(
    'camelCase',
    s => s?.replace(/[-_\s]+(.)?/g, (_, c) => c?.toUpperCase() || '') || ''
  );
  env.addFilter('pascalCase', s => {
    const c = s?.replace(/[-_\s]+(.)?/g, (_, c) => c?.toUpperCase() || '') || '';
    return c.charAt(0).toUpperCase() + c.slice(1);
  });
  env.addFilter(
    'snakeCase',
    s =>
      s
        ?.replace(/([A-Z])/g, '_$1')
        .toLowerCase()
        .replace(/^_/, '')
        .replace(/[-\s]+/g, '_') || ''
  );
  env.addFilter(
    'kebabCase',
    s =>
      s
        ?.replace(/([A-Z])/g, '-$1')
        .toLowerCase()
        .replace(/^-/, '')
        .replace(/[_\s]+/g, '-') || ''
  );
  env.addFilter('slug', s => {
    return s
      ?.toLowerCase()
      .trim()
      .replace(/[^\w\s-]+/g, '')
      .replace(/[\s_]+/g, '-')
      .replace(/[^\w\s-]+/g, '') || '';
  });
  env.addFilter('title', s => {
    return s
      ?.replace(/[-_\s]+(.)?/g, (_, c) => ' ' + (c?.toUpperCase() || ''))
      .trim()
      .replace(/^\w/, c => c.toUpperCase()) || '';
  });

  // RDF filters
  env.addFilter('localName', uri => uri?.split(/[#/]/).pop() || '');
  env.addFilter('namespace', uri => {
    const i = Math.max(uri?.lastIndexOf('#') ?? -1, uri?.lastIndexOf('/') ?? -1);
    return i >= 0 ? uri.substring(0, i + 1) : '';
  });
  // Merged prefixes: caller-supplied prefixes override defaults
  const mergedPrefixes = { ...DEFAULT_PREFIXES, ...prefixes };
  env.addFilter('expand', (prefixedName, overridePrefixes) => {
    if (!prefixedName?.includes(':')) return prefixedName;
    const [prefix, local] = prefixedName.split(':', 2);
    const effective = overridePrefixes || mergedPrefixes;
    const ns = effective[prefix] || DEFAULT_PREFIXES[prefix];
    return ns ? ns + local : prefixedName;
  });
  env.addFilter('toTurtle', triples => {
    if (!Array.isArray(triples)) return '';
    return triples.map(t => `${t.s} ${t.p} ${t.o} .`).join('\n');
  });

  // Type filters
  env.addFilter(
    'zodType',
    t =>
      ({
        string: 'z.string()',
        integer: 'z.number().int()',
        int: 'z.number().int()',
        float: 'z.number()',
        boolean: 'z.boolean()',
        date: 'z.string().date()',
        anyURI: 'z.string().url()',
      })[(t || 'string').replace(/^xsd:|^http:.*#/, '')] || 'z.string()'
  );
  env.addFilter(
    'jsdocType',
    t =>
      ({ string: 'string', integer: 'number', int: 'number', float: 'number', boolean: 'boolean' })[
        (t || 'string').replace(/^xsd:|^http:.*#/, '')
      ] || 'string'
  );

  // Data filters
  env.addFilter('groupBy', (arr, key) => {
    const g = {};
    for (const i of arr || []) {
      const k = i[key] || i[`?${key}`] || 'undefined';
      (g[k] = g[k] || []).push(i);
    }
    return g;
  });
  env.addFilter('distinctValues', (arr, key) => [
    ...new Set((arr || []).map(i => i[key] || i[`?${key}`]).filter(v => v != null)),
  ]);
  env.addFilter('requiredArgs', arr => {
    if (!arr || !Array.isArray(arr)) return [];
    const seen = new Set();
    const result = [];
    for (const row of arr) {
      const name = row['argName'] ?? row['?argName'];
      const req = row['required'] ?? row['?required'];
      if (!name || name === 'undefined' || seen.has(name)) continue;
      if (req === 'true' || req === true) {
        seen.add(name);
        result.push(name);
      }
    }
    return result;
  });
  env.addFilter('sortBy', (arr, key, dir = 'asc') => {
    const s = [...(arr || [])].sort((a, b) => {
      const av = a[key] || a[`?${key}`] || '',
        bv = b[key] || b[`?${key}`] || '';
      return av < bv ? -1 : av > bv ? 1 : 0;
    });
    return dir === 'desc' ? s.reverse() : s;
  });
  env.addFilter('keys', obj => (obj ? Object.keys(obj) : []));
  env.addFilter('values', obj => (obj ? Object.values(obj) : []));
  env.addFilter('items', obj => (obj ? Object.entries(obj) : []));
  env.addFilter('join', (arr, sep = ',') => Array.isArray(arr) ? arr.join(sep) : '');

  // String filters
  env.addFilter(
    'indent',
    (s, n = 2) =>
      s
        ?.split('\n')
        .map(l => ' '.repeat(n) + l)
        .join('\n') || ''
  );
  env.addFilter(
    'quote',
    (s, c = '"') => `${c}${String(s ?? '').replace(new RegExp(c, 'g'), '\\' + c)}${c}`
  );
  env.addFilter('zodDefault', (val, type) => {
    if (type === 'boolean') return val === 'true' || val === true ? 'true' : 'false';
    if (type === 'number') return isNaN(Number(val)) ? '0' : String(Number(val));
    return `'${String(val ?? '').replace(/'/g, "\\'")}'`;
  });
  env.addFilter('date', (d, f = 'YYYY-MM-DD') => {
    const dt = d instanceof Date ? d : new Date();
    const p = n => String(n).padStart(2, '0');
    return f
      .replace('YYYY', dt.getFullYear())
      .replace('MM', p(dt.getMonth() + 1))
      .replace('DD', p(dt.getDate()))
      .replace('HH', p(dt.getHours()))
      .replace('mm', p(dt.getMinutes()))
      .replace('ss', p(dt.getSeconds()));
  });
  env.addFilter('artifact_extension', type => {
    const extensions = {
      'powl': 'ttl',
      'sparql': 'rq',
      'shacl': 'ttl',
      'mcp-tool': 'mjs',
      'documentation': 'md',
      'python': 'py',
      'typescript': 'ts',
      'elixir': 'ex',
      'go': 'go',
      'rust': 'rs',
      'java': 'java',
      'yaml': 'yaml',
      'json': 'json',
      'ttl': 'ttl',
      'njk': 'njk',
      'rq': 'rq',
      'mjs': 'mjs'
    };
    return extensions[type] || 'txt';
  });
  env.addFilter('default', (val, defaultValue) => val != null ? val : defaultValue);

  // wordwrap filter
  env.addFilter('wordwrap', (s, width = 80) => {
    if (!s) return '';
    const str = String(s);
    const lines = [];
    let currentLine = '';
    for (const word of str.split(/\s+/)) {
      if (!currentLine) currentLine = word;
      else if ((currentLine + ' ' + word).length <= width) currentLine += ' ' + word;
      else { lines.push(currentLine); currentLine = word; }
    }
    if (currentLine) lines.push(currentLine);
    return lines.join('\n');
  });

  // Alias filters for snake_case naming convention
  env.addFilter('camel_case', env.getFilter('camelCase'));
  env.addFilter('pascal_case', env.getFilter('pascalCase'));
  env.addFilter('snake_case', env.getFilter('snakeCase'));
  env.addFilter('kebab_case', env.getFilter('kebabCase'));
  env.addFilter('zod_type', env.getFilter('zodType'));
  env.addFilter('jsdoc_type', env.getFilter('jsdocType'));
  env.addFilter('word_wrap', env.getFilter('wordwrap'));

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
  const enginePrefixes = { ...DEFAULT_PREFIXES, ...prefixes };
  const env = createNunjucksEnvironment(templateDir, enginePrefixes);

  // Re-apply filter aliases (createNunjucksEnvironment registers them, but
  // the expand override above replaces the filter — ensure aliases survive)
  env.addFilter('camel_case',  env.getFilter('camelCase'));
  env.addFilter('pascal_case', env.getFilter('pascalCase'));
  env.addFilter('snake_case',  env.getFilter('snakeCase'));
  env.addFilter('kebab_case',  env.getFilter('kebabCase'));
  env.addFilter('zod_type',    env.getFilter('zodType'));
  env.addFilter('jsdoc_type',  env.getFilter('jsdocType'));
  env.addFilter('local_name',  env.getFilter('localName'));
  env.addFilter('pascal',      env.getFilter('pascalCase'));

  // wordwrap may be overridden by expand; re-register to be safe
  env.addFilter('wordwrap', (s, width = 79) => {
    if (!s) return '';
    const str = String(s);
    const lines = [];
    let currentLine = '';
    for (const word of str.split(/\s+/)) {
      if (!currentLine) currentLine = word;
      else if ((currentLine + ' ' + word).length <= width) currentLine += ' ' + word;
      else { lines.push(currentLine); currentLine = word; }
    }
    if (currentLine) lines.push(currentLine);
    return lines.join('\n');
  });

  // Add global functions
  env.addGlobal('uri', prefixedName => {
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

  env.addGlobal('blankNode', id => `_:${id}`);

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
  const {
    dryRun = false,
    outputDir, baseDir,
    force: optionsForce = false,
    mode: optionsMode,
    outputPath: overridePath,
    context: providedContext = {},
    ...rest
  } = options;
  const context = { ...providedContext, ...rest, outputPath: overridePath };
  const result = await renderTemplate(templatePath, sparqlResults, {
    ...context,
    output_dir: outputDir,
  });

  // Effective mode: frontmatter takes precedence, then options, then default 'overwrite'
  const effectiveMode = result.frontmatter.mode || optionsMode || result.mode || 'overwrite';

  // Merge force from frontmatter (Hygen parity: force: true in template)
  const effectiveForce = optionsForce || Boolean(result.frontmatter.force);

  // If renderTemplate returned skipped, propagate
  if (result.skipped) {
    return {
      ...result,
      finalPath: null,
      status: 'skipped',
      written: false,
      skipped: true,
      dryRun: false,
    };
  }

  // Allow outputPath override from options
  let finalOutputPath = overridePath || result.outputPath;
  if (finalOutputPath && finalOutputPath.includes('{{')) {
    const env = createNunjucksEnvironment(context.templates_dir || baseDir, context.prefixes || {});
    finalOutputPath = env.renderString(finalOutputPath, context);
  }
  
  if (!finalOutputPath) throw new Error(`Template ${templatePath} does not specify output path`);

  // Determine final path: outputDir (CLI/config) > baseDir (project relative) > templateDir
  let finalPath;
  if (isAbsolute(finalOutputPath)) {
    finalPath = finalOutputPath;
  } else if (outputDir) {
    finalPath = resolve(outputDir, finalOutputPath);
  } else if (baseDir && result.frontmatter.to === finalOutputPath) {
    finalPath = resolve(baseDir, finalOutputPath);
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

  const fm = result.frontmatter;

  // skipIf/skip_if: regex — skip if content exists in file (Hygen parity)
  const skipExpr = (fm.skipIf || fm.skip_if)?.trim();
  if (existsSync(finalPath) && !effectiveForce && skipExpr) {
    const regexMatch = skipExpr.match(/^\/(.+)\/([gimsuy]*)$/);
    if (regexMatch) {
      const existing = await readFile(finalPath, "utf-8");
      const regex = new RegExp(regexMatch[1], regexMatch[2]);
      if (regex.test(existing)) {
        return { ...result, finalPath, status: "skipped", written: false, skipped: true, reason: 'skipIf matched' };
      }
    } else {
      // String match
      const existing = await readFile(finalPath, "utf-8");
      if (existing.includes(skipExpr)) {
        return { ...result, finalPath, status: "skipped", written: false, skipped: true, reason: 'skipIf matched' };
      }
    }
  }

  // mode: skip_existing check
  if (existsSync(finalPath) && !effectiveForce && effectiveMode === 'skip_existing') {
    return {
      ...result,
      finalPath,
      status: 'skipped',
      written: false,
      skipped: true,
      dryRun: false,
      reason: 'file exists and mode is skip_existing',
    };
  }

  // unless_exists: true — skip if file already exists (Hygen parity)
  if (existsSync(finalPath) && result.frontmatter.unless_exists && !effectiveForce) {
    return {
      ...result,
      finalPath,
      status: 'skipped',
      written: false,
      skipped: true,
      dryRun: false,
      reason: 'file exists and unless_exists is true',
    };
  }

  await mkdir(dirname(finalPath), { recursive: true });

  // Backup before modification if enabled
  if (options.backup_before_overwrite && existsSync(finalPath)) {
    const backupPath = finalPath + (options.backup_suffix || '.bak');
    // Only create backup if it doesn't exist (don't overwrite backups)
    if (!existsSync(backupPath)) {
      await copyFile(finalPath, backupPath);
    }
  }

  // Determine operation mode from frontmatter
  const opMode = getOperationMode(result.frontmatter);

  // eof_last: control trailing newline on injected content
  let contentToWrite = result.content;
  if (typeof fm.eof_last === 'boolean') {
    if (fm.eof_last === false) {
      contentToWrite = contentToWrite.trimEnd();
    } else if (fm.eof_last === true) {
      contentToWrite = contentToWrite.endsWith('\n') ? contentToWrite : contentToWrite + '\n';
    }
  }

  if (opMode.mode === 'inject' || opMode.mode === 'append' || result.mode === 'append') {
    if (existsSync(finalPath)) {
      const existing = await readFile(finalPath, 'utf-8');
      const separator = existing.endsWith('\n') ? '' : '\n';
      await writeFile(finalPath, existing + separator + contentToWrite, 'utf-8');
    } else {
      await writeFile(finalPath, contentToWrite, 'utf-8');
    }
  } else if (opMode.mode === 'prepend' || result.mode === 'prepend') {
    // Fix: support both opMode.prepend AND frontmatter.mode === 'prepend'
    if (existsSync(finalPath)) {
      const existing = await readFile(finalPath, 'utf-8');
      const separator = contentToWrite.endsWith('\n') ? '' : '\n';
      await writeFile(finalPath, contentToWrite + separator + existing, 'utf-8');
    } else {
      await writeFile(finalPath, contentToWrite, 'utf-8');
    }
  } else if (opMode.mode === 'before') {
    if (existsSync(finalPath)) {
      const existing = await readFile(finalPath, 'utf-8');
      const anchorPattern = opMode.anchor;
      // Hygen parity: support regex for before anchor
      // Detect regex: starts with /, ends with / or /flags, and has content between
      const isRegex =
        anchorPattern.startsWith('/') &&
        /\/[gimsuy]*$/.test(anchorPattern) &&
        anchorPattern.length > 2;
      let idx;
      if (isRegex) {
        const inner = anchorPattern.slice(1);
        const lastSlash = inner.lastIndexOf('/');
        const pattern = lastSlash > 0 ? inner.slice(0, lastSlash) : inner;
        const flags = lastSlash > 0 ? inner.slice(lastSlash + 1) : '';
        const regex = new RegExp(pattern, flags);
        const match = existing.match(regex);
        idx = match && match.index !== -1 ? match.index : -1;
      } else {
        idx = existing.indexOf(anchorPattern);
      }
      if (idx !== -1) {
        const before = existing.substring(0, idx);
        const after = existing.substring(idx);
        const separator = contentToWrite.endsWith('\n') ? '' : '\n';
        await writeFile(finalPath, before + contentToWrite + separator + after, 'utf-8');
      } else {
        const separator = existing.endsWith('\n') ? '' : '\n';
        await writeFile(finalPath, existing + separator + contentToWrite, 'utf-8');
      }
    } else {
      await writeFile(finalPath, contentToWrite, 'utf-8');
    }
  } else if (opMode.mode === 'after') {
    if (existsSync(finalPath)) {
      const existing = await readFile(finalPath, 'utf-8');
      const anchorPattern = opMode.anchor;
      // Hygen parity: support regex for after anchor
      // Detect regex: starts with /, ends with / or /flags, and has content between
      const isRegex =
        anchorPattern.startsWith('/') &&
        /\/[gimsuy]*$/.test(anchorPattern) &&
        anchorPattern.length > 2;
      let insertAt;
      if (isRegex) {
        const inner = anchorPattern.slice(1);
        const lastSlash = inner.lastIndexOf('/');
        const pattern = lastSlash > 0 ? inner.slice(0, lastSlash) : inner;
        const flags = lastSlash > 0 ? inner.slice(lastSlash + 1) : '';
        const regex = new RegExp(pattern, flags);
        const match = existing.match(regex);
        if (match && match.index !== -1) {
          insertAt = match.index + match[0].length;
        } else {
          insertAt = -1;
        }
      } else {
        const idx = existing.indexOf(anchorPattern);
        insertAt = idx !== -1 ? idx + anchorPattern.length : -1;
      }
      if (insertAt !== -1) {
        const before = existing.substring(0, insertAt);
        const after = existing.substring(insertAt);
        const separatorBefore = before.endsWith('\n') ? '' : '\n';
        const separatorAfter = (contentToWrite.endsWith('\n') || after.startsWith('\n')) ? '' : '\n';
        await writeFile(finalPath, before + separatorBefore + contentToWrite + separatorAfter + after, 'utf-8');
      } else {
        const separator = existing.endsWith('\n') ? '' : '\n';
        await writeFile(finalPath, existing + separator + contentToWrite, 'utf-8');
      }
    } else {
      await writeFile(finalPath, contentToWrite, 'utf-8');
    }
  } else if (opMode.mode === 'lineAt') {
    const lineNum = opMode.line;
    if (existsSync(finalPath)) {
      const existing = await readFile(finalPath, 'utf-8');
      const lines = existing.split('\n');
      const clampedLine = Math.max(0, Math.min(lineNum, lines.length));
      lines.splice(clampedLine, 0, contentToWrite.trimEnd());
      await writeFile(finalPath, lines.join('\n'), 'utf-8');
    } else {
      await writeFile(finalPath, contentToWrite, 'utf-8');
    }
  } else {
    // Default: overwrite
    await writeFile(finalPath, contentToWrite, 'utf-8');
  }

  // chmod directive: set file permissions (Hygen parity)
  if (result.frontmatter.chmod) {
    const mode = typeof result.frontmatter.chmod === 'string'
      ? parseInt(result.frontmatter.chmod, 8)
      : result.frontmatter.chmod;
    await chmod(finalPath, mode);
  }

  // sh directive: execute shell command after write (Hygen parity)
  if (result.frontmatter.sh) {
    const { exec } = await import('child_process');
    const { promisify } = await import('util');
    const execAsync = promisify(exec);
    try {
      const env = createNunjucksEnvironment(dirname(resolve(templatePath)));
      const shCmd = env.renderString(result.frontmatter.sh, {
        ...context,
        finalPath,
        content: result.content,
      });
      await execAsync(shCmd);
    } catch (err) {
      if (!result.frontmatter.sh_ignore_exit) {
        throw new Error(`Shell command failed: ${err.message}`);
      }
    }
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
export async function batchRender(templates, sharedContext = {}, _options = {}) {
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
    const preprocessedContent = preprocessFrontmatter(templateContent);
    let frontmatter, templateBody;
    try {
      const parsed = matter(preprocessedContent);
      frontmatter = stripQuotesFromTemplateVars(parsed.data);
      templateBody = parsed.content;
    } catch (error) {
      results.push({
        error: `Failed to parse template frontmatter from ${absPath}:\n${error.message}\n\n` +
                `Tip: If your frontmatter values contain colons, wrap them in quotes.`,
        templatePath: absPath
      });
      continue;
    }
    const templatesDir = mergedContext.templates_dir ? resolve(mergedContext.templates_dir) : null;
    const batchSearchPaths = [dirname(absPath)];
    if (templatesDir && templatesDir !== dirname(absPath)) {
      batchSearchPaths.unshift(templatesDir);
    }
    const env = createNunjucksEnvironment(batchSearchPaths);

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
      const errorDetails =
        lineInfo.line !== null
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
          const preprocessedContent = preprocessFrontmatter(content);
          let frontmatter = {};
          let validationErrors = [];
          try {
            frontmatter = stripQuotesFromTemplateVars(matter(preprocessedContent).data);
            const listParser = new FrontmatterParser({ strict: true });
            const v = listParser.validate(frontmatter);
            if (!v.valid) validationErrors = v.errors;
          } catch (err) {
            frontmatter = { _frontmatterParseError: true };
            validationErrors.push(`YAML parse error: ${err.message}`);
          }

          templates.push({
            name: basename(entry, ext),
            path: fullPath,
            extension: ext,
            hasOutputPath: !!frontmatter.to,
            description: frontmatter.description,
            frontmatter,
            valid: validationErrors.length === 0,
            validationErrors,
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
      suggestions.push(
        `Variable '${varName}' is not available. Available: ${Object.keys(context).join(', ')}`
      );
    } else {
      suggestions.push(`Available context variables: ${Object.keys(context).join(', ')}`);
    }
  }

  if (msg.includes('filter') || msg.includes('unknown filter')) {
    suggestions.push('Check that custom filters are registered with the template engine');
    suggestions.push(
      'Available filters: camelCase, pascalCase, snakeCase, kebabCase, localName, namespace, expand, etc.'
    );
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

export { stripQuotesFromTemplateVars };

export default {
  renderTemplate,
  createNunjucksEnvironment,
  createTemplateEngine,
  renderWithOptions,
  batchRender,
  discoverTemplates,
  TEMPLATE_EXTENSIONS,
  DEFAULT_PREFIXES,
};
