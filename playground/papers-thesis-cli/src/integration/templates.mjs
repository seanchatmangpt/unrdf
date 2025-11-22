/**
 * @fileoverview Nunjucks template engine integration
 *
 * @description
 * Provides template rendering functionality using Nunjucks.
 * Includes custom filters for LaTeX escaping and other transformations.
 * Supports async rendering, template discovery, and context validation.
 *
 * @module integration/templates
 * @version 1.0.0
 * @license MIT
 */

import nunjucks from 'nunjucks';
import { promises as fs } from 'node:fs';
import { join, dirname, resolve, basename } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';

import {
  texescape,
  bibtexkey,
  latexjoin,
  formatdate,
  uppercase,
  lowercase,
  slugify,
  capitalize,
  titlecase,
  wraptext,
  truncate,
  striptags,
  formatnumber,
  ordinal,
  pluralize,
  formatauthor,
  registerAllFilters
} from './nunjucks-filters.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Default templates directory path
 * @type {string}
 */
export const DEFAULT_TEMPLATES_DIR = resolve(join(__dirname, '../../templates'));

/**
 * Available template families
 * @type {string[]}
 */
export const TEMPLATE_FAMILIES = ['imrad', 'dsr', 'argument', 'contribution', 'monograph', 'narrative'];

/**
 * Template file extension
 * @type {string}
 */
export const TEMPLATE_EXTENSION = '.tex.njk';

/**
 * Author schema for context validation
 */
export const AuthorSchema = z.object({
  name: z.string().min(1, 'Author name is required'),
  affiliation: z.string().optional(),
  role: z.string().optional(),
  email: z.string().email().optional(),
  orcid: z.string().optional()
});

/**
 * Section schema for context validation
 */
export const SectionSchema = z.object({
  heading: z.string().min(1, 'Section heading is required'),
  content: z.string(),
  level: z.number().int().min(1).max(4).optional(),
  subsections: z.array(z.lazy(() => SectionSchema)).optional()
});

/**
 * Bibliography entry schema
 */
export const BibliographyEntrySchema = z.object({
  key: z.string().min(1),
  title: z.string().min(1),
  authors: z.union([z.string(), z.array(z.string())]),
  year: z.union([z.number(), z.string()]),
  source: z.string().optional(),
  doi: z.string().optional(),
  url: z.string().url().optional(),
  type: z.enum(['article', 'book', 'inproceedings', 'misc', 'thesis']).optional()
});

/**
 * Figure schema
 */
export const FigureSchema = z.object({
  caption: z.string().min(1),
  path: z.string().min(1),
  width: z.string().optional(),
  label: z.string().optional()
});

/**
 * Table schema
 */
export const TableSchema = z.object({
  caption: z.string().min(1),
  data: z.array(z.array(z.string())),
  headers: z.array(z.string()).optional(),
  label: z.string().optional()
});

/**
 * Complete template context schema
 */
export const TemplateContextSchema = z.object({
  // Metadata
  title: z.string().min(1, 'Title is required'),
  abstract: z.string().optional(),
  authors: z.array(AuthorSchema).min(1, 'At least one author is required'),
  date: z.union([z.string(), z.date()]).optional(),
  keywords: z.array(z.string()).optional(),

  // Structure
  sections: z.array(SectionSchema).optional(),

  // Bibliography and figures
  bibliography: z.array(BibliographyEntrySchema).optional(),
  figures: z.array(FigureSchema).optional(),
  tables: z.array(TableSchema).optional(),

  // Document settings
  toc: z.boolean().optional(),
  pageNumbers: z.boolean().optional(),
  twocolumn: z.boolean().optional(),
  documentClass: z.string().optional(),
  fontSize: z.enum(['10pt', '11pt', '12pt']).optional(),
  paperSize: z.enum(['a4paper', 'letterpaper']).optional()
});

/**
 * @typedef {z.infer<typeof TemplateContextSchema>} TemplateContext
 */

/**
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether context is valid
 * @property {string[]} [errors] - Validation errors if invalid
 */

/**
 * @typedef {Object} TemplateInfo
 * @property {string} name - Template name (without extension)
 * @property {string} path - Full path to template file
 * @property {string} family - Template family (imrad, dsr, etc.)
 * @property {boolean} exists - Whether template file exists
 */

/**
 * Nunjucks environment instance
 * @type {nunjucks.Environment | null}
 */
let envInstance = null;

/**
 * Initialize Nunjucks environment with custom configuration
 *
 * @param {Object} [options] - Configuration options
 * @param {string} [options.templatesDir] - Path to templates directory
 * @param {boolean} [options.autoescape=false] - Enable auto-escaping (disabled for LaTeX)
 * @param {boolean} [options.watch=false] - Watch templates for changes
 * @returns {nunjucks.Environment} Configured Nunjucks environment
 *
 * @example
 * const env = initTemplateEngine();
 * const env = initTemplateEngine({ templatesDir: './custom-templates' });
 */
export function initTemplateEngine(options = {}) {
  const { templatesDir = DEFAULT_TEMPLATES_DIR, autoescape = false, watch = false } = options;

  // Configure Nunjucks environment
  const env = nunjucks.configure(templatesDir, {
    autoescape, // Disabled for LaTeX output
    trimBlocks: true,
    lstripBlocks: true,
    throwOnUndefined: false,
    watch
  });

  // Register all custom filters
  registerAllFilters(env);

  // Add convenience globals
  env.addGlobal('currentYear', new Date().getFullYear());
  env.addGlobal('currentDate', new Date().toISOString().split('T')[0]);

  // Store instance for reuse
  envInstance = env;

  return env;
}

/**
 * Get the current Nunjucks environment, initializing if needed
 *
 * @returns {nunjucks.Environment} Nunjucks environment
 */
export function getTemplateEngine() {
  if (!envInstance) {
    return initTemplateEngine();
  }
  return envInstance;
}

/**
 * Render a template by name
 *
 * @param {string} templateName - Template name (e.g., 'imrad' or 'imrad.tex.njk')
 * @param {Object} context - Variables for template
 * @returns {Promise<string>} Rendered LaTeX content
 * @throws {Error} If template not found or render fails
 *
 * @example
 * const latex = await renderTemplate('imrad', { paper: { title: 'My Paper', ... } });
 */
export async function renderTemplate(templateName, context) {
  const env = getTemplateEngine();

  // Normalize template name
  const templateFile = templateName.endsWith(TEMPLATE_EXTENSION)
    ? templateName
    : `${templateName}${TEMPLATE_EXTENSION}`;

  return new Promise((resolve, reject) => {
    env.render(templateFile, context, (err, result) => {
      if (err) {
        // Provide helpful error message
        if (err.message.includes('template not found')) {
          const availableTemplates = TEMPLATE_FAMILIES.join(', ');
          reject(
            new Error(
              `Template '${templateName}' not found. Available templates: ${availableTemplates}`
            )
          );
        } else {
          // Extract line number if available
          const lineMatch = err.message.match(/Line (\d+)/);
          const lineInfo = lineMatch ? ` (at line ${lineMatch[1]})` : '';
          reject(new Error(`Template render error${lineInfo}: ${err.message}`));
        }
      } else {
        resolve(result);
      }
    });
  });
}

/**
 * Render a template string directly (without file)
 *
 * @param {string} templateStr - Template string
 * @param {Object} context - Template variables
 * @returns {Promise<string>} Rendered content
 *
 * @example
 * const result = await renderTemplateString('Hello {{ name | uppercase }}', { name: 'world' });
 */
export async function renderTemplateString(templateStr, context) {
  const env = getTemplateEngine();

  return new Promise((resolve, reject) => {
    env.renderString(templateStr, context, (err, result) => {
      if (err) {
        reject(new Error(`Template string render error: ${err.message}`));
      } else {
        resolve(result);
      }
    });
  });
}

/**
 * Get list of available templates
 *
 * @param {string} [templatesDir] - Templates directory (defaults to DEFAULT_TEMPLATES_DIR)
 * @returns {Promise<TemplateInfo[]>} Array of available template info
 *
 * @example
 * const templates = await getAvailableTemplates();
 * console.log(templates.map(t => t.name));
 */
export async function getAvailableTemplates(templatesDir = DEFAULT_TEMPLATES_DIR) {
  const resolvedDir = resolve(templatesDir);
  const templates = [];

  try {
    const entries = await fs.readdir(resolvedDir, { withFileTypes: true });

    for (const entry of entries) {
      if (entry.isFile() && entry.name.endsWith(TEMPLATE_EXTENSION)) {
        const name = entry.name.replace(TEMPLATE_EXTENSION, '');
        templates.push({
          name,
          path: join(resolvedDir, entry.name),
          family: name,
          exists: true
        });
      }
    }

    // Add standard families that may not have files yet
    for (const family of TEMPLATE_FAMILIES) {
      if (!templates.find((t) => t.name === family)) {
        templates.push({
          name: family,
          path: join(resolvedDir, `${family}${TEMPLATE_EXTENSION}`),
          family,
          exists: false
        });
      }
    }

    return templates.sort((a, b) => a.name.localeCompare(b.name));
  } catch (error) {
    if (error.code === 'ENOENT') {
      // Directory doesn't exist, return empty list
      return TEMPLATE_FAMILIES.map((family) => ({
        name: family,
        path: join(resolvedDir, `${family}${TEMPLATE_EXTENSION}`),
        family,
        exists: false
      }));
    }
    throw new Error(`Failed to list templates: ${error.message}`, { cause: error });
  }
}

/**
 * Check if a template exists
 *
 * @param {string} templateName - Template name to check
 * @param {string} [templatesDir] - Templates directory
 * @returns {Promise<boolean>} True if template exists
 *
 * @example
 * if (await templateExists('imrad')) {
 *   console.log('IMRAD template available');
 * }
 */
export async function templateExists(templateName, templatesDir = DEFAULT_TEMPLATES_DIR) {
  const templateFile = templateName.endsWith(TEMPLATE_EXTENSION)
    ? templateName
    : `${templateName}${TEMPLATE_EXTENSION}`;

  const templatePath = join(resolve(templatesDir), templateFile);

  try {
    await fs.access(templatePath);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate template context against schema
 *
 * @param {Object} context - Context to validate
 * @returns {Promise<ValidationResult>} Validation result
 *
 * @example
 * const result = await validateContext({ title: 'My Paper', authors: [] });
 * if (!result.valid) {
 *   console.error('Validation errors:', result.errors);
 * }
 */
export async function validateContext(context) {
  try {
    TemplateContextSchema.parse(context);
    return { valid: true };
  } catch (error) {
    if (error instanceof z.ZodError) {
      const errors = error.errors.map((e) => {
        const path = e.path.join('.');
        return path ? `${path}: ${e.message}` : e.message;
      });
      return { valid: false, errors };
    }
    return { valid: false, errors: [error.message] };
  }
}

/**
 * Validate partial context (doesn't require all fields)
 *
 * @param {Object} context - Partial context to validate
 * @returns {Promise<ValidationResult>} Validation result
 */
export async function validatePartialContext(context) {
  try {
    TemplateContextSchema.partial().parse(context);
    return { valid: true };
  } catch (error) {
    if (error instanceof z.ZodError) {
      const errors = error.errors.map((e) => {
        const path = e.path.join('.');
        return path ? `${path}: ${e.message}` : e.message;
      });
      return { valid: false, errors };
    }
    return { valid: false, errors: [error.message] };
  }
}

/**
 * Create a template context factory for a specific template family
 *
 * @param {string} family - Template family name
 * @returns {Function} Context factory function
 *
 * @example
 * const createIMRADContext = createContextFactory('imrad');
 * const context = createIMRADContext({
 *   title: 'My Paper',
 *   authors: [{ name: 'John Smith' }]
 * });
 */
export function createContextFactory(family) {
  return function createContext(overrides = {}) {
    const defaults = {
      date: new Date(),
      toc: false,
      pageNumbers: true,
      twocolumn: false,
      documentClass: 'article',
      fontSize: '11pt',
      paperSize: 'a4paper'
    };

    // Family-specific defaults
    const familyDefaults = {
      imrad: { sections: [] },
      dsr: { sections: [] },
      monograph: { toc: true, documentClass: 'report' },
      narrative: { sections: [] },
      argument: { sections: [] },
      contribution: { sections: [] }
    };

    return {
      ...defaults,
      ...(familyDefaults[family] || {}),
      ...overrides
    };
  };
}

/**
 * Get template metadata
 *
 * @param {string} templateName - Template name
 * @param {string} [templatesDir] - Templates directory
 * @returns {Promise<Object>} Template metadata
 */
export async function getTemplateMetadata(templateName, templatesDir = DEFAULT_TEMPLATES_DIR) {
  const templateFile = templateName.endsWith(TEMPLATE_EXTENSION)
    ? templateName
    : `${templateName}${TEMPLATE_EXTENSION}`;

  const templatePath = join(resolve(templatesDir), templateFile);

  try {
    const content = await fs.readFile(templatePath, 'utf-8');
    const stats = await fs.stat(templatePath);

    // Extract metadata from template comments
    const descriptionMatch = content.match(/\{#\s*(.+?)\s*#\}/);
    const variablesMatch = content.match(/\{#\s*Variables:\s*(.+?)\s*#\}/);

    return {
      name: templateName,
      path: templatePath,
      size: stats.size,
      modified: stats.mtime,
      description: descriptionMatch ? descriptionMatch[1].trim() : null,
      variables: variablesMatch ? variablesMatch[1].trim() : null,
      exists: true
    };
  } catch (error) {
    if (error.code === 'ENOENT') {
      return {
        name: templateName,
        path: templatePath,
        exists: false
      };
    }
    throw error;
  }
}

// Re-export filters for direct use
export {
  texescape,
  bibtexkey,
  latexjoin,
  formatdate,
  uppercase,
  lowercase,
  slugify,
  capitalize,
  titlecase,
  wraptext,
  truncate,
  striptags,
  formatnumber,
  ordinal,
  pluralize,
  formatauthor
};

// Also export the legacy names for backward compatibility
export { texescape as texEscape };
export { bibtexkey as toBibtexKey };
export { wraptext as wrapText };

/**
 * Create a configured template engine (factory function)
 *
 * @param {string} [templatesDir] - Path to templates directory
 * @returns {Object} Template engine interface
 *
 * @example
 * const engine = createTemplateEngine('./my-templates');
 * const latex = await engine.render('imrad', context);
 */
export function createTemplateEngine(templatesDir = DEFAULT_TEMPLATES_DIR) {
  const env = initTemplateEngine({ templatesDir });

  return {
    /**
     * Render a template by name
     * @param {string} templateName - Template name
     * @param {Object} context - Template variables
     * @returns {Promise<string>} Rendered content
     */
    async render(templateName, context) {
      return renderTemplate(templateName, context);
    },

    /**
     * Render a template string
     * @param {string} templateStr - Template string
     * @param {Object} context - Template variables
     * @returns {Promise<string>} Rendered content
     */
    async renderString(templateStr, context) {
      return renderTemplateString(templateStr, context);
    },

    /**
     * Get list of available templates
     * @returns {Promise<string[]>} Template names
     */
    async getTemplates() {
      const templates = await getAvailableTemplates(templatesDir);
      return templates.filter((t) => t.exists).map((t) => t.name);
    },

    /**
     * Validate context against schema
     * @param {Object} context - Context to validate
     * @returns {Promise<ValidationResult>} Validation result
     */
    async validateContext(context) {
      return validateContext(context);
    },

    /**
     * Get the raw Nunjucks environment
     * @returns {nunjucks.Environment}
     */
    getEnvironment() {
      return env;
    },

    /**
     * Add a custom filter
     * @param {string} name - Filter name
     * @param {Function} fn - Filter function
     */
    addFilter(name, fn) {
      env.addFilter(name, fn);
    },

    /**
     * Add a global variable
     * @param {string} name - Variable name
     * @param {*} value - Variable value
     */
    addGlobal(name, value) {
      env.addGlobal(name, value);
    }
  };
}

/**
 * Default template engine instance
 */
export const templateEngine = createTemplateEngine();

/**
 * Render a paper to LaTeX
 *
 * @param {Object} paper - Paper object with title, authors, sections, etc.
 * @returns {Promise<string>} LaTeX content
 *
 * @example
 * const latex = await renderPaper({
 *   title: 'My Research Paper',
 *   family: 'imrad',
 *   authors: [{ name: 'John Smith' }],
 *   abstract: 'This paper presents...',
 *   sections: [{ heading: 'Introduction', content: '...' }]
 * });
 */
export async function renderPaper(paper) {
  const templateName = paper.family || 'imrad';
  return renderTemplate(templateName, { paper });
}

/**
 * Render a thesis to LaTeX
 *
 * @param {Object} thesis - Thesis object
 * @returns {Promise<string>} LaTeX content
 *
 * @example
 * const latex = await renderThesis({
 *   title: 'My PhD Thesis',
 *   type: 'monograph',
 *   authors: [{ name: 'Jane Doe' }],
 *   chapters: [...]
 * });
 */
export async function renderThesis(thesis) {
  const templateName = thesis.type || 'monograph';
  return renderTemplate(templateName, { thesis });
}

/**
 * Preview a template with sample data
 *
 * @param {string} templateName - Template name
 * @returns {Promise<string>} Rendered preview
 */
export async function previewTemplate(templateName) {
  const sampleContext = {
    paper: {
      title: 'Sample Paper Title',
      abstract: 'This is a sample abstract demonstrating the template structure.',
      authors: [
        { name: 'John Smith', affiliation: 'University of Example' },
        { name: 'Jane Doe', affiliation: 'Research Institute' }
      ],
      keywords: ['sample', 'template', 'preview'],
      sections: [
        { heading: 'Introduction', content: 'Sample introduction content...' },
        { heading: 'Methods', content: 'Sample methods content...' },
        { heading: 'Results', content: 'Sample results content...' },
        { heading: 'Discussion', content: 'Sample discussion content...' },
        { heading: 'Conclusion', content: 'Sample conclusion content...' }
      ]
    }
  };

  return renderTemplate(templateName, sampleContext);
}
