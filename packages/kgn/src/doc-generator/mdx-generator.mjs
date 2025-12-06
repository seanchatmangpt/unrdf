/**
 * @fileoverview MDX Generator using @unrdf/kgn templates
 * Converts parsed JSDoc + RDF data to MDX files for Nextra
 */

import nunjucks from 'nunjucks';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';
import { readFileSync } from 'fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Generate MDX from parsed module data
 * @param {Object} parsedModule - Output from parser.parseFile()
 * @param {string} templatePath - Path to Nunjucks template
 * @returns {string} Generated MDX content
 */
export function generateModuleMDX(parsedModule, templatePath = null) {
  // Set up Nunjucks environment
  const templatesDir = join(__dirname, '../../templates');
  const env = new nunjucks.Environment(
    new nunjucks.FileSystemLoader(templatesDir),
    { autoescape: false, trimBlocks: true, lstripBlocks: true }
  );

  // Add custom filters
  env.addFilter('map', (arr, attr) => arr.map(item => item[attr]));
  env.addFilter('join', (arr, sep) => arr.join(sep));

  // Default template
  if (!templatePath) {
    templatePath = 'api/module.njk';
  }

  // Prepare data for template
  const templateData = {
    module: {
      name: parsedModule.relativePath.split('/').pop().replace(/\.(m?js|ts)$/, ''),
      relativePath: parsedModule.relativePath,
      file: parsedModule.file,
      exports: parsedModule.exports,
      imports: parsedModule.imports,
      commentCount: parsedModule.comments,
    }
  };

  // Render template
  return env.render(templatePath, templateData);
}

/**
 * Generate MDX for multiple modules
 * @param {Array} parsedModules - Array of parsed module data
 * @returns {Map} Map of file paths to MDX content
 */
export function generateModulesMDX(parsedModules) {
  const mdxMap = new Map();

  parsedModules.forEach(module => {
    if (module.error) return;

    const mdx = generateModuleMDX(module);
    mdxMap.set(module.relativePath, mdx);
  });

  return mdxMap;
}

export default { generateModuleMDX, generateModulesMDX };
