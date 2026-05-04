/**
 * Template Utilities - High-level convenience functions
 * Migrated from ~/unjucks with enhanced error handling and validation
 */

import { TemplateEngine } from '../engine/template-engine.js';
import { TemplateLinter } from '../linter/determinism.js';
import { VariableExtractor } from '../parser/variables.js';
import { FrontmatterParser } from '../parser/frontmatter.js';

/**
 * Quick template rendering with default options
 */
export async function renderTemplate(templatePath, context = {}, options = {}) {
  const engine = new TemplateEngine({
    templatesDir: options.templatesDir,
    deterministicMode: options.deterministicMode !== false,
    strictMode: options.strictMode !== false,
    ...options.engineOptions
  });

  return await engine.render(templatePath, context, options);
}

/**
 * Render template from string content
 */
export function renderString(templateString, context = {}, options = {}) {
  const engine = new TemplateEngine({
    deterministicMode: options.deterministicMode !== false,
    strictMode: options.strictMode !== false,
    ...options.engineOptions
  });

  return engine.renderString(templateString, context, options);
}

/**
 * Validate template for determinism and common issues
 */
export async function validateTemplate(templatePath, options = {}) {
  try {
    // Read template content
    const fs = await import('fs/promises');
    const path = await import('path');
    
    const fullPath = options.templatesDir 
      ? path.resolve(options.templatesDir, templatePath)
      : path.resolve(templatePath);
    
    const content = await fs.readFile(fullPath, 'utf8');
    
    // Parse frontmatter
    const parser = new FrontmatterParser();
    const { frontmatter } = parser.parse(content);
    
    // Lint template
    const linter = new TemplateLinter({
      strict: options.strict !== false,
      customRules: options.customRules
    });
    
    const lintResult = linter.lint(content, frontmatter);
    
    return {
      success: true,
      templatePath: fullPath,
      ...lintResult
    };
    
  } catch (error) {
    return {
      success: false,
      error: error.message,
      templatePath
    };
  }
}

/**
 * Extract variables from template
 */
export async function extractVariables(templatePath, options = {}) {
  try {
    // Read template content
    const fs = await import('fs/promises');
    const path = await import('path');
    
    const fullPath = options.templatesDir 
      ? path.resolve(options.templatesDir, templatePath)
      : path.resolve(templatePath);
    
    const content = await fs.readFile(fullPath, 'utf8');
    
    // Parse frontmatter to get content without frontmatter
    const parser = new FrontmatterParser();
    const { frontmatter, content: templateContent } = parser.parse(content);
    
    // Extract variables
    const extractor = new VariableExtractor({
      includeFilters: options.includeFilters !== false,
      includeFunctions: options.includeFunctions !== false
    });
    
    const extracted = extractor.extract(templateContent);
    
    return {
      success: true,
      templatePath: fullPath,
      frontmatter,
      ...extracted
    };
    
  } catch (error) {
    return {
      success: false,
      error: error.message,
      templatePath
    };
  }
}

/**
 * Lint template for determinism issues
 */
export async function lintTemplate(templatePath, options = {}) {
  try {
    // Read template content
    const fs = await import('fs/promises');
    const path = await import('path');
    
    const fullPath = options.templatesDir 
      ? path.resolve(options.templatesDir, templatePath)
      : path.resolve(templatePath);
    
    const content = await fs.readFile(fullPath, 'utf8');
    
    // Parse frontmatter
    const parser = new FrontmatterParser();
    const { frontmatter } = parser.parse(content);
    
    // Lint template
    const linter = new TemplateLinter({
      strict: options.strict !== false,
      warnOnly: options.warnOnly === true,
      customRules: options.customRules || []
    });
    
    const result = linter.lint(content, frontmatter);
    
    // Auto-fix if requested
    if (options.autoFix) {
      const fixResult = linter.autoFix(content);
      result.autoFix = fixResult;
    }
    
    return {
      success: true,
      templatePath: fullPath,
      ...result
    };
    
  } catch (error) {
    return {
      success: false,
      error: error.message,
      templatePath
    };
  }
}

/**
 * Analyze template comprehensively
 */
export async function analyzeTemplate(templatePath, options = {}) {
  const results = await Promise.all([
    extractVariables(templatePath, options),
    validateTemplate(templatePath, options),
    lintTemplate(templatePath, options)
  ]);

  const [variables, validation, linting] = results;

  if (!variables.success || !validation.success || !linting.success) {
    const errors = [
      variables.error,
      validation.error,
      linting.error
    ].filter(Boolean);
    
    return {
      success: false,
      error: `Analysis failed: ${errors.join(', ')}`,
      templatePath
    };
  }

  return {
    success: true,
    templatePath: variables.templatePath,
    analysis: {
      variables: variables.variables,
      filters: variables.filters,
      functions: variables.functions,
      complexity: variables.complexity,
      frontmatter: variables.frontmatter,
      deterministic: validation.deterministic,
      score: validation.score,
      issues: validation.issues,
      warnings: validation.warnings,
      suggestions: validation.suggestions,
      linting: {
        passesLint: linting.summary.passesLint,
        totalIssues: linting.summary.totalIssues,
        criticalIssues: linting.summary.criticalIssues
      }
    },
    summary: {
      variableCount: variables.variables.length,
      filterCount: variables.filters.length,
      determinismScore: validation.score,
      isDeterministic: validation.deterministic,
      hasIssues: validation.issues.length > 0,
      complexity: variables.complexity
    }
  };
}

/**
 * Test template with sample data
 */
export async function testTemplate(templatePath, testData = {}, options = {}) {
  try {
    // Extract required variables
    const variableResult = await extractVariables(templatePath, options);
    if (!variableResult.success) {
      return variableResult;
    }

    // Check if test data provides required variables
    const missingVars = variableResult.variables.filter(
      varName => !(varName in testData) && !varName.startsWith('_')
    );

    if (missingVars.length > 0 && options.strict !== false) {
      return {
        success: false,
        error: `Missing test data for variables: ${missingVars.join(', ')}`,
        missingVariables: missingVars,
        templatePath
      };
    }

    // Render template with test data
    const renderResult = await renderTemplate(templatePath, testData, {
      ...options,
      validateVars: options.strict !== false
    });

    if (!renderResult.success) {
      return renderResult;
    }

    // Test determinism if requested
    let determinismTest = null;
    if (options.testDeterminism !== false) {
      const engine = new TemplateEngine({
        deterministicMode: true,
        ...options.engineOptions
      });

      const verification = await engine.deterministicRenderer.verifyDeterminism(
        engine.env, 
        renderResult.content, 
        testData, 
        options.determinismIterations || 3
      );

      determinismTest = verification;
    }

    return {
      success: true,
      templatePath: renderResult.templatePath,
      renderResult,
      determinismTest,
      testData,
      missingVariables: missingVars
    };

  } catch (error) {
    return {
      success: false,
      error: error.message,
      templatePath
    };
  }
}

/**
 * Discover templates in directory
 */
export async function discoverTemplates(directory, options = {}) {
  try {
    const fs = await import('fs/promises');
    const path = await import('path');
    
    const templates = [];
    const extensions = options.extensions || ['.njk', '.j2', '.html'];
    
    async function scanDirectory(dir, relativePath = '') {
      const entries = await fs.readdir(dir, { withFileTypes: true });
      
      for (const entry of entries) {
        if (entry.isDirectory() && !entry.name.startsWith('.')) {
          if (options.recursive !== false) {
            await scanDirectory(
              path.join(dir, entry.name),
              path.join(relativePath, entry.name)
            );
          }
        } else if (entry.isFile()) {
          const ext = path.extname(entry.name);
          if (extensions.includes(ext)) {
            const templatePath = path.join(relativePath, entry.name);
            const fullPath = path.join(dir, entry.name);
            
            const template = {
              name: path.basename(entry.name, ext),
              path: templatePath,
              fullPath,
              extension: ext,
              directory: relativePath,
              size: (await fs.stat(fullPath)).size,
              modified: (await fs.stat(fullPath)).mtime.toISOString()
            };

            // Add analysis if requested
            if (options.analyze) {
              try {
                const analysis = await analyzeTemplate(fullPath, options);
                template.analysis = analysis.success ? analysis.summary : null;
                template.analysisError = analysis.success ? null : analysis.error;
              } catch (error) {
                template.analysisError = error.message;
              }
            }

            templates.push(template);
          }
        }
      }
    }

    await scanDirectory(directory);
    
    return {
      success: true,
      directory,
      templates: templates.sort((a, b) => a.path.localeCompare(b.path)),
      count: templates.length
    };

  } catch (error) {
    return {
      success: false,
      error: error.message,
      directory
    };
  }
}

/**
 * Batch process templates
 */
export async function batchProcess(templates, operation, options = {}) {
  const results = [];
  const concurrency = options.concurrency || 5;
  
  // Process in batches for better performance
  for (let i = 0; i < templates.length; i += concurrency) {
    const batch = templates.slice(i, i + concurrency);
    const batchPromises = batch.map(async (template) => {
      try {
        const result = await operation(template, options);
        return {
          template,
          result,
          success: true
        };
      } catch (error) {
        return {
          template,
          error: error.message,
          success: false
        };
      }
    });

    const batchResults = await Promise.all(batchPromises);
    results.push(...batchResults);
  }

  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);

  return {
    total: results.length,
    successful: successful.length,
    failed: failed.length,
    results,
    errors: failed.map(f => ({ template: f.template, error: f.error }))
  };
}

export default {
  renderTemplate,
  renderString,
  validateTemplate,
  extractVariables,
  lintTemplate,
  analyzeTemplate,
  testTemplate,
  discoverTemplates,
  batchProcess
};