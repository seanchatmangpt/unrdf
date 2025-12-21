/**
 * KGEN Filter Templates - Deterministic filter definitions
 * 
 * Provides templates for creating custom filters that maintain deterministic behavior
 * All filter templates are designed to be composable and reusable
 */

import crypto from 'crypto';

/**
 *
 */
export class KGenFilterTemplates {
  /**
   *
   */
  constructor(options = {}) {
    this.options = {
      deterministicMode: options.deterministicMode !== false,
      staticBuildTime: options.staticBuildTime || '2024-01-01T00:00:00.000Z',
      namespace: options.namespace || 'kgen',
      ...options
    };
    
    this.filterTemplates = new Map();
    this.initializeFilterTemplates();
  }

  /**
   * Initialize built-in filter templates
   */
  initializeFilterTemplates() {
    // Basic filter template
    this.registerFilterTemplate('basic_filter', {
      name: '{{ filterName }}',
      description: '{{ filterDescription | default("Custom filter") }}',
      code: `(value, ...args) => {
  // Input validation
  if (value === null || value === undefined) {
    return {{ defaultValue | default('value') }};
  }
  
  // Filter logic
  {{ filterLogic | default('return value;') }}
}`,
      tests: [
        {
          input: '{{ testInput | default("test") }}',
          expected: '{{ testOutput | default("test") }}',
          description: 'Basic test case'
        }
      ]
    });

    // Text processing filter template
    this.registerFilterTemplate('text_filter', {
      name: '{{ filterName }}',
      description: 'Text processing filter: {{ filterDescription }}',
      code: `(text, options = {}) => {
  const str = String(text || '');
  
  if (str === '') {
    return {{ emptyValue | default('str') }};
  }
  
  // Text processing options
  const {
    {{ filterOptions | join(',\n    ') | default('trim = true') }}
  } = options;
  
  let result = str;
  
  {{ textProcessingSteps | default('// Processing steps here') }}
  
  return result;
}`,
      tests: [
        {
          input: '"  Hello World  "',
          expected: '"Hello World"',
          description: 'Text trimming'
        }
      ]
    });

    // Array processing filter template
    this.registerFilterTemplate('array_filter', {
      name: '{{ filterName }}',
      description: 'Array processing filter: {{ filterDescription }}',
      code: `(array, {{ filterParams | default('') }}) => {
  if (!Array.isArray(array)) {
    return {{ nonArrayDefault | default('[]') }};
  }
  
  if (array.length === 0) {
    return {{ emptyArrayDefault | default('array') }};
  }
  
  // Array processing
  const result = array{{ arrayMethod | default('.map(item => item)') }};
  
  {{ additionalProcessing | default('// Additional processing') }}
  
  return result;
}`,
      tests: [
        {
          input: '[1, 2, 3, 4, 5]',
          expected: '[1, 2, 3, 4, 5]',
          description: 'Array processing test'
        }
      ]
    });

    // Object processing filter template
    this.registerFilterTemplate('object_filter', {
      name: '{{ filterName }}',
      description: 'Object processing filter: {{ filterDescription }}',
      code: `(obj, {{ filterParams | default('') }}) => {
  if (typeof obj !== 'object' || obj === null) {
    return {{ nonObjectDefault | default('{}') }};
  }
  
  const result = {{ objectDefault | default('{}') }};
  
  {{ objectProcessing | default('// Object processing logic') }}
  
  return result;
}`,
      tests: [
        {
          input: '{ "name": "test" }',
          expected: '{ "name": "test" }',
          description: 'Object processing test'
        }
      ]
    });

    // Deterministic filter template
    this.registerFilterTemplate('deterministic_filter', {
      name: '{{ filterName }}',
      description: 'Deterministic filter: {{ filterDescription }}',
      code: `(value, options = {}) => {
  const { deterministicMode = ${this.options.deterministicMode} } = options;
  
  // Deterministic behavior check
  if (deterministicMode) {
    {{ deterministicLogic | default('// Deterministic implementation') }}
  } else {
    {{ nonDeterministicLogic | default('// Non-deterministic implementation') }}
  }
  
  return value;
}`,
      tests: [
        {
          input: '"test"',
          options: '{ deterministicMode: true }',
          expected: '"test"',
          description: 'Deterministic mode test'
        }
      ]
    });

    // Hash filter template
    this.registerFilterTemplate('hash_filter', {
      name: '{{ filterName }}',
      description: 'Hash-based filter: {{ filterDescription }}',
      code: `(content, algorithm = 'sha256', length = null) => {
  const hash = crypto.createHash(algorithm)
    .update(String(content || ''), 'utf8')
    .digest('hex');
  
  return length ? hash.substring(0, length) : hash;
}`,
      tests: [
        {
          input: '"hello"',
          expected: 'hash string',
          description: 'Hash generation test'
        }
      ]
    });

    // Validation filter template
    this.registerFilterTemplate('validation_filter', {
      name: '{{ filterName }}',
      description: 'Validation filter: {{ filterDescription }}',
      code: `(value, rules = {}) => {
  const errors = [];
  
  {{ validationRules | default('// Validation rules') }}
  
  if (errors.length > 0) {
    {{ onValidationError | default('throw new Error(\'Validation failed: \' + errors.join(\', \'));') }}
  }
  
  return value;
}`,
      tests: [
        {
          input: '"valid_value"',
          expected: '"valid_value"',
          description: 'Valid input test'
        }
      ]
    });

    // Format filter template
    this.registerFilterTemplate('format_filter', {
      name: '{{ filterName }}',
      description: 'Format filter: {{ filterDescription }}',
      code: `(value, format = '{{ defaultFormat | default("default") }}') => {
  if (value === null || value === undefined) {
    return '';
  }
  
  switch (format) {
    {{ formatCases | default('case "default": return String(value);') }}
    default:
      return String(value);
  }
}`,
      tests: [
        {
          input: '"test"',
          format: '"default"',
          expected: '"test"',
          description: 'Default format test'
        }
      ]
    });

    // Composition filter template
    this.registerFilterTemplate('composition_filter', {
      name: '{{ filterName }}',
      description: 'Composition filter: {{ filterDescription }}',
      code: `(value, filters = [], context = {}) => {
  let result = value;
  
  for (const filter of filters) {
    if (typeof filter === 'function') {
      result = filter(result, context);
    } else if (typeof filter === 'string' && context.filters && context.filters[filter]) {
      result = context.filters[filter](result);
    }
  }
  
  return result;
}`,
      tests: [
        {
          input: '"test"',
          filters: '[]',
          expected: '"test"',
          description: 'Empty composition test'
        }
      ]
    });
  }

  /**
   * Register a filter template
   */
  registerFilterTemplate(name, template) {
    this.filterTemplates.set(name, {
      ...template,
      registered: this.options.staticBuildTime,
      namespace: this.options.namespace
    });
  }

  /**
   * Generate filter code from template
   */
  generateFilter(templateName, context = {}) {
    const template = this.filterTemplates.get(templateName);
    if (!template) {
      throw new Error(`Filter template '${templateName}' not found`);
    }
    
    // Simple template substitution
    let code = template.code;
    
    // Replace template variables
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`{{\\s*${key}\\s*(?:\\|[^}]*)?\\s*}}`, 'g');
      
      // Handle default values
      const matches = code.match(regex);
      if (matches) {
        for (const match of matches) {
          const defaultMatch = match.match(/\|\s*default\('([^']*)'\)/);
          const replacement = value !== undefined ? value : (defaultMatch ? defaultMatch[1] : match);
          code = code.replace(match, replacement);
        }
      } else {
        // Simple replacement without defaults
        const simpleRegex = new RegExp(`{{\\s*${key}\\s*}}`, 'g');
        code = code.replace(simpleRegex, String(value || ''));
      }
    }
    
    return {
      name: this.interpolate(template.name, context),
      description: this.interpolate(template.description, context),
      code,
      tests: template.tests?.map(test => ({
        ...test,
        input: this.interpolate(test.input, context),
        expected: this.interpolate(test.expected, context)
      })) || []
    };
  }

  /**
   * Generate complete filter module
   */
  generateFilterModule(filters = [], moduleName = 'CustomFilters') {
    const generatedFilters = filters.map(filter => 
      this.generateFilter(filter.template, filter.context)
    );
    
    const code = [`/**`];
    code.push(` * ${moduleName} - Generated KGEN Filters`);
    code.push(` * `);
    code.push(` * @generated ${this.options.staticBuildTime}`);
    code.push(` * @namespace ${this.options.namespace}`);
    code.push(` */`);
    code.push('');
    code.push('import crypto from \'crypto\';');
    code.push('');
    code.push(`export class ${moduleName} {`);
    code.push('  constructor(options = {}) {');
    code.push('    this.options = { ...options };');
    code.push('    this.filters = new Map();');
    code.push('    this.registerFilters();');
    code.push('  }');
    code.push('');
    code.push('  registerFilters() {');
    
    // Add each generated filter
    for (const filter of generatedFilters) {
      code.push(`    // ${filter.description}`);
      code.push(`    this.filters.set('${filter.name}', ${filter.code});`);
      code.push('');
    }
    
    code.push('  }');
    code.push('');
    code.push('  getFilter(name) {');
    code.push('    return this.filters.get(name);');
    code.push('  }');
    code.push('');
    code.push('  getAllFilters() {');
    code.push('    return Object.fromEntries(this.filters);');
    code.push('  }');
    code.push('}');
    code.push('');
    code.push(`export default ${moduleName};`);
    
    return {
      code: code.join('\n'),
      filters: generatedFilters,
      metadata: {
        moduleName,
        filterCount: generatedFilters.length,
        generated: this.options.staticBuildTime,
        namespace: this.options.namespace
      }
    };
  }

  /**
   * Simple template interpolation
   */
  interpolate(template, context) {
    let result = template;
    
    for (const [key, value] of Object.entries(context)) {
      const regex = new RegExp(`{{\\s*${key}\\s*(?:\\|[^}]*)?\\s*}}`, 'g');
      
      const matches = result.match(regex);
      if (matches) {
        for (const match of matches) {
          const defaultMatch = match.match(/\|\s*default\('([^']*)'\)/);
          const replacement = value !== undefined ? value : (defaultMatch ? defaultMatch[1] : match);
          result = result.replace(match, replacement);
        }
      }
    }
    
    return result;
  }

  /**
   * Get available filter template names
   */
  getTemplateNames() {
    return Array.from(this.filterTemplates.keys()).sort();
  }

  /**
   * Get filter template by name
   */
  getTemplate(name) {
    return this.filterTemplates.get(name);
  }

  /**
   * Generate filter documentation
   */
  generateDocs(templateNames = []) {
    const templates = templateNames.length > 0 
      ? templateNames.map(name => ({ name, ...this.filterTemplates.get(name) })).filter(t => t.name)
      : Array.from(this.filterTemplates.entries()).map(([name, template]) => ({ name, ...template }));
    
    const docs = ['# KGEN Filter Templates'];
    docs.push('');
    docs.push(`Generated: ${this.options.staticBuildTime}`);
    docs.push(`Templates: ${templates.length}`);
    docs.push('');
    
    for (const template of templates.sort((a, b) => a.name.localeCompare(b.name))) {
      docs.push(`## ${template.name}`);
      docs.push('');
      docs.push(template.description);
      docs.push('');
      
      if (template.tests && template.tests.length > 0) {
        docs.push('### Tests');
        docs.push('');
        
        for (const test of template.tests) {
          docs.push(`- **${test.description}**`);
          docs.push(`  - Input: \`${test.input}\``);
          docs.push(`  - Expected: \`${test.expected}\``);
          docs.push('');
        }
      }
      
      docs.push('---');
      docs.push('');
    }
    
    return docs.join('\n');
  }

  /**
   * Export filter templates
   */
  exportTemplates(templateNames = []) {
    const templatesToExport = templateNames.length > 0 
      ? templateNames.filter(name => this.filterTemplates.has(name))
      : Array.from(this.filterTemplates.keys());
    
    const exported = {
      format: 'kgen-filter-templates',
      version: '1.0.0',
      generated: this.options.staticBuildTime,
      namespace: this.options.namespace,
      templates: {}
    };
    
    for (const name of templatesToExport) {
      exported.templates[name] = this.filterTemplates.get(name);
    }
    
    return exported;
  }

  /**
   * Get template statistics
   */
  getStats() {
    return {
      totalTemplates: this.filterTemplates.size,
      namespace: this.options.namespace,
      deterministicMode: this.options.deterministicMode,
      templates: Array.from(this.filterTemplates.keys()).sort()
    };
  }
}

export default KGenFilterTemplates;