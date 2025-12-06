/**
 * Variable Extractor - Extract and validate template variables
 * Migrated from ~/unjucks with enhanced pattern matching
 */

export class VariableExtractor {
  constructor(options = {}) {
    this.includeFilters = options.includeFilters !== false;
    this.includeFunctions = options.includeFunctions !== false;
    this.strict = options.strict !== false;
  }

  /**
   * Extract all variables from template content
   */
  extract(content) {
    const variables = new Set();
    const filters = new Set();
    const functions = new Set();

    // Extract {{ variable }} patterns
    this.extractOutputVariables(content, variables, filters);
    
    // Extract {% for variable in ... %} patterns
    this.extractLoopVariables(content, variables);
    
    // Extract {% if variable %} patterns
    this.extractConditionalVariables(content, variables);
    
    // Extract {% set variable = ... %} patterns
    this.extractAssignmentVariables(content, variables);
    
    // Extract function calls
    if (this.includeFunctions) {
      this.extractFunctionCalls(content, functions);
    }

    return {
      variables: Array.from(variables).sort(),
      filters: this.includeFilters ? Array.from(filters).sort() : [],
      functions: this.includeFunctions ? Array.from(functions).sort() : [],
      totalVariables: variables.size,
      complexity: this.calculateComplexity(content)
    };
  }

  /**
   * Extract variables from output expressions {{ ... }}
   */
  extractOutputVariables(content, variables, filters) {
    // Match {{ variable | filter1 | filter2 }} patterns
    const outputPattern = /\{\{\s*([^}]+?)\s*\}\}/g;
    let match;

    while ((match = outputPattern.exec(content)) !== null) {
      const expression = match[1].trim();
      this.parseExpression(expression, variables, filters);
    }
  }

  /**
   * Extract variables from loop constructs
   */
  extractLoopVariables(content, variables) {
    // Match {% for item in collection %} patterns
    const forPattern = /\{\%\s*for\s+(\w+)(?:\s*,\s*(\w+))?\s+in\s+([^%]+?)\s*\%\}/g;
    let match;

    while ((match = forPattern.exec(content)) !== null) {
      const [, itemVar, indexVar, collection] = match;
      
      // Collection is a variable we need
      const collectionVar = collection.trim().split('.')[0].split('|')[0].trim();
      if (collectionVar && this.isValidVariable(collectionVar)) {
        variables.add(collectionVar);
      }
      
      // Note: itemVar and indexVar are loop-local, not template variables
    }
  }

  /**
   * Extract variables from conditional statements
   */
  extractConditionalVariables(content, variables) {
    // Match {% if condition %} patterns
    const ifPattern = /\{\%\s*if\s+([^%]+?)\s*\%\}/g;
    const elifPattern = /\{\%\s*elif\s+([^%]+?)\s*\%\}/g;
    
    let match;

    // Process if statements
    while ((match = ifPattern.exec(content)) !== null) {
      const condition = match[1].trim();
      this.parseCondition(condition, variables);
    }

    // Process elif statements
    while ((match = elifPattern.exec(content)) !== null) {
      const condition = match[1].trim();
      this.parseCondition(condition, variables);
    }
  }

  /**
   * Extract variables from assignment statements
   */
  extractAssignmentVariables(content, variables) {
    // Match {% set variable = expression %} patterns
    const setPattern = /\{\%\s*set\s+\w+\s*=\s*([^%]+?)\s*\%\}/g;
    let match;

    while ((match = setPattern.exec(content)) !== null) {
      const expression = match[1].trim();
      this.parseExpression(expression, variables, new Set());
    }
  }

  /**
   * Extract function calls from template
   */
  extractFunctionCalls(content, functions) {
    // Match function calls like func(args)
    const functionPattern = /(\w+)\s*\(/g;
    let match;

    while ((match = functionPattern.exec(content)) !== null) {
      const funcName = match[1];
      if (!this.isBuiltinFunction(funcName)) {
        functions.add(funcName);
      }
    }
  }

  /**
   * Parse expression to extract variables and filters
   */
  parseExpression(expression, variables, filters) {
    // Handle complex expressions with operators
    const parts = expression.split(/[+\-*\/\(\)]/);
    
    for (let part of parts) {
      part = part.trim();
      
      // Skip empty parts and literals
      if (!part || this.isLiteral(part)) continue;
      
      // Check for filter chain: variable | filter1 | filter2
      if (part.includes('|')) {
        const [varPart, ...filterParts] = part.split('|').map(p => p.trim());
        
        // Extract variable
        const rootVar = this.extractRootVariable(varPart);
        if (rootVar && this.isValidVariable(rootVar)) {
          variables.add(rootVar);
        }
        
        // Extract filters
        filterParts.forEach(filter => {
          const filterName = filter.split('(')[0].trim();
          if (filterName && this.includeFilters) {
            filters.add(filterName);
          }
        });
      } else {
        // Simple variable reference
        const rootVar = this.extractRootVariable(part);
        if (rootVar && this.isValidVariable(rootVar)) {
          variables.add(rootVar);
        }
      }
    }
  }

  /**
   * Parse conditional expression to extract variables
   */
  parseCondition(condition, variables) {
    // Handle operators: and, or, not, ==, !=, <, >, <=, >=, in, is
    const conditionParts = condition.split(/\s+(?:and|or|not|==|!=|<=|>=|<|>|in|is)\s+/);
    
    conditionParts.forEach(part => {
      part = part.trim();
      if (part && !this.isLiteral(part)) {
        const rootVar = this.extractRootVariable(part);
        if (rootVar && this.isValidVariable(rootVar)) {
          variables.add(rootVar);
        }
      }
    });
  }

  /**
   * Extract root variable from complex expression
   */
  extractRootVariable(expression) {
    // Handle dot notation: user.name -> user
    // Handle array access: users[0] -> users
    // Handle function calls: len(users) -> users
    
    let cleaned = expression.trim();
    
    // Remove function calls but keep arguments
    cleaned = cleaned.replace(/\w+\(([^)]+)\)/g, '$1');
    
    // Extract variable before dot or bracket
    const match = cleaned.match(/^([a-zA-Z_][a-zA-Z0-9_]*)/);
    return match ? match[1] : null;
  }

  /**
   * Check if value is a literal (string, number, boolean)
   */
  isLiteral(value) {
    // String literals
    if ((value.startsWith('"') && value.endsWith('"')) || 
        (value.startsWith("'") && value.endsWith("'"))) {
      return true;
    }
    
    // Number literals
    if (/^-?\d+(\.\d+)?$/.test(value)) {
      return true;
    }
    
    // Boolean literals
    if (['true', 'false', 'True', 'False'].includes(value)) {
      return true;
    }
    
    // None/null literals
    if (['none', 'null', 'None', 'Null'].includes(value)) {
      return true;
    }
    
    return false;
  }

  /**
   * Check if variable name is valid
   */
  isValidVariable(name) {
    // Valid variable names: letters, numbers, underscore
    // Must start with letter or underscore
    return /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(name) && 
           !this.isBuiltinVariable(name);
  }

  /**
   * Check if variable is a builtin
   */
  isBuiltinVariable(name) {
    const builtins = [
      'loop', 'super', 'self', 'varargs', 'kwargs', 
      'joiner', 'cycler', 'range', 'lipsum'
    ];
    return builtins.includes(name);
  }

  /**
   * Check if function is a builtin
   */
  isBuiltinFunction(name) {
    const builtins = [
      'range', 'lipsum', 'dict', 'list', 'tuple', 'set',
      'len', 'str', 'int', 'float', 'bool'
    ];
    return builtins.includes(name);
  }

  /**
   * Calculate template complexity
   */
  calculateComplexity(content) {
    const patterns = {
      variables: /\{\{\s*[^}]+?\s*\}\}/g,
      conditions: /\{\%\s*if\s+[^%]+?\s*\%\}/g,
      loops: /\{\%\s*for\s+[^%]+?\s*\%\}/g,
      includes: /\{\%\s*include\s+[^%]+?\s*\%\}/g,
      macros: /\{\%\s*macro\s+[^%]+?\s*\%\}/g,
      blocks: /\{\%\s*block\s+[^%]+?\s*\%\}/g
    };

    let complexity = 0;
    Object.values(patterns).forEach(pattern => {
      const matches = content.match(pattern);
      complexity += matches ? matches.length : 0;
    });

    return complexity;
  }

  /**
   * Validate extracted variables against provided context
   */
  validateContext(extractedVars, context, options = {}) {
    const errors = [];
    const warnings = [];
    
    const availableVars = new Set([
      ...Object.keys(context),
      '__meta', // Always available
      ...options.additionalVars || []
    ]);

    extractedVars.forEach(varName => {
      if (!availableVars.has(varName)) {
        if (options.strictMode) {
          errors.push(`Required variable '${varName}' not found in context`);
        } else {
          warnings.push(`Variable '${varName}' not found in context`);
        }
      }
    });

    // Check for unused context variables
    if (options.warnUnused) {
      Object.keys(context).forEach(contextVar => {
        if (!extractedVars.includes(contextVar) && !contextVar.startsWith('_')) {
          warnings.push(`Context variable '${contextVar}' is unused in template`);
        }
      });
    }

    return {
      valid: errors.length === 0,
      errors,
      warnings
    };
  }

  /**
   * Get extractor statistics
   */
  getStats() {
    return {
      includeFilters: this.includeFilters,
      includeFunctions: this.includeFunctions,
      strict: this.strict
    };
  }
}

export default VariableExtractor;