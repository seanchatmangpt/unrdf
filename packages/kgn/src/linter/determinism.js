/**
 * Template Linter - Enforce deterministic template patterns
 * Migrated from ~/unjucks with comprehensive determinism checking
 */

export class TemplateLinter {
  constructor(options = {}) {
    this.strict = options.strict !== false;
    this.warnOnly = options.warnOnly === true;
    this.customRules = options.customRules || [];
  }

  /**
   * Lint template for determinism issues
   */
  lint(templateContent, frontmatter = {}) {
    const issues = [];
    const warnings = [];
    const suggestions = [];

    // Run all lint checks
    this.checkNonDeterministicOperations(templateContent, issues);
    this.checkDateTimeUsage(templateContent, issues, warnings);
    this.checkRandomOperations(templateContent, issues);
    this.checkSystemDependentOperations(templateContent, warnings);
    this.checkVariableConsistency(templateContent, warnings);
    this.checkFrontmatterCompliance(frontmatter, warnings, suggestions);
    
    // Run custom rules if provided
    this.customRules.forEach(rule => {
      try {
        rule(templateContent, frontmatter, issues, warnings, suggestions);
      } catch (error) {
        warnings.push({
          rule: 'custom-rule',
          message: `Custom rule failed: ${error.message}`,
          severity: 'warning',
          line: 0
        });
      }
    });

    // Calculate determinism score
    const score = this.calculateDeterminismScore(issues, warnings);

    return {
      deterministic: issues.length === 0,
      score,
      issues: issues.sort((a, b) => b.severity.localeCompare(a.severity)),
      warnings: warnings.sort((a, b) => a.line - b.line),
      suggestions: suggestions.sort((a, b) => a.priority - b.priority),
      summary: {
        totalIssues: issues.length,
        criticalIssues: issues.filter(i => i.severity === 'error').length,
        warnings: warnings.length,
        suggestions: suggestions.length,
        passesLint: issues.filter(i => i.severity === 'error').length === 0
      }
    };
  }

  /**
   * Check for non-deterministic operations
   */
  checkNonDeterministicOperations(content, issues) {
    const nonDeterministicPatterns = [
      {
        pattern: /\{\{\s*([^}]*now[^}]*)\s*\}\}/gi,
        message: 'Non-deterministic time operation: {{ now }}. Use {{ timestamp }} filter instead.',
        rule: 'no-current-time',
        severity: 'error',
        fix: 'Replace with {{ timestamp }} or use deterministic date filters'
      },
      {
        pattern: /\{\{\s*([^}]*random[^}]*)\s*\}\}/gi,
        message: 'Non-deterministic random operation. Use {{ content | hash }} for consistent randomness.',
        rule: 'no-random',
        severity: 'error',
        fix: 'Use hash filters for deterministic pseudo-randomness'
      },
      {
        pattern: /\{\{\s*([^}]*uuid[^}]*)\s*\}\}/gi,
        message: 'Non-deterministic UUID generation. Use {{ content | hash | shortHash }} instead.',
        rule: 'no-uuid',
        severity: 'error',
        fix: 'Use hash-based ID generation for consistency'
      },
      {
        pattern: /Math\.random\(\)/gi,
        message: 'Direct Math.random() usage is non-deterministic.',
        rule: 'no-math-random',
        severity: 'error',
        fix: 'Use hash-based pseudo-random generation'
      }
    ];

    nonDeterministicPatterns.forEach(({ pattern, message, rule, severity, fix }) => {
      const matches = [...content.matchAll(pattern)];
      matches.forEach(match => {
        const line = this.getLineNumber(content, match.index);
        issues.push({
          rule,
          message,
          severity,
          line,
          column: match.index - this.getLineStart(content, match.index),
          match: match[0],
          fix
        });
      });
    });
  }

  /**
   * Check date/time usage patterns
   */
  checkDateTimeUsage(content, issues, warnings) {
    const dateTimePatterns = [
      {
        pattern: /new Date\(\)/gi,
        message: 'new Date() without arguments is non-deterministic.',
        type: 'error',
        rule: 'no-current-date'
      },
      {
        pattern: /Date\.now\(\)/gi,
        message: 'Date.now() is non-deterministic.',
        type: 'error',
        rule: 'no-date-now'
      },
      {
        pattern: /\{\{\s*([^}]*date[^}]*)\s*\}\}/gi,
        message: 'Date usage detected. Verify it uses deterministic formatting.',
        type: 'warning',
        rule: 'verify-date-usage'
      }
    ];

    dateTimePatterns.forEach(({ pattern, message, type, rule }) => {
      const matches = [...content.matchAll(pattern)];
      matches.forEach(match => {
        const line = this.getLineNumber(content, match.index);
        const issue = {
          rule,
          message,
          line,
          column: match.index - this.getLineStart(content, match.index),
          match: match[0]
        };

        if (type === 'error') {
          issue.severity = 'error';
          issues.push(issue);
        } else {
          issue.severity = 'warning';
          warnings.push(issue);
        }
      });
    });
  }

  /**
   * Check for random operations
   */
  checkRandomOperations(content, issues) {
    const randomPatterns = [
      /crypto\.randomUUID\(\)/gi,
      /crypto\.getRandomValues\(/gi,
      /Math\.floor\(Math\.random\(\)/gi,
      /\|\s*random\s*[\|}]/gi
    ];

    randomPatterns.forEach(pattern => {
      const matches = [...content.matchAll(pattern)];
      matches.forEach(match => {
        const line = this.getLineNumber(content, match.index);
        issues.push({
          rule: 'no-random-operations',
          message: `Random operation detected: ${match[0]}. This breaks deterministic rendering.`,
          severity: 'error',
          line,
          column: match.index - this.getLineStart(content, match.index),
          match: match[0],
          fix: 'Use hash-based deterministic alternatives'
        });
      });
    });
  }

  /**
   * Check for system-dependent operations
   */
  checkSystemDependentOperations(content, warnings) {
    const systemPatterns = [
      {
        pattern: /process\.env\./gi,
        message: 'Environment variable usage may cause non-deterministic output across systems.',
        rule: 'env-vars-warning'
      },
      {
        pattern: /os\./gi,
        message: 'OS-dependent operations may cause different output across platforms.',
        rule: 'os-dependent-warning'
      },
      {
        pattern: /require\(['"]fs['"]\)/gi,
        message: 'File system operations may be non-deterministic.',
        rule: 'fs-operations-warning'
      }
    ];

    systemPatterns.forEach(({ pattern, message, rule }) => {
      const matches = [...content.matchAll(pattern)];
      matches.forEach(match => {
        const line = this.getLineNumber(content, match.index);
        warnings.push({
          rule,
          message,
          severity: 'warning',
          line,
          column: match.index - this.getLineStart(content, match.index),
          match: match[0]
        });
      });
    });
  }

  /**
   * Check variable naming consistency
   */
  checkVariableConsistency(content, warnings) {
    // Extract all variable references
    const variablePattern = /\{\{\s*([a-zA-Z_][a-zA-Z0-9_.]*)/g;
    const variables = new Map();
    
    let match;
    while ((match = variablePattern.exec(content)) !== null) {
      const varName = match[1].split('.')[0]; // Get root variable name
      const line = this.getLineNumber(content, match.index);
      
      if (!variables.has(varName)) {
        variables.set(varName, []);
      }
      variables.get(varName).push({ line, usage: match[1] });
    }

    // Check for inconsistent variable usage patterns
    variables.forEach((usages, varName) => {
      if (usages.length === 1) {
        warnings.push({
          rule: 'unused-variable',
          message: `Variable '${varName}' is only used once. Consider if it's necessary.`,
          severity: 'info',
          line: usages[0].line,
          column: 0
        });
      }
    });
  }

  /**
   * Check frontmatter compliance
   */
  checkFrontmatterCompliance(frontmatter, warnings, suggestions) {
    const requiredFields = ['name', 'description'];
    const recommendedFields = ['version', 'author', 'category'];

    // Check required fields
    requiredFields.forEach(field => {
      if (!frontmatter[field]) {
        warnings.push({
          rule: 'missing-required-frontmatter',
          message: `Missing required frontmatter field: ${field}`,
          severity: 'warning',
          line: 0,
          section: 'frontmatter'
        });
      }
    });

    // Check recommended fields
    recommendedFields.forEach(field => {
      if (!frontmatter[field]) {
        suggestions.push({
          rule: 'recommended-frontmatter',
          message: `Consider adding frontmatter field: ${field}`,
          priority: 2,
          section: 'frontmatter'
        });
      }
    });

    // Validate variables documentation
    if (frontmatter.variables) {
      if (typeof frontmatter.variables !== 'object') {
        warnings.push({
          rule: 'invalid-variables-format',
          message: 'Frontmatter variables should be an object with descriptions',
          severity: 'warning',
          line: 0,
          section: 'frontmatter'
        });
      }
    } else {
      suggestions.push({
        rule: 'document-variables',
        message: 'Consider documenting template variables in frontmatter',
        priority: 1,
        section: 'frontmatter'
      });
    }
  }

  /**
   * Calculate determinism score (0-100)
   */
  calculateDeterminismScore(issues, warnings) {
    let score = 100;
    
    // Subtract points for issues
    issues.forEach(issue => {
      switch (issue.severity) {
        case 'error':
          score -= 20;
          break;
        case 'warning':
          score -= 5;
          break;
        default:
          score -= 1;
      }
    });

    // Subtract points for warnings
    warnings.forEach(warning => {
      score -= 2;
    });

    return Math.max(0, score);
  }

  /**
   * Get line number for character position
   */
  getLineNumber(content, position) {
    return content.substring(0, position).split('\n').length;
  }

  /**
   * Get start position of line containing character position
   */
  getLineStart(content, position) {
    const beforePosition = content.substring(0, position);
    const lastNewline = beforePosition.lastIndexOf('\n');
    return lastNewline === -1 ? 0 : lastNewline + 1;
  }

  /**
   * Auto-fix determinism issues where possible
   */
  autoFix(templateContent) {
    let fixed = templateContent;
    let fixCount = 0;

    const fixes = [
      {
        pattern: /\{\{\s*now\s*\}\}/gi,
        replacement: '{{ timestamp }}',
        description: 'Replace {{ now }} with {{ timestamp }}'
      },
      {
        pattern: /\{\{\s*([^}]*)\s*\|\s*random\s*\}\}/gi,
        replacement: '{{ $1 | hash | shortHash }}',
        description: 'Replace random filter with hash-based alternative'
      },
      {
        pattern: /Math\.random\(\)/gi,
        replacement: '/* Use hash-based randomness instead */',
        description: 'Comment out Math.random() usage'
      }
    ];

    fixes.forEach(({ pattern, replacement, description }) => {
      const matches = fixed.match(pattern);
      if (matches) {
        fixed = fixed.replace(pattern, replacement);
        fixCount += matches.length;
      }
    });

    return {
      fixed,
      fixCount,
      modified: fixCount > 0
    };
  }

  /**
   * Get linter statistics
   */
  getStats() {
    return {
      strict: this.strict,
      warnOnly: this.warnOnly,
      customRulesCount: this.customRules.length
    };
  }
}

export default TemplateLinter;