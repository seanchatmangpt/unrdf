/**
 * @fileoverview Project Engine - Intelligent project analysis and code generation
 * @module project-engine
 * @description Analyzes project structure and generates domain-specific patterns
 */

/**
 * Gets default project configuration
 * @returns {Object} Project configuration with file patterns and conventions
 * @example
 * const config = getProjectConfig();
 */
export function getProjectConfig() {
  return {
    fs: {
      ignorePatterns: ['node_modules', 'dist', '.git', '.vscode'],
      sourcePaths: ['src', 'packages'],
      testPaths: ['test', '**/*.test.mjs'],
    },
    project: {
      conventions: {
        sourcePaths: ['src'],
        testPaths: ['test'],
        docPaths: ['docs'],
        benchmarkPaths: ['benchmarks'],
      },
    },
  };
}

/**
 * Analyzes file patterns in project structure
 * @param {Array<string>} paths - File paths to analyze
 * @returns {Object} Pattern analysis with extension counts
 * @example
 * const patterns = analyzeFilePatterns(['src/index.mjs', 'src/utils.mjs']);
 */
export function analyzeFilePatterns(paths) {
  const patterns = {
    extensions: {},
    directories: {},
    fileCount: paths.length,
  };

  for (const path of paths) {
    const parts = path.split('/');
    const fileName = parts[parts.length - 1];
    const ext = fileName.split('.').pop();

    // Count by extension
    patterns.extensions[ext] = (patterns.extensions[ext] || 0) + 1;

    // Count by directory
    if (parts.length > 1) {
      const dir = parts[0];
      patterns.directories[dir] = (patterns.directories[dir] || 0) + 1;
    }
  }

  return patterns;
}

/**
 * Infers template kind from file path
 * @param {string} filePath - File path to analyze
 * @returns {string} Inferred template kind
 * @example
 * inferTemplateKinds('src/components/MyComponent.mjs'); // 'Component'
 */
export function inferTemplateKinds(filePath) {
  if (filePath.includes('Component')) return 'Component';
  if (filePath.includes('Page')) return 'Page';
  if (filePath.includes('test')) return 'Test';
  if (filePath.includes('Hook')) return 'Hook';
  if (filePath.includes('Service')) return 'Service';
  if (filePath.includes('store')) return 'Store';
  if (filePath.includes('util')) return 'Utility';
  return 'Unknown';
}

/**
 * Analyzes project dependencies and structure
 * @param {Object} projectRoot - Project root path
 * @returns {Object} Dependency analysis
 */
export function analyzeDependencies(projectRoot = '.') {
  return {
    root: projectRoot,
    packages: [],
    dependencies: {},
    devDependencies: {},
  };
}

/**
 * Generates code scaffold for new modules
 * @param {string} moduleName - Module name to scaffold
 * @param {string} kind - Module kind (Component, Service, etc)
 * @returns {string} Generated scaffold code
 */
export function generateScaffold(moduleName, kind = 'Service') {
  const camelCase = moduleName
    .split('-')
    .map((part, i) =>
      i === 0 ? part : part.charAt(0).toUpperCase() + part.slice(1)
    )
    .join('');

  const className =
    kind.charAt(0).toUpperCase() + kind.slice(1).toLowerCase();

  return `/**
 * @fileoverview ${moduleName}
 * @module ${moduleName}
 * @description Brief description of ${moduleName}
 */

/**
 * Creates a new ${className} instance
 * @param {Object} options - Configuration options
 * @returns {${className}} New ${className} instance
 */
export function create${camelCase}(options = {}) {
  return {
    // Implementation
  };
}

export default { create${camelCase} };
`;
}

/**
 * Validates project structure
 * @param {Object} structure - Project structure to validate
 * @returns {Object} Validation result with issues
 */
export function validateStructure(structure) {
  const issues = [];
  const warnings = [];

  if (!structure.fs?.sourcePaths) {
    issues.push('Missing fs.sourcePaths');
  }

  if (!structure.project?.conventions) {
    warnings.push('Missing project conventions');
  }

  return {
    valid: issues.length === 0,
    issues,
    warnings,
  };
}

export default {
  getProjectConfig,
  analyzeFilePatterns,
  inferTemplateKinds,
  analyzeDependencies,
  generateScaffold,
  validateStructure,
};
