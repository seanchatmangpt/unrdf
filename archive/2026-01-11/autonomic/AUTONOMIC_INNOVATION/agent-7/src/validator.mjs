/**
 * Code validation for generated façades
 * @module validator
 */

/**
 * Validate generated code against profile
 * @param {string} code - Generated source code
 * @param {CompiledProfile} profile - Target conventions
 * @returns {{ ok: boolean, violations: string[] }}
 */
export function validateGeneratedCode(code, profile) {
  const violations = [];

  // Static analysis
  violations.push(...checkImportOrder(code));
  violations.push(...checkJSDocFormat(code, profile));
  violations.push(...checkLineLength(code, 100));
  violations.push(...checkIndentation(code));
  violations.push(...checkTrailingNewline(code));

  // Convention compliance
  violations.push(...checkErrorHandling(code, profile));

  // Determinism check
  violations.push(...checkNonDeterministicAPIs(code));

  return {
    ok: violations.length === 0,
    violations
  };
}

/**
 * Check import statement ordering
 * @param {string} code - Source code
 * @returns {string[]} Violations
 */
export function checkImportOrder(code) {
  const violations = [];
  const importRegex = /^import\s+.*?from\s+['"](.+?)['"]/gm;
  const imports = [];
  let match;

  while ((match = importRegex.exec(code)) !== null) {
    imports.push({
      line: match[0],
      from: match[1],
      index: match.index
    });
  }

  if (imports.length === 0) return violations;

  // Group imports
  const groups = {
    builtin: [],
    external: [],
    internal: []
  };

  for (const imp of imports) {
    if (imp.from.startsWith('node:') || isBuiltinModule(imp.from)) {
      groups.builtin.push(imp);
    } else if (imp.from.startsWith('.')) {
      groups.internal.push(imp);
    } else {
      groups.external.push(imp);
    }
  }

  // Check each group is sorted
  const checkGroupSorted = (group, name) => {
    for (let i = 1; i < group.length; i++) {
      if (group[i - 1].from > group[i].from) {
        violations.push(`${name} imports not sorted: ${group[i - 1].from} > ${group[i].from}`);
      }
    }
  };

  checkGroupSorted(groups.builtin, 'Builtin');
  checkGroupSorted(groups.external, 'External');
  checkGroupSorted(groups.internal, 'Internal');

  return violations;
}

/**
 * Check if module is built-in
 * @param {string} moduleName - Module name
 * @returns {boolean} True if built-in
 */
function isBuiltinModule(moduleName) {
  const builtins = ['fs', 'path', 'http', 'https', 'crypto', 'util', 'stream', 'events', 'assert'];
  return builtins.includes(moduleName);
}

/**
 * Check JSDoc format matches profile
 * @param {string} code - Source code
 * @param {CompiledProfile} profile - Target conventions
 * @returns {string[]} Violations
 */
export function checkJSDocFormat(code, profile) {
  const violations = [];
  const jsdocBlocks = extractJSDocBlocks(code);
  const expectedOrder = profile?.conventions?.jsdoc?.fieldOrder ||
    ['description', 'param', 'returns', 'throws', 'example'];

  for (let i = 0; i < jsdocBlocks.length; i++) {
    const block = jsdocBlocks[i];
    const tags = block.tags;

    // Check order
    for (let j = 1; j < tags.length; j++) {
      const prevIdx = expectedOrder.indexOf(tags[j - 1]);
      const currIdx = expectedOrder.indexOf(tags[j]);

      if (prevIdx > currIdx && prevIdx !== -1 && currIdx !== -1) {
        violations.push(
          `JSDoc block ${i}: tags out of order (${tags[j - 1]} at ${prevIdx} > ${tags[j]} at ${currIdx})`
        );
      }
    }
  }

  return violations;
}

/**
 * Extract JSDoc blocks from code
 * @param {string} code - Source code
 * @returns {Array<{tags: string[], content: string}>} JSDoc blocks
 */
function extractJSDocBlocks(code) {
  const blocks = [];
  const jsdocRegex = /\/\*\*[\s\S]*?\*\//g;
  let match;

  while ((match = jsdocRegex.exec(code)) !== null) {
    const content = match[0];
    const tags = [];

    // Extract tags
    const tagRegex = /@(\w+)/g;
    let tagMatch;
    while ((tagMatch = tagRegex.exec(content)) !== null) {
      tags.push(tagMatch[1]);
    }

    // Add description as first tag if no tags found at start
    if (tags.length === 0 || !content.includes(' * @')) {
      tags.unshift('description');
    } else if (!content.match(/\/\*\*\s*\n\s*\*\s*@/)) {
      tags.unshift('description');
    }

    blocks.push({ tags, content });
  }

  return blocks;
}

/**
 * Check line length
 * @param {string} code - Source code
 * @param {number} maxLength - Maximum line length
 * @returns {string[]} Violations
 */
export function checkLineLength(code, maxLength = 100) {
  const violations = [];
  const lines = code.split('\n');

  for (let i = 0; i < lines.length; i++) {
    if (lines[i].length > maxLength) {
      violations.push(`Line ${i + 1} exceeds ${maxLength} characters (${lines[i].length})`);
    }
  }

  return violations;
}

/**
 * Check indentation consistency
 * @param {string} code - Source code
 * @returns {string[]} Violations
 */
export function checkIndentation(code) {
  const violations = [];
  const lines = code.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Skip JSDoc lines (lines that start with ' *' or are '/**' or ' */')
    if (line.match(/^\s*\*/) || line.trim() === '/**' || line.trim() === '*/') {
      continue;
    }

    if (line.match(/^\t/)) {
      violations.push(`Line ${i + 1} uses tabs instead of spaces`);
    }

    // Check for mixed spaces (not multiples of 2)
    // Only check non-empty lines
    if (line.trim().length > 0) {
      const leadingSpaces = line.match(/^( *)/)?.[1]?.length || 0;
      if (leadingSpaces > 0 && leadingSpaces % 2 !== 0) {
        violations.push(`Line ${i + 1} has inconsistent indentation (${leadingSpaces} spaces)`);
      }
    }
  }

  return violations;
}

/**
 * Check trailing newline
 * @param {string} code - Source code
 * @returns {string[]} Violations
 */
export function checkTrailingNewline(code) {
  const violations = [];

  if (!code.endsWith('\n')) {
    violations.push('File must end with a single newline');
  }

  if (code.endsWith('\n\n')) {
    violations.push('File must not have multiple trailing newlines');
  }

  return violations;
}

/**
 * Check error handling matches profile
 * @param {string} code - Source code
 * @param {CompiledProfile} profile - Target conventions
 * @returns {string[]} Violations
 */
export function checkErrorHandling(code, profile) {
  const violations = [];
  const errorStyle = profile?.conventions?.errorHandling?.style || 'try-catch-zod';

  if (errorStyle === 'try-catch-zod') {
    // Check for try-catch blocks
    if (!code.includes('try {') && code.includes('function')) {
      violations.push('Expected try-catch blocks for error handling');
    }

    // Check for proper error wrapping
    if (code.includes('throw err') && !code.includes('throw new')) {
      violations.push('Errors should be wrapped in custom error classes');
    }
  }

  return violations;
}

/**
 * Check for non-deterministic API usage
 * @param {string} code - Source code
 * @returns {string[]} Violations
 */
export function checkNonDeterministicAPIs(code) {
  const violations = [];
  const forbidden = [
    { pattern: /Date\.now\(\)/, name: 'Date.now()' },
    { pattern: /Math\.random\(\)/, name: 'Math.random()' },
    { pattern: /process\.hrtime/, name: 'process.hrtime' },
    { pattern: /crypto\.randomUUID/, name: 'crypto.randomUUID' },
    { pattern: /performance\.now/, name: 'performance.now' }
  ];

  for (const { pattern, name } of forbidden) {
    if (pattern.test(code)) {
      violations.push(`Non-deterministic API detected: ${name}`);
    }
  }

  return violations;
}

/**
 * Check logging fields match lens transformations
 * @param {string} code - Source code
 * @param {CompiledProfile} profile - Target conventions
 * @returns {string[]} Violations
 */
export function checkLoggingFields(code, profile) {
  const violations = [];

  // Extract logging calls
  const loggingRegex = /logOperation\(\{[\s\S]*?\}\)/g;
  const calls = code.match(loggingRegex) || [];

  for (const call of calls) {
    // Check fields are sorted
    const fieldsMatch = call.match(/(\w+):/g);
    if (fieldsMatch) {
      const fields = fieldsMatch.map(f => f.replace(':', ''));
      const sorted = [...fields].sort();

      for (let i = 0; i < fields.length; i++) {
        if (fields[i] !== sorted[i]) {
          violations.push(`Logging fields not sorted: ${fields.join(', ')} (expected: ${sorted.join(', ')})`);
          break;
        }
      }
    }
  }

  return violations;
}
