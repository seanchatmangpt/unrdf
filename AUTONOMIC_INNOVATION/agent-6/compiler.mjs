/**
 * @file Conventions Profile Compiler
 * @description Compile and validate code against organizational conventions
 */

import { readFileSync } from 'node:fs';
import { parse } from 'acorn';
import { ConventionsProfileSchema, CompiledProfileSchema, ValidationResultSchema } from './profile-schema.mjs';

/**
 * Compile a conventions profile
 * @param {object} profileObj - Raw profile object
 * @returns {import('./profile-schema.mjs').CompiledProfile} Compiled profile
 * @throws {Error} If profile is invalid
 */
export function compileProfile(profileObj) {
  // Validate against schema
  const parseResult = ConventionsProfileSchema.safeParse(profileObj);

  if (!parseResult.success) {
    const errors = parseResult.error.issues.map(e => `${e.path.join('.')}: ${e.message}`).join(', ');
    throw new Error(`Invalid profile: ${errors}`);
  }

  const compiled = {
    schema: parseResult.data,
    violations: [],
    compiled: true,
    timestamp: new Date().toISOString()
  };

  // Validate compiled output
  return CompiledProfileSchema.parse(compiled);
}

/**
 * Check if identifier matches naming convention
 * @param {string} identifier - Identifier to check
 * @param {string} convention - Naming convention
 * @returns {boolean} Whether identifier matches convention
 */
function matchesNamingConvention(identifier, convention) {
  switch (convention) {
    case 'PascalCase':
      return /^[A-Z][a-zA-Z0-9]*$/.test(identifier);
    case 'camelCase':
      return /^[a-z][a-zA-Z0-9]*$/.test(identifier);
    case 'snake_case':
      return /^[a-z][a-z0-9_]*$/.test(identifier);
    case 'UPPER_SNAKE_CASE':
    case 'SCREAMING_SNAKE_CASE':
      return /^[A-Z][A-Z0-9_]*$/.test(identifier);
    case 'kebab-case':
      return /^[a-z][a-z0-9-]*$/.test(identifier);
    default:
      return false;
  }
}

/**
 * Convert identifier to naming convention
 * @param {string} identifier - Current identifier
 * @param {string} convention - Target convention
 * @returns {string} Converted identifier
 */
function convertToConvention(identifier, convention) {
  // Split on various delimiters
  const words = identifier
    .replace(/([A-Z])/g, ' $1')
    .replace(/[_-]/g, ' ')
    .trim()
    .toLowerCase()
    .split(/\s+/);

  switch (convention) {
    case 'PascalCase':
      return words.map(w => w.charAt(0).toUpperCase() + w.slice(1)).join('');
    case 'camelCase':
      return words.map((w, i) => i === 0 ? w : w.charAt(0).toUpperCase() + w.slice(1)).join('');
    case 'snake_case':
      return words.join('_');
    case 'UPPER_SNAKE_CASE':
    case 'SCREAMING_SNAKE_CASE':
      return words.join('_').toUpperCase();
    case 'kebab-case':
      return words.join('-');
    default:
      return identifier;
  }
}

/**
 * Extract class names from AST
 * @param {object} ast - Acorn AST
 * @returns {Array<{name: string, line: number, column: number}>} Class names
 */
function extractClassNames(ast) {
  const classes = [];

  function walk(node, parent = null) {
    if (!node || typeof node !== 'object') return;

    if (node.type === 'ClassDeclaration' && node.id) {
      classes.push({
        name: node.id.name,
        line: node.loc?.start.line,
        column: node.loc?.start.column
      });
    }

    for (const key in node) {
      if (key === 'loc' || key === 'range') continue;
      const child = node[key];
      if (Array.isArray(child)) {
        child.forEach(c => walk(c, node));
      } else if (typeof child === 'object') {
        walk(child, node);
      }
    }
  }

  walk(ast);
  return classes;
}

/**
 * Extract method names from AST
 * @param {object} ast - Acorn AST
 * @returns {Array<{name: string, line: number, column: number}>} Method names
 */
function extractMethodNames(ast) {
  const methods = [];

  function walk(node) {
    if (!node || typeof node !== 'object') return;

    // Function declarations
    if (node.type === 'FunctionDeclaration' && node.id) {
      methods.push({
        name: node.id.name,
        line: node.loc?.start.line,
        column: node.loc?.start.column
      });
    }

    // Method definitions in classes
    if (node.type === 'MethodDefinition' && node.key) {
      methods.push({
        name: node.key.name || node.key.value,
        line: node.loc?.start.line,
        column: node.loc?.start.column
      });
    }

    for (const key in node) {
      if (key === 'loc' || key === 'range') continue;
      const child = node[key];
      if (Array.isArray(child)) {
        child.forEach(walk);
      } else if (typeof child === 'object') {
        walk(child);
      }
    }
  }

  walk(ast);
  return methods;
}

/**
 * Extract constant names from AST (module-level only)
 * @param {object} ast - Acorn AST
 * @returns {Array<{name: string, line: number, column: number}>} Constant names
 */
function extractConstantNames(ast) {
  const constants = [];

  // Only check top-level const declarations (not inside functions/classes)
  if (ast.type === 'Program' && ast.body) {
    for (const statement of ast.body) {
      if (statement.type === 'VariableDeclaration' && statement.kind === 'const') {
        for (const decl of statement.declarations) {
          if (decl.id?.type === 'Identifier') {
            constants.push({
              name: decl.id.name,
              line: decl.loc?.start.line,
              column: decl.loc?.start.column
            });
          }
        }
      }
      // Also check ExportNamedDeclaration
      if (statement.type === 'ExportNamedDeclaration' && statement.declaration) {
        const decl = statement.declaration;
        if (decl.type === 'VariableDeclaration' && decl.kind === 'const') {
          for (const d of decl.declarations) {
            if (d.id?.type === 'Identifier') {
              constants.push({
                name: d.id.name,
                line: d.loc?.start.line,
                column: d.loc?.start.column
              });
            }
          }
        }
      }
    }
  }

  return constants;
}

/**
 * Validate file against compiled profile
 * @param {string} filePath - Path to file
 * @param {import('./profile-schema.mjs').CompiledProfile} compiledProfile - Compiled profile
 * @returns {import('./profile-schema.mjs').Violation[]} Violations found
 */
function validateFile(filePath, compiledProfile) {
  const violations = [];
  const { schema } = compiledProfile;

  try {
    const content = readFileSync(filePath, 'utf-8');
    const ast = parse(content, {
      ecmaVersion: 'latest',
      sourceType: 'module',
      locations: true
    });

    // Check class naming
    const classes = extractClassNames(ast);
    for (const cls of classes) {
      if (!matchesNamingConvention(cls.name, schema.fileLayout.naming.serviceClass)) {
        violations.push({
          file: filePath,
          rule: 'naming.serviceClass',
          message: `Class '${cls.name}' does not match ${schema.fileLayout.naming.serviceClass} convention`,
          suggestion: `Rename to '${convertToConvention(cls.name, schema.fileLayout.naming.serviceClass)}'`,
          line: cls.line,
          column: cls.column
        });
      }
    }

    // Check method naming
    const methods = extractMethodNames(ast);
    for (const method of methods) {
      if (!matchesNamingConvention(method.name, schema.fileLayout.naming.method)) {
        violations.push({
          file: filePath,
          rule: 'naming.method',
          message: `Method '${method.name}' does not match ${schema.fileLayout.naming.method} convention`,
          suggestion: `Rename to '${convertToConvention(method.name, schema.fileLayout.naming.method)}'`,
          line: method.line,
          column: method.column
        });
      }
    }

    // Check constant naming
    const constants = extractConstantNames(ast);
    for (const constant of constants) {
      if (!matchesNamingConvention(constant.name, schema.fileLayout.naming.constant)) {
        violations.push({
          file: filePath,
          rule: 'naming.constant',
          message: `Constant '${constant.name}' does not match ${schema.fileLayout.naming.constant} convention`,
          suggestion: `Rename to '${convertToConvention(constant.name, schema.fileLayout.naming.constant)}'`,
          line: constant.line,
          column: constant.column
        });
      }
    }

  } catch (error) {
    violations.push({
      file: filePath,
      rule: 'syntax',
      message: `Failed to parse file: ${error.message}`,
      suggestion: 'Fix syntax errors before running validation'
    });
  }

  return violations;
}

/**
 * Validate code against compiled profile
 * @param {import('./profile-schema.mjs').CompiledProfile} compiledProfile - Compiled profile
 * @param {string[]} targetFiles - Files to validate
 * @returns {import('./profile-schema.mjs').ValidationResult} Validation result
 */
export function validateAgainstProfile(compiledProfile, targetFiles) {
  const violations = [];

  for (const file of targetFiles) {
    const fileViolations = validateFile(file, compiledProfile);
    violations.push(...fileViolations);
  }

  const result = {
    valid: violations.length === 0,
    violations,
    filesChecked: targetFiles.length,
    timestamp: new Date().toISOString()
  };

  return ValidationResultSchema.parse(result);
}

/**
 * Generate diagnostic report from violations
 * @param {import('./profile-schema.mjs').Violation[]} violations - Violations to report
 * @returns {string} Human-readable diagnostic report
 */
export function diagnosticReport(violations) {
  if (violations.length === 0) {
    return '✅ No violations found. Code is compliant with profile.';
  }

  const lines = [];
  lines.push(`\n❌ Found ${violations.length} violation${violations.length === 1 ? '' : 's'}\n`);

  // Group by rule
  const byRule = {};
  for (const v of violations) {
    if (!byRule[v.rule]) byRule[v.rule] = [];
    byRule[v.rule].push(v);
  }

  // Report by rule type
  for (const [rule, ruleViolations] of Object.entries(byRule)) {
    lines.push(`\n📋 ${rule} (${ruleViolations.length} violation${ruleViolations.length === 1 ? '' : 's'}):`);
    lines.push('─'.repeat(60));

    for (const v of ruleViolations) {
      const location = v.line ? `:${v.line}:${v.column || 0}` : '';
      lines.push(`\n  File: ${v.file}${location}`);
      lines.push(`  Issue: ${v.message}`);
      if (v.suggestion) {
        lines.push(`  Fix: ${v.suggestion}`);
      }
    }
  }

  lines.push('\n' + '='.repeat(60));
  lines.push(`\nSummary: ${violations.length} total violations across ${new Set(violations.map(v => v.file)).size} files\n`);

  return lines.join('\n');
}
