/**
 * @fileoverview Injection Checker - Detect dangerous code patterns
 *
 * Implements Q_no_dangerous_apis invariant:
 * - Detects eval(), Function constructor, dynamic requires
 * - Identifies command injection vulnerabilities
 * - Finds prototype pollution risks
 * - Checks for XSS-prone patterns
 *
 * @module security/injection-checker
 */

import { z } from 'zod';
import { readFileSync } from 'node:fs';
import { readdir } from 'node:fs/promises';
import { join, extname, relative } from 'node:path';

/**
 * Injection finding schema
 */
export const InjectionFindingSchema = z.object({
  type: z.enum([
    'code_injection',
    'command_injection',
    'prototype_pollution',
    'xss',
    'sql_injection',
    'path_traversal',
    'regex_dos',
    'unsafe_deserialization'
  ]),
  severity: z.enum(['critical', 'high', 'medium', 'low']),
  file: z.string(),
  line: z.number(),
  column: z.number().optional(),
  code: z.string(),
  rule: z.string(),
  description: z.string(),
  recommendation: z.string()
});

/**
 * @typedef {z.infer<typeof InjectionFindingSchema>} InjectionFinding
 */

/**
 * Dangerous pattern detection rules
 */
const INJECTION_RULES = [
  // Code Injection
  {
    name: 'eval_usage',
    type: 'code_injection',
    pattern: /\beval\s*\(/g,
    severity: 'critical',
    description: 'Direct eval() usage allows arbitrary code execution',
    recommendation: 'Use JSON.parse() for JSON data, or AST-based evaluation for expressions'
  },
  {
    name: 'function_constructor',
    type: 'code_injection',
    pattern: /new\s+Function\s*\(/g,
    severity: 'critical',
    description: 'Function constructor can execute arbitrary code',
    recommendation: 'Use predefined functions or a safe expression parser'
  },
  {
    name: 'indirect_eval',
    type: 'code_injection',
    pattern: /\(\s*0\s*,\s*eval\s*\)/g,
    severity: 'critical',
    description: 'Indirect eval call bypasses strict mode restrictions',
    recommendation: 'Remove all eval usage from the codebase'
  },
  {
    name: 'settimeout_string',
    type: 'code_injection',
    pattern: /setTimeout\s*\(\s*["'`]/g,
    severity: 'high',
    description: 'setTimeout with string argument executes as eval',
    recommendation: 'Pass a function reference instead of a string'
  },
  {
    name: 'setinterval_string',
    type: 'code_injection',
    pattern: /setInterval\s*\(\s*["'`]/g,
    severity: 'high',
    description: 'setInterval with string argument executes as eval',
    recommendation: 'Pass a function reference instead of a string'
  },
  {
    name: 'dynamic_import_variable',
    type: 'code_injection',
    pattern: /import\s*\(\s*[^"'`\s][^)]*\)/g,
    severity: 'medium',
    description: 'Dynamic import with variable path could load malicious modules',
    recommendation: 'Use a whitelist of allowed modules'
  },
  {
    name: 'dynamic_require',
    type: 'code_injection',
    pattern: /require\s*\(\s*[^"'`\s][^)]*\)/g,
    severity: 'medium',
    description: 'Dynamic require with variable path is unsafe',
    recommendation: 'Use static requires or a module whitelist'
  },

  // Command Injection
  {
    name: 'exec_command',
    type: 'command_injection',
    pattern: /child_process.*\.exec\s*\(/g,
    severity: 'critical',
    description: 'exec() passes commands through shell, enabling injection',
    recommendation: 'Use execFile() with argument array instead'
  },
  {
    name: 'exec_sync_command',
    type: 'command_injection',
    pattern: /execSync\s*\(/g,
    severity: 'critical',
    description: 'execSync() with shell=true enables command injection',
    recommendation: 'Use execFileSync() with explicit arguments'
  },
  {
    name: 'spawn_shell',
    type: 'command_injection',
    pattern: /spawn\s*\([^)]*shell\s*:\s*true/g,
    severity: 'high',
    description: 'spawn with shell:true enables command injection',
    recommendation: 'Remove shell:true and pass arguments as array'
  },
  {
    name: 'template_in_exec',
    type: 'command_injection',
    pattern: /exec(?:Sync)?\s*\(\s*`[^`]*\$\{/g,
    severity: 'critical',
    description: 'Template literal in exec allows injection',
    recommendation: 'Use execFile with separate argument array'
  },

  // Prototype Pollution
  {
    name: 'object_merge_unsafe',
    type: 'prototype_pollution',
    pattern: /Object\.assign\s*\(\s*\{\s*\}\s*,\s*(?!Object\.create)/g,
    severity: 'medium',
    description: 'Object.assign may be vulnerable to prototype pollution',
    recommendation: 'Use Object.create(null) as target or validate input'
  },
  {
    name: 'proto_access',
    type: 'prototype_pollution',
    pattern: /\[["'`]__proto__["'`]\]/g,
    severity: 'high',
    description: 'Direct __proto__ access enables prototype pollution',
    recommendation: 'Filter __proto__ from object keys'
  },
  {
    name: 'constructor_access',
    type: 'prototype_pollution',
    pattern: /\[["'`]constructor["'`]\]\s*\[["'`]prototype["'`]\]/g,
    severity: 'high',
    description: 'constructor.prototype access enables pollution',
    recommendation: 'Validate and filter object keys'
  },
  {
    name: 'lodash_merge',
    type: 'prototype_pollution',
    pattern: /(?:_|\$|lodash)\.(?:merge|defaultsDeep|set)\s*\(/g,
    severity: 'medium',
    description: 'lodash deep operations may be vulnerable',
    recommendation: 'Update to latest lodash version and validate input'
  },

  // XSS
  {
    name: 'innerhtml_assignment',
    type: 'xss',
    pattern: /\.innerHTML\s*=/g,
    severity: 'high',
    description: 'innerHTML assignment can execute injected scripts',
    recommendation: 'Use textContent or sanitize HTML with DOMPurify'
  },
  {
    name: 'document_write',
    type: 'xss',
    pattern: /document\.write\s*\(/g,
    severity: 'high',
    description: 'document.write can inject malicious content',
    recommendation: 'Use DOM manipulation methods instead'
  },
  {
    name: 'outerhtml_assignment',
    type: 'xss',
    pattern: /\.outerHTML\s*=/g,
    severity: 'high',
    description: 'outerHTML assignment can execute scripts',
    recommendation: 'Use safe DOM methods with sanitized content'
  },
  {
    name: 'insertadjacenthtml',
    type: 'xss',
    pattern: /\.insertAdjacentHTML\s*\(/g,
    severity: 'medium',
    description: 'insertAdjacentHTML can inject unsanitized content',
    recommendation: 'Sanitize HTML before insertion'
  },
  {
    name: 'dangerously_set_innerhtml',
    type: 'xss',
    pattern: /dangerouslySetInnerHTML\s*=\s*\{/g,
    severity: 'medium',
    description: 'React dangerouslySetInnerHTML bypasses XSS protection',
    recommendation: 'Sanitize content with DOMPurify before using'
  },

  // SQL Injection
  {
    name: 'string_concat_query',
    type: 'sql_injection',
    pattern: /(?:query|execute)\s*\(\s*["'`][^"'`]*\+\s*\w+/g,
    severity: 'critical',
    description: 'String concatenation in SQL query enables injection',
    recommendation: 'Use parameterized queries with placeholders'
  },
  {
    name: 'template_query',
    type: 'sql_injection',
    pattern: /(?:query|execute)\s*\(\s*`[^`]*\$\{/g,
    severity: 'critical',
    description: 'Template literal in SQL enables injection',
    recommendation: 'Use parameterized queries'
  },
  {
    name: 'raw_sql',
    type: 'sql_injection',
    pattern: /\.raw\s*\(\s*`[^`]*\$\{/g,
    severity: 'high',
    description: 'Raw SQL with interpolation is vulnerable',
    recommendation: 'Use query builder methods instead'
  },

  // Path Traversal
  {
    name: 'path_join_user_input',
    type: 'path_traversal',
    pattern: /path\.join\s*\([^)]*req\./g,
    severity: 'high',
    description: 'path.join with user input allows directory traversal',
    recommendation: 'Validate and normalize paths, check for ".."'
  },
  {
    name: 'fs_user_path',
    type: 'path_traversal',
    pattern: /fs\.(?:readFile|writeFile|unlink|rmdir)\s*\([^)]*req\./g,
    severity: 'critical',
    description: 'File system operations with user input are dangerous',
    recommendation: 'Validate paths against a whitelist of allowed directories'
  },
  {
    name: 'resolve_user_path',
    type: 'path_traversal',
    pattern: /path\.resolve\s*\([^)]*req\./g,
    severity: 'high',
    description: 'path.resolve with user input enables traversal',
    recommendation: 'Validate the resolved path stays within allowed directory'
  },

  // Regex DoS
  {
    name: 'catastrophic_backtracking',
    type: 'regex_dos',
    pattern: /new\s+RegExp\s*\([^)]*\+[^)]*\)/g,
    severity: 'medium',
    description: 'Dynamic regex may be vulnerable to ReDoS',
    recommendation: 'Validate regex complexity or use re2'
  },
  {
    name: 'nested_quantifiers',
    type: 'regex_dos',
    pattern: /\/[^/]*\([^)]*\+[^)]*\)\+[^/]*\//g,
    severity: 'medium',
    description: 'Nested quantifiers can cause exponential backtracking',
    recommendation: 'Refactor regex to avoid nested quantifiers'
  },

  // Unsafe Deserialization
  {
    name: 'json_parse_no_reviver',
    type: 'unsafe_deserialization',
    pattern: /JSON\.parse\s*\(\s*(?:req\.|body|input|data)/g,
    severity: 'medium',
    description: 'Parsing untrusted JSON without validation',
    recommendation: 'Validate JSON structure with Zod or similar'
  },
  {
    name: 'yaml_load',
    type: 'unsafe_deserialization',
    pattern: /yaml\.load\s*\(/g,
    severity: 'high',
    description: 'yaml.load can execute arbitrary code',
    recommendation: 'Use yaml.safeLoad or yaml.load with safe schema'
  },
  {
    name: 'vm_run',
    type: 'unsafe_deserialization',
    pattern: /vm\.(?:runInContext|runInNewContext|runInThisContext)\s*\(/g,
    severity: 'critical',
    description: 'vm module execution of untrusted code is dangerous',
    recommendation: 'Use vm2 or isolated-vm for sandboxing'
  }
];

/**
 * Files to exclude from scanning
 */
const EXCLUDE_PATTERNS = [
  /node_modules/,
  /\.git\//,
  /dist\//,
  /build\//,
  /\.min\.js$/,
  /\.d\.ts$/,
  /vendor\//
];

/**
 * Context patterns that indicate safe usage
 */
const SAFE_CONTEXT_PATTERNS = [
  /\/\/\s*eslint-disable.*no-eval/i,
  /\/\*\s*eslint-disable.*no-eval/i,
  /\/\/\s*security:\s*safe/i,
  /\/\/\s*nosec/i,
  /\/\/\s*@security-ignore/i
];

/**
 * Get context lines around a match
 *
 * @param {string} content - File content
 * @param {number} position - Match position
 * @param {number} lines - Number of context lines
 * @returns {string} Context lines
 */
function getContextLines(content, position, lines = 1) {
  const allLines = content.split('\n');
  let charCount = 0;
  let lineNumber = 0;

  for (let i = 0; i < allLines.length; i++) {
    charCount += allLines[i].length + 1; // +1 for newline
    if (charCount >= position) {
      lineNumber = i;
      break;
    }
  }

  const start = Math.max(0, lineNumber - lines);
  const end = Math.min(allLines.length, lineNumber + lines + 1);

  return allLines.slice(start, end).join('\n');
}

/**
 * Check if match is in a safe context
 *
 * @param {string} context - Code context
 * @returns {boolean} True if safe
 */
function isSafeContext(context) {
  for (const pattern of SAFE_CONTEXT_PATTERNS) {
    if (pattern.test(context)) {
      return true;
    }
  }
  return false;
}

/**
 * Get line number for position
 *
 * @param {string} content - File content
 * @param {number} position - Character position
 * @returns {number} Line number
 */
function getLineNumber(content, position) {
  return content.substring(0, position).split('\n').length;
}

/**
 * Scan a single file for injection vulnerabilities
 *
 * @param {string} filePath - Path to file
 * @param {Object} [options] - Scan options
 * @returns {Array<InjectionFinding>} Found vulnerabilities
 */
export function scanFile(filePath, options = {}) {
  const findings = [];
  const basePath = options.basePath || '';

  // Check exclusions
  const relativePath = basePath ? relative(basePath, filePath) : filePath;
  for (const pattern of EXCLUDE_PATTERNS) {
    if (pattern.test(relativePath)) {
      return findings;
    }
  }

  // Read file
  let content;
  try {
    content = readFileSync(filePath, 'utf-8');
  } catch {
    return findings;
  }

  // Skip binary files
  if (content.includes('\0')) {
    return findings;
  }

  // Apply all rules
  for (const rule of INJECTION_RULES) {
    const regex = new RegExp(rule.pattern.source, rule.pattern.flags);
    let match;

    while ((match = regex.exec(content)) !== null) {
      const line = getLineNumber(content, match.index);
      const context = getContextLines(content, match.index, 2);

      // Skip if in safe context
      if (isSafeContext(context)) {
        continue;
      }

      // Skip if it's a comment
      const lineContent = content.split('\n')[line - 1] || '';
      if (lineContent.trim().startsWith('//') || lineContent.trim().startsWith('*')) {
        continue;
      }

      findings.push({
        type: rule.type,
        severity: rule.severity,
        file: relativePath,
        line,
        code: match[0],
        rule: rule.name,
        description: rule.description,
        recommendation: rule.recommendation
      });
    }
  }

  return findings;
}

/**
 * Recursively get all JavaScript/TypeScript files
 *
 * @param {string} dir - Directory to scan
 * @param {Array<string>} [files] - Accumulated files
 * @returns {Promise<Array<string>>} All file paths
 */
async function getAllFiles(dir, files = []) {
  let entries;
  try {
    entries = await readdir(dir, { withFileTypes: true });
  } catch {
    return files;
  }

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);

    // Check exclusions
    let shouldSkip = false;
    for (const pattern of EXCLUDE_PATTERNS) {
      if (pattern.test(fullPath)) {
        shouldSkip = true;
        break;
      }
    }
    if (shouldSkip) continue;

    if (entry.isDirectory()) {
      await getAllFiles(fullPath, files);
    } else if (entry.isFile()) {
      const ext = extname(entry.name).toLowerCase();
      if (['.js', '.mjs', '.cjs', '.ts', '.tsx', '.jsx'].includes(ext)) {
        files.push(fullPath);
      }
    }
  }

  return files;
}

/**
 * Scan a directory for injection vulnerabilities
 *
 * @param {string} directory - Directory to scan
 * @param {Object} [options] - Scan options
 * @returns {Promise<Object>} Scan results
 */
export async function scanDirectory(directory, options = {}) {
  const startTime = Date.now();
  const allFindings = [];
  let filesScanned = 0;

  const files = await getAllFiles(directory);

  for (const file of files) {
    const findings = scanFile(file, { ...options, basePath: directory });
    allFindings.push(...findings);
    filesScanned++;
  }

  // Group by severity
  const bySeverity = {
    critical: allFindings.filter(f => f.severity === 'critical'),
    high: allFindings.filter(f => f.severity === 'high'),
    medium: allFindings.filter(f => f.severity === 'medium'),
    low: allFindings.filter(f => f.severity === 'low')
  };

  // Group by type
  const byType = {};
  for (const finding of allFindings) {
    byType[finding.type] = (byType[finding.type] || 0) + 1;
  }

  // Calculate risk score
  const riskScore = Math.min(100,
    bySeverity.critical.length * 30 +
    bySeverity.high.length * 15 +
    bySeverity.medium.length * 5 +
    bySeverity.low.length * 1
  );

  return {
    passed: bySeverity.critical.length === 0 && bySeverity.high.length === 0,
    findings: allFindings,
    summary: {
      total: allFindings.length,
      bySeverity: {
        critical: bySeverity.critical.length,
        high: bySeverity.high.length,
        medium: bySeverity.medium.length,
        low: bySeverity.low.length
      },
      byType,
      riskScore,
      filesScanned,
      scanDuration: Date.now() - startTime
    },
    metadata: {
      directory,
      timestamp: new Date().toISOString(),
      rulesApplied: INJECTION_RULES.length
    }
  };
}

/**
 * Format findings as human-readable report
 *
 * @param {Object} result - Scan result
 * @returns {string} Formatted report
 */
export function formatReport(result) {
  const lines = [];

  lines.push('='.repeat(60));
  lines.push('INJECTION VULNERABILITY REPORT');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Scan Time: ${result.metadata.timestamp}`);
  lines.push(`Directory: ${result.metadata.directory}`);
  lines.push(`Files Scanned: ${result.summary.filesScanned}`);
  lines.push(`Scan Duration: ${result.summary.scanDuration}ms`);
  lines.push('');
  lines.push('-'.repeat(60));
  lines.push('SUMMARY');
  lines.push('-'.repeat(60));
  lines.push(`Total Findings: ${result.summary.total}`);
  lines.push(`Risk Score: ${result.summary.riskScore}/100`);
  lines.push(`Status: ${result.passed ? 'PASSED' : 'FAILED'}`);
  lines.push('');
  lines.push('By Severity:');
  lines.push(`  Critical: ${result.summary.bySeverity.critical}`);
  lines.push(`  High: ${result.summary.bySeverity.high}`);
  lines.push(`  Medium: ${result.summary.bySeverity.medium}`);
  lines.push(`  Low: ${result.summary.bySeverity.low}`);

  if (Object.keys(result.summary.byType).length > 0) {
    lines.push('');
    lines.push('By Type:');
    for (const [type, count] of Object.entries(result.summary.byType)) {
      lines.push(`  ${type}: ${count}`);
    }
  }

  if (result.findings.length > 0) {
    lines.push('');
    lines.push('-'.repeat(60));
    lines.push('FINDINGS');
    lines.push('-'.repeat(60));

    for (const finding of result.findings) {
      lines.push('');
      lines.push(`[${finding.severity.toUpperCase()}] ${finding.type}`);
      lines.push(`  File: ${finding.file}:${finding.line}`);
      lines.push(`  Rule: ${finding.rule}`);
      lines.push(`  Code: ${finding.code}`);
      lines.push(`  Issue: ${finding.description}`);
      lines.push(`  Fix: ${finding.recommendation}`);
    }
  }

  lines.push('');
  lines.push('='.repeat(60));

  return lines.join('\n');
}

/**
 * Get all injection rules
 *
 * @returns {Array} All detection rules
 */
export function getRules() {
  return INJECTION_RULES.map(rule => ({
    name: rule.name,
    type: rule.type,
    severity: rule.severity,
    description: rule.description,
    recommendation: rule.recommendation
  }));
}

export default {
  scanFile,
  scanDirectory,
  formatReport,
  getRules,
  INJECTION_RULES
};
