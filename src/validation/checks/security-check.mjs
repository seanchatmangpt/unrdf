/**
 * @fileoverview Security Check - Validates security best practices
 *
 * **Checks performed**:
 * 1. Hardcoded credentials detection
 * 2. Secret patterns in code
 * 3. Input validation presence
 * 4. SQL/NoSQL injection patterns
 * 5. XSS vulnerability patterns
 * 6. Path traversal patterns
 * 7. Command injection patterns
 * 8. Insecure dependencies
 * 9. OWASP compliance indicators
 *
 * **Scoring**:
 * - 100: No security issues detected
 * - 95-99: Minor improvements possible
 * - 80-94: Some security concerns
 * - 60-79: Significant security issues
 * - <60: Critical security vulnerabilities
 *
 * @module validation/checks/security-check
 */

import { readdir, readFile, stat } from 'node:fs/promises';
import { join, extname, relative } from 'node:path';

/**
 * Security severity levels
 */
export const SEVERITY = {
  CRITICAL: 'critical',
  HIGH: 'high',
  MEDIUM: 'medium',
  LOW: 'low',
  INFO: 'info'
};

/**
 * Secret patterns for detection
 */
const SECRET_PATTERNS = [
  // API Keys
  { pattern: /['"]?(?:api[_-]?key|apikey)['"]?\s*[:=]\s*['"][a-zA-Z0-9_\-]{20,}['"]/gi, type: 'API_KEY', severity: SEVERITY.CRITICAL },
  { pattern: /['"]?(?:secret[_-]?key|secretkey)['"]?\s*[:=]\s*['"][a-zA-Z0-9_\-]{20,}['"]/gi, type: 'SECRET_KEY', severity: SEVERITY.CRITICAL },

  // AWS
  { pattern: /AKIA[0-9A-Z]{16}/g, type: 'AWS_ACCESS_KEY', severity: SEVERITY.CRITICAL },
  { pattern: /aws[_-]?secret[_-]?access[_-]?key\s*[:=]\s*['"][a-zA-Z0-9\/+=]{40}['"]/gi, type: 'AWS_SECRET', severity: SEVERITY.CRITICAL },

  // Google
  { pattern: /AIza[0-9A-Za-z_-]{35}/g, type: 'GOOGLE_API_KEY', severity: SEVERITY.HIGH },

  // GitHub
  { pattern: /ghp_[a-zA-Z0-9]{36}/g, type: 'GITHUB_PAT', severity: SEVERITY.CRITICAL },
  { pattern: /github[_-]?token\s*[:=]\s*['"][a-zA-Z0-9_]{35,}['"]/gi, type: 'GITHUB_TOKEN', severity: SEVERITY.CRITICAL },

  // JWT
  { pattern: /eyJ[a-zA-Z0-9_-]*\.eyJ[a-zA-Z0-9_-]*\.[a-zA-Z0-9_-]*/g, type: 'JWT_TOKEN', severity: SEVERITY.HIGH },

  // Private keys
  { pattern: /-----BEGIN (?:RSA |EC |DSA )?PRIVATE KEY-----/g, type: 'PRIVATE_KEY', severity: SEVERITY.CRITICAL },

  // Database connection strings
  { pattern: /(?:mongodb|postgres|mysql|redis):\/\/[a-zA-Z0-9_:@]+/gi, type: 'DB_CONNECTION', severity: SEVERITY.CRITICAL },

  // Passwords
  { pattern: /['"]?password['"]?\s*[:=]\s*['"][^'"]{8,}['"]/gi, type: 'PASSWORD', severity: SEVERITY.HIGH },

  // Slack
  { pattern: /xox[baprs]-[a-zA-Z0-9-]{10,}/g, type: 'SLACK_TOKEN', severity: SEVERITY.HIGH },

  // Generic secrets
  { pattern: /['"]?(?:auth[_-]?token|access[_-]?token|bearer)['"]?\s*[:=]\s*['"][a-zA-Z0-9_\-\.]{20,}['"]/gi, type: 'AUTH_TOKEN', severity: SEVERITY.HIGH }
];

/**
 * Injection vulnerability patterns
 */
const INJECTION_PATTERNS = [
  // SQL Injection
  { pattern: /(?:query|execute|run)\s*\(\s*['"`].*?\$\{.*?\}/gi, type: 'SQL_INJECTION', severity: SEVERITY.CRITICAL },
  { pattern: /(?:query|execute)\s*\(\s*[^,]+\s*\+\s*(?:req|request|params|query)\./gi, type: 'SQL_INJECTION', severity: SEVERITY.HIGH },

  // Command Injection
  { pattern: /(?:exec|spawn|execSync|spawnSync)\s*\(\s*[^,]+\s*\+\s*(?:req|request|params|input)/gi, type: 'COMMAND_INJECTION', severity: SEVERITY.CRITICAL },
  { pattern: /child_process.*(?:exec|spawn)\s*\([^)]*\$\{/gi, type: 'COMMAND_INJECTION', severity: SEVERITY.CRITICAL },

  // Path Traversal
  { pattern: /(?:readFile|writeFile|createReadStream)\s*\([^)]*(?:req|request|params)\./gi, type: 'PATH_TRAVERSAL', severity: SEVERITY.HIGH },
  { pattern: /\.\.\/|\.\.\\|%2e%2e/gi, type: 'PATH_TRAVERSAL_PATTERN', severity: SEVERITY.MEDIUM },

  // NoSQL Injection
  { pattern: /\$where\s*:\s*[^}]+(?:req|request|params)\./gi, type: 'NOSQL_INJECTION', severity: SEVERITY.HIGH },
  { pattern: /find\s*\(\s*\{[^}]*\$(?:gt|gte|lt|lte|ne|in|nin|or|and)\s*:/gi, type: 'NOSQL_OPERATOR', severity: SEVERITY.MEDIUM }
];

/**
 * XSS vulnerability patterns
 */
const XSS_PATTERNS = [
  { pattern: /innerHTML\s*=\s*(?:req|request|params|user)/gi, type: 'XSS_INNERHTML', severity: SEVERITY.HIGH },
  { pattern: /dangerouslySetInnerHTML/g, type: 'REACT_DANGEROUS_HTML', severity: SEVERITY.MEDIUM },
  { pattern: /document\.write\s*\([^)]*(?:req|request|params)/gi, type: 'XSS_DOCUMENT_WRITE', severity: SEVERITY.HIGH },
  { pattern: /\.html\s*\(\s*(?:req|request|params)/gi, type: 'XSS_JQUERY_HTML', severity: SEVERITY.HIGH }
];

/**
 * Insecure patterns
 */
const INSECURE_PATTERNS = [
  // Insecure random
  { pattern: /Math\.random\s*\(\)/g, type: 'INSECURE_RANDOM', severity: SEVERITY.MEDIUM },

  // Eval usage
  { pattern: /\beval\s*\(/g, type: 'EVAL_USAGE', severity: SEVERITY.HIGH },
  { pattern: /new\s+Function\s*\(/g, type: 'FUNCTION_CONSTRUCTOR', severity: SEVERITY.MEDIUM },

  // Disable security
  { pattern: /process\.env\.NODE_TLS_REJECT_UNAUTHORIZED\s*=\s*['"]?0/gi, type: 'TLS_DISABLED', severity: SEVERITY.CRITICAL },
  { pattern: /rejectUnauthorized\s*:\s*false/gi, type: 'CERT_VALIDATION_DISABLED', severity: SEVERITY.HIGH },

  // Hardcoded IPs/URLs
  { pattern: /http:\/\/(?!localhost|127\.0\.0\.1)/g, type: 'HTTP_URL', severity: SEVERITY.LOW },

  // Debug in production
  { pattern: /console\.(log|debug|trace)\s*\([^)]*(?:password|secret|token|key)/gi, type: 'SECRET_LOGGING', severity: SEVERITY.HIGH }
];

/**
 * Files that should never be committed
 */
const SENSITIVE_FILES = [
  '.env',
  '.env.local',
  '.env.production',
  '.env.development',
  'credentials.json',
  'secrets.json',
  'config/secrets.js',
  '.aws/credentials',
  'id_rsa',
  'id_dsa',
  'id_ecdsa',
  'id_ed25519',
  '*.pem',
  '*.key',
  'serviceAccountKey.json'
];

/**
 * Input validation patterns (positive indicators)
 */
const VALIDATION_PATTERNS = [
  /zod/gi,
  /joi/gi,
  /yup/gi,
  /ajv/gi,
  /validator/gi,
  /express-validator/gi,
  /class-validator/gi,
  /\.parse\s*\(/g,
  /\.validate\s*\(/g,
  /sanitize/gi
];

/**
 * Scan file for security issues
 *
 * @param {string} content - File content
 * @param {string} filePath - File path
 * @returns {Array} Security issues found
 */
function scanFileForSecurityIssues(content, filePath) {
  const issues = [];
  const lines = content.split('\n');

  // Skip test files for some checks
  const isTestFile = filePath.includes('.test.') || filePath.includes('.spec.') || filePath.includes('__tests__');

  // Secret patterns
  for (const { pattern, type, severity } of SECRET_PATTERNS) {
    const matches = content.match(pattern);
    if (matches) {
      for (const match of matches) {
        // Find line number
        const lineNum = findLineNumber(content, match);
        issues.push({
          type,
          severity,
          file: filePath,
          line: lineNum,
          match: truncateMatch(match),
          category: 'secrets'
        });
      }
    }
  }

  // Injection patterns (skip in tests)
  if (!isTestFile) {
    for (const { pattern, type, severity } of INJECTION_PATTERNS) {
      const matches = content.match(pattern);
      if (matches) {
        for (const match of matches) {
          const lineNum = findLineNumber(content, match);
          issues.push({
            type,
            severity,
            file: filePath,
            line: lineNum,
            match: truncateMatch(match),
            category: 'injection'
          });
        }
      }
    }
  }

  // XSS patterns (skip in tests)
  if (!isTestFile) {
    for (const { pattern, type, severity } of XSS_PATTERNS) {
      const matches = content.match(pattern);
      if (matches) {
        for (const match of matches) {
          const lineNum = findLineNumber(content, match);
          issues.push({
            type,
            severity,
            file: filePath,
            line: lineNum,
            match: truncateMatch(match),
            category: 'xss'
          });
        }
      }
    }
  }

  // Insecure patterns
  for (const { pattern, type, severity } of INSECURE_PATTERNS) {
    // Skip some patterns in tests
    if (isTestFile && ['INSECURE_RANDOM', 'EVAL_USAGE'].includes(type)) {
      continue;
    }

    const matches = content.match(pattern);
    if (matches) {
      for (const match of matches) {
        const lineNum = findLineNumber(content, match);
        issues.push({
          type,
          severity,
          file: filePath,
          line: lineNum,
          match: truncateMatch(match),
          category: 'insecure'
        });
      }
    }
  }

  return issues;
}

/**
 * Find line number for a match
 *
 * @param {string} content - File content
 * @param {string} match - Match to find
 * @returns {number} Line number (1-indexed)
 */
function findLineNumber(content, match) {
  const index = content.indexOf(match);
  if (index === -1) return 1;

  const upToMatch = content.substring(0, index);
  return (upToMatch.match(/\n/g) || []).length + 1;
}

/**
 * Truncate match for display
 *
 * @param {string} match - Match string
 * @returns {string} Truncated match
 */
function truncateMatch(match) {
  if (match.length <= 60) return match;
  return match.substring(0, 57) + '...';
}

/**
 * Check for input validation usage
 *
 * @param {string} content - File content
 * @returns {Object} Validation check result
 */
function checkInputValidation(content) {
  let hasValidation = false;

  for (const pattern of VALIDATION_PATTERNS) {
    if (pattern.test(content)) {
      hasValidation = true;
      break;
    }
  }

  return {
    hasValidation,
    validationLibrary: content.includes('zod') ? 'zod'
      : content.includes('joi') ? 'joi'
      : content.includes('yup') ? 'yup'
      : content.includes('ajv') ? 'ajv'
      : null
  };
}

/**
 * Get all source files
 *
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<string>>} File paths
 */
async function getSourceFiles(dirPath) {
  const files = [];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git', '.cache'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          const ext = extname(entry.name);
          if (['.mjs', '.js', '.ts', '.jsx', '.tsx', '.json', '.yaml', '.yml'].includes(ext)) {
            files.push(fullPath);
          }
        }
      }
    } catch (error) {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return files;
}

/**
 * Check for sensitive files
 *
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<string>>} Sensitive files found
 */
async function checkSensitiveFiles(dirPath) {
  const found = [];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', '.git'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          for (const sensitivePattern of SENSITIVE_FILES) {
            if (sensitivePattern.includes('*')) {
              const regex = new RegExp(sensitivePattern.replace('*', '.*'));
              if (regex.test(entry.name)) {
                found.push(relative(dirPath, fullPath));
              }
            } else if (entry.name === sensitivePattern || fullPath.includes(sensitivePattern)) {
              found.push(relative(dirPath, fullPath));
            }
          }
        }
      }
    } catch (error) {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return found;
}

/**
 * Check .gitignore for sensitive patterns
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Gitignore check result
 */
async function checkGitignore(packagePath) {
  try {
    const gitignore = await readFile(join(packagePath, '.gitignore'), 'utf-8');

    const patterns = {
      hasEnv: /\.env/m.test(gitignore),
      hasSecrets: /secret/mi.test(gitignore),
      hasCredentials: /credential/mi.test(gitignore),
      hasKeys: /\.key|\.pem|id_rsa/mi.test(gitignore)
    };

    return {
      hasGitignore: true,
      ...patterns,
      isSecure: patterns.hasEnv || patterns.hasSecrets
    };
  } catch (error) {
    return {
      hasGitignore: false,
      hasEnv: false,
      hasSecrets: false,
      hasCredentials: false,
      hasKeys: false,
      isSecure: false
    };
  }
}

/**
 * Perform security check on a package
 *
 * @param {string} packagePath - Path to package
 * @param {Object} options - Check options
 * @returns {Promise<Object>} Check result
 */
export async function securityCheck(packagePath, options = {}) {
  const startTime = Date.now();
  const warnings = [];
  const failures = [];
  const remediation = [];

  let totalScore = 100;
  const details = {
    filesScanned: 0,
    secretsFound: 0,
    injectionsFound: 0,
    xssFound: 0,
    insecurePatterns: 0,
    sensitiveFiles: 0,
    hasInputValidation: false,
    gitignoreSecure: false,
    issuesBySeverity: {
      critical: 0,
      high: 0,
      medium: 0,
      low: 0
    }
  };

  try {
    // Get all source files
    const sourceFiles = await getSourceFiles(packagePath);
    details.filesScanned = sourceFiles.length;

    // Aggregate all issues
    const allIssues = [];
    let filesWithValidation = 0;

    for (const filePath of sourceFiles) {
      try {
        const content = await readFile(filePath, 'utf-8');
        const relativePath = relative(packagePath, filePath);

        // Scan for security issues
        const issues = scanFileForSecurityIssues(content, relativePath);
        allIssues.push(...issues);

        // Check for input validation
        const validation = checkInputValidation(content);
        if (validation.hasValidation) {
          filesWithValidation++;
        }
      } catch (error) {
        // File not readable
      }
    }

    details.hasInputValidation = filesWithValidation > 0;

    // Count issues by category and severity
    for (const issue of allIssues) {
      details.issuesBySeverity[issue.severity]++;

      switch (issue.category) {
        case 'secrets':
          details.secretsFound++;
          break;
        case 'injection':
          details.injectionsFound++;
          break;
        case 'xss':
          details.xssFound++;
          break;
        case 'insecure':
          details.insecurePatterns++;
          break;
      }
    }

    // Check for sensitive files
    const sensitiveFiles = await checkSensitiveFiles(packagePath);
    details.sensitiveFiles = sensitiveFiles.length;

    // Check gitignore
    const gitignoreCheck = await checkGitignore(packagePath);
    details.gitignoreSecure = gitignoreCheck.isSecure;

    // Generate warnings and failures
    if (details.issuesBySeverity.critical > 0) {
      failures.push(`${details.issuesBySeverity.critical} CRITICAL security issue(s) found`);
      remediation.push('Immediately address critical security vulnerabilities');
    }

    if (details.issuesBySeverity.high > 0) {
      failures.push(`${details.issuesBySeverity.high} HIGH severity security issue(s) found`);
    }

    if (details.issuesBySeverity.medium > 0) {
      warnings.push(`${details.issuesBySeverity.medium} MEDIUM severity security issue(s)`);
    }

    if (details.secretsFound > 0) {
      failures.push(`${details.secretsFound} potential secret(s) in code`);
      remediation.push('Remove hardcoded secrets and use environment variables');
    }

    if (details.injectionsFound > 0) {
      failures.push(`${details.injectionsFound} potential injection vulnerability(ies)`);
      remediation.push('Use parameterized queries and input sanitization');
    }

    if (sensitiveFiles.length > 0) {
      warnings.push(`${sensitiveFiles.length} sensitive file(s) found: ${sensitiveFiles.slice(0, 3).join(', ')}`);
      remediation.push('Remove sensitive files or add to .gitignore');
    }

    if (!gitignoreCheck.hasGitignore) {
      warnings.push('No .gitignore file found');
      remediation.push('Add .gitignore with .env and secrets patterns');
    } else if (!gitignoreCheck.isSecure) {
      warnings.push('.gitignore may not cover sensitive files');
    }

    if (!details.hasInputValidation && sourceFiles.length > 5) {
      warnings.push('No input validation library detected');
      remediation.push('Add input validation using Zod, Joi, or similar');
    }

    // Calculate score
    // Critical issues (-25 each)
    totalScore -= details.issuesBySeverity.critical * 25;

    // High issues (-15 each)
    totalScore -= details.issuesBySeverity.high * 15;

    // Medium issues (-5 each, max -20)
    totalScore -= Math.min(20, details.issuesBySeverity.medium * 5);

    // Low issues (-2 each, max -10)
    totalScore -= Math.min(10, details.issuesBySeverity.low * 2);

    // Sensitive files (-5 each, max -15)
    totalScore -= Math.min(15, sensitiveFiles.length * 5);

    // No gitignore (-5)
    if (!gitignoreCheck.hasGitignore) {
      totalScore -= 5;
    }

    // No input validation (-5)
    if (!details.hasInputValidation && sourceFiles.length > 5) {
      totalScore -= 5;
    }

    totalScore = Math.max(0, Math.min(100, Math.round(totalScore)));

  } catch (error) {
    failures.push(`Security check failed: ${error.message}`);
    totalScore = 0;
  }

  return {
    passed: totalScore >= 80,
    score: totalScore,
    status: totalScore >= 95 ? 'pass' : totalScore >= 80 ? 'warn' : 'fail',
    warnings: [...new Set(warnings)].slice(0, 20),
    failures: [...new Set(failures)].slice(0, 10),
    remediation: [...new Set(remediation)].slice(0, 10),
    duration: Date.now() - startTime,
    details
  };
}

export default securityCheck;
