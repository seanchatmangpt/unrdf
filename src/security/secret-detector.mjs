/**
 * @fileoverview Secret Detector - Find hardcoded secrets using regex and entropy analysis
 *
 * Implements Q_no_hardcoded_secrets invariant:
 * - Pattern-based detection for common secret formats
 * - Shannon entropy analysis for high-randomness strings
 * - Context-aware false positive reduction
 * - Multi-file scanning with caching
 *
 * @module security/secret-detector
 */

import { z } from 'zod';
import { readFileSync, existsSync, statSync } from 'node:fs';
import { readdir } from 'node:fs/promises';
import { join, extname, relative } from 'node:path';
import { createHash } from 'node:crypto';

/**
 * Secret detection result schema
 */
export const SecretFindingSchema = z.object({
  type: z.enum([
    'api_key',
    'password',
    'token',
    'private_key',
    'aws_key',
    'github_token',
    'jwt',
    'database_url',
    'generic_secret',
    'high_entropy'
  ]),
  severity: z.enum(['critical', 'high', 'medium', 'low']),
  file: z.string(),
  line: z.number(),
  column: z.number().optional(),
  match: z.string(),
  maskedMatch: z.string(),
  context: z.string(),
  entropy: z.number().optional(),
  confidence: z.number().min(0).max(1),
  rule: z.string()
});

/**
 * @typedef {z.infer<typeof SecretFindingSchema>} SecretFinding
 */

/**
 * Detection rules with regex patterns and metadata
 */
const DETECTION_RULES = [
  // AWS Keys
  {
    name: 'aws_access_key',
    type: 'aws_key',
    pattern: /(?:AKIA|A3T|AGPA|AIDA|AROA|AIPA|ANPA|ANVA|ASIA)[A-Z0-9]{16}/g,
    severity: 'critical',
    confidence: 0.95,
    description: 'AWS Access Key ID'
  },
  {
    name: 'aws_secret_key',
    type: 'aws_key',
    pattern: /aws_secret_access_key\s*[=:]\s*["']?([A-Za-z0-9/+=]{40})["']?/gi,
    severity: 'critical',
    confidence: 0.9,
    description: 'AWS Secret Access Key'
  },

  // GitHub Tokens
  {
    name: 'github_token',
    type: 'github_token',
    pattern: /gh[pousr]_[A-Za-z0-9_]{36,}/g,
    severity: 'critical',
    confidence: 0.98,
    description: 'GitHub Personal Access Token'
  },
  {
    name: 'github_oauth',
    type: 'github_token',
    pattern: /github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59}/g,
    severity: 'critical',
    confidence: 0.98,
    description: 'GitHub Fine-grained Personal Access Token'
  },

  // Generic API Keys
  {
    name: 'api_key_assignment',
    type: 'api_key',
    pattern: /(?:api[_-]?key|apikey)\s*[=:]\s*["']([A-Za-z0-9_\-]{16,64})["']/gi,
    severity: 'high',
    confidence: 0.8,
    description: 'API Key assignment'
  },
  {
    name: 'bearer_token',
    type: 'token',
    pattern: /bearer\s+[A-Za-z0-9_\-\.]{20,}/gi,
    severity: 'high',
    confidence: 0.85,
    description: 'Bearer token'
  },

  // Passwords
  {
    name: 'password_assignment',
    type: 'password',
    pattern: /(?:password|passwd|pwd)\s*[=:]\s*["']([^"']{8,})["']/gi,
    severity: 'high',
    confidence: 0.75,
    description: 'Password assignment'
  },
  {
    name: 'password_url',
    type: 'password',
    pattern: /[a-z]+:\/\/[^:]+:([^@]+)@/gi,
    severity: 'high',
    confidence: 0.7,
    description: 'Password in URL'
  },

  // Private Keys
  {
    name: 'private_key_pem',
    type: 'private_key',
    pattern: /-----BEGIN (?:RSA |EC |DSA |OPENSSH )?PRIVATE KEY-----/g,
    severity: 'critical',
    confidence: 0.99,
    description: 'PEM-encoded private key'
  },
  {
    name: 'private_key_content',
    type: 'private_key',
    pattern: /private[_-]?key\s*[=:]\s*["']([A-Za-z0-9+/=]{64,})["']/gi,
    severity: 'critical',
    confidence: 0.85,
    description: 'Private key content'
  },

  // JWT
  {
    name: 'jwt_token',
    type: 'jwt',
    pattern: /eyJ[A-Za-z0-9_-]{10,}\.eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}/g,
    severity: 'high',
    confidence: 0.95,
    description: 'JWT token'
  },

  // Database URLs
  {
    name: 'database_url',
    type: 'database_url',
    pattern: /(?:mongodb|postgres|mysql|redis|amqp):\/\/[^:]+:[^@]+@[^\s"']+/gi,
    severity: 'critical',
    confidence: 0.9,
    description: 'Database connection string with credentials'
  },

  // Generic Secrets
  {
    name: 'secret_assignment',
    type: 'generic_secret',
    pattern: /(?:secret|token|credential)\s*[=:]\s*["']([A-Za-z0-9_\-]{16,})["']/gi,
    severity: 'medium',
    confidence: 0.6,
    description: 'Generic secret assignment'
  },

  // Slack
  {
    name: 'slack_token',
    type: 'token',
    pattern: /xox[baprs]-[0-9]{10,}-[0-9]+-[a-zA-Z0-9]+/g,
    severity: 'high',
    confidence: 0.95,
    description: 'Slack token'
  },

  // Stripe
  {
    name: 'stripe_key',
    type: 'api_key',
    pattern: /sk_(?:live|test)_[A-Za-z0-9]{24,}/g,
    severity: 'critical',
    confidence: 0.98,
    description: 'Stripe API key'
  },

  // SendGrid
  {
    name: 'sendgrid_key',
    type: 'api_key',
    pattern: /SG\.[A-Za-z0-9_-]{22}\.[A-Za-z0-9_-]{43}/g,
    severity: 'high',
    confidence: 0.95,
    description: 'SendGrid API key'
  },

  // NPM
  {
    name: 'npm_token',
    type: 'token',
    pattern: /npm_[A-Za-z0-9]{36}/g,
    severity: 'high',
    confidence: 0.95,
    description: 'NPM access token'
  }
];

/**
 * Files and paths to ignore
 */
const IGNORE_PATTERNS = [
  /node_modules/,
  /\.git\//,
  /dist\//,
  /build\//,
  /coverage\//,
  /\.min\.js$/,
  /\.map$/,
  /package-lock\.json$/,
  /pnpm-lock\.yaml$/,
  /yarn\.lock$/,
  /\.test\.(js|mjs|ts)$/,
  /\.spec\.(js|mjs|ts)$/,
  /__tests__\//,
  /fixtures?\//i,
  /mock[s]?\//i,
  /\.md$/
];

/**
 * Known false positive patterns
 */
const FALSE_POSITIVE_PATTERNS = [
  /example/i,
  /placeholder/i,
  /your[_-]?api[_-]?key/i,
  /xxx+/i,
  /test[_-]?key/i,
  /dummy/i,
  /sample/i,
  /demo/i,
  /fake/i,
  /<[A-Z_]+>/,  // Placeholder tokens like <API_KEY>
  /\$\{[^}]+\}/, // Template variables
  /process\.env\./,
  /import\.meta\.env\./
];

/**
 * Calculate Shannon entropy of a string
 * Higher entropy = more random = more likely to be a real secret
 *
 * @param {string} str - String to analyze
 * @returns {number} Shannon entropy (bits per character)
 */
export function calculateEntropy(str) {
  if (!str || str.length === 0) return 0;

  const freq = new Map();
  for (const char of str) {
    freq.set(char, (freq.get(char) || 0) + 1);
  }

  let entropy = 0;
  for (const count of freq.values()) {
    const p = count / str.length;
    entropy -= p * Math.log2(p);
  }

  return entropy;
}

/**
 * Mask sensitive data for safe logging
 *
 * @param {string} secret - Secret to mask
 * @returns {string} Masked secret
 */
export function maskSecret(secret) {
  if (!secret || secret.length < 8) {
    return '***REDACTED***';
  }

  const showChars = Math.min(4, Math.floor(secret.length / 4));
  const prefix = secret.substring(0, showChars);
  const suffix = secret.substring(secret.length - showChars);
  const masked = '*'.repeat(Math.max(8, secret.length - showChars * 2));

  return `${prefix}${masked}${suffix}`;
}

/**
 * Check if a match is likely a false positive
 *
 * @param {string} match - Matched string
 * @param {string} context - Surrounding code context
 * @returns {boolean} True if likely false positive
 */
function isFalsePositive(match, context) {
  // Check against false positive patterns
  for (const pattern of FALSE_POSITIVE_PATTERNS) {
    if (pattern.test(match) || pattern.test(context)) {
      return true;
    }
  }

  // Check for common test/example values
  const lowerMatch = match.toLowerCase();
  if (
    lowerMatch.includes('example') ||
    lowerMatch.includes('test') ||
    lowerMatch.includes('sample') ||
    lowerMatch.includes('demo') ||
    /^[a-z]+$/.test(match) || // Only lowercase letters
    /^[0-9]+$/.test(match) || // Only digits
    /^x+$/.test(lowerMatch) // Only x's
  ) {
    return true;
  }

  return false;
}

/**
 * Extract context around a match
 *
 * @param {string} content - File content
 * @param {number} matchStart - Start index of match
 * @param {number} contextChars - Characters of context to include
 * @returns {string} Context string
 */
function extractContext(content, matchStart, contextChars = 50) {
  const start = Math.max(0, matchStart - contextChars);
  const end = Math.min(content.length, matchStart + contextChars);
  let context = content.substring(start, end);

  // Trim to nearest word boundary if possible
  if (start > 0) {
    const firstSpace = context.indexOf(' ');
    if (firstSpace > 0 && firstSpace < 10) {
      context = '...' + context.substring(firstSpace + 1);
    }
  }

  if (end < content.length) {
    const lastSpace = context.lastIndexOf(' ');
    if (lastSpace > context.length - 10) {
      context = context.substring(0, lastSpace) + '...';
    }
  }

  return context.replace(/\n/g, ' ').trim();
}

/**
 * Get line number and column for a position in content
 *
 * @param {string} content - File content
 * @param {number} position - Character position
 * @returns {{line: number, column: number}} Line and column info
 */
function getLineInfo(content, position) {
  const beforeMatch = content.substring(0, position);
  const lines = beforeMatch.split('\n');
  const line = lines.length;
  const column = lines[lines.length - 1].length + 1;
  return { line, column };
}

/**
 * Scan a single file for secrets
 *
 * @param {string} filePath - Path to file
 * @param {Object} [options] - Scan options
 * @returns {Array<SecretFinding>} Found secrets
 */
export function scanFile(filePath, options = {}) {
  const {
    entropyThreshold = 4.5,
    minSecretLength = 16,
    includeHighEntropy = true
  } = options;

  const findings = [];

  // Check if file should be ignored
  const relativePath = options.basePath
    ? relative(options.basePath, filePath)
    : filePath;

  for (const pattern of IGNORE_PATTERNS) {
    if (pattern.test(relativePath)) {
      return findings;
    }
  }

  // Read file content
  let content;
  try {
    content = readFileSync(filePath, 'utf-8');
  } catch (error) {
    // Skip files that can't be read
    return findings;
  }

  // Skip binary files
  if (content.includes('\0')) {
    return findings;
  }

  // Apply detection rules
  for (const rule of DETECTION_RULES) {
    const regex = new RegExp(rule.pattern.source, rule.pattern.flags);
    let match;

    while ((match = regex.exec(content)) !== null) {
      const matchedStr = match[1] || match[0];
      const { line, column } = getLineInfo(content, match.index);
      const context = extractContext(content, match.index);

      // Skip false positives
      if (isFalsePositive(matchedStr, context)) {
        continue;
      }

      const entropy = calculateEntropy(matchedStr);

      // Adjust confidence based on entropy
      let adjustedConfidence = rule.confidence;
      if (entropy > 4) {
        adjustedConfidence = Math.min(0.99, adjustedConfidence + 0.1);
      } else if (entropy < 3) {
        adjustedConfidence = Math.max(0.1, adjustedConfidence - 0.2);
      }

      findings.push({
        type: rule.type,
        severity: rule.severity,
        file: relativePath,
        line,
        column,
        match: matchedStr,
        maskedMatch: maskSecret(matchedStr),
        context: maskSecret(context),
        entropy,
        confidence: adjustedConfidence,
        rule: rule.name
      });
    }
  }

  // High entropy string detection
  if (includeHighEntropy) {
    // Find quoted strings
    const stringPattern = /["'`]([A-Za-z0-9+/=_\-]{20,})["'`]/g;
    let match;

    while ((match = stringPattern.exec(content)) !== null) {
      const str = match[1];
      const entropy = calculateEntropy(str);

      if (entropy >= entropyThreshold && str.length >= minSecretLength) {
        const { line, column } = getLineInfo(content, match.index);
        const context = extractContext(content, match.index);

        // Skip if already found by specific rules
        const isDuplicate = findings.some(f =>
          f.line === line && f.match === str
        );
        if (isDuplicate) continue;

        // Skip false positives
        if (isFalsePositive(str, context)) continue;

        // Skip base64-encoded common values
        try {
          const decoded = Buffer.from(str, 'base64').toString('utf-8');
          if (/^[\x20-\x7E]+$/.test(decoded) && decoded.length > 10) {
            // Likely just base64 encoded text, not a secret
            continue;
          }
        } catch {
          // Not valid base64, continue
        }

        findings.push({
          type: 'high_entropy',
          severity: 'medium',
          file: relativePath,
          line,
          column,
          match: str,
          maskedMatch: maskSecret(str),
          context: maskSecret(context),
          entropy,
          confidence: Math.min(0.9, (entropy - 3) / 3),
          rule: 'high_entropy_string'
        });
      }
    }
  }

  return findings;
}

/**
 * Recursively get all files in a directory
 *
 * @param {string} dir - Directory path
 * @param {Array<string>} [files] - Accumulated files
 * @returns {Promise<Array<string>>} All file paths
 */
async function getAllFiles(dir, files = []) {
  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);

    // Skip ignored directories
    let shouldSkip = false;
    for (const pattern of IGNORE_PATTERNS) {
      if (pattern.test(fullPath)) {
        shouldSkip = true;
        break;
      }
    }
    if (shouldSkip) continue;

    if (entry.isDirectory()) {
      await getAllFiles(fullPath, files);
    } else if (entry.isFile()) {
      // Only scan code files
      const ext = extname(entry.name).toLowerCase();
      const codeExtensions = [
        '.js', '.mjs', '.cjs', '.ts', '.tsx', '.jsx',
        '.json', '.yaml', '.yml', '.toml', '.ini',
        '.env', '.sh', '.bash', '.zsh',
        '.py', '.rb', '.go', '.rs', '.java'
      ];
      if (codeExtensions.includes(ext) || entry.name.startsWith('.env')) {
        files.push(fullPath);
      }
    }
  }

  return files;
}

/**
 * Scan an entire directory for secrets
 *
 * @param {string} directory - Directory to scan
 * @param {Object} [options] - Scan options
 * @returns {Promise<Object>} Scan results
 */
export async function scanDirectory(directory, options = {}) {
  const startTime = Date.now();
  const allFindings = [];
  let filesScanned = 0;
  let filesSkipped = 0;

  const files = await getAllFiles(directory);

  for (const file of files) {
    try {
      const stat = statSync(file);
      if (stat.size > 1024 * 1024) { // Skip files > 1MB
        filesSkipped++;
        continue;
      }

      const findings = scanFile(file, { ...options, basePath: directory });
      allFindings.push(...findings);
      filesScanned++;
    } catch (error) {
      filesSkipped++;
    }
  }

  // Group by severity
  const bySeverity = {
    critical: allFindings.filter(f => f.severity === 'critical'),
    high: allFindings.filter(f => f.severity === 'high'),
    medium: allFindings.filter(f => f.severity === 'medium'),
    low: allFindings.filter(f => f.severity === 'low')
  };

  // Calculate risk score (0-100)
  const riskScore = Math.min(100,
    bySeverity.critical.length * 25 +
    bySeverity.high.length * 10 +
    bySeverity.medium.length * 3 +
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
      byType: groupBy(allFindings, 'type'),
      riskScore,
      filesScanned,
      filesSkipped,
      scanDuration: Date.now() - startTime
    },
    metadata: {
      directory,
      timestamp: new Date().toISOString(),
      rulesApplied: DETECTION_RULES.length,
      entropyThreshold: options.entropyThreshold || 4.5
    }
  };
}

/**
 * Group array by property
 *
 * @param {Array} array - Array to group
 * @param {string} key - Property to group by
 * @returns {Object} Grouped counts
 */
function groupBy(array, key) {
  return array.reduce((acc, item) => {
    const value = item[key];
    acc[value] = (acc[value] || 0) + 1;
    return acc;
  }, {});
}

/**
 * Generate hash of scan configuration for caching
 *
 * @param {Object} options - Scan options
 * @returns {string} Configuration hash
 */
export function getConfigHash(options = {}) {
  const config = {
    rulesVersion: '1.0.0',
    entropyThreshold: options.entropyThreshold || 4.5,
    includeHighEntropy: options.includeHighEntropy !== false
  };
  return createHash('sha256').update(JSON.stringify(config)).digest('hex').slice(0, 16);
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
  lines.push('SECRET DETECTION REPORT');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Scan Time: ${result.metadata.timestamp}`);
  lines.push(`Directory: ${result.metadata.directory}`);
  lines.push(`Files Scanned: ${result.summary.filesScanned}`);
  lines.push(`Files Skipped: ${result.summary.filesSkipped}`);
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
      lines.push(`  Match: ${finding.maskedMatch}`);
      lines.push(`  Entropy: ${finding.entropy?.toFixed(2) || 'N/A'}`);
      lines.push(`  Confidence: ${(finding.confidence * 100).toFixed(0)}%`);
    }
  }

  lines.push('');
  lines.push('='.repeat(60));

  return lines.join('\n');
}

export default {
  scanFile,
  scanDirectory,
  calculateEntropy,
  maskSecret,
  formatReport,
  getConfigHash,
  DETECTION_RULES
};
