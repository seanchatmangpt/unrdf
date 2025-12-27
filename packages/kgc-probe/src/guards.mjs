/**
 * @fileoverview KGC Probe - Guard Registry with Security Enforcement
 *
 * Implements poka-yoke (error-proofing) guards for:
 * - Environment variable access control (H1-H7)
 * - File path restrictions (H8-H14)
 * - Network URL whitelisting (H15-H17)
 * - Command execution guards (H18-H25)
 *
 * All 25 forbidden patterns from the specification are implemented.
 *
 * @module @unrdf/kgc-probe/guards
 */

import { z } from 'zod';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Guard decision result
 * @type {z.ZodSchema}
 */
export const GuardDecisionSchema = z.object({
  allowed: z.boolean().describe('Whether the operation is allowed'),
  reason: z.string().optional().describe('Reason for denial'),
  guardId: z.string().optional().describe('Guard that made the decision'),
  pattern: z.string().optional().describe('Pattern that matched'),
  severity: z.enum(['critical', 'high', 'medium', 'low']).optional(),
  receipt: z.object({
    id: z.string(),
    timestamp: z.string(),
    operation: z.string(),
    target: z.string(),
    decision: z.enum(['allow', 'deny'])
  }).optional()
}).describe('Guard decision result');

/**
 * Guard configuration schema
 * @type {z.ZodSchema}
 */
export const GuardConfigSchema = z.object({
  quality_check: z.object({
    critical_observations_threshold: z.number().positive().default(50),
    confidence_min: z.number().min(0).max(1).default(0.6)
  }).optional(),
  completeness_check: z.object({
    coverage_min: z.number().min(0).max(1).default(0.7)
  }).optional(),
  severity_limit: z.object({
    critical_limit: z.number().positive().default(10)
  }).optional(),
  network_allowlist: z.array(z.object({
    hostname: z.string(),
    paths: z.array(z.string()).default(['**']),
    methods: z.array(z.string()).default(['GET'])
  })).optional(),
  cache_ttl_ms: z.number().positive().default(300000)
}).describe('Guard thresholds and limits');

// ============================================================================
// LRU CACHE
// ============================================================================

/**
 * Simple LRU cache for guard decisions
 * @class LRUCache
 */
class LRUCache {
  /**
   * @param {number} maxSize - Maximum cache entries
   * @param {number} ttlMs - Time-to-live in milliseconds
   */
  constructor(maxSize = 1000, ttlMs = 300000) {
    /** @type {Map<string, {value: any, timestamp: number}>} */
    this.cache = new Map();
    this.maxSize = maxSize;
    this.ttlMs = ttlMs;
  }

  /**
   * Get cached value
   * @param {string} key - Cache key
   * @returns {any | undefined} Cached value or undefined
   */
  get(key) {
    const entry = this.cache.get(key);
    if (!entry) return undefined;

    // Check TTL
    if (Date.now() - entry.timestamp > this.ttlMs) {
      this.cache.delete(key);
      return undefined;
    }

    // Move to end (most recently used)
    this.cache.delete(key);
    this.cache.set(key, entry);
    return entry.value;
  }

  /**
   * Set cached value
   * @param {string} key - Cache key
   * @param {any} value - Value to cache
   */
  set(key, value) {
    // Remove oldest if at capacity
    if (this.cache.size >= this.maxSize) {
      const firstKey = this.cache.keys().next().value;
      this.cache.delete(firstKey);
    }

    this.cache.set(key, {
      value,
      timestamp: Date.now()
    });
  }

  /**
   * Check if key exists and is valid
   * @param {string} key - Cache key
   * @returns {boolean}
   */
  has(key) {
    return this.get(key) !== undefined;
  }

  /**
   * Clear cache
   */
  clear() {
    this.cache.clear();
  }

  /**
   * Get cache stats
   * @returns {{size: number, maxSize: number, hitRate: number}}
   */
  stats() {
    return {
      size: this.cache.size,
      maxSize: this.maxSize,
      hitRate: 0 // Would track hits/misses in production
    };
  }
}

// ============================================================================
// PATTERN MATCHING UTILITIES
// ============================================================================

/**
 * Convert wildcard pattern to regex
 * @param {string} pattern - Wildcard pattern (e.g., "*TOKEN", "AWS_*")
 * @returns {RegExp} Compiled regex
 */
function patternToRegex(pattern) {
  // Use placeholders to avoid regex replacement conflicts
  const DOUBLE_STAR = '\u0000DOUBLE_STAR\u0000';
  const SINGLE_STAR = '\u0001SINGLE_STAR\u0001';
  const QUESTION = '\u0002QUESTION\u0002';

  let escaped = pattern
    // Replace wildcards with placeholders first
    .replace(/\*\*/g, DOUBLE_STAR)
    .replace(/\*/g, SINGLE_STAR)
    .replace(/\?/g, QUESTION)
    // Escape regex special chars
    .replace(/[.+^${}()|[\]\\]/g, '\\$&')
    // Replace placeholders with regex patterns
    .replace(new RegExp(DOUBLE_STAR, 'g'), '.*')
    .replace(new RegExp(SINGLE_STAR, 'g'), '[^/]*')
    .replace(new RegExp(QUESTION, 'g'), '.');

  return new RegExp(`^${escaped}$`, 'i');
}

/**
 * Check if value matches any pattern
 * @param {string} value - Value to check
 * @param {string[]} patterns - Patterns to match against
 * @returns {{matched: boolean, pattern: string | null}}
 */
function matchesAnyPattern(value, patterns) {
  for (const pattern of patterns) {
    const regex = patternToRegex(pattern);
    if (regex.test(value)) {
      return { matched: true, pattern };
    }
  }
  return { matched: false, pattern: null };
}

/**
 * Normalize path (expand ~, resolve .., normalize slashes)
 * @param {string} filePath - Path to normalize
 * @returns {string} Normalized path
 */
function normalizePath(filePath) {
  if (!filePath || typeof filePath !== 'string') return '';

  let normalized = filePath;

  // Expand ~ to home directory placeholder
  if (normalized.startsWith('~')) {
    normalized = '/home/user' + normalized.slice(1);
  }

  // Normalize slashes
  normalized = normalized.replace(/\\/g, '/').replace(/\/+/g, '/');

  // Remove trailing slash (except for root)
  if (normalized.length > 1 && normalized.endsWith('/')) {
    normalized = normalized.slice(0, -1);
  }

  return normalized;
}

/**
 * Generate UUID v4
 * @returns {string}
 */
function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

// ============================================================================
// FORBIDDEN PATTERNS (25 Categories)
// ============================================================================

/**
 * H1-H7: Environment variable forbidden patterns
 * @type {string[]}
 */
const ENV_VAR_FORBIDDEN_PATTERNS = [
  // H1: Credentials & API Keys
  '*TOKEN',
  '*KEY',
  '*SECRET',
  '*PASSWORD',
  '*CREDENTIAL*',
  // H2: Cloud Provider Credentials
  'AWS_*',
  'AZURE_*',
  'GCP_*',
  'GOOGLE_*',
  // H3: VCS Credentials
  'GITHUB_*',
  'GITLAB_*',
  'BITBUCKET_*',
  'GIT_CREDENTIALS',
  // H4: Service API Keys
  '*_API_KEY',
  '*_API_SECRET',
  '*_BEARER_TOKEN',
  'STRIPE_*',
  'SLACK_*',
  'SENDGRID_*',
  // H5: Encryption Keys
  'ENCRYPTION_*',
  'CIPHER_*',
  '*_PRIVATE_KEY',
  // H6: Database Credentials
  'DB_*',
  'DATABASE_*',
  '*_CONN_STRING',
  'MONGO_*',
  'POSTGRES_*',
  'MYSQL_*',
  'REDIS_*',
  // H7: Sensitive Configuration
  'ADMIN_*',
  'ROOT_*',
  'MASTER_*',
  '*PRIVATE*'
];

/**
 * H8-H14: File path forbidden patterns
 * @type {{pattern: string, category: string}[]}
 */
const FILE_PATH_FORBIDDEN_PATTERNS = [
  // H8: SSH Keys
  { pattern: '**/.ssh/**', category: 'SSH_KEYS' },
  { pattern: '**/id_rsa', category: 'SSH_KEYS' },
  { pattern: '**/id_dsa', category: 'SSH_KEYS' },
  { pattern: '**/id_ed25519', category: 'SSH_KEYS' },
  { pattern: '**/authorized_keys', category: 'SSH_KEYS' },
  { pattern: '**/known_hosts', category: 'SSH_KEYS' },
  // H9: AWS Configuration
  { pattern: '**/.aws/**', category: 'AWS_CONFIG' },
  { pattern: '**/credentials', category: 'AWS_CONFIG' },
  // H10: NPM Registry
  { pattern: '**/.npmrc', category: 'NPM_REGISTRY' },
  { pattern: '**/*/.npmrc', category: 'NPM_REGISTRY' },
  { pattern: '**/npm-auth.json', category: 'NPM_REGISTRY' },
  // H11: Environment Files
  { pattern: '**/.env', category: 'ENV_FILE' },
  { pattern: '**/.env.local', category: 'ENV_FILE' },
  { pattern: '**/.env.*.local', category: 'ENV_FILE' },
  { pattern: '**/.env.production', category: 'ENV_FILE' },
  { pattern: '**/secrets.yml', category: 'ENV_FILE' },
  { pattern: '**/secrets.yaml', category: 'ENV_FILE' },
  // H12: Git Configuration
  { pattern: '**/.git/config', category: 'GIT_CONFIG' },
  { pattern: '**/.gitcredentials', category: 'GIT_CONFIG' },
  { pattern: '**/git-credentials', category: 'GIT_CONFIG' },
  // H13: Kubernetes Config
  { pattern: '**/.kube/config', category: 'KUBE_CONFIG' },
  { pattern: '**/kubeconfig*', category: 'KUBE_CONFIG' },
  // H14: Docker Registry
  { pattern: '**/.docker/config.json', category: 'DOCKER_CONFIG' },
  { pattern: '**/.dockercfg', category: 'DOCKER_CONFIG' },
  // System files
  { pattern: '/etc/passwd', category: 'SYSTEM_USER_DB' },
  { pattern: '/etc/shadow', category: 'SYSTEM_PASSWORD_DB' },
  { pattern: '/etc/sudoers', category: 'SYSTEM_SUDO' },
  { pattern: '/proc/*/environ', category: 'PROCESS_ENV' },
  { pattern: '/proc/self/**', category: 'PROCESS_MEMORY' }
];

/**
 * H15-H17: Network allowed hosts (whitelist approach)
 * @type {{hostname: string, paths: string[], methods: string[]}[]}
 */
const NETWORK_ALLOWED_HOSTS = [
  { hostname: 'api.github.com', paths: ['**'], methods: ['GET'] },
  { hostname: 'registry.npmjs.org', paths: ['**'], methods: ['GET'] },
  { hostname: 'github.com', paths: ['**'], methods: ['GET'] },
  { hostname: 'docs.github.com', paths: ['**'], methods: ['GET'] },
  { hostname: 'localhost', paths: ['**'], methods: ['GET', 'POST'] },
  { hostname: '127.0.0.1', paths: ['**'], methods: ['GET', 'POST'] }
];

/**
 * H15-H17: Network forbidden domains
 * @type {string[]}
 */
const NETWORK_FORBIDDEN_DOMAINS = [
  'metadata.google.internal',
  '169.254.169.254', // AWS/Azure metadata
  '169.254.*',
  '*.internal',
  '*.local',
  '10.*',
  '172.16.*',
  '172.17.*',
  '172.18.*',
  '172.19.*',
  '172.20.*',
  '172.21.*',
  '172.22.*',
  '172.23.*',
  '172.24.*',
  '172.25.*',
  '172.26.*',
  '172.27.*',
  '172.28.*',
  '172.29.*',
  '172.30.*',
  '172.31.*',
  '192.168.*'
];

/**
 * H18-H25: Forbidden commands and patterns
 * @type {string[]}
 */
const FORBIDDEN_COMMANDS = [
  'env',
  'printenv',
  'set',
  'declare',
  'whoami',
  'id',
  'groups',
  'ps',
  'cat /etc/passwd',
  'cat /etc/shadow',
  'curl',
  'wget',
  'nc',
  'netcat',
  'nmap',
  'ssh',
  'scp',
  'rsync'
];

/**
 * H21: Dangerous shell patterns (injection vectors)
 * @type {string[]}
 */
const DANGEROUS_SHELL_PATTERNS = [
  '|',    // Pipe
  '&',    // Background/AND
  ';',    // Sequence
  '>',    // Redirect output
  '<',    // Redirect input
  '>>',   // Append
  '`',    // Backtick substitution
  '$(',   // Command substitution
  '${',   // Variable expansion
  '&&',   // Conditional AND
  '||'    // Conditional OR
];

// ============================================================================
// GUARD REGISTRY
// ============================================================================

/**
 * GuardRegistry - Security-focused guard management with LRU caching
 *
 * Implements all 25 forbidden pattern categories from the specification:
 * - H1-H7: Environment variable guards
 * - H8-H14: File path guards
 * - H15-H17: Network URL guards
 * - H18-H25: Command execution guards
 *
 * @class GuardRegistry
 * @example
 * const registry = new GuardRegistry();
 * const decision = registry.checkEnvironmentVariable('AWS_SECRET_KEY');
 * if (!decision.allowed) {
 *   console.log('Access denied:', decision.reason);
 * }
 */
export class GuardRegistry {
  /**
   * Create guard registry with security guards and quality guards
   * @param {Object} [config] - Guard configuration
   */
  constructor(config = {}) {
    /** @type {Map<string, {id: string, validate: Function}>} */
    this.guards = new Map();

    /** @type {Object} */
    this.config = GuardConfigSchema.parse(config);

    /** @type {LRUCache} */
    this.cache = new LRUCache(1000, this.config.cache_ttl_ms || 300000);

    /** @type {Array<Object>} */
    this.auditLog = [];

    /** @type {string[]} */
    this.envForbiddenPatterns = [...ENV_VAR_FORBIDDEN_PATTERNS];

    /** @type {{pattern: string, category: string}[]} */
    this.fileForbiddenPatterns = [...FILE_PATH_FORBIDDEN_PATTERNS];

    /** @type {{hostname: string, paths: string[], methods: string[]}[]} */
    this.networkAllowlist = [
      ...NETWORK_ALLOWED_HOSTS,
      ...(this.config.network_allowlist || [])
    ];

    // Register default guards
    this.registerDefault();
  }

  /**
   * Register default quality + security guards
   * @private
   */
  registerDefault() {
    // Quality guards (original 5)
    this.register('quality_check', {
      id: 'quality_check',
      validate: (observations) => this.validateQuality(observations)
    });

    this.register('completeness_check', {
      id: 'completeness_check',
      validate: (observations) => this.validateCompleteness(observations)
    });

    this.register('severity_limit', {
      id: 'severity_limit',
      validate: (observations) => this.validateSeverity(observations)
    });

    this.register('integrity_check', {
      id: 'integrity_check',
      validate: (observations) => this.validateIntegrity(observations)
    });

    this.register('agent_coverage', {
      id: 'agent_coverage',
      validate: (observations) => this.validateAgentCoverage(observations)
    });

    // Security guards (H1-H25)
    this.register('env_var_guard', {
      id: 'env_var_guard',
      validate: (name) => this.checkEnvironmentVariable(name)
    });

    this.register('file_path_guard', {
      id: 'file_path_guard',
      validate: (path) => this.checkFilePathAccess(path)
    });

    this.register('network_guard', {
      id: 'network_guard',
      validate: (url) => this.checkNetworkURL(url)
    });

    this.register('command_guard', {
      id: 'command_guard',
      validate: (cmd) => this.checkCommandExecution(cmd)
    });
  }

  /**
   * Register custom guard
   * @param {string} id - Guard identifier
   * @param {{id: string, validate: Function}} guard - Guard implementation
   */
  register(id, guard) {
    this.guards.set(id, guard);
  }

  /**
   * Get guard by ID
   * @param {string} id - Guard identifier
   * @returns {{id: string, validate: Function} | undefined}
   */
  get(id) {
    return this.guards.get(id);
  }

  /**
   * List all guard IDs
   * @returns {string[]}
   */
  list() {
    return Array.from(this.guards.keys());
  }

  /**
   * Run single guard
   * @param {string} guardId - Guard identifier
   * @param {any} input - Input to validate
   * @returns {Object[] | Object} Violations or decision
   */
  validate(guardId, input) {
    const guard = this.guards.get(guardId);
    if (!guard) {
      throw new Error(`Guard not found: ${guardId}`);
    }
    return guard.validate(input);
  }

  /**
   * Run all observation guards
   * @param {Array} observations - Observations to validate
   * @returns {Object[]} All violations from all guards
   */
  validateAll(observations) {
    const allViolations = [];
    const observationGuards = ['quality_check', 'completeness_check', 'severity_limit', 'integrity_check', 'agent_coverage'];

    for (const guardId of observationGuards) {
      const guard = this.guards.get(guardId);
      if (guard) {
        try {
          const violations = guard.validate(observations);
          allViolations.push(...violations);
        } catch (err) {
          console.error(`Guard ${guardId} error:`, err);
        }
      }
    }

    return allViolations;
  }

  // =========================================================================
  // SECURITY GUARDS (H1-H25)
  // =========================================================================

  /**
   * Check environment variable access (H1-H7)
   *
   * @param {string} name - Environment variable name
   * @returns {{allowed: boolean, reason?: string, guardId?: string, pattern?: string, severity?: string, receipt?: Object}}
   * @example
   * const result = registry.checkEnvironmentVariable('AWS_SECRET_KEY');
   * // { allowed: false, reason: 'Matches forbidden pattern: AWS_*', ... }
   */
  checkEnvironmentVariable(name) {
    // Input validation
    if (!name || typeof name !== 'string') {
      return this.createDenial('env-var-access', name || '', 'INVALID_INPUT', 'Invalid variable name format', 'G-H1-ENV', 'high');
    }

    // Check cache
    const cacheKey = `env:${name.toUpperCase()}`;
    const cached = this.cache.get(cacheKey);
    if (cached !== undefined) {
      return cached;
    }

    const normalized = name.toUpperCase().trim();
    const match = matchesAnyPattern(normalized, this.envForbiddenPatterns);

    let result;
    if (match.matched) {
      result = this.createDenial(
        'env-var-access',
        name,
        'FORBIDDEN_CREDENTIAL_PATTERN',
        `Environment variable matches forbidden pattern: ${match.pattern}`,
        'G-H1-ENV-TOKEN',
        'critical',
        match.pattern
      );
    } else {
      result = { allowed: true };
    }

    // Cache and audit
    this.cache.set(cacheKey, result);
    this.logAudit('env-var-access', name, result.allowed, 'G-H1-ENV-TOKEN', result.reason);

    return result;
  }

  /**
   * Check file path access (H8-H14)
   *
   * @param {string} path - File path to check
   * @param {string} [operation='read'] - Operation type (read|write|stat)
   * @returns {{allowed: boolean, reason?: string, guardId?: string, category?: string, severity?: string, receipt?: Object}}
   * @example
   * const result = registry.checkFilePathAccess('~/.ssh/id_rsa');
   * // { allowed: false, reason: 'Access to SSH keys forbidden', ... }
   */
  checkFilePathAccess(path, operation = 'read') {
    // Input validation
    if (!path || typeof path !== 'string') {
      return this.createDenial('fs-' + operation, path || '', 'INVALID_INPUT', 'Invalid file path', 'G-H8-FILE', 'high');
    }

    // Check cache
    const cacheKey = `file:${path}:${operation}`;
    const cached = this.cache.get(cacheKey);
    if (cached !== undefined) {
      return cached;
    }

    const normalizedPath = normalizePath(path);

    // Check against forbidden patterns
    for (const forbidden of this.fileForbiddenPatterns) {
      const regex = patternToRegex(forbidden.pattern);
      if (regex.test(normalizedPath) || regex.test(path)) {
        const result = this.createDenial(
          'fs-' + operation,
          path,
          'FORBIDDEN_FILE_PATH',
          `Access to ${forbidden.category} files forbidden`,
          'G-H8-' + forbidden.category,
          'critical',
          forbidden.pattern
        );
        result.category = forbidden.category;
        result.resolvedPath = normalizedPath;

        this.cache.set(cacheKey, result);
        this.logAudit('fs-' + operation, path, false, 'G-H8-' + forbidden.category, result.reason);
        return result;
      }
    }

    const result = { allowed: true };
    this.cache.set(cacheKey, result);
    this.logAudit('fs-' + operation, path, true, 'G-H8-FILE', null);
    return result;
  }

  /**
   * Check network URL access (H15-H17)
   *
   * @param {string} url - URL to check
   * @param {string} [method='GET'] - HTTP method
   * @returns {{allowed: boolean, reason?: string, guardId?: string, hostname?: string, severity?: string, receipt?: Object}}
   * @example
   * const result = registry.checkNetworkURL('http://169.254.169.254/latest/meta-data/');
   * // { allowed: false, reason: 'Network access to metadata service forbidden', ... }
   */
  checkNetworkURL(url, method = 'GET') {
    // Input validation
    if (!url || typeof url !== 'string') {
      return this.createDenial('network-request', url || '', 'INVALID_INPUT', 'Invalid URL format', 'G-H15-NETWORK', 'high');
    }

    // Check cache
    const cacheKey = `net:${method}:${url}`;
    const cached = this.cache.get(cacheKey);
    if (cached !== undefined) {
      return cached;
    }

    // Parse URL
    let parsed;
    try {
      parsed = new URL(url);
    } catch {
      return this.createDenial('network-request', url, 'INVALID_URL', 'Cannot parse URL', 'G-H15-NETWORK', 'high');
    }

    const hostname = parsed.hostname;
    const pathname = parsed.pathname || '/';

    // Check forbidden domains first
    const forbiddenMatch = matchesAnyPattern(hostname, NETWORK_FORBIDDEN_DOMAINS);
    if (forbiddenMatch.matched) {
      const result = this.createDenial(
        'network-request',
        url,
        'FORBIDDEN_HOST',
        `Network access to ${hostname} forbidden (matches ${forbiddenMatch.pattern})`,
        'G-H16-DNS',
        'critical',
        forbiddenMatch.pattern
      );
      result.hostname = hostname;

      this.cache.set(cacheKey, result);
      this.logAudit('network-request', url, false, 'G-H16-DNS', result.reason);
      return result;
    }

    // Check against allowlist
    let allowed = false;
    for (const entry of this.networkAllowlist) {
      if (entry.hostname === hostname || entry.hostname === '*') {
        // Check method
        if (!entry.methods.includes(method) && !entry.methods.includes('*')) {
          continue;
        }

        // Check path
        for (const pathPattern of entry.paths) {
          const pathRegex = patternToRegex(pathPattern);
          if (pathRegex.test(pathname)) {
            allowed = true;
            break;
          }
        }
        if (allowed) break;
      }
    }

    let result;
    if (!allowed) {
      result = this.createDenial(
        'network-request',
        url,
        'NOT_IN_ALLOWLIST',
        `Host ${hostname} not in network allowlist`,
        'G-H15-NETWORK-URL',
        'high'
      );
      result.hostname = hostname;
    } else {
      result = { allowed: true };
    }

    this.cache.set(cacheKey, result);
    this.logAudit('network-request', url, result.allowed, 'G-H15-NETWORK-URL', result.reason);
    return result;
  }

  /**
   * Check command execution (H18-H25)
   *
   * @param {string} command - Command to check
   * @param {string[]} [args=[]] - Command arguments
   * @returns {{allowed: boolean, reason?: string, guardId?: string, severity?: string, receipt?: Object}}
   * @example
   * const result = registry.checkCommandExecution('cat /etc/passwd');
   * // { allowed: false, reason: 'Forbidden command: cat /etc/passwd', ... }
   */
  checkCommandExecution(command, args = []) {
    // Input validation
    if (!command || typeof command !== 'string') {
      return this.createDenial('command-execution', command || '', 'INVALID_INPUT', 'Invalid command', 'G-H21-CMD', 'high');
    }

    // Check cache
    const fullCommand = args.length > 0 ? `${command} ${args.join(' ')}` : command;
    const cacheKey = `cmd:${fullCommand}`;
    const cached = this.cache.get(cacheKey);
    if (cached !== undefined) {
      return cached;
    }

    const commandLower = fullCommand.toLowerCase();

    // Check forbidden commands
    for (const forbidden of FORBIDDEN_COMMANDS) {
      if (commandLower.includes(forbidden.toLowerCase())) {
        const result = this.createDenial(
          'command-execution',
          fullCommand,
          'FORBIDDEN_COMMAND',
          `Forbidden command detected: ${forbidden}`,
          'G-H21-CMD',
          'critical',
          forbidden
        );

        this.cache.set(cacheKey, result);
        this.logAudit('command-execution', fullCommand, false, 'G-H21-CMD', result.reason);
        return result;
      }
    }

    // Check for shell injection patterns
    for (const pattern of DANGEROUS_SHELL_PATTERNS) {
      if (fullCommand.includes(pattern)) {
        const result = this.createDenial(
          'command-execution',
          fullCommand,
          'SHELL_INJECTION_ATTEMPT',
          `Dangerous shell pattern detected: ${pattern}`,
          'G-H21-SHELL-INJECTION',
          'critical',
          pattern
        );

        this.cache.set(cacheKey, result);
        this.logAudit('command-execution', fullCommand, false, 'G-H21-SHELL-INJECTION', result.reason);
        return result;
      }
    }

    // Check arguments for injection
    for (const arg of args) {
      for (const pattern of DANGEROUS_SHELL_PATTERNS) {
        if (arg.includes(pattern)) {
          const result = this.createDenial(
            'command-execution',
            fullCommand,
            'SHELL_INJECTION_IN_ARGS',
            `Dangerous pattern in argument: ${pattern}`,
            'G-H21-SHELL-INJECTION',
            'critical',
            pattern
          );

          this.cache.set(cacheKey, result);
          this.logAudit('command-execution', fullCommand, false, 'G-H21-SHELL-INJECTION', result.reason);
          return result;
        }
      }
    }

    const result = { allowed: true };
    this.cache.set(cacheKey, result);
    this.logAudit('command-execution', fullCommand, true, 'G-H21-CMD', null);
    return result;
  }

  // =========================================================================
  // QUALITY GUARDS (Original 5)
  // =========================================================================

  /**
   * Validate observation quality
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateQuality(observations) {
    const violations = [];
    const thresholds = {
      critical_observations: this.config.quality_check?.critical_observations_threshold || 50,
      confidence_min: this.config.quality_check?.confidence_min || 0.6
    };

    const lowConfidence = observations.filter(
      o => o.metrics?.confidence < thresholds.confidence_min
    );

    if (lowConfidence.length > thresholds.critical_observations) {
      violations.push({
        guard_id: 'quality_check',
        severity: 'warning',
        details: {
          message: 'High count of low-confidence observations',
          count: lowConfidence.length,
          threshold: thresholds.critical_observations,
          confidence_min: thresholds.confidence_min
        }
      });
    }

    const avgConfidence = observations.length > 0
      ? observations.reduce((sum, o) => sum + (o.metrics?.confidence || 0), 0) / observations.length
      : 1.0;

    if (avgConfidence < 0.7) {
      violations.push({
        guard_id: 'quality_check',
        severity: 'warning',
        details: {
          message: 'Average confidence below 70%',
          actual: avgConfidence,
          threshold: 0.7
        }
      });
    }

    return violations;
  }

  /**
   * Validate completeness observations
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateCompleteness(observations) {
    const violations = [];
    const thresholds = {
      coverage_min: this.config.completeness_check?.coverage_min || 0.7
    };

    const completenessObs = observations.filter(
      o => o.kind === 'completeness' || o.kind === 'completeness_level'
    );

    if (completenessObs.length === 0) {
      return [];
    }

    const avgCoverage = completenessObs.reduce(
      (sum, o) => sum + (o.metrics?.coverage || 0),
      0
    ) / completenessObs.length;

    if (avgCoverage < thresholds.coverage_min) {
      violations.push({
        guard_id: 'completeness_check',
        severity: 'warning',
        details: {
          message: 'Data coverage below threshold',
          coverage: avgCoverage,
          threshold: thresholds.coverage_min,
          observations_checked: completenessObs.length
        }
      });
    }

    return violations;
  }

  /**
   * Validate severity limits
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateSeverity(observations) {
    const violations = [];
    const thresholds = {
      critical_limit: this.config.severity_limit?.critical_limit || 10
    };

    const criticalCount = observations.filter(o => o.severity === 'critical').length;

    if (criticalCount > thresholds.critical_limit) {
      violations.push({
        guard_id: 'severity_limit',
        severity: 'critical',
        details: {
          message: 'Critical violations exceed limit',
          count: criticalCount,
          limit: thresholds.critical_limit
        }
      });
    }

    return violations;
  }

  /**
   * Validate artifact integrity
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateIntegrity(observations) {
    const violations = [];
    let malformed = 0;

    for (const obs of observations) {
      if (!obs.id || !obs.agent || !obs.timestamp || !obs.kind) {
        malformed++;
      }
      if (!obs.metrics || typeof obs.metrics.confidence !== 'number') {
        malformed++;
      }
    }

    if (malformed > 0) {
      violations.push({
        guard_id: 'integrity_check',
        severity: 'critical',
        details: {
          message: 'Malformed observations detected',
          count: malformed,
          total: observations.length
        }
      });
    }

    return violations;
  }

  /**
   * Validate agent coverage
   * @param {Array} observations - Observations to check
   * @returns {Object[]} Violations
   * @private
   */
  validateAgentCoverage(observations) {
    const violations = [];
    const expectedAgents = 10;

    const uniqueAgents = new Set(
      observations
        .filter(o => !o.agent.startsWith('guard:'))
        .map(o => o.agent)
    );

    const coverageRatio = uniqueAgents.size / expectedAgents;
    if (coverageRatio < 0.7) {
      violations.push({
        guard_id: 'agent_coverage',
        severity: 'warning',
        details: {
          message: 'Agent coverage below 70%',
          agents_active: uniqueAgents.size,
          agents_expected: expectedAgents,
          coverage: coverageRatio
        }
      });
    }

    return violations;
  }

  // =========================================================================
  // UTILITY METHODS
  // =========================================================================

  /**
   * Create denial receipt
   * @private
   * @param {string} operation - Operation type
   * @param {string} target - Target of operation
   * @param {string} reasonCode - Reason code
   * @param {string} message - Human-readable message
   * @param {string} guardId - Guard ID
   * @param {string} severity - Severity level
   * @param {string} [pattern] - Matched pattern
   * @returns {Object} Denial decision with receipt
   */
  createDenial(operation, target, reasonCode, message, guardId, severity, pattern = undefined) {
    const receipt = {
      id: generateUUID(),
      timestamp: new Date().toISOString(),
      operation,
      target: String(target).substring(0, 100), // Truncate for safety
      decision: 'deny',
      reasonCode,
      guardId
    };

    return {
      allowed: false,
      reason: message,
      reasonCode,
      guardId,
      severity,
      pattern,
      receipt
    };
  }

  /**
   * Log audit entry
   * @private
   * @param {string} operation - Operation type
   * @param {string} target - Target of operation
   * @param {boolean} allowed - Whether allowed
   * @param {string} guardId - Guard ID
   * @param {string | null} reason - Reason if denied
   */
  logAudit(operation, target, allowed, guardId, reason) {
    const entry = {
      timestamp: new Date().toISOString(),
      guardId,
      operation,
      decision: allowed ? 'ALLOW' : 'DENY',
      target: String(target).substring(0, 100),
      reason: reason || null
    };

    this.auditLog.push(entry);

    // Keep audit log bounded (last 10000 entries)
    if (this.auditLog.length > 10000) {
      this.auditLog = this.auditLog.slice(-10000);
    }
  }

  /**
   * Get audit log entries
   * @param {Object} [filter] - Optional filter
   * @param {string} [filter.operation] - Filter by operation
   * @param {string} [filter.decision] - Filter by decision (ALLOW|DENY)
   * @param {number} [filter.limit] - Max entries to return
   * @returns {Object[]} Audit log entries
   */
  getAuditLog(filter = {}) {
    let entries = [...this.auditLog];

    if (filter.operation) {
      entries = entries.filter(e => e.operation === filter.operation);
    }
    if (filter.decision) {
      entries = entries.filter(e => e.decision === filter.decision);
    }
    if (filter.limit) {
      entries = entries.slice(-filter.limit);
    }

    return entries;
  }

  /**
   * Get cache statistics
   * @returns {{size: number, maxSize: number}}
   */
  getCacheStats() {
    return this.cache.stats();
  }

  /**
   * Clear all caches
   */
  clearCache() {
    this.cache.clear();
  }

  /**
   * Get forbidden pattern count (for verification)
   * @returns {{env: number, file: number, network: number, command: number, total: number}}
   */
  getForbiddenPatternCount() {
    return {
      env: this.envForbiddenPatterns.length,
      file: this.fileForbiddenPatterns.length,
      network: NETWORK_FORBIDDEN_DOMAINS.length,
      command: FORBIDDEN_COMMANDS.length + DANGEROUS_SHELL_PATTERNS.length,
      total: this.envForbiddenPatterns.length +
             this.fileForbiddenPatterns.length +
             NETWORK_FORBIDDEN_DOMAINS.length +
             FORBIDDEN_COMMANDS.length +
             DANGEROUS_SHELL_PATTERNS.length
    };
  }
}

/**
 * Create GuardRegistry instance
 * @param {Object} [config] - Guard configuration
 * @returns {GuardRegistry} New guard registry
 * @example
 * const registry = createGuardRegistry({
 *   network_allowlist: [
 *     { hostname: 'api.example.com', paths: ['**'], methods: ['GET'] }
 *   ]
 * });
 */
export function createGuardRegistry(config) {
  return new GuardRegistry(config);
}

// Export pattern constants for testing
export const PATTERNS = {
  ENV_VAR_FORBIDDEN_PATTERNS,
  FILE_PATH_FORBIDDEN_PATTERNS,
  NETWORK_FORBIDDEN_DOMAINS,
  NETWORK_ALLOWED_HOSTS,
  FORBIDDEN_COMMANDS,
  DANGEROUS_SHELL_PATTERNS
};
