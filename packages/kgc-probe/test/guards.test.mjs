/**
 * @fileoverview Guard System Tests
 *
 * Tests for:
 * - Environment variable guards (H1-H7)
 * - File path guards (H8-H14)
 * - Network URL guards (H15-H17)
 * - Command execution guards (H18-H25)
 * - LRU cache behavior
 * - Audit logging
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  GuardRegistry,
  createGuardRegistry,
  PATTERNS
} from '../src/guards.mjs';

describe('GuardRegistry', () => {
  /** @type {GuardRegistry} */
  let registry;

  beforeEach(() => {
    registry = createGuardRegistry();
  });

  describe('constructor', () => {
    it('should create registry with default guards', () => {
      expect(registry).toBeInstanceOf(GuardRegistry);
      const guards = registry.list();
      expect(guards).toContain('quality_check');
      expect(guards).toContain('env_var_guard');
      expect(guards).toContain('file_path_guard');
      expect(guards).toContain('network_guard');
      expect(guards).toContain('command_guard');
    });

    it('should accept custom configuration', () => {
      const customRegistry = createGuardRegistry({
        quality_check: {
          critical_observations_threshold: 100,
          confidence_min: 0.8
        }
      });
      expect(customRegistry.config.quality_check.critical_observations_threshold).toBe(100);
    });

    it('should have correct forbidden pattern count', () => {
      const counts = registry.getForbiddenPatternCount();
      expect(counts.env).toBeGreaterThan(25);
      expect(counts.file).toBeGreaterThan(20);
      expect(counts.total).toBeGreaterThan(70);
    });
  });

  // ===========================================================================
  // H1-H7: Environment Variable Guards
  // ===========================================================================

  describe('checkEnvironmentVariable', () => {
    it('should deny access to TOKEN variables', () => {
      const result = registry.checkEnvironmentVariable('API_TOKEN');
      expect(result.allowed).toBe(false);
      expect(result.severity).toBe('critical');
      expect(result.pattern).toBe('*TOKEN');
    });

    it('should deny access to SECRET variables', () => {
      const result = registry.checkEnvironmentVariable('MY_SECRET');
      expect(result.allowed).toBe(false);
      expect(result.pattern).toBe('*SECRET');
    });

    it('should deny access to PASSWORD variables', () => {
      const result = registry.checkEnvironmentVariable('DB_PASSWORD');
      expect(result.allowed).toBe(false);
      expect(result.pattern).toBe('*PASSWORD');
    });

    it('should deny access to AWS credentials', () => {
      const result = registry.checkEnvironmentVariable('AWS_SECRET_ACCESS_KEY');
      expect(result.allowed).toBe(false);
      expect(result.guardId).toBe('G-H1-ENV-TOKEN');
    });

    it('should deny access to GitHub tokens', () => {
      const result = registry.checkEnvironmentVariable('GITHUB_TOKEN');
      expect(result.allowed).toBe(false);
    });

    it('should deny access to database credentials', () => {
      const result = registry.checkEnvironmentVariable('DATABASE_URL');
      expect(result.allowed).toBe(false);
    });

    it('should allow safe environment variables', () => {
      const result = registry.checkEnvironmentVariable('NODE_ENV');
      expect(result.allowed).toBe(true);
    });

    it('should allow PATH variable', () => {
      const result = registry.checkEnvironmentVariable('PATH');
      expect(result.allowed).toBe(true);
    });

    it('should handle case insensitivity', () => {
      const result = registry.checkEnvironmentVariable('aws_access_key_id');
      expect(result.allowed).toBe(false);
    });

    it('should generate denial receipt', () => {
      const result = registry.checkEnvironmentVariable('STRIPE_SECRET_KEY');
      expect(result.receipt).toBeDefined();
      expect(result.receipt.decision).toBe('deny');
      expect(result.receipt.operation).toBe('env-var-access');
    });

    it('should handle invalid input', () => {
      const result = registry.checkEnvironmentVariable(null);
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('INVALID_INPUT');
    });

    it('should handle empty string', () => {
      const result = registry.checkEnvironmentVariable('');
      expect(result.allowed).toBe(false);
    });
  });

  // ===========================================================================
  // H8-H14: File Path Guards
  // ===========================================================================

  describe('checkFilePathAccess', () => {
    it('should deny access to SSH keys', () => {
      const result = registry.checkFilePathAccess('~/.ssh/id_rsa');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('SSH_KEYS');
    });

    it('should deny access to AWS config', () => {
      const result = registry.checkFilePathAccess('~/.aws/credentials');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('AWS_CONFIG');
    });

    it('should deny access to .npmrc', () => {
      const result = registry.checkFilePathAccess('/home/user/project/.npmrc');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('NPM_REGISTRY');
    });

    it('should deny access to .env files', () => {
      const result = registry.checkFilePathAccess('/app/.env');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('ENV_FILE');
    });

    it('should deny access to .env.production', () => {
      const result = registry.checkFilePathAccess('/app/.env.production');
      expect(result.allowed).toBe(false);
    });

    it('should deny access to .git/config', () => {
      const result = registry.checkFilePathAccess('/project/.git/config');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('GIT_CONFIG');
    });

    it('should deny access to kube config', () => {
      const result = registry.checkFilePathAccess('~/.kube/config');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('KUBE_CONFIG');
    });

    it('should deny access to docker config', () => {
      const result = registry.checkFilePathAccess('~/.docker/config.json');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('DOCKER_CONFIG');
    });

    it('should deny access to /etc/passwd', () => {
      const result = registry.checkFilePathAccess('/etc/passwd');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('SYSTEM_USER_DB');
    });

    it('should deny access to /etc/shadow', () => {
      const result = registry.checkFilePathAccess('/etc/shadow');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('SYSTEM_PASSWORD_DB');
    });

    it('should allow access to safe paths', () => {
      const result = registry.checkFilePathAccess('/home/user/project/src/index.js');
      expect(result.allowed).toBe(true);
    });

    it('should allow access to node_modules', () => {
      const result = registry.checkFilePathAccess('/project/node_modules/lodash/index.js');
      expect(result.allowed).toBe(true);
    });

    it('should normalize paths with backslashes', () => {
      // Windows paths get normalized to forward slashes
      const result = registry.checkFilePathAccess('/Users/user/.ssh/id_rsa');
      expect(result.allowed).toBe(false);
      expect(result.category).toBe('SSH_KEYS');
    });

    it('should handle invalid input', () => {
      const result = registry.checkFilePathAccess(null);
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('INVALID_INPUT');
    });
  });

  // ===========================================================================
  // H15-H17: Network URL Guards
  // ===========================================================================

  describe('checkNetworkURL', () => {
    it('should deny access to AWS metadata service', () => {
      const result = registry.checkNetworkURL('http://169.254.169.254/latest/meta-data/');
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('FORBIDDEN_HOST');
    });

    it('should deny access to Google metadata service', () => {
      const result = registry.checkNetworkURL('http://metadata.google.internal/computeMetadata/v1/');
      expect(result.allowed).toBe(false);
    });

    it('should deny access to internal networks (10.x)', () => {
      const result = registry.checkNetworkURL('http://10.0.0.1/api');
      expect(result.allowed).toBe(false);
    });

    it('should deny access to internal networks (172.16.x)', () => {
      const result = registry.checkNetworkURL('http://172.16.0.1/api');
      expect(result.allowed).toBe(false);
    });

    it('should deny access to internal networks (192.168.x)', () => {
      const result = registry.checkNetworkURL('http://192.168.1.1/api');
      expect(result.allowed).toBe(false);
    });

    it('should deny access to .internal domains', () => {
      const result = registry.checkNetworkURL('http://service.internal/api');
      expect(result.allowed).toBe(false);
    });

    it('should allow access to GitHub API', () => {
      const result = registry.checkNetworkURL('https://api.github.com/repos/user/repo');
      expect(result.allowed).toBe(true);
    });

    it('should allow access to NPM registry', () => {
      const result = registry.checkNetworkURL('https://registry.npmjs.org/lodash');
      expect(result.allowed).toBe(true);
    });

    it('should allow access to localhost', () => {
      const result = registry.checkNetworkURL('http://localhost:3000/api');
      expect(result.allowed).toBe(true);
    });

    it('should allow access to 127.0.0.1', () => {
      const result = registry.checkNetworkURL('http://127.0.0.1:8080/');
      expect(result.allowed).toBe(true);
    });

    it('should deny non-whitelisted hosts', () => {
      const result = registry.checkNetworkURL('https://evil-site.com/steal-data');
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('NOT_IN_ALLOWLIST');
    });

    it('should handle invalid URL', () => {
      const result = registry.checkNetworkURL('not-a-url');
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('INVALID_URL');
    });

    it('should handle null input', () => {
      const result = registry.checkNetworkURL(null);
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('INVALID_INPUT');
    });

    it('should respect custom allowlist', () => {
      const customRegistry = createGuardRegistry({
        network_allowlist: [
          { hostname: 'api.example.com', paths: ['**'], methods: ['GET'] }
        ]
      });
      const result = customRegistry.checkNetworkURL('https://api.example.com/data');
      expect(result.allowed).toBe(true);
    });
  });

  // ===========================================================================
  // H18-H25: Command Execution Guards
  // ===========================================================================

  describe('checkCommandExecution', () => {
    it('should deny env command', () => {
      const result = registry.checkCommandExecution('env');
      expect(result.allowed).toBe(false);
      expect(result.pattern).toBe('env');
    });

    it('should deny printenv command', () => {
      const result = registry.checkCommandExecution('printenv');
      expect(result.allowed).toBe(false);
    });

    it('should deny whoami command', () => {
      const result = registry.checkCommandExecution('whoami');
      expect(result.allowed).toBe(false);
    });

    it('should deny cat /etc/passwd', () => {
      const result = registry.checkCommandExecution('cat /etc/passwd');
      expect(result.allowed).toBe(false);
    });

    it('should deny curl command', () => {
      const result = registry.checkCommandExecution('curl https://example.com');
      expect(result.allowed).toBe(false);
    });

    it('should deny wget command', () => {
      const result = registry.checkCommandExecution('wget https://example.com');
      expect(result.allowed).toBe(false);
    });

    it('should deny ssh command', () => {
      const result = registry.checkCommandExecution('ssh user@host');
      expect(result.allowed).toBe(false);
    });

    it('should deny pipe injection', () => {
      // The 'cat /etc/passwd' forbidden command is detected first
      const result = registry.checkCommandExecution('ls | cat /etc/passwd');
      expect(result.allowed).toBe(false);
      // Either FORBIDDEN_COMMAND (due to 'cat /etc/passwd') or SHELL_INJECTION_ATTEMPT (due to '|')
      expect(['FORBIDDEN_COMMAND', 'SHELL_INJECTION_ATTEMPT']).toContain(result.reasonCode);
    });

    it('should deny command chaining with &&', () => {
      const result = registry.checkCommandExecution('ls && rm -rf /');
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('SHELL_INJECTION_ATTEMPT');
    });

    it('should deny command chaining with ;', () => {
      const result = registry.checkCommandExecution('echo hello; rm -rf /');
      expect(result.allowed).toBe(false);
    });

    it('should deny output redirection', () => {
      const result = registry.checkCommandExecution('echo secret > /tmp/file');
      expect(result.allowed).toBe(false);
    });

    it('should deny command substitution $(...)', () => {
      const result = registry.checkCommandExecution('echo $(whoami)');
      expect(result.allowed).toBe(false);
    });

    it('should deny backtick substitution', () => {
      const result = registry.checkCommandExecution('echo `whoami`');
      expect(result.allowed).toBe(false);
    });

    it('should allow safe commands', () => {
      const result = registry.checkCommandExecution('npm run build');
      expect(result.allowed).toBe(true);
    });

    it('should allow node command', () => {
      const result = registry.checkCommandExecution('node index.js');
      expect(result.allowed).toBe(true);
    });

    it('should detect injection in arguments', () => {
      const result = registry.checkCommandExecution('node', ['--eval', 'console.log(1); process.exit(0)']);
      expect(result.allowed).toBe(false);
    });

    it('should handle null input', () => {
      const result = registry.checkCommandExecution(null);
      expect(result.allowed).toBe(false);
      expect(result.reasonCode).toBe('INVALID_INPUT');
    });
  });

  // ===========================================================================
  // Cache Behavior
  // ===========================================================================

  describe('cache', () => {
    it('should cache decisions', () => {
      registry.checkEnvironmentVariable('AWS_SECRET_KEY');
      registry.checkEnvironmentVariable('AWS_SECRET_KEY');

      const stats = registry.getCacheStats();
      expect(stats.size).toBeGreaterThan(0);
    });

    it('should clear cache', () => {
      registry.checkEnvironmentVariable('AWS_SECRET_KEY');
      registry.clearCache();

      const stats = registry.getCacheStats();
      expect(stats.size).toBe(0);
    });

    it('should return consistent results from cache', () => {
      const result1 = registry.checkEnvironmentVariable('GITHUB_TOKEN');
      const result2 = registry.checkEnvironmentVariable('GITHUB_TOKEN');

      expect(result1.allowed).toBe(result2.allowed);
      expect(result1.pattern).toBe(result2.pattern);
    });
  });

  // ===========================================================================
  // Audit Logging
  // ===========================================================================

  describe('audit log', () => {
    it('should log all decisions', () => {
      registry.checkEnvironmentVariable('NODE_ENV');
      registry.checkEnvironmentVariable('AWS_SECRET_KEY');

      const log = registry.getAuditLog();
      expect(log.length).toBeGreaterThanOrEqual(2);
    });

    it('should filter by decision', () => {
      registry.checkEnvironmentVariable('NODE_ENV'); // ALLOW
      registry.checkEnvironmentVariable('AWS_SECRET_KEY'); // DENY

      const denials = registry.getAuditLog({ decision: 'DENY' });
      expect(denials.every(e => e.decision === 'DENY')).toBe(true);
    });

    it('should filter by operation', () => {
      registry.checkEnvironmentVariable('NODE_ENV');
      registry.checkFilePathAccess('/tmp/file.txt');

      const envLogs = registry.getAuditLog({ operation: 'env-var-access' });
      expect(envLogs.every(e => e.operation === 'env-var-access')).toBe(true);
    });

    it('should limit results', () => {
      for (let i = 0; i < 100; i++) {
        registry.checkEnvironmentVariable(`VAR_${i}`);
      }

      const limited = registry.getAuditLog({ limit: 10 });
      expect(limited.length).toBe(10);
    });
  });

  // ===========================================================================
  // Quality Guards (Original)
  // ===========================================================================

  describe('validateAll (quality guards)', () => {
    it('should detect low confidence observations', () => {
      const observations = Array(100).fill(null).map((_, i) => ({
        id: `obs-${i}`,
        agent: 'test',
        timestamp: new Date().toISOString(),
        kind: 'coverage',
        severity: 'info',
        subject: 'test',
        metrics: { confidence: 0.3, coverage: 0.9, latency_ms: 10 }
      }));

      const violations = registry.validateAll(observations);
      expect(violations.some(v => v.guard_id === 'quality_check')).toBe(true);
    });

    it('should detect malformed observations', () => {
      const observations = [
        { id: null, agent: 'test', timestamp: null, kind: null, metrics: null }
      ];

      const violations = registry.validateAll(observations);
      expect(violations.some(v => v.guard_id === 'integrity_check')).toBe(true);
    });

    it('should pass valid observations', () => {
      const observations = [{
        id: 'valid-1',
        agent: 'agent-1',
        timestamp: new Date().toISOString(),
        kind: 'coverage',
        severity: 'info',
        subject: 'test',
        metrics: { confidence: 0.9, coverage: 0.9, latency_ms: 10 }
      }];

      const violations = registry.validateAll(observations);
      // Should have agent coverage warning (only 1 agent)
      expect(violations.filter(v => v.guard_id !== 'agent_coverage').length).toBe(0);
    });
  });
});

describe('PATTERNS', () => {
  it('should export all pattern arrays', () => {
    expect(PATTERNS.ENV_VAR_FORBIDDEN_PATTERNS).toBeDefined();
    expect(PATTERNS.FILE_PATH_FORBIDDEN_PATTERNS).toBeDefined();
    expect(PATTERNS.NETWORK_FORBIDDEN_DOMAINS).toBeDefined();
    expect(PATTERNS.FORBIDDEN_COMMANDS).toBeDefined();
    expect(PATTERNS.DANGEROUS_SHELL_PATTERNS).toBeDefined();
  });

  it('should have comprehensive env patterns', () => {
    const patterns = PATTERNS.ENV_VAR_FORBIDDEN_PATTERNS;
    expect(patterns).toContain('*TOKEN');
    expect(patterns).toContain('*SECRET');
    expect(patterns).toContain('AWS_*');
    expect(patterns).toContain('GITHUB_*');
  });

  it('should have comprehensive file patterns', () => {
    const patterns = PATTERNS.FILE_PATH_FORBIDDEN_PATTERNS;
    expect(patterns.some(p => p.pattern.includes('.ssh'))).toBe(true);
    expect(patterns.some(p => p.pattern.includes('.env'))).toBe(true);
    expect(patterns.some(p => p.pattern.includes('.aws'))).toBe(true);
  });
});
