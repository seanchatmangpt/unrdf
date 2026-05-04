/**
 * @fileoverview Guard Test Cases Fixtures
 *
 * 25 forbidden patterns with 9 test scenarios:
 * - 7 deny cases
 * - 2 allow cases
 *
 * Each case includes:
 * - Input data
 * - Expected guard response
 * - Expected deny reason (if applicable)
 *
 * @module @unrdf/kgc-probe/test/fixtures/guard-test-cases
 */

import { FROZEN_TIMESTAMP } from './frozen-environment.mjs';

// ============================================================================
// FORBIDDEN PATTERNS (25 total)
// ============================================================================

/**
 * Forbidden environment variable patterns
 * These should NEVER appear in any observation
 */
export const FORBIDDEN_ENV_VARS = [
  'API_KEY',
  'SECRET_KEY',
  'ACCESS_TOKEN',
  'PASSWORD',
  'PRIVATE_KEY',
  'AWS_ACCESS_KEY_ID',
  'AWS_SECRET_ACCESS_KEY',
  'GITHUB_TOKEN',
  'NPM_TOKEN',
  'DATABASE_PASSWORD'
];

/**
 * Forbidden filesystem paths
 * Access to these should be blocked
 */
export const FORBIDDEN_PATHS = [
  '/etc/passwd',
  '/etc/shadow',
  '/root/.ssh/id_rsa',
  '/home/*/.ssh/*',
  '**/.env',
  '**/.env.local',
  '**/credentials.json',
  '**/secrets.yaml'
];

/**
 * Forbidden URL patterns
 * Network requests to these should be blocked
 */
export const FORBIDDEN_URLS = [
  'file://*',
  'javascript:*',
  'data:*',
  '*.local',
  'localhost:*',
  '127.0.0.1:*',
  '169.254.169.254/*'  // AWS metadata endpoint
];

/**
 * All forbidden patterns combined
 */
export const ALL_FORBIDDEN_PATTERNS = [
  ...FORBIDDEN_ENV_VARS,
  ...FORBIDDEN_PATHS,
  ...FORBIDDEN_URLS
];

// ============================================================================
// TEST SCENARIOS
// ============================================================================

/**
 * Test Scenario 1: DENY - API Key in observation
 */
export const CASE_DENY_API_KEY = {
  name: 'API_KEY in observation evidence',
  input: {
    id: 'test-obs-001',
    agent: 'test-agent',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'completeness',
    severity: 'info',
    subject: 'example:entity1',
    evidence: {
      query: 'SELECT * WHERE { ?s ?p ?o }',
      result: {
        API_KEY: 'sk-live-abc123xyz'  // FORBIDDEN
      },
      witnesses: []
    },
    metrics: {
      confidence: 0.9,
      coverage: 0.8,
      latency_ms: 100
    },
    tags: []
  },
  expected: {
    allowed: false,
    guard_id: 'forbidden_payload',
    reason: 'Observation contains forbidden pattern: API_KEY',
    pattern_matched: 'API_KEY'
  }
};

/**
 * Test Scenario 2: DENY - Password in result
 */
export const CASE_DENY_PASSWORD = {
  name: 'PASSWORD in observation result',
  input: {
    id: 'test-obs-002',
    agent: 'test-agent',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'consistency',
    severity: 'warning',
    subject: 'user:admin',
    evidence: {
      query: 'SELECT ?password WHERE { ?user :hasPassword ?password }',
      result: {
        password: 'super_secret_password123!'  // FORBIDDEN
      },
      witnesses: ['user:admin']
    },
    metrics: {
      confidence: 0.95,
      coverage: 0.9,
      latency_ms: 50
    },
    tags: ['security']
  },
  expected: {
    allowed: false,
    guard_id: 'forbidden_payload',
    reason: 'Observation contains forbidden pattern: PASSWORD',
    pattern_matched: 'PASSWORD'
  }
};

/**
 * Test Scenario 3: DENY - AWS credentials
 */
export const CASE_DENY_AWS_CREDS = {
  name: 'AWS credentials in observation',
  input: {
    id: 'test-obs-003',
    agent: 'test-agent',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'coverage',
    severity: 'critical',
    subject: 'aws:config',
    evidence: {
      query: 'AWS configuration check',
      result: {
        AWS_ACCESS_KEY_ID: 'AKIAIOSFODNN7EXAMPLE',
        AWS_SECRET_ACCESS_KEY: 'wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY'
      },
      witnesses: []
    },
    metrics: {
      confidence: 1.0,
      coverage: 1.0,
      latency_ms: 20
    },
    tags: ['aws', 'config']
  },
  expected: {
    allowed: false,
    guard_id: 'forbidden_payload',
    reason: 'Observation contains forbidden pattern: AWS_ACCESS_KEY_ID',
    pattern_matched: 'AWS_ACCESS_KEY_ID'
  }
};

/**
 * Test Scenario 4: DENY - Private key content
 */
export const CASE_DENY_PRIVATE_KEY = {
  name: 'Private key content in observation',
  input: {
    id: 'test-obs-004',
    agent: 'test-agent',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'conformance',
    severity: 'critical',
    subject: 'file:/root/.ssh/id_rsa',  // Also forbidden path
    evidence: {
      query: 'File content check',
      result: {
        content: '-----BEGIN RSA PRIVATE KEY-----\nMIIEow...'
      },
      witnesses: []
    },
    metrics: {
      confidence: 1.0,
      coverage: 1.0,
      latency_ms: 10
    },
    tags: ['ssh', 'key']
  },
  expected: {
    allowed: false,
    guard_id: 'forbidden_path',
    reason: 'Observation references forbidden path: /root/.ssh/id_rsa',
    pattern_matched: '/root/.ssh/id_rsa'
  }
};

/**
 * Test Scenario 5: DENY - Environment file access
 */
export const CASE_DENY_ENV_FILE = {
  name: '.env file access attempt',
  input: {
    id: 'test-obs-005',
    agent: 'filesystem',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'fs_check',
    severity: 'info',
    subject: 'file:/project/.env',  // FORBIDDEN
    evidence: {
      query: 'readFile("/project/.env")',
      result: {
        exists: true,
        readable: true
      },
      witnesses: ['file:/project/.env']
    },
    metrics: {
      confidence: 0.9,
      coverage: 0.8,
      latency_ms: 5
    },
    tags: ['env', 'config']
  },
  expected: {
    allowed: false,
    guard_id: 'forbidden_path',
    reason: 'Observation references forbidden path pattern: **/.env',
    pattern_matched: '.env'
  }
};

/**
 * Test Scenario 6: DENY - Metadata endpoint access
 */
export const CASE_DENY_METADATA = {
  name: 'AWS metadata endpoint access',
  input: {
    id: 'test-obs-006',
    agent: 'network',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'net_check',
    severity: 'info',
    subject: 'http://169.254.169.254/latest/meta-data/',  // FORBIDDEN
    evidence: {
      query: 'HTTP GET metadata endpoint',
      result: {
        status: 200
      },
      witnesses: []
    },
    metrics: {
      confidence: 1.0,
      coverage: 1.0,
      latency_ms: 50
    },
    tags: ['network', 'metadata']
  },
  expected: {
    allowed: false,
    guard_id: 'forbidden_url',
    reason: 'Observation references forbidden URL pattern: 169.254.169.254/*',
    pattern_matched: '169.254.169.254'
  }
};

/**
 * Test Scenario 7: DENY - Token in tags
 */
export const CASE_DENY_TOKEN_IN_TAGS = {
  name: 'Token value embedded in tags',
  input: {
    id: 'test-obs-007',
    agent: 'test-agent',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'completeness',
    severity: 'info',
    subject: 'example:entity',
    evidence: {
      query: 'SELECT ?s WHERE { ?s a :Entity }',
      result: { count: 10 },
      witnesses: []
    },
    metrics: {
      confidence: 0.9,
      coverage: 0.8,
      latency_ms: 100
    },
    tags: ['test', 'GITHUB_TOKEN:ghp_abc123xyz']  // FORBIDDEN
  },
  expected: {
    allowed: false,
    guard_id: 'forbidden_payload',
    reason: 'Observation contains forbidden pattern: GITHUB_TOKEN',
    pattern_matched: 'GITHUB_TOKEN'
  }
};

/**
 * Test Scenario 8: ALLOW - Clean observation
 */
export const CASE_ALLOW_CLEAN = {
  name: 'Clean observation with no forbidden patterns',
  input: {
    id: 'test-obs-008',
    agent: 'completion',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'completeness',
    severity: 'info',
    subject: 'example:entity1',
    predicate: 'rdf:type',
    object: 'example:Person',
    evidence: {
      query: 'SELECT ?s WHERE { ?s a example:Person. MINUS { ?s rdfs:label ?l } }',
      result: { count: 5 },
      witnesses: ['example:entity1', 'example:entity2']
    },
    metrics: {
      confidence: 0.92,
      coverage: 0.85,
      latency_ms: 150
    },
    tags: ['required_property', 'label']
  },
  expected: {
    allowed: true,
    guard_id: null,
    reason: null,
    pattern_matched: null
  }
};

/**
 * Test Scenario 9: ALLOW - Safe system info
 */
export const CASE_ALLOW_SYSTEM_INFO = {
  name: 'Safe system information observation',
  input: {
    id: 'test-obs-009',
    agent: 'runtime',
    timestamp: FROZEN_TIMESTAMP,
    kind: 'runtime_check',
    severity: 'info',
    subject: 'node:version',
    predicate: 'sys:version',
    object: 'v22.12.0',
    evidence: {
      query: 'process.version',
      result: {
        version: 'v22.12.0',
        platform: 'linux',
        arch: 'x64'
      },
      witnesses: []
    },
    metrics: {
      confidence: 1.0,
      coverage: 1.0,
      latency_ms: 1
    },
    tags: ['runtime', 'version']
  },
  expected: {
    allowed: true,
    guard_id: null,
    reason: null,
    pattern_matched: null
  }
};

/**
 * All test cases in array form
 */
export const ALL_TEST_CASES = [
  CASE_DENY_API_KEY,
  CASE_DENY_PASSWORD,
  CASE_DENY_AWS_CREDS,
  CASE_DENY_PRIVATE_KEY,
  CASE_DENY_ENV_FILE,
  CASE_DENY_METADATA,
  CASE_DENY_TOKEN_IN_TAGS,
  CASE_ALLOW_CLEAN,
  CASE_ALLOW_SYSTEM_INFO
];

/**
 * Deny cases only
 */
export const DENY_CASES = ALL_TEST_CASES.filter(c => !c.expected.allowed);

/**
 * Allow cases only
 */
export const ALLOW_CASES = ALL_TEST_CASES.filter(c => c.expected.allowed);

/**
 * Guard validation function
 * Checks observation for forbidden patterns
 * @param {Object} observation - Observation to validate
 * @returns {Object} Validation result
 */
export function validateObservationGuard(observation) {
  const obsStr = JSON.stringify(observation);
  const obsStrUpper = obsStr.toUpperCase();

  // Check forbidden env vars (case-insensitive)
  for (const pattern of FORBIDDEN_ENV_VARS) {
    if (obsStrUpper.includes(pattern.toUpperCase())) {
      return {
        allowed: false,
        guard_id: 'forbidden_payload',
        reason: `Observation contains forbidden pattern: ${pattern}`,
        pattern_matched: pattern
      };
    }
  }

  // Check forbidden paths in subject
  for (const pattern of FORBIDDEN_PATHS) {
    const regex = patternToRegex(pattern);
    if (regex.test(observation.subject || '')) {
      return {
        allowed: false,
        guard_id: 'forbidden_path',
        reason: `Observation references forbidden path pattern: ${pattern}`,
        pattern_matched: pattern
      };
    }
  }

  // Check forbidden URLs
  for (const pattern of FORBIDDEN_URLS) {
    const regex = patternToRegex(pattern);
    if (regex.test(observation.subject || '')) {
      return {
        allowed: false,
        guard_id: 'forbidden_url',
        reason: `Observation references forbidden URL pattern: ${pattern}`,
        pattern_matched: pattern
      };
    }
  }

  return {
    allowed: true,
    guard_id: null,
    reason: null,
    pattern_matched: null
  };
}

/**
 * Convert glob pattern to regex
 * @param {string} pattern - Glob pattern
 * @returns {RegExp} Regular expression
 */
function patternToRegex(pattern) {
  const escaped = pattern
    .replace(/[.+^${}()|[\]\\]/g, '\\$&')
    .replace(/\*\*/g, '.*')
    .replace(/\*/g, '[^/]*');
  return new RegExp(escaped);
}

export default {
  FORBIDDEN_ENV_VARS,
  FORBIDDEN_PATHS,
  FORBIDDEN_URLS,
  ALL_FORBIDDEN_PATTERNS,
  ALL_TEST_CASES,
  DENY_CASES,
  ALLOW_CASES,
  validateObservationGuard,
  CASE_DENY_API_KEY,
  CASE_DENY_PASSWORD,
  CASE_DENY_AWS_CREDS,
  CASE_DENY_PRIVATE_KEY,
  CASE_DENY_ENV_FILE,
  CASE_DENY_METADATA,
  CASE_DENY_TOKEN_IN_TAGS,
  CASE_ALLOW_CLEAN,
  CASE_ALLOW_SYSTEM_INFO
};
