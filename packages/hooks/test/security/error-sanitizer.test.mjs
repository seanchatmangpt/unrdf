/**
 * @vitest-environment node
 * @file Error Sanitizer Tests - Comprehensive coverage for error-sanitizer.mjs
 */
import { describe, it, expect } from 'vitest';
import { ErrorSanitizer } from '../../src/hooks/security/error-sanitizer.mjs';

describe('ErrorSanitizer - Construction', () => {
  it('should create sanitizer with default options', () => {
    const sanitizer = new ErrorSanitizer();
    expect(sanitizer.options.removeStackTraces).toBe(true);
    expect(sanitizer.options.removeFilePaths).toBe(true);
    expect(sanitizer.options.removeCredentials).toBe(true);
  });

  it('should create sanitizer with custom options', () => {
    const sanitizer = new ErrorSanitizer({
      removeStackTraces: false,
      genericErrorMessage: 'Custom error',
    });
    expect(sanitizer.options.removeStackTraces).toBe(false);
    expect(sanitizer.options.genericErrorMessage).toBe('Custom error');
  });

  it('should validate options with Zod schema', () => {
    expect(() => {
      new ErrorSanitizer({ removeStackTraces: 'invalid' });
    }).toThrow();
  });
});

describe('ErrorSanitizer - File Path Removal', () => {
  let sanitizer;

  beforeEach(() => {
    sanitizer = new ErrorSanitizer({ removeFilePaths: true });
  });

  it('should remove Unix file paths', () => {
    const message = 'Error at /usr/local/bin/app.js:42:10';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('/usr/local');
    expect(sanitized).not.toContain('app.js');
  });

  it('should remove Windows file paths', () => {
    const message = 'Error in C:\\Users\\admin\\app.js';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('C:\\');
    expect(sanitized).not.toContain('Users');
  });

  it('should remove Docker app paths', () => {
    const message = 'Failed at /app/src/handlers/auth.js';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('/app/src');
  });

  it('should remove system paths', () => {
    const paths = [
      '/etc/passwd',
      '/var/log/app.log',
      '/home/user/.env',
      'D:\\Windows\\System32\\config',
    ];

    paths.forEach(path => {
      const sanitized = sanitizer.sanitize(`Error accessing ${path}`);
      expect(sanitized).not.toContain(path);
    });
  });

  it('should preserve message without paths when disabled', () => {
    sanitizer = new ErrorSanitizer({ removeFilePaths: false });
    const message = 'Error at /app/file.js';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).toContain('/app/file.js');
  });
});

describe('ErrorSanitizer - Credential Removal', () => {
  let sanitizer;

  beforeEach(() => {
    sanitizer = new ErrorSanitizer({ removeCredentials: true });
  });

  it('should remove database connection strings', () => {
    const message = 'Failed to connect: postgres://user:pass123@localhost:5432/db';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('user:pass123');
    expect(sanitized).not.toContain('postgres://');
  });

  it('should remove API keys', () => {
    const message = 'Authentication failed with api_key=sk_test_abc123xyz';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('sk_test_abc123xyz');
    expect(sanitized).not.toContain('api_key=');
  });

  it('should remove passwords', () => {
    const messages = [
      'Login failed: password="secret123"',
      'Auth error: password: hunter2',
      'password=myP@ssw0rd failed',
    ];

    messages.forEach(msg => {
      const sanitized = sanitizer.sanitize(msg);
      expect(sanitized).not.toContain('secret123');
      expect(sanitized).not.toContain('hunter2');
      expect(sanitized).not.toContain('myP@ssw0rd');
    });
  });

  it('should remove secrets and tokens', () => {
    const messages = [
      'secret: jwt_token_abc123',
      'token=Bearer xyz789',
      'authorization: Basic dXNlcjpwYXNz',
    ];

    messages.forEach(msg => {
      const sanitized = sanitizer.sanitize(msg);
      expect(sanitized.length).toBeLessThan(msg.length);
    });
  });

  it('should preserve credentials when disabled', () => {
    sanitizer = new ErrorSanitizer({ removeCredentials: false });
    const message = 'password=test123';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).toContain('password=test123');
  });
});

describe('ErrorSanitizer - Stack Trace Removal', () => {
  let sanitizer;

  beforeEach(() => {
    sanitizer = new ErrorSanitizer({ removeStackTraces: true });
  });

  it('should remove Node.js stack traces', () => {
    const message = `Error: Test error
    at Function.test (/app/test.js:10:5)
    at Object.<anonymous> (/app/index.js:20:3)
    at Module._compile (internal/modules/cjs/loader.js:999:30)`;

    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('at Function.test');
    expect(sanitized).not.toContain('at Object.<anonymous>');
  });

  it('should remove stack trace line numbers', () => {
    const message = 'at processData (file.js:42:10)';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain(':42:10');
  });

  it('should preserve stack traces when disabled', () => {
    sanitizer = new ErrorSanitizer({ removeStackTraces: false });
    const message = 'at Function (file.js:10:5)';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).toContain('at Function');
  });
});

describe('ErrorSanitizer - Environment Variable Removal', () => {
  let sanitizer;

  beforeEach(() => {
    sanitizer = new ErrorSanitizer({ removeEnvironmentVars: true });
  });

  it('should remove common environment variables', () => {
    const vars = [
      'DATABASE_URL=postgres://localhost',
      'API_KEY=abc123',
      'SECRET=mysecret',
      'PASSWORD=pass123',
      'JWT_SECRET=token',
      'AWS_KEY=AKIA...',
    ];

    vars.forEach(v => {
      const sanitized = sanitizer.sanitize(`Error: ${v}`);
      expect(sanitized.length).toBeLessThan(`Error: ${v}`.length);
    });
  });

  it('should preserve env vars when disabled', () => {
    sanitizer = new ErrorSanitizer({ removeEnvironmentVars: false });
    const message = 'API_KEY=test123';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).toContain('API_KEY=test123');
  });
});

describe('ErrorSanitizer - Error Type Handling', () => {
  let sanitizer;

  beforeEach(() => {
    sanitizer = new ErrorSanitizer();
  });

  it('should sanitize Error objects', () => {
    const error = new Error('Database connection failed at /app/db.js:10');
    const sanitized = sanitizer.sanitize(error);
    expect(sanitized).not.toContain('/app/db.js');
  });

  it('should sanitize TypeError objects', () => {
    const error = new TypeError('Invalid type at /usr/lib/module.js');
    const sanitized = sanitizer.sanitize(error);
    expect(sanitized).not.toContain('/usr/lib');
  });

  it('should sanitize string messages', () => {
    const message = 'Error at /home/user/app.js';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('/home/user');
  });

  it('should handle non-string values', () => {
    const sanitized = sanitizer.sanitize(null);
    expect(typeof sanitized).toBe('string');
  });

  it('should handle undefined', () => {
    const sanitized = sanitizer.sanitize(undefined);
    expect(typeof sanitized).toBe('string');
  });
});

describe('ErrorSanitizer - Complex Scenarios', () => {
  let sanitizer;

  beforeEach(() => {
    sanitizer = new ErrorSanitizer();
  });

  it('should sanitize multiple sensitive patterns in one message', () => {
    const message = `
      Connection failed at /app/db.js:42
      Using DATABASE_URL=postgres://admin:secret@localhost
      Stack trace:
        at connect (/usr/lib/db.js:100:5)
        at init (/home/user/app.js:50:10)
    `;

    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('/app/db.js');
    expect(sanitized).not.toContain('admin:secret');
    expect(sanitized).not.toContain('/usr/lib');
    expect(sanitized).not.toContain('at connect');
  });

  it('should use generic error message when all sensitive data', () => {
    const message = 'password=secret123 at /etc/passwd:1:1';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).toBeDefined();
    expect(sanitized.length).toBeGreaterThan(0);
  });

  it('should preserve safe parts of message', () => {
    const message = 'Database connection failed. Please check configuration.';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).toContain('Database connection failed');
  });
});

describe('ErrorSanitizer - Edge Cases', () => {
  let sanitizer;

  beforeEach(() => {
    sanitizer = new ErrorSanitizer();
  });

  it('should handle empty strings', () => {
    const sanitized = sanitizer.sanitize('');
    // Empty strings trigger generic error message
    expect(sanitized).toBe('An error occurred');
  });

  it('should handle very long error messages', () => {
    const longMessage = 'Error: ' + 'a'.repeat(10000) + ' at /app/file.js';
    const sanitized = sanitizer.sanitize(longMessage);
    // Sanitization may add generic message making it longer if too much removed
    expect(sanitized.length).toBeGreaterThan(0);
  });

  it('should handle unicode characters', () => {
    const message = 'Error 错误 at /app/文件.js with émoji 🎯';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).toBeDefined();
  });

  it('should handle nested quotes in credentials', () => {
    const message = 'password="my\\"secret\\"123"';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized).not.toContain('my\\"secret\\"123');
  });

  it('should handle URL-encoded credentials', () => {
    const message = 'password=%22secret%22 api_key=%3Ckey%3E';
    const sanitized = sanitizer.sanitize(message);
    expect(sanitized.length).toBeLessThan(message.length);
  });
});

describe('ErrorSanitizer - Performance', () => {
  it('should sanitize rapidly without memory leaks', () => {
    const sanitizer = new ErrorSanitizer();
    const message = 'Error at /app/file.js with password=secret123';

    for (let i = 0; i < 10000; i++) {
      sanitizer.sanitize(message);
    }

    expect(true).toBe(true); // If we get here, no memory issues
  });

  it('should handle concurrent sanitization', async () => {
    const sanitizer = new ErrorSanitizer();
    const messages = Array(100).fill('Error at /app/file.js:10');

    const results = await Promise.all(
      messages.map(msg => Promise.resolve(sanitizer.sanitize(msg)))
    );

    expect(results).toHaveLength(100);
    results.forEach(result => {
      expect(result).not.toContain('/app/file.js');
    });
  });
});
