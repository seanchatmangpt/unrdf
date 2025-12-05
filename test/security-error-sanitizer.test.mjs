import { describe, it, expect } from 'vitest';
import { ErrorSanitizer } from '../packages/knowledge-engine/security/error-sanitizer.mjs';

describe('Security - ErrorSanitizer', () => {
  it('redacts credentials and file paths', () => {
    const sanitizer = new ErrorSanitizer();
    const msg =
      'Error connecting to postgres://user:pass@localhost:5432/db at /Users/alice/app/index.js:10:5';
    const sanitized = sanitizer.sanitize(msg);
    expect(sanitized).not.toMatch(/postgres:\/\/user:pass/);
    expect(sanitized).toMatch(/postgres:\/\/\*\*\*:\*\*\*@/);
    expect(sanitized).not.toMatch(/\/Users\/alice\/app\/index.js/);
  });
});
