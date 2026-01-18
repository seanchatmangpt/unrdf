/**
 * @file Fast error sanitizer tests (minimal)
 * @description 2 essential tests for error message sanitization
 */

import { describe, it, expect } from 'vitest';

const createErrorSanitizer = () => ({
  sanitize: (message) => {
    let sanitized = message;
    sanitized = sanitized.replace(/([a-z]+:\/\/)([^:]+):([^@]+)@/gi, '$1***:***@');
    sanitized = sanitized.replace(/(API_KEY=)([a-zA-Z0-9_-]{20,})/gi, '$1[REDACTED]');
    return sanitized;
  },
});

describe('Error Sanitizer', () => {
  const sanitizer = createErrorSanitizer();

  it('redacts database credentials', () => {
    const msg = 'postgres://admin:secret@localhost:5432/db';
    const sanitized = sanitizer.sanitize(msg);

    expect(sanitized).not.toMatch(/admin:secret/);
    expect(sanitized).toMatch(/\*\*\*:\*\*\*@/);
  });

  it('redacts API keys', () => {
    const msg = 'API_KEY=sk_live_51234567890abcdefghij';
    const sanitized = sanitizer.sanitize(msg);

    expect(sanitized).not.toMatch(/sk_live_/);
    expect(sanitized).toMatch(/\[REDACTED\]/);
  });
});
