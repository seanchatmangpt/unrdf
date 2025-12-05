import { describe, it, expect } from 'vitest';
import { ErrorSanitizer } from '../packages/knowledge-engine/security/error-sanitizer.mjs';

describe('ErrorSanitizer - avoids false positives', () => {
  it('does not redact benign URLs or words that look like tokens', () => {
    const sanitizer = new ErrorSanitizer();
    const benign =
      'Fetch failed for https://example.com/api/status with tokenized message but no credentials';
    const out = sanitizer.sanitize(benign);
    expect(out).toContain('https://example.com/api/status');
    expect(out.toLowerCase()).toContain('tokenized');
  });
});
