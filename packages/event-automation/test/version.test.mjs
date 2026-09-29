/**
 * @file Version regression test
 * @description VERSION must track package.json and never be a placeholder.
 */
import { describe, it, expect } from 'vitest';
import { createRequire } from 'node:module';
import { VERSION } from '../src/index.mjs';

const pkg = createRequire(import.meta.url)('../package.json');

describe('event-automation VERSION', () => {
  it('equals package.json version and is not a placeholder', () => {
    expect(VERSION).toBe(pkg.version);
    expect(VERSION).not.toContain('[');
  });
});
