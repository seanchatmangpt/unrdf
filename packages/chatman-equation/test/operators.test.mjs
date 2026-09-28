/**
 * @file evaluateCondition tests, including a code-injection regression.
 * Previously evaluateCondition() spliced context values into source text and ran it
 * through `new Function`, so a string value containing a quote executed arbitrary code.
 */

import { describe, it, expect, afterEach, vi } from 'vitest';
import { evaluateCondition, applyRule } from '../src/operators.mjs';

describe('evaluateCondition', () => {
  afterEach(() => {
    delete globalThis.__chatman_pwned;
    vi.restoreAllMocks();
  });

  it('evaluates numeric comparisons against the context', () => {
    expect(evaluateCondition('user_count > 100', { user_count: 150 })).toBe(true);
    expect(evaluateCondition('user_count > 100', { user_count: 50 })).toBe(false);
    expect(evaluateCondition('a >= 2 AND b <= 3', { a: 2, b: 3 })).toBe(true);
    expect(evaluateCondition('a != 2', { a: 2 })).toBe(false);
  });

  it('supports AND / OR / NOT, && / || / ! and parentheses with correct precedence', () => {
    expect(evaluateCondition('a > 1 OR b > 1 AND c > 1', { a: 2, b: 0, c: 0 })).toBe(true);
    expect(evaluateCondition('(a > 1 OR b > 1) AND c > 1', { a: 2, b: 0, c: 0 })).toBe(false);
    expect(evaluateCondition('NOT flag', { flag: false })).toBe(true);
    expect(evaluateCondition('!flag && a == 1', { flag: false, a: 1 })).toBe(true);
  });

  it('compares strings and booleans', () => {
    expect(evaluateCondition('domain == "market"', { domain: 'market' })).toBe(true);
    expect(evaluateCondition("domain == 'market'", { domain: 'org' })).toBe(false);
    expect(evaluateCondition('active == true', { active: true })).toBe(true);
  });

  it('returns false (and warns) for unknown identifiers or malformed expressions', () => {
    const warn = vi.spyOn(console, 'warn').mockImplementation(() => {});
    expect(evaluateCondition('missing > 1', {})).toBe(false);
    expect(evaluateCondition('a >', { a: 1 })).toBe(false);
    expect(warn).toHaveBeenCalledTimes(2);
  });

  it('does not execute code smuggled in through a string context value', () => {
    vi.spyOn(console, 'warn').mockImplementation(() => {});
    const evil = 'x" + (globalThis.__chatman_pwned = 1) + "';
    evaluateCondition('name == "abc"', { name: evil });
    expect(globalThis.__chatman_pwned).toBeUndefined();
  });

  it('does not execute code placed in the condition text itself', () => {
    vi.spyOn(console, 'warn').mockImplementation(() => {});
    const r = evaluateCondition('(globalThis.__chatman_pwned = 1) == 1', {});
    expect(r).toBe(false);
    expect(globalThis.__chatman_pwned).toBeUndefined();
    expect(evaluateCondition('process.exit(1)', {})).toBe(false);
  });

  it('a string containing a quote is compared verbatim', () => {
    expect(evaluateCondition('name == "a\\"b"', { name: 'a"b' })).toBe(true);
    expect(evaluateCondition('name == "abc"', { name: 'a" || true || "' })).toBe(false);
  });
});

describe('applyRule', () => {
  it('triggers only when the condition holds', () => {
    const rule = { name: 'growth', condition: 'user_count > 100', action: 'scale', parameters: { by: 2 } };
    const hit = applyRule(rule, { user_count: 500 });
    expect(hit).toMatchObject({ rule: 'growth', action: 'scale', parameters: { by: 2 }, triggered: true });
    expect(applyRule(rule, { user_count: 5 })).toBeNull();
  });
});
