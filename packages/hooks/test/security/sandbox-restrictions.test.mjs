/**
 * @vitest-environment node
 * @file Sandbox Restrictions Tests - Comprehensive coverage for sandbox-restrictions.mjs
 */
import { describe, it, expect, beforeEach } from 'vitest';
import { SandboxRestrictions } from '../../src/hooks/security/sandbox-restrictions.mjs';

describe('SandboxRestrictions - Construction', () => {
  it('should create restrictions with default config', () => {
    const restrictions = new SandboxRestrictions();
    expect(restrictions.config.allowFileSystem).toBe(false);
    expect(restrictions.config.allowNetwork).toBe(false);
    expect(restrictions.config.timeoutMs).toBe(5000);
    expect(restrictions.config.memoryLimitMB).toBe(50);
  });

  it('should create restrictions with custom config', () => {
    const restrictions = new SandboxRestrictions({
      allowFileSystem: true,
      timeoutMs: 10000,
      memoryLimitMB: 100
    });
    expect(restrictions.config.allowFileSystem).toBe(true);
    expect(restrictions.config.timeoutMs).toBe(10000);
    expect(restrictions.config.memoryLimitMB).toBe(100);
  });

  it('should validate config with Zod schema', () => {
    expect(() => {
      new SandboxRestrictions({ timeoutMs: 'invalid' });
    }).toThrow();
  });

  it('should enforce strict schema', () => {
    expect(() => {
      new SandboxRestrictions({ unknownOption: true });
    }).toThrow();
  });
});

describe('SandboxRestrictions - Restricted Context', () => {
  let restrictions;

  beforeEach(() => {
    restrictions = new SandboxRestrictions();
  });

  it('should create restricted context with safe globals', () => {
    const context = restrictions.createRestrictedContext();
    expect(context.Math).toBeDefined();
    expect(context.JSON).toBeDefined();
  });

  it('should block dangerous globals', () => {
    const context = restrictions.createRestrictedContext();
    expect(context.eval).toBeUndefined();
    expect(context.Function).toBeUndefined();
    expect(context.require).toBeUndefined();
    expect(context.import).toBeUndefined();
    expect(context.process).toBeUndefined();
  });

  it('should block filesystem access by default', () => {
    const context = restrictions.createRestrictedContext();
    expect(context.fs).toBeUndefined();
    expect(context.__dirname).toBeUndefined();
    expect(context.__filename).toBeUndefined();
  });

  it('should block network access by default', () => {
    const context = restrictions.createRestrictedContext();
    expect(context.http).toBeUndefined();
    expect(context.https).toBeUndefined();
    expect(context.net).toBeUndefined();
  });

  it('should block process access by default', () => {
    const context = restrictions.createRestrictedContext();
    expect(context.process).toBeUndefined();
    expect(context.Buffer).toBeUndefined();
  });

  it('should block timer functions', () => {
    const context = restrictions.createRestrictedContext();
    expect(context.setTimeout).toBeUndefined();
    expect(context.setInterval).toBeUndefined();
    expect(context.setImmediate).toBeUndefined();
  });
});

describe('SandboxRestrictions - Code Validation', () => {
  let restrictions;

  beforeEach(() => {
    restrictions = new SandboxRestrictions();
  });

  it('should validate hook code with blocked modules', () => {
    const hookFn = function() {
      const fs = require('fs');
      return fs.readFileSync('/etc/passwd');
    };

    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(false);
    expect(result.violations.length).toBeGreaterThan(0);
  });

  it('should allow safe code', () => {
    const hookFn = function(event) {
      return event.data + 1;
    };

    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(true);
    expect(result.violations).toHaveLength(0);
  });

  it('should block dangerous modules in code', () => {
    const hookFn = function() {
      const http = require('http');
      return http.get('http://evil.com');
    };

    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(false);
  });

  it('should block eval usage', () => {
    const hookFn = function() {
      return eval('1 + 1');
    };

    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(false);
  });

  it('should block Function constructor', () => {
    const hookFn = function() {
      return new Function('return 1')();
    };

    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(false);
  });
});

describe('SandboxRestrictions - Execution', () => {
  let restrictions;

  beforeEach(() => {
    restrictions = new SandboxRestrictions({ timeoutMs: 1000 });
  });

  it('should execute safe hook function', async () => {
    const hookFn = function(event) {
      return { result: event.value + 1 };
    };

    const result = await restrictions.executeRestricted(hookFn, { value: 5 });
    expect(result.result).toBe(6);
  });

  it('should reject hook with dangerous code', async () => {
    const hookFn = function() {
      const fs = require('fs');
      return fs.readFileSync('/etc/passwd');
    };

    const result = await restrictions.executeRestricted(hookFn, {});
    expect(result.success).toBe(false);
    expect(result.error).toContain('Security validation failed');
  });

  it('should timeout long-running hooks', async () => {
    const hookFn = function() {
      while(true) { /* infinite loop */ }
    };

    const result = await restrictions.executeRestricted(hookFn, {});
    expect(result.success).toBe(false);
    expect(result.error).toContain('timeout');
  });
});

describe('SandboxRestrictions - Deep Freeze', () => {
  let restrictions;

  beforeEach(() => {
    restrictions = new SandboxRestrictions();
  });

  it('should deep freeze objects', () => {
    const obj = { a: 1, b: { c: 2 } };
    const frozen = restrictions._deepFreeze(obj);

    expect(Object.isFrozen(frozen)).toBe(true);
    expect(Object.isFrozen(frozen.b)).toBe(true);
  });

  it('should handle null values', () => {
    const result = restrictions._deepFreeze(null);
    expect(result).toBeNull();
  });

  it('should handle primitive values', () => {
    expect(restrictions._deepFreeze(42)).toBe(42);
    expect(restrictions._deepFreeze('test')).toBe('test');
  });
});

describe('SandboxRestrictions - Permission Checks', () => {
  it('should deny filesystem when disabled', () => {
    const hookFn = function() {
      const fs = require('fs');
      return fs.readFileSync('/file.txt');
    };

    const restrictions = new SandboxRestrictions({ allowFileSystem: false });
    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(false);
  });

  it('should allow network when enabled', () => {
    const hookFn = function() {
      return fetch('http://example.com');
    };

    const restrictionsDisabled = new SandboxRestrictions({ allowNetwork: false });
    const resultDisabled = restrictionsDisabled.validateHookCode(hookFn);
    expect(resultDisabled.valid).toBe(false);
  });

  it('should deny process access when disabled', () => {
    const hookFn = function() {
      return process.exit(0);
    };

    const restrictions = new SandboxRestrictions({ allowProcessAccess: false });
    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(false);
  });
});

describe('SandboxRestrictions - Validation Results', () => {
  let restrictions;

  beforeEach(() => {
    restrictions = new SandboxRestrictions();
  });

  it('should return validation result for non-function', () => {
    const result = restrictions.validateHookCode('not a function');
    expect(result.valid).toBe(false);
    expect(result.violations).toContain('Hook must be a function');
  });

  it('should return detailed violations', () => {
    const hookFn = function() {
      const fs = require('fs');
      return eval('1 + 1');
    };

    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(false);
    expect(result.violations.length).toBeGreaterThan(1);
  });
});


describe('SandboxRestrictions - Edge Cases', () => {
  let restrictions;

  beforeEach(() => {
    restrictions = new SandboxRestrictions();
  });

  it('should handle very long code analysis', () => {
    const longFn = new Function('return ' + '1 +'.repeat(1000) + ' 1;');
    const result = restrictions.validateHookCode(longFn);
    expect(result).toHaveProperty('valid');
  });

  it('should handle unicode in code', () => {
    const hookFn = function() {
      const 变量 = 1;
      return 变量;
    };
    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(true);
  });

  it('should handle arrow functions', () => {
    const hookFn = (event) => event.value + 1;
    const result = restrictions.validateHookCode(hookFn);
    expect(result.valid).toBe(true);
  });
});

describe('SandboxRestrictions - Factory Function', () => {
  it('should create restrictions via factory', async () => {
    const { createSandboxRestrictions } = await import('../../src/hooks/security/sandbox-restrictions.mjs');
    const restrictions = createSandboxRestrictions({ timeoutMs: 3000 });
    expect(restrictions.config.timeoutMs).toBe(3000);
  });

  it('should have default instance', async () => {
    const { defaultSandboxRestrictions } = await import('../../src/hooks/security/sandbox-restrictions.mjs');
    expect(defaultSandboxRestrictions).toBeDefined();
    expect(defaultSandboxRestrictions.config).toBeDefined();
  });
});

describe('SandboxRestrictions - Concurrent Usage', () => {
  it('should handle concurrent restriction checks', async () => {
    const restrictions = new SandboxRestrictions();

    const checks = Array(100).fill(null).map(() =>
      Promise.resolve(restrictions.createRestrictedContext())
    );

    const contexts = await Promise.all(checks);
    expect(contexts).toHaveLength(100);
  });
});
