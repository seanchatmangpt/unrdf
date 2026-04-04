/**
 * @vitest-environment node
 * @file Effect Sandbox Security Tests - 50+ code injection attack patterns
 *
 * Tests the hardened effect executor's defenses against:
 * - Function constructor escape attempts
 * - eval() execution attempts
 * - Prototype pollution attacks
 * - require() injection attempts
 * - __proto__ manipulation
 * - constructor.prototype access
 * - Indirect eval and dynamic code execution
 * - Symbol-based sandbox escapes
 * - Timer-based code execution
 * - WebAssembly / SharedArrayBuffer attacks
 */
import { describe, it, expect, beforeEach } from 'vitest';
import {
  validateCodeInjection,
  validateFunction,
  createHardenedContext,
  createSafeFunctionProxy,
  logInjectionAttempt,
  executeHardened,
} from '../src/hooks/effect-executor.mjs';

// ============================================================================
// Code Validation Tests
// ============================================================================

describe('validateCodeInjection - input validation', () => {
  it('should reject non-string input', () => {
    const result = validateCodeInjection(42);
    expect(result.valid).toBe(false);
    expect(result.violations).toContain('Code must be a string');
  });

  it('should reject code exceeding max length', () => {
    const longCode = 'a'.repeat(200000);
    const result = validateCodeInjection(longCode);
    expect(result.valid).toBe(false);
    expect(result.violations[0]).toContain('exceeds maximum length');
  });

  it('should allow safe code', () => {
    const result = validateCodeInjection('const x = 1 + 2; return x;');
    expect(result.valid).toBe(true);
    expect(result.violations).toHaveLength(0);
  });

  it('should allow safe arrow function code', () => {
    const result = validateCodeInjection('(event) => event.data + 1');
    expect(result.valid).toBe(true);
  });
});

// ============================================================================
// Function Constructor Escape Attempts (Tests 1-8)
// ============================================================================

describe('Function constructor escape attempts', () => {
  it('1. should block new Function() constructor', () => {
    const code = 'new Function("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('Function'))).toBe(true);
  });

  it('2. should block Function() without new', () => {
    const code = 'Function("return process")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('3. should block Function constructor with template literals', () => {
    const code = 'new Function(`return process.env`)()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('4. should block Function.prototype access', () => {
    const code = 'Function.prototype.call.apply(eval)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('Function.prototype'))).toBe(true);
  });

  it('5. should block Function.constructor chaining', () => {
    const code = 'Function.constructor("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('6. should block Function bracket access', () => {
    const code = 'Function["prototype"]["constructor"]("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('7. should block arrow function to Function constructor', () => {
    const code = 'const f = Function; f("return process")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('8. should block multi-arg Function constructor', () => {
    const code = 'new Function("a", "b", "return a + b")(1, 2)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// eval() Execution Attempts (Tests 9-16)
// ============================================================================

describe('eval() execution attempts', () => {
  it('9. should block direct eval()', () => {
    const code = 'eval("1 + 1")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('eval'))).toBe(true);
  });

  it('10. should block eval with variable', () => {
    const code = 'const code = "process.exit()"; eval(code)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('11. should block indirect eval via comma operator', () => {
    const code = '(0, eval)("return this")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('indirect eval'))).toBe(true);
  });

  it('12. should block eval bracket access', () => {
    const code = 'eval["call"](null, "1+1")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('13. should block eval via string reference', () => {
    const code = 'const fn = ("eval"); this[fn]("code")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('14. should block eval in template literal', () => {
    const code = 'eval(`process.env.SECRET`)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('15. should block eval with concat', () => {
    const code = 'eval("pro" + "cess.exit()")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('16. should block eval in nested expression', () => {
    const code = 'const result = [eval("1")][0]';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Prototype Pollution Attacks (Tests 17-26)
// ============================================================================

describe('Prototype pollution attacks', () => {
  it('17. should block __proto__ direct access', () => {
    const code = 'obj.__proto__.polluted = true';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('__proto__'))).toBe(true);
  });

  it('18. should block __proto__ in assignment', () => {
    const code = 'const p = {}; p.__proto__ = { isAdmin: true }';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('19. should block __proto__ in object literal', () => {
    const code = '({ "__proto__": { polluted: true } })';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('20. should block prototype bracket access', () => {
    const code = 'Object.prototype["isAdmin"] = true';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('21. should block prototype.constructor chain', () => {
    const code = '"".constructor.prototype.constructor("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('22. should block Object.setPrototypeOf', () => {
    const code = 'Object.setPrototypeOf(target, maliciousProto)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('23. should block Object.defineProperty', () => {
    const code = 'Object.defineProperty(obj, "key", { value: "pwned" })';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('24. should block Object.defineProperties', () => {
    const code = 'Object.defineProperties(obj, { polluted: { value: true } })';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('25. should block Object.getPrototypeOf', () => {
    const code = 'Object.getPrototypeOf(obj).polluted = true';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('26. should block Reflect.setPrototypeOf', () => {
    const code = 'Reflect.setPrototypeOf(target, evil)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// require() Injection Attempts (Tests 27-32)
// ============================================================================

describe('require() injection attempts', () => {
  it('27. should block require("fs")', () => {
    const code = 'const fs = require("fs"); fs.readFileSync("/etc/passwd")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('require'))).toBe(true);
  });

  it('28. should block require("child_process")', () => {
    const code = 'require("child_process").execSync("rm -rf /")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('29. should block require with variable', () => {
    const code = 'const mod = "fs"; require(mod)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('30. should block dynamic import()', () => {
    const code = 'import("fs").then(fs => fs.readFileSync("/etc/passwd"))';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('import'))).toBe(true);
  });

  it('31. should block import.meta access', () => {
    const code = 'const url = import.meta.url';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('32. should block require with template literal', () => {
    const code = 'require(`child_process`).exec("whoami")';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// __proto__ Manipulation (Tests 33-37)
// ============================================================================

describe('__proto__ manipulation', () => {
  it('33. should block __proto__ getter escape', () => {
    const code = 'const proto = ({}).__proto__';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('34. should block nested __proto__ chains', () => {
    const code = 'a.__proto__.__proto__.__proto__.constructor';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('35. should block JSON parse __proto__ pollution', () => {
    const code = 'JSON.parse(\'{"__proto__": {"isAdmin": true}}\')';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('36. should block __proto__ in spread', () => {
    const code = 'const merged = { ...base, __proto__: evil }';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('37. should block __proto__ in destructuring context', () => {
    const code = 'const { __proto__: proto } = obj';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// constructor.prototype Access (Tests 38-43)
// ============================================================================

describe('constructor.prototype access', () => {
  it('38. should block string constructor escape', () => {
    const code = '"".constructor("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('39. should block array constructor escape', () => {
    const code = '[].constructor.constructor("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('40. should block number constructor escape', () => {
    const code = '(0).constructor.constructor("return process")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('41. should block regex constructor escape', () => {
    const code = '/a/.constructor.constructor("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('42. should block constructor via bracket notation', () => {
    const code = 'obj["constructor"]["constructor"]("return this")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('43. should block constructor via template literal bracket', () => {
    const code = 'obj[`constructor`].prototype';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Process/Global Access (Tests 44-48)
// ============================================================================

describe('Process and global access attempts', () => {
  it('44. should block process.env access', () => {
    const code = 'process.env.SECRET_KEY';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('process'))).toBe(true);
  });

  it('45. should block process.exit()', () => {
    const code = 'process.exit(1)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('46. should block process bracket access', () => {
    const code = 'process["env"]["SECRET"]';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('47. should block globalThis access', () => {
    const code = 'globalThis.process.exit()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('48. should block this.constructor escape', () => {
    const code = 'this.constructor.constructor("return process")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Timer-based Code Execution (Tests 49-51)
// ============================================================================

describe('Timer-based code execution', () => {
  it('49. should block setTimeout with string', () => {
    const code = 'setTimeout("process.exit()", 0)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('50. should block setInterval with string', () => {
    const code = 'setInterval("alert(1)", 1000)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('51. should block setImmediate', () => {
    const code = 'setImmediate(() => process.exit())';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// WebAssembly / SharedArrayBuffer (Tests 52-54)
// ============================================================================

describe('WebAssembly and SharedArrayBuffer attacks', () => {
  it('52. should block WebAssembly instantiation', () => {
    const code = 'WebAssembly.instantiate(buffer)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.some(v => v.includes('WebAssembly'))).toBe(true);
  });

  it('53. should block SharedArrayBuffer', () => {
    const code = 'new SharedArrayBuffer(1024)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('54. should block Atomics access', () => {
    const code = 'Atomics.wait(buffer, 0, 0)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Symbol-based Sandbox Escapes (Tests 55-57)
// ============================================================================

describe('Symbol-based sandbox escapes', () => {
  it('55. should block Symbol.unscopables manipulation', () => {
    const code = 'obj[Symbol.unscopables] = { x: true }';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('56. should block Symbol.hasInstance manipulation', () => {
    const code = 'class Evil { static [Symbol.hasInstance]() { return true; } }';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('57. should block Symbol.toPrimitive abuse', () => {
    const code = 'obj[Symbol.toPrimitive] = () => "evil"';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Reflect-based Attacks (Tests 58-60)
// ============================================================================

describe('Reflect-based attacks', () => {
  it('58. should block Reflect.construct', () => {
    const code = 'Reflect.construct(Function, ["return this"])()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('59. should block Reflect.defineProperty', () => {
    const code = 'Reflect.defineProperty(obj, "evil", { value: true })';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('60. should block Reflect.setPrototypeOf', () => {
    const code = 'Reflect.setPrototypeOf(target, evilProto)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Advanced / Combined Attacks (Tests 61-65)
// ============================================================================

describe('Advanced and combined attack patterns', () => {
  it('61. should block WeakRef constructor', () => {
    const code = 'new WeakRef(target)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('62. should block FinalizationRegistry', () => {
    const code = 'new FinalizationRegistry(callback)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('63. should block Proxy constructor', () => {
    const code = 'new Proxy(target, handler)';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('64. should block this["constructor"] bracket access', () => {
    const code = 'this["constructor"]("return process")()';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
  });

  it('65. should detect multiple violations in one code string', () => {
    const code = 'eval("1"); new Function("2"); require("fs"); process.exit(); __proto__.x = 1';
    const result = validateCodeInjection(code);
    expect(result.valid).toBe(false);
    expect(result.violations.length).toBeGreaterThanOrEqual(5);
  });
});

// ============================================================================
// validateFunction Tests
// ============================================================================

describe('validateFunction', () => {
  it('should reject non-function input', () => {
    const result = validateFunction('not a function');
    expect(result.valid).toBe(false);
    expect(result.violations).toContain('Input must be a function');
  });

  it('should validate safe function', () => {
    const fn = (event) => event.data + 1;
    const result = validateFunction(fn);
    expect(result.valid).toBe(true);
  });

  it('should block function with eval', () => {
     
    const fn = function() { return eval('1'); };
    const result = validateFunction(fn);
    expect(result.valid).toBe(false);
  });

  it('should block function with require', () => {
    const fn = function() { return require('fs'); };
    const result = validateFunction(fn);
    expect(result.valid).toBe(false);
  });

  it('should block function accessing __proto__', () => {
    const fn = function(obj) { return obj.__proto__; };
    const result = validateFunction(fn);
    expect(result.valid).toBe(false);
  });
});

// ============================================================================
// Hardened Context Tests
// ============================================================================

describe('createHardenedContext', () => {
  let ctx;

  beforeEach(() => {
    ctx = createHardenedContext();
  });

  it('should provide safe Math', () => {
    expect(ctx.Math).toBe(Math);
  });

  it('should provide safe JSON', () => {
    expect(ctx.JSON).toBe(JSON);
  });

  it('should provide safe Array/String/Object/Number/Boolean', () => {
    expect(ctx.Array).toBe(Array);
    expect(ctx.String).toBe(String);
    expect(ctx.Object).toBe(Object);
    expect(ctx.Number).toBe(Number);
    expect(ctx.Boolean).toBe(Boolean);
  });

  it('should disable Function', () => {
    expect(ctx.Function).toBeUndefined();
  });

  it('should disable eval', () => {
    expect(ctx.eval).toBeUndefined();
  });

  it('should disable constructor', () => {
    expect(ctx.constructor).toBeUndefined();
  });

  it('should disable process', () => {
    expect(ctx.process).toBeUndefined();
  });

  it('should disable require', () => {
    expect(ctx.require).toBeUndefined();
  });

  it('should disable globalThis', () => {
    expect(ctx.globalThis).toBeUndefined();
  });

  it('should disable Proxy', () => {
    expect(ctx.Proxy).toBeUndefined();
  });

  it('should disable Reflect', () => {
    expect(ctx.Reflect).toBeUndefined();
  });

  it('should disable WebAssembly', () => {
    expect(ctx.WebAssembly).toBeUndefined();
  });

  it('should disable SharedArrayBuffer', () => {
    expect(ctx.SharedArrayBuffer).toBeUndefined();
  });

  it('should disable timers', () => {
    expect(ctx.setTimeout).toBeUndefined();
    expect(ctx.setInterval).toBeUndefined();
    expect(ctx.setImmediate).toBeUndefined();
  });

  it('should be frozen (immutable)', () => {
    expect(Object.isFrozen(ctx)).toBe(true);
    expect(() => { ctx.evil = true; }).toThrow();
  });

  it('should provide safe console', () => {
    expect(typeof ctx.console.log).toBe('function');
    expect(typeof ctx.console.warn).toBe('function');
    expect(typeof ctx.console.error).toBe('function');
    expect(Object.isFrozen(ctx.console)).toBe(true);
  });
});

// ============================================================================
// SafeFunction Proxy Tests
// ============================================================================

describe('createSafeFunctionProxy', () => {
  let safeFunction;

  beforeEach(() => {
    safeFunction = createSafeFunctionProxy();
  });

  it('should reject construction with new', () => {
    expect(() => new safeFunction('return 1')).toThrow();
  });

  it('should reject direct call', () => {
    expect(() => safeFunction('return 1')).toThrow('SecurityError');
  });

  it('should return undefined for .constructor', () => {
    expect(safeFunction.constructor).toBeUndefined();
  });

  it('should return undefined for .prototype', () => {
    expect(safeFunction.prototype).toBeUndefined();
  });

  it('should return undefined for .__proto__', () => {
    expect(safeFunction.__proto__).toBeUndefined();
  });

  it('should reject property assignment', () => {
    expect(() => { safeFunction.evil = true; }).toThrow('SecurityError');
  });

  it('should reject defineProperty', () => {
    expect(() => {
      Object.defineProperty(safeFunction, 'evil', { value: true });
    }).toThrow('SecurityError');
  });

  it('should reject deleteProperty', () => {
    expect(() => { delete safeFunction.name; }).toThrow('SecurityError');
  });

  it('should return null for getPrototypeOf', () => {
    expect(Object.getPrototypeOf(safeFunction)).toBeNull();
  });

  it('should reject setPrototypeOf', () => {
    expect(() => {
      Object.setPrototypeOf(safeFunction, {});
    }).toThrow('SecurityError');
  });

  it('should reject instanceof checks via Symbol.hasInstance', () => {
    // The Symbol.hasInstance getter returns a function that returns false
    expect(safeFunction[Symbol.hasInstance]).toBeDefined();
  });
});

// ============================================================================
// Injection Logging Tests
// ============================================================================

describe('logInjectionAttempt', () => {
  it('should log attempt with full context', () => {
    const log = [];
    const attempt = logInjectionAttempt('eval("evil")', ['Blocked: eval() call'], log);

    expect(log).toHaveLength(1);
    expect(attempt.code).toBe('eval("evil")');
    expect(attempt.violations).toEqual(['Blocked: eval() call']);
    expect(attempt.timestamp).toBeInstanceOf(Date);
    expect(attempt.codeHash).toBeDefined();
  });

  it('should truncate long code strings', () => {
    const log = [];
    const longCode = 'x'.repeat(1000);
    const attempt = logInjectionAttempt(longCode, ['test'], log);

    expect(attempt.code.length).toBe(500);
  });

  it('should handle non-string code gracefully', () => {
    const log = [];
    const attempt = logInjectionAttempt(null, ['test'], log);
    expect(attempt.code).toBe('');
  });

  it('should accumulate multiple attempts', () => {
    const log = [];
    logInjectionAttempt('eval("1")', ['eval'], log);
    logInjectionAttempt('require("fs")', ['require'], log);
    logInjectionAttempt('process.exit()', ['process'], log);

    expect(log).toHaveLength(3);
  });
});

// ============================================================================
// executeHardened Integration Tests
// ============================================================================

describe('executeHardened - safe execution', () => {
  it('should execute safe function successfully', async () => {
    const effect = (ctx) => ctx.value + 1;
    const result = await executeHardened(effect, { value: 5 }, { logAttempts: false });

    expect(result.success).toBe(true);
    expect(result.result).toBe(6);
    expect(result.blocked).toBe(false);
    expect(result.duration).toBeGreaterThanOrEqual(0);
  });

  it('should execute safe function returning object', async () => {
    const effect = (ctx) => ({ doubled: ctx.value * 2 });
    const result = await executeHardened(effect, { value: 3 }, { logAttempts: false });

    expect(result.success).toBe(true);
    expect(result.result).toEqual({ doubled: 6 });
  });

  it('should execute async function', async () => {
    const effect = async (ctx) => ctx.value * 3;
    const result = await executeHardened(effect, { value: 4 }, { logAttempts: false });

    expect(result.success).toBe(true);
    expect(result.result).toBe(12);
  });
});

describe('executeHardened - blocking injection attempts', () => {
  it('should block function with eval', async () => {
    const effect = function() { return eval('1'); };
    const result = await executeHardened(effect, {}, { logAttempts: false });

    expect(result.success).toBe(false);
    expect(result.blocked).toBe(true);
    expect(result.error).toContain('SecurityError');
    expect(result.violations.length).toBeGreaterThan(0);
  });

  it('should block function with require', async () => {
    const effect = function() { return require('fs'); };
    const result = await executeHardened(effect, {}, { logAttempts: false });

    expect(result.success).toBe(false);
    expect(result.blocked).toBe(true);
  });

  it('should block function with __proto__', async () => {
    const effect = function(obj) { return obj.__proto__; };
    const result = await executeHardened(effect, {}, { logAttempts: false });

    expect(result.success).toBe(false);
    expect(result.blocked).toBe(true);
  });

  it('should block function with constructor access', async () => {
    const effect = function() { return "".constructor.constructor("return this")(); };
    const result = await executeHardened(effect, {}, { logAttempts: false });

    expect(result.success).toBe(false);
    expect(result.blocked).toBe(true);
  });

  it('should block function with process access', async () => {
    const effect = function() { return process.env; };
    const result = await executeHardened(effect, {}, { logAttempts: false });

    expect(result.success).toBe(false);
    expect(result.blocked).toBe(true);
  });

  it('should log blocked attempts when logAttempts is true', async () => {
    const log = [];
    const effect = function() { return eval('1'); };
    await executeHardened(effect, {}, { logAttempts: true, _injectionLog: log });

    expect(log.length).toBeGreaterThan(0);
    expect(log[0].violations.length).toBeGreaterThan(0);
  });
});

describe('executeHardened - error handling', () => {
  it('should catch runtime errors in effect', async () => {
    const effect = () => { throw new Error('runtime boom'); };
    const result = await executeHardened(effect, {}, { logAttempts: false });

    expect(result.success).toBe(false);
    expect(result.error).toBe('runtime boom');
    expect(result.blocked).toBe(false);
  });

  it('should handle non-function effect gracefully', async () => {
    const result = await executeHardened('not a function', {}, { logAttempts: false });

    expect(result.success).toBe(false);
    expect(result.blocked).toBe(true);
  });
});
