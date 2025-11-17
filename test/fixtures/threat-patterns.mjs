/**
 * @fileoverview Security Threat Patterns for Sandbox Testing
 * @module test/fixtures/threat-patterns
 *
 * @description
 * Comprehensive collection of VM escape attempts and security threat patterns
 * for testing isolated-vm and sandbox security measures.
 *
 * These patterns represent real-world attack vectors that sandboxes must defend against.
 */

/**
 * VM Escape Threat Patterns
 * Each pattern represents a known attack vector for escaping JavaScript sandboxes
 */
export const threatPatterns = [
  {
    id: 'constructor-escape',
    name: 'Constructor Chain Escape',
    description: 'Attempt to escape via constructor chain to access Function constructor',
    code: `
      (function(){}.constructor.constructor('return this')());
    `,
    expectedBehavior: 'Should be blocked or return sanitized context',
    severity: 'critical',
    category: 'constructor-manipulation'
  },

  {
    id: 'proto-pollution',
    name: 'Prototype Pollution',
    description: 'Attempt to pollute Object.prototype to inject malicious code',
    code: `
      Object.prototype.polluted = 'malicious';
      ({}).polluted;
    `,
    expectedBehavior: 'Should prevent prototype modification or isolate prototypes',
    severity: 'high',
    category: 'prototype-manipulation'
  },

  {
    id: 'process-access',
    name: 'Process Object Access',
    description: 'Attempt to access Node.js process object',
    code: `
      typeof process !== 'undefined' && process.exit(1);
    `,
    expectedBehavior: 'process should be undefined or sanitized',
    severity: 'critical',
    category: 'global-access'
  },

  {
    id: 'require-access',
    name: 'Require Function Access',
    description: 'Attempt to access Node.js require function',
    code: `
      typeof require !== 'undefined' && require('fs').readFileSync('/etc/passwd');
    `,
    expectedBehavior: 'require should be undefined',
    severity: 'critical',
    category: 'module-loading'
  },

  {
    id: 'import-meta',
    name: 'Import.meta Access',
    description: 'Attempt to access import.meta for file system information',
    code: `
      typeof import.meta !== 'undefined' && import.meta.url;
    `,
    expectedBehavior: 'import.meta should be sanitized or undefined',
    severity: 'medium',
    category: 'module-loading'
  },

  {
    id: 'global-this',
    name: 'GlobalThis Escape',
    description: 'Attempt to access global context via globalThis',
    code: `
      globalThis.constructor.constructor('return process')();
    `,
    expectedBehavior: 'Should not expose host process object',
    severity: 'critical',
    category: 'global-access'
  },

  {
    id: 'symbol-manipulation',
    name: 'Symbol Registry Manipulation',
    description: 'Attempt to access cross-realm symbols',
    code: `
      const key = Symbol.for('__INTERNAL__');
      globalThis[key] = 'compromised';
    `,
    expectedBehavior: 'Symbol registry should be isolated',
    severity: 'medium',
    category: 'symbol-manipulation'
  },

  {
    id: 'async-hooks',
    name: 'Async Hooks Escape',
    description: 'Attempt to access async_hooks for context inspection',
    code: `
      typeof process !== 'undefined' && process.binding('async_wrap');
    `,
    expectedBehavior: 'process.binding should be unavailable',
    severity: 'high',
    category: 'internal-access'
  },

  {
    id: 'buffer-allocation',
    name: 'Buffer Memory Access',
    description: 'Attempt to allocate large buffers for memory exhaustion',
    code: `
      const buffers = [];
      for (let i = 0; i < 1000; i++) {
        buffers.push(new ArrayBuffer(1024 * 1024 * 100)); // 100MB each
      }
    `,
    expectedBehavior: 'Should enforce memory limits',
    severity: 'high',
    category: 'resource-exhaustion'
  },

  {
    id: 'infinite-loop',
    name: 'Infinite Loop DoS',
    description: 'Attempt to execute infinite loop for denial of service',
    code: `
      while(true) { /* infinite loop */ }
    `,
    expectedBehavior: 'Should timeout and terminate',
    severity: 'medium',
    category: 'denial-of-service'
  },

  {
    id: 'eval-escape',
    name: 'Eval Escape',
    description: 'Attempt to use eval to execute arbitrary code',
    code: `
      eval('this.constructor.constructor("return process")()')
    `,
    expectedBehavior: 'eval should be disabled or sanitized',
    severity: 'critical',
    category: 'code-injection'
  },

  {
    id: 'wasm-escape',
    name: 'WebAssembly Escape',
    description: 'Attempt to execute WebAssembly for low-level access',
    code: `
      const wasmCode = new Uint8Array([0,97,115,109,1,0,0,0]);
      new WebAssembly.Module(wasmCode);
    `,
    expectedBehavior: 'WebAssembly should be disabled',
    severity: 'high',
    category: 'wasm-execution'
  },

  {
    id: 'proxy-trap',
    name: 'Proxy Trap Manipulation',
    description: 'Attempt to use Proxy traps to intercept security checks',
    code: `
      new Proxy({}, {
        get(target, prop) {
          if (prop === 'constructor') {
            return Function;
          }
        }
      });
    `,
    expectedBehavior: 'Proxy should be monitored or restricted',
    severity: 'medium',
    category: 'proxy-manipulation'
  }
];

/**
 * Benign Test Patterns
 * Safe code patterns that should execute successfully in sandbox
 */
export const benignPatterns = [
  {
    id: 'safe-math',
    name: 'Safe Mathematical Operations',
    code: `
      const result = Math.pow(2, 10) + Math.sqrt(144);
      result;
    `,
    expectedResult: 1036,
    category: 'safe-operations'
  },

  {
    id: 'safe-string',
    name: 'Safe String Manipulation',
    code: `
      const str = 'hello world';
      str.toUpperCase().split(' ').join('-');
    `,
    expectedResult: 'HELLO-WORLD',
    category: 'safe-operations'
  },

  {
    id: 'safe-array',
    name: 'Safe Array Operations',
    code: `
      const arr = [1, 2, 3, 4, 5];
      arr.map(x => x * 2).filter(x => x > 5).reduce((a, b) => a + b, 0);
    `,
    expectedResult: 24,
    category: 'safe-operations'
  },

  {
    id: 'safe-object',
    name: 'Safe Object Operations',
    code: `
      const obj = { a: 1, b: 2, c: 3 };
      Object.keys(obj).length;
    `,
    expectedResult: 3,
    category: 'safe-operations'
  },

  {
    id: 'safe-json',
    name: 'Safe JSON Operations',
    code: `
      const data = { name: 'test', value: 42 };
      JSON.parse(JSON.stringify(data)).value;
    `,
    expectedResult: 42,
    category: 'safe-operations'
  },

  {
    id: 'safe-async',
    name: 'Safe Async Operations',
    code: `
      (async () => {
        const promise = Promise.resolve(42);
        return await promise;
      })();
    `,
    expectedResult: Promise.resolve(42),
    category: 'safe-async'
  }
];

/**
 * Performance Test Patterns
 * Patterns for testing sandbox performance characteristics
 */
export const performancePatterns = [
  {
    id: 'fibonacci',
    name: 'Fibonacci Calculation',
    code: `
      function fib(n) {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
      }
      fib(20);
    `,
    expectedMaxDuration: 1000, // ms
    category: 'cpu-intensive'
  },

  {
    id: 'array-iteration',
    name: 'Large Array Iteration',
    code: `
      const arr = Array(100000).fill(0).map((_, i) => i);
      arr.reduce((sum, val) => sum + val, 0);
    `,
    expectedMaxDuration: 500,
    category: 'memory-intensive'
  },

  {
    id: 'string-concat',
    name: 'String Concatenation',
    code: `
      let str = '';
      for (let i = 0; i < 10000; i++) {
        str += 'test';
      }
      str.length;
    `,
    expectedMaxDuration: 300,
    category: 'string-intensive'
  }
];

/**
 * Memory Safety Patterns
 * Patterns for testing memory management and limits
 */
export const memorySafetyPatterns = [
  {
    id: 'circular-reference',
    name: 'Circular Reference Handling',
    code: `
      const obj1 = {};
      const obj2 = { ref: obj1 };
      obj1.ref = obj2;
      JSON.stringify(obj1); // Should handle or error gracefully
    `,
    shouldThrow: true,
    category: 'memory-safety'
  },

  {
    id: 'large-object',
    name: 'Large Object Allocation',
    code: `
      const large = {};
      for (let i = 0; i < 100000; i++) {
        large['key' + i] = { data: Array(100).fill(i) };
      }
      Object.keys(large).length;
    `,
    expectedMaxMemory: 50 * 1024 * 1024, // 50MB
    category: 'memory-limits'
  }
];

/**
 * Get threat pattern by ID
 * @param {string} id - Threat pattern ID
 * @returns {Object|null} Threat pattern or null
 */
export function getThreatPattern(id) {
  return threatPatterns.find(p => p.id === id) || null;
}

/**
 * Get threat patterns by category
 * @param {string} category - Category name
 * @returns {Array} Array of threat patterns
 */
export function getThreatPatternsByCategory(category) {
  return threatPatterns.filter(p => p.category === category);
}

/**
 * Get threat patterns by severity
 * @param {string} severity - Severity level (critical, high, medium, low)
 * @returns {Array} Array of threat patterns
 */
export function getThreatPatternsBySeverity(severity) {
  return threatPatterns.filter(p => p.severity === severity);
}

/**
 * Get all critical threats
 * @returns {Array} Array of critical threat patterns
 */
export function getCriticalThreats() {
  return getThreatPatternsBySeverity('critical');
}

/**
 * Validate sandbox against all threat patterns
 * @param {Function} executeCode - Function to execute code in sandbox
 * @returns {Promise<Object>} Validation results
 */
export async function validateSandbox(executeCode) {
  const results = {
    total: threatPatterns.length,
    blocked: 0,
    escaped: 0,
    errors: 0,
    details: []
  };

  for (const pattern of threatPatterns) {
    try {
      const result = await executeCode(pattern.code);

      // Check if threat was properly neutralized
      if (result === null || result === undefined || typeof result === 'object' && !result.constructor) {
        results.blocked++;
        results.details.push({
          pattern: pattern.id,
          status: 'blocked',
          severity: pattern.severity
        });
      } else {
        results.escaped++;
        results.details.push({
          pattern: pattern.id,
          status: 'escaped',
          severity: pattern.severity,
          result
        });
      }
    } catch (error) {
      results.errors++;
      results.details.push({
        pattern: pattern.id,
        status: 'error',
        severity: pattern.severity,
        error: error.message
      });
    }
  }

  return results;
}

export default {
  threatPatterns,
  benignPatterns,
  performancePatterns,
  memorySafetyPatterns,
  getThreatPattern,
  getThreatPatternsByCategory,
  getThreatPatternsBySeverity,
  getCriticalThreats,
  validateSandbox
};
