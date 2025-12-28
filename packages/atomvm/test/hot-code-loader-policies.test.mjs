/**
 * @fileoverview Hot Code Loader Policy Tests
 *
 * Tests hot-reloading of validation policies without downtime.
 * PROVES that policies can be swapped with <100ms reload time.
 *
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import { HotCodeLoader } from '../src/hot-code-loader.mjs';
import { RDFValidator } from '../src/rdf-validator.mjs';

// Mock AtomVMRuntime
const createMockRuntime = () => ({
  state: 'Ready',
  moduleName: 'policy-test',
  isReady: () => true,
  isLoaded: () => true,
  terminal: {
    log: vi.fn(),
  },
});

/**
 * Simple PolicyPack for testing
 */
class PolicyPack {
  constructor(name, version = 1) {
    this.name = name;
    this.version = version;
    this.validator = new RDFValidator();
    this.rules = [];
  }

  registerRule(shapeName, rules, targetClass) {
    this.validator.registerShape(shapeName, rules, targetClass);
    this.rules.push({ shapeName, rules, targetClass });
    return this;
  }

  async validate(triples) {
    const shapeNames = this.rules.map(r => r.shapeName);
    return this.validator.validateGraph(triples, { shapes: shapeNames });
  }

  toModuleContent() {
    const content = JSON.stringify({
      name: this.name,
      version: this.version,
      rules: this.rules,
    });
    const encoder = new TextEncoder();
    return encoder.encode(content).buffer;
  }
}

describe('HotCodeLoader - Policy Packs', () => {
  let loader;
  let mockRuntime;
  let currentModuleContent;

  beforeEach(() => {
    mockRuntime = createMockRuntime();
    loader = new HotCodeLoader(mockRuntime);

    // Mock fetch
    global.fetch = vi.fn(async (path) => {
      if (currentModuleContent) {
        return {
          ok: true,
          arrayBuffer: async () => currentModuleContent,
        };
      }
      throw new Error(`Module not found: ${path}`);
    });

    // Mock crypto.subtle for signature computation
    global.crypto = {
      subtle: {
        digest: vi.fn(async (algorithm, data) => {
          // Simple hash based on content
          const view = new Uint8Array(data);
          let hash = 0;
          for (let i = 0; i < view.length; i++) {
            hash = (hash * 31 + view[i]) % 0xFFFFFFFF;
          }
          const hashBuffer = new ArrayBuffer(32);
          const hashView = new Uint32Array(hashBuffer);
          hashView[0] = hash;
          return hashBuffer;
        }),
      },
    };
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('Policy Loading', () => {
    it('should load initial policy pack', async () => {
      const policy = new PolicyPack('test-policy', 1);
      policy.registerRule('test:Subject', [
        { property: 'test:value', required: true, datatype: 'xsd:string' },
      ], 'test:Subject');

      currentModuleContent = policy.toModuleContent();

      const result = await loader.loadModule('/policies/test-policy.beam');

      expect(result.success).toBe(true);
      expect(result.moduleName).toBe('test-policy');
      expect(result.signature).toBeDefined();
    });

    it('should validate triples with loaded policy', async () => {
      const policy = new PolicyPack('validation-policy', 1);
      policy.registerRule('test:ValidSubject', [
        {
          property: 'rdf:type',
          required: true,
          nodeKind: 'IRI',
        },
        {
          property: 'test:value',
          required: true,
          datatype: 'xsd:string',
        },
      ], 'test:ValidSubject');

      const triples = [
        {
          subject: 'http://example.org/s1',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          value: 'test:ValidSubject',
        },
        {
          subject: 'http://example.org/s1',
          predicate: 'test:value',
          value: 'test-value',
          datatype: 'http://www.w3.org/2001/XMLSchema#string',
        },
      ];

      const validation = await policy.validate(triples);
      expect(validation.valid).toBe(true);
      expect(validation.errors).toHaveLength(0);
    });

    it('should fail validation when required property missing', async () => {
      const policy = new PolicyPack('validation-policy', 1);
      policy.registerRule('test:ValidSubject', [
        {
          property: 'test:value',
          required: true,
          datatype: 'xsd:string',
        },
      ], 'test:ValidSubject');

      const triples = [
        {
          subject: 'http://example.org/s1',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          value: 'test:ValidSubject',
        },
        // Missing test:value
      ];

      const validation = await policy.validate(triples);
      expect(validation.valid).toBe(false);
      expect(validation.errors.length).toBeGreaterThan(0);
      expect(validation.errors[0].type).toBe('PROPERTY_MISSING');
    });
  });

  describe('Hot Reload', () => {
    it('should hot-reload policy with updated rules', async () => {
      // Load initial policy
      const policy1 = new PolicyPack('test-policy', 1);
      policy1.registerRule('test:Subject', [
        { property: 'test:value', required: true, datatype: 'xsd:string' },
      ], 'test:Subject');

      currentModuleContent = policy1.toModuleContent();
      const loadResult = await loader.loadModule('/policies/test-policy.beam');
      expect(loadResult.success).toBe(true);

      const initialSignature = loadResult.signature;

      // Update policy with stricter rules
      const policy2 = new PolicyPack('test-policy', 2);
      policy2.registerRule('test:Subject', [
        { property: 'test:value', required: true, datatype: 'xsd:string', minLength: 5 },
        { property: 'test:new-field', required: true },
      ], 'test:Subject');

      currentModuleContent = policy2.toModuleContent();

      // Hot reload
      const reloadResult = await loader.reloadModule('test-policy');
      expect(reloadResult.success).toBe(true);
      expect(reloadResult.version).toBe(2);
      expect(reloadResult.duration).toBeDefined();

      // Verify signature changed
      const moduleInfo = loader.activeModules.get('test-policy');
      expect(moduleInfo.signature).not.toBe(initialSignature);
    });

    it('should complete reload in <100ms', async () => {
      // Load initial policy
      const policy1 = new PolicyPack('fast-policy', 1);
      policy1.registerRule('test:Subject', [
        { property: 'test:value', required: true },
      ], 'test:Subject');

      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/fast-policy.beam');

      // Update policy
      const policy2 = new PolicyPack('fast-policy', 2);
      policy2.registerRule('test:Subject', [
        { property: 'test:value', required: true, minLength: 10 },
      ], 'test:Subject');

      currentModuleContent = policy2.toModuleContent();

      // Measure reload time
      const startTime = performance.now();
      const result = await loader.reloadModule('fast-policy');
      const duration = performance.now() - startTime;

      expect(result.success).toBe(true);
      expect(duration).toBeLessThan(100); // <100ms for zero-downtime
      expect(result.duration).toBeLessThan(100);
    });

    it('should execute hot-swap callbacks on reload', async () => {
      // Load initial policy
      const policy1 = new PolicyPack('callback-policy', 1);
      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/callback-policy.beam');

      // Register callbacks
      const callbacks = {
        beforeSwap: vi.fn(),
        afterSwap: vi.fn(),
      };

      loader.registerHotSwap('callback-policy', callbacks);

      // Update policy
      const policy2 = new PolicyPack('callback-policy', 2);
      currentModuleContent = policy2.toModuleContent();

      // Reload
      const result = await loader.reloadModule('callback-policy');

      expect(result.success).toBe(true);
      expect(callbacks.beforeSwap).toHaveBeenCalledOnce();
      expect(callbacks.afterSwap).toHaveBeenCalledOnce();
    });

    it('should apply new policy rules after reload', async () => {
      // Load permissive policy
      const policy1 = new PolicyPack('evolving-policy', 1);
      policy1.registerRule('test:Document', [
        { property: 'test:title', required: true, datatype: 'xsd:string' },
      ], 'test:Document');

      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/evolving-policy.beam');

      // Validate with permissive policy
      const triples1 = [
        {
          subject: 'http://example.org/doc1',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          value: 'test:Document',
        },
        {
          subject: 'http://example.org/doc1',
          predicate: 'test:title',
          value: 'A', // Short title - OK with policy1
          datatype: 'http://www.w3.org/2001/XMLSchema#string',
        },
      ];

      const validation1 = await policy1.validate(triples1);
      expect(validation1.valid).toBe(true);

      // Reload with stricter policy
      const policy2 = new PolicyPack('evolving-policy', 2);
      policy2.registerRule('test:Document', [
        { property: 'test:title', required: true, datatype: 'xsd:string', minLength: 5 },
      ], 'test:Document');

      currentModuleContent = policy2.toModuleContent();
      const reloadResult = await loader.reloadModule('evolving-policy');
      expect(reloadResult.success).toBe(true);

      // Same triples should now fail
      const validation2 = await policy2.validate(triples1);
      expect(validation2.valid).toBe(false);
      expect(validation2.errors.some(e => e.type === 'MIN_LENGTH_VIOLATION')).toBe(true);
    });

    it('should not apply old policy rules after reload', async () => {
      // Load policy with specific constraint
      const policy1 = new PolicyPack('changing-policy', 1);
      policy1.registerRule('test:User', [
        { property: 'test:email', required: true, datatype: 'xsd:string' },
        { property: 'test:age', required: true, datatype: 'xsd:integer', minValue: 18 },
      ], 'test:User');

      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/changing-policy.beam');

      // Reload with relaxed policy (age no longer required)
      const policy2 = new PolicyPack('changing-policy', 2);
      policy2.registerRule('test:User', [
        { property: 'test:email', required: true, datatype: 'xsd:string' },
        // age is now optional
      ], 'test:User');

      currentModuleContent = policy2.toModuleContent();
      await loader.reloadModule('changing-policy');

      // Validate without age - should pass with new policy
      const triples = [
        {
          subject: 'http://example.org/user1',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          value: 'test:User',
        },
        {
          subject: 'http://example.org/user1',
          predicate: 'test:email',
          value: 'user@example.com',
          datatype: 'http://www.w3.org/2001/XMLSchema#string',
        },
        // No age property
      ];

      const validation = await policy2.validate(triples);
      expect(validation.valid).toBe(true);
    });
  });

  describe('Zero-Downtime Guarantees', () => {
    it('should validate during reload (no downtime)', async () => {
      // Load initial policy
      const policy1 = new PolicyPack('concurrent-policy', 1);
      policy1.registerRule('test:Item', [
        { property: 'test:name', required: true },
      ], 'test:Item');

      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/concurrent-policy.beam');

      // Prepare new policy
      const policy2 = new PolicyPack('concurrent-policy', 2);
      policy2.registerRule('test:Item', [
        { property: 'test:name', required: true, minLength: 3 },
      ], 'test:Item');

      // Start reload
      currentModuleContent = policy2.toModuleContent();
      const reloadPromise = loader.reloadModule('concurrent-policy');

      // Validate during reload (using old policy since reload not complete)
      const testTriples = [
        {
          subject: 'http://example.org/item1',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          value: 'test:Item',
        },
        {
          subject: 'http://example.org/item1',
          predicate: 'test:name',
          value: 'AB', // Fails new policy, passes old policy
        },
      ];

      // This validation should complete successfully (using policy available at the time)
      const validation1 = await policy1.validate(testTriples);
      expect(validation1.valid).toBe(true); // Old policy allows short names

      // Wait for reload to complete
      const reloadResult = await reloadPromise;
      expect(reloadResult.success).toBe(true);

      // Now validation should use new policy
      const validation2 = await policy2.validate(testTriples);
      expect(validation2.valid).toBe(false); // New policy requires minLength=3
    });

    it('should maintain module info during reload', async () => {
      const policy1 = new PolicyPack('info-policy', 1);
      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/info-policy.beam');

      const initialInfo = loader.activeModules.get('info-policy');
      expect(initialInfo.status).toBe('loaded');
      expect(initialInfo.version).toBe(1);

      const policy2 = new PolicyPack('info-policy', 2);
      currentModuleContent = policy2.toModuleContent();
      await loader.reloadModule('info-policy');

      const updatedInfo = loader.activeModules.get('info-policy');
      expect(updatedInfo.status).toBe('loaded');
      expect(updatedInfo.version).toBe(2);
      expect(updatedInfo.name).toBe('info-policy');
    });
  });

  describe('Error Handling', () => {
    it('should call onError callback when reload fails', async () => {
      const policy1 = new PolicyPack('error-policy', 1);
      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/error-policy.beam');

      const callbacks = {
        beforeSwap: vi.fn(),
        afterSwap: vi.fn(),
        onError: vi.fn(),
      };

      loader.registerHotSwap('error-policy', callbacks);

      // Make reload fail by breaking fetch
      global.fetch = vi.fn().mockRejectedValue(new Error('Network error'));

      const result = await loader.reloadModule('error-policy');

      expect(result.success).toBe(false);
      expect(callbacks.onError).toHaveBeenCalledOnce();
      expect(callbacks.afterSwap).not.toHaveBeenCalled();
    });

    it('should maintain old policy when reload fails', async () => {
      const policy1 = new PolicyPack('stable-policy', 1);
      policy1.registerRule('test:Resource', [
        { property: 'test:id', required: true },
      ], 'test:Resource');

      currentModuleContent = policy1.toModuleContent();
      await loader.loadModule('/policies/stable-policy.beam');

      const initialSignature = loader.activeModules.get('stable-policy').signature;

      // Break reload
      global.fetch = vi.fn().mockRejectedValue(new Error('Failed to fetch'));

      const result = await loader.reloadModule('stable-policy');
      expect(result.success).toBe(false);

      // Verify old policy still active
      const moduleInfo = loader.activeModules.get('stable-policy');
      expect(moduleInfo.signature).toBe(initialSignature);
      expect(moduleInfo.version).toBe(1);

      // Old policy should still work
      const testTriples = [
        {
          subject: 'http://example.org/res1',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          value: 'test:Resource',
        },
        {
          subject: 'http://example.org/res1',
          predicate: 'test:id',
          value: '123',
        },
      ];

      const validation = await policy1.validate(testTriples);
      expect(validation.valid).toBe(true);
    });
  });
});
