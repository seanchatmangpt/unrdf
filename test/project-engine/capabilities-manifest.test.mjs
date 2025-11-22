/**
 * @vitest-environment node
 * @file Capabilities Manifest Tests (Chicago School TDD)
 */

import { describe, it, expect } from 'vitest';
import {
  CODE_COMPLEXITY_JS,
  CAPABILITIES,
  FEATURE_FLAGS,
  isCapabilityEnabled,
  getCapabilityMetadata,
  getEnabledCapabilities,
  setCapabilityEnabled,
} from '../../src/project-engine/capabilities-manifest.mjs';

describe('Capabilities Manifest', () => {
  describe('CODE_COMPLEXITY_JS constant', () => {
    it('should define metadata for code complexity capability', () => {
      expect(CODE_COMPLEXITY_JS).toBeDefined();
      expect(CODE_COMPLEXITY_JS.id).toBe('code_complexity_js');
      expect(CODE_COMPLEXITY_JS.name).toBe('JavaScript Code Complexity Analysis');
    });

    it('should include feature flag configuration', () => {
      expect(CODE_COMPLEXITY_JS.featureFlag).toBeDefined();
      expect(CODE_COMPLEXITY_JS.featureFlag.name).toBe('code_complexity_js');
      expect(CODE_COMPLEXITY_JS.featureFlag.status).toBe('stable');
      expect(CODE_COMPLEXITY_JS.featureFlag.enabled).toBe(true);
    });

    it('should reference correct module path', () => {
      expect(CODE_COMPLEXITY_JS.module).toBe('./code-complexity-js.mjs');
      expect(CODE_COMPLEXITY_JS.exportName).toBe('analyzeJsComplexity');
    });

    it('should include default configuration', () => {
      expect(CODE_COMPLEXITY_JS.config).toBeDefined();
      expect(CODE_COMPLEXITY_JS.config.mode).toBe('observe');
      expect(CODE_COMPLEXITY_JS.config.excludePatterns).toContain('**/node_modules/**');
    });
  });

  describe('CAPABILITIES array', () => {
    it('should contain at least the code complexity capability', () => {
      expect(CAPABILITIES).toBeInstanceOf(Array);
      expect(CAPABILITIES.length).toBeGreaterThan(0);
      expect(CAPABILITIES.some(cap => cap.id === 'code_complexity_js')).toBe(true);
    });

    it('should have code complexity as first/primary capability', () => {
      expect(CAPABILITIES[0].id).toBe('code_complexity_js');
    });
  });

  describe('FEATURE_FLAGS map', () => {
    it('should be a Map instance', () => {
      expect(FEATURE_FLAGS).toBeInstanceOf(Map);
    });

    it('should have code_complexity_js flag enabled by default', () => {
      expect(FEATURE_FLAGS.has('code_complexity_js')).toBe(true);
      expect(FEATURE_FLAGS.get('code_complexity_js')).toBe(true);
    });
  });

  describe('isCapabilityEnabled()', () => {
    it('should return true for enabled capabilities', () => {
      expect(isCapabilityEnabled('code_complexity_js')).toBe(true);
    });

    it('should return false for non-existent capabilities', () => {
      expect(isCapabilityEnabled('nonexistent_capability')).toBe(false);
    });

    it('should respect runtime overrides', () => {
      const overrides = { code_complexity_js: false };
      expect(isCapabilityEnabled('code_complexity_js', overrides)).toBe(false);
    });

    it('should enable new capabilities via overrides', () => {
      const overrides = { hypothetical_feature: true };
      expect(isCapabilityEnabled('hypothetical_feature', overrides)).toBe(true);
    });
  });

  describe('getCapabilityMetadata()', () => {
    it('should return metadata for known capabilities', () => {
      const meta = getCapabilityMetadata('code_complexity_js');
      expect(meta).toBeDefined();
      expect(meta?.id).toBe('code_complexity_js');
      expect(meta?.name).toBe('JavaScript Code Complexity Analysis');
    });

    it('should return null for unknown capabilities', () => {
      const meta = getCapabilityMetadata('unknown_capability');
      expect(meta).toBeNull();
    });

    it('should include all required metadata fields', () => {
      const meta = getCapabilityMetadata('code_complexity_js');
      expect(meta?.id).toBeDefined();
      expect(meta?.name).toBeDefined();
      expect(meta?.description).toBeDefined();
      expect(meta?.phase).toBeDefined();
      expect(meta?.module).toBeDefined();
      expect(meta?.exportName).toBeDefined();
      expect(meta?.version).toBeDefined();
      expect(meta?.tags).toBeDefined();
      expect(meta?.featureFlag).toBeDefined();
    });
  });

  describe('getEnabledCapabilities()', () => {
    it('should return array of enabled capabilities', () => {
      const enabled = getEnabledCapabilities();
      expect(Array.isArray(enabled)).toBe(true);
      expect(enabled.length).toBeGreaterThan(0);
    });

    it('should include code_complexity_js by default', () => {
      const enabled = getEnabledCapabilities();
      expect(enabled.some(cap => cap.id === 'code_complexity_js')).toBe(true);
    });

    it('should respect feature flag overrides', () => {
      const overrides = { code_complexity_js: false };
      const enabled = getEnabledCapabilities(overrides);
      expect(enabled.some(cap => cap.id === 'code_complexity_js')).toBe(false);
    });

    it('should return empty array when all capabilities disabled', () => {
      const overrides = { code_complexity_js: false };
      const enabled = getEnabledCapabilities(overrides);
      expect(enabled.length).toBe(0);
    });
  });

  describe('setCapabilityEnabled()', () => {
    it('should disable capability at runtime', () => {
      setCapabilityEnabled('code_complexity_js', false);
      expect(isCapabilityEnabled('code_complexity_js')).toBe(false);

      // Restore original state
      setCapabilityEnabled('code_complexity_js', true);
    });

    it('should re-enable capability', () => {
      setCapabilityEnabled('code_complexity_js', false);
      expect(isCapabilityEnabled('code_complexity_js')).toBe(false);

      setCapabilityEnabled('code_complexity_js', true);
      expect(isCapabilityEnabled('code_complexity_js')).toBe(true);
    });

    it('should persist across multiple calls', () => {
      setCapabilityEnabled('code_complexity_js', false);
      setCapabilityEnabled('code_complexity_js', false);
      expect(isCapabilityEnabled('code_complexity_js')).toBe(false);

      // Restore
      setCapabilityEnabled('code_complexity_js', true);
    });
  });

  describe('Feature Flag Metadata', () => {
    it('should have version information', () => {
      const flag = CODE_COMPLEXITY_JS.featureFlag;
      expect(flag.since).toBe('4.0.0');
    });

    it('should have status indicating stability', () => {
      const flag = CODE_COMPLEXITY_JS.featureFlag;
      expect(['stable', 'beta', 'experimental', 'deprecated']).toContain(flag.status);
    });

    it('should provide human-readable description', () => {
      const flag = CODE_COMPLEXITY_JS.featureFlag;
      expect(flag.description).toBeTruthy();
      expect(flag.description?.length).toBeGreaterThan(10);
    });
  });

  describe('Capability Phase Information', () => {
    it('should indicate pipeline phase for code complexity', () => {
      expect(CODE_COMPLEXITY_JS.phase).toBe('Phase 6.5');
    });

    it('should include searchable tags', () => {
      expect(CODE_COMPLEXITY_JS.tags).toContain('analysis');
      expect(CODE_COMPLEXITY_JS.tags).toContain('metrics');
      expect(CODE_COMPLEXITY_JS.tags).toContain('javascript');
    });
  });
});
