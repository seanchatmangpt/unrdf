/**
 * API Version Tests
 */

import { describe, it, expect } from 'vitest';
import {
  APIVersionManager,
  CURRENT_API_VERSION,
  isPluginCompatible,
  validatePluginVersion,
} from '../src/api-version.mjs';

describe('APIVersionManager', () => {
  let versionManager;

  beforeEach(() => {
    versionManager = new APIVersionManager();
  });

  it('should return current API version', () => {
    expect(versionManager.getCurrentVersion()).toBe(CURRENT_API_VERSION);
  });

  it('should check version compatibility', () => {
    // Same major.minor = compatible
    expect(versionManager.isCompatible('5.0.0', '5.0.1')).toBe(true);

    // Higher minor = compatible
    expect(versionManager.isCompatible('5.0.0', '5.1.0')).toBe(true);

    // Lower minor = not compatible
    expect(versionManager.isCompatible('5.1.0', '5.0.0')).toBe(false);

    // Different major = not compatible
    expect(versionManager.isCompatible('5.0.0', '6.0.0')).toBe(false);
  });

  it('should detect deprecated versions', () => {
    const deprecated = versionManager.isDeprecated('4.0.0');
    expect(deprecated).toBe(true);

    const stable = versionManager.isDeprecated('5.0.1');
    expect(stable).toBe(false);
  });

  it('should calculate days until removal for deprecated versions', () => {
    const days = versionManager.getDaysUntilRemoval('4.0.0');
    expect(typeof days).toBe('number');
  });

  it('should list all versions', () => {
    const versions = versionManager.listVersions();
    expect(versions.length).toBeGreaterThan(0);
    expect(versions.some(v => v.version === CURRENT_API_VERSION)).toBe(true);
  });
});

describe('Plugin Version Validation', () => {
  it('should validate compatible plugin versions', () => {
    expect(isPluginCompatible('5.0.0')).toBe(true);
    expect(isPluginCompatible('5.0.1')).toBe(true);
  });

  it('should reject incompatible plugin versions', () => {
    expect(isPluginCompatible('6.0.0')).toBe(false);
    expect(isPluginCompatible('4.0.0')).toBe(false);
  });

  it('should throw for removed versions', () => {
    expect(() => {
      validatePluginVersion('3.0.0');
    }).toThrow(/has been removed/);
  });
});
