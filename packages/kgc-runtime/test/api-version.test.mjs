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
    expect(versionManager.isCompatible('[VERSION]', '[VERSION]')).toBe(true);

    // Higher minor = compatible
    expect(versionManager.isCompatible('[VERSION]', '[VERSION]')).toBe(true);

    // Lower minor = not compatible
    expect(versionManager.isCompatible('[VERSION]', '[VERSION]')).toBe(false);

    // Different major = not compatible
    expect(versionManager.isCompatible('[VERSION]', '[VERSION]')).toBe(false);
  });

  it('should detect deprecated versions', () => {
    const deprecated = versionManager.isDeprecated('[VERSION]');
    expect(deprecated).toBe(true);

    const stable = versionManager.isDeprecated('[VERSION]');
    expect(stable).toBe(false);
  });

  it('should calculate days until removal for deprecated versions', () => {
    const days = versionManager.getDaysUntilRemoval('[VERSION]');
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
    expect(isPluginCompatible('[VERSION]')).toBe(true);
    expect(isPluginCompatible('[VERSION]')).toBe(true);
  });

  it('should reject incompatible plugin versions', () => {
    expect(isPluginCompatible('[VERSION]')).toBe(false);
    expect(isPluginCompatible('[VERSION]')).toBe(false);
  });

  it('should throw for removed versions', () => {
    expect(() => {
      validatePluginVersion('[VERSION]');
    }).toThrow(/has been removed/);
  });
});
