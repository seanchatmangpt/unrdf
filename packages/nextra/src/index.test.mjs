/**
 * @file Nextra package basic tests
 */

import { describe, it, expect } from 'vitest';
import { metadata, getPackageInfo, isInstalled } from './index.mjs';

describe('@unrdf/nextra-docs', () => {
  it('should export package metadata', () => {
    expect(metadata).toBeDefined();
    expect(metadata.name).toBe('@unrdf/nextra-docs');
    expect(metadata.version).toBe('5.0.1');
  });

  it('should return package info', () => {
    const info = getPackageInfo();
    expect(info).toEqual(metadata);
  });

  it('should validate installation', () => {
    expect(isInstalled()).toBe(true);
  });
});
