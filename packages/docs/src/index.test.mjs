/**
 * @file Docs package basic tests
 */

import { describe, it, expect } from 'vitest';
import { metadata, getPackageInfo, isInstalled, getServerUtils } from './index.mjs';

describe('docs', () => {
  it('should export package metadata', () => {
    expect(metadata).toBeDefined();
    expect(metadata.name).toBe('docs');
    expect(metadata.version).toBe('5.0.1');
  });

  it('should return package info', () => {
    const info = getPackageInfo();
    expect(info).toEqual(metadata);
  });

  it('should validate installation', () => {
    expect(isInstalled()).toBe(true);
  });

  it('should return server utilities paths', () => {
    const utils = getServerUtils();
    expect(utils).toBeDefined();
    expect(utils.api).toBe('server/api');
    expect(utils.database).toBe('server/database');
    expect(utils.routes).toBe('server/routes');
    expect(utils.utils).toBe('server/utils');
  });
});
