/**
 * @file Project Engine Tests
 * @description Tests for golden structure generation and project materialization
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { generateGoldenStructure } from '../src/golden-structure.mjs';

describe('Golden Structure - Profile Loading', () => {
  it('should generate structure for react-feature-v1 profile', async () => {
    const store = await generateGoldenStructure({ profile: 'react-feature-v1' });

    expect(store).toBeDefined();
    expect(store.size).toBeGreaterThan(0);
  });

  it('should generate structure for next-app-router-v1 profile', async () => {
    const store = await generateGoldenStructure({ profile: 'next-app-router-v1' });

    expect(store).toBeDefined();
    expect(store.size).toBeGreaterThan(0);
  });

  it('should generate structure for nest-api-v1 profile', async () => {
    const store = await generateGoldenStructure({ profile: 'nest-api-v1' });

    expect(store).toBeDefined();
    expect(store.size).toBeGreaterThan(0);
  });

  it('should generate structure for next-pages-v1 profile', async () => {
    const store = await generateGoldenStructure({ profile: 'next-pages-v1' });

    expect(store).toBeDefined();
  });

  it('should generate structure for express-api-v1 profile', async () => {
    const store = await generateGoldenStructure({ profile: 'express-api-v1' });

    expect(store).toBeDefined();
  });
});

describe('Golden Structure - Validation', () => {
  it('should throw on invalid profile', async () => {
    await expect(async () => {
      await generateGoldenStructure({ profile: 'invalid-profile' });
    }).rejects.toThrow();
  });

  it('should validate profile enum', async () => {
    const validProfiles = [
      'react-feature-v1',
      'next-app-router-v1',
      'next-pages-v1',
      'nest-api-v1',
      'express-api-v1',
    ];

    for (const profile of validProfiles) {
      const store = await generateGoldenStructure({ profile });
      expect(store).toBeDefined();
    }
  });
});

describe('Golden Structure - Content', () => {
  it('should contain base golden structure triples', async () => {
    const store = await generateGoldenStructure({ profile: 'react-feature-v1' });

    expect(store.size).toBeGreaterThan(5);
  });

  it('should generate different structures for different profiles', async () => {
    const reactStore = await generateGoldenStructure({ profile: 'react-feature-v1' });
    const nestStore = await generateGoldenStructure({ profile: 'nest-api-v1' });

    // Different profiles should have different number of triples
    expect(reactStore.size).not.toBe(nestStore.size);
  });
});

describe('Golden Structure - Performance', () => {
  it('should generate structure in <100ms', async () => {
    const start = performance.now();
    await generateGoldenStructure({ profile: 'react-feature-v1' });
    const duration = performance.now() - start;

    expect(duration).toBeLessThan(100);
  });

  it('should handle multiple profiles quickly', async () => {
    const profiles = [
      'react-feature-v1',
      'next-app-router-v1',
      'nest-api-v1',
    ];

    const start = performance.now();
    for (const profile of profiles) {
      await generateGoldenStructure({ profile });
    }
    const duration = performance.now() - start;

    expect(duration).toBeLessThan(300);
  });
});
