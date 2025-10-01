/**
 * @fileoverview Tests for CLI ID generation commands
 * Testing UUID, hash, and generic ID generation
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  execCLI,
  createCLITestContext,
  assertSuccess
} from './test-helpers.mjs';

describe('CLI: ID generation commands', () => {
  let ctx;

  beforeEach(() => {
    ctx = createCLITestContext();
  });

  afterEach(async () => {
    await ctx.cleanup();
  });

  describe('UUID generation (Critical Path)', () => {
    it('should generate single UUID', async () => {
      const result = await execCLI(['id', 'uuid']);

      assertSuccess(result);

      const uuid = result.stdout.trim();
      // UUID format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
      expect(uuid).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i);
    });

    it('should generate multiple UUIDs', async () => {
      const result = await execCLI(['id', 'uuid', '--count', '5']);

      assertSuccess(result);

      const uuids = result.stdout.trim().split('\n');
      expect(uuids.length).toBe(5);

      // Each should be a valid UUID
      for (const uuid of uuids) {
        expect(uuid).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i);
      }

      // Should be unique
      const uniqueUUIDs = new Set(uuids);
      expect(uniqueUUIDs.size).toBe(5);
    });

    it('should generate large number of unique UUIDs', async () => {
      const result = await execCLI(['id', 'uuid', '--count', '100']);

      assertSuccess(result);

      const uuids = result.stdout.trim().split('\n');
      expect(uuids.length).toBe(100);

      const uniqueUUIDs = new Set(uuids);
      expect(uniqueUUIDs.size).toBe(100);
    });
  });

  describe('Hash-based IDs (Critical Path)', () => {
    it('should generate hash ID from input string', async () => {
      const result = await execCLI(['id', 'hash', 'test-input']);

      assertSuccess(result);

      const hashId = result.stdout.trim();
      expect(hashId).toBeTruthy();
      expect(hashId.length).toBeGreaterThan(0);
    });

    it('should generate consistent hashes for same input', async () => {
      const result1 = await execCLI(['id', 'hash', 'consistent-input']);
      const result2 = await execCLI(['id', 'hash', 'consistent-input']);

      assertSuccess(result1);
      assertSuccess(result2);

      expect(result1.stdout.trim()).toBe(result2.stdout.trim());
    });

    it('should generate different hashes for different inputs', async () => {
      const result1 = await execCLI(['id', 'hash', 'input-one']);
      const result2 = await execCLI(['id', 'hash', 'input-two']);

      assertSuccess(result1);
      assertSuccess(result2);

      expect(result1.stdout.trim()).not.toBe(result2.stdout.trim());
    });

    it('should handle special characters in input', async () => {
      const result = await execCLI(['id', 'hash', 'test@#$%^&*()']);

      assertSuccess(result);

      const hashId = result.stdout.trim();
      expect(hashId).toBeTruthy();
    });
  });

  describe('Generic ID generation', () => {
    it('should generate ID without prefix', async () => {
      const result = await execCLI(['id', 'generate']);

      assertSuccess(result);

      const id = result.stdout.trim();
      expect(id).toBeTruthy();
      expect(id.length).toBeGreaterThan(0);
    });

    it('should generate ID with prefix', async () => {
      const result = await execCLI(['id', 'generate', '--prefix', 'test']);

      assertSuccess(result);

      const id = result.stdout.trim();
      expect(id).toContain('test');
    });

    it('should generate unique IDs', async () => {
      const result1 = await execCLI(['id', 'generate']);
      const result2 = await execCLI(['id', 'generate']);

      assertSuccess(result1);
      assertSuccess(result2);

      expect(result1.stdout.trim()).not.toBe(result2.stdout.trim());
    });
  });

  describe('Performance', () => {
    it('should generate 1000 UUIDs quickly', async () => {
      const start = Date.now();
      const result = await execCLI(['id', 'uuid', '--count', '1000']);
      const duration = Date.now() - start;

      assertSuccess(result);

      const uuids = result.stdout.trim().split('\n');
      expect(uuids.length).toBe(1000);

      // Performance target: 1000 UUIDs in under 1 second
      expect(duration).toBeLessThan(1000);
    });

    it('should hash large input efficiently', async () => {
      const largeInput = 'x'.repeat(10000);

      const start = Date.now();
      const result = await execCLI(['id', 'hash', largeInput]);
      const duration = Date.now() - start;

      assertSuccess(result);

      // Performance target: hash 10KB input in under 100ms
      expect(duration).toBeLessThan(100);
    });
  });
});
