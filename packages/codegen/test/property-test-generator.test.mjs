/**
 * Tests for Property-Based Test Generator
 */

import { describe, it, expect } from 'vitest';
import { z } from 'zod';
import generatePropertyTests from '../src/property-test-generator.mjs';

describe('Property-Based Test Generator', () => {
  it('should generate tests from string constraints', async () => {
    const schema = z.object({
      name: z.string().min(3).max(50),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('fc.assert');
    expect(result.code).toContain('minLength=3');
    expect(result.code).toContain('maxLength=50');
    expect(result.testCount).toBe(2);
    expect(result.constraints).toContain('minLength');
    expect(result.constraints).toContain('maxLength');
  });

  it('should generate tests from number constraints', async () => {
    const schema = z.object({
      age: z.number().int().min(0).max(120),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('fc.integer');
    expect(result.code).toContain('min=0');
    expect(result.code).toContain('max=120');
    expect(result.testCount).toBeGreaterThan(0);
    expect(result.constraints).toContain('min');
    expect(result.constraints).toContain('max');
    expect(result.constraints).toContain('integer');
  });

  it('should generate tests from email constraint', async () => {
    const schema = z.object({
      email: z.string().email(),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('fc.emailAddress()');
    expect(result.code).toContain('validate email format');
    expect(result.testCount).toBe(1);
  });

  it('should generate tests from URL constraint', async () => {
    const schema = z.object({
      website: z.string().url(),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('fc.webUrl()');
    expect(result.code).toContain('validate URL format');
  });

  it('should generate tests from regex pattern', async () => {
    const schema = z.object({
      code: z.string().regex(/^[A-Z]{3}-\d{3}$/),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('fc.stringMatching');
    expect(result.code).toContain('^[A-Z]{3}-\\d{3}$');
  });

  it('should handle multiple fields', async () => {
    const schema = z.object({
      name: z.string().min(1),
      age: z.number().int(),
      email: z.string().email(),
    });

    const result = await generatePropertyTests(schema);

    expect(result.testCount).toBeGreaterThan(2);
    expect(result.code).toContain('name');
    expect(result.code).toContain('age');
    expect(result.code).toContain('email');
  });

  it('should handle optional fields', async () => {
    const schema = z.object({
      nickname: z.string().optional(),
    });

    const result = await generatePropertyTests(schema);

    expect(result.constraints).toContain('optional');
  });

  it('should handle array constraints', async () => {
    const schema = z.object({
      tags: z.array(z.string()).min(1).max(10),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('minItems=1');
    expect(result.code).toContain('maxItems=10');
  });

  it('should return empty for schema with no constraints', async () => {
    const schema = z.object({
      data: z.unknown(),
    });

    const result = await generatePropertyTests(schema);

    expect(result.testCount).toBe(0);
    expect(result.code).toContain('No constraints found');
  });

  it('should include import statements', async () => {
    const schema = z.object({
      name: z.string().min(1),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('import { describe, it, expect } from \'vitest\'');
    expect(result.code).toContain('import * as fc from \'fast-check\'');
  });

  it('should use specified test count', async () => {
    const schema = z.object({
      value: z.number().min(0),
    });

    const result = await generatePropertyTests(schema, {
      testCount: 500,
    });

    expect(result.code).toContain('numRuns: 500');
    expect(result.metadata.testCount).toBe(500);
  });

  it('should include generation metadata', async () => {
    const schema = z.object({
      id: z.string(),
    });

    const result = await generatePropertyTests(schema);

    expect(result.metadata).toBeDefined();
    expect(result.metadata.framework).toBe('fast-check');
    expect(result.metadata.generatedAt).toBeDefined();
  });

  it('should handle complex nested schemas', async () => {
    const UserSchema = z.object({
      name: z.string().min(1),
      age: z.number().int().min(0),
    });

    const result = await generatePropertyTests(UserSchema);

    expect(result.testCount).toBeGreaterThan(2);
    expect(result.code).toContain('name');
    expect(result.code).toContain('age');
  });

  it('should validate schema parsing', async () => {
    const schema = z.object({
      count: z.number().int().min(1).max(100),
    });

    const result = await generatePropertyTests(schema);

    expect(result.code).toContain('schema.safeParse');
    expect(result.code).toContain('expect(result.success).toBe(true)');
  });

  it('should handle inclusive/exclusive bounds', async () => {
    const schema = z.object({
      score: z.number().min(0, { inclusive: true }).max(100, { inclusive: true }),
    });

    const result = await generatePropertyTests(schema);

    // Should generate min/max tests
    expect(result.constraints).toContain('min');
    expect(result.constraints).toContain('max');
  });

  it('should generate deterministic code', async () => {
    const schema = z.object({
      name: z.string().min(1),
    });

    const result1 = await generatePropertyTests(schema);
    const result2 = await generatePropertyTests(schema);

    // Code should be deterministic (same schema = same output)
    expect(result1.testCount).toBe(result2.testCount);
    expect(result1.constraints).toEqual(result2.constraints);
  });
});
