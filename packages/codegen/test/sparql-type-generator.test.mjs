/**
 * Tests for SPARQL Type Generator
 */

import { describe, it, expect } from 'vitest';
import { generateTypesFromSPARQL, createGenerationReceipt } from '../src/sparql-type-generator.mjs';

// Mock RDF store for testing
class MockRDFStore {
  constructor(data = []) {
    this.data = data;
  }

  async query(sparql) {
    // Simple mock: return predefined results based on query
    if (sparql.includes('SELECT DISTINCT ?class')) {
      return this.data.classes || [];
    }
    if (sparql.includes('SELECT ?prop')) {
      return this.data.properties || [];
    }
    return [];
  }
}

describe('SPARQL Type Generator', () => {
  it('should generate Zod schemas from RDF classes', async () => {
    const store = new MockRDFStore({
      classes: [
        new Map([
          ['class', { value: 'http://example.org/User' }],
          ['comment', { value: 'A user entity' }],
        ]),
      ],
      properties: [
        new Map([
          ['prop', { value: 'http://example.org/name' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#string' }],
          ['minCardinality', { value: '1' }],
        ]),
        new Map([
          ['prop', { value: 'http://example.org/age' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#integer' }],
        ]),
      ],
    });

    const result = await generateTypesFromSPARQL(store);

    expect(result.content).toContain('export const UserSchema');
    expect(result.content).toContain('z.object({');
    expect(result.content).toContain('name: z.string()');
    expect(result.content).toContain('age: z.number().int().optional()');
    expect(result.metadata.classCount).toBe(1);
    expect(result.metadata.propertyCount).toBe(2);
  });

  it('should handle multiple classes', async () => {
    const store = new MockRDFStore({
      classes: [
        new Map([
          ['class', { value: 'http://example.org/User' }],
        ]),
        new Map([
          ['class', { value: 'http://example.org/Post' }],
        ]),
      ],
      properties: [],
    });

    const result = await generateTypesFromSPARQL(store);

    expect(result.content).toContain('UserSchema');
    expect(result.content).toContain('PostSchema');
    expect(result.metadata.classCount).toBe(2);
  });

  it('should include TypeScript type inference', async () => {
    const store = new MockRDFStore({
      classes: [
        new Map([
          ['class', { value: 'http://example.org/Product' }],
        ]),
      ],
      properties: [
        new Map([
          ['prop', { value: 'http://example.org/price' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#decimal' }],
        ]),
      ],
    });

    const result = await generateTypesFromSPARQL(store);

    expect(result.content).toContain('export type Product = z.infer<typeof ProductSchema>');
  });

  it('should generate deterministic output', async () => {
    const store = new MockRDFStore({
      classes: [
        new Map([
          ['class', { value: 'http://example.org/Item' }],
        ]),
      ],
      properties: [],
    });

    const result1 = await generateTypesFromSPARQL(store);
    const result2 = await generateTypesFromSPARQL(store);

    expect(result1.metadata.hash).toBe(result2.metadata.hash);
  });

  it('should create receipt for generation', async () => {
    const store = new MockRDFStore({
      classes: [],
      properties: [],
    });
    store.size = 100;

    const result = await generateTypesFromSPARQL(store);
    const receipt = createGenerationReceipt(result, store);

    expect(receipt.operation).toBe('sparql-type-generation');
    expect(receipt.metadata.deterministic).toBe(true);
    expect(JSON.parse(receipt.result).hash).toBe(result.metadata.hash);
  });

  it('should handle classes with no properties', async () => {
    const store = new MockRDFStore({
      classes: [
        new Map([
          ['class', { value: 'http://example.org/EmptyClass' }],
        ]),
      ],
      properties: [],
    });

    const result = await generateTypesFromSPARQL(store);

    expect(result.content).toContain('EmptyClassSchema');
    expect(result.content).toContain('z.object({');
    expect(result.content).toContain('})');
  });

  it('should map XSD datatypes correctly', async () => {
    const store = new MockRDFStore({
      classes: [
        new Map([
          ['class', { value: 'http://example.org/DataTypes' }],
        ]),
      ],
      properties: [
        new Map([
          ['prop', { value: 'http://example.org/isActive' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#boolean' }],
        ]),
        new Map([
          ['prop', { value: 'http://example.org/createdAt' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#dateTime' }],
        ]),
        new Map([
          ['prop', { value: 'http://example.org/website' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#anyURI' }],
        ]),
      ],
    });

    const result = await generateTypesFromSPARQL(store);

    expect(result.content).toContain('isActive: z.boolean()');
    expect(result.content).toContain('createdAt: z.string().datetime()');
    expect(result.content).toContain('website: z.string().url()');
  });

  it('should handle optional vs required properties', async () => {
    const store = new MockRDFStore({
      classes: [
        new Map([
          ['class', { value: 'http://example.org/Person' }],
        ]),
      ],
      properties: [
        new Map([
          ['prop', { value: 'http://example.org/email' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#string' }],
          ['minCardinality', { value: '1' }],
        ]),
        new Map([
          ['prop', { value: 'http://example.org/phone' }],
          ['range', { value: 'http://www.w3.org/2001/XMLSchema#string' }],
          ['minCardinality', { value: '0' }],
        ]),
      ],
    });

    const result = await generateTypesFromSPARQL(store);

    // Required field
    expect(result.content).toMatch(/email:\s*z\.string\(\)(?!\.optional)/);
    // Optional field
    expect(result.content).toContain('phone: z.string().optional()');
  });
});
