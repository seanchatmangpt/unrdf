/**
 * @file API Contract Validator tests - 80/20 consolidated
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { createStore } from '@unrdf/oxigraph';
import {
  generateAPISchema,
  _generateAllAPISchemas,
  validateAPIFiles,
  detectContractBreaks,
  detectAllContractBreaks,
} from '../../src/project-engine/api-contract-validator.mjs';

const { namedNode, literal } = DataFactory;
const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  dom: 'http://example.org/unrdf/domain#',
};

function createDomainStore(entityName, fields) {
  const store = createStore();
  const entityIri = `http://example.org/unrdf/domain#${entityName}`;
  store.addQuad(namedNode(entityIri), namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`));
  store.addQuad(namedNode(entityIri), namedNode(`${NS.rdfs}label`), literal(entityName));
  for (const field of fields) {
    const fieldIri = namedNode(`${entityIri}.${field.name}`);
    store.addQuad(namedNode(entityIri), namedNode(`${NS.dom}hasField`), fieldIri);
    store.addQuad(fieldIri, namedNode(`${NS.dom}fieldName`), literal(field.name));
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}fieldType`),
      namedNode(field.type || `${NS.xsd}string`)
    );
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}isOptional`),
      literal(field.optional ? 'true' : 'false', namedNode(`${NS.xsd}boolean`))
    );
    store.addQuad(
      fieldIri,
      namedNode(`${NS.dom}isArray`),
      literal(field.array ? 'true' : 'false', namedNode(`${NS.xsd}boolean`))
    );
  }
  return store;
}

describe('api-contract-validator', () => {
  describe('generateAPISchema', () => {
    it('generates schema with fields and Zod schema string', () => {
      const store = createDomainStore('User', [
        { name: 'id', type: `${NS.xsd}string` },
        { name: 'email', type: `${NS.xsd}string`, optional: true },
      ]);
      const schema = generateAPISchema(store, 'User');
      expect(schema.entityName).toBe('User');
      expect(schema.fields.map(f => f.name)).toContain('id');
      expect(schema.fields.find(f => f.name === 'email').optional).toBe(true);
      expect(schema.zodSchema).toContain('UserSchema');
      expect(schema.zodSchema).toContain('z.object');
    });
  });

  describe('validateAPIFiles', () => {
    it('detects missing fields and validation', () => {
      const store = createDomainStore('User', [
        { name: 'email', type: `${NS.xsd}string` },
        { name: 'password', type: `${NS.xsd}string` },
      ]);
      const schema = generateAPISchema(store, 'User');
      const apiContent = `export async function POST(req) { const { email } = await req.json(); return NextResponse.json({ email }) }`;

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: apiContent }],
        schema
      );

      expect(result.violations.filter(v => v.type === 'missing_field').length).toBeGreaterThan(0);
      expect(result.violations.some(v => v.type === 'missing_validation')).toBe(true);
      expect(result.breaking).toBe(true);
    });

    it('passes valid API with validation', () => {
      const store = createDomainStore('User', [{ name: 'email', type: `${NS.xsd}string` }]);
      const schema = generateAPISchema(store, 'User');
      const validContent = `import { UserSchema } from '@/schemas/user'; export async function POST(req) { const { email } = await req.json(); UserSchema.parse({ email }); return NextResponse.json({ email }) }`;

      const result = validateAPIFiles(
        [{ path: 'app/api/users/route.ts', content: validContent }],
        schema
      );
      expect(result.violations.filter(v => v.type === 'missing_field')).toHaveLength(0);
    });
  });

  describe('detectContractBreaks', () => {
    it('detects field removal and type changes as breaking', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [
          { name: 'id', type: 'string', optional: false, array: false },
          { name: 'legacy', type: 'string', optional: false, array: false },
        ],
      };
      const newSchema = {
        entityName: 'User',
        fields: [{ name: 'id', type: 'number', optional: false, array: false }],
      };

      const result = detectContractBreaks(oldSchema, newSchema);
      expect(result.safe).toBe(false);
      expect(result.breakingChanges.some(c => c.type === 'field_removed')).toBe(true);
      expect(result.breakingChanges.some(c => c.type === 'type_changed')).toBe(true);
    });

    it('allows non-breaking additions', () => {
      const oldSchema = {
        entityName: 'User',
        fields: [{ name: 'id', type: 'string', optional: false, array: false }],
      };
      const newSchema = {
        entityName: 'User',
        fields: [
          { name: 'id', type: 'string', optional: false, array: false },
          { name: 'new', type: 'string', optional: true, array: false },
        ],
      };

      expect(detectContractBreaks(oldSchema, newSchema).safe).toBe(true);
    });
  });

  describe('detectAllContractBreaks', () => {
    it('detects entity removal', () => {
      const oldStore = createStore();
      const newStore = createStore();
      oldStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      );
      oldStore.addQuad(
        namedNode(`${NS.dom}Product`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      );
      newStore.addQuad(
        namedNode(`${NS.dom}User`),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.dom}Entity`)
      );

      const results = detectAllContractBreaks(oldStore, newStore);
      expect(results.has('Product')).toBe(true);
      expect(results.get('Product').breakingChanges[0].type).toBe('entity_removed');
    });
  });
});
