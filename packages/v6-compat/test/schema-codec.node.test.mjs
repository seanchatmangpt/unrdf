import test from 'node:test';
import assert from 'node:assert/strict';
import {
  parseTypeScriptType,
  typeScriptTypeToZod,
  zodSchemaToTypeScript,
  generateTypeScriptDeclaration,
} from '../src/schema-codec.mjs';

const schema = (type, extra = {}) => ({ _def: { type, ...extra } });

test('TypeScript primitives, arrays and literals compile to Zod', () => {
  assert.equal(typeScriptTypeToZod('string'), 'z.string()');
  assert.equal(typeScriptTypeToZod('number[]'), 'z.array(z.number())');
  assert.equal(typeScriptTypeToZod("'open' | 'closed'"), 'z.union([z.literal("open"), z.literal("closed")])');
});

test('object optionality and index signatures compile', () => {
  const code = typeScriptTypeToZod('{ id: string; count?: number; [key: string]: unknown }');
  assert.match(code, /id: z\.string\(\)/);
  assert.match(code, /count: z\.number\(\)\.optional\(\)/);
  assert.match(code, /catchall\(z\.unknown\(\)\)/);
});

test('generic containers compile', () => {
  assert.equal(typeScriptTypeToZod('Promise<Array<string>>'), 'z.promise(z.array(z.string()))');
  assert.equal(typeScriptTypeToZod('Record<string, number>'), 'z.record(z.string(), z.number())');
  assert.equal(typeScriptTypeToZod('Map<string, Set<number>>'), 'z.map(z.string(), z.set(z.number()))');
});

test('tuples, unions and intersections preserve structure', () => {
  assert.equal(typeScriptTypeToZod('[string, number | null]'), 'z.tuple([z.string(), z.union([z.number(), z.null()])])');
  assert.equal(typeScriptTypeToZod('{ id: string } & { name: string }'), 'z.intersection(z.object({ id: z.string() }), z.object({ name: z.string() }))');
});

test('named references become lazy schema references', () => {
  assert.equal(typeScriptTypeToZod('User'), 'z.lazy(() => UserSchema)');
  assert.equal(typeScriptTypeToZod('User', { reference: name => `schemas.${name}` }), 'schemas.User');
});

test('Zod primitives and wrappers compile to TypeScript', () => {
  assert.equal(zodSchemaToTypeScript(schema('string')), 'string');
  assert.equal(zodSchemaToTypeScript(schema('optional', { innerType: schema('number') })), 'number | undefined');
  assert.equal(zodSchemaToTypeScript(schema('nullable', { innerType: schema('string') })), 'string | null');
  assert.equal(zodSchemaToTypeScript(schema('promise', { innerType: schema('boolean') })), 'Promise<boolean>');
});

test('Zod arrays, tuples, unions and intersections compile', () => {
  assert.equal(zodSchemaToTypeScript(schema('array', { element: schema('string') })), 'string[]');
  assert.equal(zodSchemaToTypeScript(schema('tuple', { items: [schema('string'), schema('number')] })), '[string, number]');
  assert.equal(zodSchemaToTypeScript(schema('union', { options: [schema('string'), schema('null')] })), 'string | null');
  assert.equal(zodSchemaToTypeScript(schema('intersection', { left: schema('string'), right: schema('literal', { value: 'x' }) })), 'string & "x"');
});

test('Zod object fields retain optionality', () => {
  const user = schema('object', { shape: {
    id: schema('string'),
    age: schema('optional', { innerType: schema('number') }),
  } });
  assert.equal(zodSchemaToTypeScript(user), '{ id: string; age?: number; }');
  assert.equal(generateTypeScriptDeclaration(user, { name: 'User' }), 'export interface User {id: string; age?: number;}');
});

test('Zod records, maps, sets, enums and literals compile', () => {
  assert.equal(zodSchemaToTypeScript(schema('record', { keyType: schema('string'), valueType: schema('number') })), 'Record<string, number>');
  assert.equal(zodSchemaToTypeScript(schema('map', { keyType: schema('string'), valueType: schema('boolean') })), 'Map<string, boolean>');
  assert.equal(zodSchemaToTypeScript(schema('set', { valueType: schema('string') })), 'Set<string>');
  assert.equal(zodSchemaToTypeScript(schema('enum', { values: ['a', 'b'] })), '"a" | "b"');
  assert.equal(zodSchemaToTypeScript(schema('literal', { value: 42 })), '42');
});

test('parser rejects trailing or malformed input', () => {
  assert.throws(() => parseTypeScriptType('{ id string }'), /Expected/);
  assert.throws(() => parseTypeScriptType('string nonsense'), /Unexpected token/);
});
