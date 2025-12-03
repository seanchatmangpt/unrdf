/**
 * @unrdf/hooks - Basic Example
 *
 * Demonstrates basic usage of the Knowledge Hooks package.
 */

import { DataFactory } from 'n3';
import {
  defineHook,
  createHookRegistry,
  registerHook,
  executeHooksByTrigger,
  builtinHooks,
} from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

console.log('=== @unrdf/hooks Basic Example ===\n');

// 1. Define a custom validation hook
const validateEmailProperty = defineHook({
  name: 'validate-email-property',
  trigger: 'before-add',
  validate: (quad) => {
    if (quad.predicate.value === 'http://schema.org/email') {
      if (quad.object.termType !== 'Literal') {
        return false;
      }
      const email = quad.object.value;
      return email.includes('@') && email.includes('.');
    }
    return true;
  },
  metadata: {
    description: 'Validates email format for schema:email properties',
  },
});

console.log('1. Defined custom email validation hook');

// 2. Create registry and register hooks
const registry = createHookRegistry();
registerHook(registry, validateEmailProperty);
registerHook(registry, builtinHooks.validatePredicateIRI);

console.log('2. Registered hooks in registry\n');

// 3. Create test quads
const validQuad = quad(
  namedNode('http://example.org/person1'),
  namedNode('http://schema.org/email'),
  literal('user@example.com')
);

const invalidQuad = quad(
  namedNode('http://example.org/person2'),
  namedNode('http://schema.org/email'),
  literal('invalid-email')
);

// 4. Execute hooks on quads
console.log('3. Executing hooks on valid email quad:');
const result1 = executeHooksByTrigger(
  [validateEmailProperty, builtinHooks.validatePredicateIRI],
  'before-add',
  validQuad
);
console.log(`   Valid: ${result1.valid}`);
console.log(`   Hooks executed: ${result1.results.length}\n`);

console.log('4. Executing hooks on invalid email quad:');
const result2 = executeHooksByTrigger(
  [validateEmailProperty, builtinHooks.validatePredicateIRI],
  'before-add',
  invalidQuad
);
console.log(`   Valid: ${result2.valid}`);
console.log(`   Error: ${result2.error}\n`);

// 5. Demonstrate transformation hook
const normalizeEmail = defineHook({
  name: 'normalize-email',
  trigger: 'before-add',
  transform: (quad) => {
    if (quad.predicate.value === 'http://schema.org/email' &&
        quad.object.termType === 'Literal') {
      return DataFactory.quad(
        quad.subject,
        quad.predicate,
        literal(quad.object.value.toLowerCase()),
        quad.graph
      );
    }
    return quad;
  },
});

console.log('5. Applying transformation hook:');
const upperEmail = quad(
  namedNode('http://example.org/person3'),
  namedNode('http://schema.org/email'),
  literal('USER@EXAMPLE.COM')
);

const result3 = executeHooksByTrigger(
  [normalizeEmail],
  'before-add',
  upperEmail
);
console.log(`   Original: ${upperEmail.object.value}`);
console.log(`   Transformed: ${result3.quad.object.value}\n`);

console.log('=== Example Complete ===');
