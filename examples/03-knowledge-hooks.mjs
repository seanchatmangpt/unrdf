/**
 * Example 03: Knowledge Hooks
 *
 * This example demonstrates:
 * - Defining reactive hooks
 * - INSERT triggers
 * - DELETE triggers
 * - Validation hooks
 * - Transformation hooks
 *
 * Run: node examples/03-knowledge-hooks.mjs
 */

import {
  createStore,
  namedNode,
  literal,
  FOAF,
  RDF
} from '@unrdf/core';

import {
  defineHook,
  executeHook,
  executeHooksByTrigger
} from '@unrdf/hooks';

console.log('=== Example 03: Knowledge Hooks ===\n');

// Create store
const store = createStore();
const ex = (name) => namedNode(`http://example.org/${name}`);

// ============================================================================
// 1. Simple INSERT Hook
// ============================================================================
console.log('--- 1. Simple INSERT Hook ---');

const notifyHook = defineHook({
  meta: {
    name: 'notify-new-person',
    description: 'Notify when a new person is added'
  },
  trigger: 'after-add',
  pattern: '?person a foaf:Person .',

  run(event) {
    const person = event.quad.subject;
    const nameQuads = store.getQuads(person, FOAF.name, null);
    const name = nameQuads.length > 0 ? nameQuads[0].object.value : person.value;

    console.log(`  🔔 New person added: ${name}`);
    return { allowed: true };
  }
});

// Add a person (trigger the hook)
const aliceQuad = {
  subject: ex('alice'),
  predicate: RDF.type,
  object: FOAF.Person
};

store.addQuad(aliceQuad.subject, aliceQuad.predicate, aliceQuad.object);
store.addQuad(ex('alice'), FOAF.name, literal('Alice Smith'));

// Execute the hook
const result1 = executeHook(notifyHook, {
  quad: aliceQuad,
  store,
  trigger: 'after-add'
});

console.log(`  Hook result: ${result1.allowed ? 'Allowed' : 'Denied'}\n`);

// ============================================================================
// 2. Validation Hook
// ============================================================================
console.log('--- 2. Validation Hook ---');

const validateEmailHook = defineHook({
  meta: {
    name: 'validate-email',
    description: 'Ensure all emails are valid'
  },
  trigger: 'after-add',
  pattern: '?person foaf:mbox ?email .',

  run(event) {
    const email = event.quad.object.value;
    const isValid = email.includes('@') && email.includes('.');

    if (!isValid) {
      console.log(`  ❌ Invalid email: ${email}`);
      return {
        allowed: false,
        message: `Invalid email format: ${email}`
      };
    }

    console.log(`  ✅ Valid email: ${email}`);
    return { allowed: true };
  }
});

// Test with valid email
const validEmailQuad = {
  subject: ex('alice'),
  predicate: FOAF.mbox,
  object: literal('alice@example.com')
};

const validResult = executeHook(validateEmailHook, {
  quad: validEmailQuad,
  store,
  trigger: 'after-add'
});

console.log(`  Result: ${validResult.allowed ? 'Accepted' : 'Rejected'}`);

// Test with invalid email
const invalidEmailQuad = {
  subject: ex('bob'),
  predicate: FOAF.mbox,
  object: literal('invalid-email')
};

const invalidResult = executeHook(validateEmailHook, {
  quad: invalidEmailQuad,
  store,
  trigger: 'after-add'
});

console.log(`  Result: ${invalidResult.allowed ? 'Accepted' : 'Rejected'}`);
console.log(`  Message: ${invalidResult.message}\n`);

// ============================================================================
// 3. Transformation Hook
// ============================================================================
console.log('--- 3. Transformation Hook ---');

const normalizeNameHook = defineHook({
  meta: {
    name: 'normalize-name',
    description: 'Normalize names to title case'
  },
  trigger: 'after-add',
  pattern: '?person foaf:name ?name .',

  run(event) {
    const originalName = event.quad.object.value;

    // Transform to title case
    const normalized = originalName
      .toLowerCase()
      .split(' ')
      .map(word => word.charAt(0).toUpperCase() + word.slice(1))
      .join(' ');

    if (originalName !== normalized) {
      console.log(`  🔧 Normalized: "${originalName}" → "${normalized}"`);

      return {
        allowed: true,
        transformed: {
          subject: event.quad.subject,
          predicate: event.quad.predicate,
          object: literal(normalized)
        }
      };
    }

    console.log(`  ✓ Name already normalized: "${originalName}"`);
    return { allowed: true };
  }
});

// Test transformation
const unnormalizedQuad = {
  subject: ex('bob'),
  predicate: FOAF.name,
  object: literal('bob JOHNSON')
};

const transformResult = executeHook(normalizeNameHook, {
  quad: unnormalizedQuad,
  store,
  trigger: 'after-add'
});

if (transformResult.transformed) {
  console.log(`  Final value: ${transformResult.transformed.object.value}\n`);
}

// ============================================================================
// 4. DELETE Hook
// ============================================================================
console.log('--- 4. DELETE Hook ---');

const auditDeleteHook = defineHook({
  meta: {
    name: 'audit-delete',
    description: 'Log all deletions'
  },
  trigger: 'before-remove',
  pattern: '?s ?p ?o .',

  run(event) {
    const { subject, predicate, object } = event.quad;
    console.log(`  📝 Audit: Deleting triple`);
    console.log(`      Subject: ${subject.value}`);
    console.log(`      Predicate: ${predicate.value}`);
    console.log(`      Object: ${object.value}`);

    return { allowed: true };
  }
});

// Simulate deletion
const deleteQuad = {
  subject: ex('alice'),
  predicate: FOAF.name,
  object: literal('Alice Smith')
};

executeHook(auditDeleteHook, {
  quad: deleteQuad,
  store,
  trigger: 'before-remove'
});

console.log();

// ============================================================================
// 5. Chained Hooks
// ============================================================================
console.log('--- 5. Multiple Hooks on Same Event ---');

const hooks = [
  defineHook({
    meta: { name: 'hook-1' },
    trigger: 'after-add',
    pattern: '?person a foaf:Person .',
    run() {
      console.log('  Hook 1: Validation passed');
      return { allowed: true };
    }
  }),
  defineHook({
    meta: { name: 'hook-2' },
    trigger: 'after-add',
    pattern: '?person a foaf:Person .',
    run() {
      console.log('  Hook 2: Logging event');
      return { allowed: true };
    }
  }),
  defineHook({
    meta: { name: 'hook-3' },
    trigger: 'after-add',
    pattern: '?person a foaf:Person .',
    run() {
      console.log('  Hook 3: Sending notification');
      return { allowed: true };
    }
  })
];

const personQuad = {
  subject: ex('carol'),
  predicate: RDF.type,
  object: FOAF.Person
};

console.log('  Executing 3 hooks in sequence...');
for (const hook of hooks) {
  executeHook(hook, {
    quad: personQuad,
    store,
    trigger: 'after-add'
  });
}

console.log();

// ============================================================================
// Summary
// ============================================================================
console.log('--- Summary ---');
console.log('  Knowledge Hooks enable reactive behaviors:');
console.log('  ✓ Validation: Ensure data quality');
console.log('  ✓ Transformation: Normalize data');
console.log('  ✓ Notifications: React to changes');
console.log('  ✓ Auditing: Track all operations');
console.log('  ✓ Policy enforcement: Apply business rules');

console.log('\n✅ Example complete!');
console.log('\nNext: Try examples/04-validation.mjs for SHACL validation');
