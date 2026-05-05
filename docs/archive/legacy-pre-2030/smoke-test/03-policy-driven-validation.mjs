/**
 * README Example 2: Policy-Driven Validation (lines 379-423)
 * Tests Knowledge Hooks with validation
 */

import { createDarkMatterCore, defineHook, registerHook } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';

console.log('🧪 Testing Policy-Driven Validation Example...\n');

try {
  const system = await createDarkMatterCore();

  // Define validation hook
  const validateAge = defineHook({
    meta: { name: 'age-validation', description: 'Ensure age is >= 18' },
    when: {
      kind: 'sparql-ask',
      query: `
        ASK {
          ?person <http://example.org/age> ?age .
          FILTER (?age < 18)
        }
      `
    },
    run: async (event) => {
      if (event.result) {
        throw new Error('All persons must be 18 or older');
      }
    }
  });

  await registerHook(validateAge);

  // This should fail validation
  let validationFailed = false;
  let errorMessage = '';

  try {
    await system.executeTransaction({
      additions: [
        quad(
          namedNode('http://example.org/charlie'),
          namedNode('http://example.org/age'),
          literal('16', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      ],
      removals: [],
      actor: 'system'
    });
  } catch (err) {
    validationFailed = true;
    errorMessage = err.message;
    console.log('Validation failed as expected:', err.message);
  }

  if (validationFailed && errorMessage.includes('18 or older')) {
    console.log('✅ Policy-Driven Validation example PASSED');
    console.log('   - Hook defined and registered successfully');
    console.log('   - Validation hook triggered correctly');
    console.log('   - Transaction rejected for age < 18');
  } else {
    console.log('❌ Policy-Driven Validation example FAILED');
    console.log('   - Expected: Validation error for age < 18');
    console.log('   - Got: Transaction succeeded (should have failed)');
  }

  await system.cleanup();

  process.exit(validationFailed ? 0 : 1);
} catch (error) {
  console.log('❌ Policy-Driven Validation example FAILED with error:');
  console.error(error);
  process.exit(1);
}
