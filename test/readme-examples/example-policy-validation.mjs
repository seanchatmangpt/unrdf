/**
 * README Example: Policy-Driven Validation (lines 383-426)
 * Tests validation hooks with age requirement
 */

import { createDarkMatterCore, defineHook, registerHook, DataFactory } from '../../src/knowledge-engine/index.mjs';
const { namedNode, quad, literal } = DataFactory;

async function testPolicyValidation() {
  console.log('🧪 Testing Policy-Driven Validation Example...');

  const system = await createDarkMatterCore();

  try {
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
    console.log('✅ Defined validation hook');

    await registerHook(validateAge);
    console.log('✅ Registered hook');

    // This will fail validation
    let validationFailed = false;
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
      console.log('✅ Validation failed as expected:', err.message);
      validationFailed = true;
    }

    if (!validationFailed) {
      throw new Error('Expected validation to fail for age < 18');
    }

    await system.cleanup();
    console.log('\n✅ Policy-Driven Validation Example: PASSED\n');
    return true;
  } catch (error) {
    await system.cleanup();
    console.error('❌ Policy-Driven Validation Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testPolicyValidation();
  process.exit(success ? 0 : 1);
}

export { testPolicyValidation };
