/**
 * README Example: Knowledge Hooks (lines 143-168)
 * Tests hook definition and registration
 */

import { defineHook, registerHook } from '../../packages/knowledge-engine/index.mjs';

/**
 *
 */
async function testKnowledgeHooks() {
  console.log('🧪 Testing Knowledge Hooks Example...');

  try {
    const hook = defineHook({
      meta: {
        name: 'data-quality-gate',
        description: 'Ensures all persons have names',
      },
      when: {
        kind: 'sparql-ask',
        query: `
          ASK {
            ?person a <http://xmlns.com/foaf/0.1/Person> .
            FILTER NOT EXISTS { ?person <http://xmlns.com/foaf/0.1/name> ?name }
          }
        `,
      },
      run: async event => {
        if (event.result === true) {
          throw new Error('All persons must have names');
        }
      },
    });
    console.log('✅ Defined hook');

    // Validate hook structure
    if (!hook.meta || hook.meta.name !== 'data-quality-gate') {
      throw new Error('Hook meta validation failed');
    }

    if (!hook.when || hook.when.kind !== 'sparql-ask') {
      throw new Error('Hook when validation failed');
    }

    if (typeof hook.run !== 'function') {
      throw new Error('Hook run validation failed');
    }

    await registerHook(hook);
    console.log('✅ Registered hook');

    console.log('\n✅ Knowledge Hooks Example: PASSED\n');
    return true;
  } catch (error) {
    console.error('❌ Knowledge Hooks Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testKnowledgeHooks();
  process.exit(success ? 0 : 1);
}

export { testKnowledgeHooks };
