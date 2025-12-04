/**
 * @fileoverview Example usage of KnowledgeHookManager
 */

import { KnowledgeHookManager, defineHook } from '@unrdf/hooks';
import { createStore, dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal, quad  } = dataFactory;

// Example 1: Create manager with built-in hooks
const manager = new KnowledgeHookManager({ includeBuiltins: true });

console.log('✅ Manager created with built-in hooks');
console.log('Hooks loaded:', manager.listHooks().length);

// Example 2: Register custom hook
const customHook = defineHook({
  id: 'custom-validation',
  name: 'custom-validation',
  description: 'Custom validation hook',
  version: '1.0.0',
  trigger: 'before-add',
  enabled: true,
  validate: (quad) => {
    // Custom validation logic
    if (quad.object.value.length > 100) {
      return {
        valid: false,
        errors: ['Object value too long'],
      };
    }
    return { valid: true };
  },
});

manager.registerHook(customHook);
console.log('✅ Custom hook registered');

// Example 3: Execute hooks on data
const testQuad = quad(
  namedNode('http://example.org/subject'),
  namedNode('http://example.org/predicate'),
  literal('test value')
);

const result = await manager.executeByTrigger('before-add', testQuad);
console.log('✅ Hooks executed:', result.valid ? 'PASSED' : 'FAILED');

// Example 4: Check if data would pass (dry-run)
const wouldPass = await manager.wouldPass('before-add', testQuad);
console.log('✅ Validation check:', wouldPass ? 'WOULD PASS' : 'WOULD FAIL');

// Example 5: Get hooks by trigger
const beforeAddHooks = manager.getHooksByTrigger('before-add');
console.log('✅ Hooks for before-add trigger:', beforeAddHooks.length);

// Example 6: Get statistics
const stats = manager.getStats();
console.log('✅ Statistics:', {
  total: stats.totalHooks,
  triggers: Object.keys(stats.byTrigger),
});

// Example 7: Access built-in hooks statically
const builtins = KnowledgeHookManager.getBuiltinHooks();
console.log('✅ Built-in hooks available:', builtins.length);
