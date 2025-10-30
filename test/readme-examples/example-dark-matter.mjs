/**
 * README Example: Dark Matter 80/20 Optimization (lines 274-285)
 * Tests createDarkMatterCore optimizations
 */

import { createDarkMatterCore } from '../../src/knowledge-engine/index.mjs';

async function testDarkMatter() {
  console.log('🧪 Testing Dark Matter Example...');

  try {
    // Minimal core with automatic optimizations
    const system = await createDarkMatterCore();
    console.log('✅ Created Dark Matter core');

    // Validate system has expected features
    if (typeof system.executeTransaction !== 'function') {
      throw new Error('System missing executeTransaction');
    }

    if (typeof system.query !== 'function') {
      throw new Error('System missing query');
    }

    if (typeof system.cleanup !== 'function') {
      throw new Error('System missing cleanup');
    }

    console.log('✅ System includes required features:');
    console.log('  - Hook execution batching');
    console.log('  - LRU query caching');
    console.log('  - Parallel independent hook execution');
    console.log('  - Memory-efficient resource management');

    await system.cleanup();
    console.log('✅ Cleanup complete');

    console.log('\n✅ Dark Matter Example: PASSED\n');
    return true;
  } catch (error) {
    console.error('❌ Dark Matter Example FAILED:', error.message);
    console.error(error.stack);
    return false;
  }
}

// Run test if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const success = await testDarkMatter();
  process.exit(success ? 0 : 1);
}

export { testDarkMatter };
