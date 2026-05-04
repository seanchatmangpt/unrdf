/**
 * @unrdf/fusion - Capability Compositions
 * 
 * High-level compositions of UNRDF atomic capabilities
 * Each composition provides emergent functionality not available from atoms alone
 */

export { createFrozenReceipt, demo as demoFreezeReceipt } from './freeze-receipt.mjs';
export { createPolicyGatedStore, demo as demoPolicyGate } from './hook-policy-gate.mjs';

/**
 * Run all composition demos
 */
export async function runAllDemos() {
  const { demo: demoFreezeReceipt } = await import('./freeze-receipt.mjs');
  const { demo: demoPolicyGate } = await import('./hook-policy-gate.mjs');
  
  console.log('='.repeat(60));
  console.log('UNRDF Capability Compositions - Demo Suite');
  console.log('='.repeat(60));
  
  await demoFreezeReceipt();
  demoPolicyGate();
  
  console.log('='.repeat(60));
  console.log('All demos completed successfully');
  console.log('='.repeat(60));
}

// Run all demos if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllDemos().catch(console.error);
}
