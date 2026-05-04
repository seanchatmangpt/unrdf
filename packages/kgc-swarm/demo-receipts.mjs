#!/usr/bin/env node
/**
 * Receipt Chain Demo - Tamper-Proof Audit Trail
 *
 * Demonstrates:
 * - Creating receipt chains
 * - Chain integrity verification
 * - Merkle root computation
 * - Tamper detection
 */

import {
  Receipt,
  ReceiptChain,
  hash,
  createGenesisReceipt,
  createTransitionReceipt,
  verifyChain,
} from './src/receipts.mjs';

console.log('═══════════════════════════════════════════════════════════════');
console.log('  Receipt Chain with Merkle Verification Demo');
console.log('═══════════════════════════════════════════════════════════════\n');

// ============================================================================
// Demo 1: Create Receipt Chain
// ============================================================================

console.log('📝 Demo 1: Creating Receipt Chain\n');

// Genesis receipt
const genesisState = hash('initial-database-state');
const r0 = createGenesisReceipt(genesisState, hash('system-init'), {
  id: 'receipt-0',
});

console.log(`r₀ (Genesis):
  ID:        ${r0.id}
  before:    ${r0.before.slice(0, 16)}...
  after:     ${r0.after.slice(0, 16)}...
  delta:     ${r0.delta.slice(0, 16)}...
  timestamp: ${new Date(r0.timestamp).toISOString()}
  hash:      ${r0.hash().slice(0, 16)}...\n`);

// Initialize chain
const chain = new ReceiptChain([r0]);

// Add state transitions
const transitions = [
  { name: 'add-user-alice', state: hash('state-after-alice') },
  { name: 'add-user-bob', state: hash('state-after-bob') },
  { name: 'update-alice-email', state: hash('state-after-update') },
];

let currentState = r0.after;
let receiptNum = 1;

for (const { name, state } of transitions) {
  const receipt = createTransitionReceipt(
    currentState,
    state,
    hash(name),
    { id: `receipt-${receiptNum}` }
  );
  chain.add(receipt);

  console.log(`r�${receiptNum} (${name}):
  before:    ${receipt.before.slice(0, 16)}...
  after:     ${receipt.after.slice(0, 16)}...
  delta:     ${receipt.delta.slice(0, 16)}...
  hash:      ${receipt.hash().slice(0, 16)}...`);

  // Verify link
  const prev = chain.get(receiptNum - 1);
  const linkValid = receipt.before === prev.after;
  console.log(`  ✓ Link r�${receiptNum-1} → r�${receiptNum}: ${linkValid ? 'VALID' : 'BROKEN'}\n`);

  currentState = state;
  receiptNum++;
}

// ============================================================================
// Demo 2: Chain Verification
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('🔍 Demo 2: Chain Verification\n');

const isValid = chain.verify();
console.log(`Chain integrity: ${isValid ? '✅ VALID' : '❌ INVALID'}`);
console.log(`Chain length: ${chain.length} receipts`);
console.log(`Chain: r₀ → r₁ → r₂ → r₃\n`);

// Verify each link
for (let i = 1; i < chain.length; i++) {
  const prev = chain.get(i - 1);
  const curr = chain.get(i);
  const linkValid = curr.before === prev.after;
  console.log(`  Link ${i-1}→${i}: ${linkValid ? '✅' : '❌'} (r₍${i-1}₎.after === r₍${i}₎.before)`);
}

// ============================================================================
// Demo 3: Merkle Root Computation
// ============================================================================

console.log('\n═══════════════════════════════════════════════════════════════');
console.log('🌳 Demo 3: Merkle Root Computation\n');

const merkleRoot = chain.getMerkleRoot();
console.log(`Merkle root: ${merkleRoot.slice(0, 32)}...${merkleRoot.slice(-8)}`);
console.log(`Root length: ${merkleRoot.length} chars (SHA-256 hex)`);

// Show tree structure
console.log('\nMerkle Tree Structure:');
console.log('       Root');
console.log('      /    \\');
console.log('    H01    H23');
console.log('   /  \\   /  \\');
console.log('  H0  H1 H2  H3');
console.log('  r₀  r₁ r₂  r₃\n');

// Serialize chain
const chainObj = chain.toObject();
console.log('Chain serialization:');
console.log(`  Receipts: ${chainObj.receipts.length}`);
console.log(`  Merkle root: ${chainObj.merkleRoot.slice(0, 16)}...`);

// ============================================================================
// Demo 4: Tamper Detection
// ============================================================================

console.log('\n═══════════════════════════════════════════════════════════════');
console.log('⚠️  Demo 4: Tamper Detection\n');

// Store original state
const originalRoot = chain.getMerkleRoot();
const originalR1Hash = chain.get(1).hash();

console.log('Original state:');
console.log(`  Merkle root: ${originalRoot.slice(0, 16)}...`);
console.log(`  r₁ hash:     ${originalR1Hash.slice(0, 16)}...`);
console.log(`  Chain valid: ${chain.verify() ? '✅' : '❌'}\n`);

// Tamper with receipt
console.log('🔨 Tampering with r₁.delta...\n');
const originalDelta = chain.receipts[1].delta;
chain.receipts[1].delta = hash('TAMPERED-OPERATION');

// Check integrity after tampering
const tamperedRoot = chain.getMerkleRoot();
const tamperedR1Hash = chain.get(1).hash();
const stillValid = chain.verify();

console.log('After tampering:');
console.log(`  Merkle root: ${tamperedRoot.slice(0, 16)}...`);
console.log(`  r₁ hash:     ${tamperedR1Hash.slice(0, 16)}...`);
console.log(`  Chain valid: ${stillValid ? '✅' : '❌'}\n`);

console.log('Detection results:');
console.log(`  ✅ Merkle root changed: ${originalRoot !== tamperedRoot}`);
console.log(`  ✅ Receipt hash changed: ${originalR1Hash !== tamperedR1Hash}`);
console.log(`  ✅ Tamper detected via Merkle verification\n`);

// Restore and verify
chain.receipts[1].delta = originalDelta;
const restoredRoot = chain.getMerkleRoot();
console.log('After restoration:');
console.log(`  Merkle root matches original: ${restoredRoot === originalRoot ? '✅' : '❌'}`);
console.log(`  Chain valid: ${chain.verify() ? '✅' : '❌'}\n`);

// ============================================================================
// Demo 5: Breaking Chain Integrity
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('💥 Demo 5: Breaking Chain Integrity\n');

// Tamper with r₂.before (breaks link r₁ → r₂)
console.log('🔨 Tampering with r₂.before (breaks link)...\n');
const originalBefore = chain.receipts[2].before;
chain.receipts[2].before = hash('FAKE-STATE');

const brokenChain = chain.verify();
console.log(`Chain integrity after breaking link: ${brokenChain ? '✅ VALID' : '❌ BROKEN'}`);

// Show broken link
const r1 = chain.get(1);
const r2 = chain.get(2);
console.log(`  r₁.after:  ${r1.after.slice(0, 16)}...`);
console.log(`  r₂.before: ${r2.before.slice(0, 16)}...`);
console.log(`  Match: ${r1.after === r2.before ? '✅' : '❌ BROKEN'}\n`);

// Verify using utility function
const verifyResult = verifyChain(chain);
console.log('Verification result:');
console.log(`  Valid: ${verifyResult.valid}`);
if (verifyResult.error) {
  console.log(`  Error: ${verifyResult.error}\n`);
}

// ============================================================================
// Demo 6: Anchoring New Receipts
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('⚓ Demo 6: Anchoring New Receipts\n');

// Restore chain
chain.receipts[2].before = originalBefore;
console.log(`Chain restored: ${chain.verify() ? '✅' : '❌'}\n`);

// Create new receipt
const newState = hash('state-after-delete');
const r4 = createTransitionReceipt(
  chain.get(chain.length - 1).after,
  newState,
  hash('delete-user-bob'),
  { id: 'receipt-4' }
);

// Anchor before adding
const anchoredRoot = chain.anchor(r4);
console.log('Anchoring new receipt r₄:');
console.log(`  Predicted root: ${anchoredRoot.slice(0, 16)}...`);

// Add and verify
chain.add(r4);
const actualRoot = chain.getMerkleRoot();
console.log(`  Actual root:    ${actualRoot.slice(0, 16)}...`);
console.log(`  Match: ${anchoredRoot === actualRoot ? '✅' : '❌'}\n`);

// ============================================================================
// Summary
// ============================================================================

console.log('═══════════════════════════════════════════════════════════════');
console.log('📊 Summary\n');

console.log(`✅ Final chain length: ${chain.length} receipts`);
console.log(`✅ Chain integrity: ${chain.verify() ? 'VALID' : 'INVALID'}`);
console.log(`✅ Merkle root: ${chain.getMerkleRoot().slice(0, 32)}...`);
console.log('\nCapabilities demonstrated:');
console.log('  ✅ Receipt generation with before/after/delta hashes');
console.log('  ✅ Chain integrity: r_i.before = r_{i-1}.after');
console.log('  ✅ Merkle tree construction for batch verification');
console.log('  ✅ Tamper detection via Merkle root changes');
console.log('  ✅ Chain verification and error detection');
console.log('  ✅ Receipt anchoring for predictive roots');
console.log('\n═══════════════════════════════════════════════════════════════\n');
