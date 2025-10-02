/**
 * README Example 3: Cryptographic Audit Trail (lines 426-460)
 * Tests LockchainWriter with Merkle verification
 */

import { createDarkMatterCore, LockchainWriter } from 'unrdf';
import { namedNode, quad, literal } from '@rdfjs/data-model';
import { mkdtempSync } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';

console.log('🧪 Testing Cryptographic Audit Trail Example...\n');

try {
  const system = await createDarkMatterCore();

  // Create temporary directory for lockchain
  const tempDir = mkdtempSync(join(tmpdir(), 'unrdf-audit-'));

  const lockchain = new LockchainWriter({
    repoPath: tempDir,
    enableMerkle: true
  });

  await lockchain.init();

  // Execute transaction with audit
  const additions = [
    quad(
      namedNode('http://example.org/user1'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    )
  ];

  const result = await system.executeTransaction({
    additions,
    removals: [],
    actor: 'alice@example.org'
  });

  // Write cryptographically signed receipt
  const receipt = await lockchain.writeReceipt({
    actor: 'alice@example.org',
    action: 'add-user',
    delta: result.delta,
    timestamp: new Date(),
    metadata: { ip: '192.168.1.1', reason: 'User registration' }
  });

  console.log('Receipt created:', {
    actor: receipt.actor,
    action: receipt.action,
    merkleRoot: receipt.merkleRoot,
    hasMetadata: !!receipt.metadata
  });

  // Verify integrity
  const isValid = await lockchain.verifyReceipt(receipt);
  console.log('Audit trail valid:', isValid);

  if (isValid && receipt.merkleRoot && receipt.actor === 'alice@example.org') {
    console.log('✅ Cryptographic Audit Trail example PASSED');
    console.log('   - Lockchain initialized successfully');
    console.log('   - Receipt written with Merkle root');
    console.log('   - Receipt verification passed');
    console.log('   - Merkle root:', receipt.merkleRoot.substring(0, 16) + '...');
  } else {
    console.log('❌ Cryptographic Audit Trail example FAILED');
    console.log('   - Receipt validation:', isValid);
    console.log('   - Merkle root:', receipt.merkleRoot);
  }

  await system.cleanup();

  process.exit(isValid ? 0 : 1);
} catch (error) {
  console.log('❌ Cryptographic Audit Trail example FAILED with error:');
  console.error(error);
  process.exit(1);
}
