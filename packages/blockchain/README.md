# @unrdf/blockchain

Blockchain integration for UNRDF - Cryptographic receipt anchoring and immutable audit trails using Ethereum smart contracts.

## Features

### 1. On-Chain Receipt Anchoring
- Store YAWL receipt hashes on Ethereum blockchain
- Immutable, tamper-proof audit trail
- Public verifiability of workflow transitions

### 2. Smart Contract Integration
- `WorkflowVerifier.sol` - Solidity contract for receipt verification
- Individual, batch, and Merkle root anchoring
- Event emission for off-chain indexing

### 3. Merkle Proof System
- Efficient batch anchoring using Merkle trees
- Up to 99% gas savings for large batches
- Proof generation and verification

## Architecture

```
packages/blockchain/
├── src/
│   ├── anchoring/
│   │   └── receipt-anchorer.mjs       # Ethereum anchoring client
│   ├── contracts/
│   │   ├── WorkflowVerifier.sol       # Smart contract
│   │   └── workflow-verifier.mjs      # Contract wrapper
│   ├── merkle/
│   │   └── merkle-proof-generator.mjs # Merkle tree implementation
│   └── index.mjs                      # Main exports
├── examples/
│   └── blockchain-demo.mjs            # Comprehensive demo
└── test/
    └── integration.test.mjs           # Integration tests
```

## Installation

```bash
pnpm add @unrdf/blockchain
```

### Dependencies
- `ethers` - Ethereum interaction
- `@noble/hashes` - Cryptographic hashing
- `merkletreejs` - Merkle tree implementation
- `@unrdf/yawl` - Receipt generation (peer dependency)

## Quick Start

### 1. Deploy Contract

```javascript
import { WorkflowVerifier } from '@unrdf/blockchain';

const verifier = new WorkflowVerifier({
  provider: 'http://localhost:8545',
  privateKey: process.env.PRIVATE_KEY
});

const address = await verifier.deploy();
console.log(`Contract deployed: ${address}`);
```

### 2. Anchor Single Receipt

```javascript
import { ReceiptAnchorer } from '@unrdf/blockchain';
import { generateReceipt } from '@unrdf/yawl/receipt';

const anchorer = new ReceiptAnchorer({
  provider: 'http://localhost:8545',
  privateKey: process.env.PRIVATE_KEY,
  contractAddress: '0x...'
});

const receipt = generateReceipt({...});
const result = await anchorer.anchorReceipt(receipt);

console.log(`Anchored at block ${result.blockNumber}`);
console.log(`Gas cost: ${result.costETH} ETH`);
```

### 3. Batch Anchoring with Merkle Tree

```javascript
import { MerkleProofGenerator, ReceiptAnchorer } from '@unrdf/blockchain';

// Build Merkle tree
const generator = new MerkleProofGenerator();
receipts.forEach(r => generator.addReceipt(r));
const root = generator.buildTree();

// Anchor root (single transaction for ALL receipts)
const anchorer = new ReceiptAnchorer({...});
const result = await anchorer.anchorMerkleRoot(root, receipts.length);

// Generate proof for any receipt
const proof = generator.generateProof(receipts[0]);
const isValid = generator.verifyProof(proof);
```

### 4. Verify Receipt

```javascript
const verification = await anchorer.verifyReceipt(receipt);

if (verification.isAnchored) {
  console.log(`Anchored at block ${verification.blockNumber}`);
} else {
  console.log('Receipt not found on-chain');
}
```

## Gas Cost Analysis

| Operation | Receipts | Gas Cost | Cost/Receipt |
|-----------|----------|----------|--------------|
| Individual | 1 | ~50,000 | 50,000 |
| Batch | 10 | ~230,000 | 23,000 |
| Merkle | 100 | ~60,000 | 600 |
| Merkle | 1,000 | ~60,000 | 60 |

**Savings**: Merkle anchoring provides **99%+ gas savings** for batches > 100 receipts.

### Recommendations
- **< 3 receipts**: Use individual anchoring
- **3-50 receipts**: Use batch anchoring
- **> 50 receipts**: Use Merkle anchoring

## Smart Contract

### WorkflowVerifier.sol

Deployed Solidity contract that provides:

```solidity
// Anchor single receipt
function anchorReceipt(bytes32 receiptHash) external returns (bool);

// Anchor batch
function anchorBatch(bytes32[] memory receiptHashes) external returns (bool);

// Anchor Merkle root
function anchorMerkleRoot(bytes32 merkleRoot, uint256 receiptCount) external returns (bool);

// Verify receipt
function verifyReceipt(bytes32 receiptHash) external view
  returns (bool exists, uint256 blockNumber, bytes32 txHash);

// Events
event ReceiptAnchored(bytes32 indexed receiptHash, uint256 blockNumber, uint256 timestamp);
event MerkleRootAnchored(bytes32 indexed merkleRoot, uint256 receiptCount, uint256 blockNumber);
```

### Deployment

Using Hardhat:
```bash
npx hardhat compile
npx hardhat run scripts/deploy.js --network sepolia
```

Using Foundry:
```bash
forge build
forge create --rpc-url $RPC_URL --private-key $PRIVATE_KEY src/contracts/WorkflowVerifier.sol:WorkflowVerifier
```

## Advanced Usage

### Event Monitoring

```javascript
const verifier = new WorkflowVerifier({...});
verifier.connect(contractAddress);

verifier.onReceiptAnchored(({ receiptHash, blockNumber, timestamp }) => {
  console.log(`Receipt ${receiptHash} anchored at block ${blockNumber}`);
});

verifier.onMerkleRootAnchored(({ merkleRoot, receiptCount }) => {
  console.log(`Merkle root anchored with ${receiptCount} receipts`);
});
```

### Gas Estimation

```javascript
const estimate = await anchorer.estimateGas('merkle', 100);
console.log(`Estimated gas: ${estimate.estimatedGas}`);
console.log(`Estimated cost: ${estimate.costETH} ETH`);
```

### Export/Import Merkle Trees

```javascript
// Export for storage
const generator = new MerkleProofGenerator();
// ... build tree ...
const exportedData = generator.export();
await fs.writeFile('merkle-tree.json', JSON.stringify(exportedData));

// Import later
const imported = JSON.parse(await fs.readFile('merkle-tree.json'));
const newGenerator = new MerkleProofGenerator();
newGenerator.import(imported);

// Generate proofs from imported tree
const proof = newGenerator.generateProof(receipt);
```

## Demo

Run the comprehensive demo:

```bash
# Start local Ethereum node (Hardhat)
npx hardhat node

# In another terminal, run demo
cd packages/blockchain
pnpm demo
```

The demo demonstrates:
1. Contract deployment
2. Single receipt anchoring
3. Batch anchoring (5 receipts)
4. Merkle tree anchoring (100 receipts)
5. On-chain verification
6. Gas cost analysis
7. Contract statistics

## Testing

```bash
pnpm test
```

Integration tests verify:
- Contract deployment
- Receipt anchoring (individual, batch, Merkle)
- On-chain verification
- Merkle proof generation/verification
- Gas cost benchmarks

## Security Considerations

1. **Private Key Management**: Never commit private keys. Use environment variables or key management services.

2. **Network Selection**: Test on testnets (Sepolia, Goerli) before mainnet deployment.

3. **Gas Price**: Monitor gas prices and set appropriate limits to avoid excessive costs.

4. **Hash Integrity**: Verify receipt hashes are computed correctly before anchoring.

5. **Contract Ownership**: The contract owner has special privileges. Use multi-sig for production.

## Integration with YAWL

```javascript
import { ProofChain, generateReceipt } from '@unrdf/yawl/receipt';
import { ReceiptAnchorer } from '@unrdf/blockchain';

// Generate YAWL receipt
const receipt = generateReceipt({
  event: 'TASK_COMPLETED',
  caseId: 'case-123',
  taskId: 'task-456',
  payload: { decision: 'APPROVE' }
});

// Anchor to blockchain
const anchorer = new ReceiptAnchorer({...});
const result = await anchorer.anchorReceipt(receipt);

// Verify later
const verification = await anchorer.verifyReceipt(receipt);
console.log(`Receipt anchored: ${verification.isAnchored}`);
```

## API Reference

### ReceiptAnchorer

#### `constructor(config)`
- `config.provider` - Ethereum provider URL
- `config.privateKey` - Private key for signing
- `config.contractAddress` - WorkflowVerifier contract address

#### `anchorReceipt(receipt): Promise<AnchorResult>`
Anchor single receipt hash to blockchain.

#### `anchorBatch(receipts): Promise<AnchorResult>`
Anchor multiple receipts in single transaction.

#### `anchorMerkleRoot(root, count): Promise<AnchorResult>`
Anchor Merkle tree root.

#### `verifyReceipt(receipt): Promise<VerificationResult>`
Verify if receipt is anchored on-chain.

### MerkleProofGenerator

#### `addReceipt(receipt): void`
Add receipt to tree.

#### `buildTree(): string`
Build Merkle tree, returns root hash.

#### `generateProof(receipt): MerkleProof`
Generate proof for specific receipt.

#### `verifyProof(proof): boolean`
Verify Merkle proof validity.

#### `getTreeInfo(): TreeInfo`
Get tree statistics (depth, leaf count, etc.).

### WorkflowVerifier

#### `deploy(): Promise<string>`
Deploy new contract, returns address.

#### `connect(address): void`
Connect to existing contract.

#### `getStats(): Promise<ContractStats>`
Get contract statistics.

#### Event Listeners
- `onReceiptAnchored(callback)`
- `onBatchAnchored(callback)`
- `onMerkleRootAnchored(callback)`

## License

MIT

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md)

## Related Packages

- `@unrdf/yawl` - Workflow engine and receipt generation
- `@unrdf/kgc-4d` - Time-travel capabilities
- `@unrdf/core` - Core RDF functionality
