# Blockchain-Verified Audit Trail

Cryptographic audit trail using YAWL workflow receipts and blockchain verification.

## Features

- **Cryptographic receipts**: SHA-256 hashes for workflow executions
- **Immutable audit trail**: Blockchain-backed verification
- **Time-travel verification**: Verify historical states
- **Compliance reporting**: Generate SOX/GDPR compliance reports

## Quick Start

```bash
pnpm install
node src/audit-trail.mjs
```

## Usage

```javascript
import { AuditTrail } from './src/audit-trail.mjs';

const audit = new AuditTrail({
  blockchainUrl: process.env.BLOCKCHAIN_URL,
  contractAddress: process.env.CONTRACT_ADDRESS,
});

// Record workflow execution
const workflow = {
  id: 'payment-processing',
  tasks: [
    { id: 'validate', action: 'validate_payment' },
    { id: 'process', action: 'process_payment' },
    { id: 'confirm', action: 'send_confirmation' },
  ],
};

const record = await audit.recordExecution(workflow, { amount: 1000 });

console.log('Receipt hash:', record.receipt.hash);
console.log('Blockchain TX:', record.txHash);

// Verify execution
const verification = await audit.verify(record.workflowId);

console.log('Valid:', verification.valid);
console.log('Blockchain verified:', verification.blockchain);

// Generate compliance report
const report = await audit.generateComplianceReport({
  startDate: new Date('2025-01-01'),
  endDate: new Date('2025-12-31'),
});

console.log('Total workflows:', report.summary.totalWorkflows);
console.log('Verified receipts:', report.summary.verifiedReceipts);
```

## Docker

```bash
docker build -t blockchain-audit .
docker run -p 3000:3000 blockchain-audit
```

## Testing

```bash
pnpm test
pnpm test:coverage
```

## Architecture

1. **Workflow Execution**: Execute workflow and capture state
2. **Receipt Generation**: Create cryptographic receipt (SHA-256)
3. **Blockchain Recording**: Submit receipt hash to blockchain
4. **Verification**: Verify receipt integrity and blockchain record
5. **Compliance Reporting**: Generate audit reports

## Security

- SHA-256 hashing for receipts
- Blockchain immutability
- Tamper-proof audit trail
- Cryptographic verification
