# How-To: Implement Audit Trails

**Problem**: You need to create a tamper-proof, cryptographically verifiable audit trail for compliance requirements (GDPR, SOC2, HIPAA) that tracks all data changes with actor attribution and timestamps.

## Solution

Use `LockchainWriter` to create persistent, cryptographically signed receipts for every transaction. Lockchain anchors entries to Git with Merkle tree verification, ensuring tamper-proof provenance.

---

## Basic Setup

Initialize a LockchainWriter for audit trail persistence:

```javascript
import { LockchainWriter, createDarkMatterCore, DataFactory } from 'unrdf';
const { namedNode, quad, literal } = DataFactory;

// Initialize lockchain writer
const lockchain = new LockchainWriter({
  gitRepo: process.cwd(),           // Git repository path
  refName: 'refs/notes/lockchain',  // Git ref for entries
  enableMerkle: true,               // SHA3-256 Merkle trees
  enableGitAnchoring: true,         // Anchor to Git commits
  batchSize: 10,                    // Auto-commit after 10 entries
  algorithm: 'ed25519',             // Signing algorithm
});

await lockchain.init();
console.log('Lockchain initialized:', lockchain.getStats());
```

---

## Write and Verify Receipts

### Basic Receipt Writing

```javascript
const system = await createDarkMatterCore();

// Execute a transaction
const result = await system.executeTransaction({
  additions: [
    quad(
      namedNode('http://example.org/user/alice'),
      namedNode('http://schema.org/name'),
      literal('Alice Smith')
    ),
  ],
  removals: [],
  actor: 'admin@example.org',
});

// Write cryptographically signed receipt
const receipt = await lockchain.writeReceipt({
  actor: 'admin@example.org',
  action: 'create-user',
  delta: result.delta,
  timestamp: new Date(),
  metadata: {
    ip: '192.168.1.100',
    reason: 'User onboarding',
    sessionId: 'sess_abc123',
  },
});

console.log('Receipt ID:', receipt.id);
console.log('Merkle Root:', receipt.merkleRoot);
console.log('Signature:', receipt.signature.value);
```

### Verify Receipt Integrity

```javascript
// Verify cryptographic integrity
const isValid = await lockchain.verifyReceipt(receipt);

if (isValid) {
  console.log('Receipt cryptographically valid');
} else {
  console.error('TAMPER DETECTED: Receipt integrity compromised');
}

// Full verification with details
const verification = await lockchain.verifyEntry(receipt.id);
console.log('Verification result:', verification);
// { valid: true, entry: { id, timestamp, receipt, signature, merkleRoot } }
```

---

## Compliance Scenarios

### GDPR Article 30: Processing Activities Record

Track all personal data processing activities:

```javascript
import { defineHook, registerHook } from 'unrdf';

const gdprHook = defineHook({
  meta: {
    name: 'gdpr-article-30',
    description: 'GDPR Article 30 processing activities record',
  },

  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  async after({ event, success }) {
    if (!success) return;

    // Detect personal data (schema:Person, foaf:Person patterns)
    const personalDataQuads = event.delta.additions.filter(quad =>
      quad.predicate.value.includes('schema.org') ||
      quad.predicate.value.includes('foaf') ||
      quad.object.value.includes('@') // Email patterns
    );

    if (personalDataQuads.length > 0) {
      await lockchain.writeReceipt({
        actor: event.metadata?.userId || 'system',
        action: 'process-personal-data',
        delta: { additions: personalDataQuads, removals: [] },
        timestamp: new Date(),
        metadata: {
          gdprBasis: 'legitimate-interest', // or 'consent', 'contract'
          dataSubject: extractDataSubject(personalDataQuads),
          processingPurpose: 'user-management',
          retentionPolicy: '3-years',
        },
      });
    }
  },
});

registerHook(gdprHook);
```

### SOC2 CC7.2: System Monitoring

Monitor and log system changes for SOC2 compliance:

```javascript
const soc2Hook = defineHook({
  meta: {
    name: 'soc2-cc7-monitoring',
    description: 'SOC2 CC7.2 change monitoring',
  },

  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  async after({ event, success }) {
    // Classify change severity
    const severity = classifyChangeSeverity(event.delta);

    await lockchain.writeReceipt({
      actor: event.metadata?.userId || 'system',
      action: success ? 'change-applied' : 'change-rejected',
      delta: event.delta,
      timestamp: new Date(),
      metadata: {
        changeType: detectChangeType(event.delta),
        severity,                              // 'critical', 'high', 'medium', 'low'
        affectedResources: countResources(event.delta),
        controlObjective: 'CC7.2',
        evidenceId: `SOC2-${Date.now()}`,
      },
    });

    // Alert on critical changes
    if (severity === 'critical') {
      await sendSecurityAlert({
        type: 'critical-change',
        actor: event.metadata?.userId,
        timestamp: new Date(),
      });
    }
  },
});

function classifyChangeSeverity(delta) {
  const totalChanges = delta.additions.length + delta.removals.length;
  if (totalChanges > 100) return 'critical';
  if (totalChanges > 50) return 'high';
  if (totalChanges > 10) return 'medium';
  return 'low';
}

function detectChangeType(delta) {
  if (delta.removals.length === 0) return 'create';
  if (delta.additions.length === 0) return 'delete';
  return 'update';
}

function countResources(delta) {
  const subjects = new Set();
  delta.additions.forEach(q => subjects.add(q.subject.value));
  delta.removals.forEach(q => subjects.add(q.subject.value));
  return subjects.size;
}

registerHook(soc2Hook);
```

### HIPAA 164.312(b): Audit Controls

Track access to Protected Health Information (PHI):

```javascript
const hipaaHook = defineHook({
  meta: {
    name: 'hipaa-audit-controls',
    description: 'HIPAA 164.312(b) audit controls for PHI',
  },

  channel: {
    graphs: ['http://example.org/phi/*'],  // PHI-specific graphs
  },
  when: { kind: 'transaction' },

  async before({ payload, event }) {
    // Log access attempt (even if it fails)
    await lockchain.writeReceipt({
      actor: event.metadata?.userId || 'unknown',
      action: 'phi-access-attempt',
      delta: { additions: [], removals: [] },
      timestamp: new Date(),
      metadata: {
        accessType: 'write',
        intendedAction: payload.action,
        hipaaCategory: '164.312(b)',
        accessGranted: false, // Will be updated after
      },
    });

    return payload;
  },

  async after({ event, success }) {
    // Log successful PHI modification
    if (success && event.delta.additions.length > 0) {
      await lockchain.writeReceipt({
        actor: event.metadata?.userId,
        action: 'phi-modified',
        delta: event.delta,
        timestamp: new Date(),
        metadata: {
          hipaaCategory: '164.312(b)',
          phiFields: extractPhiFields(event.delta),
          patientIds: extractPatientIds(event.delta),
          accessGranted: true,
          auditTrailId: `HIPAA-${Date.now()}`,
        },
      });
    }
  },
});

function extractPhiFields(delta) {
  return delta.additions
    .map(q => q.predicate.value.split('/').pop())
    .filter(field => PHI_FIELDS.includes(field));
}

function extractPatientIds(delta) {
  return [...new Set(
    delta.additions
      .map(q => q.subject.value)
      .filter(s => s.includes('/patient/'))
  )];
}

const PHI_FIELDS = [
  'name', 'dateOfBirth', 'ssn', 'address', 'phone',
  'email', 'medicalRecordNumber', 'diagnosis', 'treatment'
];

registerHook(hipaaHook);
```

---

## Querying Audit History

### Query by Actor

```javascript
import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';

function queryAuditsByActor(storagePath, actorId) {
  const entries = readdirSync(storagePath)
    .filter(f => f.startsWith('entry-'))
    .map(f => JSON.parse(readFileSync(join(storagePath, f), 'utf8')))
    .filter(entry => entry.receipt.actor === actorId)
    .sort((a, b) => b.timestamp - a.timestamp);

  return entries;
}

// Usage
const aliceAudits = queryAuditsByActor('.lockchain', 'alice@example.org');
console.log(`Found ${aliceAudits.length} audit entries for alice`);
```

### Query by Action Type

```javascript
function queryAuditsByAction(storagePath, action) {
  const entries = readdirSync(storagePath)
    .filter(f => f.startsWith('entry-'))
    .map(f => JSON.parse(readFileSync(join(storagePath, f), 'utf8')))
    .filter(entry => entry.receipt.action === action);

  return entries;
}

// Find all PHI modifications
const phiModifications = queryAuditsByAction('.lockchain', 'phi-modified');
```

### Generate Compliance Report

```javascript
function generateComplianceReport(storagePath, startDate, endDate) {
  const entries = readdirSync(storagePath)
    .filter(f => f.startsWith('entry-'))
    .map(f => JSON.parse(readFileSync(join(storagePath, f), 'utf8')))
    .filter(entry => {
      const ts = entry.timestamp;
      return ts >= startDate.getTime() && ts <= endDate.getTime();
    });

  const report = {
    period: { start: startDate, end: endDate },
    totalEntries: entries.length,
    byActor: {},
    byAction: {},
    byCompliance: { gdpr: 0, soc2: 0, hipaa: 0 },
  };

  entries.forEach(entry => {
    const actor = entry.receipt.actor;
    const action = entry.receipt.action;

    report.byActor[actor] = (report.byActor[actor] || 0) + 1;
    report.byAction[action] = (report.byAction[action] || 0) + 1;

    // Categorize by compliance standard
    if (entry.receipt.metadata?.gdprBasis) report.byCompliance.gdpr++;
    if (entry.receipt.metadata?.controlObjective) report.byCompliance.soc2++;
    if (entry.receipt.metadata?.hipaaCategory) report.byCompliance.hipaa++;
  });

  return report;
}

// Generate Q4 2024 report
const report = generateComplianceReport(
  '.lockchain',
  new Date('2024-10-01'),
  new Date('2024-12-31')
);
console.log(JSON.stringify(report, null, 2));
```

---

## Performance Optimization

### Batch Commits

```javascript
// Configure larger batch size for high-volume systems
const highVolumeLockchain = new LockchainWriter({
  batchSize: 100,           // Commit every 100 entries
  enableGitAnchoring: true,
  enableMerkle: true,
});

// Manually commit when needed
await highVolumeLockchain.commitBatch();
console.log('Batch committed to Git');
```

### Async Writing

```javascript
// Fire-and-forget for non-critical audits
async function writeAuditAsync(lockchain, receipt) {
  // Don't await - let it complete in background
  lockchain.writeReceipt(receipt).catch(err => {
    console.error('Audit write failed:', err);
    // Queue for retry
  });
}
```

### Disable Git for Development

```javascript
// Faster in development (no Git operations)
const devLockchain = new LockchainWriter({
  enableGitAnchoring: false,  // Skip Git commits
  enableMerkle: true,         // Keep cryptographic verification
});
```

---

## Advanced Patterns

### Multi-Standard Compliance

```javascript
const multiComplianceHook = defineHook({
  meta: { name: 'multi-compliance' },
  channel: { graphs: ['*'] },
  when: { kind: 'transaction' },

  async after({ event, success }) {
    const standards = detectApplicableStandards(event);

    for (const standard of standards) {
      await lockchain.writeReceipt({
        actor: event.metadata?.userId,
        action: `${standard.code}-audit`,
        delta: event.delta,
        timestamp: new Date(),
        metadata: {
          standard: standard.name,
          control: standard.control,
          evidence: standard.evidence,
        },
      });
    }
  },
});

function detectApplicableStandards(event) {
  const standards = [];

  // GDPR: Personal data
  if (hasPersonalData(event.delta)) {
    standards.push({ code: 'gdpr', name: 'GDPR', control: 'Article 30' });
  }

  // SOC2: All changes
  standards.push({ code: 'soc2', name: 'SOC2', control: 'CC7.2' });

  // HIPAA: Healthcare data
  if (hasHealthcareData(event.delta)) {
    standards.push({ code: 'hipaa', name: 'HIPAA', control: '164.312(b)' });
  }

  return standards;
}
```

### Tamper Detection

```javascript
async function verifyAuditChain(storagePath) {
  const entries = readdirSync(storagePath)
    .filter(f => f.startsWith('entry-'))
    .map(f => JSON.parse(readFileSync(join(storagePath, f), 'utf8')))
    .sort((a, b) => a.timestamp - b.timestamp);

  const results = { valid: true, errors: [] };

  for (let i = 0; i < entries.length; i++) {
    const entry = entries[i];

    // Verify signature
    const verification = await lockchain.verifyEntry(entry.id);
    if (!verification.valid) {
      results.valid = false;
      results.errors.push({
        entryId: entry.id,
        error: verification.error,
      });
    }

    // Verify chain (previousHash matches prior entry)
    if (i > 0 && entry.previousHash) {
      const prevEntry = entries[i - 1];
      if (entry.previousHash !== prevEntry.signature.value) {
        results.valid = false;
        results.errors.push({
          entryId: entry.id,
          error: 'Chain broken: previousHash mismatch',
        });
      }
    }
  }

  return results;
}

// Run tamper detection
const chainVerification = await verifyAuditChain('.lockchain');
if (!chainVerification.valid) {
  console.error('TAMPER DETECTED:', chainVerification.errors);
}
```

---

## Testing Audit Trails

### Unit Test: Basic Audit

```javascript
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { LockchainWriter } from 'unrdf';
import { mkdirSync, rmSync } from 'fs';

describe('Audit Trail', () => {
  let lockchain;
  const testDir = './test-lockchain';

  beforeEach(async () => {
    mkdirSync(testDir, { recursive: true });
    lockchain = new LockchainWriter({
      gitRepo: testDir,
      enableGitAnchoring: false, // Faster for tests
      enableMerkle: true,
    });
    await lockchain.init();
  });

  afterEach(() => {
    rmSync(testDir, { recursive: true, force: true });
  });

  it('should write and verify receipt', async () => {
    const receipt = await lockchain.writeReceipt({
      actor: 'test@example.org',
      action: 'test-action',
      delta: { additions: [], removals: [] },
      timestamp: new Date(),
    });

    expect(receipt.id).toBeDefined();
    expect(receipt.merkleRoot).toBeDefined();

    const isValid = await lockchain.verifyReceipt(receipt);
    expect(isValid).toBe(true);
  });

  it('should detect tampered entries', async () => {
    const receipt = await lockchain.writeReceipt({
      actor: 'test@example.org',
      action: 'original-action',
      delta: { additions: [], removals: [] },
      timestamp: new Date(),
    });

    // Tamper with receipt
    receipt.receipt.action = 'tampered-action';

    const isValid = await lockchain.verifyReceipt(receipt);
    expect(isValid).toBe(false);
  });
});
```

---

## Related Guides

- [How-To: Create Knowledge Hooks](./create-knowledge-hooks.md) - Hook definition patterns
- [How-To: Handle Transactions](./handle-transactions.md) - Transaction lifecycle
- [Reference: LockchainWriter API](../reference/api-reference.md#lockchainwriter) - Full API documentation
- [Explanation: Cryptographic Provenance](../explanation/concepts/cryptographic-provenance.md) - Design philosophy
