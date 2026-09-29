/**
 * @file Tamper detection tests
 * @description verify() must fail when receipts or the block chain are altered.
 */

import { describe, it, expect } from 'vitest';
import { AuditTrail } from '../src/audit-trail.mjs';

const workflow = { id: 'wf', tasks: [{ id: 't1' }] };

describe('AuditTrail tamper detection', () => {
  it('detects a modified receipt payload', async () => {
    const audit = new AuditTrail();
    const record = await audit.recordExecution(workflow, { amount: 10 });

    audit.receipts.get(record.workflowId).data.input.amount = 1000000;

    const result = await audit.verify(record.workflowId);
    expect(result.valid).toBe(false);
    expect(result.receipt).toBe(false);
  });

  it('detects a modified block in the chain', async () => {
    const audit = new AuditTrail();
    const first = await audit.recordExecution(workflow, { n: 1 });
    await audit.recordExecution(workflow, { n: 2 });

    audit.blocks[0].timestamp += 1;

    const result = await audit.verify(first.workflowId);
    expect(result.valid).toBe(false);
    expect(result.chain).toBe(false);
  });

  it('detects a broken previousHash link', async () => {
    const audit = new AuditTrail();
    await audit.recordExecution(workflow, { n: 1 });
    const second = await audit.recordExecution(workflow, { n: 2 });

    audit.blocks[1].previousHash = 'f'.repeat(64);

    const result = await audit.verify(second.workflowId);
    expect(result.chain).toBe(false);
    expect(result.valid).toBe(false);
  });

  it('accepts an untouched multi-block chain', async () => {
    const audit = new AuditTrail();
    const a = await audit.recordExecution(workflow, { n: 1 });
    const b = await audit.recordExecution(workflow, { n: 2 });

    expect((await audit.verify(a.workflowId)).valid).toBe(true);
    expect((await audit.verify(b.workflowId)).valid).toBe(true);
    expect(audit.blocks).toHaveLength(2);
  });
});
