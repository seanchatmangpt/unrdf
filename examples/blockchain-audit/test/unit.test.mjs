/**
 * @file Unit Tests
 * @description Unit tests for blockchain audit components
 */

import { describe, it, expect } from 'vitest';
import { AuditTrail } from '../src/audit-trail.mjs';

describe('AuditTrail', () => {
  it('should create audit trail', () => {
    const audit = new AuditTrail();
    expect(audit).toBeDefined();
    expect(audit.receipts).toBeDefined();
    expect(audit.blockHashes).toEqual([]);
  });

  it('should record workflow execution', async () => {
    const audit = new AuditTrail();

    const workflow = {
      id: 'test',
      tasks: [{ id: 'task1' }],
    };

    const record = await audit.recordExecution(workflow, { data: 'test' });

    expect(record.workflowId).toBeDefined();
    expect(record.receipt).toBeDefined();
    expect(record.receipt.hash).toBeDefined();
    expect(record.txHash).toBeDefined();
  });

  it('should verify audit record', async () => {
    const audit = new AuditTrail();

    const workflow = { id: 'verify-test', tasks: [] };
    const record = await audit.recordExecution(workflow, {});

    const verification = await audit.verify(record.workflowId);

    expect(verification.valid).toBe(true);
    expect(verification.workflowId).toBe(record.workflowId);
  });

  it('should generate compliance report', async () => {
    const audit = new AuditTrail();

    const report = await audit.generateComplianceReport();

    expect(report).toBeDefined();
    expect(report.summary).toBeDefined();
    expect(report.compliance).toBeDefined();
  });
});
