# YAWL Use Cases

Real-world production scenarios using YAWL workflows.

## 1. Document Approval Workflow

**Scenario:** Multi-stage document approval with parallel reviewers and conditional final approval.

### Business Requirements

- Documents submitted by authors
- 3 reviewers approve in parallel (legal, technical, compliance)
- Manager approves if document value > $10,000
- All approvals must complete before publication
- Full audit trail required

### Workflow Design

```javascript
import { createWorkflow } from '@unrdf/yawl';

const workflow = {
  id: 'doc-approval',
  name: 'Document Approval Workflow',
  tasks: [
    { id: 'submit', name: 'Submit Document', kind: 'atomic' },
    { id: 'legal-review', name: 'Legal Review', kind: 'atomic' },
    { id: 'tech-review', name: 'Technical Review', kind: 'atomic' },
    { id: 'compliance-review', name: 'Compliance Review', kind: 'atomic' },
    { id: 'manager-approval', name: 'Manager Approval', kind: 'atomic' },
    { id: 'publish', name: 'Publish Document', kind: 'atomic' },
  ],
  flow: [
    // AND-split: Submit enables all 3 reviews in parallel
    { from: 'submit', to: 'legal-review', splitType: 'AND' },
    { from: 'submit', to: 'tech-review', splitType: 'AND' },
    { from: 'submit', to: 'compliance-review', splitType: 'AND' },

    // AND-join: Manager approval waits for ALL reviews
    { from: 'legal-review', to: 'manager-approval', joinType: 'AND' },
    { from: 'tech-review', to: 'manager-approval', joinType: 'AND' },
    { from: 'compliance-review', to: 'manager-approval', joinType: 'AND' },

    // Final publication
    { from: 'manager-approval', to: 'publish' },
  ],
};
```

### Implementation

```javascript
import { createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';

async function approveDocument(store, workflowId, documentData) {
  // Start case
  const { case_id } = await createCase(store, {
    workflowId,
    caseId: `doc-${documentData.id}`,
    initialData: documentData,
  });

  // Submit document
  const submitWI = await enableTask(store, { caseId: case_id, taskId: 'submit' });
  await startTask(store, {
    caseId: case_id,
    workItemId: submitWI.work_item_id,
    actor: documentData.author,
  });
  const submitReceipt = await completeTask(store, {
    caseId: case_id,
    workItemId: submitWI.work_item_id,
    outputData: { submittedAt: new Date().toISOString() },
  });

  // Parallel reviews (in production, different users would do these)
  await executeReview(store, case_id, 'legal-review', 'legal@company.com');
  await executeReview(store, case_id, 'tech-review', 'tech@company.com');
  await executeReview(store, case_id, 'compliance-review', 'compliance@company.com');

  // Manager approval
  const managerWI = await enableTask(store, { caseId: case_id, taskId: 'manager-approval' });
  await startTask(store, {
    caseId: case_id,
    workItemId: managerWI.work_item_id,
    actor: 'manager@company.com',
  });
  await completeTask(store, {
    caseId: case_id,
    workItemId: managerWI.work_item_id,
    outputData: { approved: true, approvedAt: new Date().toISOString() },
  });

  // Publish
  const publishWI = await enableTask(store, { caseId: case_id, taskId: 'publish' });
  await startTask(store, {
    caseId: case_id,
    workItemId: publishWI.work_item_id,
    actor: 'system@company.com',
  });
  const publishReceipt = await completeTask(store, {
    caseId: case_id,
    workItemId: publishWI.work_item_id,
    outputData: { publishedUrl: `https://docs.company.com/${documentData.id}` },
  });

  return publishReceipt;
}

async function executeReview(store, caseId, taskId, actor) {
  const wi = await enableTask(store, { caseId, taskId });
  await startTask(store, { caseId, workItemId: wi.work_item_id, actor });
  return await completeTask(store, {
    caseId,
    workItemId: wi.work_item_id,
    outputData: { approved: true, reviewedBy: actor, reviewedAt: new Date().toISOString() },
  });
}
```

### Benefits

- Parallel reviews reduce approval time by 60%
- Cryptographic receipts provide audit trail for compliance
- Time-travel queries for debugging approval issues
- SPARQL queries for analytics (average approval time, bottlenecks)

---

## 2. E-commerce Order Processing

**Scenario:** Order fulfillment with payment, inventory, shipping, and cancellation support.

### Business Requirements

- Customer places order
- Payment processed
- Inventory reserved
- Order shipped
- Customer can cancel before shipping
- Refund processed if cancelled

### Workflow Design

```javascript
const workflow = {
  id: 'order-fulfillment',
  name: 'E-commerce Order Fulfillment',
  tasks: [
    { id: 'place-order', name: 'Place Order', kind: 'atomic' },
    { id: 'payment', name: 'Process Payment', kind: 'atomic' },
    { id: 'reserve-inventory', name: 'Reserve Inventory', kind: 'atomic' },
    { id: 'ship', name: 'Ship Order', kind: 'atomic' },
    { id: 'cancel', name: 'Cancel Order', kind: 'atomic' },
    { id: 'refund', name: 'Process Refund', kind: 'atomic' },
    { id: 'release-inventory', name: 'Release Inventory', kind: 'atomic' },
    { id: 'complete', name: 'Order Complete', kind: 'atomic' },
  ],
  flow: [
    // Happy path
    { from: 'place-order', to: 'payment' },
    { from: 'payment', to: 'reserve-inventory' },
    { from: 'reserve-inventory', to: 'ship' },
    { from: 'ship', to: 'complete' },

    // Cancellation path
    { from: 'place-order', to: 'cancel' },
    { from: 'cancel', to: 'refund', splitType: 'AND' },
    { from: 'cancel', to: 'release-inventory', splitType: 'AND' },
    { from: 'refund', to: 'complete', joinType: 'AND' },
    { from: 'release-inventory', to: 'complete', joinType: 'AND' },
  ],
  cancellationRegions: [
    {
      id: 'cancellable-region',
      tasks: ['payment', 'reserve-inventory', 'ship'],
      cancelTrigger: 'cancel',
      rollbackActions: ['refund', 'release-inventory'],
    },
  ],
};
```

### Key Features

- **Cancellation Regions**: Cancel order anytime before shipping
- **Rollback Actions**: Automatic refund + inventory release on cancel
- **Parallel Cleanup**: Refund and inventory release happen simultaneously
- **Event Sourcing**: Complete order history for customer support

---

## 3. DevOps CI/CD Pipeline

**Scenario:** Automated software deployment with parallel tests and conditional deployment.

### Workflow Design

```javascript
const workflow = {
  id: 'ci-cd-pipeline',
  name: 'CI/CD Deployment Pipeline',
  tasks: [
    { id: 'code-commit', name: 'Code Commit', kind: 'atomic' },
    { id: 'build', name: 'Build', kind: 'atomic' },
    { id: 'unit-tests', name: 'Unit Tests', kind: 'atomic' },
    { id: 'integration-tests', name: 'Integration Tests', kind: 'atomic' },
    { id: 'security-scan', name: 'Security Scan', kind: 'atomic' },
    { id: 'deploy-staging', name: 'Deploy to Staging', kind: 'atomic' },
    { id: 'smoke-tests', name: 'Smoke Tests', kind: 'atomic' },
    { id: 'deploy-prod', name: 'Deploy to Production', kind: 'atomic' },
    { id: 'rollback', name: 'Rollback', kind: 'atomic' },
  ],
  flow: [
    // Build
    { from: 'code-commit', to: 'build' },

    // Parallel tests (AND-split)
    { from: 'build', to: 'unit-tests', splitType: 'AND' },
    { from: 'build', to: 'integration-tests', splitType: 'AND' },
    { from: 'build', to: 'security-scan', splitType: 'AND' },

    // All tests must pass (AND-join)
    { from: 'unit-tests', to: 'deploy-staging', joinType: 'AND' },
    { from: 'integration-tests', to: 'deploy-staging', joinType: 'AND' },
    { from: 'security-scan', to: 'deploy-staging', joinType: 'AND' },

    // Staging → Production
    { from: 'deploy-staging', to: 'smoke-tests' },
    {
      from: 'smoke-tests',
      to: 'deploy-prod',
      splitType: 'XOR',
      condition: { tests_passed: true },
    },
    {
      from: 'smoke-tests',
      to: 'rollback',
      splitType: 'XOR',
      condition: { tests_passed: false },
    },
  ],
};
```

### Benefits

- **Parallel Tests**: Run unit, integration, and security tests simultaneously (3x faster)
- **Conditional Deployment**: Auto-deploy if tests pass, rollback if fail
- **Audit Trail**: Every deployment tracked with cryptographic receipts
- **Time Travel**: Debug failed deployments by replaying pipeline state

---

## 4. Healthcare Patient Care Pathway

**Scenario:** Patient admission through diagnosis, treatment, and discharge.

### Workflow Design

```javascript
const workflow = {
  id: 'patient-care',
  name: 'Patient Care Pathway',
  tasks: [
    { id: 'admission', name: 'Patient Admission', kind: 'atomic' },
    { id: 'triage', name: 'Triage Assessment', kind: 'atomic' },
    { id: 'diagnosis', name: 'Diagnosis', kind: 'atomic' },
    { id: 'lab-tests', name: 'Lab Tests', kind: 'atomic' },
    { id: 'imaging', name: 'Medical Imaging', kind: 'atomic' },
    { id: 'treatment-plan', name: 'Create Treatment Plan', kind: 'atomic' },
    { id: 'treatment', name: 'Administer Treatment', kind: 'atomic' },
    { id: 'followup', name: 'Follow-up Assessment', kind: 'atomic' },
    { id: 'discharge', name: 'Discharge Patient', kind: 'atomic' },
    { id: 'emergency', name: 'Emergency Intervention', kind: 'atomic' },
  ],
  flow: [
    // Admission → Triage
    { from: 'admission', to: 'triage' },

    // Triage routing
    {
      from: 'triage',
      to: 'emergency',
      splitType: 'XOR',
      condition: { severity: 'critical' },
    },
    {
      from: 'triage',
      to: 'diagnosis',
      splitType: 'XOR',
      condition: { severity_ne: 'critical' },
    },

    // Parallel diagnostics (OR-split: only needed tests)
    { from: 'diagnosis', to: 'lab-tests', splitType: 'OR', condition: { needs_lab: true } },
    { from: 'diagnosis', to: 'imaging', splitType: 'OR', condition: { needs_imaging: true } },

    // Treatment plan waits for completed diagnostics (OR-join)
    { from: 'lab-tests', to: 'treatment-plan', joinType: 'OR' },
    { from: 'imaging', to: 'treatment-plan', joinType: 'OR' },
    { from: 'diagnosis', to: 'treatment-plan', joinType: 'OR' },

    // Treatment → Follow-up → Discharge
    { from: 'treatment-plan', to: 'treatment' },
    { from: 'treatment', to: 'followup' },
    { from: 'followup', to: 'discharge' },

    // Emergency → Treatment
    { from: 'emergency', to: 'treatment' },
  ],
};
```

### Key Features

- **Dynamic Routing**: Only run needed diagnostic tests (OR-split)
- **Emergency Path**: Fast-track critical patients
- **OR-join**: Treatment plan waits for whichever tests were ordered
- **Compliance**: Full audit trail for HIPAA compliance

---

## 5. Financial Transaction Processing

**Scenario:** Multi-level approval for large transactions with fraud detection.

### Workflow Design

```javascript
const workflow = {
  id: 'transaction-approval',
  name: 'Financial Transaction Approval',
  tasks: [
    { id: 'submit', name: 'Submit Transaction', kind: 'atomic' },
    { id: 'fraud-check', name: 'Fraud Detection', kind: 'atomic' },
    { id: 'auto-approve', name: 'Auto Approve', kind: 'atomic' },
    { id: 'supervisor-approval', name: 'Supervisor Approval', kind: 'atomic' },
    { id: 'director-approval', name: 'Director Approval', kind: 'atomic' },
    { id: 'compliance-check', name: 'Compliance Check', kind: 'atomic' },
    { id: 'settlement', name: 'Settlement', kind: 'atomic' },
    { id: 'reject', name: 'Reject Transaction', kind: 'atomic' },
  ],
  flow: [
    // Fraud check
    { from: 'submit', to: 'fraud-check' },

    // Fraud detection routing
    {
      from: 'fraud-check',
      to: 'reject',
      splitType: 'XOR',
      condition: { fraud_score_gt: 0.8 },
    },
    {
      from: 'fraud-check',
      to: 'auto-approve',
      splitType: 'XOR',
      condition: { amount_lt: 10000, fraud_score_lt: 0.3 },
    },
    {
      from: 'fraud-check',
      to: 'supervisor-approval',
      splitType: 'XOR',
      condition: { amount_gte: 10000, amount_lt: 100000 },
    },
    {
      from: 'fraud-check',
      to: 'director-approval',
      splitType: 'XOR',
      condition: { amount_gte: 100000 },
    },

    // All paths → Compliance check
    { from: 'auto-approve', to: 'compliance-check', joinType: 'XOR' },
    { from: 'supervisor-approval', to: 'compliance-check', joinType: 'XOR' },
    { from: 'director-approval', to: 'compliance-check', joinType: 'XOR' },

    // Settlement
    { from: 'compliance-check', to: 'settlement' },
  ],
};
```

### Benefits

- **Multi-Level Approval**: Automatic routing based on amount
- **Fraud Detection**: Block suspicious transactions
- **Compliance**: All transactions checked before settlement
- **Audit Trail**: Cryptographic receipts for financial auditing

---

## Best Practices

### 1. Workflow Design

- Keep workflows focused (single responsibility)
- Use descriptive task names
- Document conditions clearly
- Model exceptional paths (cancellation, errors)

### 2. Error Handling

- Define cancellation regions for rollback
- Use receipts to track failures
- Implement retry logic for transient failures
- Log all exceptions for debugging

### 3. Performance

- Minimize RDF store queries
- Use batch operations where possible
- Cache workflow definitions
- Index SPARQL queries

### 4. Testing

- Test happy path + error paths
- Test all conditional branches
- Verify cancellation logic
- Load test with concurrent cases

### 5. Production Deployment

- Monitor workflow execution metrics
- Set up alerts for stuck cases
- Archive completed cases periodically
- Back up RDF store regularly

---

## Next Steps

- Explore [YAWL Patterns Guide](./yawl-patterns.md) for all patterns
- Run [Examples](../../examples/yawl/) to see patterns in action
- Read [API Reference](../../packages/yawl/README.md) for full documentation

---

**Questions?** See [UNRDF Documentation](https://github.com/unrdf/unrdf)
