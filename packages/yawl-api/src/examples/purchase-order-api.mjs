/**
 * @file Purchase Order Approval API - Complete example
 * @module @unrdf/yawl-api/examples/purchase-order-api
 *
 * @description
 * Demonstrates a complete purchase order approval workflow exposed as REST API:
 *
 * Workflow:
 * 1. Submit PO → 2. Review → 3a. Approve OR 3b. Reject
 *
 * Features:
 * - Auto-generated REST endpoints
 * - OpenAPI documentation
 * - HATEOAS hypermedia controls
 * - Zod validation
 * - Swagger UI at /docs
 *
 * @example
 * # Start the server
 * node src/examples/purchase-order-api.mjs
 *
 * # Create a purchase order
 * curl -X POST http://localhost:3000/api/workflows/purchase-order/cases \
 *   -H "Content-Type: application/json" \
 *   -d '{"initialData": {"amount": 1500, "vendor": "Acme Corp"}}'
 */

import { createYAWLAPIServer } from '../server.mjs';
import { createWorkflowEngine } from '@unrdf/yawl';
import { SPLIT_TYPE, JOIN_TYPE } from '@unrdf/yawl';

// =============================================================================
// Purchase Order Workflow Definition
// =============================================================================

/**
 * Purchase order approval workflow with conditional routing
 */
const purchaseOrderWorkflow = {
  id: 'purchase-order',
  name: 'Purchase Order Approval Workflow',
  version: '1.0.0',
  description: 'Three-step approval process with amount-based routing',

  tasks: [
    {
      id: 'submit',
      name: 'Submit Purchase Order',
      description: 'Employee submits a purchase order request',
      inputSchema: {
        type: 'object',
        properties: {
          amount: { type: 'number', minimum: 0 },
          vendor: { type: 'string', minLength: 1 },
          description: { type: 'string' },
          category: { type: 'string', enum: ['supplies', 'equipment', 'services'] },
        },
        required: ['amount', 'vendor'],
      },
      outputSchema: {
        type: 'object',
        properties: {
          submittedBy: { type: 'string' },
          submittedAt: { type: 'string', format: 'date-time' },
        },
      },
      splitType: SPLIT_TYPE.AND,
      joinType: JOIN_TYPE.XOR,
    },
    {
      id: 'review',
      name: 'Review Purchase Order',
      description: 'Manager reviews the purchase order',
      inputSchema: {
        type: 'object',
        properties: {
          amount: { type: 'number' },
          vendor: { type: 'string' },
        },
      },
      outputSchema: {
        type: 'object',
        properties: {
          reviewedBy: { type: 'string' },
          reviewNotes: { type: 'string' },
          decision: { type: 'string', enum: ['approve', 'reject'] },
        },
        required: ['reviewedBy', 'decision'],
      },
      splitType: SPLIT_TYPE.XOR, // Decision point
      joinType: JOIN_TYPE.XOR,
      role: 'manager',
    },
    {
      id: 'approve',
      name: 'Approve Purchase Order',
      description: 'Purchase order is approved',
      inputSchema: {
        type: 'object',
        properties: {
          amount: { type: 'number' },
        },
      },
      outputSchema: {
        type: 'object',
        properties: {
          approvedBy: { type: 'string' },
          poNumber: { type: 'string' },
        },
      },
      splitType: SPLIT_TYPE.AND,
      joinType: JOIN_TYPE.XOR,
    },
    {
      id: 'reject',
      name: 'Reject Purchase Order',
      description: 'Purchase order is rejected',
      inputSchema: {
        type: 'object',
        properties: {
          reason: { type: 'string' },
        },
      },
      outputSchema: {
        type: 'object',
        properties: {
          rejectedBy: { type: 'string' },
          rejectionReason: { type: 'string' },
        },
      },
      splitType: SPLIT_TYPE.AND,
      joinType: JOIN_TYPE.XOR,
    },
  ],

  flows: [
    {
      from: 'submit',
      to: 'review',
      description: 'Submit to review',
    },
    {
      from: 'review',
      to: 'approve',
      condition: '(ctx) => ctx.decision === "approve"',
      priority: 10,
      description: 'Approve if decision is approve',
    },
    {
      from: 'review',
      to: 'reject',
      condition: '(ctx) => ctx.decision === "reject"',
      priority: 5,
      isDefault: true,
      description: 'Reject if decision is reject',
    },
  ],

  startTaskId: 'submit',
  endTaskIds: ['approve', 'reject'],
};

// =============================================================================
// Simpler Alternative: Small Purchase Order Workflow
// =============================================================================

/**
 * Simplified workflow for small purchases (< $1000) - auto-approve
 */
const smallPurchaseWorkflow = {
  id: 'small-purchase',
  name: 'Small Purchase Fast Track',
  version: '1.0.0',
  description: 'Auto-approval for purchases under $1000',

  tasks: [
    {
      id: 'submit-small',
      name: 'Submit Small Purchase',
      description: 'Submit purchase under $1000',
      splitType: SPLIT_TYPE.AND,
      joinType: JOIN_TYPE.XOR,
    },
    {
      id: 'auto-approve',
      name: 'Auto Approve',
      description: 'Automatically approved',
      splitType: SPLIT_TYPE.AND,
      joinType: JOIN_TYPE.XOR,
    },
  ],

  flows: [
    {
      from: 'submit-small',
      to: 'auto-approve',
    },
  ],

  startTaskId: 'submit-small',
  endTaskIds: ['auto-approve'],
};

// =============================================================================
// Server Setup and Startup
// =============================================================================

/**
 * Initialize and start the Purchase Order API server
 */
async function startPurchaseOrderAPI() {
  console.log('='.repeat(70));
  console.log('YAWL Purchase Order Approval API');
  console.log('='.repeat(70));

  // Create workflow engine
  const engine = createWorkflowEngine({
    nodeId: 'purchase-order-api-node',
    maxConcurrentCases: 1000,
  });

  // Register workflows
  engine.registerWorkflow(purchaseOrderWorkflow);
  engine.registerWorkflow(smallPurchaseWorkflow);

  console.log('\nRegistered workflows:');
  console.log(`  - ${purchaseOrderWorkflow.id}: ${purchaseOrderWorkflow.name}`);
  console.log(`  - ${smallPurchaseWorkflow.id}: ${smallPurchaseWorkflow.name}`);

  // Create API server
  const server = await createYAWLAPIServer({
    engine,
    baseUrl: 'http://localhost:3000',
    enableSwagger: true,
    swaggerOptions: {
      openapi: {
        info: {
          title: 'Purchase Order Approval API',
          description: `
# Purchase Order Approval Workflow API

RESTful API for managing purchase order approvals using YAWL workflow engine.

## Features

- **HATEOAS**: Hypermedia controls guide you through available actions
- **OpenAPI 3.1**: Full API documentation
- **Zod Validation**: Type-safe request/response validation
- **Workflow Patterns**: XOR splits, AND joins, conditional routing

## Quick Start

### 1. Create a Purchase Order Case

\`\`\`bash
curl -X POST http://localhost:3000/api/workflows/purchase-order/cases \\
  -H "Content-Type: application/json" \\
  -d '{
    "initialData": {
      "amount": 1500,
      "vendor": "Acme Corp",
      "description": "Office supplies",
      "category": "supplies"
    }
  }'
\`\`\`

### 2. Follow HATEOAS Links

The response includes \`_links\` with available actions:
- Start enabled tasks
- Complete running tasks
- View case status

### 3. Complete the Workflow

Use the links to:
1. Start the review task
2. Complete review with decision (approve/reject)
3. Case automatically completes

## Workflow Structure

\`\`\`
Submit PO → Review → [Approve OR Reject]
\`\`\`

- **Submit**: Employee submits purchase order
- **Review**: Manager reviews (XOR split point)
- **Approve**: Approved path (if decision=approve)
- **Reject**: Rejected path (if decision=reject)
          `,
          version: '1.0.0',
        },
        servers: [
          {
            url: 'http://localhost:3000',
            description: 'Local development server',
          },
        ],
      },
    },
  });

  // Start server
  const address = await server.listen({ port: 3000, host: '0.0.0.0' });

  console.log('\n' + '='.repeat(70));
  console.log('Server Information');
  console.log('='.repeat(70));
  console.log(`\nAPI Server:    ${address}`);
  console.log(`Swagger UI:    ${address}/docs`);
  console.log(`Health Check:  ${address}/health`);

  console.log('\n' + '='.repeat(70));
  console.log('Example API Calls');
  console.log('='.repeat(70));

  console.log('\n1. List workflows:');
  console.log(`   curl ${address}/api/workflows`);

  console.log('\n2. Create purchase order case:');
  console.log(`   curl -X POST ${address}/api/workflows/purchase-order/cases \\`);
  console.log(`     -H "Content-Type: application/json" \\`);
  console.log(`     -d '{"initialData": {"amount": 1500, "vendor": "Acme Corp"}}'`);

  console.log('\n3. List all cases:');
  console.log(`   curl ${address}/api/cases`);

  console.log('\n4. Get case details (with HATEOAS links):');
  console.log(`   curl ${address}/api/cases/<caseId>`);

  console.log('\n' + '='.repeat(70));
  console.log('Press Ctrl+C to stop the server');
  console.log('='.repeat(70) + '\n');

  // Handle shutdown gracefully
  process.on('SIGINT', async () => {
    console.log('\n\nShutting down server...');
    await server.close();
    console.log('Server closed');
    process.exit(0);
  });
}

// =============================================================================
// Start Server (if run directly)
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  startPurchaseOrderAPI().catch(err => {
    console.error('Failed to start server:', err);
    process.exit(1);
  });
}

export { startPurchaseOrderAPI, purchaseOrderWorkflow, smallPurchaseWorkflow };
