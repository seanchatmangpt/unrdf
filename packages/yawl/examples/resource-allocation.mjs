/**
 * @file YAWL Resource Allocation Example
 *
 * Demonstrates:
 * - Creating resource types (Participant, Tool, Role)
 * - Policy pack registration with SPARQL eligibility
 * - Resource allocation with capacity tracking
 * - Resource pools with allocation strategies
 * - Availability calendar management
 *
 * @module @unrdf/yawl/examples/resource-allocation
 */

import {
  createResourceManager,
  createParticipant,
  createTool,
  createRole,
  createPolicyPack,
  ResourceType,
} from '../src/resources/index.mjs';

/* ========================================================================= */
/* Example: Basic Resource Allocation                                        */
/* ========================================================================= */

/**
 * Demonstrates basic resource types and allocation
 */
async function basicResourceAllocation() {
  console.log('='.repeat(60));
  console.log('Example 1: Basic Resource Allocation');
  console.log('='.repeat(60));

  // Create resource manager
  const manager = createResourceManager();

  // Define resources
  const alice = createParticipant({
    id: 'alice',
    name: 'Alice Smith',
    capacity: 2, // Can handle 2 concurrent work items
  });

  const bob = createParticipant({
    id: 'bob',
    name: 'Bob Johnson',
    capacity: 1,
  });

  const emailService = createTool({
    id: 'email-service',
    name: 'Email Notification Service',
    capacity: -1, // Unlimited
  });

  const approvers = createRole({
    id: 'approvers',
    name: 'Approval Team',
    capacity: -1, // Role members can vary
  });

  console.log('\nCreated resources:');
  console.log('- Participant: Alice (capacity: 2)');
  console.log('- Participant: Bob (capacity: 1)');
  console.log('- Tool: Email Service (unlimited capacity)');
  console.log('- Role: Approvers');

  // Create policy pack
  const policyPack = createPolicyPack({
    id: 'approval-workflow',
    name: 'Approval Workflow Resources',
    resources: [alice, bob, emailService, approvers],
    priority: 10,
  });

  // Register with manager
  manager.registerPolicyPack(policyPack);
  console.log('\nRegistered policy pack: approval-workflow');

  // Allocate resource to work item
  const workItem1 = {
    id: 'wi-001',
    taskId: 'review-document',
    caseId: 'case-123',
    createdAt: new Date().toISOString(),
  };

  try {
    const receipt = await manager.allocateResource(workItem1, alice);
    console.log('\nAllocation successful:');
    console.log(`  Work Item: ${receipt.workItemId}`);
    console.log(`  Resource: ${receipt.resourceId} (${receipt.resourceType})`);
    console.log(`  Allocated At: ${receipt.allocatedAt}`);
    console.log(`  Proof:`, receipt.proof);
  } catch (error) {
    console.log(`\nAllocation failed: ${error.message}`);
  }

  // Check capacity status
  const status = manager.getCapacityStatus('alice');
  console.log('\nAlice capacity status:');
  console.log(`  Current: ${status.current}/${status.max}`);
  console.log(`  Available: ${status.available}`);
  console.log(`  Utilization: ${status.utilizationPercent}%`);

  return manager;
}

/* ========================================================================= */
/* Example: SPARQL Eligibility Conditions                                    */
/* ========================================================================= */

/**
 * Demonstrates SPARQL-based eligibility rules
 */
async function sparqlEligibility() {
  console.log('\n' + '='.repeat(60));
  console.log('Example 2: SPARQL Eligibility Conditions');
  console.log('='.repeat(60));

  const manager = createResourceManager();

  // Resource with SPARQL eligibility condition
  const seniorApprover = createRole({
    id: 'senior-approvers',
    name: 'Senior Approval Team',
    sparql: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
      ASK {
        ?person foaf:hasRole <http://example.org/roles/senior-approver> ;
                yawl:experienceYears ?years .
        FILTER(?years >= 5)
      }
    `,
  });

  console.log('\nCreated role with SPARQL eligibility:');
  console.log('  - senior-approvers: Requires 5+ years experience');

  const policyPack = createPolicyPack({
    id: 'senior-review',
    name: 'Senior Review Process',
    resources: [seniorApprover],
    priority: 20,
  });

  manager.registerPolicyPack(policyPack);

  // Get eligible resources for a task
  const eligible = await manager.getEligibleResources('critical-review', 'case-456');
  console.log(`\nEligible resources for 'critical-review': ${eligible.length}`);

  return manager;
}

/* ========================================================================= */
/* Example: Resource Pools                                                   */
/* ========================================================================= */

/**
 * Demonstrates resource pool allocation strategies
 */
async function resourcePools() {
  console.log('\n' + '='.repeat(60));
  console.log('Example 3: Resource Pools');
  console.log('='.repeat(60));

  const manager = createResourceManager();

  // Create pool of reviewers with round-robin allocation
  const pool = manager.createResourcePool({
    id: 'reviewers-pool',
    name: 'Document Reviewers',
    resources: [
      createParticipant({ id: 'reviewer-1', name: 'Reviewer 1', capacity: 1 }),
      createParticipant({ id: 'reviewer-2', name: 'Reviewer 2', capacity: 1 }),
      createParticipant({ id: 'reviewer-3', name: 'Reviewer 3', capacity: 1 }),
    ],
    allocationStrategy: 'round-robin',
  });

  console.log('\nCreated resource pool: reviewers-pool');
  console.log('  Strategy: round-robin');
  console.log(`  Resources: ${pool.resources.length}`);

  // Allocate from pool (round-robin)
  const workItems = [
    { id: 'wi-100', taskId: 'review', caseId: 'case-a' },
    { id: 'wi-101', taskId: 'review', caseId: 'case-b' },
    { id: 'wi-102', taskId: 'review', caseId: 'case-c' },
  ];

  console.log('\nAllocating work items via round-robin:');
  for (const wi of workItems) {
    const receipt = await pool.allocateAny(wi);
    if (receipt) {
      console.log(`  ${wi.id} -> ${receipt.resourceId}`);
    } else {
      console.log(`  ${wi.id} -> No resource available`);
    }
  }

  // Check pool availability
  const availability = pool.getAvailability();
  console.log('\nPool availability:');
  console.log(`  Available: ${availability.availableCount}/${availability.totalCount}`);

  return manager;
}

/* ========================================================================= */
/* Example: Availability Calendar                                            */
/* ========================================================================= */

/**
 * Demonstrates resource availability/calendar management
 */
async function availabilityCalendar() {
  console.log('\n' + '='.repeat(60));
  console.log('Example 4: Availability Calendar');
  console.log('='.repeat(60));

  const manager = createResourceManager();

  // Create participant
  const alice = createParticipant({
    id: 'alice-calendar',
    name: 'Alice (with schedule)',
    capacity: 1,
  });

  // Register via policy pack
  manager.registerPolicyPack(createPolicyPack({
    id: 'calendar-test',
    resources: [alice],
  }));

  // Set availability schedule
  const today = new Date();
  const tomorrow = new Date(today.getTime() + 86400000);

  manager.setAvailability('alice-calendar', true, [
    {
      start: today.toISOString(),
      end: new Date(today.getTime() + 8 * 3600000).toISOString(), // 8 hours
      available: true,
    },
    {
      start: new Date(today.getTime() + 8 * 3600000).toISOString(),
      end: tomorrow.toISOString(),
      available: false, // Off hours
    },
  ]);

  console.log('\nSet availability for alice-calendar:');
  console.log('  - Available: Today 8 hours');
  console.log('  - Unavailable: After 8 hours');

  // Check availability
  const availability = manager.getAvailability('alice-calendar');
  console.log('\nCurrent availability:');
  console.log(`  Available: ${availability.available}`);
  console.log(`  Windows: ${availability.windows.length}`);

  return manager;
}

/* ========================================================================= */
/* Example: Policy Pack Priority                                             */
/* ========================================================================= */

/**
 * Demonstrates policy pack priority for resource selection
 */
async function policyPackPriority() {
  console.log('\n' + '='.repeat(60));
  console.log('Example 5: Policy Pack Priority');
  console.log('='.repeat(60));

  const manager = createResourceManager();

  // Low priority pack
  manager.registerPolicyPack(createPolicyPack({
    id: 'default-resources',
    name: 'Default Resources',
    resources: [
      createParticipant({ id: 'default-user', capacity: 10 }),
    ],
    priority: 0,
  }));

  // High priority pack
  manager.registerPolicyPack(createPolicyPack({
    id: 'premium-resources',
    name: 'Premium Resources',
    resources: [
      createParticipant({ id: 'premium-user', capacity: 5 }),
    ],
    priority: 100,
  }));

  console.log('\nRegistered policy packs:');
  for (const pack of manager.listPolicyPacks()) {
    console.log(`  - ${pack.id} (priority: ${pack.priority})`);
  }

  // Get eligible resources (ordered by priority)
  const eligible = await manager.getEligibleResources('any-task', 'any-case');
  console.log('\nEligible resources (by priority):');
  for (const resource of eligible) {
    console.log(`  - ${resource.id} (from ${resource._policyPackId}, priority: ${resource._priority})`);
  }

  return manager;
}

/* ========================================================================= */
/* Example: Complete Workflow Resource Scenario                              */
/* ========================================================================= */

/**
 * Demonstrates a complete workflow resource allocation scenario
 */
async function completeWorkflowScenario() {
  console.log('\n' + '='.repeat(60));
  console.log('Example 6: Complete Workflow Scenario');
  console.log('='.repeat(60));

  const manager = createResourceManager();

  // Setup: Create workflow resources
  console.log('\nSetup: Creating workflow resources...');

  // Define the approval workflow resources
  const policyPack = createPolicyPack({
    id: 'expense-approval-workflow',
    name: 'Expense Approval Workflow',
    resources: [
      // Human participants
      createParticipant({
        id: 'clerk-01',
        name: 'Expense Clerk',
        capacity: 5,
      }),
      createParticipant({
        id: 'manager-01',
        name: 'Department Manager',
        capacity: 3,
      }),
      createParticipant({
        id: 'finance-01',
        name: 'Finance Officer',
        capacity: 2,
      }),

      // Automated tools
      createTool({
        id: 'validation-service',
        name: 'Expense Validation Service',
        capacity: -1,
      }),
      createTool({
        id: 'payment-gateway',
        name: 'Payment Gateway',
        capacity: 100,
      }),

      // Roles
      createRole({
        id: 'approvers',
        name: 'Expense Approvers',
      }),
    ],
    priority: 50,
  });

  manager.registerPolicyPack(policyPack);

  // Simulate workflow execution
  console.log('\nWorkflow Execution:');

  // Step 1: Submit expense (clerk receives)
  const case1 = { id: 'exp-2024-001', taskId: 'submit-expense', caseId: 'expense-2024-001' };
  console.log('\n[Step 1] Submit Expense');

  const clerkResource = policyPack.resources.find(r => r.id === 'clerk-01');
  const receipt1 = await manager.allocateResource(case1, clerkResource);
  console.log(`  Allocated: ${receipt1.resourceId}`);
  console.log(`  Receipt ID: ${receipt1.id}`);

  // Check clerk capacity
  const clerkStatus = manager.getCapacityStatus('clerk-01');
  console.log(`  Clerk capacity: ${clerkStatus.current}/${clerkStatus.max}`);

  // Step 2: Auto-validation (tool)
  console.log('\n[Step 2] Auto-Validation');
  const case2 = { id: 'exp-2024-001-validate', taskId: 'validate-expense', caseId: 'expense-2024-001' };
  const validationTool = policyPack.resources.find(r => r.id === 'validation-service');
  const receipt2 = await manager.allocateResource(case2, validationTool);
  console.log(`  Allocated: ${receipt2.resourceId} (Tool)`);
  console.log(`  Capacity: unlimited`);

  // Step 3: Complete validation and deallocate
  console.log('\n[Step 3] Complete Validation');
  manager.deallocateResource(receipt2.id);
  console.log(`  Deallocated: ${receipt2.id}`);

  // Step 4: Manager approval
  console.log('\n[Step 4] Manager Approval');
  const case3 = { id: 'exp-2024-001-approve', taskId: 'approve-expense', caseId: 'expense-2024-001' };
  const managerResource = policyPack.resources.find(r => r.id === 'manager-01');
  const receipt3 = await manager.allocateResource(case3, managerResource);
  console.log(`  Allocated: ${receipt3.resourceId}`);
  console.log(`  Manager capacity: ${manager.getCapacityStatus('manager-01').current}/${manager.getCapacityStatus('manager-01').max}`);

  // Summary
  console.log('\n[Summary] Active Allocations:');
  const activeAllocations = manager.getActiveAllocations();
  for (const alloc of activeAllocations) {
    console.log(`  - ${alloc.allocationId}: ${alloc.resourceId} <- ${alloc.workItemId}`);
  }

  return manager;
}

/* ========================================================================= */
/* Run All Examples                                                          */
/* ========================================================================= */

async function runAllExamples() {
  console.log('\n');
  console.log('*'.repeat(60));
  console.log('*  YAWL Resource Allocation Examples');
  console.log('*'.repeat(60));

  try {
    await basicResourceAllocation();
    await sparqlEligibility();
    await resourcePools();
    await availabilityCalendar();
    await policyPackPriority();
    await completeWorkflowScenario();

    console.log('\n' + '='.repeat(60));
    console.log('All examples completed successfully!');
    console.log('='.repeat(60) + '\n');
  } catch (error) {
    console.error('\nExample failed:', error);
    process.exit(1);
  }
}

// Run examples if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllExamples();
}

export {
  basicResourceAllocation,
  sparqlEligibility,
  resourcePools,
  availabilityCalendar,
  policyPackPriority,
  completeWorkflowScenario,
  runAllExamples,
};
