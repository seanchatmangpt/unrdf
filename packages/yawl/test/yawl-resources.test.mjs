/**
 * @file YAWL Resource Allocation Tests
 *
 * Tests for YAWL resource allocation semantics including:
 * - Resource types (Participant, Tool, Role)
 * - Policy pack registration
 * - Capacity tracking
 * - Eligibility checks
 * - Resource pools
 * - Availability calendar
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  YawlResourceManager,
  ResourcePool,
  createResourceManager,
  createParticipant,
  createTool,
  createRole,
  createPolicyPack,
  ResourceType,
  YAWL_NS,
} from '../src/resources/index.mjs';
import {
  createTestWorkflow,
  createTestTask,
  createTestCase,
} from './test-helpers.mjs';

describe('YawlResourceManager', () => {
  /** @type {YawlResourceManager} */
  let manager;

  beforeEach(() => {
    manager = createResourceManager();
  });

  describe('Resource Types', () => {
    it('should create Participant resource with capacity=1', () => {
      const participant = createParticipant({
        id: 'alice',
        name: 'Alice',
        capacity: 1,
      });

      expect(participant.type).toBe(ResourceType.PARTICIPANT);
      expect(participant.id).toBe('alice');
      expect(participant.capacity).toBe(1);
    });

    it('should create Tool resource with unlimited capacity', () => {
      const tool = createTool({
        id: 'email-service',
        name: 'Email Service',
      });

      expect(tool.type).toBe(ResourceType.TOOL);
      expect(tool.capacity).toBe(-1); // Unlimited
    });

    it('should create Role resource with SPARQL eligibility', () => {
      const sparqlQuery = 'ASK { ?person foaf:hasRole ?role }';
      const role = createRole({
        id: 'approvers',
        sparql: sparqlQuery,
      });

      expect(role.type).toBe(ResourceType.ROLE);
      expect(role.sparql).toBe(sparqlQuery);
    });
  });

  describe('Policy Pack Registration', () => {
    it('should register policy pack with resources', () => {
      const policyPack = createPolicyPack({
        id: 'test-pack',
        name: 'Test Policy Pack',
        resources: [
          createParticipant({ id: 'user-1' }),
          createTool({ id: 'tool-1' }),
        ],
        priority: 10,
      });

      manager.registerPolicyPack(policyPack);

      const retrieved = manager.getPolicyPack('test-pack');
      expect(retrieved).toBeDefined();
      expect(retrieved.id).toBe('test-pack');
      expect(retrieved.resources).toHaveLength(2);
      expect(retrieved.priority).toBe(10);
    });

    it('should list policy packs sorted by priority', () => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'low-priority',
        resources: [],
        priority: 1,
      }));

      manager.registerPolicyPack(createPolicyPack({
        id: 'high-priority',
        resources: [],
        priority: 100,
      }));

      const packs = manager.listPolicyPacks();
      expect(packs[0].id).toBe('high-priority');
      expect(packs[1].id).toBe('low-priority');
    });

    it('should unregister policy pack', () => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'to-remove',
        resources: [],
      }));

      expect(manager.unregisterPolicyPack('to-remove')).toBe(true);
      expect(manager.getPolicyPack('to-remove')).toBeUndefined();
    });
  });

  describe('Resource Allocation', () => {
    beforeEach(() => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'allocation-test',
        resources: [
          createParticipant({ id: 'alice', capacity: 2 }),
          createTool({ id: 'service', capacity: -1 }),
        ],
      }));
    });

    it('should allocate resource and return receipt', async () => {
      const workItem = {
        id: 'wi-001',
        taskId: 'task-1',
        caseId: 'case-1',
      };

      const resource = createParticipant({ id: 'alice', capacity: 2 });
      const receipt = await manager.allocateResource(workItem, resource);

      expect(receipt).toBeDefined();
      expect(receipt.workItemId).toBe('wi-001');
      expect(receipt.resourceId).toBe('alice');
      expect(receipt.resourceType).toBe(ResourceType.PARTICIPANT);
      expect(receipt.proof.capacityCheck).toBe(true);
      expect(receipt.proof.eligibilityCheck).toBe(true);
    });

    it('should reject allocation when capacity exceeded', async () => {
      const resource = createParticipant({ id: 'limited', capacity: 1 });

      manager.registerPolicyPack(createPolicyPack({
        id: 'limited-test',
        resources: [resource],
      }));

      // First allocation should succeed
      await manager.allocateResource(
        { id: 'wi-1', taskId: 't', caseId: 'c' },
        resource
      );

      // Second allocation should fail
      await expect(
        manager.allocateResource(
          { id: 'wi-2', taskId: 't', caseId: 'c' },
          resource
        )
      ).rejects.toThrow(/Capacity exceeded/);
    });

    it('should deallocate resource', async () => {
      const resource = createParticipant({ id: 'bob', capacity: 1 });
      manager.registerPolicyPack(createPolicyPack({
        id: 'dealloc-test',
        resources: [resource],
      }));

      const receipt = await manager.allocateResource(
        { id: 'wi-1', taskId: 't', caseId: 'c' },
        resource
      );

      const success = manager.deallocateResource(receipt.id);
      expect(success).toBe(true);

      // Should be able to allocate again
      const receipt2 = await manager.allocateResource(
        { id: 'wi-2', taskId: 't', caseId: 'c' },
        resource
      );
      expect(receipt2).toBeDefined();
    });
  });

  describe('Capacity Tracking', () => {
    it('should track capacity status', async () => {
      const resource = createParticipant({ id: 'tracked', capacity: 3 });
      manager.registerPolicyPack(createPolicyPack({
        id: 'capacity-test',
        resources: [resource],
      }));

      // Initial state
      let status = manager.getCapacityStatus('tracked');
      expect(status.current).toBe(0);
      expect(status.max).toBe(3);
      expect(status.available).toBe(3);

      // After one allocation
      await manager.allocateResource(
        { id: 'wi-1', taskId: 't', caseId: 'c' },
        resource
      );

      status = manager.getCapacityStatus('tracked');
      expect(status.current).toBe(1);
      expect(status.available).toBe(2);
      expect(status.utilizationPercent).toBe(33);
    });

    it('should list active allocations', async () => {
      const resource = createParticipant({ id: 'active-test', capacity: 5 });
      manager.registerPolicyPack(createPolicyPack({
        id: 'active-test',
        resources: [resource],
      }));

      await manager.allocateResource(
        { id: 'wi-1', taskId: 't', caseId: 'c' },
        resource
      );
      await manager.allocateResource(
        { id: 'wi-2', taskId: 't', caseId: 'c' },
        resource
      );

      const allocations = manager.getActiveAllocations();
      expect(allocations.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('Resource Eligibility', () => {
    it('should get eligible resources ordered by priority', async () => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'high-priority',
        resources: [createParticipant({ id: 'priority-user' })],
        priority: 100,
      }));

      manager.registerPolicyPack(createPolicyPack({
        id: 'low-priority',
        resources: [createParticipant({ id: 'regular-user' })],
        priority: 1,
      }));

      const eligible = await manager.getEligibleResources('task-1', 'case-1');

      expect(eligible.length).toBe(2);
      expect(eligible[0].id).toBe('priority-user');
      expect(eligible[1].id).toBe('regular-user');
    });

    it('should filter eligible resources by type', async () => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'mixed',
        resources: [
          createParticipant({ id: 'person-1' }),
          createTool({ id: 'tool-1' }),
        ],
      }));

      const tools = await manager.getEligibleResources('task-1', 'case-1', {
        resourceType: ResourceType.TOOL,
      });

      expect(tools.length).toBe(1);
      expect(tools[0].type).toBe(ResourceType.TOOL);
    });
  });

  describe('Resource Availability', () => {
    it('should get availability for resource', () => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'avail-test',
        resources: [createParticipant({ id: 'avail-user' })],
      }));

      const availability = manager.getAvailability('avail-user');

      expect(availability.available).toBe(true);
      expect(availability.windows).toBeDefined();
      expect(availability.windows.length).toBeGreaterThan(0);
    });

    it('should set and retrieve availability windows', () => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'window-test',
        resources: [createParticipant({ id: 'window-user' })],
      }));

      const now = new Date();
      const later = new Date(now.getTime() + 3600000);

      manager.setAvailability('window-user', true, [
        {
          start: now.toISOString(),
          end: later.toISOString(),
          available: true,
        },
      ]);

      const availability = manager.getAvailability('window-user');
      expect(availability.available).toBe(true);
      expect(availability.windows).toBeDefined();
      expect(availability.windows.length).toBe(1);
      // Edge case fix: Verify window data is preserved
      // Note: RDF storage may trim trailing zeros from milliseconds, so compare timestamps as dates
      expect(new Date(availability.windows[0].start).getTime()).toBe(now.getTime());
      expect(new Date(availability.windows[0].end).getTime()).toBe(later.getTime());
      expect(availability.windows[0].available).toBe(true);
    });
  });

  describe('Resource Pools', () => {
    it('should create resource pool', () => {
      const pool = manager.createResourcePool({
        id: 'test-pool',
        name: 'Test Pool',
        resources: [
          createParticipant({ id: 'pool-user-1' }),
          createParticipant({ id: 'pool-user-2' }),
        ],
        allocationStrategy: 'round-robin',
      });

      expect(pool.id).toBe('test-pool');
      expect(pool.name).toBe('Test Pool');
      expect(pool.resources).toHaveLength(2);
    });

    it('should allocate from pool with round-robin', async () => {
      const pool = manager.createResourcePool({
        id: 'rr-pool',
        resources: [
          createParticipant({ id: 'rr-1', capacity: 1 }),
          createParticipant({ id: 'rr-2', capacity: 1 }),
        ],
        allocationStrategy: 'round-robin',
      });

      const receipt1 = await pool.allocateAny({
        id: 'wi-1', taskId: 't', caseId: 'c',
      });
      const receipt2 = await pool.allocateAny({
        id: 'wi-2', taskId: 't', caseId: 'c',
      });

      expect(receipt1.resourceId).not.toBe(receipt2.resourceId);
    });

    it('should return null when pool exhausted', async () => {
      const pool = manager.createResourcePool({
        id: 'limited-pool',
        resources: [
          createParticipant({ id: 'limited-1', capacity: 1 }),
        ],
      });

      await pool.allocateAny({ id: 'wi-1', taskId: 't', caseId: 'c' });
      const receipt = await pool.allocateAny({ id: 'wi-2', taskId: 't', caseId: 'c' });

      expect(receipt).toBeNull();
    });

    it('should get pool availability status', async () => {
      const pool = manager.createResourcePool({
        id: 'status-pool',
        resources: [
          createParticipant({ id: 'status-1', capacity: 1 }),
          createParticipant({ id: 'status-2', capacity: 1 }),
        ],
      });

      // Initially all available
      let status = pool.getAvailability();
      expect(status.available).toBe(true);
      expect(status.availableCount).toBe(2);

      // After one allocation
      await pool.allocateAny({ id: 'wi-1', taskId: 't', caseId: 'c' });
      status = pool.getAvailability();
      expect(status.availableCount).toBe(1);
    });

    it('should retrieve pool by ID', () => {
      manager.createResourcePool({
        id: 'retrievable',
        resources: [createParticipant({ id: 'r-1' })],
      });

      const pool = manager.getResourcePool('retrievable');
      expect(pool).toBeDefined();
      expect(pool.id).toBe('retrievable');
    });
  });

  describe('SPARQL Query', () => {
    it('should execute SPARQL query on store', () => {
      manager.registerPolicyPack(createPolicyPack({
        id: 'sparql-test',
        resources: [createParticipant({ id: 'sparql-user' })],
      }));

      // Should not throw
      const query = `
        PREFIX yawl: <${YAWL_NS}>
        SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10
      `;

      const results = manager.query(query);
      expect(results).toBeDefined();
    });
  });

  describe('Validation', () => {
    it('should validate resource schema', () => {
      expect(() => createParticipant({ id: '' })).toThrow();
      expect(() => createParticipant({ id: 'valid' })).not.toThrow();
    });

    it('should validate policy pack schema', () => {
      expect(() => createPolicyPack({ id: '', resources: [] })).toThrow();
      expect(() => createPolicyPack({ id: 'valid', resources: [] })).not.toThrow();
    });

    it('should validate work item in allocation', async () => {
      const resource = createParticipant({ id: 'valid-user' });
      manager.registerPolicyPack(createPolicyPack({
        id: 'validation',
        resources: [resource],
      }));

      await expect(
        manager.allocateResource({ id: '', taskId: 'x', caseId: 'y' }, resource)
      ).rejects.toThrow();
    });
  });
});

describe('ResourcePool', () => {
  it('should support priority allocation strategy', async () => {
    const manager = createResourceManager();
    const pool = manager.createResourcePool({
      id: 'priority-pool',
      resources: [
        createParticipant({ id: 'p1', capacity: 1 }),
        createParticipant({ id: 'p2', capacity: 1 }),
      ],
      allocationStrategy: 'priority',
    });

    // Priority should allocate in order
    const receipt = await pool.allocateAny({ id: 'wi-1', taskId: 't', caseId: 'c' });
    expect(receipt).toBeDefined();
  });

  it('should support random allocation strategy', async () => {
    const manager = createResourceManager();
    const pool = manager.createResourcePool({
      id: 'random-pool',
      resources: [
        createParticipant({ id: 'r1', capacity: 10 }),
        createParticipant({ id: 'r2', capacity: 10 }),
        createParticipant({ id: 'r3', capacity: 10 }),
      ],
      allocationStrategy: 'random',
    });

    // Should allocate (randomness is hard to test deterministically)
    const receipt = await pool.allocateAny({ id: 'wi-1', taskId: 't', caseId: 'c' });
    expect(receipt).toBeDefined();
  });
});
