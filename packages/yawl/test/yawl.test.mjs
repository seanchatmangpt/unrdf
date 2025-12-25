/**
 * @file YAWL Package Tests
 * @description Tests for YAWL ontology and store operations
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  // Ontology
  YAWL,
  YAWL_CASE,
  YAWL_TASK,
  YAWL_WORK,
  SPARQL_PREFIXES,
  WorkflowCase,
  WorkItem,
  Task,
  Case_Active,
  Case_Completed,
  WorkItem_Enabled,
  WorkItem_Completed,
  AtomicTask,
  XOR_Split,
  AND_Join,
  caseUri,
  taskUri,
  workItemUri,
  caseGraph,
  dateTimeLiteral,
  bindQuery,
  SPARQL_QUERIES,

  // Store
  createYawlStore,
  addCase,
  getCase,
  updateCaseStatus,
  addWorkItem,
  queryWorkItems,
  queryEnabledTasks,
  updateWorkItemStatus,
  addWorkflowSpec,
  getStoreStats,
  clearCase,
  exportCaseAsTurtle,
} from '../src/index.mjs';

describe('YAWL Ontology', () => {
  describe('Namespace definitions', () => {
    it('should define correct YAWL namespace', () => {
      expect(YAWL).toBe('http://unrdf.org/yawl#');
    });

    it('should define correct case namespace', () => {
      expect(YAWL_CASE).toBe('http://unrdf.org/yawl/case#');
    });

    it('should define correct task namespace', () => {
      expect(YAWL_TASK).toBe('http://unrdf.org/yawl/task#');
    });

    it('should define correct work item namespace', () => {
      expect(YAWL_WORK).toBe('http://unrdf.org/yawl/workitem#');
    });
  });

  describe('RDF Classes', () => {
    it('should define WorkflowCase class', () => {
      expect(WorkflowCase.value).toBe(YAWL + 'WorkflowCase');
    });

    it('should define WorkItem class', () => {
      expect(WorkItem.value).toBe(YAWL + 'WorkItem');
    });

    it('should define Task class', () => {
      expect(Task.value).toBe(YAWL + 'Task');
    });
  });

  describe('Status values', () => {
    it('should define case status values', () => {
      expect(Case_Active.value).toBe(YAWL + 'Case_Active');
      expect(Case_Completed.value).toBe(YAWL + 'Case_Completed');
    });

    it('should define work item status values', () => {
      expect(WorkItem_Enabled.value).toBe(YAWL + 'WorkItem_Enabled');
      expect(WorkItem_Completed.value).toBe(YAWL + 'WorkItem_Completed');
    });
  });

  describe('Split/Join behaviors', () => {
    it('should define split behaviors', () => {
      expect(XOR_Split.value).toBe(YAWL + 'XOR_Split');
    });

    it('should define join behaviors', () => {
      expect(AND_Join.value).toBe(YAWL + 'AND_Join');
    });
  });

  describe('URI factory functions', () => {
    it('should create case URI', () => {
      const uri = caseUri('case-123');
      expect(uri.value).toBe(YAWL_CASE + 'case-123');
    });

    it('should create task URI', () => {
      const uri = taskUri('task-1');
      expect(uri.value).toBe(YAWL_TASK + 'task-1');
    });

    it('should create work item URI', () => {
      const uri = workItemUri('item-1');
      expect(uri.value).toBe(YAWL_WORK + 'item-1');
    });

    it('should create case graph URI', () => {
      const uri = caseGraph('case-123');
      expect(uri.value).toBe(YAWL_CASE + 'case-123/graph');
    });
  });

  describe('Literal factory functions', () => {
    it('should create dateTime literal', () => {
      const date = new Date('2025-12-24T12:00:00Z');
      const lit = dateTimeLiteral(date);
      expect(lit.value).toBe('2025-12-24T12:00:00.000Z');
    });
  });

  describe('Query utilities', () => {
    it('should include SPARQL prefixes', () => {
      expect(SPARQL_PREFIXES).toContain('PREFIX yawl:');
      expect(SPARQL_PREFIXES).toContain('PREFIX yawl-case:');
    });

    it('should bind query placeholders', () => {
      const template = 'SELECT * WHERE { ?s ?p <CASE_URI> }';
      const result = bindQuery(template, { CASE_URI: 'http://example.org/case-1' });
      expect(result).toBe('SELECT * WHERE { ?s ?p <http://example.org/case-1> }');
    });

    it('should have predefined SPARQL queries', () => {
      expect(SPARQL_QUERIES.GET_CASE_WORK_ITEMS).toBeDefined();
      expect(SPARQL_QUERIES.GET_ENABLED_WORK_ITEMS).toBeDefined();
    });
  });
});

describe('YAWL Store', () => {
  let store;

  beforeEach(() => {
    store = createYawlStore();
  });

  describe('Store creation', () => {
    it('should create a YAWL store', () => {
      expect(store).toBeDefined();
      expect(typeof store.add).toBe('function');
      expect(typeof store.query).toBe('function');
    });

    it('should have ontology loaded', () => {
      const stats = getStoreStats(store);
      expect(stats.totalQuads).toBeGreaterThan(0);
    });
  });

  describe('Case operations', () => {
    it('should add a case', () => {
      const uri = addCase(store, {
        id: 'case-123',
        specId: 'expense-approval',
        status: 'active',
        createdAt: new Date('2025-12-24T10:00:00Z'),
      });

      expect(uri.value).toBe(YAWL_CASE + 'case-123');
    });

    it('should get a case', () => {
      addCase(store, {
        id: 'case-456',
        specId: 'leave-request',
        status: 'active',
      });

      const caseData = getCase(store, 'case-456');

      expect(caseData).not.toBeNull();
      expect(caseData.id).toBe('case-456');
      expect(caseData.specId).toBe('leave-request');
      expect(caseData.status).toBe('active');
    });

    it('should return null for non-existent case', () => {
      const caseData = getCase(store, 'non-existent');
      expect(caseData).toBeNull();
    });

    it('should update case status', () => {
      addCase(store, {
        id: 'case-789',
        specId: 'test-spec',
        status: 'active',
      });

      const result = updateCaseStatus(store, 'case-789', 'completed');
      expect(result).toBe(true);

      const caseData = getCase(store, 'case-789');
      expect(caseData.status).toBe('completed');
    });
  });

  describe('Work item operations', () => {
    beforeEach(() => {
      addCase(store, {
        id: 'case-100',
        specId: 'test-workflow',
        status: 'active',
      });
    });

    it('should add a work item', () => {
      const uri = addWorkItem(store, {
        id: 'work-1',
        taskId: 'task-submit',
        caseId: 'case-100',
        status: 'enabled',
      });

      expect(uri.value).toBe(YAWL_WORK + 'work-1');
    });

    it('should query work items for a case', () => {
      addWorkItem(store, {
        id: 'work-2',
        taskId: 'task-review',
        caseId: 'case-100',
        status: 'enabled',
      });

      addWorkItem(store, {
        id: 'work-3',
        taskId: 'task-approve',
        caseId: 'case-100',
        status: 'started',
      });

      const workItems = queryWorkItems(store, 'case-100');

      expect(workItems.length).toBe(2);
    });

    it('should query enabled tasks', () => {
      addWorkItem(store, {
        id: 'work-4',
        taskId: 'task-a',
        caseId: 'case-100',
        status: 'enabled',
      });

      addWorkItem(store, {
        id: 'work-5',
        taskId: 'task-b',
        caseId: 'case-100',
        status: 'completed',
      });

      const enabled = queryEnabledTasks(store, 'case-100');

      expect(enabled.length).toBe(1);
      expect(enabled[0].status).toBe('enabled');
    });

    it('should update work item status', () => {
      addWorkItem(store, {
        id: 'work-6',
        taskId: 'task-x',
        caseId: 'case-100',
        status: 'enabled',
      });

      const result = updateWorkItemStatus(store, 'work-6', 'case-100', 'completed', {
        completedBy: 'http://example.org/user/alice',
      });

      expect(result).toBe(true);

      const workItems = queryWorkItems(store, 'case-100');
      const item = workItems.find(w => w.id.includes('work-6'));
      expect(item.status).toBe('completed');
    });
  });

  describe('Workflow specification operations', () => {
    it('should add a workflow specification', () => {
      const uri = addWorkflowSpec(store, {
        id: 'expense-workflow',
        name: 'Expense Approval Process',
        tasks: [
          {
            id: 'submit',
            name: 'Submit Expense',
            kind: 'manual',
            joinsTo: ['review'],
          },
          {
            id: 'review',
            name: 'Review Expense',
            kind: 'manual',
            joinBehavior: 'xor',
            splitBehavior: 'xor',
            joinsTo: ['approve', 'reject'],
          },
          {
            id: 'approve',
            name: 'Approve Expense',
            kind: 'automated',
          },
          {
            id: 'reject',
            name: 'Reject Expense',
            kind: 'automated',
          },
        ],
      });

      expect(uri.value).toBe(YAWL + 'spec-expense-workflow');
    });
  });

  describe('Utility operations', () => {
    it('should get store statistics', () => {
      const stats = getStoreStats(store);

      expect(stats.totalQuads).toBeGreaterThanOrEqual(0);
      expect(stats.timestamp).toBeDefined();
    });

    it('should clear case data', () => {
      addCase(store, {
        id: 'case-to-clear',
        specId: 'test',
        status: 'active',
      });

      addWorkItem(store, {
        id: 'work-to-clear',
        taskId: 'task-1',
        caseId: 'case-to-clear',
        status: 'enabled',
      });

      const removed = clearCase(store, 'case-to-clear');
      expect(removed).toBeGreaterThan(0);

      const caseData = getCase(store, 'case-to-clear');
      expect(caseData).toBeNull();
    });

    it('should export case as Turtle', () => {
      addCase(store, {
        id: 'export-case',
        specId: 'test-export',
        status: 'active',
      });

      const turtle = exportCaseAsTurtle(store, 'export-case');

      expect(turtle).toContain('@prefix yawl:');
      expect(turtle).toContain('WorkflowCase');
    });
  });
});

describe('YAWL RDF Structure Examples', () => {
  let store;

  beforeEach(() => {
    store = createYawlStore();
  });

  it('should create proper Case RDF structure', () => {
    // Create case as specified in requirements
    addCase(store, {
      id: 'case-123',
      specId: 'workflow-spec-1',
      status: 'active',
      createdAt: new Date('2025-12-24T10:00:00Z'),
    });

    // Verify RDF structure
    const caseData = getCase(store, 'case-123');

    expect(caseData.id).toBe('case-123');
    expect(caseData.specId).toBe('workflow-spec-1');
    expect(caseData.status).toBe('active');
    expect(caseData.createdAt).toContain('2025-12-24');
  });

  it('should create proper Task RDF structure', () => {
    // Create workflow spec with tasks as specified
    addWorkflowSpec(store, {
      id: 'spec-1',
      name: 'Test Workflow',
      tasks: [
        {
          id: 'task-1',
          name: 'Approve Expense',
          kind: 'atomic',
          joinsTo: ['task-2', 'task-3'],
        },
        {
          id: 'task-2',
          name: 'Process Payment',
          kind: 'automated',
        },
        {
          id: 'task-3',
          name: 'Notify User',
          kind: 'automated',
        },
      ],
    });

    // Verify by querying store
    const stats = getStoreStats(store);
    expect(stats.totalQuads).toBeGreaterThan(5);
  });

  it('should create proper WorkItem RDF structure', () => {
    addCase(store, {
      id: 'case-123',
      specId: 'workflow-spec-1',
      status: 'active',
    });

    // Create work item as specified
    addWorkItem(store, {
      id: 'item-1',
      taskId: 'task-1',
      caseId: 'case-123',
      status: 'enabled',
      owner: 'http://xmlns.com/foaf/0.1/person-alice',
    });

    // Verify RDF structure
    const workItems = queryWorkItems(store, 'case-123');
    expect(workItems.length).toBe(1);

    const item = workItems[0];
    expect(item.status).toBe('enabled');
    expect(item.owner).toBe('http://xmlns.com/foaf/0.1/person-alice');
  });

  it('should support named graphs for case isolation', () => {
    // Create two separate cases
    addCase(store, {
      id: 'case-A',
      specId: 'spec-1',
      status: 'active',
    });

    addCase(store, {
      id: 'case-B',
      specId: 'spec-1',
      status: 'active',
    });

    addWorkItem(store, {
      id: 'work-A1',
      taskId: 'task-1',
      caseId: 'case-A',
      status: 'enabled',
    });

    addWorkItem(store, {
      id: 'work-B1',
      taskId: 'task-1',
      caseId: 'case-B',
      status: 'completed',
    });

    // Verify isolation - each case only sees its own work items
    const workItemsA = queryWorkItems(store, 'case-A');
    const workItemsB = queryWorkItems(store, 'case-B');

    expect(workItemsA.length).toBe(1);
    expect(workItemsB.length).toBe(1);

    expect(workItemsA[0].status).toBe('enabled');
    expect(workItemsB[0].status).toBe('completed');
  });
});
