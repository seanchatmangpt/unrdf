/**
 * @file YAWL API Server Tests
 * @module @unrdf/yawl-api/test/api
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createYAWLAPIServer } from '../src/server.mjs';
import { createWorkflowEngine, SPLIT_TYPE, JOIN_TYPE } from '@unrdf/yawl';

describe('YAWLAPIServer', () => {
  let server;
  let engine;

  // Simple test workflow
  const testWorkflow = {
    id: 'test-workflow',
    name: 'Test Workflow',
    version: '1.0.0',
    tasks: [
      {
        id: 'start-task',
        name: 'Start Task',
        splitType: SPLIT_TYPE.AND,
        joinType: JOIN_TYPE.XOR,
      },
      {
        id: 'middle-task',
        name: 'Middle Task',
        splitType: SPLIT_TYPE.AND,
        joinType: JOIN_TYPE.XOR,
      },
      {
        id: 'end-task',
        name: 'End Task',
        splitType: SPLIT_TYPE.AND,
        joinType: JOIN_TYPE.XOR,
      },
    ],
    flows: [
      { from: 'start-task', to: 'middle-task' },
      { from: 'middle-task', to: 'end-task' },
    ],
    startTaskId: 'start-task',
    endTaskIds: ['end-task'],
  };

  beforeEach(async () => {
    engine = createWorkflowEngine();
    engine.registerWorkflow(testWorkflow);

    server = await createYAWLAPIServer({
      engine,
      baseUrl: 'http://localhost:3000',
      fastifyOptions: { logger: false },
    });
  });

  afterEach(async () => {
    if (server) {
      await server.close();
    }
  });

  describe('Health Check', () => {
    it('should return health status', async () => {
      const response = await server.getServer().inject({
        method: 'GET',
        url: '/health',
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.status).toBe('ok');
      expect(data.engine).toBeDefined();
      expect(data.engine.workflows).toBe(1);
      expect(data.timestamp).toBeDefined();
    });
  });

  describe('Workflow Endpoints', () => {
    it('should register a new workflow', async () => {
      const newWorkflow = {
        id: 'new-workflow',
        name: 'New Test Workflow',
        version: '1.0.0',
        tasks: [
          {
            id: 'task1',
            name: 'Task 1',
            splitType: SPLIT_TYPE.AND,
            joinType: JOIN_TYPE.XOR,
          },
        ],
        flows: [],
        startTaskId: 'task1',
        endTaskIds: ['task1'],
      };

      const response = await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows',
        payload: newWorkflow,
      });

      expect(response.statusCode).toBe(201);
      const data = JSON.parse(response.payload);
      expect(data.workflowId).toBe('new-workflow');
      expect(data.message).toContain('registered');
    });

    it('should list all workflows', async () => {
      const response = await server.getServer().inject({
        method: 'GET',
        url: '/api/workflows',
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.workflows).toHaveLength(1);
      expect(data.workflows[0].id).toBe('test-workflow');
      expect(data.workflows[0].taskCount).toBe(3);
    });

    it('should get workflow details', async () => {
      const response = await server.getServer().inject({
        method: 'GET',
        url: '/api/workflows/test-workflow',
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.id).toBe('test-workflow');
      expect(data.name).toBe('Test Workflow');
      expect(data.tasks).toHaveLength(3);
      expect(data.startTaskId).toBe('start-task');
    });

    it('should return 404 for non-existent workflow', async () => {
      const response = await server.getServer().inject({
        method: 'GET',
        url: '/api/workflows/non-existent',
      });

      expect(response.statusCode).toBe(404);
      const data = JSON.parse(response.payload);
      expect(data.error).toContain('not found');
    });
  });

  describe('Case Management Endpoints', () => {
    it('should create a new case', async () => {
      const response = await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: {
          initialData: { testField: 'testValue' },
        },
      });

      expect(response.statusCode).toBe(201);
      const data = JSON.parse(response.payload);
      expect(data.case).toBeDefined();
      expect(data.case.id).toBeDefined();
      expect(data.case.workflowId).toBe('test-workflow');
      expect(data.case.status).toBe('running');
      expect(data.case.data.testField).toBe('testValue');
      expect(data.receipt).toBeDefined();

      // Check HATEOAS links
      expect(data.case._links).toBeDefined();
      expect(data.case._links.self).toBeDefined();
      expect(data.case._links.workflow).toBeDefined();
      expect(data.case._links.enabledTasks).toBeDefined();
      expect(data.case._links.enabledTasks.length).toBeGreaterThan(0);
    });

    it('should list all cases', async () => {
      // Create a case first
      await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: { initialData: {} },
      });

      const response = await server.getServer().inject({
        method: 'GET',
        url: '/api/cases',
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.cases).toHaveLength(1);
      expect(data.cases[0].workflowId).toBe('test-workflow');
    });

    it('should filter cases by workflowId', async () => {
      // Create case
      await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: { initialData: {} },
      });

      const response = await server.getServer().inject({
        method: 'GET',
        url: '/api/cases?workflowId=test-workflow',
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.cases).toHaveLength(1);
    });

    it('should get case details with HATEOAS links', async () => {
      // Create case
      const createResponse = await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: { initialData: {} },
      });

      const createData = JSON.parse(createResponse.payload);
      const caseId = createData.case.id;

      // Get case details
      const response = await server.getServer().inject({
        method: 'GET',
        url: `/api/cases/${caseId}`,
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.id).toBe(caseId);
      expect(data._links).toBeDefined();
      expect(data._links.enabledTasks).toBeDefined();
    });

    it('should return 404 for non-existent case', async () => {
      const response = await server.getServer().inject({
        method: 'GET',
        url: '/api/cases/non-existent-case',
      });

      expect(response.statusCode).toBe(404);
    });
  });

  describe('Task Execution Endpoints', () => {
    let caseId;
    let startTaskWorkItemId;

    beforeEach(async () => {
      // Create a case for each test
      const createResponse = await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: { initialData: {} },
      });

      const createData = JSON.parse(createResponse.payload);
      caseId = createData.case.id;

      // Get the enabled work item
      const caseData = createData.case;
      startTaskWorkItemId = caseData._links.enabledTasks[0].workItemId;
    });

    it('should start a task', async () => {
      const response = await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${startTaskWorkItemId}/start`,
        payload: {
          actor: 'test-user',
        },
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.task).toBeDefined();
      expect(data.task.status).toBe('running');
      expect(data.task.startedAt).toBeDefined();
      expect(data.receipt).toBeDefined();
    });

    it('should complete a task and enable downstream', async () => {
      // First start the task
      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${startTaskWorkItemId}/start`,
        payload: { actor: 'test-user' },
      });

      // Then complete it
      const response = await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${startTaskWorkItemId}/complete`,
        payload: {
          output: { result: 'success' },
          actor: 'test-user',
        },
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.task).toBeDefined();
      expect(data.task.status).toBe('completed');
      expect(data.task.completedAt).toBeDefined();
      expect(data.downstreamEnabled).toBeDefined();
      expect(data.downstreamEnabled.length).toBeGreaterThan(0);
      expect(data.receipt).toBeDefined();
    });

    it('should cancel a task', async () => {
      const response = await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${startTaskWorkItemId}/cancel`,
        payload: {
          reason: 'Test cancellation',
          actor: 'test-user',
        },
      });

      expect(response.statusCode).toBe(200);
      const data = JSON.parse(response.payload);
      expect(data.task).toBeDefined();
      expect(data.task.status).toBe('cancelled');
      expect(data.receipt).toBeDefined();
    });
  });

  describe('HATEOAS Link Generation', () => {
    it('should include correct HATEOAS links for enabled tasks', async () => {
      const createResponse = await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: { initialData: {} },
      });

      const data = JSON.parse(createResponse.payload);
      const links = data.case._links;

      expect(links.self).toBeDefined();
      expect(links.self.href).toContain('/api/cases/');
      expect(links.workflow).toBeDefined();
      expect(links.enabledTasks).toBeDefined();
      expect(links.enabledTasks[0].actions.start).toBeDefined();
      expect(links.enabledTasks[0].actions.cancel).toBeDefined();
    });

    it('should include running task links after starting', async () => {
      // Create case
      const createResponse = await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: { initialData: {} },
      });

      const createData = JSON.parse(createResponse.payload);
      const caseId = createData.case.id;
      const workItemId = createData.case._links.enabledTasks[0].workItemId;

      // Start task
      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${workItemId}/start`,
        payload: { actor: 'test-user' },
      });

      // Get case details
      const response = await server.getServer().inject({
        method: 'GET',
        url: `/api/cases/${caseId}`,
      });

      const data = JSON.parse(response.payload);
      expect(data._links.runningTasks).toBeDefined();
      expect(data._links.runningTasks[0].actions.complete).toBeDefined();
      expect(data._links.runningTasks[0].actions.cancel).toBeDefined();
    });
  });

  describe('OpenAPI Documentation', () => {
    it('should generate OpenAPI spec', async () => {
      const response = await server.getServer().inject({
        method: 'GET',
        url: '/docs/json',
      });

      expect(response.statusCode).toBe(200);
      const spec = JSON.parse(response.payload);
      expect(spec.openapi).toBeDefined();
      expect(spec.info).toBeDefined();
      expect(spec.info.title).toContain('YAWL');
      expect(spec.paths).toBeDefined();
      expect(spec.paths['/api/workflows']).toBeDefined();
      expect(spec.paths['/api/cases']).toBeDefined();
    });
  });

  describe('Complete Workflow Execution via API', () => {
    it('should execute complete workflow through REST API', async () => {
      // 1. Create case
      const createResponse = await server.getServer().inject({
        method: 'POST',
        url: '/api/workflows/test-workflow/cases',
        payload: { initialData: { test: 'data' } },
      });

      expect(createResponse.statusCode).toBe(201);
      const createData = JSON.parse(createResponse.payload);
      const caseId = createData.case.id;

      // 2. Get first enabled task
      let caseResponse = await server.getServer().inject({
        method: 'GET',
        url: `/api/cases/${caseId}`,
      });

      let caseData = JSON.parse(caseResponse.payload);
      let workItemId = caseData._links.enabledTasks[0].workItemId;

      // 3. Start and complete first task
      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${workItemId}/start`,
      });

      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${workItemId}/complete`,
        payload: { output: { step1: 'done' } },
      });

      // 4. Get next enabled task
      caseResponse = await server.getServer().inject({
        method: 'GET',
        url: `/api/cases/${caseId}`,
      });

      caseData = JSON.parse(caseResponse.payload);
      workItemId = caseData._links.enabledTasks[0].workItemId;

      // 5. Start and complete second task
      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${workItemId}/start`,
      });

      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${workItemId}/complete`,
        payload: { output: { step2: 'done' } },
      });

      // 6. Get next enabled task (final)
      caseResponse = await server.getServer().inject({
        method: 'GET',
        url: `/api/cases/${caseId}`,
      });

      caseData = JSON.parse(caseResponse.payload);
      workItemId = caseData._links.enabledTasks[0].workItemId;

      // 7. Complete final task
      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${workItemId}/start`,
      });

      await server.getServer().inject({
        method: 'POST',
        url: `/api/cases/${caseId}/tasks/${workItemId}/complete`,
        payload: { output: { step3: 'done' } },
      });

      // 8. Verify case completed
      caseResponse = await server.getServer().inject({
        method: 'GET',
        url: `/api/cases/${caseId}`,
      });

      caseData = JSON.parse(caseResponse.payload);
      expect(caseData.status).toBe('completed');
      expect(caseData.completedAt).toBeDefined();
    });
  });
});
