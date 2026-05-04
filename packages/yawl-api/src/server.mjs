/**
 * @file YAWL API Server - High-performance REST API framework for YAWL workflows
 * @module @unrdf/yawl-api/server
 *
 * @description
 * Auto-generates RESTful APIs from YAWL workflow definitions with:
 * - OpenAPI 3.1 documentation generation
 * - Zod-based request/response validation
 * - HATEOAS hypermedia links based on workflow state
 * - Swagger UI for interactive API exploration
 * - Full YAWL workflow lifecycle management via REST
 *
 * @example
 * import { createYAWLAPIServer } from '@unrdf/yawl-api';
 * import { createWorkflowEngine } from '@unrdf/yawl';
 *
 * const engine = createWorkflowEngine();
 * const server = await createYAWLAPIServer({ engine });
 * await server.listen({ port: 3000 });
 */

import Fastify from 'fastify';
import fastifySwagger from '@fastify/swagger';
import fastifySwaggerUI from '@fastify/swagger-ui';
import fastifyCors from '@fastify/cors';
import { z } from 'zod';
import zodToJsonSchema from 'zod-to-json-schema';
import { createWorkflowEngine } from '@unrdf/yawl';
import { toISO, now } from '@unrdf/kgc-4d';

// =============================================================================
// Zod Schemas for API Request/Response Validation
// =============================================================================

const CreateCaseRequestSchema = z.object({
  workflowId: z.string().describe('Workflow specification ID'),
  initialData: z.record(z.any()).optional().describe('Initial case data'),
  caseId: z.string().optional().describe('Custom case ID (auto-generated if not provided)'),
});

const TaskActionRequestSchema = z.object({
  actor: z.string().optional().describe('Actor performing the action'),
  resourceId: z.string().optional().describe('Resource to allocate'),
  output: z.record(z.any()).optional().describe('Task output data'),
});

const CompleteTaskRequestSchema = z.object({
  actor: z.string().optional().describe('Actor completing the task'),
  output: z.record(z.any()).optional().describe('Task output data'),
});

const CancelTaskRequestSchema = z.object({
  reason: z.string().optional().describe('Cancellation reason'),
  actor: z.string().optional().describe('Actor canceling the task'),
});

const WorkflowDefinitionSchema = z.object({
  id: z.string().describe('Unique workflow identifier'),
  name: z.string().optional().describe('Human-readable name'),
  version: z.string().optional().default('1.0.0').describe('Semantic version'),
  tasks: z.array(z.record(z.any())).describe('Task definitions'),
  flows: z.array(z.record(z.any())).optional().describe('Flow definitions'),
});

// =============================================================================
// HATEOAS Link Generator
// =============================================================================

/**
 * Generate hypermedia links based on case state and enabled tasks
 *
 * @param {Object} caseInstance - YAWL case instance
 * @param {string} baseUrl - Base URL for links
 * @returns {Object} HATEOAS links
 */
function generateHATEOASLinks(caseInstance, baseUrl = '') {
  const links = {
    self: {
      href: `${baseUrl}/api/cases/${caseInstance.id}`,
      method: 'GET',
      description: 'Get case details',
    },
    workflow: {
      href: `${baseUrl}/api/workflows/${caseInstance.workflowId}`,
      method: 'GET',
      description: 'Get workflow definition',
    },
  };

  // Get enabled tasks and add action links
  const enabledTasks = caseInstance.getEnabledWorkItems();

  if (enabledTasks.length > 0) {
    links.enabledTasks = enabledTasks.map(task => ({
      taskId: task.taskId,
      workItemId: task.id,
      name: task.name,
      actions: {
        start: {
          href: `${baseUrl}/api/cases/${caseInstance.id}/tasks/${task.id}/start`,
          method: 'POST',
          description: `Start task: ${task.name}`,
        },
        cancel: {
          href: `${baseUrl}/api/cases/${caseInstance.id}/tasks/${task.id}/cancel`,
          method: 'POST',
          description: `Cancel task: ${task.name}`,
        },
      },
    }));
  }

  // Get running tasks and add completion links
  const runningTasks = caseInstance.getRunningWorkItems();

  if (runningTasks.length > 0) {
    links.runningTasks = runningTasks.map(task => ({
      taskId: task.taskId,
      workItemId: task.id,
      name: task.name,
      startedAt: task.startedAt ? toISO(task.startedAt) : null,
      assignedResource: task.assignedResource,
      actions: {
        complete: {
          href: `${baseUrl}/api/cases/${caseInstance.id}/tasks/${task.id}/complete`,
          method: 'POST',
          description: `Complete task: ${task.name}`,
        },
        cancel: {
          href: `${baseUrl}/api/cases/${caseInstance.id}/tasks/${task.id}/cancel`,
          method: 'POST',
          description: `Cancel task: ${task.name}`,
        },
      },
    }));
  }

  return links;
}

/**
 * Serialize case for API response
 *
 * @param {Object} caseInstance - YAWL case instance
 * @param {string} baseUrl - Base URL for HATEOAS links
 * @returns {Object} API response object
 */
function serializeCaseForAPI(caseInstance, baseUrl = '') {
  return {
    id: caseInstance.id,
    workflowId: caseInstance.workflowId,
    status: caseInstance.status,
    data: caseInstance.data,
    createdAt: caseInstance.createdAt ? toISO(caseInstance.createdAt) : null,
    startedAt: caseInstance.startedAt ? toISO(caseInstance.startedAt) : null,
    completedAt: caseInstance.completedAt ? toISO(caseInstance.completedAt) : null,
    workItems: Array.from(caseInstance.workItems.values()).map(wi => ({
      id: wi.id,
      taskId: wi.taskId,
      name: wi.name,
      status: wi.status,
      enabledAt: wi.enabledAt ? toISO(wi.enabledAt) : null,
      startedAt: wi.startedAt ? toISO(wi.startedAt) : null,
      completedAt: wi.completedAt ? toISO(wi.completedAt) : null,
      assignedResource: wi.assignedResource,
    })),
    _links: generateHATEOASLinks(caseInstance, baseUrl),
  };
}

// =============================================================================
// YAWLAPIServer Class
// =============================================================================

/**
 * YAWL API Server - Auto-generates REST API from YAWL workflows
 *
 * @class
 */
export class YAWLAPIServer {
  /**
   * @param {Object} options - Server options
   * @param {Object} [options.engine] - YAWL workflow engine instance
   * @param {Object} [options.fastifyOptions={}] - Fastify server options
   * @param {string} [options.baseUrl=''] - Base URL for HATEOAS links
   * @param {boolean} [options.enableSwagger=true] - Enable Swagger UI
   * @param {Object} [options.swaggerOptions={}] - Swagger configuration
   */
  constructor(options = {}) {
    const {
      engine = createWorkflowEngine(),
      fastifyOptions = {},
      baseUrl = '',
      enableSwagger = true,
      swaggerOptions = {},
    } = options;

    this.engine = engine;
    this.baseUrl = baseUrl;
    this.fastify = Fastify({
      logger: true,
      ...fastifyOptions,
    });

    // Register plugins
    this._registerPlugins(enableSwagger, swaggerOptions);

    // Register routes
    this._registerRoutes();
  }

  /**
   * Register Fastify plugins
   * @private
   */
  async _registerPlugins(enableSwagger, swaggerOptions) {
    // CORS support
    await this.fastify.register(fastifyCors, {
      origin: true,
    });

    if (enableSwagger) {
      // OpenAPI/Swagger documentation
      await this.fastify.register(fastifySwagger, {
        openapi: {
          info: {
            title: 'YAWL Workflow API',
            description: 'RESTful API for YAWL workflow management with HATEOAS hypermedia controls',
            version: '1.0.0',
          },
          tags: [
            { name: 'workflows', description: 'Workflow definition endpoints' },
            { name: 'cases', description: 'Case management endpoints' },
            { name: 'tasks', description: 'Task execution endpoints' },
          ],
          ...swaggerOptions.openapi,
        },
        ...swaggerOptions,
      });

      // Swagger UI
      await this.fastify.register(fastifySwaggerUI, {
        routePrefix: '/docs',
        uiConfig: {
          docExpansion: 'list',
          deepLinking: true,
        },
        staticCSP: true,
        transformStaticCSP: (header) => header,
        ...swaggerOptions.ui,
      });
    }
  }

  /**
   * Register API routes
   * @private
   */
  _registerRoutes() {
    // Health check
    this.fastify.get('/health', {
      schema: {
        description: 'Health check endpoint',
        tags: ['health'],
        response: {
          200: zodToJsonSchema(z.object({
            status: z.string(),
            timestamp: z.string(),
            engine: z.object({
              workflows: z.number(),
              cases: z.number(),
              health: z.string(),
            }),
          })),
        },
      },
    }, async (request, reply) => {
      const health = this.engine.getHealth();
      return {
        status: 'ok',
        timestamp: toISO(now()),
        engine: {
          workflows: this.engine.workflows.size,
          cases: this.engine.cases.size,
          health: health.status,
        },
      };
    });

    // =========================================================================
    // Workflow Definition Endpoints
    // =========================================================================

    // Register workflow
    this.fastify.post('/api/workflows', {
      schema: {
        description: 'Register a new workflow definition',
        tags: ['workflows'],
        body: zodToJsonSchema(WorkflowDefinitionSchema),
        response: {
          201: zodToJsonSchema(z.object({
            workflowId: z.string(),
            message: z.string(),
          })),
        },
      },
    }, async (request, reply) => {
      const workflowSpec = request.body;
      this.engine.registerWorkflow(workflowSpec);
      reply.code(201);
      return {
        workflowId: workflowSpec.id,
        message: 'Workflow registered successfully',
      };
    });

    // List workflows
    this.fastify.get('/api/workflows', {
      schema: {
        description: 'List all registered workflows',
        tags: ['workflows'],
        response: {
          200: zodToJsonSchema(z.object({
            workflows: z.array(z.object({
              id: z.string(),
              name: z.string().optional(),
              version: z.string(),
              taskCount: z.number(),
            })),
          })),
        },
      },
    }, async (request, reply) => {
      const workflows = Array.from(this.engine.workflows.values()).map(wf => ({
        id: wf.id,
        name: wf.name,
        version: wf.version,
        taskCount: wf.tasks.length,
      }));
      return { workflows };
    });

    // Get workflow details
    this.fastify.get('/api/workflows/:workflowId', {
      schema: {
        description: 'Get workflow definition details',
        tags: ['workflows'],
        params: zodToJsonSchema(z.object({
          workflowId: z.string(),
        })),
      },
    }, async (request, reply) => {
      const { workflowId } = request.params;
      const workflow = this.engine.workflows.get(workflowId);

      if (!workflow) {
        reply.code(404);
        return { error: 'Workflow not found' };
      }

      return {
        id: workflow.id,
        name: workflow.name,
        version: workflow.version,
        tasks: workflow.tasks,
        flows: workflow.flows,
        startTaskId: workflow.startTaskId,
        endTaskIds: workflow.endTaskIds,
      };
    });

    // =========================================================================
    // Case Management Endpoints
    // =========================================================================

    // Create case
    this.fastify.post('/api/workflows/:workflowId/cases', {
      schema: {
        description: 'Create a new workflow case instance',
        tags: ['cases'],
        params: zodToJsonSchema(z.object({
          workflowId: z.string(),
        })),
        body: zodToJsonSchema(z.object({
          initialData: z.record(z.any()).optional(),
          caseId: z.string().optional(),
        })),
        response: {
          201: zodToJsonSchema(z.object({
            case: z.any(),
            receipt: z.any(),
          })),
        },
      },
    }, async (request, reply) => {
      const { workflowId } = request.params;
      const { initialData = {}, caseId } = request.body || {};

      const result = await this.engine.createCase(workflowId, initialData, { caseId });

      reply.code(201);
      return {
        case: serializeCaseForAPI(result.case, this.baseUrl),
        receipt: result.receipt,
      };
    });

    // List cases
    this.fastify.get('/api/cases', {
      schema: {
        description: 'List all workflow cases',
        tags: ['cases'],
        querystring: zodToJsonSchema(z.object({
          workflowId: z.string().optional(),
          status: z.string().optional(),
        })),
      },
    }, async (request, reply) => {
      const { workflowId, status } = request.query || {};

      let cases = Array.from(this.engine.cases.values());

      if (workflowId) {
        cases = cases.filter(c => c.workflowId === workflowId);
      }

      if (status) {
        cases = cases.filter(c => c.status === status);
      }

      return {
        cases: cases.map(c => serializeCaseForAPI(c, this.baseUrl)),
      };
    });

    // Get case details
    this.fastify.get('/api/cases/:caseId', {
      schema: {
        description: 'Get case details with HATEOAS links',
        tags: ['cases'],
        params: zodToJsonSchema(z.object({
          caseId: z.string(),
        })),
      },
    }, async (request, reply) => {
      const { caseId } = request.params;
      const caseInstance = this.engine.cases.get(caseId);

      if (!caseInstance) {
        reply.code(404);
        return { error: 'Case not found' };
      }

      return serializeCaseForAPI(caseInstance, this.baseUrl);
    });

    // =========================================================================
    // Task Execution Endpoints
    // =========================================================================

    // Start task
    this.fastify.post('/api/cases/:caseId/tasks/:workItemId/start', {
      schema: {
        description: 'Start a work item',
        tags: ['tasks'],
        params: zodToJsonSchema(z.object({
          caseId: z.string(),
          workItemId: z.string(),
        })),
        body: zodToJsonSchema(TaskActionRequestSchema),
        response: {
          200: zodToJsonSchema(z.object({
            task: z.any(),
            receipt: z.any(),
            resource: z.any().optional(),
          })),
        },
      },
    }, async (request, reply) => {
      const { caseId, workItemId } = request.params;
      const { actor, resourceId } = request.body || {};

      const result = await this.engine.startTask(caseId, workItemId, { actor, resourceId });

      return {
        task: {
          id: result.task.id,
          taskId: result.task.taskId,
          status: result.task.status,
          startedAt: result.task.startedAt ? toISO(result.task.startedAt) : null,
          assignedResource: result.task.assignedResource,
        },
        receipt: result.receipt,
        resource: result.resource,
      };
    });

    // Complete task
    this.fastify.post('/api/cases/:caseId/tasks/:workItemId/complete', {
      schema: {
        description: 'Complete a work item',
        tags: ['tasks'],
        params: zodToJsonSchema(z.object({
          caseId: z.string(),
          workItemId: z.string(),
        })),
        body: zodToJsonSchema(CompleteTaskRequestSchema),
        response: {
          200: zodToJsonSchema(z.object({
            task: z.any(),
            receipt: z.any(),
            downstreamEnabled: z.array(z.any()),
          })),
        },
      },
    }, async (request, reply) => {
      const { caseId, workItemId } = request.params;
      const { output = {}, actor } = request.body || {};

      const result = await this.engine.completeTask(caseId, workItemId, output, actor);

      return {
        task: {
          id: result.task.id,
          taskId: result.task.taskId,
          status: result.task.status,
          completedAt: result.task.completedAt ? toISO(result.task.completedAt) : null,
        },
        receipt: result.receipt,
        downstreamEnabled: result.downstreamEnabled.map(d => ({
          taskId: d.taskId,
          workItemId: d.workItemId,
        })),
      };
    });

    // Cancel task
    this.fastify.post('/api/cases/:caseId/tasks/:workItemId/cancel', {
      schema: {
        description: 'Cancel a work item',
        tags: ['tasks'],
        params: zodToJsonSchema(z.object({
          caseId: z.string(),
          workItemId: z.string(),
        })),
        body: zodToJsonSchema(CancelTaskRequestSchema),
      },
    }, async (request, reply) => {
      const { caseId, workItemId } = request.params;
      const { reason, actor } = request.body || {};

      const result = await this.engine.cancelTask(caseId, workItemId, reason, actor);

      return {
        task: {
          id: result.task.id,
          taskId: result.task.taskId,
          status: result.task.status,
        },
        receipt: result.receipt,
      };
    });
  }

  /**
   * Start the server
   *
   * @param {Object} options - Listen options
   * @param {number} [options.port=3000] - Port to listen on
   * @param {string} [options.host='0.0.0.0'] - Host to bind to
   * @returns {Promise<string>} Server address
   */
  async listen(options = {}) {
    const { port = 3000, host = '0.0.0.0' } = options;

    try {
      const address = await this.fastify.listen({ port, host });
      this.fastify.log.info(`YAWL API Server listening at ${address}`);
      this.fastify.log.info(`Swagger UI available at ${address}/docs`);
      return address;
    } catch (err) {
      this.fastify.log.error(err);
      throw err;
    }
  }

  /**
   * Stop the server
   * @returns {Promise<void>}
   */
  async close() {
    await this.fastify.close();
  }

  /**
   * Get the underlying Fastify instance
   * @returns {Object} Fastify instance
   */
  getServer() {
    return this.fastify;
  }

  /**
   * Get the YAWL engine instance
   * @returns {Object} YAWL engine
   */
  getEngine() {
    return this.engine;
  }
}

// =============================================================================
// Factory Function
// =============================================================================

/**
 * Create a YAWL API server instance
 *
 * @param {Object} options - Server options
 * @returns {Promise<YAWLAPIServer>} Configured API server
 *
 * @example
 * import { createYAWLAPIServer } from '@unrdf/yawl-api';
 *
 * const server = await createYAWLAPIServer({
 *   baseUrl: 'http://localhost:3000',
 *   enableSwagger: true,
 * });
 *
 * await server.listen({ port: 3000 });
 */
export async function createYAWLAPIServer(options = {}) {
  const server = new YAWLAPIServer(options);
  await server.fastify.ready();
  return server;
}

// =============================================================================
// Exports
// =============================================================================

export default {
  YAWLAPIServer,
  createYAWLAPIServer,
};
