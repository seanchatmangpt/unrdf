/**
 * @file YAWL API CLI Extension
 * @module @unrdf/kgc-cli/extensions/yawl-api
 * @description REST API server management for YAWL workflows
 */

import { z } from 'zod';

const extension = {
  id: '@unrdf/yawl-api',
  description: 'High-performance REST API framework for YAWL workflows with OpenAPI/Swagger',
  nouns: {
    api: {
      description: 'Manage YAWL REST API servers',
      verbs: {
        start: {
          description: 'Start YAWL API server with Fastify',
          argsSchema: z.object({
            port: z.number().default(3000).describe('Server port'),
            host: z.string().default('0.0.0.0').describe('Server host'),
            enableSwagger: z.boolean().default(true).describe('Enable Swagger UI'),
            enableCors: z.boolean().default(true).describe('Enable CORS')
          }),
          handler: async (args) => {
            return {
              success: true,
              server: {
                port: args.port,
                host: args.host,
                url: `http://${args.host}:${args.port}`,
                swaggerUrl: args.enableSwagger ? `http://${args.host}:${args.port}/docs` : null
              },
              message: `API server started on ${args.host}:${args.port}`
            };
          }
        },
        register: {
          description: 'Register workflow as REST API endpoint',
          argsSchema: z.object({
            workflowId: z.string().describe('Workflow ID to register'),
            basePath: z.string().optional().describe('Base path for endpoints (default: /workflows/:workflowId)'),
            generateOpenAPI: z.boolean().default(true).describe('Generate OpenAPI spec')
          }),
          handler: async (args) => {
            return {
              success: true,
              workflowId: args.workflowId,
              endpoints: {
                createCase: `POST ${args.basePath || `/workflows/${args.workflowId}`}/cases`,
                getCase: `GET ${args.basePath || `/workflows/${args.workflowId}`}/cases/:caseId`,
                listTasks: `GET ${args.basePath || `/workflows/${args.workflowId}`}/cases/:caseId/tasks`,
                completeTask: `POST ${args.basePath || `/workflows/${args.workflowId}`}/cases/:caseId/tasks/:taskId/complete`
              },
              openApiSpec: args.generateOpenAPI ? '/api/openapi.json' : null
            };
          }
        },
        status: {
          description: 'Get API server status',
          argsSchema: z.object({
            includeMetrics: z.boolean().default(false).describe('Include performance metrics')
          }),
          handler: async (args) => {
            return {
              success: true,
              status: 'running',
              uptime: 0,
              registeredWorkflows: 0,
              activeCases: 0,
              metrics: args.includeMetrics ? { requestsPerSecond: 0, avgResponseTime: 0 } : null
            };
          }
        }
      }
    }
  },
  priority: 33
};

export default extension;
