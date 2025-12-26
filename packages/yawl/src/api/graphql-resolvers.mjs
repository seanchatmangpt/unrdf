/**
 * GraphQL Resolvers for YAWL Workflows
 * Resolver functions for GraphQL operations
 *
 * @module @unrdf/yawl/api/graphql-resolvers
 */

/**
 * Create GraphQL resolvers for YAWL engine
 *
 * @param {Object} engine - YAWL engine instance
 * @returns {Object} Resolver map
 */
export function createResolvers(engine) {
  return {
    Query: {
      workflow: async (_, { id }) => {
        return await engine.getWorkflow(id);
      },

      workflows: async (_, { limit = 50, offset = 0 }) => {
        const all = await engine.listWorkflows();
        return all.slice(offset, offset + limit);
      },

      case: async (_, { id }) => {
        return await engine.getCase(id);
      },

      cases: async (_, { specId, status, limit = 50, offset = 0 }) => {
        const all = await engine.listCases(specId);
        let filtered = all;

        if (status) {
          filtered = all.filter((c) => c.status === status);
        }

        return filtered.slice(offset, offset + limit);
      },

      taskInstance: async (_, { id }) => {
        return await engine.getTaskInstance(id);
      },

      workItem: async (_, { id }) => {
        return await engine.getWorkItem(id);
      },

      receipts: async (_, { caseId, limit = 100, offset = 0 }) => {
        const all = await engine.getReceipts(caseId);
        return all.slice(offset, offset + limit);
      },

      searchWorkflows: async (_, { query, limit = 10 }) => {
        const all = await engine.listWorkflows();
        return all
          .filter((w) => w.name.toLowerCase().includes(query.toLowerCase()))
          .slice(0, limit);
      },

      workflowStats: async (_, { specId }) => {
        return await engine.getWorkflowStatistics(specId);
      },
    },

    Mutation: {
      createWorkflow: async (_, { input }) => {
        return await engine.createWorkflow(input);
      },

      updateWorkflow: async (_, { id, input }) => {
        return await engine.updateWorkflow(id, input);
      },

      deleteWorkflow: async (_, { id }) => {
        await engine.deleteWorkflow(id);
        return true;
      },

      startCase: async (_, { specId, data }) => {
        return await engine.startCase(specId, data);
      },

      completeTask: async (_, { taskInstanceId, result }) => {
        return await engine.completeTask(taskInstanceId, result);
      },

      cancelTask: async (_, { taskInstanceId, reason }) => {
        return await engine.cancelTask(taskInstanceId, reason);
      },

      cancelCase: async (_, { caseId, reason }) => {
        return await engine.cancelCase(caseId, reason);
      },

      allocateWorkItem: async (_, { workItemId, resourceId }) => {
        return await engine.allocateWorkItem(workItemId, resourceId);
      },

      startWorkItem: async (_, { workItemId }) => {
        return await engine.startWorkItem(workItemId);
      },

      completeWorkItem: async (_, { workItemId, result }) => {
        return await engine.completeWorkItem(workItemId, result);
      },
    },

    Subscription: {
      caseEvents: {
        subscribe: async function* (_, { caseId }) {
          const eventEmitter = engine.eventEmitter;
          const queue = [];
          let resolver = null;

          const listener = (event) => {
            if (event.caseId === caseId) {
              if (resolver) {
                resolver(event);
                resolver = null;
              } else {
                queue.push(event);
              }
            }
          };

          eventEmitter.on('caseEvent', listener);

          try {
            while (true) {
              if (queue.length > 0) {
                yield { caseEvents: queue.shift() };
              } else {
                await new Promise((resolve) => {
                  resolver = resolve;
                });
                if (queue.length > 0) {
                  yield { caseEvents: queue.shift() };
                }
              }
            }
          } finally {
            eventEmitter.off('caseEvent', listener);
          }
        },
      },

      workflowEvents: {
        subscribe: async function* (_, { specId }) {
          const eventEmitter = engine.eventEmitter;
          const queue = [];
          let resolver = null;

          const listener = (event) => {
            if (event.specId === specId) {
              if (resolver) {
                resolver(event);
                resolver = null;
              } else {
                queue.push(event);
              }
            }
          };

          eventEmitter.on('workflowEvent', listener);

          try {
            while (true) {
              if (queue.length > 0) {
                yield { workflowEvents: queue.shift() };
              } else {
                await new Promise((resolve) => {
                  resolver = resolve;
                });
                if (queue.length > 0) {
                  yield { workflowEvents: queue.shift() };
                }
              }
            }
          } finally {
            eventEmitter.off('workflowEvent', listener);
          }
        },
      },
    },
  };
}
