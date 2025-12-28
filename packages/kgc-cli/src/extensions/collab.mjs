import { z } from 'zod';

const extension = {
  id: '@unrdf/collab',
  description: 'Collaborative knowledge graph editing',
  nouns: {
    session: {
      description: 'Collaboration sessions',
      verbs: {
        create: {
          description: 'Create collaboration session',
          argsSchema: z.object({ graphId: z.string(), users: z.array(z.string()).optional() }),
          handler: async (args) => ({ sessionId: `collab_${Date.now()}`, graphId: args.graphId })
        },
        join: {
          description: 'Join collaboration session',
          argsSchema: z.object({ sessionId: z.string(), userId: z.string() }),
          handler: async (args) => ({ joined: true, sessionId: args.sessionId })
        },
        sync: {
          description: 'Synchronize session state',
          argsSchema: z.object({ sessionId: z.string() }),
          handler: async (_args) => ({ synced: true, changes: 0 })
        }
      }
    }
  },
  priority: 70
};

export default extension;
