import { z } from 'zod';

const extension = {
  id: '@unrdf/kgc-claude',
  description: 'KGC-Claude integration for knowledge capture',
  nouns: {
    session: {
      description: 'Manage Claude knowledge capture sessions',
      verbs: {
        start: {
          description: 'Start knowledge capture session',
          argsSchema: z.object({ context: z.string(), mode: z.string().optional() }),
          handler: async (args) => ({ sessionId: `claude_${Date.now()}`, context: args.context })
        },
        capture: {
          description: 'Capture knowledge from session',
          argsSchema: z.object({ sessionId: z.string() }),
          handler: async (args) => ({ captured: true, sessionId: args.sessionId })
        }
      }
    }
  },
  priority: 14
};

export default extension;
