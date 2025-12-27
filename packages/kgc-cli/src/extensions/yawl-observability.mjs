import { z } from 'zod';

const extension = {
  id: '@unrdf/yawl-observability',
  description: 'Workflow observability',
  nouns: {
    observe: {
      description: 'Monitor workflows',
      verbs: {
        trace: {
          description: 'Trace workflow execution',
          argsSchema: z.object({ executionId: z.string() }),
          handler: async (args) => ({ executionId: args.executionId, spans: [] })
        }
      }
    }
  },
  priority: 32
};

export default extension;
