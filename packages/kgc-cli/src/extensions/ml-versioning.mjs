import { z } from 'zod';

const extension = {
  id: '@unrdf/ml-versioning',
  description: 'ML model versioning',
  nouns: {
    version: {
      description: 'Manage model versions',
      verbs: {
        snapshot: {
          description: 'Create model snapshot',
          argsSchema: z.object({ modelId: z.string() }),
          handler: async (args) => ({ snapshotId: `v_${Date.now()}`, modelId: args.modelId })
        }
      }
    }
  },
  priority: 41
};

export default extension;
