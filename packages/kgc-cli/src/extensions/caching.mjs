import { z } from 'zod';

const extension = {
  id: '@unrdf/caching',
  description: 'Cache management',
  nouns: {
    cache: {
      description: 'Manage caches',
      verbs: {
        clear: {
          description: 'Clear cache',
          argsSchema: z.object({ key: z.string().optional() }),
          handler: async (_args) => ({ cleared: true })
        }
      }
    }
  },
  priority: 51
};

export default extension;
