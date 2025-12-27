import { z } from 'zod';

const extension = {
  id: '@unrdf/cli',
  description: 'Legacy CLI utilities',
  nouns: {
    util: {
      description: 'CLI utilities',
      verbs: {
        config: {
          description: 'Manage CLI configuration',
          argsSchema: z.object({ key: z.string(), value: z.string().optional() }),
          handler: async (args) => ({ key: args.key, value: args.value || null })
        },
        version: {
          description: 'Show version info',
          argsSchema: z.object({}),
          handler: async () => ({ version: '1.0.0', node: process.version })
        }
      }
    }
  },
  priority: 80
};

export default extension;
