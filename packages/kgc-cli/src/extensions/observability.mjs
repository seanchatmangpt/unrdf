import { z } from 'zod';

const extension = {
  id: '@unrdf/observability',
  description: 'System observability',
  nouns: {
    observe: {
      description: 'Observe system metrics',
      verbs: {
        metrics: {
          description: 'Get system metrics',
          handler: async () => ({ uptime: 12345, memory: { used: 100 } })
        }
      }
    }
  },
  priority: 50
};

export default extension;
