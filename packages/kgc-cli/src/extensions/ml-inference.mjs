import { z } from 'zod';

const extension = {
  id: '@unrdf/ml-inference',
  description: 'ML model inference',
  nouns: {
    model: {
      description: 'Run inference models',
      verbs: {
        predict: {
          description: 'Run prediction',
          argsSchema: z.object({ modelId: z.string(), input: z.string() }),
          handler: async (args) => ({ modelId: args.modelId, prediction: null })
        }
      }
    }
  },
  priority: 40
};

export default extension;
