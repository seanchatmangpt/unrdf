import { getGroqProvider } from '@unrdf/daemon';
import { generateText } from 'ai';

const provider = getGroqProvider();
const model = provider.getDefaultModel();

const result = await generateText({
  model,
  prompt: 'Analyze this RDF graph and suggest enrichments',
  tools: {
    queryGraph: {
      description: 'Query the knowledge graph',
      execute: async ({ query }) => await store.query(query),
    },
    enrichGraph: {
      description: 'Add triples to improve the graph',
      execute: async ({ triple }) => await store.add(triple),
    },
  },
  toolChoice: 'auto',
});