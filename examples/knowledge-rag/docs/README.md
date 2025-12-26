# Knowledge Graph RAG

Production-grade RAG (Retrieval-Augmented Generation) system for knowledge graphs with semantic search and LLM integration.

## Features

- **Semantic search**: Vector-based search over RDF triples
- **LLM-powered query understanding**: Intelligent query expansion
- **Context-aware answer generation**: LLM generates answers from knowledge graph
- **Fact verification**: Verify answer claims against source data

## Quick Start

```bash
pnpm install
node src/rag-pipeline.mjs
```

## Usage

```javascript
import { RAGPipeline } from './src/rag-pipeline.mjs';

const rag = new RAGPipeline({
  llmConfig: {
    model: 'gpt-4',
    apiKey: process.env.OPENAI_API_KEY,
  },
});

// Load knowledge graph
await rag.loadKnowledgeGraph('./data/knowledge.ttl', 'turtle');

// Query
const result = await rag.query({
  question: 'What companies does John work for?',
  maxResults: 5,
  includeReasoning: true,
});

console.log('Answer:', result.answer);
console.log('Confidence:', result.confidence);
console.log('Citations:', result.citations);
```

## Docker

```bash
docker build -t knowledge-rag .
docker run -p 3000:3000 knowledge-rag
```

## Testing

```bash
pnpm test
pnpm test:coverage
```

## Architecture

1. **Query Expansion**: Expand user query with synonyms and related terms
2. **Semantic Search**: Find relevant triples using vector similarity
3. **Reasoning**: Apply inference rules to enrich context
4. **Context Assembly**: Build structured context from triples
5. **Answer Generation**: LLM generates answer from context
6. **Fact Verification**: Verify claims against source data

## Performance

- Query latency: <500ms
- Semantic search: <100ms for 10K triples
- Answer generation: 1-3 seconds (LLM dependent)
