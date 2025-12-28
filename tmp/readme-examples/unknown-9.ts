import {
  createKnowledgeSubstrateCore,
  type KnowledgeSubstrate,
  type SPARQLResult,
} from '@unrdf/core';

const core: KnowledgeSubstrate = await createKnowledgeSubstrateCore();
const results: SPARQLResult[] = await core.query(store, sparql);