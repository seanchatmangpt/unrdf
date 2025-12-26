/**
 * @unrdf/yawl-langchain - LangChain Integration for YAWL
 *
 * AI-powered workflow orchestration combining LangChain agents with YAWL
 * workflow patterns and RDF knowledge graph integration.
 *
 * @module @unrdf/yawl-langchain
 */

export {
  YAWLLangChainAdapter,
  createLangChainTaskExecutor,
  createPromptEngineeringHook,
  YAWL_LC_NS,
  LangChainTaskConfigSchema,
} from './adapter.mjs';

// Re-export commonly used YAWL types for convenience
export {
  TaskDefinition,
  TaskInstance,
  TaskStatus,
  Workflow,
  WorkflowEngine,
} from '@unrdf/yawl';

// Re-export RDF utilities
export {
  createStore,
  dataFactory,
} from '@unrdf/oxigraph';
