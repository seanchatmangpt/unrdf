/**
 * YAWL-LangChain Adapter
 *
 * Bridges LangChain agents/chains with YAWL workflow tasks, enabling:
 * - AI-powered workflow orchestration
 * - RDF knowledge graph context injection into LLM prompts
 * - Automatic storage of LLM outputs as RDF triples
 * - Hook-based prompt engineering policies
 *
 * @module @unrdf/yawl-langchain/adapter
 */

import { z } from 'zod';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { TaskDefinition, TaskStatus } from '@unrdf/yawl';
import { now } from '@unrdf/kgc-4d';

const { quad, namedNode, literal } = dataFactory;

// =============================================================================
// LangChain Task Adapter Schema
// =============================================================================

/**
 * Configuration for wrapping a LangChain agent as a YAWL task
 */
export const LangChainTaskConfigSchema = z.object({
  /** Task ID in the workflow */
  taskId: z.string().min(1),
  /** Human-readable task name */
  taskName: z.string().optional(),
  /** LangChain agent, chain, or runnable */
  agent: z.any(), // LangChain's Runnable interface
  /** Input variable mapping from YAWL inputData to LangChain prompt */
  inputMapping: z.record(z.string()).optional(),
  /** Output field to extract from LangChain response */
  outputField: z.string().default('output'),
  /** RDF context query (SPARQL) to inject into prompt */
  contextQuery: z.string().optional(),
  /** Prompt template with placeholders for context and input */
  promptTemplate: z.string().optional(),
  /** RDF predicate for storing agent output */
  rdfPredicate: z.string().optional(),
  /** Pre-execution hook for prompt engineering */
  preHook: z.function().optional(),
  /** Post-execution hook for output validation */
  postHook: z.function().optional(),
  /** Timeout in milliseconds */
  timeout: z.number().positive().default(30000),
});

// =============================================================================
// RDF Namespace for YAWL-LangChain Integration
// =============================================================================

const YAWL_LC = 'http://unrdf.dev/yawl/langchain#';
const YAWL_LC_NS = {
  Agent: `${YAWL_LC}Agent`,
  agentOutput: `${YAWL_LC}agentOutput`,
  agentInput: `${YAWL_LC}agentInput`,
  executedAt: `${YAWL_LC}executedAt`,
  executionTime: `${YAWL_LC}executionTime`,
  modelName: `${YAWL_LC}modelName`,
  tokenUsage: `${YAWL_LC}tokenUsage`,
  rdfContext: `${YAWL_LC}rdfContext`,
  promptUsed: `${YAWL_LC}promptUsed`,
};

// =============================================================================
// YAWLLangChainAdapter Class
// =============================================================================

/**
 * Adapter that wraps LangChain agents as YAWL workflow tasks
 *
 * @example
 * const adapter = new YAWLLangChainAdapter({
 *   taskId: 'analyze-code',
 *   agent: new ChatOpenAI({ modelName: 'gpt-4' }),
 *   contextQuery: 'SELECT ?code WHERE { ?file :hasContent ?code }',
 *   rdfPredicate: 'http://example.org/analysis',
 * });
 *
 * const taskDef = adapter.createTaskDefinition();
 * const result = await adapter.execute(taskInstance, caseData);
 */
export class YAWLLangChainAdapter {
  /**
   * @param {Object} config - LangChain task configuration
   */
  constructor(config) {
    this.config = LangChainTaskConfigSchema.parse(config);
    this.rdfStore = createStore();
    this.executionHistory = [];
  }

  /**
   * Create a YAWL TaskDefinition for this LangChain agent
   *
   * @param {Object} [options] - Additional task definition options
   * @returns {TaskDefinition} YAWL task definition
   */
  createTaskDefinition(options = {}) {
    return new TaskDefinition({
      id: this.config.taskId,
      name: this.config.taskName ?? this.config.taskId,
      kind: 'AtomicTask',
      timeout: this.config.timeout,
      ...options,
      // Pre-condition: validate RDF context is available
      preCondition: async (context) => {
        if (this.config.contextQuery) {
          const rdfContext = await this._queryRDFContext(context.inputData.rdfStore);
          if (!rdfContext || rdfContext.length === 0) {
            return { valid: false, reason: 'RDF context query returned no results' };
          }
        }
        if (this.config.preHook) {
          return this.config.preHook(context);
        }
        return { valid: true, reason: 'Pre-conditions satisfied' };
      },
      // Post-condition: validate agent output
      postCondition: async (context) => {
        if (this.config.postHook) {
          return this.config.postHook(context);
        }
        return { valid: true, reason: 'Post-conditions satisfied' };
      },
    });
  }

  /**
   * Execute the LangChain agent for a YAWL task instance
   *
   * @param {Object} taskInstance - YAWL task instance
   * @param {Object} caseData - Case-level data including RDF store
   * @returns {Promise<Object>} Execution result with RDF triples
   */
  async execute(taskInstance, caseData = {}) {
    const startTime = now();

    try {
      // 1. Extract RDF context if configured
      const rdfContext = this.config.contextQuery && caseData.rdfStore
        ? await this._queryRDFContext(caseData.rdfStore)
        : null;

      // 2. Build prompt with context and input data
      const prompt = this._buildPrompt(taskInstance.inputData, rdfContext);

      // 3. Invoke LangChain agent
      const response = await this.config.agent.invoke({
        input: prompt,
        ...this._mapInputData(taskInstance.inputData),
      });

      // 4. Extract output
      const output = typeof response === 'string'
        ? response
        : response[this.config.outputField] ?? response.output ?? String(response);

      const executionTime = now() - startTime;

      // 5. Store output as RDF if configured
      const rdfTriples = this._storeAsRDF(taskInstance, output, rdfContext, executionTime);

      // 6. Record execution history
      this.executionHistory.push({
        taskId: taskInstance.id,
        timestamp: startTime,
        executionTime,
        prompt,
        output,
        rdfTriples: rdfTriples.length,
      });

      return {
        output,
        rdfTriples,
        executionTime,
        metadata: {
          taskId: taskInstance.id,
          agentType: this.config.agent.constructor.name,
          timestamp: startTime,
          contextUsed: !!rdfContext,
        },
      };
    } catch (error) {
      throw new Error(`LangChain agent execution failed: ${error.message}`);
    }
  }

  /**
   * Query RDF store for context using SPARQL
   *
   * @private
   * @param {Object} rdfStore - Oxigraph RDF store
   * @returns {Promise<Array<Object>>} Query results
   */
  async _queryRDFContext(rdfStore) {
    if (!this.config.contextQuery || !rdfStore) return null;

    try {
      const results = [];
      for (const binding of rdfStore.query(this.config.contextQuery)) {
        const row = {};
        for (const [key, value] of binding) {
          row[key] = value.value;
        }
        results.push(row);
      }
      return results;
    } catch (error) {
      console.warn(`RDF context query failed: ${error.message}`);
      return null;
    }
  }

  /**
   * Build prompt combining template, context, and input data
   *
   * @private
   * @param {Object} inputData - Task input data
   * @param {Array<Object>|null} rdfContext - RDF query results
   * @returns {string} Formatted prompt
   */
  _buildPrompt(inputData, rdfContext) {
    let prompt = this.config.promptTemplate ?? 'Input: {input}';

    // Inject RDF context
    if (rdfContext && rdfContext.length > 0) {
      const contextText = JSON.stringify(rdfContext, null, 2);
      prompt = `Knowledge Graph Context:\n${contextText}\n\n${prompt}`;
    }

    // Replace placeholders with input data
    for (const [key, value] of Object.entries(inputData)) {
      prompt = prompt.replace(`{${key}}`, String(value));
    }

    return prompt;
  }

  /**
   * Map YAWL input data to LangChain agent input format
   *
   * @private
   * @param {Object} inputData - Task input data
   * @returns {Object} Mapped input for LangChain
   */
  _mapInputData(inputData) {
    if (!this.config.inputMapping) return inputData;

    const mapped = {};
    for (const [yawlKey, langchainKey] of Object.entries(this.config.inputMapping)) {
      if (inputData[yawlKey] !== undefined) {
        mapped[langchainKey] = inputData[yawlKey];
      }
    }
    return mapped;
  }

  /**
   * Store LangChain output as RDF triples
   *
   * @private
   * @param {Object} taskInstance - YAWL task instance
   * @param {string} output - Agent output
   * @param {Array<Object>|null} rdfContext - RDF context used
   * @param {bigint} executionTime - Execution time in nanoseconds
   * @returns {Array<Object>} Generated RDF triples
   */
  _storeAsRDF(taskInstance, output, rdfContext, executionTime) {
    const triples = [];
    const taskUri = namedNode(`http://unrdf.dev/tasks/${taskInstance.id}`);

    // Store basic execution metadata
    triples.push(
      quad(taskUri, namedNode(YAWL_LC_NS.agentOutput), literal(output)),
      quad(taskUri, namedNode(YAWL_LC_NS.executedAt), literal(new Date().toISOString())),
      quad(taskUri, namedNode(YAWL_LC_NS.executionTime), literal(String(executionTime))),
    );

    // Store input data
    if (taskInstance.inputData) {
      triples.push(
        quad(taskUri, namedNode(YAWL_LC_NS.agentInput), literal(JSON.stringify(taskInstance.inputData)))
      );
    }

    // Store RDF context reference if used
    if (rdfContext) {
      triples.push(
        quad(taskUri, namedNode(YAWL_LC_NS.rdfContext), literal(JSON.stringify(rdfContext)))
      );
    }

    // Store with custom predicate if configured
    if (this.config.rdfPredicate) {
      triples.push(
        quad(taskUri, namedNode(this.config.rdfPredicate), literal(output))
      );
    }

    // Add triples to internal store
    for (const triple of triples) {
      this.rdfStore.add(triple);
    }

    return triples;
  }

  /**
   * Get RDF store with all agent outputs
   *
   * @returns {Object} Oxigraph RDF store
   */
  getRDFStore() {
    return this.rdfStore;
  }

  /**
   * Get execution history
   *
   * @returns {Array<Object>} Execution records
   */
  getExecutionHistory() {
    return [...this.executionHistory];
  }

  /**
   * Export RDF store as Turtle format
   *
   * @returns {string} Turtle serialization
   */
  exportAsTurtle() {
    return this.rdfStore.dump('text/turtle');
  }
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Create a YAWL task executor that wraps a LangChain agent
 *
 * @param {Object} config - LangChain task configuration
 * @returns {Function} Task executor function for YAWL engine
 */
export function createLangChainTaskExecutor(config) {
  const adapter = new YAWLLangChainAdapter(config);

  return async (taskInstance, caseData) => {
    const result = await adapter.execute(taskInstance, caseData);

    // Update task instance with output
    taskInstance.setOutputData({
      ...result,
      executedBy: 'langchain-adapter',
    });

    return result;
  };
}

/**
 * Create a policy hook for prompt engineering
 *
 * @param {Function} promptModifier - Function to modify prompts
 * @returns {Function} YAWL hook function
 */
export function createPromptEngineeringHook(promptModifier) {
  return async (context) => {
    const { taskInstance } = context;

    if (taskInstance.inputData.prompt) {
      taskInstance.inputData.prompt = await promptModifier(
        taskInstance.inputData.prompt,
        context
      );
    }

    return { valid: true, reason: 'Prompt engineering hook applied' };
  };
}

// =============================================================================
// Exports
// =============================================================================

export { YAWL_LC_NS, LangChainTaskConfigSchema };
