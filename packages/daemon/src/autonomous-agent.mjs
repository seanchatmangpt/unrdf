/**
 * @file Autonomous Knowledge Agent
 * @module @unrdf/daemon/autonomous-agent
 * @description Agent with persistent, self-improving knowledge graph
 */

import { EventEmitter } from 'events';
import { generateText } from 'ai';
import { AutonomousRefinementEngine } from './autonomous-refinement-engine.mjs';
import { getGroqProvider } from './providers/groq.mjs';

/**
 * Autonomous agent with persistent knowledge graph
 */
export class AutonomousKnowledgeAgent extends EventEmitter {
  constructor(name, config = {}) {
    super();
    this.name = name;
    this.kg = new AutonomousRefinementEngine({
      graphId: `${name}-kg`,
      goalTriples: config.goalTriples || 100,
      maxIterations: config.maxIterations || 50,
      ...config,
    });
    this.reasoning = []; // Decision audit trail
    this.llmProvider = config.llmProvider || getGroqProvider();
  }

  /**
   * Reason about a task using KG context
   */
  async reason(task, context = {}) {
    const startTime = Date.now();

    try {
      // 1. Query KG for context
      const kgContext = await this.kg.query(
        context.query || 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 20'
      );

      // 2. LLM reasons with task + context
      const model = this.llmProvider.getDefaultModel();
      const decision = await generateText({
        model,
        prompt: `You are "${this.name}".
Task: ${task}
Known facts (from your knowledge graph):
${kgContext || '(empty)'}

What should you do? Be specific and actionable.`,
        maxTokens: 200,
      });

      // 3. Execute decision (parse and act)
      const result = await this.execute(decision.text, context);

      // 4. Learn: refine KG based on result
      if (result.success) {
        await this.kg.refine(
          context.store || this.kg.store,
          this.llmProvider
        );
      }

      // 5. Record reasoning trace
      const reasoningEntry = {
        timestamp: Date.now(),
        task,
        decision: decision.text.substring(0, 100),
        result: result.success ? 'success' : 'failed',
        duration: Date.now() - startTime,
      };
      this.reasoning.push(reasoningEntry);

      this.emit('reasoning-complete', reasoningEntry);
      return result;
    } catch (err) {
      this.emit('error', err);
      throw err;
    }
  }

  /**
   * Execute a decision
   */
  async execute(decision, context = {}) {
    try {
      // Parse decision and extract action
      const action = this.parseAction(decision);

      // Execute action
      if (action.type === 'query') {
        return { success: true, action: 'query', data: action };
      } else if (action.type === 'add') {
        return { success: true, action: 'add', data: action };
      } else {
        return { success: false, error: 'unknown action' };
      }
    } catch (err) {
      return { success: false, error: err.message };
    }
  }

  /**
   * Parse LLM decision to extract action
   */
  parseAction(text) {
    // Simple heuristic: if mentions "query", "search", "find" → query
    // if mentions "add", "create", "learn" → add
    if (text.toLowerCase().includes('query') ||
        text.toLowerCase().includes('search')) {
      return { type: 'query', text };
    }
    if (text.toLowerCase().includes('add') ||
        text.toLowerCase().includes('create')) {
      return { type: 'add', text };
    }
    return { type: 'unknown', text };
  }

  /**
   * Get audit trail of reasoning
   */
  getReasoningTrace() {
    return this.reasoning;
  }

  /**
   * Get agent's knowledge graph
   */
  getKnowledgeGraph() {
    return this.kg;
  }

  /**
   * Reset agent state
   */
  async reset() {
    this.reasoning = [];
    // KG can be reset via AutonomousRefinementEngine
  }
}

export default AutonomousKnowledgeAgent;
