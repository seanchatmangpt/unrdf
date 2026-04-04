/**
 * @file Knowledge Hooks Self-Play Autonomics Loop
 * @module @unrdf/daemon/knowledge-self-play
 * @description
 *
 * Self-play autonomics system where RDF graph state determines hook execution,
 * hooks mutate the graph via SPARQL CONSTRUCT effects, and the loop terminates
 * when the graph reaches a stable state (no hooks fire) or max iterations.
 *
 * - State: Oxigraph RDF store
 * - Moves: Hook effects (SPARQL CONSTRUCT)
 * - Decision: SPARQL conditions in hook definitions
 * - Feedback: Store delta per iteration (input_hash ≠ output_hash = progress)
 * - Termination: Convergence (no change) or max iterations
 * - History: Episode metadata written back to store as RDF quads
 */

import { randomUUID } from 'crypto';

/**
 * Knowledge-driven self-play loop
 *
 * Executes hooks against an RDF store until the graph converges (no more changes).
 * Each iteration: evaluate hook conditions → fire matching hooks → record receipt → check convergence.
 */
export class KnowledgeSelfPlayLoop {
  #store;
  #engine;
  #maxIterations;
  #triggerType;
  #history;
  #episodeId;
  #startTime;

  /**
   * Create a new knowledge self-play loop
   *
   * @param {Object} options - Configuration
   * @param {Object} options.store - Oxigraph RDF store instance
   * @param {Object} options.engine - KnowledgeHookEngine instance
   * @param {number} [options.maxIterations=10] - Maximum iterations before force-stop
   * @param {string} [options.triggerType='continuous-improvement'] - Hook trigger type to filter
   */
  constructor({ store, engine, maxIterations = 10, triggerType = 'continuous-improvement' }) {
    if (!store) throw new Error('store is required');
    if (!engine) throw new Error('engine is required');
    if (!engine.execute) throw new Error('engine must have execute method');

    this.#store = store;
    this.#engine = engine;
    this.#maxIterations = maxIterations;
    this.#triggerType = triggerType;
    this.#history = [];
    this.#episodeId = randomUUID();
    this.#startTime = Date.now();
  }

  /**
   * Execute one iteration of the self-play loop
   *
   * @param {Object} [delta={}] - RDF delta context (previous receipt, metadata, etc.)
   * @returns {Promise<Object>} Step result with receipt, feedback, and convergence signal
   */
  async step(delta = {}) {
    // Execute hooks: evaluate conditions, fire matching hooks, return receipt
    const result = await this.#engine.execute(this.#store, delta, {
      trigger: this.#triggerType,
      episodeId: this.#episodeId,
    });

    const { receipt, executionResults = [] } = result;

    // Detect convergence: store changed = progress; no change = converged
    const storeChanged = receipt.input_hash !== receipt.output_hash;

    // Feedback: +0.1 per hook executed if store changed, else 0 (convergence reward)
    const feedback = storeChanged ? 0.1 * Math.max(1, executionResults.length) : 0;

    // Record step in history
    this.#history.push({
      stepNumber: this.#history.length,
      timestamp: Date.now(),
      receipt,
      feedback,
      storeChanged,
      hooksExecuted: executionResults.length,
    });

    return { receipt, feedback, storeChanged, hooksExecuted: executionResults.length };
  }

  /**
   * Run the self-play loop until convergence or max iterations
   *
   * @param {Object} [initialDelta={}] - Initial RDF delta context
   * @returns {Promise<Object>} Run result with episode ID, iterations, receipts, feedback, convergence flag
   */
  async run(initialDelta = {}) {
    let delta = initialDelta;
    let converged = false;
    const receipts = [];

    for (let i = 0; i < this.#maxIterations; i++) {
      const { receipt, storeChanged } = await this.step(delta);
      receipts.push(receipt);

      // Thread receipt chain forward: next iteration receives prior receipt
      delta = {
        previousReceipt: receipt,
        iterationNumber: i + 1,
      };

      // Early exit on convergence
      if (!storeChanged) {
        converged = true;
        break;
      }
    }

    const totalFeedback = this.#history.reduce((sum, step) => sum + step.feedback, 0);

    return {
      episodeId: this.#episodeId,
      startTime: this.#startTime,
      endTime: Date.now(),
      iterations: receipts.length,
      receipts,
      totalFeedback,
      converged,
      avgFeedbackPerIteration: this.#history.length > 0 ? totalFeedback / this.#history.length : 0,
    };
  }

  /**
   * Materialize episode metadata as RDF quads in the store
   *
   * @param {Object} runResult - Result from run()
   * @returns {Array} Array of quads written to store
   */
  materializeEpisodeRDF(runResult) {
    const episodeBase = `urn:unrdf:episode/${runResult.episodeId}`;
    const quads = [];

    // Episode summary triples
    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
      object: { value: 'urn:unrdf:SelfPlayEpisode' },
    });

    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'urn:unrdf:episodeId' },
      object: { value: runResult.episodeId, termType: 'Literal' },
    });

    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'urn:unrdf:iterations' },
      object: { value: String(runResult.iterations), datatype: 'http://www.w3.org/2001/XMLSchema#integer', termType: 'Literal' },
    });

    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'urn:unrdf:converged' },
      object: { value: String(runResult.converged), datatype: 'http://www.w3.org/2001/XMLSchema#boolean', termType: 'Literal' },
    });

    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'urn:unrdf:totalFeedback' },
      object: { value: String(runResult.totalFeedback), datatype: 'http://www.w3.org/2001/XMLSchema#decimal', termType: 'Literal' },
    });

    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'urn:unrdf:avgFeedbackPerIteration' },
      object: { value: String(runResult.avgFeedbackPerIteration), datatype: 'http://www.w3.org/2001/XMLSchema#decimal', termType: 'Literal' },
    });

    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'urn:unrdf:startTime' },
      object: { value: String(runResult.startTime), datatype: 'http://www.w3.org/2001/XMLSchema#long', termType: 'Literal' },
    });

    quads.push({
      subject: { value: episodeBase },
      predicate: { value: 'urn:unrdf:endTime' },
      object: { value: String(runResult.endTime), datatype: 'http://www.w3.org/2001/XMLSchema#long', termType: 'Literal' },
    });

    // Receipt chain: link each receipt hash to the episode
    runResult.receipts.forEach((receipt, idx) => {
      const receiptUri = `${episodeBase}/receipt/${idx}`;
      quads.push({
        subject: { value: episodeBase },
        predicate: { value: 'urn:unrdf:hasReceipt' },
        object: { value: receiptUri },
      });

      quads.push({
        subject: { value: receiptUri },
        predicate: { value: 'urn:unrdf:receiptHash' },
        object: { value: receipt.receiptHash, termType: 'Literal' },
      });

      if (receipt.previousReceiptHash) {
        quads.push({
          subject: { value: receiptUri },
          predicate: { value: 'urn:unrdf:previousReceiptHash' },
          object: { value: receipt.previousReceiptHash, termType: 'Literal' },
        });
      }
    });

    // Add all quads to store (immutable add operation)
    for (const quad of quads) {
      try {
        this.#store.add(quad);
      } catch (err) {
        console.warn(`Failed to add episode quad: ${err.message}`, quad);
      }
    }

    return quads;
  }

  /**
   * Get the execution history of all steps
   *
   * @returns {Array} Array of step records { stepNumber, timestamp, receipt, feedback, storeChanged, hooksExecuted }
   */
  getHistory() {
    return JSON.parse(JSON.stringify(this.#history));
  }

  /**
   * Get the episode ID
   *
   * @returns {string} Episode UUID
   */
  getEpisodeId() {
    return this.#episodeId;
  }

  /**
   * Get current episode metrics
   *
   * @returns {Object} Metrics including total feedback, iterations so far, convergence status
   */
  getMetrics() {
    const totalFeedback = this.#history.reduce((sum, step) => sum + step.feedback, 0);
    const stepsConverged = this.#history.filter(step => !step.storeChanged).length;
    const hooksExecutedTotal = this.#history.reduce((sum, step) => sum + step.hooksExecuted, 0);

    return {
      episodeId: this.#episodeId,
      totalSteps: this.#history.length,
      totalFeedback,
      stepsConverged,
      hooksExecutedTotal,
      avgHooksPerStep: this.#history.length > 0 ? hooksExecutedTotal / this.#history.length : 0,
    };
  }
}

/**
 * Factory function to create a KnowledgeSelfPlayLoop
 *
 * @param {Object} store - Oxigraph RDF store
 * @param {Object} engine - KnowledgeHookEngine instance
 * @param {Object} [options={}] - Configuration options
 * @returns {KnowledgeSelfPlayLoop} Self-play loop instance
 */
export function createKnowledgeSelfPlayLoop(store, engine, options = {}) {
  return new KnowledgeSelfPlayLoop({ store, engine, ...options });
}
