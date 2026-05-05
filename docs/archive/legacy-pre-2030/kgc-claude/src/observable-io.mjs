/**
 * Observable-only I/O System
 *
 * Σ_io (observable-only):
 *   O ≔ O_vm ⊔ O_bb
 *   O_vm ≔ Obs(E)                         // permitted VM observations
 *   O_bb ≔ ⊔_{α∈𝔄} ⊔_{x∈X} Trace(α(x))   // black-box traces on controlled X
 *
 * @module @unrdf/kgc-claude/observable-io
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now, VectorClock } from '@unrdf/kgc-4d';

/**
 * Trace entry schema - black-box trace of agent execution
 */
export const TraceEntrySchema = z.object({
  id: z.string(),
  agent_id: z.string(),
  input_hash: z.string(),
  output_hash: z.string().optional(),
  t_start: z.bigint(),
  t_end: z.bigint().optional(),
  status: z.enum(['pending', 'running', 'completed', 'failed']),
  error: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof TraceEntrySchema>} TraceEntry
 */

/**
 * VM Observation schema
 */
export const VMObservationSchema = z.object({
  id: z.string(),
  type: z.enum(['memory', 'cpu', 'fs', 'network', 'process']),
  t_ns: z.bigint(),
  data: z.record(z.any()),
  hash: z.string(),
});

/**
 * @typedef {z.infer<typeof VMObservationSchema>} VMObservation
 */

/**
 * Controlled input schema
 */
export const ControlledInputSchema = z.object({
  id: z.string(),
  type: z.string(),
  value: z.any(),
  hash: z.string(),
});

/**
 * @typedef {z.infer<typeof ControlledInputSchema>} ControlledInput
 */

/**
 * Observable I/O collector - captures O_vm and O_bb
 */
export class ObservableIO {
  constructor() {
    /** @type {VMObservation[]} */
    this.vmObservations = [];
    /** @type {Map<string, TraceEntry[]>} */
    this.agentTraces = new Map();
    /** @type {ControlledInput[]} */
    this.controlledInputs = [];
    this.vectorClock = new VectorClock('observable-io');
  }

  /**
   * Generate UUID
   */
  generateId() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return crypto.randomUUID();
    }
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = (Math.random() * 16) | 0;
      const v = c === 'x' ? r : (r & 0x3) | 0x8;
      return v.toString(16);
    });
  }

  /**
   * Record VM observation
   * O_vm ≔ Obs(E)
   * @param {string} type
   * @param {Object} data
   * @returns {Promise<VMObservation>}
   */
  async recordVMObservation(type, data) {
    const t_ns = now();
    const id = `obs-${type}-${t_ns}`;

    const obsData = JSON.stringify({ type, data, t_ns: t_ns.toString() });
    const hash = await blake3(obsData);

    const observation = VMObservationSchema.parse({
      id,
      type,
      t_ns,
      data,
      hash,
    });

    this.vmObservations.push(observation);
    this.vectorClock.increment();

    return observation;
  }

  /**
   * Start trace for agent execution
   * @param {string} agentId
   * @param {any} input
   * @returns {Promise<TraceEntry>}
   */
  async startTrace(agentId, input) {
    const t_start = now();
    const id = `trace-${agentId}-${t_start}`;

    const inputData = JSON.stringify(input);
    const input_hash = await blake3(inputData);

    const trace = TraceEntrySchema.parse({
      id,
      agent_id: agentId,
      input_hash,
      t_start,
      status: 'running',
    });

    if (!this.agentTraces.has(agentId)) {
      this.agentTraces.set(agentId, []);
    }
    this.agentTraces.get(agentId).push(trace);

    return trace;
  }

  /**
   * Complete trace
   * @param {string} traceId
   * @param {any} output
   * @param {string} [error]
   * @returns {Promise<TraceEntry>}
   */
  async completeTrace(traceId, output, error) {
    const t_end = now();

    for (const [agentId, traces] of this.agentTraces) {
      const trace = traces.find(t => t.id === traceId);
      if (trace) {
        const outputData = output ? JSON.stringify(output) : '';
        const output_hash = output ? await blake3(outputData) : undefined;

        trace.t_end = t_end;
        trace.output_hash = output_hash;
        trace.status = error ? 'failed' : 'completed';
        trace.error = error;

        return trace;
      }
    }

    throw new Error(`Trace not found: ${traceId}`);
  }

  /**
   * Register controlled input
   * X := minimal controlled inputs set
   * @param {string} type
   * @param {any} value
   * @returns {Promise<ControlledInput>}
   */
  async registerInput(type, value) {
    const id = this.generateId();
    const valueData = JSON.stringify(value);
    const hash = await blake3(valueData);

    const input = ControlledInputSchema.parse({
      id,
      type,
      value,
      hash,
    });

    this.controlledInputs.push(input);
    return input;
  }

  /**
   * Get all observations (O = O_vm ⊔ O_bb)
   * @returns {{vm: VMObservation[], traces: Map<string, TraceEntry[]>}}
   */
  getObservations() {
    return {
      vm: [...this.vmObservations],
      traces: new Map(this.agentTraces),
    };
  }

  /**
   * Get agent-specific traces
   * O_bb ≔ ⊔_{α∈𝔄} ⊔_{x∈X} Trace(α(x))
   * @param {string} agentId
   * @returns {TraceEntry[]}
   */
  getAgentTraces(agentId) {
    return this.agentTraces.get(agentId) || [];
  }

  /**
   * Union observations from another collector
   * O = O ⊔ O'
   * @param {ObservableIO} other
   * @returns {void}
   */
  union(other) {
    this.vmObservations.push(...other.vmObservations);

    for (const [agentId, traces] of other.agentTraces) {
      if (!this.agentTraces.has(agentId)) {
        this.agentTraces.set(agentId, []);
      }
      this.agentTraces.get(agentId).push(...traces);
    }

    this.controlledInputs.push(...other.controlledInputs);
  }

  /**
   * Compute observation hash for provenance
   * @returns {Promise<string>}
   */
  async computeHash() {
    const data = {
      vm: this.vmObservations.map(o => o.hash),
      traces: Array.from(this.agentTraces.entries()).map(([agentId, traces]) => ({
        agentId,
        hashes: traces.map(t => `${t.input_hash}:${t.output_hash || 'pending'}`),
      })),
      inputs: this.controlledInputs.map(i => i.hash),
    };

    return blake3(JSON.stringify(data));
  }

  /**
   * Clear all observations
   */
  clear() {
    this.vmObservations = [];
    this.agentTraces.clear();
    this.controlledInputs = [];
  }
}

/**
 * Observable wrapper for functions
 * Wraps any function to produce O_bb traces
 * @param {ObservableIO} io
 * @param {string} agentId
 * @returns {Function}
 */
export function withObservable(io, agentId) {
  /**
   * @param {Function} fn
   * @returns {Function}
   */
  return function(fn) {
    return async function(...args) {
      const trace = await io.startTrace(agentId, args);

      try {
        const result = await fn(...args);
        await io.completeTrace(trace.id, result);
        return result;
      } catch (error) {
        await io.completeTrace(trace.id, null, error.message);
        throw error;
      }
    };
  };
}

/**
 * Create observable IO with VM monitoring
 * @returns {ObservableIO}
 */
export function createObservableIO() {
  return new ObservableIO();
}

export default ObservableIO;
