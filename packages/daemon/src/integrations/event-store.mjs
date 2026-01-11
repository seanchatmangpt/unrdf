/**
 * @file Event Store Integration for Daemon
 * @module @unrdf/daemon/integrations/event-store
 * @description Daemon integration with KGC event store for operation logging,
 * receipt generation, and audit trails with nanosecond timestamps.
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

const AuditSchema = z.object({
  id: z.string(),
  operationId: z.string(),
  timestamp: z.bigint(),
  status: z.enum(['enqueued', 'started', 'success', 'failure']),
  operationType: z.string(),
  inputHash: z.string().optional(),
  outputHash: z.string().optional(),
  error: z.string().optional(),
});

/**
 * Operation Auditor - Tracks operation lifecycle with event logging and receipts
 */
export class OperationAuditor {
  /**
   * @param {Object} options - Configuration
   * @param {Object} options.eventStore - Event store with appendEvent method
   * @param {Object} options.receiptGenerator - Generator with generateBatchReceipt method
   * @param {Object} [options.logger] - Logger instance
   */
  constructor(options = {}) {
    if (!options.eventStore?.appendEvent) throw new TypeError('eventStore required');
    if (!options.receiptGenerator?.generateBatchReceipt) throw new TypeError('receiptGenerator required');
    this.eventStore = options.eventStore;
    this.receiptGenerator = options.receiptGenerator;
    this.logger = options.logger || console;
    this.auditLog = [];
    this.receiptChain = [];
    this.ops = new Map();
  }

  /**
   *
   */
  _getNs() {
    return typeof process?.hrtime?.bigint === 'function'
      ? process.hrtime.bigint()
      : BigInt(Date.now()) * 1_000_000n;
  }

  /**
   *
   */
  async _hash(data) {
    const s = JSON.stringify(data, (k, v) => typeof v === 'bigint' ? v.toString() : v);
    return blake3(s);
  }

  /**
   *
   */
  async _id() {
    if (typeof crypto?.randomUUID === 'function') return crypto.randomUUID();
    try { return require('crypto').randomUUID(); } catch {
      return `${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    }
  }

  /**
   * Log operation enqueued
   * @param {string} opId - Operation ID
   * @param {string} type - Operation type
   * @param {Object} input - Input data
   */
  async logEnqueued(opId, type, input) {
    if (!opId?.trim()) throw new TypeError('opId required');
    const t = this._getNs();
    const h = await this._hash(input);
    const e = { id: await this._id(), operationId: opId, timestamp: t, status: 'enqueued', operationType: type, inputHash: h };
    AuditSchema.parse(e);
    this.auditLog.push(e);
    this.ops.set(opId, e);
    this.logger.log(`[Audit] Enqueued: ${opId}`);
    await this.eventStore.appendEvent({ type: 'OPERATION_ENQUEUED', payload: { operationId: opId, operationType: type, inputHash: h, timestamp: t.toString() } }, []);
    return e;
  }

  /**
   * Log operation started
   * @param {string} opId - Operation ID
   */
  async logStarted(opId) {
    if (!opId?.trim()) throw new TypeError('opId required');
    const e = this.ops.get(opId);
    if (!e) throw new Error(`Operation ${opId} not found`);
    const t = this._getNs();
    e.timestamp = t;
    e.status = 'started';
    this.logger.log(`[Audit] Started: ${opId}`);
    await this.eventStore.appendEvent({ type: 'OPERATION_STARTED', payload: { operationId: opId, timestamp: t.toString() } }, []);
    return e;
  }

  /**
   * Log operation success - generates receipt and maintains Merkle chain
   * @param {string} opId - Operation ID
   * @param {Object} output - Result data
   */
  async logSuccess(opId, output) {
    if (!opId?.trim()) throw new TypeError('opId required');
    const e = this.ops.get(opId);
    if (!e) throw new Error(`Operation ${opId} not found`);
    const t = this._getNs();
    const h = await this._hash(output);
    e.outputHash = h;
    e.timestamp = t;
    e.status = 'success';
    this.logger.log(`[Audit] Success: ${opId}`);
    const prev = this.receiptChain[this.receiptChain.length - 1];
    const receipt = await this.receiptGenerator.generateBatchReceipt({
      universeID: 'Q*_daemon_universe',
      operations: [{ type: 'morphism', subject: opId, predicate: 'http://kgc.io/daemon/result', object: h, timestamp: t }],
      operationType: e.operationType,
      merkleRoot: prev?.Q_PROV?.merkleRoot,
    });
    this.receiptChain.push(receipt);
    await this.eventStore.appendEvent({
      type: 'OPERATION_SUCCESS',
      payload: { operationId: opId, operationType: e.operationType, inputHash: e.inputHash, outputHash: h, receiptId: receipt.Q_ID, previousReceiptId: prev?.Q_ID, timestamp: t.toString() },
    }, []);
    return receipt;
  }

  /**
   * Log operation failure
   * @param {string} opId - Operation ID
   * @param {Error} err - Error object
   */
  async logFailure(opId, err) {
    if (!opId?.trim()) throw new TypeError('opId required');
    const e = this.ops.get(opId);
    if (!e) throw new Error(`Operation ${opId} not found`);
    const t = this._getNs();
    const m = err instanceof Error ? err.message : String(err);
    e.error = m;
    e.timestamp = t;
    e.status = 'failure';
    this.logger.error(`[Audit] Failed: ${opId} - ${m}`);
    await this.eventStore.appendEvent({ type: 'OPERATION_FAILURE', payload: { operationId: opId, operationType: e.operationType, error: m, timestamp: t.toString() } }, []);
    return e;
  }

  /**
   *
   */
  getAuditTrail(opId) { return this.auditLog.filter(e => e.operationId === opId); }
  /**
   *
   */
  getReceiptChain() { return [...this.receiptChain]; }
}

/**
 * Integrate event store with daemon
 * Sets up event logging for daemon operations with nanosecond timestamps
 * @param {Object} daemon - UnrdfDaemon instance (EventEmitter)
 * @param {Object} eventStore - Event store with appendEvent method
 */
export async function integrateEventStore(daemon, eventStore) {
  if (!daemon?.on) throw new TypeError('daemon must be EventEmitter');
  if (!eventStore?.appendEvent) throw new TypeError('eventStore.appendEvent required');
  const _getNs = () => typeof process?.hrtime?.bigint === 'function' ? process.hrtime.bigint() : BigInt(Date.now()) * 1_000_000n;
  daemon.on('task:enqueued', async (taskId, payload) => {
    try {
      await eventStore.appendEvent({ type: 'TASK_ENQUEUED', payload: { taskId, timestamp: _getNs().toString(), ...payload } }, []);
    } catch (err) { daemon.logger?.error(`[Event] enqueued failed:`, err); }
  });
  daemon.on('task:started', async (taskId, payload) => {
    try {
      await eventStore.appendEvent({ type: 'TASK_STARTED', payload: { taskId, timestamp: _getNs().toString(), ...payload } }, []);
    } catch (err) { daemon.logger?.error(`[Event] started failed:`, err); }
  });
  daemon.on('task:completed', async (taskId, result) => {
    try {
      const h = await blake3(JSON.stringify(result));
      await eventStore.appendEvent({ type: 'TASK_COMPLETED', payload: { taskId, timestamp: _getNs().toString(), resultHash: h } }, []);
    } catch (err) { daemon.logger?.error(`[Event] completed failed:`, err); }
  });
  return { integrated: true, daemonId: daemon.id };
}

/**
 * Integrate receipt generator with daemon
 * Generates receipts for daemon operations and maintains Merkle chain
 * @param {Object} daemon - UnrdfDaemon instance
 * @param {Object} receiptGenerator - Generator with generateBatchReceipt method
 */
export async function integrateReceiptGenerator(daemon, receiptGenerator) {
  if (!daemon?.on) throw new TypeError('daemon must be EventEmitter');
  if (!receiptGenerator?.generateBatchReceipt) throw new TypeError('receiptGenerator.generateBatchReceipt required');
  const auditor = new OperationAuditor({ eventStore: { appendEvent: async () => {} }, receiptGenerator, logger: daemon.logger });
  daemon.on('operation:success', async (opId, output) => {
    try { await auditor.logSuccess(opId, output); } catch (err) { daemon.logger?.error(`[Receipt] success failed:`, err); }
  });
  daemon.on('operation:failure', async (opId, error) => {
    try { await auditor.logFailure(opId, error); } catch (err) { daemon.logger?.error(`[Receipt] failure failed:`, err); }
  });
  return auditor;
}

/**
 * Replay operations from event store
 * Reconstructs operation history between timestamps
 * @param {Object} eventStore - Event store with queryEventLog method
 * @param {bigint} startTime - Start nanosecond timestamp
 * @param {bigint} endTime - End nanosecond timestamp
 */
export async function replayOperations(eventStore, startTime, endTime) {
  if (!eventStore?.queryEventLog) throw new TypeError('eventStore.queryEventLog required');
  if (typeof startTime !== 'bigint') throw new TypeError('startTime must be bigint');
  if (typeof endTime !== 'bigint') throw new TypeError('endTime must be bigint');
  if (startTime > endTime) throw new RangeError('startTime must be <= endTime');
  const ops = [];
  let errs = 0;
  try {
    const r = await eventStore.queryEventLog(`
      SELECT ?eventId ?timestamp ?type ?payload WHERE {
        GRAPH <http://kgc.io/event-log> {
          ?event <http://kgc.io/t_ns> ?timestamp .
          ?event <http://kgc.io/type> ?type .
          ?event <http://kgc.io/payload> ?payload .
        }
      }
      ORDER BY ?timestamp
    `);
    if (r?.results?.bindings) {
      for (const b of r.results.bindings) {
        const ts = BigInt(b.timestamp?.value || 0);
        if (ts >= startTime && ts <= endTime) {
          ops.push({ id: b.eventId?.value, timestamp: ts, type: b.type?.value, payload: b.payload?.value ? JSON.parse(b.payload.value) : {} });
        }
      }
    }
  } catch (err) { errs++; }
  return { success: errs === 0, operationCount: ops.length, operations: ops, verified: 0, errors: errs, startTime: startTime.toString(), endTime: endTime.toString() };
}
