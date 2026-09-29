/**
 * @file Minimal local workflow engine for the example projects
 * @description
 * The examples were written against `@unrdf/yawl`, a package that no longer
 * exists in this monorepo (removed together with packages/yawl). This module
 * provides the small subset of that API the examples actually use so that they
 * (and their tests) run against real behaviour instead of a dangling import:
 *
 * - createWorkflow(def)                      -> validated workflow definition
 * - new WorkflowEngine({ store })            -> in-memory case engine
 *     createCase(workflow, input)            -> case instance ({ id, ... })
 *     getEnabledTasks(caseId)                -> tasks whose dependencies are done
 *     getTask(caseId, taskId)                -> task record
 *     startTask(caseId, taskId)              -> pending -> running (dispatched)
 *     resetTask(caseId, taskId)              -> running -> pending (dispatch failed)
 *     completeTask(caseId, taskId, result)   -> marks task done, completes case
 *     getStatus(caseId)                      -> { state, completed, total }
 * - createReceipt(caseId, payload)           -> { hash, data, timestamp }
 *
 * Task dependencies are expressed with `dependsOn: string[]` (default none).
 * The case store is bounded so long-running examples cannot grow without limit.
 *
 * @module examples/lib/workflow-engine
 */

import { createHash, randomUUID } from 'node:crypto';

const MAX_CASES = 10000;

/**
 * Validate and normalise a workflow definition.
 *
 * @param {{id?: string, name?: string, tasks?: Array<{id: string, dependsOn?: string[]}>}} def
 * @returns {object} Normalised workflow
 */
export function createWorkflow(def) {
  if (!def || typeof def !== 'object') {
    throw new TypeError('createWorkflow: definition must be an object');
  }
  const tasks = Array.isArray(def.tasks) ? def.tasks : [];
  const ids = new Set();
  for (const task of tasks) {
    if (!task || typeof task.id !== 'string' || task.id === '') {
      throw new TypeError('createWorkflow: every task needs a string id');
    }
    if (ids.has(task.id)) {
      throw new Error(`createWorkflow: duplicate task id "${task.id}"`);
    }
    ids.add(task.id);
  }
  for (const task of tasks) {
    for (const dep of task.dependsOn ?? []) {
      if (!ids.has(dep)) {
        throw new Error(`createWorkflow: task "${task.id}" depends on unknown task "${dep}"`);
      }
    }
  }
  return {
    id: def.id ?? randomUUID(),
    name: def.name ?? def.id ?? 'workflow',
    tasks: tasks.map(t => ({ ...t, dependsOn: [...(t.dependsOn ?? [])] })),
  };
}

/**
 * In-memory workflow case engine.
 */
export class WorkflowEngine {
  /**
   * @param {{store?: object}} [options]
   */
  constructor(options = {}) {
    this.store = options.store ?? null;
    this.cases = new Map();
  }

  /**
   * @param {object} workflow - Workflow definition (see createWorkflow)
   * @param {object} [input]
   * @returns {Promise<object>} Case instance
   */
  async createCase(workflow, input = {}) {
    const def = workflow?.tasks ? createWorkflow(workflow) : createWorkflow({});
    if (this.cases.size >= MAX_CASES) {
      // Evict the oldest case (Map preserves insertion order).
      this.cases.delete(this.cases.keys().next().value);
    }
    const id = randomUUID();
    const tasks = new Map(
      def.tasks.map(t => [t.id, { ...t, status: 'pending', result: undefined }])
    );
    const instance = {
      id,
      workflowId: def.id,
      input,
      state: tasks.size === 0 ? 'completed' : 'running',
      createdAt: Date.now(),
      tasks,
    };
    this.cases.set(id, instance);
    return { id, workflowId: def.id, input, state: instance.state };
  }

  _case(caseId) {
    const instance = this.cases.get(caseId);
    if (!instance) {
      throw new Error(`Case ${caseId} not found`);
    }
    return instance;
  }

  /**
   * @param {string} caseId
   * @returns {Promise<object[]>} Pending tasks whose dependencies are complete
   */
  async getEnabledTasks(caseId) {
    const instance = this._case(caseId);
    return [...instance.tasks.values()]
      .filter(
        t =>
          t.status === 'pending' &&
          t.dependsOn.every(dep => instance.tasks.get(dep).status === 'completed')
      )
      .map(t => ({ id: t.id, type: t.type, dependsOn: t.dependsOn }));
  }

  /**
   * @param {string} caseId
   * @param {string} taskId
   * @returns {Promise<object>} Task record
   */
  async getTask(caseId, taskId) {
    const task = this._case(caseId).tasks.get(taskId);
    if (!task) {
      throw new Error(`Task ${taskId} not found in case ${caseId}`);
    }
    return { ...task };
  }

  /**
   * Mark a pending task as dispatched so it is no longer reported as enabled.
   *
   * @param {string} caseId
   * @param {string} taskId
   * @returns {Promise<void>}
   */
  async startTask(caseId, taskId) {
    const task = this._case(caseId).tasks.get(taskId);
    if (!task) {
      throw new Error(`Task ${taskId} not found in case ${caseId}`);
    }
    if (task.status === 'pending') {
      task.status = 'running';
    }
  }

  /**
   * Return a running task to pending (worker lost / dispatch failed).
   *
   * @param {string} caseId
   * @param {string} taskId
   * @returns {Promise<void>}
   */
  async resetTask(caseId, taskId) {
    const task = this._case(caseId).tasks.get(taskId);
    if (!task) {
      throw new Error(`Task ${taskId} not found in case ${caseId}`);
    }
    if (task.status === 'running') {
      task.status = 'pending';
    }
  }

  /**
   * @param {string} caseId
   * @param {string} taskId
   * @param {*} result
   * @returns {Promise<void>}
   */
  async completeTask(caseId, taskId, result) {
    const instance = this._case(caseId);
    const task = instance.tasks.get(taskId);
    if (!task) {
      throw new Error(`Task ${taskId} not found in case ${caseId}`);
    }
    if (task.status === 'completed') {
      throw new Error(`Task ${taskId} already completed`);
    }
    task.status = 'completed';
    task.result = result;
    if ([...instance.tasks.values()].every(t => t.status === 'completed')) {
      instance.state = 'completed';
    }
  }

  /**
   * @param {string} caseId
   * @returns {Promise<{state: string, completed: number, total: number}>}
   */
  async getStatus(caseId) {
    const instance = this._case(caseId);
    const tasks = [...instance.tasks.values()];
    return {
      state: instance.state,
      completed: tasks.filter(t => t.status === 'completed').length,
      total: tasks.length,
    };
  }
}

/**
 * Create a hash-verifiable receipt: `hash === sha256(JSON.stringify(data))`.
 *
 * @param {string} caseId
 * @param {object} payload
 * @returns {Promise<{caseId: string, data: object, hash: string, timestamp: number}>}
 */
export async function createReceipt(caseId, payload = {}) {
  const data = { caseId, ...payload };
  const hash = createHash('sha256').update(JSON.stringify(data)).digest('hex');
  return { caseId, data, hash, timestamp: payload.timestamp ?? Date.now() };
}
