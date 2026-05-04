/**
 * POWL8 Process DAG Builder
 * 
 * Provides a Fluent API / Builder pattern to construct
 * POWL8 (Process Ontology Web Language) DAGs easily for the HardenedAtomVM.
 */

export class Powl8Builder {
  constructor() {
    this.dag = [];
  }

  /**
   * Spawns a sequential process task.
   * @param {Array<Object>} children - Microtasks to execute sequentially.
   * @returns {Powl8Builder}
   */
  spawnSeq(children) {
    this.dag.push({
      opcode: 'OP_SPAWN_SEQ',
      children
    });
    return this;
  }

  /**
   * Spawns a choice gateway (XOR split).
   * @param {string|function} predicate - The condition to evaluate.
   * @param {Object} branches - Map of branch outcomes to microtasks.
   * @returns {Powl8Builder}
   */
  spawnChoice(predicate, branches) {
    this.dag.push({
      opcode: 'OP_SPAWN_CHOICE',
      predicate,
      branches
    });
    return this;
  }

  /**
   * Spawns a synchronization barrier (AND join).
   * @param {Array<string>} dependencies - Process IDs to wait for.
   * @returns {Powl8Builder}
   */
  awaitSync(dependencies) {
    this.dag.push({
      opcode: 'OP_AWAIT_SYNC',
      dependencies
    });
    return this;
  }

  /**
   * Spawns a standard microtask (not POWL8 specific).
   * @param {Object} task - The task payload.
   * @returns {Powl8Builder}
   */
  addTask(task) {
    this.dag.push(task);
    return this;
  }

  /**
   * Returns the constructed DAG ready for execution.
   * @returns {Array<Object>}
   */
  build() {
    return this.dag;
  }
}
