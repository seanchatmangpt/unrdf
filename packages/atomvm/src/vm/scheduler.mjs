/**
 * POWL8 Micro-Task Scheduler
 * Handles DAG process execution and synchronization barriers.
 */

export class Powl8Scheduler {
  constructor(vm) {
    this.vm = vm;
    this.taskQueue = [];
    this.runningProcesses = new Map();
  }

  schedule(taskNode) {
    this.taskQueue.push(taskNode);
  }

  async runLoop() {
    while (this.taskQueue.length > 0) {
      const task = this.taskQueue.shift();
      
      switch (task.opcode) {
        case 'OP_SPAWN_SEQ':
          await this._executeSequential(task);
          break;
        case 'OP_SPAWN_CHOICE':
          await this._executeChoice(task);
          break;
        case 'OP_AWAIT_SYNC':
          await this._executeSyncBarrier(task);
          break;
        default:
          await this.vm.executeMicrotask(task);
      }
    }
  }

  async _executeSequential(task) {
    for (const step of task.children) {
      await this.vm.executeMicrotask(step);
    }
  }

  async _executeChoice(task) {
    // Evaluates an acceptance predicate (Doctor Gate)
    const validBranch = await this.vm.evaluatePredicate(task.predicate);
    if (validBranch) {
      await this.vm.executeMicrotask(task.branches[validBranch]);
    }
  }

  async _executeSyncBarrier(task) {
    // Awaits multiple receipts/promises
    await Promise.all(task.dependencies.map(depId => this.runningProcesses.get(depId)));
  }
}
