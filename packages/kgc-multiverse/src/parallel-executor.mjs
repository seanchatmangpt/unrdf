  /**
   * Trigger GC if available
   * @private
   */
  _gcHint() {
    if (typeof global !== 'undefined' && global.gc) {
      global.gc();
    }
  }

  /**
   * Run a single task in the worker pool
   *
   * @param {Object} task - Task configuration
   * @param {Object} worker - Worker instance
   * @returns {Promise<Object>}
   * @private
   */
  async _runTask(task, worker) {
    const { id, type, payload } = task;
    const start = typeof performance !== 'undefined' ? performance.now() : Date.now();
    
    try {
      const result = await worker.run(type, payload);
      const duration = (typeof performance !== 'undefined' ? performance.now() : Date.now()) - start;
      return { id, result, duration };
    } catch (error) {
      throw error;
    }
  }
