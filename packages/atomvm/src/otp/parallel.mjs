import { Result } from './result.mjs';

export const Parallel = Object.freeze({
  async all(tasks) {
    if (!Array.isArray(tasks)) throw new TypeError('tasks must be an array');
    const controller = new AbortController();
    let failed = false;
    const promises = tasks.map((task, index) => Promise.resolve().then(async () => {
      if (typeof task !== 'function') throw new TypeError(`task ${index} must be a function`);
      if (controller.signal.aborted) throw controller.signal.reason;
      return task(controller.signal, index);
    }).catch(error => {
      if (!failed) {
        failed = true;
        controller.abort(error);
      }
      throw error;
    }));

    try {
      return Result.ok(await Promise.all(promises));
    } catch (error) {
      await Promise.allSettled(promises);
      return Result.err(error);
    }
  },
});
