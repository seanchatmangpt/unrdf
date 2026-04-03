/**
 * Job Queue System (BullMQ-compatible)
 * Distributed task processing with retries, scheduling, priorities
 */

export class JobQueue {
  constructor() {
    this.queues = new Map();
    this.jobs = new Map();
    this.workers = new Map();
  }

  /**
   * Create queue
   */
  createQueue(name, options = {}) {
    const queue = {
      name,
      concurrency: options.concurrency || 5,
      maxRetries: options.maxRetries || 3,
      timeout: options.timeout || 30000,
      jobs: [],
      processing: new Set(),
      completed: [],
      failed: [],
      created: new Date()
    };
    this.queues.set(name, queue);
    return queue;
  }

  /**
   * Add job to queue
   */
  async add(queueName, jobName, data, options = {}) {
    const queue = this.queues.get(queueName);
    if (!queue) throw new Error(`Queue not found: ${queueName}`);

    const jobId = `job_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    const job = {
      id: jobId,
      name: jobName,
      queueName,
      data,
      state: 'pending',
      attempts: 0,
      maxAttempts: options.maxAttempts || 3,
      priority: options.priority || 0,
      delay: options.delay || 0,
      scheduled: Date.now() + (options.delay || 0),
      created: new Date()
    };

    queue.jobs.push(job);
    this.jobs.set(jobId, job);

    return { jobId, queueName, jobName };
  }

  /**
   * Register job processor (worker)
   */
  registerWorker(queueName, processor) {
    if (!this.queues.has(queueName)) {
      throw new Error(`Queue not found: ${queueName}`);
    }

    const worker = {
      id: `worker_${Date.now()}_${Math.random().toString(36).substring(7)}`,
      queueName,
      processor,
      processing: null,
      processedCount: 0
    };

    this.workers.set(worker.id, worker);
    this._startWorker(worker);

    return worker.id;
  }

  _startWorker(worker) {
    const queue = this.queues.get(worker.queueName);
    const process = async () => {
      if (queue.processing.size >= queue.concurrency) {
        setTimeout(() => process(), 1000);
        return;
      }

      const job = queue.jobs
        .filter(j => j.state === 'pending' && j.scheduled <= Date.now())
        .sort((a, b) => b.priority - a.priority)[0];

      if (!job) {
        setTimeout(() => process(), 1000);
        return;
      }

      queue.processing.add(job.id);
      job.state = 'processing';
      job.attempts++;

      try {
        const result = await worker.processor(job);
        job.state = 'completed';
        job.result = result;
        job.completedAt = new Date();
        queue.completed.push(job);
        worker.processedCount++;
      } catch (err) {
        if (job.attempts >= job.maxAttempts) {
          job.state = 'failed';
          job.error = err.message;
          queue.failed.push(job);
        } else {
          job.state = 'pending';
          job.scheduled = Date.now() + Math.pow(2, job.attempts) * 1000;
        }
      } finally {
        queue.processing.delete(job.id);
        process();
      }
    };

    process();
  }

  /**
   * Get queue stats
   */
  getQueueStats(queueName) {
    const queue = this.queues.get(queueName);
    if (!queue) return null;

    return {
      name: queue.name,
      pending: queue.jobs.filter(j => j.state === 'pending').length,
      processing: queue.processing.size,
      completed: queue.completed.length,
      failed: queue.failed.length,
      workers: Array.from(this.workers.values()).filter(w => w.queueName === queueName).length
    };
  }
}

export default JobQueue;
