/**
 * Erlang/AtomVM Gateway (JavaScript Implementation)
 *
 * Implements C3 diagram components:
 * - HTTP Frontend
 * - IntentRouter
 * - GuardServer
 * - TaskQueue
 * - WorkerBridge
 *
 * Uses existing AtomVM pattern from @unrdf/kgc-4d
 * Big Bang 80/20: Reuse existing patterns, minimal new code
 */

import { EventEmitter } from 'node:events';

/**
 * HTTP Frontend - Accepts decision intents from chair portal
 */
class HttpFrontend extends EventEmitter {
  constructor() {
    super();
    this.requests = [];
  }

  /**
   * Accept POST /decision_intent
   * @param {Object} payload - Decision intent from chairperson
   * @returns {Promise<Object>} Response with request ID
   */
  async handleDecisionIntent(payload) {
    const requestId = `req_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`;

    const request = {
      requestId,
      timestamp: Date.now(),
      payload,
      status: 'received',
    };

    this.requests.push(request);
    this.emit('intent', request);

    return { requestId, status: 'accepted' };
  }

  /**
   * GET /status/:requestId
   */
  getStatus(requestId) {
    return this.requests.find(r => r.requestId === requestId);
  }
}

/**
 * IntentRouter - Classifies decision intents
 */
class IntentRouter extends EventEmitter {
  constructor() {
    super();
    this.routes = new Map();
  }

  /**
   * Route decision intent to appropriate handler
   * @param {Object} request - Request from HTTP frontend
   */
  route(request) {
    const { payload } = request;

    // Classify intent type
    const intentType = this.classifyIntent(payload);

    const routed = {
      ...request,
      intentType,
      routedAt: Date.now(),
    };

    this.emit('routed', routed);
    return routed;
  }

  /**
   * Classify intent based on payload structure
   */
  classifyIntent(payload) {
    if (payload.type === 'new_decision') return 'NEW_DECISION';
    if (payload.type === 'scenario_request') return 'SCENARIO';
    if (payload.type === 'update') return 'UPDATE';
    if (payload.type === 'query') return 'QUERY';
    return 'UNKNOWN';
  }
}

/**
 * GuardServer - Applies fast guards and constraints
 *
 * Implements μ invariants:
 * - Capital constraints
 * - Risk thresholds
 * - Regulatory limits
 * - Authorization checks
 */
class GuardServer extends EventEmitter {
  constructor(constraints = {}) {
    super();
    this.constraints = {
      maxCapital: constraints.maxCapital || 1e12, // $1T default
      minRating: constraints.minRating || 'BBB',
      maxRisk: constraints.maxRisk || 0.05, // 5% VaR
      ...constraints,
    };
    this.guardResults = [];
  }

  /**
   * Apply guards to routed intent
   * @param {Object} routed - Routed request from IntentRouter
   * @returns {Object} Guard result with allowed/denied status
   */
  async applyGuards(routed) {
    const { payload } = routed;

    const checks = [
      this.checkCapitalConstraint(payload),
      this.checkRiskConstraint(payload),
      this.checkAuthorization(payload),
    ];

    const results = await Promise.all(checks);
    const allowed = results.every(r => r.passed);

    const guardResult = {
      requestId: routed.requestId,
      allowed,
      checks: results,
      timestamp: Date.now(),
      violatedConstraints: results.filter(r => !r.passed).map(r => r.constraint),
    };

    this.guardResults.push(guardResult);

    if (allowed) {
      this.emit('allowed', { ...routed, guardResult });
    } else {
      this.emit('denied', { ...routed, guardResult });
    }

    return guardResult;
  }

  /**
   * Check capital constraint (μ invariant)
   */
  checkCapitalConstraint(payload) {
    const requested = payload.capital || 0;
    const passed = requested <= this.constraints.maxCapital;

    return {
      constraint: 'capital',
      passed,
      requested,
      limit: this.constraints.maxCapital,
      message: passed
        ? 'Capital within bounds'
        : `Capital ${requested} exceeds limit ${this.constraints.maxCapital}`,
    };
  }

  /**
   * Check risk constraint (μ invariant)
   */
  checkRiskConstraint(payload) {
    const risk = payload.estimatedRisk || 0;
    const passed = risk <= this.constraints.maxRisk;

    return {
      constraint: 'risk',
      passed,
      risk,
      limit: this.constraints.maxRisk,
      message: passed
        ? 'Risk within threshold'
        : `Risk ${risk} exceeds threshold ${this.constraints.maxRisk}`,
    };
  }

  /**
   * Check authorization (μ invariant)
   */
  checkAuthorization(payload) {
    // In real system: check user permissions, board authority, etc.
    const authorized = payload.authorizedBy === 'chairperson';

    return {
      constraint: 'authorization',
      passed: authorized,
      authorizedBy: payload.authorizedBy,
      message: authorized ? 'Authorized' : 'Not authorized',
    };
  }
}

/**
 * TaskQueue - Queues validated decision tasks
 *
 * Implements Erlang-style process queue with ordering guarantees
 */
class TaskQueue extends EventEmitter {
  constructor() {
    super();
    this.queue = [];
    this.processing = new Set();
    this.completed = new Map();
  }

  /**
   * Enqueue validated task
   */
  enqueue(task) {
    const queuedTask = {
      ...task,
      queuedAt: Date.now(),
      status: 'queued',
    };

    this.queue.push(queuedTask);
    this.emit('enqueued', queuedTask);

    return queuedTask;
  }

  /**
   * Dequeue next task for processing
   */
  dequeue() {
    if (this.queue.length === 0) return null;

    const task = this.queue.shift();
    task.status = 'processing';
    task.dequeuedAt = Date.now();

    this.processing.add(task.requestId);
    this.emit('dequeued', task);

    return task;
  }

  /**
   * Mark task as complete
   */
  complete(requestId, result) {
    this.processing.delete(requestId);
    this.completed.set(requestId, {
      requestId,
      result,
      completedAt: Date.now(),
    });

    this.emit('completed', { requestId, result });
  }

  /**
   * Get queue stats
   */
  stats() {
    return {
      queued: this.queue.length,
      processing: this.processing.size,
      completed: this.completed.size,
    };
  }
}

/**
 * WorkerBridge - Bridges Erlang messages to worker cluster
 *
 * In real system: NATS/Kafka/gRPC
 * Here: EventEmitter for in-process communication
 */
class WorkerBridge extends EventEmitter {
  constructor() {
    super();
    this.workers = new Map();
    this.dispatched = new Map();
  }

  /**
   * Register worker
   */
  registerWorker(workerId, worker) {
    this.workers.set(workerId, worker);
  }

  /**
   * Dispatch task to worker
   */
  async dispatch(task) {
    // Round-robin worker selection
    const workers = Array.from(this.workers.values());
    if (workers.length === 0) {
      throw new Error('No workers available');
    }

    const worker = workers[this.dispatched.size % workers.length];

    const dispatch = {
      taskId: task.requestId,
      task,
      workerId: worker.workerId,
      dispatchedAt: Date.now(),
    };

    this.dispatched.set(task.requestId, dispatch);
    this.emit('dispatched', dispatch);

    // Send task to worker
    const result = await worker.processTask(task);

    this.emit('completed', { taskId: task.requestId, result });
    return result;
  }

  /**
   * Get bridge stats
   */
  stats() {
    return {
      workers: this.workers.size,
      dispatched: this.dispatched.size,
    };
  }
}

/**
 * ErlangGateway - Main gateway orchestrator
 *
 * Wires together all C3 components following Erlang supervision pattern
 */
export class ErlangGateway extends EventEmitter {
  constructor(constraints = {}) {
    super();

    // Initialize components
    this.httpFrontend = new HttpFrontend();
    this.intentRouter = new IntentRouter();
    this.guardServer = new GuardServer(constraints);
    this.taskQueue = new TaskQueue();
    this.workerBridge = new WorkerBridge();

    // Wire components together (Erlang-style supervision tree)
    this.wireComponents();
  }

  /**
   * Wire components into processing pipeline
   */
  wireComponents() {
    // HTTP → Router
    this.httpFrontend.on('intent', request => {
      const routed = this.intentRouter.route(request);
      // Router → Guards (async, don't await here)
      this.guardServer.applyGuards(routed);
    });

    // Guards → Queue (if allowed)
    this.guardServer.on('allowed', task => {
      this.taskQueue.enqueue(task);
    });

    // Guards → Deny (if not allowed)
    this.guardServer.on('denied', ({ requestId, guardResult }) => {
      this.emit('denied', { requestId, guardResult });
    });

    // Queue → Bridge (worker dispatches task when ready)
    this.taskQueue.on('enqueued', () => {
      this.emit('task-queued', this.taskQueue.stats());
    });
  }

  /**
   * Public API: Submit decision intent (from chairperson portal)
   */
  async submitDecisionIntent(payload) {
    const response = await this.httpFrontend.handleDecisionIntent(payload);
    return response;
  }

  /**
   * Public API: Get status
   */
  getStatus(requestId) {
    return this.httpFrontend.getStatus(requestId);
  }

  /**
   * Public API: Register worker
   */
  registerWorker(workerId, worker) {
    this.workerBridge.registerWorker(workerId, worker);
  }

  /**
   * Public API: Dispatch next task to worker
   */
  async dispatchNextTask() {
    const task = this.taskQueue.dequeue();
    if (!task) return null;

    const result = await this.workerBridge.dispatch(task);
    this.taskQueue.complete(task.requestId, result);

    return { task, result };
  }

  /**
   * Get gateway stats
   */
  stats() {
    return {
      http: {
        requests: this.httpFrontend.requests.length,
      },
      guards: {
        results: this.guardServer.guardResults.length,
      },
      queue: this.taskQueue.stats(),
      workers: this.workerBridge.stats(),
    };
  }
}
