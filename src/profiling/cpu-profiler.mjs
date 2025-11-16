/**
 * @fileoverview CPU Profiler for Node.js
 * @module profiling/cpu-profiler
 */

import { Session } from 'node:inspector';
import { writeFileSync, mkdirSync } from 'node:fs';
import { join } from 'node:path';

/**
 * @typedef {Object} CpuSession
 * @property {string} id - Session ID
 * @property {string} operationName - Operation name
 * @property {number} startTime - Start timestamp
 * @property {Session} inspector - Inspector session
 * @property {Object[]} events - Profile events
 */

/**
 * @typedef {Object} CpuMetrics
 * @property {number} totalTime - Total CPU time in microseconds
 * @property {Object[]} hotFunctions - Hot functions analysis
 * @property {number} sampleCount - Number of samples
 * @property {Object} profile - Raw V8 CPU profile
 * @property {string} [profilePath] - Path to saved profile
 */

/**
 * CPU Profiler using V8 Inspector
 * Note: Only works in Node.js environment
 */
export class CpuProfiler {
  constructor(options = {}) {
    this.options = {
      sampleInterval: 100, // microseconds
      saveProfiles: false,
      profileDir: '.profiles',
      ...options
    };

    this.activeSessions = new Map();

    if (this.options.saveProfiles) {
      try {
        mkdirSync(this.options.profileDir, { recursive: true });
      } catch (err) {
        console.warn('Failed to create profile directory:', err.message);
      }
    }
  }

  /**
   * Check if CPU profiling is available
   * @returns {boolean}
   */
  static isAvailable() {
    try {
      return typeof Session !== 'undefined';
    } catch {
      return false;
    }
  }

  /**
   * Start CPU profiling
   * @param {string} operationName - Operation name
   * @returns {Promise<string>} Session ID
   */
  async start(operationName) {
    if (!CpuProfiler.isAvailable()) {
      throw new Error('CPU profiling not available in this environment');
    }

    const sessionId = `cpu-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    const inspector = new Session();

    inspector.connect();

    // Enable profiler
    await this.post(inspector, 'Profiler.enable');

    // Start profiling
    await this.post(inspector, 'Profiler.start', {
      interval: this.options.sampleInterval
    });

    const session = {
      id: sessionId,
      operationName,
      startTime: Date.now(),
      inspector,
      events: []
    };

    this.activeSessions.set(sessionId, session);
    return sessionId;
  }

  /**
   * Stop CPU profiling and return metrics
   * @param {string} sessionId - Session ID
   * @returns {Promise<CpuMetrics>}
   */
  async stop(sessionId) {
    const session = this.activeSessions.get(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    try {
      // Stop profiling and get profile
      const { profile } = await this.post(session.inspector, 'Profiler.stop');

      // Disable profiler
      await this.post(session.inspector, 'Profiler.disable');

      // Disconnect inspector
      session.inspector.disconnect();

      // Analyze profile
      const metrics = this.analyzeProfile(profile, session);

      // Save profile if enabled
      if (this.options.saveProfiles) {
        metrics.profilePath = this.saveProfile(profile, session);
      }

      // Cleanup
      this.activeSessions.delete(sessionId);

      return metrics;
    } catch (err) {
      session.inspector.disconnect();
      this.activeSessions.delete(sessionId);
      throw err;
    }
  }

  /**
   * Send command to inspector
   * @private
   * @param {Session} inspector - Inspector session
   * @param {string} method - Method name
   * @param {Object} [params={}] - Parameters
   * @returns {Promise<any>}
   */
  post(inspector, method, params = {}) {
    return new Promise((resolve, reject) => {
      inspector.post(method, params, (err, result) => {
        if (err) reject(err);
        else resolve(result);
      });
    });
  }

  /**
   * Analyze CPU profile
   * @private
   * @param {Object} profile - V8 CPU profile
   * @param {CpuSession} session - Session data
   * @returns {CpuMetrics}
   */
  analyzeProfile(profile, session) {
    const nodes = profile.nodes || [];
    const samples = profile.samples || [];
    const timeDeltas = profile.timeDeltas || [];

    // Calculate total time
    const totalTime = timeDeltas.reduce((sum, delta) => sum + delta, 0);

    // Build function execution times
    const functionTimes = new Map();

    samples.forEach((nodeId, index) => {
      const node = nodes.find(n => n.id === nodeId);
      if (node) {
        const functionName = this.getFunctionName(node);
        const time = timeDeltas[index] || 0;

        const current = functionTimes.get(functionName) || {
          name: functionName,
          selfTime: 0,
          totalTime: 0,
          hitCount: 0,
          url: node.callFrame?.url || 'unknown'
        };

        current.selfTime += time;
        current.hitCount += 1;

        functionTimes.set(functionName, current);
      }
    });

    // Sort by self time
    const hotFunctions = Array.from(functionTimes.values())
      .sort((a, b) => b.selfTime - a.selfTime)
      .slice(0, 20) // Top 20 functions
      .map(fn => ({
        name: fn.name,
        selfTime: fn.selfTime,
        selfTimePercent: (fn.selfTime / totalTime) * 100,
        hitCount: fn.hitCount,
        url: fn.url
      }));

    return {
      totalTime,
      sampleCount: samples.length,
      hotFunctions,
      profile,
      operationName: session.operationName,
      duration: Date.now() - session.startTime
    };
  }

  /**
   * Get function name from node
   * @private
   * @param {Object} node - Profile node
   * @returns {string}
   */
  getFunctionName(node) {
    const callFrame = node.callFrame || {};
    const functionName = callFrame.functionName || '(anonymous)';
    const url = callFrame.url || '';
    const lineNumber = callFrame.lineNumber || 0;

    if (url && lineNumber) {
      return `${functionName} (${url}:${lineNumber})`;
    }

    return functionName;
  }

  /**
   * Save profile to disk
   * @private
   * @param {Object} profile - V8 CPU profile
   * @param {CpuSession} session - Session data
   * @returns {string} Profile file path
   */
  saveProfile(profile, session) {
    const timestamp = new Date().toISOString().replace(/:/g, '-');
    const filename = `cpu-${session.operationName}-${timestamp}.cpuprofile`;
    const filepath = join(this.options.profileDir, filename);

    try {
      writeFileSync(filepath, JSON.stringify(profile, null, 2));
      return filepath;
    } catch (err) {
      console.warn('Failed to save CPU profile:', err.message);
      return null;
    }
  }

  /**
   * Profile a function
   * @param {string} operationName - Operation name
   * @param {Function} fn - Function to profile
   * @returns {Promise<Object>} Result and metrics
   */
  async profile(operationName, fn) {
    const sessionId = await this.start(operationName);

    let result;
    let error;

    try {
      result = await fn();
    } catch (err) {
      error = err;
    }

    const metrics = await this.stop(sessionId);

    if (error) {
      throw error;
    }

    return { result, metrics };
  }

  /**
   * Get hot functions summary
   * @param {CpuMetrics} metrics - CPU metrics
   * @param {number} [limit=10] - Number of functions to return
   * @returns {Object[]} Hot functions
   */
  getHotFunctions(metrics, limit = 10) {
    return metrics.hotFunctions.slice(0, limit);
  }

  /**
   * Check if CPU time exceeds budget
   * @param {CpuMetrics} metrics - CPU metrics
   * @param {Object} budget - CPU budget
   * @param {number} [budget.maxTotalTime] - Max total time in microseconds
   * @param {number} [budget.maxHotFunctionTime] - Max time for any single function
   * @returns {Object} Budget check results
   */
  checkBudget(metrics, budget) {
    const violations = [];

    if (budget.maxTotalTime !== undefined &&
        metrics.totalTime > budget.maxTotalTime) {
      violations.push({
        metric: 'totalTime',
        actual: metrics.totalTime,
        budget: budget.maxTotalTime,
        exceeded: metrics.totalTime - budget.maxTotalTime
      });
    }

    if (budget.maxHotFunctionTime !== undefined && metrics.hotFunctions.length > 0) {
      const maxFunctionTime = metrics.hotFunctions[0].selfTime;
      if (maxFunctionTime > budget.maxHotFunctionTime) {
        violations.push({
          metric: 'hotFunctionTime',
          actual: maxFunctionTime,
          budget: budget.maxHotFunctionTime,
          exceeded: maxFunctionTime - budget.maxHotFunctionTime,
          function: metrics.hotFunctions[0].name
        });
      }
    }

    return {
      passed: violations.length === 0,
      violations
    };
  }
}

/**
 * Quick CPU measurement (if available)
 * @param {string} operationName - Operation name
 * @param {Function} fn - Function to measure
 * @returns {Promise<Object>} Result and CPU metrics
 */
export async function measureCpu(operationName, fn) {
  if (!CpuProfiler.isAvailable()) {
    console.warn('CPU profiling not available - skipping');
    const result = await fn();
    return { result, metrics: null };
  }

  const profiler = new CpuProfiler();
  return await profiler.profile(operationName, fn);
}
