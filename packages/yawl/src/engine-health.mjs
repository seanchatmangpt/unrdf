/**
 * @file YAWL Engine Health - Health checks, circuit breakers, and monitoring
 * @module @unrdf/yawl/engine-health
 */

import { now } from '@unrdf/kgc-4d';
import { CaseStatus } from './case.mjs';
import { HealthStatus, ENGINE_EVENTS } from './engine-core.mjs';

/**
 * Mixin that adds health monitoring and circuit breaker capabilities
 *
 * Provides:
 * - Health checks
 * - Circuit breaker pattern
 * - Engine statistics
 * - Performance monitoring
 *
 * @param {class} Base - Base class to extend
 * @returns {class} Extended class with health capabilities
 */
export function withHealth(Base) {
  return class EngineHealth extends Base {
    // =========================================================================
    // Health Checks
    // =========================================================================

    /**
     * Perform health check
     * @returns {Object} Health status
     */
    healthCheck() {
      const errors = [];

      // Check store
      let storeHealthy = true;
      try {
        // Simple store check - try to get size
        this.store.size;
      } catch (e) {
        storeHealthy = false;
        errors.push(`Store error: ${e.message}`);
      }

      // Check Git backbone if configured
      let gitHealthy = null;
      if (this.git) {
        gitHealthy = true; // Would add actual Git health check here
      }

      // Check workflow count
      const workflowHealthy = this.workflows.size > 0 || true;

      // Check case capacity
      const casesHealthy = this.cases.size < this.maxConcurrentCases;
      if (!casesHealthy) {
        errors.push(`Case capacity reached: ${this.cases.size}/${this.maxConcurrentCases}`);
      }

      // Determine overall status
      let status = HealthStatus.HEALTHY;
      if (!storeHealthy) {
        status = HealthStatus.UNHEALTHY;
      } else if (!casesHealthy || errors.length > 0) {
        status = HealthStatus.DEGRADED;
      }

      this._health = {
        status,
        lastCheck: now(),
        components: {
          store: storeHealthy,
          git: gitHealthy,
          workflows: workflowHealthy,
          cases: casesHealthy,
        },
        errors,
        uptime: now() - this._stats.startedAt,
      };

      return this._health;
    }

    /**
     * Get engine statistics
     * @returns {Object} Engine stats
     */
    getStats() {
      return {
        ...this._stats,
        uptimeNs: (now() - this._stats.startedAt).toString(),
        uptimeMs: Number(now() - this._stats.startedAt) / 1_000_000,
        workflowCount: this.workflows.size,
        activeCaseCount: [...this.cases.values()].filter(
          c => c.status === CaseStatus.RUNNING
        ).length,
        totalCaseCount: this.cases.size,
        checkpointCount: this.checkpoints.size,
        eventCount: this.events.length,
        resourceStats: this.resourcePool.getStats(),
        circuitBreakers: Object.fromEntries(this._circuitBreakers),
      };
    }

    // =========================================================================
    // Circuit Breaker
    // =========================================================================

    /**
     * Check if circuit breaker is open
     * @param {string} key - Circuit breaker key
     * @returns {boolean}
     * @protected
     */
    _isCircuitOpen(key) {
      const breaker = this._circuitBreakers.get(key);
      if (!breaker) return false;

      if (breaker.state === 'open') {
        // Check if reset timeout has passed
        const elapsed = Number(now() - breaker.openedAt) / 1_000_000;
        if (elapsed >= this.circuitBreakerResetTimeout) {
          breaker.state = 'half-open';
          return false;
        }
        return true;
      }

      return false;
    }

    /**
     * Record a circuit breaker failure
     * @param {string} key - Circuit breaker key
     * @protected
     */
    _recordCircuitFailure(key) {
      let breaker = this._circuitBreakers.get(key);
      if (!breaker) {
        breaker = { failures: 0, state: 'closed', openedAt: null };
        this._circuitBreakers.set(key, breaker);
      }

      breaker.failures++;

      if (breaker.failures >= this.circuitBreakerThreshold) {
        breaker.state = 'open';
        breaker.openedAt = now();
        this._stats.circuitBreakerTrips++;

        this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_OPEN, {
          key,
          failures: breaker.failures,
        });
      }
    }

    /**
     * Reset circuit breaker on success
     * @param {string} key - Circuit breaker key
     * @protected
     */
    _resetCircuitBreaker(key) {
      const breaker = this._circuitBreakers.get(key);
      if (breaker) {
        const wasOpen = breaker.state !== 'closed';
        breaker.failures = 0;
        breaker.state = 'closed';
        breaker.openedAt = null;

        if (wasOpen) {
          this.emit(ENGINE_EVENTS.CIRCUIT_BREAKER_CLOSE, { key });
        }
      }
    }
  };
}
