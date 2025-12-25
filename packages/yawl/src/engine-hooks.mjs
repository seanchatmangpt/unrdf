/**
 * @file YAWL Engine Hooks - Policy pack integration and hook execution
 * @module @unrdf/yawl/engine-hooks
 */

/**
 * Mixin that adds hook and policy pack capabilities
 *
 * Provides:
 * - Policy pack registration
 * - Hook execution (validators, routers, cancellation handlers)
 * - Policy enforcement during task lifecycle
 *
 * @param {class} Base - Base class to extend
 * @returns {class} Extended class with hook capabilities
 */
export function withHooks(Base) {
  return class EngineHooks extends Base {
    // =========================================================================
    // Policy Pack Integration
    // =========================================================================

    /**
     * Register a policy pack for a workflow
     * @param {string} workflowId - Workflow ID
     * @param {Object} policyPack - Policy pack from createYAWLPolicyPack
     * @returns {void}
     */
    registerPolicyPack(workflowId, policyPack) {
      if (!this.workflows.has(workflowId)) {
        throw new Error(`Workflow ${workflowId} not found`);
      }
      this._policyPacks.set(workflowId, policyPack);
    }

    /**
     * Get policy pack for a workflow
     * @param {string} workflowId - Workflow ID
     * @returns {Object|undefined} Policy pack if registered
     */
    getPolicyPack(workflowId) {
      return this._policyPacks.get(workflowId);
    }

    // =========================================================================
    // Hook Execution Helpers
    // =========================================================================

    /**
     * Execute pre-enablement validation hook
     * @param {string} workflowId - Workflow ID
     * @param {string} taskId - Task ID
     * @param {Object} context - Execution context
     * @returns {Promise<{valid: boolean, receipt?: Object}>}
     * @protected
     */
    async _executeValidationHook(workflowId, taskId, context) {
      const policyPack = this._policyPacks.get(workflowId);
      if (!policyPack || !policyPack.getValidator) {
        return { valid: true };
      }

      const validator = policyPack.getValidator(taskId);
      if (!validator) {
        return { valid: true };
      }

      const validation = await validator(this.store, context);
      return validation;
    }

    /**
     * Execute post-completion routing hook
     * @param {string} workflowId - Workflow ID
     * @param {string} taskId - Task ID
     * @param {Object} context - Execution context
     * @returns {Promise<Object|null>} Routing result with receipt
     * @protected
     */
    async _executeRoutingHook(workflowId, taskId, context) {
      const policyPack = this._policyPacks.get(workflowId);
      if (!policyPack || !policyPack.getRouter) {
        return null;
      }

      const router = policyPack.getRouter(taskId);
      if (!router) {
        return null;
      }

      const routing = await router(this.store, context);
      return routing;
    }

    /**
     * Execute cancellation handler hook
     * @param {string} workflowId - Workflow ID
     * @param {string} taskId - Task ID
     * @param {string} reason - Cancellation reason
     * @param {Object} context - Execution context
     * @protected
     */
    _executeCancellationHook(workflowId, taskId, reason, context) {
      const policyPack = this._policyPacks.get(workflowId);
      if (!policyPack || !policyPack.getCancellationHandler) {
        return;
      }

      const handler = policyPack.getCancellationHandler(taskId);
      if (!handler) {
        return;
      }

      handler(reason, context);
    }
  };
}
