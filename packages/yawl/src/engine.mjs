/**
 * @file YAWL Engine - Main workflow execution engine with KGC-4D integration
 * @module @unrdf/yawl/engine
 *
 * @description
 * Barrel export that composes all engine modules into a unified API.
 * Split architecture:
 * - engine-core.mjs: Workflow/case management, health, stats
 * - engine-execution.mjs: Task execution and work items
 * - engine-hooks.mjs: Policy packs, KGC-4D, circuit breakers
 * - engine-coordination.mjs: Events, time-travel, resources
 */

// Import all module functions
import * as Core from './engine-core.mjs';
import * as Execution from './engine-execution.mjs';
import * as Hooks from './engine-hooks.mjs';
import * as Coordination from './engine-coordination.mjs';

// Re-export constants
export { ENGINE_EVENTS, YAWL_NS, YAWL_GRAPHS, HealthStatus } from './engine-constants.mjs';

// =============================================================================
// WorkflowEngine Class
// =============================================================================

/**
 * Main YAWL workflow engine with KGC-4D time-travel support
 */
export class WorkflowEngine {
  constructor(config = {}) {
    // Initialize state using core module
    Object.assign(this, Core.initializeEngineState(config));

    // Bind emit method so it's available for other modules
    this.emit = (eventType, data) => Coordination.emit(this, eventType, data);

    // Initialize snapshot timer if enabled
    if (this.enableSnapshots && this.git) {
      Coordination.startSnapshotTimer(this);
    }
  }

  // Workflow Management
  registerWorkflow(workflowOrSpec) {
    return Core.registerWorkflow(this, workflowOrSpec);
  }
  async loadWorkflow(workflowId) {
    return Core.loadWorkflow(this, workflowId);
  }
  getWorkflow(workflowId) {
    return Core.getWorkflow(this, workflowId);
  }
  getAllWorkflows() {
    return Core.getAllWorkflows(this);
  }

  // Policy Pack Integration
  registerPolicyPack(workflowId, policyPack) {
    return Hooks.registerPolicyPack(this, workflowId, policyPack);
  }
  getPolicyPack(workflowId) {
    return Hooks.getPolicyPack(this, workflowId);
  }

  // Case Management
  async createCase(workflowId, initialData = {}, options = {}) {
    return Core.createCase(this, workflowId, initialData, options);
  }
  getCase(caseId) {
    return Core.getCase(this, caseId);
  }
  getCaseStatus(caseId) {
    return Core.getCaseStatus(this, caseId);
  }
  getActiveCases() {
    return Core.getActiveCases(this);
  }
  getCasesForWorkflow(workflowId) {
    return Core.getCasesForWorkflow(this, workflowId);
  }

  // Task Execution
  async enableTask(caseId, taskId, actor) {
    return Execution.enableTask(this, caseId, taskId, actor);
  }
  async startTask(caseId, workItemId, options = {}) {
    return Execution.startTask(this, caseId, workItemId, options);
  }
  async completeTask(caseId, workItemId, output = {}, actor) {
    return Execution.completeTask(this, caseId, workItemId, output, actor);
  }
  async cancelTask(caseId, workItemId, reason, actor) {
    return Execution.cancelTask(this, caseId, workItemId, reason, actor);
  }
  async cancelRegion(caseId, regionId, reason, actor) {
    return Execution.cancelRegion(this, caseId, regionId, reason, actor);
  }
  async setCircuitBreaker(caseId, taskId, enabled) {
    return Execution.setCircuitBreaker(this, caseId, taskId, enabled);
  }
  async timeoutTask(caseId, workItemId) {
    return Execution.timeoutTask(this, caseId, workItemId);
  }

  // Backward compatibility aliases
  async startWorkItem(caseId, workItemId, options = {}) {
    return Execution.startTask(this, caseId, workItemId, options);
  }
  async completeWorkItem(caseId, workItemId, output = {}, actor) {
    return Execution.completeTask(this, caseId, workItemId, output, actor);
  }
  async cancelWorkItem(caseId, workItemId, reason, actor) {
    return Execution.cancelTask(this, caseId, workItemId, reason, actor);
  }
  async timeoutWorkItem(caseId, workItemId) {
    return Execution.timeoutTask(this, caseId, workItemId);
  }
  async reconstructCase(caseId, targetTime) {
    return Coordination.replayCase(this, caseId, targetTime);
  }

  // Time-Travel
  async checkpoint(label) {
    return Coordination.checkpoint(this, label);
  }
  async replayCase(caseId, targetTime) {
    return Coordination.replayCase(this, caseId, targetTime);
  }
  async getCaseHistory(caseId) {
    return Coordination.getCaseHistory(this, caseId);
  }
  async replayToReceipt(caseId, receiptId) {
    return Coordination.replayToReceipt(this, caseId, receiptId);
  }

  // Resource Management
  addResource(resourceData) {
    return Coordination.addResource(this, resourceData);
  }
  removeResource(resourceId) {
    return Coordination.removeResource(this, resourceId);
  }
  getResourceStats() {
    return Coordination.getResourceStats(this);
  }

  // Event Subscription
  on(eventType, handler) {
    return Coordination.on(this, eventType, handler);
  }
  off(eventType) {
    return Coordination.off(this, eventType);
  }

  // Health & Statistics
  healthCheck() {
    return Core.healthCheck(this);
  }
  getStats() {
    return Core.getStats(this);
  }

  // Event Log
  getEventsForCase(caseId) {
    return Core.getEventsForCase(this, caseId);
  }
  getAllEvents() {
    return Core.getAllEvents(this);
  }

  // Serialization
  toJSON() {
    return Core.serializeEngine(this);
  }

  shutdown() {
    return Coordination.shutdown(this);
  }
}

// =============================================================================
// Backward Compatibility & Exports
// =============================================================================

export const YawlEngine = WorkflowEngine;

export function createWorkflowEngine(config = {}) {
  return new WorkflowEngine(config);
}

export default {
  WorkflowEngine,
  YawlEngine,
  createWorkflowEngine,
};
