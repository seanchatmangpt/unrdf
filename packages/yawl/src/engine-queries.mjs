/**
 * @file YAWL Engine Queries - Case and workflow query capabilities
 * @module @unrdf/yawl/engine-queries
 */

import { CaseStatus } from './case.mjs';
import { EngineError } from '../errors.mjs';

/**
 * Mixin that adds query and search capabilities
 *
 * Provides:
 * - Case status queries
 * - Active case filtering
 * - Workflow-specific case queries
 * - Case state retrieval
 *
 * @param {class} Base - Base class to extend
 * @returns {class} Extended class with query capabilities
 */
export function withQueries(Base) {
  return class EngineQueries extends Base {
    // =========================================================================
    // Case Queries
    // =========================================================================

    /**
     * Get a case by ID
     * @param {string} caseId - Case ID
     * @returns {YawlCase|undefined} Case if found
     */
    getCase(caseId) {
      return this.cases.get(caseId);
    }

    /**
     * Get case status
     * @param {string} caseId - Case ID
     * @returns {Object} Case status details
     * @throws {Error} If case not found
     */
    getCaseStatus(caseId) {
      const yawlCase = this.cases.get(caseId);
      if (!yawlCase) {
        throw new Error(`Case ${caseId} not found`);
      }

      return {
        caseId,
        workflowId: yawlCase.workflowId,
        status: yawlCase.status,
        createdAt: yawlCase.createdAt?.toString(),
        startedAt: yawlCase.startedAt?.toString(),
        completedAt: yawlCase.completedAt?.toString(),
        enabledWorkItems: yawlCase.getEnabledWorkItems().length,
        runningWorkItems: yawlCase.getRunningWorkItems().length,
        completedTasks: [...yawlCase.completedTasks],
        receiptCount: yawlCase.receipts.length,
      };
    }

    /**
     * Get all active cases
     * @returns {YawlCase[]} Array of active cases
     */
    getActiveCases() {
      return [...this.cases.values()].filter(
        c => c.status === CaseStatus.RUNNING
      );
    }

    /**
     * Get all cases for a workflow
     * @param {string} workflowId - Workflow ID
     * @returns {YawlCase[]} Array of cases
     */
    getCasesForWorkflow(workflowId) {
      return [...this.cases.values()].filter(
        c => c.workflowId === workflowId
      );
    }

    /**
     * Get cases by status
     * @param {string} status - Case status (from CaseStatus)
     * @returns {YawlCase[]} Array of cases with matching status
     */
    getCasesByStatus(status) {
      return [...this.cases.values()].filter(
        c => c.status === status
      );
    }

    /**
     * Search cases by predicate
     * @param {Function} predicate - Filter function (case) => boolean
     * @returns {YawlCase[]} Array of matching cases
     */
    searchCases(predicate) {
      if (typeof predicate !== 'function') {
        throw new TypeError('Predicate must be a function');
      }
      return [...this.cases.values()].filter(predicate);
    }

    // =========================================================================
    // Statistics and Aggregations
    // =========================================================================

    /**
     * Get case count by workflow
     * @returns {Map<string, number>} Case count per workflow ID
     */
    getCaseCountByWorkflow() {
      const counts = new Map();
      for (const yawlCase of this.cases.values()) {
        const current = counts.get(yawlCase.workflowId) || 0;
        counts.set(yawlCase.workflowId, current + 1);
      }
      return counts;
    }

    /**
     * Get case count by status
     * @returns {Map<string, number>} Case count per status
     */
    getCaseCountByStatus() {
      const counts = new Map();
      for (const yawlCase of this.cases.values()) {
        const current = counts.get(yawlCase.status) || 0;
        counts.set(yawlCase.status, current + 1);
      }
      return counts;
    }
  };
}
