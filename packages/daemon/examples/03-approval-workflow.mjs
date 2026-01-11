/**
 * @file Approval Workflow Example
 * @module examples/03-approval-workflow
 * @description Demonstrates multi-step approval process with deferred choice triggering and audit trail.
 *
 * Features:
 * - Multi-step approval stages (submit, review, approve, publish)
 * - Deferred choice triggering based on external events
 * - Audit trail logging for compliance
 * - Timeout handling at each stage
 * - Parallel reviewer assignment
 *
 * Use Case: Content publishing system with editor review and admin approval
 */

import { Daemon } from '../src/daemon.mjs';
import { randomUUID } from 'crypto';
import { EventEmitter } from 'events';

/**
 * Audit trail recorder for approval tracking
 */
class AuditTrail {
  constructor() {
    this.entries = [];
  }

  /**
   * Record audit entry
   * @param {Object} entry - Audit entry
   */
  record(entry) {
    const auditEntry = {
      timestamp: Date.now(),
      id: `audit-${this.entries.length + 1}`,
      ...entry,
    };

    this.entries.push(auditEntry);
  }

  /**
   * Get entries for case
   * @param {string} caseId - Case identifier
   * @returns {Array} Audit entries
   */
  getEntriesForCase(caseId) {
    return this.entries.filter(e => e.caseId === caseId);
  }

  /**
   * Get all entries
   * @returns {Array} All audit entries
   */
  getAll() {
    return this.entries;
  }
}

/**
 * Mock Approval Workflow Engine
 */
class ApprovalWorkflowEngine extends EventEmitter {
  constructor() {
    super();
    this.cases = new Map();
    this.approvals = new Map();
    this.auditTrail = new AuditTrail();
    this.listeners = new Map();
  }

  /**
   * Create approval case
   * @param {Object} params - Case parameters
   * @returns {Promise<Object>} Created case
   */
  async createApprovalCase(params) {
    const { caseId, contentId, contentType, submittedBy, metadata = {} } = params;

    const approvalCase = {
      caseId,
      contentId,
      contentType,
      submittedBy,
      status: 'submitted',
      currentStage: 'submission',
      stages: {
        submission: { status: 'completed', completedAt: Date.now() },
        review: { status: 'pending', startedAt: null, completedAt: null },
        approval: { status: 'pending', startedAt: null, completedAt: null },
        publishing: { status: 'pending', startedAt: null, completedAt: null },
      },
      reviewers: [],
      approvers: [],
      metadata,
      createdAt: Date.now(),
    };

    this.cases.set(caseId, approvalCase);
    this.auditTrail.record({
      caseId,
      action: 'case_created',
      actor: submittedBy,
      details: { contentId, contentType },
    });

    this._emitEvent('case:created', {
      caseId,
      contentId,
      contentType,
    });

    return approvalCase;
  }

  /**
   * Start review stage
   * Assigns reviewers and enables review task
   * @param {Object} params - Parameters
   * @returns {Promise<Object>} Review stage info
   */
  async startReviewStage(params) {
    const { caseId, reviewerIds, timeoutMs = 3600000 } = params;

    const approvalCase = this.cases.get(caseId);
    if (!approvalCase) {
      throw new Error(`Case not found: ${caseId}`);
    }

    approvalCase.currentStage = 'review';
    approvalCase.stages.review.status = 'in-progress';
    approvalCase.stages.review.startedAt = Date.now();
    approvalCase.reviewers = reviewerIds;
    approvalCase.reviewDeadline = Date.now() + timeoutMs;

    this.auditTrail.record({
      caseId,
      action: 'review_started',
      actor: 'system',
      details: { reviewerIds, timeoutMs },
    });

    this._emitEvent('stage:review-started', {
      caseId,
      reviewerIds,
      deadline: approvalCase.reviewDeadline,
    });

    return {
      stage: 'review',
      reviewerIds,
      deadline: new Date(approvalCase.reviewDeadline),
    };
  }

  /**
   * Submit review result
   * Deferred choice: proceed to approval or reject
   * @param {Object} params - Review result
   * @returns {Promise<Object>} Result
   */
  async submitReview(params) {
    const { caseId, reviewerId, approved, comment } = params;

    const approvalCase = this.cases.get(caseId);
    if (!approvalCase) {
      throw new Error(`Case not found: ${caseId}`);
    }

    const review = {
      caseId,
      reviewerId,
      approved,
      comment,
      submittedAt: Date.now(),
    };

    this.approvals.set(`review-${caseId}-${reviewerId}`, review);

    this.auditTrail.record({
      caseId,
      action: 'review_submitted',
      actor: reviewerId,
      details: { approved, comment },
    });

    // Check if all reviewers have submitted
    const allReviewed = approvalCase.reviewers.length > 0 &&
      approvalCase.reviewers.every(rid =>
        this.approvals.has(`review-${caseId}-${rid}`)
      );

    let nextStage = null;
    if (allReviewed) {
      // Deferred choice: determine next stage based on reviews
      const approvedReviews = approvalCase.reviewers.filter(rid =>
        this.approvals.get(`review-${caseId}-${rid}`).approved
      );

      if (approvedReviews.length === approvalCase.reviewers.length) {
        // All approved -> proceed to approval
        nextStage = 'approval';
        approvalCase.stages.review.status = 'completed';
        approvalCase.stages.review.completedAt = Date.now();
      } else {
        // Some rejected -> end workflow
        nextStage = 'rejected';
        approvalCase.status = 'rejected';
      }

      this.auditTrail.record({
        caseId,
        action: 'review_stage_completed',
        actor: 'system',
        details: {
          approvedCount: approvedReviews.length,
          totalReviewers: approvalCase.reviewers.length,
          nextStage,
        },
      });

      this._emitEvent('stage:review-completed', {
        caseId,
        approved: nextStage === 'approval',
        approvedReviewers: approvedReviews.length,
        totalReviewers: approvalCase.reviewers.length,
      });
    }

    this._emitEvent('review:submitted', {
      caseId,
      reviewerId,
      approved,
    });

    return {
      success: true,
      allReviewed,
      nextStage,
    };
  }

  /**
   * Start approval stage
   * @param {Object} params - Parameters
   * @returns {Promise<Object>} Approval stage info
   */
  async startApprovalStage(params) {
    const { caseId, approverId, timeoutMs = 1800000 } = params;

    const approvalCase = this.cases.get(caseId);
    if (!approvalCase) {
      throw new Error(`Case not found: ${caseId}`);
    }

    approvalCase.currentStage = 'approval';
    approvalCase.stages.approval.status = 'in-progress';
    approvalCase.stages.approval.startedAt = Date.now();
    approvalCase.approvers = [approverId];
    approvalCase.approvalDeadline = Date.now() + timeoutMs;

    this.auditTrail.record({
      caseId,
      action: 'approval_started',
      actor: 'system',
      details: { approverId, timeoutMs },
    });

    this._emitEvent('stage:approval-started', {
      caseId,
      approverId,
      deadline: approvalCase.approvalDeadline,
    });

    return {
      stage: 'approval',
      approverId,
      deadline: new Date(approvalCase.approvalDeadline),
    };
  }

  /**
   * Submit approval decision
   * Deferred choice: approve and publish, or reject
   * @param {Object} params - Decision parameters
   * @returns {Promise<Object>} Result
   */
  async submitApprovalDecision(params) {
    const { caseId, approverId, approved, comment } = params;

    const approvalCase = this.cases.get(caseId);
    if (!approvalCase) {
      throw new Error(`Case not found: ${caseId}`);
    }

    const decision = {
      caseId,
      approverId,
      approved,
      comment,
      submittedAt: Date.now(),
    };

    this.approvals.set(`approval-${caseId}`, decision);

    let nextStage = null;
    if (approved) {
      approvalCase.stages.approval.status = 'completed';
      approvalCase.stages.approval.completedAt = Date.now();
      nextStage = 'publishing';
    } else {
      approvalCase.status = 'rejected';
      nextStage = 'rejected';
    }

    this.auditTrail.record({
      caseId,
      action: 'approval_decision_submitted',
      actor: approverId,
      details: { approved, comment, nextStage },
    });

    this._emitEvent('approval:decision-submitted', {
      caseId,
      approverId,
      approved,
      nextStage,
    });

    return {
      success: true,
      nextStage,
    };
  }

  /**
   * Complete publishing stage
   * @param {Object} params - Publishing parameters
   * @returns {Promise<Object>} Published content info
   */
  async publishContent(params) {
    const { caseId, publishedBy } = params;

    const approvalCase = this.cases.get(caseId);
    if (!approvalCase) {
      throw new Error(`Case not found: ${caseId}`);
    }

    approvalCase.currentStage = 'publishing';
    approvalCase.stages.publishing.status = 'in-progress';
    approvalCase.stages.publishing.startedAt = Date.now();

    // Simulate publishing
    await new Promise(resolve => setTimeout(resolve, 100));

    approvalCase.status = 'published';
    approvalCase.stages.publishing.status = 'completed';
    approvalCase.stages.publishing.completedAt = Date.now();
    approvalCase.publishedAt = Date.now();
    approvalCase.publishedBy = publishedBy;

    this.auditTrail.record({
      caseId,
      action: 'content_published',
      actor: publishedBy,
      details: { contentId: approvalCase.contentId },
    });

    this._emitEvent('content:published', {
      caseId,
      contentId: approvalCase.contentId,
      publishedAt: approvalCase.publishedAt,
    });

    return {
      success: true,
      caseId,
      publishedAt: new Date(approvalCase.publishedAt),
    };
  }

  /**
   * Get case details
   * @param {string} caseId - Case identifier
   * @returns {Object} Case object
   */
  getCase(caseId) {
    return this.cases.get(caseId);
  }

  /**
   * Get audit trail for case
   * @param {string} caseId - Case identifier
   * @returns {Array} Audit entries
   */
  getAuditTrail(caseId) {
    return this.auditTrail.getEntriesForCase(caseId);
  }

  /**
   * Register event listener
   * @param {string} eventName - Event name
   * @param {Function} handler - Event handler
   * @returns {Function} Unsubscriber
   */
  on(eventName, handler) {
    if (!this.listeners.has(eventName)) {
      this.listeners.set(eventName, []);
    }
    this.listeners.get(eventName).push(handler);

    return () => {
      const handlers = this.listeners.get(eventName);
      const idx = handlers.indexOf(handler);
      if (idx > -1) {
        handlers.splice(idx, 1);
      }
    };
  }

  /**
   * Emit event to listeners
   * @private
   */
  _emitEvent(eventName, data) {
    const handlers = this.listeners.get(eventName) || [];
    for (const handler of handlers) {
      try {
        handler(data);
      } catch (error) {
        console.error(`Event handler error for ${eventName}:`, error);
      }
    }
  }
}

/**
 * Main approval workflow example
 */
async function approvalWorkflowExample() {
  console.log('=== Approval Workflow Example ===\n');
  console.log('This example demonstrates a multi-step approval process with deferred choices\n');

  // Initialize components
  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'Approval Workflow Daemon',
  });

  const workflowEngine = new ApprovalWorkflowEngine();

  // =========================================================================
  // Setup event listeners
  // =========================================================================

  workflowEngine.on('case:created', (event) => {
    console.log(`  📄 Case created: ${event.caseId} (${event.contentType})`);
  });

  workflowEngine.on('stage:review-started', (event) => {
    console.log(`  👥 Review stage started: ${event.reviewerIds.length} reviewers assigned`);
  });

  workflowEngine.on('review:submitted', (event) => {
    const decision = event.approved ? 'APPROVED' : 'REJECTED';
    console.log(`    └─ Review from ${event.reviewerId}: ${decision}`);
  });

  workflowEngine.on('stage:review-completed', (event) => {
    const decision = event.approved ? 'APPROVED' : 'REJECTED';
    console.log(`  ✓ Review completed: ${decision} (${event.approvedReviewers}/${event.totalReviewers})`);
  });

  workflowEngine.on('stage:approval-started', (event) => {
    console.log(`  👤 Approval stage started: awaiting admin decision`);
  });

  workflowEngine.on('approval:decision-submitted', (event) => {
    const decision = event.approved ? 'APPROVED' : 'REJECTED';
    console.log(`    └─ Admin decision: ${decision}`);
  });

  workflowEngine.on('content:published', (event) => {
    console.log(`  🚀 Content published: ${event.contentId}`);
  });

  // =========================================================================
  // Define approval workflow operations
  // =========================================================================

  /**
   * Operation: Process approval workflow
   * Executes the complete approval process with deferred choices
   */
  const approvalProcessOperation = {
    id: 'approval-process',
    name: 'Approval Process',
    handler: async () => {
      console.log('\n  ▶️  Starting approval workflow...\n');

      const caseId = `appr-${Date.now()}`;
      const contentId = `content-${Math.random().toString(36).substr(2, 9)}`;

      // Step 1: Create case
      console.log('  [Step 1] Creating approval case');
      await workflowEngine.createApprovalCase({
        caseId,
        contentId,
        contentType: 'article',
        submittedBy: 'author-001',
        metadata: {
          title: 'Quarterly Business Review',
          category: 'business',
        },
      });

      // Step 2: Start review stage
      console.log('\n  [Step 2] Starting review stage');
      await workflowEngine.startReviewStage({
        caseId,
        reviewerIds: ['editor-001', 'editor-002'],
        timeoutMs: 3600000, // 1 hour
      });

      // Step 3: Submit reviews (deferred choice: all must approve)
      console.log('\n  [Step 3] Submitting reviews');
      await workflowEngine.submitReview({
        caseId,
        reviewerId: 'editor-001',
        approved: true,
        comment: 'Well written and fact-checked',
      });
      console.log('');

      const result2 = await workflowEngine.submitReview({
        caseId,
        reviewerId: 'editor-002',
        approved: true,
        comment: 'No issues detected',
      });

      if (result2.nextStage === 'approval') {
        // Step 4: Start approval stage
        console.log('\n  [Step 4] Starting approval stage');
        await workflowEngine.startApprovalStage({
          caseId,
          approverId: 'admin-001',
          timeoutMs: 1800000, // 30 minutes
        });

        // Step 5: Submit approval decision (deferred choice: approve or reject)
        console.log('\n  [Step 5] Submitting approval decision');
        const result3 = await workflowEngine.submitApprovalDecision({
          caseId,
          approverId: 'admin-001',
          approved: true,
          comment: 'Approved for publishing',
        });

        if (result3.nextStage === 'publishing') {
          // Step 6: Publish content
          console.log('\n  [Step 6] Publishing content');
          await workflowEngine.publishContent({
            caseId,
            publishedBy: 'system',
          });
        }
      }

      return {
        caseId,
        contentId,
        status: workflowEngine.getCase(caseId).status,
      };
    },
    metadata: {
      type: 'approval',
      critical: true,
    },
  };

  /**
   * Operation: Generate audit report
   * Creates comprehensive audit trail report
   */
  const auditReportOperation = {
    id: 'audit-report',
    name: 'Audit Report Generator',
    handler: async () => {
      console.log('\n  📋 Generating audit report...\n');

      const allEntries = workflowEngine.auditTrail.getAll();
      const caseIds = new Set(allEntries.map(e => e.caseId));
      const reports = [];

      for (const caseId of caseIds) {
        const entries = workflowEngine.auditTrail.getEntriesForCase(caseId);
        const caseObj = workflowEngine.getCase(caseId);

        reports.push({
          caseId,
          contentId: caseObj.contentId,
          status: caseObj.status,
          createdAt: new Date(caseObj.createdAt),
          completedAt: caseObj.publishedAt ? new Date(caseObj.publishedAt) : null,
          auditEntryCount: entries.length,
          timeline: entries.map(e => ({
            timestamp: new Date(e.timestamp),
            action: e.action,
            actor: e.actor,
          })),
        });
      }

      return {
        totalCases: reports.length,
        reports,
      };
    },
    metadata: {
      type: 'reporting',
      compliance: true,
    },
  };

  // =========================================================================
  // Start daemon
  // =========================================================================

  await daemon.start();
  console.log('✓ Daemon started\n');

  // =========================================================================
  // Schedule and execute operations
  // =========================================================================

  daemon.schedule(approvalProcessOperation);
  daemon.schedule(auditReportOperation);

  console.log('▶️  Executing operations:\n');

  try {
    // Execute approval process
    const approvalResult = await daemon.execute('approval-process');
    console.log(`\n  ✓ Approval workflow completed`);
    console.log(`    Final Status: ${approvalResult.status}`);

    // Generate audit report
    const auditResult = await daemon.execute('audit-report');
    console.log(`\n  ✓ Audit report generated`);
    console.log(`    Total Cases: ${auditResult.totalCases}`);

    // Display audit trail for first case
    if (auditResult.reports.length > 0) {
      const firstReport = auditResult.reports[0];
      console.log(`\n  📜 Audit Trail for ${firstReport.caseId}:`);
      firstReport.timeline.forEach((entry, idx) => {
        console.log(`    ${idx + 1}. ${entry.timestamp.toISOString()} - ${entry.action} (by ${entry.actor})`);
      });
    }
  } catch (error) {
    console.error(`\n✗ Execution error: ${error.message}`);
  }

  // =========================================================================
  // Display metrics
  // =========================================================================

  console.log('\n\n📊 Final Metrics:');
  const metrics = daemon.getMetrics();
  console.log(`  Executed Operations: ${metrics.totalOperations}`);
  console.log(`  Success Rate: ${metrics.successRate.toFixed(1)}%`);
  console.log(`  Average Duration: ${metrics.averageDuration.toFixed(2)}ms`);

  // =========================================================================
  // Cleanup
  // =========================================================================

  console.log('\n⏹️  Stopping daemon...');
  await daemon.stop();
  console.log('✓ Daemon stopped\n');

  console.log('✅ Example completed successfully!');
}

// Run the example
await approvalWorkflowExample();
