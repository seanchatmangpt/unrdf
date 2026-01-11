/**
 * @file Policy-Controlled Workflow Example
 * @module examples/05-policy-controlled-workflow
 * @description Demonstrates workflow execution constrained by policies with policy evaluation and enforcement.
 *
 * Features:
 * - Policy-based workflow constraints
 * - Real-time policy evaluation
 * - Policy violation detection and handling
 * - Audit logging of policy decisions
 * - Compliance reporting
 * - Policy chaining and composition
 *
 * Use Case: Compliance-driven financial transaction processing
 */

import { Daemon } from '../src/daemon.mjs';
import { randomUUID } from 'crypto';

/**
 * Policy engine for evaluating workflow constraints
 */
class PolicyEngine {
  constructor() {
    this.policies = new Map();
    this.evaluationLog = [];
    this.violations = [];
  }

  /**
   * Register a policy
   * @param {Object} policy - Policy definition
   * @param {string} policy.id - Policy identifier
   * @param {string} policy.name - Human-readable name
   * @param {Function} policy.evaluate - Evaluation function (returns boolean)
   * @param {number} [policy.priority=100] - Policy priority (higher = evaluated first)
   * @param {string} [policy.violation='reject'] - Action on violation (reject/warn/audit)
   */
  registerPolicy(policy) {
    if (!policy.id || !policy.name || typeof policy.evaluate !== 'function') {
      throw new Error('Invalid policy: must have id, name, and evaluate function');
    }

    const registered = {
      ...policy,
      priority: policy.priority || 100,
      violation: policy.violation || 'reject',
      registeredAt: Date.now(),
    };

    this.policies.set(policy.id, registered);
  }

  /**
   * Evaluate all policies for context
   * @param {Object} context - Evaluation context (transaction, user, etc.)
   * @returns {Object} Evaluation result
   */
  evaluatePolicies(context) {
    const results = [];
    const violations = [];

    // Sort policies by priority (descending)
    const sorted = Array.from(this.policies.values())
      .sort((a, b) => b.priority - a.priority);

    for (const policy of sorted) {
      try {
        const passed = policy.evaluate(context);

        const result = {
          policyId: policy.id,
          policyName: policy.name,
          passed,
          action: policy.violation,
          evaluatedAt: Date.now(),
        };

        results.push(result);
        this.evaluationLog.push(result);

        if (!passed) {
          violations.push({
            policyId: policy.id,
            policyName: policy.name,
            action: policy.violation,
            context,
          });
          this.violations.push(violations[violations.length - 1]);
        }
      } catch (error) {
        console.error(`Policy evaluation error for ${policy.id}: ${error.message}`);
      }
    }

    return {
      allPassed: violations.length === 0,
      passed: results.filter(r => r.passed).length,
      failed: results.filter(r => !r.passed).length,
      violations,
      results,
    };
  }

  /**
   * Get policies that would be violated for context
   * @param {Object} context - Evaluation context
   * @returns {Array} Violated policies
   */
  getViolatedPolicies(context) {
    const violations = [];

    for (const policy of this.policies.values()) {
      try {
        if (!policy.evaluate(context)) {
          violations.push({
            id: policy.id,
            name: policy.name,
            action: policy.violation,
          });
        }
      } catch (error) {
        // Evaluation error - treat as violation
        violations.push({
          id: policy.id,
          name: policy.name,
          error: error.message,
        });
      }
    }

    return violations;
  }

  /**
   * Get evaluation log
   * @returns {Array} All policy evaluations
   */
  getEvaluationLog() {
    return this.evaluationLog;
  }

  /**
   * Get violations
   * @returns {Array} All recorded violations
   */
  getViolations() {
    return this.violations;
  }

  /**
   * Clear logs
   */
  clearLogs() {
    this.evaluationLog = [];
    this.violations = [];
  }
}

/**
 * Policy-aware workflow engine
 */
class PolicyAwareWorkflowEngine {
  constructor(policyEngine) {
    this.policyEngine = policyEngine;
    this.transactions = new Map();
    this.approvals = [];
    this.rejections = [];
  }

  /**
   * Submit transaction with policy evaluation
   * @param {Object} params - Transaction parameters
   * @returns {Promise<Object>} Submission result
   */
  async submitTransaction(params) {
    const { transactionId, amount, senderType, recipientType, metadata = {} } = params;

    // Create transaction context for policy evaluation
    const context = {
      transactionId,
      amount,
      senderType,
      recipientType,
      timestamp: Date.now(),
      ...metadata,
    };

    // Evaluate policies
    const evaluation = this.policyEngine.evaluatePolicies(context);

    if (!evaluation.allPassed) {
      // Handle violations
      const transaction = {
        transactionId,
        status: 'rejected',
        amount,
        senderType,
        recipientType,
        violations: evaluation.violations,
        evaluatedAt: Date.now(),
      };

      this.transactions.set(transactionId, transaction);
      this.rejections.push(transaction);

      return {
        success: false,
        transactionId,
        status: 'rejected',
        violations: evaluation.violations,
      };
    }

    // All policies passed - transaction approved
    const transaction = {
      transactionId,
      status: 'approved',
      amount,
      senderType,
      recipientType,
      policyEvaluations: evaluation.results,
      approvedAt: Date.now(),
    };

    this.transactions.set(transactionId, transaction);
    this.approvals.push(transaction);

    return {
      success: true,
      transactionId,
      status: 'approved',
      policysPassed: evaluation.passed,
    };
  }

  /**
   * Get transaction details
   * @param {string} transactionId - Transaction identifier
   * @returns {Object} Transaction object
   */
  getTransaction(transactionId) {
    return this.transactions.get(transactionId);
  }

  /**
   * Get compliance statistics
   * @returns {Object} Statistics
   */
  getComplianceStats() {
    const total = this.transactions.size;
    const approved = this.approvals.length;
    const rejected = this.rejections.length;

    return {
      totalTransactions: total,
      approvedTransactions: approved,
      rejectedTransactions: rejected,
      approvalRate: total > 0 ? (approved / total * 100) : 0,
      rejectionRate: total > 0 ? (rejected / total * 100) : 0,
    };
  }
}

/**
 * Main policy-controlled workflow example
 */
async function policyControlledWorkflowExample() {
  console.log('=== Policy-Controlled Workflow Example ===\n');
  console.log('This example demonstrates policy-driven workflow execution\n');

  // =========================================================================
  // Initialize components
  // =========================================================================

  const daemon = new Daemon({
    daemonId: randomUUID(),
    name: 'Policy-Controlled Workflow Daemon',
  });

  const policyEngine = new PolicyEngine();
  const workflowEngine = new PolicyAwareWorkflowEngine(policyEngine);

  // =========================================================================
  // Register policies
  // =========================================================================

  console.log('📋 Registering Policies:\n');

  // Policy 1: Transaction amount limit
  policyEngine.registerPolicy({
    id: 'policy-amount-limit',
    name: 'Daily Transaction Limit',
    priority: 100,
    violation: 'reject',
    evaluate: (context) => {
      const maxAmount = 100000;
      const passed = context.amount <= maxAmount;

      console.log(`  [Amount Limit] ${context.amount} <= ${maxAmount}: ${passed ? 'PASS' : 'FAIL'}`);

      return passed;
    },
  });

  // Policy 2: Sender verification
  policyEngine.registerPolicy({
    id: 'policy-sender-verified',
    name: 'Sender Verification',
    priority: 95,
    violation: 'reject',
    evaluate: (context) => {
      const verifiedTypes = ['business', 'individual-verified', 'corporate'];
      const passed = verifiedTypes.includes(context.senderType);

      console.log(`  [Sender Verification] Type: ${context.senderType}: ${passed ? 'PASS' : 'FAIL'}`);

      return passed;
    },
  });

  // Policy 3: Sanctions screening (high-risk recipients)
  policyEngine.registerPolicy({
    id: 'policy-sanctions-screening',
    name: 'Sanctions List Screening',
    priority: 110, // Higher priority
    violation: 'reject',
    evaluate: (context) => {
      const blockedRecipients = ['entity-high-risk-001', 'entity-blocked-002'];
      const passed = !blockedRecipients.includes(context.recipientType);

      console.log(`  [Sanctions Check] Recipient: ${context.recipientType}: ${passed ? 'PASS' : 'FAIL'}`);

      return passed;
    },
  });

  // Policy 4: Transaction type validation
  policyEngine.registerPolicy({
    id: 'policy-transaction-type',
    name: 'Transaction Type Allowed',
    priority: 90,
    violation: 'reject',
    evaluate: (context) => {
      const allowedTypes = ['transfer', 'payment', 'refund'];
      const type = context.transactionType || 'transfer';
      const passed = allowedTypes.includes(type);

      console.log(`  [Transaction Type] Type: ${type}: ${passed ? 'PASS' : 'FAIL'}`);

      return passed;
    },
  });

  // Policy 5: Audit requirement for large transactions
  policyEngine.registerPolicy({
    id: 'policy-audit-large-amounts',
    name: 'Large Transaction Audit',
    priority: 80,
    violation: 'audit',
    evaluate: (context) => {
      const auditThreshold = 50000;
      const passed = context.amount < auditThreshold;

      if (!passed) {
        console.log(`  [Audit Flag] Large transaction: ${context.amount} (requires audit)`);
      } else {
        console.log(`  [Audit Check] Amount: ${context.amount}: PASS`);
      }

      return passed;
    },
  });

  console.log('');

  // =========================================================================
  // Define workflow operations
  // =========================================================================

  /**
   * Operation: Process transactions
   * Submits multiple transactions for policy evaluation
   */
  const transactionProcessingOperation = {
    id: 'process-transactions',
    name: 'Transaction Processing',
    handler: async () => {
      console.log('\n  💰 Processing transactions...\n');

      const transactions = [
        {
          transactionId: 'TXN-001',
          amount: 25000,
          senderType: 'business',
          recipientType: 'individual-verified',
          transactionType: 'transfer',
        },
        {
          transactionId: 'TXN-002',
          amount: 75000, // Large amount - triggers audit
          senderType: 'corporate',
          recipientType: 'business',
          transactionType: 'payment',
        },
        {
          transactionId: 'TXN-003',
          amount: 150000, // Exceeds limit - will be rejected
          senderType: 'individual-verified',
          recipientType: 'business',
          transactionType: 'transfer',
        },
        {
          transactionId: 'TXN-004',
          amount: 30000,
          senderType: 'individual', // Not verified - will be rejected
          recipientType: 'entity-high-risk-001',
          transactionType: 'transfer',
        },
        {
          transactionId: 'TXN-005',
          amount: 45000,
          senderType: 'business',
          recipientType: 'individual-verified',
          transactionType: 'refund',
        },
      ];

      const results = [];

      for (const txn of transactions) {
        console.log(`\n  [${txn.transactionId}] Amount: ${txn.amount}`);
        const result = await workflowEngine.submitTransaction(txn);
        results.push(result);
        console.log(`    → Result: ${result.status.toUpperCase()}`);

        if (!result.success && result.violations.length > 0) {
          console.log(`    → Violations:`);
          result.violations.forEach(v => {
            console.log(`       • ${v.policyName}`);
          });
        }
      }

      return {
        processedCount: results.length,
        approvedCount: results.filter(r => r.success).length,
        rejectedCount: results.filter(r => !r.success).length,
      };
    },
    metadata: {
      type: 'transaction-processing',
      compliance: true,
    },
  };

  /**
   * Operation: Generate compliance report
   */
  const complianceReportOperation = {
    id: 'compliance-report',
    name: 'Compliance Report',
    handler: async () => {
      console.log('\n  📊 Generating compliance report...\n');

      const stats = workflowEngine.getComplianceStats();
      const violations = policyEngine.getViolations();

      return {
        stats,
        violationCount: violations.length,
        policyEvaluationCount: policyEngine.getEvaluationLog().length,
      };
    },
    metadata: {
      type: 'reporting',
      compliance: true,
    },
  };

  /**
   * Operation: Policy effectiveness analysis
   */
  const policyAnalysisOperation = {
    id: 'policy-analysis',
    name: 'Policy Effectiveness Analysis',
    handler: async () => {
      console.log('\n  🔍 Analyzing policy effectiveness...\n');

      const evaluationLog = policyEngine.getEvaluationLog();
      const policyStats = new Map();

      // Aggregate statistics by policy
      for (const entry of evaluationLog) {
        if (!policyStats.has(entry.policyId)) {
          policyStats.set(entry.policyId, {
            policyId: entry.policyId,
            policyName: entry.policyName,
            evaluations: 0,
            passes: 0,
            failures: 0,
          });
        }

        const stat = policyStats.get(entry.policyId);
        stat.evaluations += 1;
        if (entry.passed) {
          stat.passes += 1;
        } else {
          stat.failures += 1;
        }
      }

      return {
        policyCount: policyStats.size,
        stats: Array.from(policyStats.values()),
      };
    },
    metadata: {
      type: 'analysis',
      compliance: true,
    },
  };

  // =========================================================================
  // Start daemon and schedule operations
  // =========================================================================

  await daemon.start();
  console.log('\n✓ Daemon started\n');

  daemon.schedule(transactionProcessingOperation);
  daemon.schedule(complianceReportOperation);
  daemon.schedule(policyAnalysisOperation);

  // =========================================================================
  // Execute operations
  // =========================================================================

  console.log('▶️  Executing operations:\n');

  try {
    // Step 1: Process transactions
    console.log('[Step 1] Processing Transactions');
    console.log('━'.repeat(60));
    const txnResult = await daemon.execute('process-transactions');

    // Step 2: Generate compliance report
    console.log('\n\n[Step 2] Compliance Report');
    console.log('━'.repeat(60));
    const reportResult = await daemon.execute('compliance-report');

    console.log(`\n  Summary:`);
    console.log(`    Total Transactions: ${reportResult.stats.totalTransactions}`);
    console.log(`    Approved: ${reportResult.stats.approvedTransactions}`);
    console.log(`    Rejected: ${reportResult.stats.rejectedTransactions}`);
    console.log(`    Approval Rate: ${reportResult.stats.approvalRate.toFixed(1)}%`);
    console.log(`    Policy Violations: ${reportResult.violationCount}`);
    console.log(`    Evaluations Performed: ${reportResult.policyEvaluationCount}`);

    // Step 3: Policy analysis
    console.log('\n\n[Step 3] Policy Effectiveness Analysis');
    console.log('━'.repeat(60));
    const analysisResult = await daemon.execute('policy-analysis');

    console.log(`\n  Active Policies: ${analysisResult.policyCount}`);
    console.log(`\n  Policy Performance:`);

    analysisResult.stats.forEach(stat => {
      const failureRate = stat.evaluations > 0
        ? (stat.failures / stat.evaluations * 100)
        : 0;
      console.log(`    • ${stat.policyName}:`);
      console.log(`      - Evaluations: ${stat.evaluations}`);
      console.log(`      - Passes: ${stat.passes}`);
      console.log(`      - Failures: ${stat.failures}`);
      console.log(`      - Failure Rate: ${failureRate.toFixed(1)}%`);
    });
  } catch (error) {
    console.error(`\n✗ Execution error: ${error.message}`);
  }

  // =========================================================================
  // Display detailed audit trail
  // =========================================================================

  console.log('\n\n📜 Detailed Audit Trail:\n');

  const violations = policyEngine.getViolations();
  if (violations.length > 0) {
    console.log('  Policy Violations:');
    violations.forEach((v, idx) => {
      console.log(`\n    ${idx + 1}. ${v.policyName}`);
      console.log(`       Action: ${v.action}`);
      console.log(`       Transaction: ${v.context.transactionId}`);
      console.log(`       Amount: ${v.context.amount}`);
    });
  }

  // =========================================================================
  // Display final metrics
  // =========================================================================

  console.log('\n\n📊 Final Metrics:\n');

  const metrics = daemon.getMetrics();
  console.log(`  Daemon Operations:`);
  console.log(`    Total Executed: ${metrics.totalOperations}`);
  console.log(`    Success Rate: ${metrics.successRate.toFixed(1)}%`);
  console.log(`    Average Duration: ${metrics.averageDuration.toFixed(2)}ms`);

  const complianceStats = workflowEngine.getComplianceStats();
  console.log(`\n  Compliance:`);
  console.log(`    Total Transactions: ${complianceStats.totalTransactions}`);
  console.log(`    Approved: ${complianceStats.approvedTransactions}`);
  console.log(`    Rejected: ${complianceStats.rejectedTransactions}`);
  console.log(`    Approval Rate: ${complianceStats.approvalRate.toFixed(1)}%`);

  // =========================================================================
  // Cleanup
  // =========================================================================

  console.log('\n⏹️  Stopping daemon...');
  await daemon.stop();
  console.log('✓ Daemon stopped\n');

  console.log('✅ Example completed successfully!');
}

// Run the example
await policyControlledWorkflowExample();
