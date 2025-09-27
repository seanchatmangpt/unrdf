/**
 * @file Example demonstrating the 80/20 Knowledge Hook contract.
 * @description This example shows how to create and use knowledge hooks
 * with the defineHook function, following the autonomic systems principles.
 */

import { defineHook } from '../src/knowledge-engine/define-hook.mjs';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

/**
 * Example hook for parliamentary motion compliance.
 * This hook ensures that motions follow Robert's Rules of Order.
 */
export const motionComplianceHook = defineHook({
  meta: {
    name: 'parliamentary:motion-compliance',
    description: 'Enforces Robert\'s Rules of Order for parliamentary motions',
    ontology: ['parliamentary', 'prov']
  },
  channel: {
    graphs: ['urn:graph:parliamentary:session'],
    view: 'delta'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/parliamentary/motion-compliance.ask.rq',
      sha256: 'a1b2c3d4e5f6789012345678901234567890abcdef1234567890abcdef123456', // Example hash
      mediaType: 'application/sparql-query'
    }
  },
  determinism: { seed: 42 },
  receipt: { anchor: 'git-notes' },

  async before({ payload, context }) {
    console.log(`[BEFORE] Motion compliance check for: ${payload?.motionId || 'unknown'}`);
    
    // Validate payload structure
    if (!payload || !payload.motionId) {
      return { cancel: true, reason: 'Missing required motionId in payload' };
    }
    
    // Add timestamp for audit trail
    return { 
      ...payload, 
      checkedAt: new Date().toISOString(),
      sessionId: context?.env?.sessionId || 'default'
    };
  },

  async run({ payload, context }) {
    console.log(`[RUN] Checking motion ${payload.motionId} for compliance`);
    
    // Simulate compliance checking logic
    const isCompliant = await checkMotionCompliance(payload.motionId, context.graph);
    
    if (!isCompliant) {
      return {
        result: { 
          status: 'non-compliant', 
          motionId: payload.motionId,
          reason: 'Motion does not meet parliamentary requirements'
        },
        assertions: [
          quad(
            namedNode(`urn:motion:${payload.motionId}`),
            namedNode('urn:parliamentary:complianceStatus'),
            literal('non-compliant')
          ),
          quad(
            namedNode(`urn:motion:${payload.motionId}`),
            namedNode('urn:parliamentary:checkedAt'),
            literal(payload.checkedAt)
          )
        ]
      };
    }
    
    return {
      result: { 
        status: 'compliant', 
        motionId: payload.motionId 
      },
      assertions: [
        quad(
          namedNode(`urn:motion:${payload.motionId}`),
          namedNode('urn:parliamentary:complianceStatus'),
          literal('compliant')
        ),
        quad(
          namedNode(`urn:motion:${payload.motionId}`),
          namedNode('urn:parliamentary:checkedAt'),
          literal(payload.checkedAt)
        )
      ]
    };
  },

  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`[AFTER] Motion compliance check cancelled: ${reason}`);
      return { result: { finalStatus: 'cancelled', reason } };
    }
    
    console.log(`[AFTER] Motion compliance check completed: ${result?.result?.status}`);
    return { result: { finalStatus: 'completed', complianceStatus: result?.result?.status } };
  }
});

/**
 * Example hook for financial transaction monitoring.
 * This hook monitors large transactions and creates audit trails.
 */
export const financialMonitoringHook = defineHook({
  meta: {
    name: 'financial:large-transaction-monitor',
    description: 'Monitors and audits large financial transactions',
    ontology: ['fibo', 'prov', 'audit']
  },
  channel: {
    graphs: ['urn:graph:financial:transactions'],
    view: 'delta'
  },
  when: {
    kind: 'sparql-select',
    ref: {
      uri: 'file://hooks/financial/large-transaction.select.rq',
      sha256: 'b2c3d4e5f6789012345678901234567890abcdef1234567890abcdef1234567', // Example hash
      mediaType: 'application/sparql-query'
    }
  },
  determinism: { seed: 123 },
  receipt: { anchor: 'git-notes' },

  async before({ payload }) {
    console.log(`[BEFORE] Financial monitoring for transaction: ${payload?.transactionId || 'unknown'}`);
    
    if (!payload || !payload.transactionId) {
      return { cancel: true, reason: 'Missing required transactionId' };
    }
    
    return { 
      ...payload, 
      monitoredAt: new Date().toISOString(),
      threshold: payload.threshold || 10000 // Default threshold
    };
  },

  async run({ payload, context }) {
    console.log(`[RUN] Monitoring transaction ${payload.transactionId}`);
    
    // Simulate transaction analysis
    const analysis = await analyzeTransaction(payload.transactionId, payload.threshold);
    
    return {
      result: {
        status: 'monitored',
        transactionId: payload.transactionId,
        analysis: analysis,
        riskLevel: analysis.amount > payload.threshold ? 'high' : 'low'
      },
      assertions: [
        quad(
          namedNode(`urn:transaction:${payload.transactionId}`),
          namedNode('urn:financial:monitoredAt'),
          literal(payload.monitoredAt)
        ),
        quad(
          namedNode(`urn:transaction:${payload.transactionId}`),
          namedNode('urn:financial:riskLevel'),
          literal(analysis.amount > payload.threshold ? 'high' : 'low')
        ),
        quad(
          namedNode(`urn:transaction:${payload.transactionId}`),
          namedNode('urn:financial:amount'),
          literal(analysis.amount.toString())
        )
      ]
    };
  },

  async after({ result, cancelled }) {
    if (cancelled) {
      console.log(`[AFTER] Financial monitoring cancelled`);
      return { result: { finalStatus: 'cancelled' } };
    }
    
    console.log(`[AFTER] Financial monitoring completed for ${result?.result?.transactionId}`);
    return { result: { finalStatus: 'completed' } };
  }
});

/**
 * Example hook for data quality validation.
 * This hook validates data against SHACL shapes.
 */
export const dataQualityHook = defineHook({
  meta: {
    name: 'quality:shacl-validation',
    description: 'Validates data quality using SHACL shapes',
    ontology: ['shacl', 'quality', 'prov']
  },
  channel: {
    graphs: ['urn:graph:data:quality'],
    view: 'after'
  },
  when: {
    kind: 'shacl',
    ref: {
      uri: 'file://hooks/quality/data-quality.shacl.ttl',
      sha256: 'c3d4e5f6789012345678901234567890abcdef1234567890abcdef12345678', // Example hash
      mediaType: 'text/turtle'
    }
  },
  determinism: { seed: 456 },
  receipt: { anchor: 'none' },

  async before({ payload, context }) {
    console.log(`[BEFORE] Data quality validation starting`);
    
    if (!context?.graph) {
      return { cancel: true, reason: 'No graph available for validation' };
    }
    
    return { 
      ...payload, 
      validatedAt: new Date().toISOString()
    };
  },

  async run({ payload, context }) {
    console.log(`[RUN] Running SHACL validation`);
    
    // Simulate SHACL validation
    const validationResult = await validateWithShacl(context.graph, payload.shapes);
    
    return {
      result: {
        status: validationResult.conforms ? 'valid' : 'invalid',
        conforms: validationResult.conforms,
        violationCount: validationResult.results?.length || 0
      },
      assertions: [
        quad(
          namedNode('urn:validation:session'),
          namedNode('urn:quality:validatedAt'),
          literal(payload.validatedAt)
        ),
        quad(
          namedNode('urn:validation:session'),
          namedNode('urn:quality:conforms'),
          literal(validationResult.conforms.toString())
        )
      ]
    };
  },

  async after({ result, cancelled }) {
    if (cancelled) {
      console.log(`[AFTER] Data quality validation cancelled`);
      return { result: { finalStatus: 'cancelled' } };
    }
    
    console.log(`[AFTER] Data quality validation completed: ${result?.result?.status}`);
    return { result: { finalStatus: 'completed' } };
  }
});

// Helper functions (simulated implementations)

/**
 * Simulate motion compliance checking.
 * @param {string} motionId - The motion identifier
 * @param {any} graph - The RDF graph
 * @returns {Promise<boolean>} Whether the motion is compliant
 */
async function checkMotionCompliance(motionId, graph) {
  // Simulate async compliance checking
  await new Promise(resolve => setTimeout(resolve, 100));
  
  // Simple mock logic - in reality this would query the graph
  return motionId.includes('compliant') || Math.random() > 0.3;
}

/**
 * Simulate transaction analysis.
 * @param {string} transactionId - The transaction identifier
 * @param {number} threshold - The monitoring threshold
 * @returns {Promise<Object>} Transaction analysis result
 */
async function analyzeTransaction(transactionId, threshold) {
  // Simulate async analysis
  await new Promise(resolve => setTimeout(resolve, 150));
  
  // Mock analysis result
  return {
    amount: Math.random() * threshold * 2,
    currency: 'USD',
    timestamp: new Date().toISOString()
  };
}

/**
 * Simulate SHACL validation.
 * @param {any} graph - The RDF graph to validate
 * @param {any} shapes - The SHACL shapes
 * @returns {Promise<Object>} Validation result
 */
async function validateWithShacl(graph, shapes) {
  // Simulate async validation
  await new Promise(resolve => setTimeout(resolve, 200));
  
  // Mock validation result
  return {
    conforms: Math.random() > 0.2,
    results: Math.random() > 0.8 ? [
      { message: 'Sample validation error', severity: 'Violation' }
    ] : []
  };
}

// Example usage
async function demonstrateHooks() {
  console.log('🏛️ Knowledge Hook Examples\n');
  
  // Example 1: Parliamentary motion compliance
  console.log('1. Parliamentary Motion Compliance Hook:');
  console.log('   Name:', motionComplianceHook.meta.name);
  console.log('   Description:', motionComplianceHook.meta.description);
  console.log('   Condition URI:', motionComplianceHook.when.ref.uri);
  console.log('   Determinism seed:', motionComplianceHook.determinism.seed);
  console.log('   Receipt anchor:', motionComplianceHook.receipt.anchor);
  
  // Example 2: Financial monitoring
  console.log('\n2. Financial Monitoring Hook:');
  console.log('   Name:', financialMonitoringHook.meta.name);
  console.log('   Description:', financialMonitoringHook.meta.description);
  console.log('   Condition URI:', financialMonitoringHook.when.ref.uri);
  console.log('   Channel view:', financialMonitoringHook.channel.view);
  
  // Example 3: Data quality validation
  console.log('\n3. Data Quality Hook:');
  console.log('   Name:', dataQualityHook.meta.name);
  console.log('   Description:', dataQualityHook.meta.description);
  console.log('   Condition kind:', dataQualityHook.when.kind);
  console.log('   Media type:', dataQualityHook.when.ref.mediaType);
  
  console.log('\n✅ All hooks defined successfully following the 80/20 contract!');
}

// Run demonstration if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  demonstrateHooks().catch(console.error);
}
