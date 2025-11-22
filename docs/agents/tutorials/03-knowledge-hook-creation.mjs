/**
 * @file Tutorial 03: Knowledge Hook Creation
 * @module agents/tutorials/knowledge-hook-creation
 * 
 * @description
 * Machine-executable tutorial for creating and registering Knowledge Hooks.
 * Demonstrates content-addressed conditions, lifecycle phases, and transaction
 * integration.
 * 
 * @targetAudience AutonomicAgent, HyperintelligentSystem
 * @format machine-executable
 */

import { createDarkMatterCore, defineHook } from 'unrdf';
import { createHash } from 'crypto';
import { readFileSync, writeFileSync } from 'fs';
import { z } from 'zod';

/**
 * Step 1: Create content-addressed condition file
 * 
 * @param {string} query - SPARQL query string
 * @param {string} filePath - Path to save condition file
 * @returns {Object} Content-addressed reference
 */
export function createContentAddressedCondition(query, filePath) {
  // Write query to file
  writeFileSync(filePath, query, 'utf-8');
  
  // Calculate SHA-256 hash
  const hash = createHash('sha256').update(query).digest('hex');
  
  return {
    uri: `file://${filePath}`,
    sha256: hash,
    mediaType: 'application/sparql-query'
  };
}

/**
 * Step 2: Define a Knowledge Hook with validation
 * 
 * @param {Object} spec - Hook specification
 * @returns {Object} Validated hook definition
 */
export function defineKnowledgeHook(spec) {
  // Validate spec with Zod
  const HookSpecSchema = z.object({
    meta: z.object({
      name: z.string().min(1).max(100),
      description: z.string().max(500).optional(),
      version: z.string().regex(/^\d+\.\d+\.\d+$/).optional(),
      tags: z.array(z.string()).optional()
    }),
    channel: z.object({
      graphs: z.array(z.string()).optional(),
      view: z.enum(['before', 'after', 'delta']).optional()
    }).optional(),
    when: z.object({
      kind: z.enum(['sparql-ask', 'sparql-select', 'shacl']),
      query: z.string().optional(),
      ref: z.object({
        uri: z.string(),
        sha256: z.string(),
        mediaType: z.string()
      }).optional()
    }).refine(
      data => data.query || data.ref,
      { message: 'Either query or ref must be provided' }
    ),
    before: z.function().optional(),
    run: z.function(),
    after: z.function().optional()
  });
  
  const validated = HookSpecSchema.parse(spec);
  
  // Create hook using defineHook
  return defineHook({
    meta: validated.meta,
    channel: validated.channel,
    when: validated.when,
    before: validated.before,
    run: validated.run,
    after: validated.after
  });
}

/**
 * Step 3: Register hook with system
 * 
 * @param {Object} hook - Hook definition from Step 2
 * @returns {Promise<Object>} Registration result
 */
export async function registerHook(hook) {
  const system = await createDarkMatterCore();
  
  try {
    await system.registerHook(hook);
    return {
      success: true,
      hookName: hook.meta.name,
      error: null
    };
  } catch (error) {
    return {
      success: false,
      hookName: hook.meta.name,
      error: error.message
    };
  }
}

/**
 * Step 4: Execute transaction to trigger hook
 * 
 * @param {Object} delta - Transaction delta
 * @returns {Promise<Object>} Transaction receipt
 */
export async function executeTransaction(delta) {
  const system = await createDarkMatterCore();
  
  const receipt = await system.executeTransaction({
    additions: delta.additions || [],
    removals: delta.removals || []
  });
  
  return {
    transactionId: receipt.id,
    status: receipt.status,
    hooksExecuted: receipt.hooksExecuted || [],
    timestamp: receipt.timestamp
  };
}

/**
 * Complete tutorial: Create and test a validation hook
 * 
 * @returns {Promise<Object>} Tutorial results
 */
export async function runTutorial() {
  const results = {
    step1: null,
    step2: null,
    step3: null,
    step4: null
  };
  
  try {
    // Step 1: Create content-addressed condition
    const conditionQuery = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      ASK {
        ?person a foaf:Person .
        FILTER NOT EXISTS { ?person foaf:name ?name }
      }
    `;
    
    results.step1 = createContentAddressedCondition(
      conditionQuery,
      './tutorial-condition.ask.rq'
    );
    
    // Step 2: Define hook
    const hook = defineKnowledgeHook({
      meta: {
        name: 'tutorial:person-name-validation',
        description: 'Validates that all persons have names',
        tags: ['validation', 'tutorial']
      },
      channel: {
        view: 'before'
      },
      when: {
        kind: 'sparql-ask',
        ref: results.step1
      },
      run: async (event) => {
        if (event.result === true) {
          throw new Error('CONSTRAINT_VIOLATION: All persons must have names');
        }
        return { validated: true };
      }
    });
    
    results.step2 = {
      name: hook.meta.name,
      condition: hook.when.kind,
      registered: false
    };
    
    // Step 3: Register hook
    results.step3 = await registerHook(hook);
    results.step2.registered = results.step3.success;
    
    // Step 4: Execute transaction (will trigger hook)
    results.step4 = await executeTransaction({
      additions: [
        {
          subject: 'ex:alice',
          predicate: 'rdf:type',
          object: 'foaf:Person'
        }
        // Missing foaf:name - hook should reject
      ]
    });
    
  } catch (error) {
    results.error = {
      message: error.message,
      stack: error.stack
    };
  }
  
  return results;
}

// Export for machine execution
export default {
  createContentAddressedCondition,
  defineKnowledgeHook,
  registerHook,
  executeTransaction,
  runTutorial
};

