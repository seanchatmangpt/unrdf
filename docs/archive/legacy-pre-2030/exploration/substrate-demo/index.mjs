#!/usr/bin/env node

/**
 * UNRDF Substrate End-to-End Demo - Agent 9
 *
 * Demonstrates UNRDF as a complete substrate across all 4 critical roles:
 *   STORE  -> DERIVE  -> ENFORCE  -> RENDER
 *
 * HYPOTHESIS:
 * "UNRDF can be used as a complete substrate by:
 *  1. STORing RDF (ingest + canonicalize)
 *  2. DERIVing new facts (SPARQL CONSTRUCT)
 *  3. ENFORCing policies (hooks/validation)
 *  4. RENDERing results (CLI reports + JSON output)"
 *
 * Usage: node exploration/substrate-demo/index.mjs
 */

import crypto from 'crypto';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = path.resolve(__dirname, '../..');

// ============================================================================
// UTILITY FUNCTIONS (adapted from agents 2, 6)
// ============================================================================

/**
 * Canonicalize quads by deterministic sorting
 * Sort by: subject, predicate, object, graph (lexicographic)
 * @param {Array} quads - Array of quads
 * @returns {Array} Sorted quads
 */
function canonicalizeQuads(quads) {
  const sortKey = (term) => {
    if (!term) return '';
    if (term.value !== undefined) return term.value;
    return String(term);
  };

  return [...quads].sort((a, b) => {
    const aSubj = sortKey(a.subject);
    const bSubj = sortKey(b.subject);
    if (aSubj !== bSubj) return aSubj.localeCompare(bSubj);

    const aPred = sortKey(a.predicate);
    const bPred = sortKey(b.predicate);
    if (aPred !== bPred) return aPred.localeCompare(bPred);

    const aObj = sortKey(a.object);
    const bObj = sortKey(b.object);
    if (aObj !== bObj) return aObj.localeCompare(bObj);

    const aGraph = sortKey(a.graph);
    const bGraph = sortKey(b.graph);
    return aGraph.localeCompare(bGraph);
  });
}

/**
 * Serialize quads to canonical N-Quads string (deterministic)
 * @param {Array} quads - Array of quads
 * @returns {string} N-Quads string
 */
function serializeToCanonical(quads) {
  return quads.map(quad => {
    const subject = serializeTerm(quad.subject);
    const predicate = serializeTerm(quad.predicate);
    const object = serializeTerm(quad.object);
    const graph = quad.graph ? serializeTerm(quad.graph) : '';

    if (graph && !graph.includes('default')) {
      return `${subject} ${predicate} ${object} ${graph} .`;
    }
    return `${subject} ${predicate} ${object} .`;
  }).join('\n');
}

/**
 * Serialize RDF term to N-Quads format
 * @param {Object} term - RDF term
 * @returns {string} Serialized term
 */
function serializeTerm(term) {
  if (!term) return '';

  if (term.termType === 'Literal') {
    const escaped = (term.value || '').replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    if (term.datatype && term.datatype.value) {
      return `"${escaped}"^^<${term.datatype.value}>`;
    }
    if (term.language) {
      return `"${escaped}"@${term.language}`;
    }
    return `"${escaped}"`;
  }

  if (term.termType === 'NamedNode' || term.value) {
    const value = term.value || term;
    return `<${value}>`;
  }

  if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  }

  return `<${String(term)}>`;
}

/**
 * Compute SHA256 hash of canonical form
 * @param {string} canonicalText - Canonical N-Quads text
 * @returns {string} SHA256 hash (hex)
 */
function hashCanonical(canonicalText) {
  return crypto.createHash('sha256').update(canonicalText).digest('hex');
}

/**
 * Receipt class for policy enforcement
 */
class Receipt {
  constructor(quad, policyName, result, reason) {
    this.timestamp = new Date().toISOString();
    this.quad = {
      subject: quad.subject?.value || String(quad.subject),
      predicate: quad.predicate?.value || String(quad.predicate),
      object: quad.object?.value || String(quad.object),
      graph: quad.graph?.value || String(quad.graph),
    };
    this.policy = policyName;
    this.result = result; // 'allow' | 'reject'
    this.reason = reason;
    this.id = `rcpt-${Date.now()}-${Math.random().toString(36).substring(7)}`;
  }

  toJSON() {
    return {
      id: this.id,
      timestamp: this.timestamp,
      quad: this.quad,
      policy: this.policy,
      result: this.result,
      reason: this.reason,
    };
  }
}

// ============================================================================
// SUBSTRATE ORCHESTRATOR
// ============================================================================

class SubstrateOrchestrator {
  constructor() {
    this.steps = {};
  }

  /**
   * STEP 1: STORE - Load RDF, canonicalize, hash
   * @param {Object} store - Oxigraph store
   * @param {Array} quads - Quads to load
   * @returns {Object} Store step result
   */
  async store(store, quads) {
    const start = Date.now();

    // Load quads into store
    for (const quad of quads) {
      store.add(quad);
    }

    // Get all quads and canonicalize
    const allQuads = store.match();
    const canonicalQuads = canonicalizeQuads(allQuads);
    const canonicalText = serializeToCanonical(canonicalQuads);
    const hash = hashCanonical(canonicalText);

    const duration = Date.now() - start;

    return {
      store,
      count: allQuads.length,
      hash: hash.substring(0, 16) + '...',
      fullHash: hash,
      status: 'success',
      duration_ms: duration,
    };
  }

  /**
   * STEP 2: DERIVE - Execute SPARQL CONSTRUCT to derive new facts
   * @param {Object} store - Oxigraph store with data
   * @param {string} sparqlQuery - SPARQL CONSTRUCT query
   * @returns {Object} Derive step result
   */
  async derive(store, sparqlQuery) {
    const start = Date.now();

    // Execute CONSTRUCT query
    const derivedQuads = store.query(sparqlQuery);

    // Add derived quads to store
    for (const quad of derivedQuads) {
      store.add(quad);
    }

    // Canonicalize derived quads
    const canonicalDerived = canonicalizeQuads(derivedQuads);
    const canonicalText = serializeToCanonical(canonicalDerived);
    const hash = hashCanonical(canonicalText);

    const duration = Date.now() - start;

    return {
      store,
      derivedQuads,
      count: derivedQuads.length,
      hash: hash.substring(0, 16) + '...',
      fullHash: hash,
      sparql_query: sparqlQuery.replace(/\s+/g, ' ').trim(),
      status: 'success',
      duration_ms: duration,
    };
  }

  /**
   * STEP 3: ENFORCE - Apply policy hooks to derived data
   * @param {Array} quads - Quads to validate
   * @param {Object} policy - Policy definition
   * @returns {Object} Enforce step result
   */
  async enforce(quads, policy) {
    const start = Date.now();
    const receipts = [];
    let allowed = 0;
    let rejected = 0;

    for (const quad of quads) {
      const isValid = policy.validate(quad);
      const receipt = new Receipt(
        quad,
        policy.name,
        isValid ? 'allow' : 'reject',
        isValid ? 'Predicate in whitelist' : 'Predicate not in allowed list'
      );
      receipts.push(receipt);
      if (isValid) {
        allowed++;
      } else {
        rejected++;
      }
    }

    const duration = Date.now() - start;

    return {
      input_quads: quads.length,
      allowed,
      rejected,
      policy: policy.name,
      receipts: receipts.map(r => r.toJSON()),
      status: 'success',
      duration_ms: duration,
    };
  }

  /**
   * STEP 4: RENDER - Generate human-readable report + JSON output
   * @param {Object} storeResult - Result from store step
   * @param {Object} deriveResult - Result from derive step
   * @param {Object} enforceResult - Result from enforce step
   * @returns {Object} Render step result
   */
  async render(storeResult, deriveResult, enforceResult) {
    const start = Date.now();

    const report = {
      timestamp: new Date().toISOString(),
      pipeline: 'store-derive-enforce-render',
      steps: {
        store: {
          input_quads: storeResult.count,
          canonical_hash: storeResult.fullHash,
          status: storeResult.status,
          duration_ms: storeResult.duration_ms,
        },
        derive: {
          input_quads: storeResult.count,
          derived_quads: deriveResult.count,
          sparql_query: deriveResult.sparql_query,
          derived_hash: deriveResult.fullHash,
          status: deriveResult.status,
          duration_ms: deriveResult.duration_ms,
        },
        enforce: {
          input_quads: enforceResult.input_quads,
          allowed: enforceResult.allowed,
          rejected: enforceResult.rejected,
          policy: enforceResult.policy,
          receipts: enforceResult.receipts.slice(0, 5), // First 5 receipts
          status: enforceResult.status,
          duration_ms: enforceResult.duration_ms,
        },
        render: {
          report_format: 'json+console',
          output_written: 'exploration/substrate-demo/result-report.json',
          status: 'success',
          duration_ms: 0, // Will be updated
        },
      },
      summary: `${storeResult.count} input quads -> ${deriveResult.count} derived -> ${enforceResult.allowed} allowed -> JSON report`,
    };

    // Write JSON report to file
    const outputPath = path.join(__dirname, 'result-report.json');
    fs.writeFileSync(outputPath, JSON.stringify(report, null, 2));

    const duration = Date.now() - start;
    report.steps.render.duration_ms = duration;

    return {
      report,
      outputPath,
      status: 'success',
      duration_ms: duration,
    };
  }
}

// ============================================================================
// MAIN EXECUTION
// ============================================================================

async function main() {
  try {
    // Import oxigraph dynamically
    const { createStore, dataFactory } = await import(
      path.resolve(ROOT_DIR, 'packages/oxigraph/src/index.mjs')
    );

    // Print header
    console.log('');
    console.log('='.repeat(64));
    console.log('         UNRDF SUBSTRATE END-TO-END DEMO');
    console.log('         Agent 9: Integration Orchestrator');
    console.log('='.repeat(64));
    console.log('');

    const substrate = new SubstrateOrchestrator();

    // ========================================================================
    // STEP 1: STORE - Load RDF data
    // ========================================================================
    console.log('[STEP 1] STORE: Loading and canonicalizing RDF data...');

    const store = createStore();

    // Create test dataset (Alice/Bob network)
    const ex = (local) => dataFactory.namedNode(`http://example.org/${local}`);
    const foaf = (local) => dataFactory.namedNode(`http://xmlns.com/foaf/0.1/${local}`);
    const rdf = (local) => dataFactory.namedNode(`http://www.w3.org/1999/02/22-rdf-syntax-ns#${local}`);

    const testQuads = [
      dataFactory.quad(ex('alice'), rdf('type'), foaf('Person')),
      dataFactory.quad(ex('alice'), foaf('name'), dataFactory.literal('Alice')),
      dataFactory.quad(ex('alice'), foaf('knows'), ex('bob')),
      dataFactory.quad(ex('bob'), rdf('type'), foaf('Person')),
      dataFactory.quad(ex('bob'), foaf('name'), dataFactory.literal('Bob')),
    ];

    const storeResult = await substrate.store(store, testQuads);
    console.log(`   STORE: ${storeResult.count} input quads | Hash: ${storeResult.hash}`);
    console.log(`   Duration: ${storeResult.duration_ms}ms`);
    console.log('');

    // ========================================================================
    // STEP 2: DERIVE - Execute SPARQL CONSTRUCT
    // ========================================================================
    console.log('[STEP 2] DERIVE: Executing SPARQL CONSTRUCT query...');

    const sparqlConstruct = `
      CONSTRUCT {
        ?person <http://example.org/hasFriend> ?friend .
        ?friend <http://example.org/isFriendOf> ?person .
        ?person <http://example.org/isConnected> ?friend .
      }
      WHERE {
        ?person <http://xmlns.com/foaf/0.1/knows> ?friend .
      }
    `;

    const deriveResult = await substrate.derive(store, sparqlConstruct);
    console.log(`   DERIVE: +${deriveResult.count} derived quads | Hash: ${deriveResult.hash}`);
    console.log(`   Duration: ${deriveResult.duration_ms}ms`);
    console.log('');

    // ========================================================================
    // STEP 3: ENFORCE - Apply policy hooks
    // ========================================================================
    console.log('[STEP 3] ENFORCE: Applying predicate whitelist policy...');

    // Define predicate whitelist policy
    const predicateWhitelistPolicy = {
      name: 'predicate-whitelist',
      whitelist: new Set([
        'http://example.org/hasFriend',
        'http://example.org/isFriendOf',
        'http://example.org/isConnected',
        'http://xmlns.com/foaf/0.1/name',
        'http://xmlns.com/foaf/0.1/knows',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
      ]),
      validate: function(quad) {
        const predicateValue = quad.predicate?.value || String(quad.predicate);
        return this.whitelist.has(predicateValue);
      },
    };

    const enforceResult = await substrate.enforce(deriveResult.derivedQuads, predicateWhitelistPolicy);
    console.log(`   ENFORCE: ${enforceResult.allowed} allowed | ${enforceResult.rejected} rejected`);
    console.log(`   Duration: ${enforceResult.duration_ms}ms`);
    console.log('');

    // ========================================================================
    // STEP 4: RENDER - Generate report
    // ========================================================================
    console.log('[STEP 4] RENDER: Generating JSON report...');

    const renderResult = await substrate.render(storeResult, deriveResult, enforceResult);
    console.log(`   RENDER: Report written to ${renderResult.outputPath}`);
    console.log(`   Duration: ${renderResult.duration_ms}ms`);
    console.log('');

    // ========================================================================
    // FINAL OUTPUT
    // ========================================================================
    console.log('='.repeat(64));
    console.log('                    SUBSTRATE DEMO COMPLETE');
    console.log('='.repeat(64));
    console.log('');
    console.log('+' + '-'.repeat(62) + '+');
    console.log('| STORE:   ' + `${storeResult.count} input quads`.padEnd(20) + ' | Hash: ' + storeResult.hash.padEnd(20) + ' |');
    console.log('| DERIVE:  ' + `+${deriveResult.count} derived quads`.padEnd(20) + ' | Hash: ' + deriveResult.hash.padEnd(20) + ' |');
    console.log('| ENFORCE: ' + `${enforceResult.allowed} allowed`.padEnd(20) + ' | ' + `${enforceResult.rejected} rejected`.padEnd(25) + ' |');
    console.log('| RENDER:  ' + 'JSON report written'.padEnd(20) + ' | result-report.json'.padEnd(25) + ' |');
    console.log('+' + '-'.repeat(62) + '+');
    console.log('');

    // Print summary
    console.log('SUMMARY:', renderResult.report.summary);
    console.log('');

    // Print sample receipts
    console.log('SAMPLE RECEIPTS (first 3):');
    for (let i = 0; i < Math.min(3, enforceResult.receipts.length); i++) {
      const r = enforceResult.receipts[i];
      console.log(`  [${i + 1}] ${r.policy}: ${r.result} - ${r.quad.predicate}`);
    }
    console.log('');

    // Verification
    console.log('VERIFICATION:');
    console.log('  [x] Store step completes (' + storeResult.count + ' quads)');
    console.log('  [x] Derive step completes (' + deriveResult.count + ' quads)');
    console.log('  [x] Enforce step completes (' + enforceResult.receipts.length + ' receipts)');
    console.log('  [x] Render step outputs JSON report + console summary');
    console.log('  [x] result-report.json written to disk');
    console.log('');

    console.log('PROOF: All 4 roles (STORE/DERIVE/ENFORCE/RENDER) executed successfully!');
    console.log('');

    process.exit(0);
  } catch (error) {
    console.error('FATAL ERROR:', error.message);
    console.error('Stack:', error.stack);
    process.exit(1);
  }
}

main();
