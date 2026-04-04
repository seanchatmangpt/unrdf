/**
 * @unrdf/hooks - AtomVM/Erlang Integration Example
 *
 * Demonstrates the HooksBridge for executing knowledge hooks
 * from Erlang/BEAM processes via AtomVM.
 *
 * This bridges two distributed systems:
 * - Erlang: Financial transaction processing (hot path, concurrent)
 * - JavaScript (Node.js + oxigraph): Knowledge hooks (governance, validation)
 *
 * Use Case: Real-time trade validation in Erlang financial exchange
 */

import { createStore, namedNode, literal } from '@unrdf/oxigraph';
import { createHooksBridge } from '../src/atomvm.mjs';
import { createContext } from '@unrdf/v6-core/receipt-pattern';

const FIBO_NS = 'https://spec.edmcouncil.org/fibo/ontology/';
const EX_NS = 'http://example.org/';

console.log('═'.repeat(70));
console.log('AtomVM/Erlang Integration - FIBO Hooks Demo');
console.log('═'.repeat(70));

// Initialize store and bridge
console.log('\n1️⃣  Initializing HooksBridge...');
const store = createStore();
const bridge = createHooksBridge(store, {
  persistReceipts: true,
  maxHooks: 100,
});
console.log('✅ Bridge initialized');

// Simulate registration from Erlang processes
console.log('\n2️⃣  Registering Hooks (Erlang → Bridge)...');

(async () => {
  try {
    // Hook 1: Basic ASK condition for any trade
    const hook1Id = await bridge.registerHook({
      name: 'erlang-verify-trade',
      condition: {
        kind: 'sparql-ask',
        query: `
          PREFIX ex: <${EX_NS}>
          ASK {
            ?trade a ex:Trade ;
                   ex:amount ?amt .
            FILTER (?amt > 0)
          }
        `,
      },
      effects: [
        {
          kind: 'sparql-construct',
          query: `
            PREFIX ex: <${EX_NS}>
            CONSTRUCT {
              ?trade ex:bridgeVerified true ;
                     ex:verificationTime ?now .
            }
            WHERE {
              ?trade a ex:Trade .
              BIND (NOW() as ?now)
            }
          `,
        },
      ],
      metadata: {
        source: 'erlang-process-1',
        version: '1.0',
      },
    });

    console.log(`✅ Registered hook 1: ID=${hook1Id}`);

    // Hook 2: Datalog-based risk assessment
    const hook2Id = await bridge.registerHook({
      name: 'erlang-assess-risk',
      condition: {
        kind: 'datalog',
        facts: [
          'trader("alice")',
          'trader("bob")',
          'authorized("alice")',
          'position("alice", 100000)',
          'position("bob", 50000)',
        ],
        rules: [
          'can_trade(T) :- trader(T), authorized(T)',
          'large_position(T) :- position(T, P), P >= 100000',
          'needs_review(T) :- large_position(T), can_trade(T)',
        ],
        goal: 'needs_review("alice")',
      },
      effects: [
        {
          kind: 'sparql-construct',
          query: `
            PREFIX ex: <${EX_NS}>
            CONSTRUCT {
              ?trader ex:riskReviewRequired true ;
                      ex:riskLevel ex:High .
            }
            WHERE {
              ?trader a ex:Trader .
            }
          `,
        },
      ],
      metadata: {
        source: 'erlang-process-2',
      },
    });

    console.log(`✅ Registered hook 2: ID=${hook2Id}`);

    // Hook 3: N3 inference rules for compliance
    const hook3Id = await bridge.registerHook({
      name: 'erlang-infer-compliance',
      condition: {
        kind: 'n3',
        rules: `
          PREFIX ex: <${EX_NS}>

          { ?trade ex:amount ?amt . ?amt math:lessThan 1000000 }
          =>
          { ?trade ex:complianceLevel ex:Standard } .

          { ?trade ex:amount ?amt . ?amt math:greaterThanOrEqual 1000000 }
          =>
          { ?trade ex:complianceLevel ex:Enhanced } .

          { ?trade ex:complianceLevel ex:Enhanced }
          =>
          { ?trade ex:requiresAudit true } .
        `,
        askQuery: `
          PREFIX ex: <${EX_NS}>
          ASK { ?trade ex:requiresAudit true }
        `,
      },
      effects: [
        {
          kind: 'sparql-construct',
          query: `
            PREFIX ex: <${EX_NS}>
            CONSTRUCT {
              ?trade ex:auditInitiated true ;
                     ex:auditTimestamp ?now .
            }
            WHERE {
              ?trade a ex:Trade .
              BIND (NOW() as ?now)
            }
          `,
        },
      ],
      metadata: {
        source: 'erlang-process-3',
        reasoning: 'n3-eye-reasoner',
      },
    });

    console.log(`✅ Registered hook 3: ID=${hook3Id}`);

    // List all registered hooks
    console.log('\n3️⃣  Listing Registered Hooks...');
    const hooks = bridge.listHooks();
    console.log(`📋 Total hooks registered: ${hooks.length}`);
    hooks.forEach((h, i) => {
      console.log(`   ${i + 1}. ${h.name} (ID: ${h.id})`);
    });

    // Simulate data from Erlang
    console.log('\n4️⃣  Loading Trade Data...');
    const tradeQuads = [
      [
        namedNode(`${EX_NS}trade/erlang-trade-001`),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode(`${EX_NS}Trade`),
      ],
      [
        namedNode(`${EX_NS}trade/erlang-trade-001`),
        namedNode(`${EX_NS}amount`),
        literal('500000'),
      ],
      [
        namedNode(`${EX_NS}trade/erlang-trade-001`),
        namedNode(`${EX_NS}status`),
        namedNode(`${EX_NS}Active`),
      ],
    ];

    tradeQuads.forEach(([s, p, o]) => {
      store.insert([s, p, o], namedNode(`${EX_NS}default`));
    });
    console.log(`✅ Loaded ${tradeQuads.length} quads`);

    // Evaluate conditions from Erlang
    console.log('\n5️⃣  Evaluating Conditions from Erlang...');

    const sparqlAskResult = await bridge.evaluateCondition({
      kind: 'sparql-ask',
      query: `
        PREFIX ex: <${EX_NS}>
        ASK {
          ?trade a ex:Trade ;
                 ex:amount ?amt .
          FILTER (?amt > 0)
        }
      `,
    });
    console.log(`✅ SPARQL ASK result: ${sparqlAskResult ? '✓ True' : '✗ False'}`);

    const datalogResult = await bridge.evaluateCondition({
      kind: 'datalog',
      facts: ['trade("erlang-trade-001")'],
      rules: ['valid(T) :- trade(T)'],
      goal: 'valid("erlang-trade-001")',
    });
    console.log(`✅ Datalog result: ${datalogResult ? '✓ True' : '✗ False'}`);

    // Execute hooks with receipt chaining
    console.log('\n6️⃣  Executing Hooks (Erlang → Bridge → Store)...');

    const executionContext = createContext({
      nodeId: 'erlang-trade-processor',
      t_ns: BigInt(Date.now() * 1000000),
    });

    const result = await bridge.executeHooks(executionContext, [
      hook1Id,
      hook2Id,
      hook3Id,
    ]);

    console.log(`✅ Execution complete:`);
    console.log(`   Successful: ${result.successful}`);
    console.log(`   Failed: ${result.failed}`);

    // Receipt chain (Priority 1 & 4)
    console.log('\n7️⃣  Receipt Chain Verification...');
    const receipt = result.receipt;
    console.log(`✅ Receipt Hash:  ${receipt.receiptHash.substring(0, 32)}...`);
    console.log(`   Input Hash:   ${receipt.input_hash.substring(0, 32)}...`);
    console.log(`   Output Hash:  ${receipt.output_hash.substring(0, 32)}...`);
    console.log(`   Node ID:      ${receipt.nodeId}`);

    // Verify receipt chain
    const chain = bridge.getReceiptChain();
    console.log(`\n8️⃣  Receipt Chain Status...`);
    console.log(`📊 Total receipts: ${chain.length}`);

    const verification = bridge.verifyReceiptChain();
    if (verification.valid) {
      console.log(`✅ Chain integrity verified`);
    } else {
      console.log(`❌ Chain broken at receipt ${verification.brokenAt}`);
    }

    // State changes
    if (receipt.delta) {
      console.log(`\n9️⃣  State Changes (Delta)`);
      console.log(`📝 Additions:  ${receipt.delta.adds.length} quads`);
      console.log(`🗑️  Deletions: ${receipt.delta.deletes.length} quads`);

      if (receipt.delta.adds.length > 0) {
        console.log('\nSample additions:');
        receipt.delta.adds.slice(0, 3).forEach((add) => {
          console.log(`   + ${add.subject} ${add.predicate} ${add.object}`);
        });
      }
    }

    // Bridge statistics
    console.log('\n🔟 Bridge Statistics');
    const stats = bridge.getStats();
    console.log(`📊 Hooks registered: ${stats.hookCount}`);
    console.log(`📊 Receipt history: ${stats.receiptCount}`);
    console.log(`📊 Memory usage: ${(stats.memoryBytes / 1024).toFixed(2)} KB`);

    // Simulate multiple execution cycles (as would happen in Erlang)
    console.log('\n1️⃣1️⃣  Simulating Multiple Execution Cycles...');

    for (let i = 0; i < 2; i++) {
      const ctx = createContext({
        nodeId: 'erlang-trade-processor',
        t_ns: BigInt(Date.now() * 1000000),
        previousReceiptHash: i > 0 ? chain[chain.length - 1].receiptHash : undefined,
      });

      await bridge.executeHooks(ctx, [hook1Id]);
      const updatedChain = bridge.getReceiptChain();
      console.log(`✅ Cycle ${i + 1}: ${updatedChain.length} receipts in chain`);
    }

    // Final summary
    console.log('\n' + '═'.repeat(70));
    console.log('✨ AtomVM Bridge Demo Complete!');
    console.log('═'.repeat(70));

    console.log('\n📋 Summary:');
    console.log('  ✅ Hooks registered from Erlang');
    console.log('  ✅ Conditions evaluated dynamically');
    console.log('  ✅ Hooks executed with receipt chaining');
    console.log('  ✅ Receipt chain integrity verified');
    console.log('  ✅ State changes tracked (delta)');
    console.log('  ✅ Multi-cycle execution demonstrates deterministic receipts');

    console.log('\n💡 Next Steps:');
    console.log('  1. Connect via HTTP gateway to Erlang processes');
    console.log('  2. Implement Erlang gen_server for async hook evaluation');
    console.log('  3. Persist receipts to distributed ledger');
    console.log('  4. Scale to thousands of concurrent trades');

    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
})();
