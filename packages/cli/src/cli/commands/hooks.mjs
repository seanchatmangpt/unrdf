/**
 * Hooks Command Suite - Knowledge Hook Management
 *
 * Commands for defining, evaluating, and executing knowledge hooks with
 * receipt tracking, condition evaluation, and governance enforcement.
 *
 * @module cli/commands/hooks
 */

import { defineCommand } from 'citty';
import { readFileSync, writeFileSync, existsSync } from 'node:fs';
import { table } from 'table';
import {
  executeHook,
  evaluateCondition,
  validateKnowledgeHook,
  KnowledgeHookSchema,
} from '@unrdf/hooks';
import { createStore } from '@unrdf/core';
import { Parser } from '@unrdf/core/rdf/n3-justified-only';

/**
 * Execute hooks against an RDF store
 */
const executeCommand = defineCommand({
  meta: {
    name: 'execute',
    description: 'Execute hooks against an RDF store',
  },
  args: {
    store: {
      type: 'string',
      description: 'Store file (NQ/Turtle/N-Triples)',
      required: true,
    },
    config: {
      type: 'string',
      description: 'Hooks config file (JSON)',
      required: true,
    },
    output: {
      type: 'string',
      description: 'Output file for results (JSON)',
      required: false,
    },
    'show-receipts': {
      type: 'boolean',
      description: 'Show receipt chain with hashes',
      default: false,
    },
  },
  async run({ args }) {
    const { store: storeFile, config: configFile, output, 'show-receipts': showReceipts } = args;

    // Validate inputs
    if (!existsSync(storeFile)) {
      console.error(`❌ Store file not found: ${storeFile}`);
      process.exit(1);
    }
    if (!existsSync(configFile)) {
      console.error(`❌ Config file not found: ${configFile}`);
      process.exit(1);
    }

    try {
      // Load store
      console.log('📂 Loading store...');
      const storeContent = readFileSync(storeFile, 'utf-8');
      const rdfStore = createStore();

      const parser = new Parser({ format: detectFormat(storeFile) });
      let quadCount = 0;

      await new Promise((resolve, reject) => {
        parser.parse(storeContent, (error, quad) => {
          if (error) {
            reject(error);
            return;
          }
          if (quad) {
            rdfStore.add(quad);
            quadCount++;
          } else {
            resolve();
          }
        });
      });

      console.log(`✅ Loaded ${quadCount} quads`);

      // Load hooks config
      console.log('📋 Loading hooks config...');
      const configContent = readFileSync(configFile, 'utf-8');
      const hooksConfig = JSON.parse(configContent);

      if (!Array.isArray(hooksConfig)) {
        console.error('❌ Config must be an array of hook definitions');
        process.exit(1);
      }

      console.log(`✅ Loaded ${hooksConfig.length} hooks`);

      // Execute hooks
      console.log('⚙️  Executing hooks...');
      const results = [];
      const receipts = [];

      for (let i = 0; i < hooksConfig.length; i++) {
        const hookDef = hooksConfig[i];

        try {
          // Validate hook definition
          const validated = validateKnowledgeHook(hookDef);

          // Execute hook
          const result = await executeHook(validated, rdfStore);

          results.push({
            id: hookDef.id || `hook-${i}`,
            name: hookDef.name || 'Unnamed',
            success: true,
            result: result,
            timestamp: new Date().toISOString(),
          });

          // Track receipt if available
          if (result.receipt) {
            receipts.push({
              hookId: hookDef.id || `hook-${i}`,
              receiptHash: result.receipt.receiptHash,
              previousHash: result.receipt.previousReceiptHash,
              timestamp: result.receipt.timestamp_iso,
            });
          }
        } catch (error) {
          results.push({
            id: hookDef.id || `hook-${i}`,
            name: hookDef.name || 'Unnamed',
            success: false,
            error: error.message,
            timestamp: new Date().toISOString(),
          });
        }
      }

      // Display summary
      const successful = results.filter(r => r.success).length;
      const failed = results.filter(r => !r.success).length;

      console.log('\n📊 Execution Summary');
      console.log('═'.repeat(50));
      console.log(`Total Hooks:    ${results.length}`);
      console.log(`Successful:     ${successful}`);
      console.log(`Failed:         ${failed}`);
      console.log(`Success Rate:   ${Math.round((successful / results.length) * 100)}%`);
      console.log('═'.repeat(50));

      // Display receipt chain if requested
      if (showReceipts && receipts.length > 0) {
        console.log('\n🔐 Receipt Chain');
        console.log('═'.repeat(50));
        const receiptData = receipts.map((r, idx) => [
          idx + 1,
          r.hookId,
          r.receiptHash.substring(0, 16) + '...',
          r.previousHash ? r.previousHash.substring(0, 16) + '...' : '(genesis)',
        ]);

        const tableData = [['#', 'Hook ID', 'Receipt Hash', 'Previous Hash'], ...receiptData];

        console.log(
          table(tableData, {
            header: {
              alignment: 'center',
              content: `Receipt Chain (${receipts.length})`,
            },
          })
        );
      }

      // Save to file if requested
      if (output) {
        const outputData = {
          metadata: {
            timestamp: new Date().toISOString(),
            storeFile,
            configFile,
            quadsLoaded: quadCount,
            hooksExecuted: results.length,
          },
          results,
          receipts: showReceipts ? receipts : [],
        };

        writeFileSync(output, JSON.stringify(outputData, null, 2), 'utf-8');
        console.log(`\n✅ Results saved to: ${output}`);
      }
    } catch (error) {
      console.error(`❌ Execution error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Define hooks from config file
 */
const defineHooksCommand = defineCommand({
  meta: {
    name: 'define',
    description: 'Define hooks from config file',
  },
  args: {
    config: {
      type: 'string',
      description: 'Hooks config file (JSON)',
      required: true,
    },
    validate: {
      type: 'boolean',
      description: 'Validate without executing',
      default: false,
    },
    output: {
      type: 'string',
      description: 'Output file for hook metadata (JSON)',
      required: false,
    },
  },
  async run({ args }) {
    const { config: configFile, validate: validateOnly, output } = args;

    if (!existsSync(configFile)) {
      console.error(`❌ Config file not found: ${configFile}`);
      process.exit(1);
    }

    try {
      console.log('📋 Loading hooks config...');
      const configContent = readFileSync(configFile, 'utf-8');
      const hooksConfig = JSON.parse(configContent);

      if (!Array.isArray(hooksConfig)) {
        console.error('❌ Config must be an array of hook definitions');
        process.exit(1);
      }

      const metadata = [];
      let validCount = 0;
      let invalidCount = 0;

      for (let i = 0; i < hooksConfig.length; i++) {
        const hookDef = hooksConfig[i];

        try {
          // Validate against schema using safeParse
          const result = KnowledgeHookSchema.safeParse(hookDef);

          if (result.success) {
            validCount++;

            metadata.push({
              index: i,
              id: hookDef.id || `hook-${i}`,
              name: hookDef.name || 'Unnamed',
              valid: true,
              conditions: hookDef.condition
                ? Array.isArray(hookDef.condition)
                  ? hookDef.condition.map(c => c.kind)
                  : [hookDef.condition.kind]
                : [],
              hasEffect: !!hookDef.effect,
              effectType: hookDef.effect ? hookDef.effect.kind : null,
            });
          } else {
            invalidCount++;
            const errorMsg = result.error.errors
              .map(e => `${e.path.join('.')}: ${e.message}`)
              .join('; ');
            metadata.push({
              index: i,
              id: hookDef.id || `hook-${i}`,
              name: hookDef.name || 'Unnamed',
              valid: false,
              error: errorMsg,
            });
          }
        } catch (error) {
          invalidCount++;
          metadata.push({
            index: i,
            id: hookDef.id || `hook-${i}`,
            name: hookDef.name || 'Unnamed',
            valid: false,
            error: error.message,
          });
        }
      }

      console.log('\n✅ Hook Definitions');
      console.log('═'.repeat(50));
      console.log(`Total Hooks:    ${hooksConfig.length}`);
      console.log(`Valid:          ${validCount}`);
      console.log(`Invalid:        ${invalidCount}`);
      console.log('═'.repeat(50));

      // Display as table
      const tableData = metadata.map(m => [
        m.index + 1,
        m.id,
        m.name,
        m.valid ? '✅' : '❌',
        m.valid ? m.conditions?.join(', ') || '-' : '-',
        m.valid ? (m.hasEffect ? m.effectType : '-') : '-',
      ]);

      const headerData = [['#', 'ID', 'Name', 'Valid', 'Conditions', 'Effect Type']];
      console.log('\n');
      console.log(
        table([...headerData, ...tableData], {
          header: {
            alignment: 'center',
            content: `Hook Definitions (${hooksConfig.length})`,
          },
        })
      );

      // Show errors if any
      const errors = metadata.filter(m => !m.valid);
      if (errors.length > 0) {
        console.log('\n⚠️  Validation Errors:');
        errors.forEach(err => {
          console.log(`  - ${err.id}: ${err.error}`);
        });
      }

      // Save metadata if requested
      if (output) {
        const outputData = {
          timestamp: new Date().toISOString(),
          configFile,
          summary: {
            total: hooksConfig.length,
            valid: validCount,
            invalid: invalidCount,
          },
          hooks: metadata,
        };

        writeFileSync(output, JSON.stringify(outputData, null, 2), 'utf-8');
        console.log(`\n✅ Metadata saved to: ${output}`);
      }

      if (!validateOnly && invalidCount === 0) {
        console.log('\n✨ All hooks are valid and ready for execution');
      }
    } catch (error) {
      console.error(`❌ Definition error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Evaluate a single condition
 */
const evaluateConditionCommand = defineCommand({
  meta: {
    name: 'evaluate-condition',
    description: 'Evaluate a single condition against a store',
  },
  args: {
    store: {
      type: 'string',
      description: 'Store file (NQ/Turtle/N-Triples)',
      required: true,
    },
    condition: {
      type: 'string',
      description:
        'Condition type (sparql-ask, sparql-select, shacl, delta, threshold, count, window, n3, datalog)',
      required: true,
    },
    config: {
      type: 'string',
      description: 'Condition config as JSON',
      required: true,
    },
  },
  async run({ args }) {
    const { store: storeFile, condition: conditionType, config: configStr } = args;

    if (!existsSync(storeFile)) {
      console.error(`❌ Store file not found: ${storeFile}`);
      process.exit(1);
    }

    try {
      // Load store
      console.log('📂 Loading store...');
      const storeContent = readFileSync(storeFile, 'utf-8');
      const rdfStore = createStore();

      const parser = new Parser({ format: detectFormat(storeFile) });
      let quadCount = 0;

      await new Promise((resolve, reject) => {
        parser.parse(storeContent, (error, quad) => {
          if (error) {
            reject(error);
            return;
          }
          if (quad) {
            rdfStore.add(quad);
            quadCount++;
          } else {
            resolve();
          }
        });
      });

      console.log(`✅ Loaded ${quadCount} quads`);

      // Parse condition config
      console.log('📋 Parsing condition config...');
      const conditionConfig = JSON.parse(configStr);
      conditionConfig.kind = conditionType;

      // Evaluate condition
      console.log(`⚙️  Evaluating condition (${conditionType})...`);
      const result = await evaluateCondition(conditionConfig, rdfStore);

      // Display result
      console.log('\n📊 Evaluation Result');
      console.log('═'.repeat(50));
      console.log(`Condition Type: ${conditionType}`);
      console.log(`Store Quads:    ${quadCount}`);
      console.log(`Result Type:    ${typeof result}`);
      console.log('═'.repeat(50));

      if (typeof result === 'boolean') {
        console.log(`Result:         ${result ? '✅ TRUE' : '❌ FALSE'}`);
      } else if (Array.isArray(result)) {
        console.log(`Result Count:   ${result.length} items`);
        if (result.length > 0) {
          console.log('\nFirst few results:');
          result.slice(0, 5).forEach((item, idx) => {
            console.log(`  ${idx + 1}. ${JSON.stringify(item)}`);
          });
          if (result.length > 5) {
            console.log(`  ... and ${result.length - 5} more`);
          }
        }
      } else {
        console.log('Result:');
        console.log(JSON.stringify(result, null, 2));
      }
    } catch (error) {
      console.error(`❌ Evaluation error: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * List available condition kinds
 */
const listConditionsCommand = defineCommand({
  meta: {
    name: 'list-conditions',
    description: 'List available condition kinds',
  },
  async run() {
    const conditions = [
      {
        kind: 'sparql-ask',
        description: 'SPARQL ASK query - returns boolean',
        example: '{ "query": "ASK { ?s ?p ?o }" }',
      },
      {
        kind: 'sparql-select',
        description: 'SPARQL SELECT query - returns result set',
        example: '{ "query": "SELECT ?s WHERE { ?s ?p ?o }" }',
      },
      {
        kind: 'shacl',
        description: 'SHACL shape validation',
        example: '{ "ref": { "uri": "path/to/shapes.ttl", "sha256": "..." } }',
      },
      {
        kind: 'delta',
        description: 'Delta change detection - detects RDF updates',
        example: '{ "spec": { "type": "additions" } }',
      },
      {
        kind: 'threshold',
        description: 'Numeric threshold comparison',
        example: '{ "property": "score", "operator": ">", "value": 0.8 }',
      },
      {
        kind: 'count',
        description: 'Count of matching patterns',
        example: '{ "query": "SELECT (COUNT(*) as ?count) WHERE { ... }" }',
      },
      {
        kind: 'window',
        description: 'Time window evaluation (events in time range)',
        example: '{ "startTime": "2024-01-01", "endTime": "2024-12-31" }',
      },
      {
        kind: 'n3',
        description: 'N3 logic rules evaluation',
        example:
          '{ "rules": "@prefix : <http://example.org/>. { ?s :prop ?o } => { ?s :inferred true }." }',
      },
      {
        kind: 'datalog',
        description: 'Datalog rule evaluation',
        example:
          '{ "rules": ["parent(alice, bob).", "ancestor(X,Y) :- parent(X,Y)."], "goal": "ancestor(alice, bob)" }',
      },
    ];

    console.log('\n🔍 Available Condition Kinds\n');
    console.log('═'.repeat(80));

    conditions.forEach((cond, idx) => {
      console.log(`\n${idx + 1}. ${cond.kind}`);
      console.log(`   Description: ${cond.description}`);
      console.log(`   Example:     ${cond.example}`);
    });

    console.log('\n' + '═'.repeat(80));
    console.log(`\nTotal: ${conditions.length} condition kinds available\n`);
  },
});

/**
 * Display receipt chain from execution result
 */
const receiptsCommand = defineCommand({
  meta: {
    name: 'receipts',
    description: 'Display receipt chain from hook execution results',
  },
  args: {
    file: {
      type: 'string',
      description: 'Execution result file (JSON)',
      required: true,
    },
    format: {
      type: 'string',
      description: 'Output format (json, table)',
      default: 'table',
    },
    verify: {
      type: 'boolean',
      description: 'Verify receipt chain integrity',
      default: false,
    },
  },
  async run({ args }) {
    const { file, format, verify } = args;

    if (!existsSync(file)) {
      console.error(`❌ File not found: ${file}`);
      process.exit(1);
    }

    try {
      console.log('📂 Loading results file...');
      const content = readFileSync(file, 'utf-8');
      const results = JSON.parse(content);

      const receipts = results.receipts || [];

      if (receipts.length === 0) {
        console.log('⚠️  No receipts found in results file');
        return;
      }

      console.log(`✅ Loaded ${receipts.length} receipts`);

      // Display receipts
      if (format === 'json') {
        console.log(JSON.stringify(receipts, null, 2));
      } else {
        // Table format
        const tableData = receipts.map((receipt, idx) => [
          idx + 1,
          receipt.hookId,
          receipt.receiptHash.substring(0, 16) + '...',
          receipt.previousHash ? receipt.previousHash.substring(0, 16) + '...' : '(genesis)',
          receipt.timestamp ? receipt.timestamp.substring(0, 19) : '-',
        ]);

        const headerData = [['#', 'Hook ID', 'Receipt Hash', 'Previous Hash', 'Timestamp']];

        console.log('\n');
        console.log(
          table([...headerData, ...tableData], {
            header: {
              alignment: 'center',
              content: `Receipt Chain (${receipts.length})`,
            },
          })
        );
      }

      // Verify chain if requested
      let chainValid = null;
      if (verify) {
        console.log('\n🔐 Verifying Receipt Chain Integrity...');
        chainValid = true;

        for (let i = 0; i < receipts.length; i++) {
          const current = receipts[i];
          const previous = i > 0 ? receipts[i - 1] : null;

          if (previous && current.previousHash !== previous.receiptHash) {
            console.log(`❌ Chain break detected at receipt ${i}`);
            chainValid = false;
            break;
          }

          if (i === 0 && current.previousHash !== null && current.previousHash !== '(genesis)') {
            console.log(`⚠️  Genesis receipt should have null previousHash`);
          }
        }

        if (chainValid) {
          console.log('✅ Receipt chain is valid and integrity verified');
        }
      }

      console.log('\n📊 Summary');
      console.log('═'.repeat(50));
      console.log(`Total Receipts: ${receipts.length}`);
      console.log(`Chain Valid:    ${verify ? (chainValid ? '✅ Yes' : '❌ No') : 'Not checked'}`);
      console.log('═'.repeat(50));
    } catch (error) {
      console.error(`❌ Error processing receipts: ${error.message}`);
      process.exit(1);
    }
  },
});

/**
 * Main hooks command
 */
export const hooksCommand = defineCommand({
  meta: {
    name: 'hooks',
    description: 'Knowledge hook definition and execution',
  },
  subCommands: {
    execute: executeCommand,
    define: defineHooksCommand,
    'evaluate-condition': evaluateConditionCommand,
    'list-conditions': listConditionsCommand,
    receipts: receiptsCommand,
  },
});

// Helper functions

/**
 * Detect RDF format from file extension
 */
function detectFormat(filename) {
  const ext = filename.split('.').pop().toLowerCase();
  switch (ext) {
    case 'ttl':
      return 'Turtle';
    case 'nt':
      return 'N-Triples';
    case 'nq':
      return 'N-Quads';
    case 'jsonld':
      return 'JSON-LD';
    case 'rdf':
    case 'xml':
      return 'RDF/XML';
    default:
      return 'Turtle';
  }
}
