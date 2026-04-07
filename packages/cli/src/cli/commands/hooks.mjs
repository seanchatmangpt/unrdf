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
import { randomUUID } from 'node:crypto';
import { table } from 'table';
import { evaluateCondition, KnowledgeHookSchema } from '@unrdf/hooks';
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
    'show-effects': {
      type: 'boolean',
      description: 'Show effect execution details per hook',
      default: false,
    },
  },
  async run({ args }) {
    const { store: storeFile, config: configFile, output, 'show-effects': showEffects } = args;

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

      // Execute hooks: evaluate conditions and apply effects
      console.log('⚙️  Executing hooks...');
      const results = [];

      for (let i = 0; i < hooksConfig.length; i++) {
        const hookDef = hooksConfig[i];

        try {
          // Parse and validate hook definition
          const hook = KnowledgeHookSchema.parse(hookDef);

          if (hook.enabled === false) {
            results.push({
              id: hook.id,
              name: hook.meta?.name || 'Unnamed',
              success: true,
              satisfied: false,
              skipped: true,
              conditionKind: hook.condition?.kind,
              timestamp: new Date().toISOString(),
            });
            continue;
          }

          // Evaluate condition against the store
          const conditionResult = await evaluateCondition(hook.condition, rdfStore);
          const satisfied =
            conditionResult === true ||
            (Array.isArray(conditionResult) && conditionResult.length > 0);

          let effectResult = null;

          // Execute effect if condition is satisfied
          if (satisfied && hook.effect?.kind === 'sparql-construct') {
            const quads = rdfStore.query(hook.effect.query);
            let quadsAdded = 0;
            for (const quad of quads) {
              rdfStore.add(quad);
              quadsAdded++;
            }
            effectResult = { kind: 'sparql-construct', quadsAdded };
          }

          results.push({
            id: hook.id,
            name: hook.meta?.name || 'Unnamed',
            success: true,
            satisfied,
            conditionKind: hook.condition?.kind,
            effectResult,
            timestamp: new Date().toISOString(),
          });
        } catch (error) {
          results.push({
            id: hookDef.id || `hook-${i}`,
            name: hookDef.meta?.name || hookDef.name || 'Unnamed',
            success: false,
            error: error.message,
            timestamp: new Date().toISOString(),
          });
        }
      }

      // Display summary
      const satisfied = results.filter(r => r.success && r.satisfied).length;
      const notSatisfied = results.filter(r => r.success && !r.satisfied && !r.skipped).length;
      const skipped = results.filter(r => r.skipped).length;
      const failed = results.filter(r => !r.success).length;
      const quadsAdded = results
        .filter(r => r.effectResult)
        .reduce((sum, r) => sum + (r.effectResult?.quadsAdded || 0), 0);

      console.log('\n📊 Hook Execution Summary');
      console.log('═'.repeat(50));
      console.log(`Total Hooks:    ${results.length}`);
      console.log(`Satisfied:      ${satisfied}`);
      console.log(`Not Satisfied:  ${notSatisfied}`);
      if (skipped > 0) console.log(`Skipped:        ${skipped}`);
      console.log(`Errors:         ${failed}`);
      if (quadsAdded > 0) console.log(`Quads Added:    ${quadsAdded}`);
      console.log('═'.repeat(50));

      if (showEffects) {
        const withEffects = results.filter(r => r.effectResult);
        if (withEffects.length > 0) {
          console.log('\n🔧 Effect Details');
          withEffects.forEach(r => {
            console.log(
              `  ${r.name}: ${r.effectResult.kind} → +${r.effectResult.quadsAdded} quads`
            );
          });
        }
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
            quadsAdded,
          },
          results,
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
 * Generate hook templates from predefined templates
 */
const templateCommand = defineCommand({
  meta: {
    name: 'template',
    description: 'Generate hook templates from predefined templates',
  },
  args: {
    type: {
      type: 'string',
      description: 'Template type (fibo, security, compliance, generic)',
      default: 'generic',
    },
    output: {
      type: 'string',
      description: 'Output file for generated hooks (JSON)',
      required: false,
    },
  },
  async run({ args }) {
    const { type, output } = args;

    let template;

    // Define templates by type
    switch (type.toLowerCase()) {
      case 'fibo':
        template = generateFIBOTemplate();
        break;
      case 'security':
        template = generateSecurityTemplate();
        break;
      case 'compliance':
        template = generateComplianceTemplate();
        break;
      case 'generic':
      default:
        template = generateGenericTemplate();
    }

    if (output) {
      try {
        writeFileSync(output, JSON.stringify(template, null, 2));
        console.log(`✅ Template written to ${output}`);
        console.log(`📊 Generated ${template.length} hooks`);
      } catch (error) {
        console.error(`❌ Failed to write output: ${error.message}`);
        process.exit(1);
      }
    } else {
      console.log(JSON.stringify(template, null, 2));
    }
  },
});

/**
 * Generate FIBO financial regulatory compliance template
 */
function generateFIBOTemplate() {
  return [
    {
      id: randomUUID(),
      meta: {
        name: 'verify-regulatory-compliance',
        version: '1.0.0',
        description: 'Verify regulatory compliance status for FIBO trades',
        tags: ['fibo', 'compliance'],
      },
      condition: {
        kind: 'sparql-ask',
        query: `
          PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/>
          ASK {
            ?trade a fibo:Trade ;
                   fibo:hasComplianceStatus ?status .
          }
        `,
      },
      effect: {
        kind: 'sparql-construct',
        query: `
            PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/>
            CONSTRUCT {
              ?trade fibo:verifiedAt ?now ;
                     fibo:complianceVersion "1.0" .
            }
            WHERE {
              ?trade a fibo:Trade .
              BIND (NOW() as ?now)
            }
          `,
      },
      priority: 80,
      enabled: true,
    },
    {
      id: randomUUID(),
      meta: {
        name: 'assess-counterparty-risk',
        version: '1.0.0',
        description: 'Flag high-risk counterparties for manual approval',
        tags: ['fibo', 'risk'],
      },
      condition: {
        kind: 'datalog',
        facts: [],
        rules: ['high_risk(X) :- exposure(X, E), E > 1000000'],
        goal: 'high_risk(counterparty)',
      },
      effect: {
        kind: 'sparql-construct',
        query: `
            PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/>
            CONSTRUCT {
              ?party fibo:riskLevel fibo:HighRisk ;
                     fibo:requiresApproval true .
            }
            WHERE {
              ?party a fibo:Counterparty .
            }
          `,
      },
      priority: 80,
      enabled: true,
    },
    {
      id: randomUUID(),
      meta: {
        name: 'maintain-audit-trail',
        version: '1.0.0',
        description: 'Mark completed transactions as audited',
        tags: ['fibo', 'audit'],
      },
      condition: {
        kind: 'sparql-select',
        query: `
          PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/>
          SELECT ?transaction WHERE {
            ?transaction a fibo:Transaction ;
                        fibo:status fibo:Completed .
          }
        `,
      },
      effect: {
        kind: 'sparql-construct',
        query: `
            PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/>
            CONSTRUCT {
              ?transaction fibo:auditedAt ?now ;
                          fibo:auditStatus fibo:Audited .
            }
            WHERE {
              ?transaction a fibo:Transaction .
              BIND (NOW() as ?now)
            }
          `,
      },
      priority: 90,
      enabled: true,
    },
  ];
}

/**
 * Generate security template
 */
function generateSecurityTemplate() {
  return [
    {
      id: randomUUID(),
      meta: {
        name: 'validate-access-control',
        version: '1.0.0',
        description: 'Grant access to authorized users',
        tags: ['security', 'access-control'],
      },
      condition: {
        kind: 'datalog',
        facts: ['admin(alice)', 'user(bob)', 'role(admin)', 'role(user)'],
        rules: ['allowed(X) :- admin(X)', 'allowed(X) :- user(X)'],
        goal: 'allowed(alice)',
      },
      effect: {
        kind: 'sparql-construct',
        query: `
            PREFIX sec: <http://example.org/security/>
            CONSTRUCT {
              ?user sec:accessGranted true ;
                    sec:grantedAt ?now .
            }
            WHERE {
              ?user a sec:User .
              BIND (NOW() as ?now)
            }
          `,
      },
      priority: 90,
      enabled: true,
    },
  ];
}

/**
 * Generate compliance template
 */
function generateComplianceTemplate() {
  return [
    {
      id: randomUUID(),
      meta: {
        name: 'check-data-quality',
        version: '1.0.0',
        description: 'Annotate records with data quality scores',
        tags: ['compliance', 'data-quality'],
      },
      condition: {
        kind: 'threshold',
        query: 'SELECT (COUNT(?record) as ?count) WHERE { ?record a :Record }',
        operator: 'greaterThan',
        value: 0,
      },
      effect: {
        kind: 'sparql-construct',
        query: `
            PREFIX dq: <http://example.org/data-quality/>
            CONSTRUCT {
              ?record dq:qualityScore ?score ;
                      dq:checkedAt ?now .
            }
            WHERE {
              ?record a :Record .
              BIND (0.95 as ?score)
              BIND (NOW() as ?now)
            }
          `,
      },
      priority: 70,
      enabled: true,
    },
  ];
}

/**
 * Generate generic/example template
 */
function generateGenericTemplate() {
  return [
    {
      id: randomUUID(),
      meta: {
        name: 'example-sparql-ask',
        version: '1.0.0',
        description: 'Basic template demonstrating SPARQL ASK condition with CONSTRUCT effect',
      },
      condition: {
        kind: 'sparql-ask',
        query: 'ASK { ?s ?p ?o }',
      },
      effect: {
        kind: 'sparql-construct',
        query: `
            PREFIX ex: <http://example.org/>
            CONSTRUCT {
              ?s ex:processed true .
            }
            WHERE {
              ?s ?p ?o .
            }
          `,
      },
      priority: 50,
      enabled: true,
    },
  ];
}

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
    template: templateCommand,
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
