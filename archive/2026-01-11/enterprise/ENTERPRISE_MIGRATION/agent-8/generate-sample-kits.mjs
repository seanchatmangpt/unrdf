/**
 * @fileoverview Generate sample kits for oxigraph and hooks domains
 * @module agent-8/generate-sample-kits
 */

import { writeFileSync, mkdirSync } from 'fs';
import { generateDomainKit, writeKitToFS, generateKitSummary } from './kit-generator.mjs';

/**
 * Sample contracts for oxigraph domain
 */
const oxigraphContracts = [
  {
    operation: 'createStore',
    description: 'Create RDF store instance',
    params: [
      { name: 'config', type: 'Object', description: 'Store configuration' }
    ],
    returns: { type: 'Store', description: 'Store instance' },
    lens: {
      type: 'identity',
      view: (input) => input,
      review: (output) => output
    }
  },
  {
    operation: 'addQuad',
    description: 'Add quad to store',
    params: [
      { name: 'store', type: 'Store', description: 'Store instance' },
      { name: 'quad', type: 'Quad', description: 'Quad to add' }
    ],
    returns: { type: 'boolean', description: 'Success flag' },
    lens: {
      type: 'transform',
      view: ({ store, quad }) => ({ store, quad }),
      review: (result) => result
    }
  },
  {
    operation: 'query',
    description: 'Execute SPARQL query',
    params: [
      { name: 'store', type: 'Store', description: 'Store instance' },
      { name: 'sparql', type: 'string', description: 'SPARQL query' }
    ],
    returns: { type: 'Array', description: 'Query results' },
    lens: {
      type: 'query',
      view: ({ store, sparql }) => ({ store, query: sparql }),
      review: (results) => results.bindings || results
    }
  }
];

/**
 * Sample contracts for hooks domain
 */
const hooksContracts = [
  {
    operation: 'registerHook',
    description: 'Register knowledge hook',
    params: [
      { name: 'name', type: 'string', description: 'Hook name' },
      { name: 'handler', type: 'Function', description: 'Hook handler' }
    ],
    returns: { type: 'Object', description: 'Hook registration' },
    lens: {
      type: 'register',
      view: ({ name, handler }) => ({ id: name, fn: handler }),
      review: (reg) => ({ name: reg.id, registered: true })
    }
  },
  {
    operation: 'executeHook',
    description: 'Execute registered hook',
    params: [
      { name: 'name', type: 'string', description: 'Hook name' },
      { name: 'context', type: 'Object', description: 'Execution context' }
    ],
    returns: { type: 'Object', description: 'Execution result' },
    lens: {
      type: 'execute',
      view: ({ name, context }) => ({ hookId: name, ctx: context }),
      review: (result) => result
    }
  },
  {
    operation: 'applyPolicyPack',
    description: 'Apply policy pack to store',
    params: [
      { name: 'store', type: 'Store', description: 'Store instance' },
      { name: 'policyPack', type: 'Object', description: 'Policy pack' }
    ],
    returns: { type: 'Object', description: 'Application result' },
    lens: {
      type: 'policy',
      view: ({ store, policyPack }) => ({ target: store, policies: policyPack }),
      review: (result) => ({ applied: result.count, errors: result.errors || [] })
    }
  }
];

/**
 * Generate and write kits
 */
function generateSampleKits() {
  const outputPath = '/home/user/unrdf/ENTERPRISE_MIGRATION/agent-8/kits';

  console.log('Generating domain kits...\n');

  // Generate oxigraph kit
  console.log('Generating oxigraph kit...');
  const oxigraphKit = generateDomainKit('oxigraph', oxigraphContracts);
  const oxigraphFiles = writeKitToFS(oxigraphKit, outputPath);

  // Write oxigraph files
  mkdirSync(`${outputPath}/oxigraph`, { recursive: true });
  writeFileSync(oxigraphFiles.files.facade, oxigraphFiles.contents.facade);
  writeFileSync(oxigraphFiles.files.adapters, oxigraphFiles.contents.adapters);
  writeFileSync(oxigraphFiles.files.scenarios, oxigraphFiles.contents.scenarios);
  writeFileSync(oxigraphFiles.files.metadata, oxigraphFiles.contents.metadata);

  const oxigraphSummary = generateKitSummary(oxigraphKit);
  console.log('Oxigraph kit:', JSON.stringify(oxigraphSummary, null, 2));

  // Generate hooks kit
  console.log('\nGenerating hooks kit...');
  const hooksKit = generateDomainKit('hooks', hooksContracts);
  const hooksFiles = writeKitToFS(hooksKit, outputPath);

  // Write hooks files
  mkdirSync(`${outputPath}/hooks`, { recursive: true });
  writeFileSync(hooksFiles.files.facade, hooksFiles.contents.facade);
  writeFileSync(hooksFiles.files.adapters, hooksFiles.contents.adapters);
  writeFileSync(hooksFiles.files.scenarios, hooksFiles.contents.scenarios);
  writeFileSync(hooksFiles.files.metadata, hooksFiles.contents.metadata);

  const hooksSummary = generateKitSummary(hooksKit);
  console.log('Hooks kit:', JSON.stringify(hooksSummary, null, 2));

  console.log('\n✅ Sample kits generated successfully');
  console.log('\nGenerated files:');
  console.log('- ', oxigraphFiles.files.facade);
  console.log('- ', oxigraphFiles.files.adapters);
  console.log('- ', oxigraphFiles.files.scenarios);
  console.log('- ', oxigraphFiles.files.metadata);
  console.log('- ', hooksFiles.files.facade);
  console.log('- ', hooksFiles.files.adapters);
  console.log('- ', hooksFiles.files.scenarios);
  console.log('- ', hooksFiles.files.metadata);

  return {
    oxigraph: oxigraphSummary,
    hooks: hooksSummary
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  generateSampleKits();
}

export { generateSampleKits, oxigraphContracts, hooksContracts };
