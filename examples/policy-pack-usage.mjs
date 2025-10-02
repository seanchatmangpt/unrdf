/**
 * Policy Pack Usage Example
 *
 * Demonstrates how to create, load, and manage policy packs for governance.
 * Policy packs bundle hooks, shapes, and rules into versioned governance units.
 */

import { PolicyPackManager, createPolicyPackManifest } from '../src/knowledge-engine/policy-pack.mjs';
import { defineHook } from '../src/knowledge-engine/define-hook.mjs';
import { mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';

// Create policy pack directory structure
const policyPackDir = resolve(process.cwd(), 'policy-packs', 'compliance-v1');
await mkdir(policyPackDir, { recursive: true });

console.log(`✓ Created policy pack directory: ${policyPackDir}\n`);

// Step 1: Create hooks for the policy pack

// Hook 1: Large transaction monitoring
const largeTxQuery = `
PREFIX ex: <http://example.org/>
PREFIX fibo: <https://spec.edmcouncil.org/fibo/ontology/>

ASK {
  ?tx a fibo:FinancialTransaction ;
      fibo:hasAmount ?amount .
  FILTER(?amount > 10000)
}
`;

await writeFile(
  resolve(policyPackDir, 'large-transaction.ask.rq'),
  largeTxQuery,
  'utf-8'
);

const largeTxHook = defineHook({
  meta: {
    name: 'compliance:large-transaction',
    description: 'Alert on transactions exceeding $10,000',
    ontology: ['fibo']
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://policy-packs/compliance-v1/large-transaction.ask.rq',
      sha256: 'mock-hash-123',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload }) {
    console.log('[HOOK] Large transaction detected!');
    return {
      result: {
        alert: 'Large transaction',
        amount: payload.amount,
        timestamp: Date.now()
      }
    };
  }
});

// Hook 2: Data quality check
const qualityQuery = `
PREFIX ex: <http://example.org/>

ASK {
  ?customer a ex:Customer .
  FILTER NOT EXISTS { ?customer ex:email ?email }
}
`;

await writeFile(
  resolve(policyPackDir, 'data-quality.ask.rq'),
  qualityQuery,
  'utf-8'
);

const qualityHook = defineHook({
  meta: {
    name: 'quality:missing-email',
    description: 'Detect customers without email addresses'
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://policy-packs/compliance-v1/data-quality.ask.rq',
      sha256: 'mock-hash-456',
      mediaType: 'application/sparql-query'
    }
  },
  async run({ payload }) {
    console.log('[HOOK] Data quality issue detected!');
    return {
      result: {
        alert: 'Missing email',
        severity: 'warning'
      }
    };
  }
});

console.log('✓ Created 2 hooks for policy pack\n');

// Step 2: Create SHACL shapes
const shaclShapes = `
@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:CustomerShape a sh:NodeShape ;
  sh:targetClass ex:Customer ;
  sh:property [
    sh:path ex:email ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    sh:message "Customer must have a valid email address" ;
  ] ;
  sh:property [
    sh:path ex:age ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150 ;
  ] .

ex:TransactionShape a sh:NodeShape ;
  sh:targetClass ex:Transaction ;
  sh:property [
    sh:path ex:amount ;
    sh:datatype xsd:decimal ;
    sh:minExclusive 0 ;
    sh:message "Transaction amount must be positive" ;
  ] ;
  sh:property [
    sh:path ex:currency ;
    sh:in ("USD" "EUR" "GBP") ;
    sh:message "Currency must be USD, EUR, or GBP" ;
  ] .
`;

await writeFile(
  resolve(policyPackDir, 'shapes.ttl'),
  shaclShapes,
  'utf-8'
);

console.log('✓ Created SHACL shapes\n');

// Step 3: Create policy pack manifest
const manifest = createPolicyPackManifest({
  name: 'compliance-v1',
  version: '1.0.0',
  description: 'Enterprise compliance policy pack',
  author: 'Compliance Team',
  hooks: [largeTxHook, qualityHook],
  shapes: ['policy-packs/compliance-v1/shapes.ttl'],
  metadata: {
    category: 'compliance',
    industry: 'financial-services',
    regulation: ['SOX', 'GDPR'],
    created: new Date().toISOString()
  }
});

await writeFile(
  resolve(policyPackDir, 'manifest.json'),
  JSON.stringify(manifest, null, 2),
  'utf-8'
);

console.log('✓ Created policy pack manifest\n');
console.log('Manifest:', JSON.stringify(manifest, null, 2));

// Step 4: Load and manage policy packs
console.log('\n--- Policy Pack Management ---\n');

const policyManager = new PolicyPackManager(process.cwd());

// Load all policy packs
await policyManager.loadAllPolicyPacks();

console.log('✓ Loaded policy packs');

// List available packs
const packs = policyManager.getAvailablePolicyPacks();
console.log('\nAvailable packs:', packs.map(p => p.name));

// Activate policy pack
policyManager.activatePolicyPack('compliance-v1');
console.log('✓ Activated compliance-v1');

// Get active packs
const activePacks = policyManager.getActivePolicyPacks();
console.log('\nActive packs:', activePacks.map(p => p.name));

// Get hooks from active packs
const activeHooks = policyManager.getActiveHooks();
console.log('\nActive hooks:', activeHooks.map(h => h.meta.name));

// Get specific policy pack
const pack = policyManager.getPolicyPack('compliance-v1');
console.log('\nPolicy pack details:');
console.log('  Name:', pack.name);
console.log('  Version:', pack.version);
console.log('  Hooks:', pack.getHooks().length);
console.log('  Shapes:', pack.getShapes().length);

// Validate policy pack
const validation = policyManager.validatePolicyPack('compliance-v1');
console.log('\nValidation:', validation.valid ? 'PASSED' : 'FAILED');
if (!validation.valid) {
  console.log('Errors:', validation.errors);
}

// Deactivate policy pack
policyManager.deactivatePolicyPack('compliance-v1');
console.log('\n✓ Deactivated compliance-v1');

// Verify deactivation
const activeAfterDeactivation = policyManager.getActivePolicyPacks();
console.log('Active packs after deactivation:', activeAfterDeactivation.length);

console.log('\n--- Policy Pack Versioning ---\n');

// Create version 2 of the policy pack
const manifestV2 = createPolicyPackManifest({
  name: 'compliance-v2',
  version: '2.0.0',
  description: 'Enterprise compliance policy pack (v2 with enhanced rules)',
  author: 'Compliance Team',
  hooks: [largeTxHook, qualityHook],
  shapes: ['policy-packs/compliance-v1/shapes.ttl'],
  dependencies: {
    'compliance-v1': '>=1.0.0'
  },
  metadata: {
    category: 'compliance',
    industry: 'financial-services',
    regulation: ['SOX', 'GDPR', 'PCI-DSS'],
    changelog: [
      'Added PCI-DSS compliance rules',
      'Enhanced transaction monitoring',
      'Improved data quality checks'
    ],
    created: new Date().toISOString()
  }
});

const policyPackDirV2 = resolve(process.cwd(), 'policy-packs', 'compliance-v2');
await mkdir(policyPackDirV2, { recursive: true });

await writeFile(
  resolve(policyPackDirV2, 'manifest.json'),
  JSON.stringify(manifestV2, null, 2),
  'utf-8'
);

console.log('✓ Created compliance-v2');

// Reload policy packs to include v2
await policyManager.loadAllPolicyPacks();

const allPacks = policyManager.getAvailablePolicyPacks();
console.log('\nAll available packs:', allPacks.map(p => `${p.name} v${p.version}`));

// Activate v2
policyManager.activatePolicyPack('compliance-v2');
console.log('✓ Activated compliance-v2');

// Check dependencies
const packV2 = policyManager.getPolicyPack('compliance-v2');
console.log('\nDependencies:', packV2.dependencies);

console.log('\n✓ Example complete!\n');

/**
 * Expected Output:
 *
 * ✓ Created policy pack directory: /path/to/policy-packs/compliance-v1
 * ✓ Created 2 hooks for policy pack
 * ✓ Created SHACL shapes
 * ✓ Created policy pack manifest
 *
 * Manifest: {
 *   "name": "compliance-v1",
 *   "version": "1.0.0",
 *   "description": "Enterprise compliance policy pack",
 *   "author": "Compliance Team",
 *   "hooks": [...],
 *   "shapes": ["policy-packs/compliance-v1/shapes.ttl"],
 *   "metadata": {
 *     "category": "compliance",
 *     "industry": "financial-services",
 *     "regulation": ["SOX", "GDPR"]
 *   }
 * }
 *
 * --- Policy Pack Management ---
 *
 * ✓ Loaded policy packs
 * Available packs: [ 'compliance-v1' ]
 * ✓ Activated compliance-v1
 * Active packs: [ 'compliance-v1' ]
 * Active hooks: [ 'compliance:large-transaction', 'quality:missing-email' ]
 *
 * Policy pack details:
 *   Name: compliance-v1
 *   Version: 1.0.0
 *   Hooks: 2
 *   Shapes: 1
 *
 * Validation: PASSED
 * ✓ Deactivated compliance-v1
 * Active packs after deactivation: 0
 *
 * --- Policy Pack Versioning ---
 *
 * ✓ Created compliance-v2
 * All available packs: [ 'compliance-v1 v1.0.0', 'compliance-v2 v2.0.0' ]
 * ✓ Activated compliance-v2
 * Dependencies: { 'compliance-v1': '>=1.0.0' }
 *
 * ✓ Example complete!
 */
