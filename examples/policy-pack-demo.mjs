#!/usr/bin/env node

/**
 * @file Policy Pack Demo
 * @description
 * Demonstrates PolicyPack management and governance units.
 */

import {
  PolicyPackManager,
  createPolicyPackManifest,
} from '../packages/knowledge-engine/src/policy-pack.mjs';
import { KnowledgeHookManager } from '../packages/knowledge-engine/src/knowledge-hook-manager.mjs';
import { createStore } from '../packages/oxigraph/src/index.mjs';

console.log('📦 Policy Pack Demo\n');

async function demonstratePolicyPacks() {
  try {
    // Create a policy pack manager
    const packManager = new PolicyPackManager();

    console.log('📦 Policy Pack Manager created');

    // Create a compliance policy pack manifest
    const complianceManifest = createPolicyPackManifest(
      'sox-compliance',
      [
        {
          meta: {
            name: 'audit-trail-hook',
            description: 'Ensures audit trail compliance',
            version: 'latest',
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: 'file://audit-trail.rq',
              sha256: 'audit-hash',
              mediaType: 'application/sparql-query',
            },
          },
          run: async _event => {
            console.log('Audit trail compliance check executed');
            return { compliant: true };
          },
        },
        {
          meta: {
            name: 'data-retention-hook',
            description: 'Enforces data retention policies',
            version: 'latest',
          },
          when: {
            kind: 'shacl',
            ref: {
              uri: 'file://retention.shacl',
              sha256: 'retention-hash',
              mediaType: 'text/turtle',
            },
          },
          run: async _event => {
            console.log('Data retention policy check executed');
            return { compliant: true };
          },
        },
      ],
      {
        description: 'SOX Compliance Policy Pack for financial data governance',
        author: 'compliance-team',
        version: 'latest',
        license: 'MIT',
        tags: ['compliance', 'sox', 'financial'],
        ontology: ['http://example.org/sox-ontology'],
      }
    );

    console.log('📋 SOX Compliance Policy Pack manifest created');
    console.log(`  Name: ${complianceManifest.meta.name}`);
    console.log(`  Version: ${complianceManifest.meta.version}`);
    console.log(`  Hooks: ${complianceManifest.hooks.length}`);

    // Create a data governance policy pack
    const governanceManifest = createPolicyPackManifest(
      'data-governance',
      [
        {
          meta: {
            name: 'privacy-hook',
            description: 'Enforces privacy regulations',
            version: 'latest',
          },
          when: {
            kind: 'sparql-ask',
            ref: {
              uri: 'file://privacy.rq',
              sha256: 'privacy-hash',
              mediaType: 'application/sparql-query',
            },
          },
          run: async _event => {
            console.log('Privacy compliance check executed');
            return { compliant: true };
          },
        },
      ],
      {
        description: 'Data Governance Policy Pack for privacy and data quality',
        author: 'data-team',
        version: 'latest',
        license: 'Apache-2.0',
        tags: ['governance', 'privacy', 'data-quality'],
      }
    );

    console.log('📋 Data Governance Policy Pack manifest created');
    console.log(`  Name: ${governanceManifest.meta.name}`);
    console.log(`  Version: ${governanceManifest.meta.version}`);
    console.log(`  Hooks: ${governanceManifest.hooks.length}`);

    // Create policy pack instances
    const compliancePack = new (await import('../src/knowledge-engine/policy-pack.mjs')).PolicyPack(
      complianceManifest
    );
    const governancePack = new (await import('../src/knowledge-engine/policy-pack.mjs')).PolicyPack(
      governanceManifest
    );

    // Test compatibility checks
    console.log('\n🔍 Testing compatibility checks...');

    const devEnvironment = {
      version: 'latest',
      environment: 'development',
      features: ['sparql', 'shacl', 'audit'],
    };

    const prodEnvironment = {
      version: 'latest',
      environment: 'production',
      features: ['sparql', 'shacl', 'audit', 'monitoring'],
    };

    const complianceCompat = compliancePack.checkCompatibility(devEnvironment);
    console.log(
      `  SOX Compliance (dev): ${complianceCompat.compatible ? '✅ compatible' : '❌ incompatible'}`
    );
    if (complianceCompat.issues.length > 0) {
      console.log(`    Issues: ${complianceCompat.issues.join(', ')}`);
    }

    const governanceCompat = governancePack.checkCompatibility(prodEnvironment);
    console.log(
      `  Data Governance (prod): ${governanceCompat.compatible ? '✅ compatible' : '❌ incompatible'}`
    );
    if (governanceCompat.issues.length > 0) {
      console.log(`    Issues: ${governanceCompat.issues.join(', ')}`);
    }

    // Get policy pack statistics
    console.log('\n📊 Policy Pack Statistics:');
    const complianceStats = compliancePack.getStats();
    console.log(
      `  SOX Compliance: ${complianceStats.hooks.total} hooks, loaded: ${complianceStats.hooks.loaded}`
    );

    const governanceStats = governancePack.getStats();
    console.log(
      `  Data Governance: ${governanceStats.hooks.total} hooks, loaded: ${governanceStats.loaded}`
    );

    // Demonstrate policy pack manager
    console.log('\n🔧 Testing Policy Pack Manager...');

    // Simulate loading policy packs (in real usage, these would be loaded from files)
    packManager.packs.set('sox-compliance', compliancePack);
    packManager.packs.set('data-governance', governancePack);

    // Activate policy packs
    packManager.activatePolicyPack('sox-compliance');
    packManager.activatePolicyPack('data-governance');

    console.log('✅ Policy packs activated');

    // Get active policy packs
    const activePacks = packManager.getActivePolicyPacks();
    console.log(`📦 Active policy packs: ${activePacks.length}`);

    // Get all hooks from active packs
    const activeHooks = packManager.getActiveHooks();
    console.log(`🔗 Active hooks: ${activeHooks.length}`);

    // Get manager statistics
    const managerStats = packManager.getStats();
    console.log('\n📊 Policy Pack Manager Statistics:');
    console.log(`  Total packs: ${managerStats.totalPacks}`);
    console.log(`  Active packs: ${managerStats.activePacks}`);
    console.log(`  Total hooks: ${managerStats.totalHooks}`);

    // Demonstrate integration with KnowledgeHookManager
    console.log('\n🔗 Testing integration with KnowledgeHookManager...');

    const hookManager = new KnowledgeHookManager({
      basePath: process.cwd(),
      strictMode: false,
    });

    // Add hooks from policy packs
    for (const hook of activeHooks) {
      try {
        hookManager.addKnowledgeHook(hook);
        console.log(`  ✅ Added hook: ${hook.meta.name}`);
      } catch (error) {
        console.log(`  ⚠️  Could not add hook ${hook.meta.name}: ${error.message}`);
      }
    }

    // Get hook manager statistics
    const hookStats = hookManager.getStats();
    console.log(`📊 Knowledge Hook Manager: ${hookStats.knowledgeHooks.total} hooks registered`);

    console.log('\n🎉 Policy Pack demo completed successfully!');
  } catch (error) {
    console.error('❌ Policy Pack demo failed:', error.message);
    throw error;
  }
}

// Run the demo
demonstratePolicyPacks().catch(error => {
  console.error('💥 Demo failed:', error);
  process.exit(1);
});
