#!/usr/bin/env node

/**
 * @file CLI Scaffolding Demo
 * @description
 * Demonstrates the CLI scaffolding commands for generating hooks and policy packs.
 */

import { execSync } from 'child_process';
import { readFileSync, existsSync, mkdirSync } from 'fs';
import { join } from 'path';

console.log('🔧 CLI Scaffolding Demo\n');

async function demonstrateCLIScaffolding() {
  try {
    // Create a temporary directory for the demo
    const demoDir = join(process.cwd(), 'demo-scaffolding');
    if (!existsSync(demoDir)) {
      mkdirSync(demoDir, { recursive: true });
    }

    console.log(`📁 Created demo directory: ${demoDir}`);

    // Change to demo directory
    process.chdir(demoDir);

    // Demo 1: Generate a knowledge hook
    console.log('\n1️⃣  Generating Knowledge Hook:');

    try {
      const hookCommand = 'node ../src/cli.mjs scaffold hook email-validation sparql-ask';
      console.log(`  Running: ${hookCommand}`);

      const hookOutput = execSync(hookCommand, {
        encoding: 'utf8',
        stdio: 'pipe',
      });

      console.log('  ✅ Hook generation output:');
      console.log(hookOutput);

      // Verify generated files
      const hookDir = join(demoDir, 'hooks', 'email-validation');
      const hookFile = join(hookDir, 'email-validation.mjs');
      const conditionFile = join(hookDir, 'email-validation.rq');
      const testFile = join(hookDir, 'email-validation.test.mjs');

      if (existsSync(hookFile)) {
        const hookContent = readFileSync(hookFile, 'utf8');
        console.log(`  📄 Hook file created: ${hookFile}`);
        console.log(`  📝 Hook content preview:`);
        console.log(hookContent.substring(0, 200) + '...');
      }

      if (existsSync(conditionFile)) {
        const conditionContent = readFileSync(conditionFile, 'utf8');
        console.log(`  📄 Condition file created: ${conditionFile}`);
        console.log(`  📝 Condition content:`);
        console.log(conditionContent);
      }

      if (existsSync(testFile)) {
        const testContent = readFileSync(testFile, 'utf8');
        console.log(`  📄 Test file created: ${testFile}`);
        console.log(`  📝 Test content preview:`);
        console.log(testContent.substring(0, 200) + '...');
      }
    } catch (error) {
      console.log(`  ⚠️  Hook generation failed: ${error.message}`);
    }

    // Demo 2: Generate a SHACL hook
    console.log('\n2️⃣  Generating SHACL Hook:');

    try {
      const shaclCommand = 'node ../src/cli.mjs scaffold hook data-integrity shacl';
      console.log(`  Running: ${shaclCommand}`);

      const shaclOutput = execSync(shaclCommand, {
        encoding: 'utf8',
        stdio: 'pipe',
      });

      console.log('  ✅ SHACL hook generation output:');
      console.log(shaclOutput);

      // Verify SHACL files
      const shaclDir = join(demoDir, 'hooks', 'data-integrity');
      const shaclConditionFile = join(shaclDir, 'data-integrity.rq');

      if (existsSync(shaclConditionFile)) {
        const shaclContent = readFileSync(shaclConditionFile, 'utf8');
        console.log(`  📄 SHACL condition file created: ${shaclConditionFile}`);
        console.log(`  📝 SHACL content:`);
        console.log(shaclContent);
      }
    } catch (error) {
      console.log(`  ⚠️  SHACL hook generation failed: ${error.message}`);
    }

    // Demo 3: Generate a policy pack
    console.log('\n3️⃣  Generating Policy Pack:');

    try {
      const policyCommand = 'node ../src/cli.mjs scaffold policy sox-compliance';
      console.log(`  Running: ${policyCommand}`);

      const policyOutput = execSync(policyCommand, {
        encoding: 'utf8',
        stdio: 'pipe',
      });

      console.log('  ✅ Policy pack generation output:');
      console.log(policyOutput);

      // Verify generated policy pack files
      const policyDir = join(demoDir, 'policy-packs', 'sox-compliance');
      const manifestFile = join(policyDir, 'manifest.json');
      const readmeFile = join(policyDir, 'README.md');
      const hooksDir = join(policyDir, 'hooks');
      const conditionsDir = join(policyDir, 'conditions');
      const resourcesDir = join(policyDir, 'resources');

      if (existsSync(manifestFile)) {
        const manifestContent = readFileSync(manifestFile, 'utf8');
        console.log(`  📄 Manifest file created: ${manifestFile}`);
        console.log(`  📝 Manifest content:`);
        console.log(manifestContent);
      }

      if (existsSync(readmeFile)) {
        const readmeContent = readFileSync(readmeFile, 'utf8');
        console.log(`  📄 README file created: ${readmeFile}`);
        console.log(`  📝 README content preview:`);
        console.log(readmeContent.substring(0, 300) + '...');
      }

      console.log(`  📁 Directories created:`);
      console.log(`    - ${hooksDir} (${existsSync(hooksDir) ? '✅' : '❌'})`);
      console.log(`    - ${conditionsDir} (${existsSync(conditionsDir) ? '✅' : '❌'})`);
      console.log(`    - ${resourcesDir} (${existsSync(resourcesDir) ? '✅' : '❌'})`);
    } catch (error) {
      console.log(`  ⚠️  Policy pack generation failed: ${error.message}`);
    }

    // Demo 4: Generate another policy pack
    console.log('\n4️⃣  Generating Another Policy Pack:');

    try {
      const policy2Command = 'node ../src/cli.mjs scaffold policy gdpr-compliance';
      console.log(`  Running: ${policy2Command}`);

      const policy2Output = execSync(policy2Command, {
        encoding: 'utf8',
        stdio: 'pipe',
      });

      console.log('  ✅ Second policy pack generation output:');
      console.log(policy2Output);
    } catch (error) {
      console.log(`  ⚠️  Second policy pack generation failed: ${error.message}`);
    }

    // Demo 5: Show directory structure
    console.log('\n5️⃣  Generated Directory Structure:');

    function showDirectoryStructure(dir, prefix = '') {
      try {
        const items = require('fs').readdirSync(dir, { withFileTypes: true });
        for (const item of items) {
          const itemPath = join(dir, item.name);
          const isDir = item.isDirectory();
          console.log(`${prefix}${isDir ? '📁' : '📄'} ${item.name}`);

          if (isDir && prefix.length < 20) {
            // Limit depth to avoid too much output
            showDirectoryStructure(itemPath, prefix + '  ');
          }
        }
      } catch (error) {
        console.log(`${prefix}❌ Error reading directory: ${error.message}`);
      }
    }

    showDirectoryStructure(demoDir);

    // Demo 6: Validate generated files
    console.log('\n6️⃣  Validating Generated Files:');

    const validationResults = [];

    // Validate hook files
    const hookFiles = [
      join(demoDir, 'hooks', 'email-validation', 'email-validation.mjs'),
      join(demoDir, 'hooks', 'email-validation', 'email-validation.rq'),
      join(demoDir, 'hooks', 'email-validation', 'email-validation.test.mjs'),
      join(demoDir, 'hooks', 'data-integrity', 'data-integrity.mjs'),
      join(demoDir, 'hooks', 'data-integrity', 'data-integrity.rq'),
      join(demoDir, 'hooks', 'data-integrity', 'data-integrity.test.mjs'),
    ];

    for (const file of hookFiles) {
      const exists = existsSync(file);
      validationResults.push({ file, exists });
      console.log(`  ${exists ? '✅' : '❌'} ${file}`);
    }

    // Validate policy pack files
    const policyFiles = [
      join(demoDir, 'policy-packs', 'sox-compliance', 'manifest.json'),
      join(demoDir, 'policy-packs', 'sox-compliance', 'README.md'),
      join(demoDir, 'policy-packs', 'gdpr-compliance', 'manifest.json'),
      join(demoDir, 'policy-packs', 'gdpr-compliance', 'README.md'),
    ];

    for (const file of policyFiles) {
      const exists = existsSync(file);
      validationResults.push({ file, exists });
      console.log(`  ${exists ? '✅' : '❌'} ${file}`);
    }

    // Summary
    const totalFiles = validationResults.length;
    const existingFiles = validationResults.filter(r => r.exists).length;

    console.log('\n📊 Scaffolding Summary:');
    console.log(`  Total files expected: ${totalFiles}`);
    console.log(`  Files created: ${existingFiles}`);
    console.log(`  Success rate: ${((existingFiles / totalFiles) * 100).toFixed(1)}%`);

    // Cleanup
    console.log('\n🧹 Cleaning up demo directory...');
    try {
      execSync(`rm -rf ${demoDir}`, { stdio: 'pipe' });
      console.log('  ✅ Demo directory cleaned up');
    } catch (error) {
      console.log(`  ⚠️  Cleanup failed: ${error.message}`);
    }

    console.log('\n🎉 CLI Scaffolding demo completed successfully!');
  } catch (error) {
    console.error('❌ CLI Scaffolding demo failed:', error.message);
    throw error;
  }
}

// Run the demo
demonstrateCLIScaffolding().catch(error => {
  console.error('💥 Demo failed:', error);
  process.exit(1);
});
