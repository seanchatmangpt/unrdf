/**
 * @unrdf/project-engine - Basic Example
 *
 * Demonstrates basic usage of the package.
 */

import {
  analyzePackage,
  generateApiDocs,
  collectMetrics,
  reportMetrics,
  verifyPackage,
  createProjectConfig,
  setupDevEnvironment,
} from '@unrdf/project-engine';

async function main() {
  console.log('='.repeat(60));
  console.log('@unrdf/project-engine - Basic Example');
  console.log('='.repeat(60));
  console.log('');

  // 1. Analyze a package
  console.log('1. PACKAGE ANALYSIS');
  console.log('-'.repeat(60));
  try {
    const packagePath = './packages/core';
    const analysis = await analyzePackage(packagePath);

    console.log(`Package: ${analysis.name}`);
    console.log(`Lines of Code: ${analysis.linesOfCode.toLocaleString()}`);
    console.log(`Exports: ${analysis.exportCount}`);
    console.log(`Test Coverage: ${analysis.testCoverage}%`);
    console.log(`Complexity: ${analysis.complexity}`);
    console.log('');
  } catch (error) {
    console.log('Package analysis skipped:', error.message);
    console.log('');
  }

  // 2. Verify package integrity
  console.log('2. PACKAGE VERIFICATION');
  console.log('-'.repeat(60));
  try {
    const packagePath = './packages/core';
    const verification = await verifyPackage(packagePath);

    console.log(`Valid: ${verification.valid ? 'YES' : 'NO'}`);
    console.log(`Checked Files: ${verification.checkedFiles.length}`);

    if (verification.errors.length > 0) {
      console.log('Errors:');
      verification.errors.forEach(err => console.log(`  - ${err}`));
    }

    if (verification.warnings.length > 0) {
      console.log('Warnings:');
      verification.warnings.forEach(warn => console.log(`  - ${warn}`));
    }

    console.log('');
  } catch (error) {
    console.log('Verification skipped:', error.message);
    console.log('');
  }

  // 3. Generate API documentation
  console.log('3. API DOCUMENTATION GENERATION');
  console.log('-'.repeat(60));
  try {
    const packagePath = './packages/core';
    const docs = await generateApiDocs(packagePath);

    console.log(`Generated ${docs.split('\n').length} lines of API docs`);
    console.log('Preview (first 10 lines):');
    console.log(
      docs
        .split('\n')
        .slice(0, 10)
        .map(line => `  ${line}`)
        .join('\n')
    );
    console.log('  ...');
    console.log('');
  } catch (error) {
    console.log('Doc generation skipped:', error.message);
    console.log('');
  }

  // 4. Collect metrics for all packages
  console.log('4. METRICS COLLECTION');
  console.log('-'.repeat(60));
  try {
    const metrics = await collectMetrics('.');

    console.log(`Total Packages: ${metrics.totalPackages}`);
    console.log(`Total Lines of Code: ${metrics.totalLinesOfCode.toLocaleString()}`);
    console.log(`Total Exports: ${metrics.totalExports}`);
    console.log(`Average Test Coverage: ${metrics.averageTestCoverage}%`);
    console.log('');

    // Show report
    const report = reportMetrics(metrics);
    console.log(report);
  } catch (error) {
    console.log('Metrics collection skipped:', error.message);
    console.log('');
  }

  // 5. Create project configuration
  console.log('5. PROJECT CONFIGURATION');
  console.log('-'.repeat(60));
  const projectConfig = createProjectConfig('library');

  console.log('Project Config:');
  console.log(`  Name: ${projectConfig.name}`);
  console.log(`  Type: ${projectConfig.type}`);
  console.log(`  Runtime: ${projectConfig.runtime}`);
  console.log(`  Testing Framework: ${projectConfig.testing.framework}`);
  console.log(`  Coverage Target: ${projectConfig.testing.coverage}%`);
  console.log('');

  // 6. Setup development environment
  console.log('6. DEVELOPMENT ENVIRONMENT');
  console.log('-'.repeat(60));
  const devConfig = setupDevEnvironment();

  console.log('Dev Tools:');
  console.log(`  Package Manager: ${devConfig.tools.packageManager}`);
  console.log(`  Node Version: ${devConfig.tools.nodeVersion}`);
  console.log('');

  console.log('Scripts:');
  Object.entries(devConfig.scripts).forEach(([name, command]) => {
    console.log(`  ${name}: ${command}`);
  });
  console.log('');

  console.log('='.repeat(60));
  console.log('Example completed successfully!');
  console.log('='.repeat(60));
}

main().catch(error => {
  console.error('Example failed:', error);
  process.exit(1);
});
