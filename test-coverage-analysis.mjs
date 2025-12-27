#!/usr/bin/env node

/**
 * Test Coverage Analysis
 * 
 * Comprehensive analysis of test coverage across the UNRDF system
 */

import { exec } from 'node:child_process';
import { promisify } from 'node:util';
import { readFile, readdir } from 'node:fs/promises';
import { join } from 'node:path';

const execAsync = promisify(exec);

console.log('📊 UNRDF Test Coverage Analysis\n');

async function analyzeTestCoverage() {
  const coverage = {
    total: {
      sourceFiles: 0,
      testFiles: 0,
      exampleFiles: 0
    },
    byModule: {},
    byType: {},
    realImplementations: {},
    testCategories: {}
  };

  try {
    // === Source Files Analysis ===
    console.log('🔍 Analyzing Source Files...');
    
    const sourceFiles = await getSourceFiles();
    coverage.total.sourceFiles = sourceFiles.length;
    
    // Group by module
    for (const file of sourceFiles) {
      const module = getModuleFromPath(file);
      if (!coverage.byModule[module]) {
        coverage.byModule[module] = { source: 0, tests: 0, examples: 0 };
      }
      coverage.byModule[module].source++;
    }

    // === Test Files Analysis ===
    console.log('🧪 Analyzing Test Files...');
    
    const testFiles = await getTestFiles();
    coverage.total.testFiles = testFiles.length;
    
    // Group by module
    for (const file of testFiles) {
      const module = getModuleFromPath(file);
      if (!coverage.byModule[module]) {
        coverage.byModule[module] = { source: 0, tests: 0, examples: 0 };
      }
      coverage.byModule[module].tests++;
    }

    // === Example Files Analysis ===
    console.log('📚 Analyzing Example Files...');
    
    const exampleFiles = await getExampleFiles();
    coverage.total.exampleFiles = exampleFiles.length;
    
    // Group by module
    for (const file of exampleFiles) {
      const module = getModuleFromPath(file);
      if (!coverage.byModule[module]) {
        coverage.byModule[module] = { source: 0, tests: 0, examples: 0 };
      }
      coverage.byModule[module].examples++;
    }

    // === Test Categories Analysis ===
    console.log('📋 Analyzing Test Categories...');
    
    coverage.testCategories = await analyzeTestCategories();

    // === Real Implementation Analysis ===
    console.log('🚀 Analyzing Real Implementations...');
    
    coverage.realImplementations = await analyzeRealImplementations();

    // === Coverage Calculations ===
    console.log('📊 Calculating Coverage Metrics...');
    
    const coverageMetrics = calculateCoverageMetrics(coverage);

    // === Display Results ===
    displayCoverageResults(coverage, coverageMetrics);

  } catch (error) {
    console.error('❌ Coverage analysis failed:', error.message);
  }
}

async function getSourceFiles() {
  const { stdout } = await execAsync('find src/ -name "*.mjs" | grep -v test-utils');
  return stdout.trim().split('\n').filter(f => f);
}

async function getTestFiles() {
  const { stdout } = await execAsync('find test/ -name "*.mjs"');
  return stdout.trim().split('\n').filter(f => f);
}

async function getExampleFiles() {
  const { stdout } = await execAsync('find examples/ -name "*.mjs"');
  return stdout.trim().split('\n').filter(f => f);
}

function getModuleFromPath(filePath) {
  if (filePath.includes('knowledge-engine/')) return 'knowledge-engine';
  if (filePath.includes('composables/')) return 'composables';
  if (filePath.includes('utils/')) return 'utils';
  if (filePath.includes('engines/')) return 'engines';
  if (filePath.includes('context/')) return 'context';
  if (filePath.includes('cli')) return 'cli';
  return 'core';
}

async function analyzeTestCategories() {
  const categories = {
    'unit-tests': 0,
    'integration-tests': 0,
    'end-to-end-tests': 0,
    'real-implementation-tests': 0,
    'dark-matter-tests': 0,
    'hook-tests': 0,
    'lockchain-tests': 0,
    'performance-tests': 0,
    'security-tests': 0,
    'compliance-tests': 0
  };

  try {
    const { stdout } = await execAsync('find test/ examples/ -name "*.mjs"');
    const files = stdout.trim().split('\n').filter(f => f);

    for (const file of files) {
      if (file.includes('hooks/')) categories['hook-tests']++;
      if (file.includes('lockchain')) categories['lockchain-tests']++;
      if (file.includes('real-system')) categories['real-implementation-tests']++;
      if (file.includes('dark-matter')) categories['dark-matter-tests']++;
      if (file.includes('comprehensive')) categories['end-to-end-tests']++;
      if (file.includes('integration')) categories['integration-tests']++;
      if (file.includes('performance')) categories['performance-tests']++;
      if (file.includes('security')) categories['security-tests']++;
      if (file.includes('compliance')) categories['compliance-tests']++;
      if (file.includes('.test.mjs')) categories['unit-tests']++;
    }
  } catch (error) {
    console.warn('Warning: Could not analyze test categories:', error.message);
  }

  return categories;
}

async function analyzeRealImplementations() {
  const implementations = {
    'real-knowledge-hooks': false,
    'real-lockchain-writer': false,
    'real-resolution-layer': false,
    'real-effect-sandbox': false,
    'real-query-optimizer': false,
    'transaction-manager': false
  };

  try {
    const { stdout } = await execAsync('find src/ -name "real-*.mjs" -o -name "transaction.mjs"');
    const files = stdout.trim().split('\n').filter(f => f);

    for (const file of files) {
      if (file.includes('real-knowledge-hooks')) implementations['real-knowledge-hooks'] = true;
      if (file.includes('real-lockchain-writer')) implementations['real-lockchain-writer'] = true;
      if (file.includes('real-resolution-layer')) implementations['real-resolution-layer'] = true;
      if (file.includes('real-effect-sandbox')) implementations['real-effect-sandbox'] = true;
      if (file.includes('real-query-optimizer')) implementations['real-query-optimizer'] = true;
      if (file.includes('transaction.mjs')) implementations['transaction-manager'] = true;
    }
  } catch (error) {
    console.warn('Warning: Could not analyze real implementations:', error.message);
  }

  return implementations;
}

function calculateCoverageMetrics(coverage) {
  const metrics = {
    overall: {
      sourceFiles: coverage.total.sourceFiles,
      testFiles: coverage.total.testFiles,
      exampleFiles: coverage.total.exampleFiles,
      totalFiles: coverage.total.sourceFiles + coverage.total.testFiles + coverage.total.exampleFiles
    },
    byModule: {},
    realImplementationCoverage: 0,
    testCategoryCoverage: 0
  };

  // Calculate module coverage
  for (const [module, data] of Object.entries(coverage.byModule)) {
    const total = data.source + data.tests + data.examples;
    const testRatio = total > 0 ? (data.tests + data.examples) / total : 0;
    
    metrics.byModule[module] = {
      sourceFiles: data.source,
      testFiles: data.tests,
      exampleFiles: data.examples,
      coverageRatio: testRatio,
      coveragePercentage: Math.round(testRatio * 100)
    };
  }

  // Calculate real implementation coverage
  const realImplCount = Object.values(coverage.realImplementations).filter(Boolean).length;
  const totalRealImpls = Object.keys(coverage.realImplementations).length;
  metrics.realImplementationCoverage = Math.round((realImplCount / totalRealImpls) * 100);

  // Calculate test category coverage
  const categoryCount = Object.values(coverage.testCategories).filter(count => count > 0).length;
  const totalCategories = Object.keys(coverage.testCategories).length;
  metrics.testCategoryCoverage = Math.round((categoryCount / totalCategories) * 100);

  return metrics;
}

function displayCoverageResults(coverage, metrics) {
  console.log('\n🎯 Test Coverage Results');
  console.log('========================\n');

  // Overall metrics
  console.log('📊 Overall Coverage:');
  console.log(`  Source Files: ${metrics.overall.sourceFiles}`);
  console.log(`  Test Files: ${metrics.overall.testFiles}`);
  console.log(`  Example Files: ${metrics.overall.exampleFiles}`);
  console.log(`  Total Files: ${metrics.overall.totalFiles}`);
  console.log(`  Test-to-Source Ratio: 1:${(metrics.overall.sourceFiles / metrics.overall.testFiles).toFixed(1)}`);

  // Module coverage
  console.log('\n📁 Module Coverage:');
  for (const [module, data] of Object.entries(metrics.byModule)) {
    const status = data.coveragePercentage >= 80 ? '✅' : 
                  data.coveragePercentage >= 60 ? '⚠️' : '❌';
    console.log(`  ${status} ${module}: ${data.coveragePercentage}% (${data.testFiles + data.exampleFiles}/${data.sourceFiles + data.testFiles + data.exampleFiles})`);
  }

  // Real implementation coverage
  console.log('\n🚀 Real Implementation Coverage:');
  console.log(`  Coverage: ${metrics.realImplementationCoverage}%`);
  for (const [impl, exists] of Object.entries(coverage.realImplementations)) {
    const status = exists ? '✅' : '❌';
    console.log(`  ${status} ${impl}`);
  }

  // Test category coverage
  console.log('\n🧪 Test Category Coverage:');
  console.log(`  Coverage: ${metrics.testCategoryCoverage}%`);
  for (const [category, count] of Object.entries(coverage.testCategories)) {
    const status = count > 0 ? '✅' : '❌';
    console.log(`  ${status} ${category}: ${count} tests`);
  }

  // Summary
  console.log('\n📈 Coverage Summary:');
  const avgModuleCoverage = Object.values(metrics.byModule)
    .reduce((sum, data) => sum + data.coveragePercentage, 0) / Object.keys(metrics.byModule).length;
  
  console.log(`  Average Module Coverage: ${Math.round(avgModuleCoverage)}%`);
  console.log(`  Real Implementation Coverage: ${metrics.realImplementationCoverage}%`);
  console.log(`  Test Category Coverage: ${metrics.testCategoryCoverage}%`);
  
  const overallScore = Math.round((avgModuleCoverage + metrics.realImplementationCoverage + metrics.testCategoryCoverage) / 3);
  console.log(`  Overall Test Coverage Score: ${overallScore}%`);
  
  if (overallScore >= 80) {
    console.log('🎉 Excellent test coverage!');
  } else if (overallScore >= 60) {
    console.log('⚠️  Good test coverage, room for improvement');
  } else {
    console.log('❌ Test coverage needs significant improvement');
  }
}

// Run the analysis
analyzeTestCoverage()
  .then(() => {
    console.log('\n✅ Coverage analysis complete');
  })
  .catch(error => {
    console.error('❌ Coverage analysis failed:', error);
    process.exit(1);
  });
