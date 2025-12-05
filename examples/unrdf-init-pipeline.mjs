/**
 * @file UNRDF Init Pipeline Example
 * @module examples/unrdf-init-pipeline
 *
 * @description
 * Demonstrates the complete UNRDF initialization pipeline that:
 * 1. Understands the tech stack
 * 2. Extracts the domain model
 * 3. Infers code generation patterns
 * 4. Plans and applies materialization
 * 5. Establishes drift detection baseline
 * 6. Derives and registers validation hooks
 *
 * Usage:
 *   node examples/unrdf-init-pipeline.mjs /path/to/project
 */

import { resolve } from 'path';
import { cwd } from 'process';
import {
  createProjectInitializationPipeline,
  scanFileSystemToStore,
  detectStackFromFs,
  buildProjectModelFromFs,
  classifyFiles,
  inferDomainModel,
  inferTemplatesFromProject,
  planMaterialization,
  applyMaterializationPlan,
  createStructureSnapshot,
  deriveHooksFromStructure,
  buildProjectReport,
} from '../packages/project-engine/index.mjs';

/**
 * Example 1: Quick initialization with the pipeline
 *
 * This is the simplest usage - one call that does everything.
 */
async function example1_QuickInit() {
  console.log('\n=== Example 1: Quick Initialization ===\n');

  const projectRoot = resolve(cwd());
  const result = await createProjectInitializationPipeline(projectRoot, {
    verbose: true,
  });

  if (result.success) {
    console.log('✅ Initialization complete!');
    console.log(`   Phases completed: ${Object.keys(result.receipt.phases).length}`);
    console.log(`   Total duration: ${result.receipt.totalDuration}ms`);
    console.log(`   Report summary: ${result.report.summary}`);
  } else {
    console.error('❌ Initialization failed');
  }
}

/**
 * Example 2: Step-by-step manual flow (for understanding)
 *
 * This shows what happens inside the pipeline, useful for
 * extending or customizing the initialization process.
 */
async function example2_ManualFlow() {
  console.log('\n=== Example 2: Manual Step-by-Step Flow ===\n');

  const projectRoot = resolve(cwd());

  // Step 1: Scan filesystem
  console.log('1️⃣  Scanning filesystem...');
  const fsStore = await scanFileSystemToStore(projectRoot);
  console.log(`   ✓ Found ${fsStore.size} files/folders`);

  // Step 2: Detect stack
  console.log('\n2️⃣  Detecting tech stack...');
  const stackProfile = detectStackFromFs({ fsStore });
  console.log(
    `   ✓ Stack: ${stackProfile.webFramework || 'unknown'} + ${stackProfile.testFramework || 'none'}`
  );

  // Step 3: Build project model
  console.log('\n3️⃣  Building project model...');
  const projectStore = buildProjectModelFromFs({
    fsStore,
    baseIri: 'http://example.org/unrdf/project#',
  });
  console.log(`   ✓ Project model created`);

  // Step 4: Classify files by role
  console.log('\n4️⃣  Classifying files by role...');
  classifyFiles({ fsStore: projectStore, stackInfo: stackProfile });
  console.log(`   ✓ Files classified`);

  // Step 5: Infer domain model
  console.log('\n5️⃣  Inferring domain model...');
  const domainResult = await inferDomainModel(projectStore, stackProfile);
  console.log(
    `   ✓ Domain model: ${domainResult.summary.entityCount} entities, ${domainResult.summary.fieldCount} fields`
  );

  // Step 6: Infer templates
  console.log('\n6️⃣  Inferring code generation templates...');
  const templateResult = inferTemplatesFromProject(projectStore, domainResult.store, stackProfile);
  console.log(
    `   ✓ Templates inferred: ${Object.values(templateResult.summary.byKind || {}).reduce((a, b) => a + b, 0)} total`
  );

  // Step 7: Create snapshot
  console.log('\n7️⃣  Creating baseline snapshot for drift detection...');
  const snapshotResult = createStructureSnapshot(projectStore, domainResult.store);
  console.log(`   ✓ Snapshot created: ${snapshotResult.receipt.hash}`);

  // Step 8: Derive hooks
  console.log('\n8️⃣  Deriving validation hooks from patterns...');
  const hooks = deriveHooksFromStructure(projectStore, stackProfile);
  console.log(`   ✓ Hooks derived: ${hooks.length} rules`);

  // Step 9: Generate report
  console.log('\n9️⃣  Generating project report...');
  const report = buildProjectReport(projectStore);
  console.log(`   ✓ Report: ${report.features.length} features, ${report.stats.totalFiles} files`);
  console.log(`   Summary: ${report.summary}`);
}

/**
 * Example 3: Full materialization workflow
 *
 * Shows how to plan and apply code generation for new features.
 */
async function example3_Materialization() {
  console.log('\n=== Example 3: Full Materialization Workflow ===\n');

  const projectRoot = resolve(cwd());

  // Initialize
  const result = await createProjectInitializationPipeline(projectRoot, {
    dryRun: true, // Don't actually write files
  });

  if (!result.success) {
    console.error('Initialization failed');
    return;
  }

  const { projectStore, _domainStore } = result.state;
  const { templateGraph } = result.state;

  // Plan materialization
  console.log('📋 Planning materialization...');
  const plan = await planMaterialization(projectStore, templateGraph, {
    entities: ['NewFeature'], // Generate for these domain entities
  });

  console.log(`✓ Plan created: ${plan.plan.writes.length} files to write`);
  plan.plan.writes.slice(0, 5).forEach(w => {
    console.log(`  - ${w.path}`);
  });
  if (plan.plan.writes.length > 5) {
    console.log(`  ... and ${plan.plan.writes.length - 5} more`);
  }

  // Apply materialization (dry-run)
  console.log('\n🎯 Applying materialization...');
  const result2 = await applyMaterializationPlan(plan, {
    dryRun: true,
    projectRoot,
  });

  console.log(`✓ Applied: ${result2.result.appliedCount} files written (dry-run)`);
  console.log(`  Plan hash: ${result2.receipt.planHash}`);
}

/**
 * Example 4: Accessing initialized state for custom workflows
 *
 * Shows how to use the state returned by the pipeline for
 * custom operations like queries, updates, or further analysis.
 */
async function example4_StateAccess() {
  console.log('\n=== Example 4: Accessing Initialized State ===\n');

  const projectRoot = resolve(cwd());

  const result = await createProjectInitializationPipeline(projectRoot);

  if (!result.success) {
    console.error('Initialization failed');
    return;
  }

  const { _fsStore, projectStore, domainStore, templateGraph, _snapshot } = result.state;

  // Query features
  console.log('📊 Querying project state:\n');

  const features = projectStore
    .getQuads(null, null, null)
    .filter(q => q.predicate.value.includes('type') && q.object.value.includes('Feature'));

  console.log(`Features: ${features.length}`);
  features.slice(0, 5).forEach(f => {
    console.log(`  - ${f.subject.value}`);
  });

  // Query domain entities
  const entities = domainStore
    .getQuads(null, null, null)
    .filter(q => q.predicate.value.includes('type') && q.object.value.includes('Entity'));

  console.log(`\nDomain Entities: ${entities.length}`);
  entities.slice(0, 5).forEach(e => {
    console.log(`  - ${e.subject.value}`);
  });

  // Query templates
  const templates = templateGraph
    .getQuads(null, null, null)
    .filter(q => q.predicate.value.includes('type') && q.object.value.includes('Template'));

  console.log(`\nTemplates: ${templates.length}`);
  templates.slice(0, 5).forEach(t => {
    console.log(`  - ${t.subject.value}`);
  });

  // Access report
  console.log(`\nProject Report:`);
  console.log(`  Stack: ${result.report.stackProfile}`);
  console.log(`  Features: ${result.report.stats.featureCount}`);
  console.log(`  Test Coverage: ${result.report.stats.testCoverageAverage}%`);
}

/**
 * Main: Run examples
 */
async function main() {
  const examples = process.argv[2];

  try {
    if (!examples || examples === '1') {
      await example1_QuickInit();
    }
    if (!examples || examples === '2') {
      await example2_ManualFlow();
    }
    if (!examples || examples === '3') {
      await example3_Materialization();
    }
    if (!examples || examples === '4') {
      await example4_StateAccess();
    }

    console.log('\n✅ Examples complete!\n');
  } catch (error) {
    console.error('❌ Error:', error.message);
    if (process.env.DEBUG) {
      console.error(error);
    }
    process.exit(1);
  }
}

main();
