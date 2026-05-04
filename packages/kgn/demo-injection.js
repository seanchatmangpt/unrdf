#!/usr/bin/env node
/**
 * KGEN Injection System Demo
 *
 * Demonstrates the atomic injection operations system
 * with real file modifications and rollback capabilities.
 */

import { promises as fs } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

import { inject, dryRun, undo, getOperationHistory } from './src/injection/api.js';
import { INJECTION_MODES } from './src/injection/constants.js';

async function runDemo() {
  console.log('🚀 KGEN Injection System Demo');
  console.log('=' .repeat(50));

  // Create temporary demo directory
  const demoDir = await fs.mkdtemp(join(tmpdir(), 'kgen-injection-demo-'));
  console.log(`📁 Demo directory: ${demoDir}`);

  try {
    await fs.mkdir(join(demoDir, 'src'), { recursive: true });

    // Demo 1: Basic Append Injection
    console.log('\n1️⃣ Demo: Basic Append Injection');
    console.log('-'.repeat(30));

    const routesFile = join(demoDir, 'src', 'routes.ts');
    const initialRoutes = `import { Router } from 'express';

const router = Router();

// Health check route
router.get('/health', (req, res) => res.json({ status: 'ok' }));

export default router;`;

    await fs.writeFile(routesFile, initialRoutes);
    console.log('✅ Created initial routes.ts file');

    const appendConfig = {
      to: 'src/routes.ts',
      inject: true,
      mode: INJECTION_MODES.APPEND
    };

    const userRouteContent = `
// User routes
router.get('/users', getUsersHandler);
router.post('/users', createUserHandler);`;

    const result1 = await inject(appendConfig, userRouteContent, {}, {
      config: { projectRoot: demoDir }
    });

    console.log(`✅ Injection successful: ${result1.operationId}`);

    const modifiedContent = await fs.readFile(routesFile, 'utf8');
    console.log('📄 File content after injection:');
    console.log(modifiedContent);

    // Demo 2: Idempotent Injection (should skip)
    console.log('\n2️⃣ Demo: Idempotent Injection');
    console.log('-'.repeat(30));

    const idempotentConfig = {
      to: 'src/routes.ts',
      inject: true,
      mode: INJECTION_MODES.APPEND,
      skipIf: 'getUsersHandler'
    };

    const result2 = await inject(idempotentConfig, userRouteContent, {}, {
      config: { projectRoot: demoDir }
    });

    console.log(`✅ Idempotent check: ${result2.skipped ? 'SKIPPED' : 'INJECTED'}`);
    console.log(`   Reason: Content already exists`);

    // Demo 3: Before Injection with Pattern Matching
    console.log('\n3️⃣ Demo: Before Injection with Pattern Matching');
    console.log('-'.repeat(30));

    const beforeConfig = {
      to: 'src/routes.ts',
      inject: true,
      mode: INJECTION_MODES.BEFORE,
      target: 'export default router;'
    };

    const middlewareContent = `
// Apply middleware
router.use('/api', authMiddleware);`;

    const result3 = await inject(beforeConfig, middlewareContent, {}, {
      config: { projectRoot: demoDir }
    });

    console.log(`✅ Before injection: ${result3.operationId}`);

    const finalContent = await fs.readFile(routesFile, 'utf8');
    console.log('📄 Final file content:');
    console.log(finalContent);

    // Demo 4: Dry Run Demo
    console.log('\n4️⃣ Demo: Dry Run (No Modifications)');
    console.log('-'.repeat(30));

    const dryRunConfig = {
      to: 'src/routes.ts',
      inject: true,
      mode: INJECTION_MODES.PREPEND
    };

    const importContent = `import { cors } from 'cors';`;

    const dryRunResult = await dryRun(dryRunConfig, importContent, {}, {
      config: { projectRoot: demoDir }
    });

    console.log('✅ Dry run results:');
    dryRunResult.targets.forEach((target, index) => {
      console.log(`   Target ${index + 1}: ${target.path}`);
      console.log(`   Valid: ${target.valid}`);
      console.log(`   Would skip: ${target.wouldSkip}`);
    });

    // Demo 5: Multiple Target Injection
    console.log('\n5️⃣ Demo: Multiple Target Injection');
    console.log('-'.repeat(30));

    // Create additional files
    const configFile = join(demoDir, 'src', 'config.ts');
    await fs.writeFile(configFile, `export const config = {\n  port: 3000\n};`);

    const utilsFile = join(demoDir, 'src', 'utils.ts');
    await fs.writeFile(utilsFile, `export const utils = {\n  helper: true\n};`);

    const globConfig = {
      to: 'src/*.ts',
      inject: true,
      mode: INJECTION_MODES.PREPEND,
      exclude: ['*.test.ts']
    };

    const headerComment = `// Auto-generated header
`;

    const result5 = await inject(globConfig, headerComment, {}, {
      config: { projectRoot: demoDir }
    });

    console.log(`✅ Multi-target injection: ${result5.targets} files modified`);

    // Demo 6: Operation History
    console.log('\n6️⃣ Demo: Operation History');
    console.log('-'.repeat(30));

    const history = getOperationHistory();
    console.log(`📊 Total operations performed: ${history.length}`);

    history.forEach((op, index) => {
      console.log(`   Operation ${index + 1}:`);
      console.log(`     ID: ${op.id}`);
      console.log(`     Phase: ${op.phase}`);
      console.log(`     Targets: ${op.targets?.length || 0}`);
      console.log(`     Duration: ${op.endTime - op.startTime}ms`);
    });

    // Demo 7: Rollback/Undo Demo
    console.log('\n7️⃣ Demo: Rollback Operation');
    console.log('-'.repeat(30));

    if (result1.operationId) {
      console.log(`🔄 Attempting to undo operation: ${result1.operationId}`);

      try {
        const undoResult = await undo(result1.operationId, {
          config: { projectRoot: demoDir }
        });

        console.log(`✅ Undo successful: ${undoResult.filesRestored} files restored`);

        const restoredContent = await fs.readFile(routesFile, 'utf8');
        console.log('📄 Content after undo:');
        console.log(restoredContent.substring(0, 200) + '...');

      } catch (error) {
        console.log(`❌ Undo failed: ${error.message}`);
      }
    }

    console.log('\n' + '='.repeat(50));
    console.log('✅ Demo completed successfully!');
    console.log(`📁 Demo files remain at: ${demoDir}`);
    console.log('   You can inspect the files to see the injection results.');

  } catch (error) {
    console.error('❌ Demo failed:', error);
    console.error(error.stack);
  } finally {
    // Clean up demo directory after 30 seconds
    setTimeout(async () => {
      try {
        await fs.rm(demoDir, { recursive: true, force: true });
        console.log('🧹 Demo directory cleaned up');
      } catch (error) {
        console.warn('⚠️ Failed to cleanup demo directory:', error.message);
      }
    }, 30000);
  }
}

// Run demo if executed directly
if (import.meta.url.endsWith(process.argv[1])) {
  runDemo().catch(console.error);
}

export { runDemo };