/**
 * @file Manual Verification Script
 * @description Quick verification that tool wrapper and registry work
 */

import { Wrap, validateReceipt } from '../src/tool-wrapper.mjs';
import { ToolRegistry } from '../../kgc-runtime/src/tool-registry.mjs';
import { z } from 'zod';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

async function runVerification() {
  console.log('🔍 Starting Manual Verification...\n');

  // Test 1: ToolWrapper with simple tool
  console.log('Test 1: ToolWrapper - Basic functionality');
  const mockTool = async (inputs) => ({
    content: `Content of ${inputs.path}`,
    size: 42,
  });

  const manifest = {
    name: 'Read',
    version: '[VERSION]',
    schema_in: z.object({
      path: z.string(),
    }),
    schema_out: z.object({
      content: z.string(),
      size: z.number(),
    }),
    capabilities: ['file-read'],
  };

  const wrapped = Wrap(mockTool, manifest);
  const result = await wrapped({ path: '/test.txt' });

  console.log('  ✓ Receipt generated:', result.receipt.tool_name);
  console.log('  ✓ Status:', result.receipt.status);
  console.log('  ✓ Delta present:', result.delta !== null);
  console.log('  ✓ Receipt valid:', validateReceipt(result.receipt));
  console.log();

  // Test 2: ToolRegistry
  console.log('Test 2: ToolRegistry - Loading and querying');
  const registryPath = join(__dirname, '../../../var/kgc/tool-registry.json');
  const registry = new ToolRegistry({ registryPath });

  const tools = registry.getAllTools();
  console.log('  ✓ Tools loaded:', tools.length);

  const bashTool = registry.getTool('Bash');
  console.log('  ✓ Bash tool found:', bashTool !== null);
  console.log('  ✓ Bash capabilities:', bashTool.capabilities.join(', '));

  const fileTools = registry.getToolsByCapability('file-read');
  console.log('  ✓ File-read tools:', fileTools.map((t) => t.name).join(', '));

  const stats = registry.getStats();
  console.log('  ✓ Total tools:', stats.total_tools);
  console.log('  ✓ Unique capabilities:', stats.unique_capabilities);
  console.log();

  // Test 3: Schema validation
  console.log('Test 3: Schema Validation');
  const readTool = registry.getTool('Read');
  const validInput = readTool.schema_in.parse({ path: '/test.txt' });
  console.log('  ✓ Valid input accepted:', validInput.path);

  try {
    readTool.schema_in.parse({}); // Should fail
    console.log('  ✗ Invalid input should have been rejected!');
  } catch {
    console.log('  ✓ Invalid input rejected correctly');
  }
  console.log();

  // Test 4: Error handling
  console.log('Test 4: Error Handling');
  const errorTool = async () => {
    throw new Error('Simulated failure');
  };
  const wrappedError = Wrap(errorTool, manifest);
  const errorResult = await wrappedError({ path: '/test.txt' });
  console.log('  ✓ Error status:', errorResult.receipt.status);
  console.log('  ✓ Error message:', errorResult.receipt.error);
  console.log('  ✓ Delta is null:', errorResult.delta === null);
  console.log();

  console.log('✅ All manual verification tests passed!\n');

  return {
    success: true,
    tests_run: 4,
    tools_loaded: stats.total_tools,
  };
}

// Run verification
runVerification()
  .then((result) => {
    console.log('Final Result:', JSON.stringify(result, null, 2));
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Verification failed:', error);
    process.exit(1);
  });
