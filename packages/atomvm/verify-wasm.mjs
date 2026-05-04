/**
 * @fileoverview Verify AtomVM WASM execution in Node.js
 */

import { AtomVMNodeRuntime } from './src/node-runtime.mjs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

async function verifyWasm() {
  console.log('Testing AtomVM Node.js Runtime...');
  
  const runtime = new AtomVMNodeRuntime({
    log: (msg) => console.log(`[Runtime] ${msg}`),
    errorLog: (msg) => console.error(`[Error] ${msg}`)
  });

  try {
    await runtime.load();
    console.log('✅ Runtime loaded successfully');

    const avmPath = join(__dirname, 'public/hello_world.avm');
    console.log(`Executing AVM: ${avmPath}`);
    
    const result = await runtime.execute(avmPath);
    console.log('✅ Execution complete');
    console.log('Output:', result.stdout);
    
    return true;
  } catch (error) {
    console.error('❌ Verification failed:');
    console.error(error.message);
    return false;
  } finally {
    runtime.destroy();
  }
}

verifyWasm().then(success => {
  process.exit(success ? 0 : 1);
});
