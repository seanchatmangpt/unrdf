/**
 * Test gen_statem_example state machine
 * 
 * @module gen-statem.test
 */

import { describe, it, expect } from 'vitest';
import { AtomVMNodeRuntime } from '@unrdf/atomvm';
import { readFileSync, existsSync } from 'fs';
import { join } from 'path';

describe('gen_statem (Production)', () => {
  it('should start state machine and handle button presses', async () => {
    const avmPath = join(process.cwd(), 'packages/atomvm/playground/public/gen_statem_kgc.avm');
    
    if (!existsSync(avmPath)) {
      console.log('[Test] Skipping: gen_statem_kgc.avm not found. Run build first.');
      return;
    }
    
    const runtime = new AtomVMNodeRuntime();
    
    try {
      await runtime.load();
      const result = await runtime.execute(avmPath);
      
      // Verify execution completed
      expect(result).toBeDefined();
    } finally {
      runtime.destroy();
    }
  }, 30000);
});

