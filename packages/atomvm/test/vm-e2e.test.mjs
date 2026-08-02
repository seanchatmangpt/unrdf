import { describe, it, expect } from 'vitest';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';
import { Powl8Builder } from '../src/vm/builder.mjs';

describe('AtomVM runtime end-to-end — real collaborators', () => {
  it('constructs a real POWL8 execution graph as a value', () => {
    const dag = new Powl8Builder()
      .addTask({ id: 'task1', name: 'Init' })
      .spawnSeq([
        { id: 'seq1', name: 'Step 1' },
        { id: 'seq2', name: 'Step 2' },
      ])
      .build();

    expect(dag).toBeDefined();
    expect(JSON.stringify(dag)).toContain('task1');
    expect(JSON.stringify(dag)).toContain('seq2');
  });

  it.skipIf(!(process.env.ATOMVM_BIN && process.env.ATOMVM_APP))(
    'executes the real AtomVM process through the public Node runtime',
    async () => {
      const runtime = new AtomVMNodeRuntime({
        atomvmBinary: process.env.ATOMVM_BIN,
        libraryPaths: process.env.ATOMVM_LIB ? [process.env.ATOMVM_LIB] : [],
        log: () => {},
        errorLog: () => {},
      });
      await runtime.load();
      const result = await runtime.execute(process.env.ATOMVM_APP);

      expect(result.runtime).toBe('AtomVM');
      expect(result.exitCode).toBe(0);
      expect(`${result.stdout}\n${result.stderr}`).toContain('atomvm_swarm_alive');
      expect(`${result.stdout}\n${result.stderr}`).toContain('Return value: ok');
    },
  );
});
