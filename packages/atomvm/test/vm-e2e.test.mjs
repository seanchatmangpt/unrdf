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

  it('executes the configured native runtime or observes its real binary refusal', async () => {
    const configured = Boolean(process.env.ATOMVM_BIN && process.env.ATOMVM_APP);
    const runtime = new AtomVMNodeRuntime({
      atomvmBinary: configured ? process.env.ATOMVM_BIN : '/unavailable/AtomVM',
      libraryPaths: configured && process.env.ATOMVM_LIB ? [process.env.ATOMVM_LIB] : [],
      log: () => {},
      errorLog: () => {},
    });

    if (!configured) {
      await expect(runtime.load()).rejects.toThrow(/ATOMVM_BINARY_NOT_FOUND_REFUSED/);
      expect(runtime.state).toBe('Error');
      return;
    }

    await runtime.load();
    const result = await runtime.execute(process.env.ATOMVM_APP);
    expect(result.runtime).toBe('AtomVM');
    expect(result.exitCode).toBe(0);
    expect(`${result.stdout}\n${result.stderr}`).toContain('atomvm_swarm_alive');
    expect(`${result.stdout}\n${result.stderr}`).toContain('Return value: ok');
  });
});
