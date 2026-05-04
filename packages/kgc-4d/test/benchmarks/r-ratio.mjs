import { Bench } from 'tinybench';
import { KGCStore } from '../../src/store.mjs';
import { JitShaclValidator } from '../../src/validation/jit-shacl.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { EVENT_TYPES } from '../../src/constants.mjs';

/**
 * R-Ratio Benchmark
 * Measures the ratio between validation overhead and raw event ingestion time.
 * Goal: R-Ratio < 0.1 (Validation adds less than 10% latency)
 */

async function runBenchmark() {
  const validator = new JitShaclValidator();
  
  // Compile a dummy shape that simulates minimal validation logic
  validator.compileShapes([{
    id: 'http://example.org/AlwaysValid',
    validator: () => true
  }]);

  const storeWithoutValidation = new KGCStore({ nodeId: 'bench-node-1' });
  const storeWithDeltaValidation = new KGCStore({ 
    nodeId: 'bench-node-2',
    harden: true,
    validator
  });

  const bench = new Bench({ time: 2000 });

  let i = 0;
  bench
    .add('appendEvent (Raw Ingestion)', async () => {
      const s = dataFactory.namedNode(`http://example.org/s${i}`);
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal(`v${i}`);
      await storeWithoutValidation.appendEvent({ type: EVENT_TYPES.CREATE }, [
        { type: 'add', subject: s, predicate: p, object: o }
      ]);
      i++;
    })
    .add('appendEvent (With DELTA Validation)', async () => {
      const s = dataFactory.namedNode(`http://example.org/s${i}`);
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal(`v${i}`);
      await storeWithDeltaValidation.appendEvent({ type: EVENT_TYPES.CREATE }, [
        { type: 'add', subject: s, predicate: p, object: o }
      ]);
      i++;
    });

  console.log('='.repeat(70));
  console.log('R-Ratio Benchmark: Latency Collapse Validation');
  console.log('='.repeat(70));
  
  await bench.warmup();
  await bench.run();

  console.table(bench.table());

  const task1 = bench.getTask('appendEvent (Raw Ingestion)');
  const task2 = bench.getTask('appendEvent (With DELTA Validation)');

  const ingestionTime = task1.result.mean;
  const totalWithValidation = task2.result.mean;
  const validationOverhead = Math.max(0, totalWithValidation - ingestionTime);

  const rRatio = validationOverhead / ingestionTime;

  console.log('\nR-Ratio Analysis:');
  console.log(`  Mean Ingestion Time:       ${ingestionTime.toFixed(4)} ms`);
  console.log(`  Mean Validation Overhead:  ${validationOverhead.toFixed(4)} ms`);
  console.log(`  R-Ratio (Val/Ingest):      ${rRatio.toFixed(4)}`);
  
  const status = rRatio < 0.1 ? 'EXCELLENT' : (rRatio < 0.5 ? 'GOOD' : 'DEGRADED');
  console.log(`  Status:                    ${status}`);
  console.log('='.repeat(70));
}

runBenchmark().catch(console.error);
