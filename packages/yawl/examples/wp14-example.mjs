/**
 * WP14 - Multiple Instances with Runtime A Priori Knowledge Example
 */

import { spawnInstancesRuntimeApriori } from '../src/multiple-instance/wp14-runtime-apriori.mjs';
import { TaskDefinition } from '../src/task-definitions.mjs';

async function main() {
  console.log('WP14: Multiple Instances with Runtime A Priori Knowledge\n');

  // Define task
  const processTask = new TaskDefinition({
    id: 'process-item',
    name: 'Process Item',
    splitType: 'sequence',
    joinType: 'and',
  });

  // Input data with items array
  const inputData = {
    items: [
      { id: 1, name: 'Item A' },
      { id: 2, name: 'Item B' },
      { id: 3, name: 'Item C' },
      { id: 4, name: 'Item D' },
      { id: 5, name: 'Item E' },
    ],
  };

  // Spawn instances - count determined from expression
  const result = await spawnInstancesRuntimeApriori(
    processTask,
    'count($.items)', // Evaluates to 5
    inputData,
    { caseId: 'case-example' }
  );

  console.log('Count evaluated:', result.countEvaluation.count);
  console.log('Spawned instances:', result.instances.length);
  console.log('Barrier status:', result.barrier.status);
  console.log('Receipt pattern:', result.receipt.pattern);

  console.log('\nInstances:');
  result.instances.forEach((inst, i) => {
    console.log(`  ${i}: ${inst.id} - ${inst.inputData.item.name}`);
  });

  console.log('\nSuccess!');
}

main().catch(console.error);
