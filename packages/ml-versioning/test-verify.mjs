import * as tf from '@tensorflow/tfjs-node';
import { MLVersionStore } from './src/version-store.mjs';

async function run() {
  const store = new MLVersionStore();
  const model = tf.sequential({ layers: [tf.layers.dense({ units: 1, inputShape: [1] })] });
  await store.saveVersion(model, { name: 'test' }, { epoch: 1 });
  await store.saveVersion(model, { name: 'test' }, { epoch: 2 });
  const verification = await store.verifyHashChain('test');
  console.log(verification.verifications);
}
run();
