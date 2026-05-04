import * as tf from '@tensorflow/tfjs-node';
import { MLVersionStore } from './packages/ml-versioning/src/version-store.mjs';

async function run() {
  const store = new MLVersionStore();
  const model = tf.sequential({ layers: [tf.layers.dense({ units: 1, inputShape: [1] })] });
  const metadata = { name: 'test-model' };
  const metrics = { accuracy: 0.9 };
  const receipt = await store.saveVersion(model, metadata, metrics);
  
  console.log('Saved:', receipt.versionId);
  const q = `SELECT * WHERE { ?s ?p ?o }`;
  const all = Array.from(await store.store.query(q));
  console.log('All triples:', all.length);
  for(let r of all) {
    console.log(r.get('s').value, r.get('p').value, r.get('o').value);
  }
}
run().catch(console.error);
