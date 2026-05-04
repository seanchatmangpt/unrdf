import * as tf from '@tensorflow/tfjs-node';
import { MLVersionStore } from './src/version-store.mjs';

async function run() {
  const store = new MLVersionStore();
  const model = tf.sequential({ layers: [tf.layers.dense({ units: 1, inputShape: [1] })] });
  const metadata = { name: 'test-model' };
  const metrics = { accuracy: 0.9 };
  const receipt = await store.saveVersion(model, metadata, metrics);
  
  const q = `SELECT * WHERE { GRAPH ?g { ?s ?p ?o } }`;
  const all = Array.from(await store.store.query(q));
  console.log('Result type:', typeof all[0]);
  console.log('Result keys:', Object.keys(all[0]));
  console.log('s:', all[0].s);
}
run().catch(console.error);