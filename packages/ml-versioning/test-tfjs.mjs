import * as tf from '@tensorflow/tfjs-node';
async function run() {
  const model = tf.sequential({ layers: [tf.layers.dense({ units: 1, inputShape: [1] })] });
  let artifacts;
  await model.save(tf.io.withSaveHandler(async a => { artifacts = a; return {}; }));
  console.log(Object.keys(artifacts));
}
run();
