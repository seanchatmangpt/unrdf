import { createPeerManager } from './src/federation/peer-manager.mjs';

async function test() {
  const pm = createPeerManager();
  pm.registerPeer('dbpedia', 'https://dbpedia.org/sparql');
  pm.registerPeer('wikidata', 'https://query.wikidata.org/sparql');
  pm.registerPeer('local', 'http://localhost:3030/dataset/sparql');

  console.log('Pinging dbpedia...');
  const d = await pm.ping('dbpedia');
  console.log('dbpedia healthy:', d, pm.getPeer('dbpedia').status);

  console.log('Pinging wikidata...');
  const w = await pm.ping('wikidata');
  console.log('wikidata healthy:', w, pm.getPeer('wikidata').status);

  console.log('Pinging local...');
  const l = await pm.ping('local');
  console.log('local healthy:', l, pm.getPeer('local').status);
}

test().catch(console.error);
