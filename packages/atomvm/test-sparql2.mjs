import { createStore, dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal, quad, blankNode } = dataFactory;
const store = createStore();
store.add(quad(namedNode('http://ex/1'), namedNode('http://ex/p'), literal('Alice')));
const results = store.match(namedNode('http://ex/1'), null, null);
console.log(results[0]);
