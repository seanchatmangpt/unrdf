import { createStore, dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal, quad, blankNode } = dataFactory;
const store = createStore();
store.add(quad(namedNode('http://ex/1'), namedNode('http://xmlns.com/foaf/0.1/name'), literal('Alice')));
const results = store.query('SELECT ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }');
console.log(results);
