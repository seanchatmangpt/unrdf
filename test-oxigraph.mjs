import { OxigraphStore } from './packages/oxigraph/src/store.mjs';
const store = new OxigraphStore();
store.addQuad({ termType: 'NamedNode', value: 'http://s' }, { termType: 'NamedNode', value: 'http://p' }, { termType: 'NamedNode', value: 'http://o' });
const results = store.query('SELECT ?s ?p ?o WHERE { ?s ?p ?o }');
console.log(Array.from(results).map(r => {
  const s = r.get ? r.get('s') : r.s;
  return s.value;
}));