import { QueryEngine } from '@comunica/query-sparql';
import { Store } from 'n3';

const comunica = new QueryEngine();
const store = new Store();

// Add some test data
store.addQuad(
  { termType: 'NamedNode', value: 'http://example.org/alice' },
  { termType: 'NamedNode', value: 'http://example.org/name' },
  { termType: 'Literal', value: 'Alice' }
);

console.log('Store size:', store.size);

try {
  const result = await comunica.query(`
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE { 
      ?s ex:name ?name 
    }
  `, {
    sources: [store]
  });
  
  console.log('Result type:', result.resultType);
  console.log('Result:', result);
  
  if (result.resultType === 'bindings') {
    console.log('Has execute method:', typeof result.execute);
    console.log('Has metadata method:', typeof result.metadata);
    
    if (result.execute) {
      console.log('Calling execute...');
      const executed = await result.execute();
      console.log('Execute result:', executed);
      
      console.log('Iterating over executed result...');
      const rows = [];
      for await (const binding of executed) {
        console.log('Binding:', binding);
        rows.push(Object.fromEntries([...binding].map(([k, v]) => [k, v.value])));
      }
      console.log('Final rows:', rows);
    }
  }
} catch (error) {
  console.error('Error:', error.message);
}
