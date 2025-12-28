// Add triples
store.addQuad(subj, pred, obj);

// Query
const quads = store.match(subj, pred, obj);

// Validate against SHACL shapes
const isValid = await validateShacl(store, shapes);