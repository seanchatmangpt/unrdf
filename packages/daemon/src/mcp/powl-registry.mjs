export const powlRegistry = {
  powl_discover: ['powl_discover'],
  powl_conformance: ['powl_conformance'],
  powl_to_ontology: ['powl_to_ontology'],
};

export const powlRegistryMetadata = [
  {
    name: 'powl_discover',
    phase: 'core',
    description: 'Discover a POWL model from an event log using wasm4pm algorithms.',
    examples: ['powl_discover({logJson: "{...}", variant: "decision_graph_cyclic"})'],
  },
  {
    name: 'powl_conformance',
    phase: 'core',
    description: 'Compute token replay fitness for a POWL model against an event log.',
    examples: ['powl_conformance({powlModel: "X(A, B)", logJson: "{...}"})'],
  },
  {
    name: 'powl_to_ontology',
    phase: 'advanced',
    description: 'Convert a POWL model to an RDF representation and load it into Open Ontologies.',
    examples: ['powl_to_ontology({powlModel: "X(A, B)"})'],
  }
];

export function getPowlMetadata(toolName) {
  return powlRegistryMetadata.find(meta => meta.name === toolName) || null;
}
