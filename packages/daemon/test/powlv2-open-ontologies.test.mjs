import { describe, it, expect } from 'vitest';
import { powl_discover, powl_to_ontology, powl_conformance } from '../src/mcp/powl-handlers.mjs';
import { executeSemanticQuery } from '@unrdf/core/utils/semantic-bridge';

describe('Vision 2030: PoWL v2 and Open Ontologies Integration', { timeout: 30000 }, () => {

  const dummyEventLog = JSON.stringify({
    traces: [
      { case_id: '1', events: [{ name: 'A' }, { name: 'B' }] },
      { case_id: '2', events: [{ name: 'A' }, { name: 'C' }] }
    ]
  });

  const dummyPowlModel = "X(A, B)";

  it('should discover a PoWL model from an event log', async () => {
    const result = await powl_discover({ logJson: dummyEventLog, variant: 'decision_graph_cyclic' });
    expect(result.content[0].text).toBeDefined();
    
    // The result should contain the discovered model info from pictl
    if(result.content[0].text.startsWith('Error')) console.error(result.content[0].text);
    const data = JSON.parse(result.content[0].text);
    expect(data.result).toBeDefined();
  });

  it('should evaluate token replay fitness for a PoWL model', async () => {
    const result = await powl_conformance({ powlModel: dummyPowlModel, logJson: dummyEventLog });
    expect(result.content[0].text).toBeDefined();
    
    if(result.content[0].text.startsWith('Error')) console.error(result.content[0].text);
    const data = JSON.parse(result.content[0].text);
    expect(data.conformanceResult.percentage).toBe(100.0);
  });

  it('should convert a PoWL model into an RDF representation and verify it in Open Ontologies', async () => {
    const result = await powl_to_ontology({ powlModel: dummyPowlModel });
    expect(result.content[0].text).toBeDefined();
    
    if(result.content[0].text.startsWith('Error')) console.error(result.content[0].text);
    const data = JSON.parse(result.content[0].text);
    
    // We expect the RDF to have been loaded and queried successfully.
    // The query checks for ?node a powl:Node.
    expect(data.result.results.length).toBeGreaterThan(0);
    
    // Verify the node is represented in the Open Ontologies response
    const nodeUri = data.result.results[0].node;
    expect(nodeUri).toContain('http://example.org/powl#powl_');
  });

});
