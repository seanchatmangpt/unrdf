/**
 * @file playground/hooks-showcase/components/demos/federation-demo.jsx
 * @description Federation hooks demonstration (80/20 implementation)
 */

"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { useKnowledgeEngine } from "../../lib/unrdf-kgc-bridge.mjs";

export function FederationDemo() {
  const { ready } = useKnowledgeEngine();
  const [endpoint1Data, setEndpoint1Data] = useState([]);
  const [endpoint2Data, setEndpoint2Data] = useState([]);
  const [mergedData, setMergedData] = useState([]);

  const executeFederatedQuery = async () => {
    setEndpoint1Data([
      { source: 'DBpedia', name: 'Tim Berners-Lee', type: 'Person' },
      { source: 'DBpedia', name: 'Semantic Web', type: 'Concept' }
    ]);

    setEndpoint2Data([
      { source: 'Wikidata', name: 'RDF', type: 'Technology' },
      { source: 'Wikidata', name: 'W3C', type: 'Organization' }
    ]);

    setMergedData([...endpoint1Data, ...endpoint2Data]);
  };

  return (
    <div className="space-y-6">
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <h3 className="text-sm font-medium text-slate-300 mb-3">Federated Query</h3>
        <Button onClick={executeFederatedQuery} disabled={!ready}>Execute Query</Button>
      </div>
      <div className="grid grid-cols-2 gap-4">
        <div className="bg-blue-900/20 p-4 rounded">
          <h4 className="text-blue-400 mb-2">DBpedia</h4>
          {endpoint1Data.map((item, i) => <div key={i}>{item.name}</div>)}
        </div>
        <div className="bg-purple-900/20 p-4 rounded">
          <h4 className="text-purple-400 mb-2">Wikidata</h4>
          {endpoint2Data.map((item, i) => <div key={i}>{item.name}</div>)}
        </div>
      </div>
    </div>
  );
}
