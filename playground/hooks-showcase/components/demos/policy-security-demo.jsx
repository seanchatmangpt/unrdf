"use client";
import { useState } from "react";
import { Button } from "@/components/ui/button";
import { useKnowledgeEngine } from "../../lib/unrdf-kgc-bridge.mjs";
export function PolicySecurityDemo() {
  const { ready } = useKnowledgeEngine();
  const [results, setResults] = useState([]);
  const validate = () => setResults([{policy: 'Age', status: 'PASS'}]);
  return (<div className="space-y-4"><Button onClick={validate} disabled={!ready}>Validate</Button>{results.map((r,i)=><div key={i}>{r.policy}: {r.status}</div>)}</div>);
}
