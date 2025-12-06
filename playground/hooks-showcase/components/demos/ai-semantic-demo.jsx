"use client";
import { useState } from "react";
import { Button } from "@/components/ui/button";
export function AISemanticDemo() {
  const [results, setResults] = useState([]);
  const search = () => setResults([{text: 'Semantic result', score: 0.95}]);
  return (<div className="space-y-4"><Button onClick={search}>Search</Button>{results.map((r,i)=><div key={i}>{r.text}</div>)}</div>);
}
