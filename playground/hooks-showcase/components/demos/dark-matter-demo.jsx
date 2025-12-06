"use client";
import { useState } from "react";
import { Button } from "@/components/ui/button";
export function DarkMatterDemo() {
  const [analysis, setAnalysis] = useState(null);
  const analyze = () => setAnalysis({time: 142, bottlenecks: ['Full scan']});
  return (<div className="space-y-4"><Button onClick={analyze}>Analyze</Button>{analysis && <div>{analysis.time}ms</div>}</div>);
}
