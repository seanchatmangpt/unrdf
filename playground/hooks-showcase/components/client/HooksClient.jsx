"use client";

import { useState } from "react";
import { CoreDemoNew as CoreDemo } from "@/components/demos/core-demo-new";
import { StreamingDemoNew as StreamingDemo } from "@/components/demos/streaming-demo-new";
import { DarkMatterDemo } from "@/components/demos/dark-matter-demo";
import { ErrorRecoveryDemo } from "@/components/demos/error-recovery-demo";
import { FormUIDemo } from "@/components/demos/form-ui-demo";

// Demos mapped to the real hook tiers — only hooks that actually exist in the bridge.
// Fabricated demo tabs (federation hooks, AI hooks, composition hooks that don't exist)
// are replaced with an honest "Not yet implemented" panel.
const REAL_DEMOS = [
  {
    id: "core",
    label: "Core (T1)",
    hooks: ["useKnowledgeEngine", "useTriples", "useSPARQLQuery", "useGraphs"],
    Demo: CoreDemo,
  },
  {
    id: "streaming",
    label: "Streaming (T1)",
    hooks: ["useChangeFeed"],
    Demo: StreamingDemo,
  },
  {
    id: "dark-matter",
    label: "Analysis (T3)",
    hooks: ["useDarkMatterCore", "useQueryAnalyzer"],
    Demo: DarkMatterDemo,
  },
  {
    id: "error",
    label: "Error (T2)",
    hooks: ["useErrorBoundary"],
    Demo: ErrorRecoveryDemo,
  },
  {
    id: "sparql-ui",
    label: "SPARQL UI",
    hooks: ["useSPARQLQuery"],
    Demo: FormUIDemo,
  },
];

function NotImplementedPanel({ label }) {
  return (
    <div className="bg-slate-900 border border-slate-700 rounded-lg p-6 text-slate-400 text-sm">
      <p>
        <strong className="text-slate-300">{label}</strong> — no hook implementation exists yet in{" "}
        <code className="text-blue-400">packages/kgc-4d/src/lib/unrdf-kgc-bridge.mjs</code>.
      </p>
      <p className="mt-2">This panel will be populated when the bridge exports the corresponding hook.</p>
    </div>
  );
}

export function HooksClient({ hooks }) {
  const [activeTab, setActiveTab] = useState("core");

  const activeDemo = REAL_DEMOS.find((d) => d.id === activeTab) ?? REAL_DEMOS[0];
  const Demo = activeDemo.Demo ?? NotImplementedPanel;

  return (
    <div className="space-y-4">
      <h2 className="text-xl font-semibold text-slate-200">Interactive Demos</h2>
      <p className="text-slate-400 text-sm">
        {hooks.length} real hooks — demos below use only APIs that exist in the bridge. Tabs for
        non-existent hook categories are not shown.
      </p>

      {/* Tab bar — only real hooks */}
      <div className="flex flex-wrap gap-2">
        {REAL_DEMOS.map((demo) => (
          <button
            key={demo.id}
            onClick={() => setActiveTab(demo.id)}
            className={`px-4 py-2 rounded-lg text-sm font-medium transition-colors ${
              activeTab === demo.id
                ? "bg-blue-600 text-white"
                : "bg-slate-700 text-slate-300 hover:bg-slate-600"
            }`}
          >
            {demo.label}
          </button>
        ))}
      </div>

      {/* Hook pills for active tab */}
      <div className="flex flex-wrap gap-2">
        {activeDemo.hooks.map((hook) => (
          <span
            key={hook}
            className="px-2 py-1 bg-slate-800 border border-slate-600 rounded text-blue-300 font-mono text-xs"
          >
            {hook}
          </span>
        ))}
      </div>

      {/* Demo panel */}
      <div className="bg-slate-800 border border-slate-700 rounded-lg p-4">
        <Demo />
      </div>
    </div>
  );
}
