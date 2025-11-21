"use client";

import { useState } from "react";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";

// Demo components for each category
import { CoreDemo } from "@/components/demos/core-demo";
import { StreamingDemo } from "@/components/demos/streaming-demo";
import { DarkMatterDemo } from "@/components/demos/dark-matter-demo";
import { FederationDemo } from "@/components/demos/federation-demo";
import { CompositionDemo } from "@/components/demos/composition-demo";
import { ErrorRecoveryDemo } from "@/components/demos/error-recovery-demo";
import { FormUIDemo } from "@/components/demos/form-ui-demo";
import { AISemanticDemo } from "@/components/demos/ai-semantic-demo";
import { AdvancedUtilityDemo } from "@/components/demos/advanced-utility-demo";
import { PolicySecurityDemo } from "@/components/demos/policy-security-demo";

const HOOK_CATEGORIES = [
  {
    id: "core",
    name: "Core",
    tier: 1,
    usage: "40%",
    hooks: ["useKnowledgeEngine", "useKnowledgeEngineContext", "useTransaction", "useKnowledgeHook"],
    description: "Essential CRUD operations for every app",
    icon: "🎯"
  },
  {
    id: "streaming",
    name: "Streaming",
    tier: 1,
    usage: "20%",
    hooks: ["useChangeFeed", "useStreamProcessor", "useSubscriptionManager", "useRealTimeValidator", "useStreamingPipeline"],
    description: "Real-time updates and change feeds",
    icon: "📡"
  },
  {
    id: "dark-matter",
    name: "Dark Matter",
    tier: 1,
    usage: "15%",
    hooks: ["useDarkMatterCore", "useQueryAnalyzer", "useOptimizer", "useCriticalPath"],
    description: "Performance analysis and optimization",
    icon: "⚡"
  },
  {
    id: "federation",
    name: "Federation",
    tier: 3,
    usage: "5%",
    hooks: ["useFederatedSystem", "useConsensusManager", "useDistributedQuery", "useDataReplication", "useFederationHealth"],
    description: "Distributed queries across multiple stores",
    icon: "🌐"
  },
  {
    id: "composition",
    name: "Composition",
    tier: 0,
    usage: "NEW",
    hooks: ["useKnowledgeStack", "useCRUDStack", "useDashboardStack", "useProductionStack", "useOfflineStore"],
    description: "Pre-configured bundles and offline-first",
    icon: "🧩"
  },
  {
    id: "error-recovery",
    name: "Error & Recovery",
    tier: 2,
    usage: "15%",
    hooks: ["useErrorBoundary", "useRecovery", "useErrorReporting"],
    description: "Production-grade error handling",
    icon: "🛡️"
  },
  {
    id: "form-ui",
    name: "Form & UI",
    tier: 2,
    usage: "10%",
    hooks: ["useSPARQLEditor", "useGraphVisualizer", "useResultsPaginator", "useQueryBuilder", "useFormValidation"],
    description: "Query interfaces and visualization",
    icon: "🎨"
  },
  {
    id: "ai-semantic",
    name: "AI & Semantic",
    tier: 3,
    usage: "5%",
    hooks: ["useSemanticAnalyzer", "useNLPQueryBuilder", "useEmbeddingsManager", "useAnomalyDetector"],
    description: "AI-powered semantic analysis and NLP",
    icon: "🤖"
  },
  {
    id: "advanced-utility",
    name: "Advanced Utility",
    tier: 4,
    usage: "3%",
    hooks: ["useGraphDiff", "useIsomorphism", "useReasoningSession", "useGraphMerge", "useQualityMetrics", "useObservabilityManager"],
    description: "Graph comparison, reasoning, and observability",
    icon: "🔧"
  },
  {
    id: "policy-security",
    name: "Policy & Security",
    tier: 3,
    usage: "5%",
    hooks: ["usePolicyPack", "useSecurityValidator", "useSandbox"],
    description: "SHACL policies, access control, and sandboxing",
    icon: "🔐"
  }
];

export default function HooksShowcase() {
  const [activeTab, setActiveTab] = useState("core");

  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-900 to-slate-800 text-white">
      {/* Header */}
      <header className="border-b border-slate-700 bg-slate-900/50 backdrop-blur">
        <div className="container mx-auto px-4 py-6">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold bg-gradient-to-r from-blue-400 to-purple-500 bg-clip-text text-transparent">
                UNRDF Hooks Showcase
              </h1>
              <p className="text-slate-400 mt-1">
                Interactive demo of 40 production-ready React hooks
              </p>
            </div>
            <div className="flex gap-2">
              <Badge variant="success">v3.1.0</Badge>
              <Badge variant="secondary">40 Hooks</Badge>
              <Badge variant="outline">10 Categories</Badge>
            </div>
          </div>
        </div>
      </header>

      {/* Stats Bar */}
      <div className="bg-slate-800/50 border-b border-slate-700">
        <div className="container mx-auto px-4 py-4">
          <div className="grid grid-cols-4 gap-4 text-center">
            <div>
              <div className="text-2xl font-bold text-blue-400">7</div>
              <div className="text-sm text-slate-400">Tier 1 (80% usage)</div>
            </div>
            <div>
              <div className="text-2xl font-bold text-green-400">9</div>
              <div className="text-sm text-slate-400">Tier 2-3 (15% usage)</div>
            </div>
            <div>
              <div className="text-2xl font-bold text-purple-400">19</div>
              <div className="text-sm text-slate-400">Tier 4 (5% usage)</div>
            </div>
            <div>
              <div className="text-2xl font-bold text-orange-400">5</div>
              <div className="text-sm text-slate-400">Innovation Hooks</div>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <main className="container mx-auto px-4 py-8">
        <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-6">
          <TabsList className="flex flex-wrap gap-2 bg-transparent h-auto p-0">
            {HOOK_CATEGORIES.map((cat) => (
              <TabsTrigger
                key={cat.id}
                value={cat.id}
                className="data-[state=active]:bg-blue-600 data-[state=active]:text-white bg-slate-700 text-slate-300 px-4 py-2 rounded-lg"
              >
                <span className="mr-2">{cat.icon}</span>
                {cat.name}
                <Badge
                  variant={cat.tier === 0 ? "success" : cat.tier === 1 ? "default" : "secondary"}
                  className="ml-2 text-xs"
                >
                  {cat.tier === 0 ? "NEW" : `T${cat.tier}`}
                </Badge>
              </TabsTrigger>
            ))}
          </TabsList>

          {/* Category Info Card */}
          {HOOK_CATEGORIES.map((cat) => (
            <TabsContent key={cat.id} value={cat.id} className="space-y-6">
              <Card className="bg-slate-800 border-slate-700">
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <div>
                      <CardTitle className="text-white flex items-center gap-2">
                        <span className="text-2xl">{cat.icon}</span>
                        {cat.name} Hooks
                      </CardTitle>
                      <CardDescription className="text-slate-400">
                        {cat.description}
                      </CardDescription>
                    </div>
                    <div className="text-right">
                      <div className="text-sm text-slate-400">Usage</div>
                      <div className="text-2xl font-bold text-blue-400">{cat.usage}</div>
                    </div>
                  </div>
                  <div className="flex flex-wrap gap-2 mt-4">
                    {cat.hooks.map((hook) => (
                      <Badge key={hook} variant="outline" className="text-slate-300 border-slate-600">
                        {hook}
                      </Badge>
                    ))}
                  </div>
                </CardHeader>
              </Card>

              {/* Demo Component */}
              <Card className="bg-slate-800 border-slate-700">
                <CardHeader>
                  <CardTitle className="text-white">Interactive Demo</CardTitle>
                  <CardDescription className="text-slate-400">
                    Try out the hooks below
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  {cat.id === "core" && <CoreDemo />}
                  {cat.id === "streaming" && <StreamingDemo />}
                  {cat.id === "dark-matter" && <DarkMatterDemo />}
                  {cat.id === "federation" && <FederationDemo />}
                  {cat.id === "composition" && <CompositionDemo />}
                  {cat.id === "error-recovery" && <ErrorRecoveryDemo />}
                  {cat.id === "form-ui" && <FormUIDemo />}
                  {cat.id === "ai-semantic" && <AISemanticDemo />}
                  {cat.id === "advanced-utility" && <AdvancedUtilityDemo />}
                  {cat.id === "policy-security" && <PolicySecurityDemo />}
                </CardContent>
              </Card>
            </TabsContent>
          ))}
        </Tabs>
      </main>

      {/* Footer */}
      <footer className="border-t border-slate-700 bg-slate-900/50">
        <div className="container mx-auto px-4 py-6 text-center text-slate-400">
          <p>UNRDF Hooks Showcase - Built with Next.js + shadcn/ui</p>
          <p className="text-sm mt-2">
            80% of apps only need Tier 1 hooks. Start simple, scale as needed.
          </p>
        </div>
      </footer>
    </div>
  );
}
