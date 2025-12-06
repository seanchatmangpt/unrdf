/**
 * @file playground/hooks-showcase/components/demos/composition-demo.jsx
 * @description Composition hooks demonstration - Pre-configured stacks
 */

"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";

export function CompositionDemo() {
  const [activeStack, setActiveStack] = useState(null);
  const [offlineMode, setOfflineMode] = useState(false);

  const stacks = [
    {
      id: "knowledge",
      name: "KnowledgeStack",
      description: "Complete knowledge management bundle",
      includes: ["useKnowledgeEngine", "useTransaction", "useValidation", "useChangeFeed"],
      color: "blue"
    },
    {
      id: "crud",
      name: "CRUDStack",
      description: "Essential CRUD operations",
      includes: ["useKnowledgeEngine", "useTriples", "useGraphs"],
      color: "green"
    },
    {
      id: "dashboard",
      name: "DashboardStack",
      description: "Real-time analytics and monitoring",
      includes: ["useQueryAnalyzer", "useChangeFeed", "useErrorBoundary"],
      color: "purple"
    },
    {
      id: "production",
      name: "ProductionStack",
      description: "Production-ready bundle with all features",
      includes: ["All Tier 1 hooks", "Error handling", "Performance monitoring", "Validation"],
      color: "orange"
    }
  ];

  const handleLoadStack = (stackId) => {
    setActiveStack(stackId);
  };

  return (
    <div className="space-y-6">
      {/* Offline Mode Toggle */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-sm font-medium text-slate-300">Offline-First Mode</h3>
            <p className="text-xs text-slate-500 mt-1">Enable IndexedDB caching for offline usage</p>
          </div>
          <Button
            variant={offlineMode ? "default" : "outline"}
            onClick={() => setOfflineMode(!offlineMode)}
          >
            {offlineMode ? "Online Mode" : "Offline Mode"}
          </Button>
        </div>
        {offlineMode && (
          <div className="mt-3 p-3 bg-green-900/20 border border-green-700/50 rounded text-xs text-green-400">
            ✓ Offline cache active - Changes will sync when connection is restored
          </div>
        )}
      </div>

      {/* Pre-configured Stacks */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {stacks.map((stack) => (
          <Card
            key={stack.id}
            className={`bg-slate-800 border-slate-700 cursor-pointer transition-all ${
              activeStack === stack.id ? `border-${stack.color}-500 shadow-lg shadow-${stack.color}-500/20` : ''
            }`}
            onClick={() => handleLoadStack(stack.id)}
          >
            <CardHeader>
              <div className="flex items-center justify-between">
                <CardTitle className="text-white text-lg">{stack.name}</CardTitle>
                {activeStack === stack.id && (
                  <Badge variant="success">Active</Badge>
                )}
              </div>
              <CardDescription className="text-slate-400">
                {stack.description}
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-2">
                <p className="text-xs font-medium text-slate-400 mb-2">Includes:</p>
                <div className="flex flex-wrap gap-1">
                  {stack.includes.map((hook, idx) => (
                    <Badge key={idx} variant="outline" className="text-xs text-slate-300 border-slate-600">
                      {hook}
                    </Badge>
                  ))}
                </div>
              </div>
            </CardContent>
          </Card>
        ))}
      </div>

      {/* Active Stack Info */}
      {activeStack && (
        <div className="bg-gradient-to-r from-blue-900/30 to-purple-900/30 border border-blue-700/50 rounded-lg p-4">
          <h3 className="text-sm font-medium text-blue-300 mb-2">Stack Initialized</h3>
          <p className="text-xs text-slate-400">
            {stacks.find(s => s.id === activeStack)?.name} is now ready. All included hooks are configured and optimized.
          </p>
          <div className="mt-3 grid grid-cols-3 gap-2 text-center">
            <div className="bg-slate-900/50 rounded p-2">
              <div className="text-lg font-bold text-green-400">✓</div>
              <div className="text-xs text-slate-400">Configured</div>
            </div>
            <div className="bg-slate-900/50 rounded p-2">
              <div className="text-lg font-bold text-blue-400">0ms</div>
              <div className="text-xs text-slate-400">Startup Time</div>
            </div>
            <div className="bg-slate-900/50 rounded p-2">
              <div className="text-lg font-bold text-purple-400">~5KB</div>
              <div className="text-xs text-slate-400">Bundle Size</div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
