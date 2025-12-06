'use client';

import Link from 'next/link';
import { useState } from 'react';
import { Card, CardHeader, CardTitle, CardDescription, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import {
  ArrowLeft,
  Activity,
  Zap,
  Database,
  FileCode,
  CheckCircle2,
  AlertCircle,
  GitBranch,
  Layers,
  Clock,
  ArrowRight,
  Play,
  Pause,
  RotateCcw
} from 'lucide-react';

/**
 * Hook Execution Flow Visualization
 *
 * Demonstrates the 4-phase execution pipeline, 3-tier caching strategy,
 * and real-time execution flow with error handling.
 */
export default function ExecutionFlowPage() {
  const [selectedView, setSelectedView] = useState('pipeline');
  const [animationStep, setAnimationStep] = useState(0);

  // 4-phase execution pipeline
  const executionPhases = [
    {
      phase: 0,
      name: 'Cache Warming',
      description: 'Initialize caches on first execution',
      icon: Database,
      color: 'bg-purple-500',
      steps: [
        'Pre-load all file content for hooks',
        'Initialize cache structures (store, condition)',
        'Emit initialization span to OTEL',
        'Setup quad pool (1000 pre-allocated quads)'
      ],
      timing: 'First-time only',
      performance: 'Amortized over many executions'
    },
    {
      phase: 1,
      name: 'Condition Evaluation',
      description: 'Parallel evaluation of hook conditions',
      icon: CheckCircle2,
      color: 'bg-blue-500',
      steps: [
        'Evaluate each hook condition ONCE per transaction',
        'Check condition cache (version-aware)',
        'Cache all results with store version',
        'Parallel execution: O(n) wall-clock time'
      ],
      timing: '0.3-0.5 μs per condition',
      performance: '40-50% reduction with cache'
    },
    {
      phase: 2,
      name: 'Hook Execution',
      description: 'Parallel hook execution with dependency ordering',
      icon: Play,
      color: 'bg-green-500',
      steps: [
        'Identify satisfied hooks from Phase 1',
        'Group by dependency (minimize serialization)',
        'Execute batches in parallel where possible',
        'Chain transformations (output → input)',
        'Collect results with receipt tracking'
      ],
      timing: '0.853 μs per operator',
      performance: '30-50% reduction with batching'
    },
    {
      phase: 3,
      name: 'Receipt Generation',
      description: 'Audit trail and telemetry',
      icon: FileCode,
      color: 'bg-orange-500',
      steps: [
        'Generate optional receipt for audit trails',
        'Log OTEL span completion',
        'Return aggregated results to caller',
        'Batch telemetry events (100ms buffer)'
      ],
      timing: '0.05-0.1 μs',
      performance: '10-15% reduction with batching'
    }
  ];

  // Three-tier caching strategy
  const cachingTiers = [
    {
      tier: 1,
      name: 'Oxigraph Store Caching',
      icon: Database,
      color: 'bg-blue-500',
      description: 'Caches RDF store instance for condition evaluation',
      hitCondition: 'Same store object used in multiple hook conditions',
      invalidation: 'Cleared on every transaction (store version change)',
      benefit: '50-70% latency reduction',
      sizeLimit: 'Default 10 cached stores (configurable)',
      baseline: '28 μs',
      withCache: '5 μs',
      improvement: '82%'
    },
    {
      tier: 2,
      name: 'Condition Evaluation Caching',
      icon: CheckCircle2,
      color: 'bg-green-500',
      description: 'Caches boolean results of condition evaluation',
      hitCondition: 'Key: (hook ID, store version)',
      invalidation: 'TTL-based (60s) or store version change',
      benefit: '40-50% latency reduction',
      sizeLimit: 'Version-tracked correctness guarantee',
      baseline: '15 μs',
      withCache: '3 μs',
      improvement: '80%'
    },
    {
      tier: 3,
      name: 'File Content Preloading',
      icon: FileCode,
      color: 'bg-purple-500',
      description: 'Pre-loads file content on engine initialization',
      hitCondition: 'First hook execution on engine instance',
      invalidation: 'All files referenced in hooks/transforms',
      benefit: '20-30% latency reduction',
      sizeLimit: 'Increased startup time (amortized)',
      baseline: '200 μs',
      withCache: '0 μs',
      improvement: '100%'
    }
  ];

  // Hook trigger types (33 total)
  const triggerCategories = [
    {
      category: 'Quad Lifecycle',
      count: 4,
      triggers: ['before-add', 'after-add', 'before-delete', 'after-delete'],
      icon: Activity,
      color: 'text-blue-500'
    },
    {
      category: 'Query Operations',
      count: 4,
      triggers: ['before-query', 'after-query', 'query-parse', 'results-filter'],
      icon: Database,
      color: 'text-green-500'
    },
    {
      category: 'Validation',
      count: 3,
      triggers: ['validate-quad', 'validate-subject', 'validate-predicate'],
      icon: CheckCircle2,
      color: 'text-purple-500'
    },
    {
      category: 'Transformation',
      count: 2,
      triggers: ['transform-namespace', 'transform-language-tag'],
      icon: GitBranch,
      color: 'text-orange-500'
    },
    {
      category: 'Data Quality',
      count: 3,
      triggers: ['quality-gate', 'defect-detection', 'spc-violation'],
      icon: AlertCircle,
      color: 'text-red-500'
    },
    {
      category: 'Session Management',
      count: 3,
      triggers: ['session-start', 'session-end', 'session-timeout'],
      icon: Clock,
      color: 'text-yellow-500'
    },
    {
      category: 'Store Management',
      count: 3,
      triggers: ['store-init', 'store-close', 'snapshot-create'],
      icon: Database,
      color: 'text-cyan-500'
    },
    {
      category: 'Transaction',
      count: 3,
      triggers: ['transaction-begin', 'transaction-commit', 'transaction-abort'],
      icon: GitBranch,
      color: 'text-indigo-500'
    },
    {
      category: 'Enrichment',
      count: 3,
      triggers: ['enrich-context', 'resolve-entity', 'expand-graph'],
      icon: Layers,
      color: 'text-pink-500'
    },
    {
      category: 'Observability',
      count: 3,
      triggers: ['emit-span', 'emit-metric', 'emit-log'],
      icon: Activity,
      color: 'text-teal-500'
    }
  ];

  // Performance features
  const performanceFeatures = [
    {
      feature: 'Parallel Execution',
      description: 'Independent hooks execute in parallel batches',
      formula: 'ExecuteInParallel(Hᵢ) where ∀Hᵢ, Hⱼ ∈ batch: ¬(Hᵢ → Hⱼ)',
      benefit: 'O(n) wall-clock time for n hooks',
      implementation: 'Dependency DAG with transitive closure',
      improvement: '30-50% reduction in wall-clock time'
    },
    {
      feature: 'Transformation Chaining',
      description: 'Output of one transform becomes input to next',
      formula: "quad' = Tₙ(Tₙ₋₁(⋯T₁(quad)))",
      benefit: 'Composable data processing',
      implementation: 'Sequential chain within same trigger',
      improvement: 'Enables complex multi-step transformations'
    },
    {
      feature: 'Quad Pooling',
      description: 'Pre-allocated quad objects to avoid GC pressure',
      formula: 'Pool size: 1000 quads (default)',
      benefit: '10-15% latency reduction',
      implementation: 'Acquire → Populate → Release pattern',
      improvement: 'Zero allocation during execution'
    },
    {
      feature: 'Dependency Batching',
      description: 'Group hooks by dependency graph before execution',
      formula: 'DAG transitive closure for independent sets',
      benefit: '30-50% reduction in wall-clock time',
      implementation: '1) Build DAG 2) Compute closure 3) Execute batches',
      improvement: 'Maximizes parallelism'
    },
    {
      feature: 'Batched Telemetry',
      description: 'Accumulate OTEL events before flushing',
      formula: 'Buffer: 100ms or until full',
      benefit: '10-15% latency reduction',
      implementation: 'Batch-emit to OTEL collector',
      improvement: 'Fewer OTEL calls'
    }
  ];

  // Error handling paths
  const errorPaths = [
    {
      type: 'Validation Failure',
      trigger: 'Hook returns error during validation',
      action: 'Stop chain, report reason',
      otel: 'Emit error event with full context',
      recovery: 'Transaction aborted, rollback state'
    },
    {
      type: 'Transformation Failure',
      trigger: 'Transform function throws exception',
      action: 'Include original quad + partial transform',
      otel: 'Emit error with stack trace',
      recovery: 'Continue with next hook, log failure'
    },
    {
      type: 'Condition Timeout',
      trigger: 'Condition evaluation exceeds timeout',
      action: 'Assume condition = false',
      otel: 'Emit timeout warning',
      recovery: 'Skip hook, continue pipeline'
    },
    {
      type: 'Effect Sandbox Timeout',
      trigger: 'Side effect exceeds 10-second timeout',
      action: 'Terminate worker thread',
      otel: 'Emit timeout event',
      recovery: 'Circuit breaker opens, fail-fast mode'
    },
    {
      type: 'Recursion Guard',
      trigger: 'Hook calls itself (depth > 3)',
      action: 'Throw recursion error',
      otel: 'Emit stack overflow prevention',
      recovery: 'Stop execution, prevent crash'
    }
  ];

  // Combined caching impact
  const cachingImpact = {
    baseline: {
      singleHook: 0.853,
      storeInstantiation: 28,
      conditionEval: 15,
      fileIO: 200,
      total10Hooks: 2850
    },
    withCache: {
      singleHook: 0.853,
      storeInstantiation: 5,
      conditionEval: 3,
      fileIO: 0,
      total10Hooks: 475
    },
    reduction: '80-92%'
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800">
      <div className="container mx-auto px-4 py-8 max-w-7xl">
        {/* Header with Back Button */}
        <div className="mb-8">
          <Link href="/hooks">
            <Button variant="ghost" className="mb-4">
              <ArrowLeft className="mr-2 h-4 w-4" />
              Back to Dashboard
            </Button>
          </Link>
          <h1 className="text-4xl font-bold text-slate-900 dark:text-slate-50 mb-2">
            Hook Execution Flow
          </h1>
          <p className="text-lg text-slate-600 dark:text-slate-400">
            3-tier caching strategy with real-time execution pipeline visualization
          </p>
        </div>

        {/* Stats Banner */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Execution Phases</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">4</div>
            <div className="text-xs text-slate-500">Cache → Condition → Execute → Receipt</div>
          </div>
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Caching Tiers</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">3</div>
            <div className="text-xs text-slate-500">Store, Condition, File</div>
          </div>
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Combined Reduction</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">80-92%</div>
            <div className="text-xs text-slate-500">Latency improvement</div>
          </div>
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Hook Triggers</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">33</div>
            <div className="text-xs text-slate-500">Across 10 categories</div>
          </div>
        </div>

        {/* Tab Navigation */}
        <div className="flex flex-wrap gap-2 mb-6 bg-white dark:bg-slate-800 p-2 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
          {['pipeline', 'caching', 'triggers', 'performance', 'errors'].map((view) => (
            <button
              key={view}
              onClick={() => setSelectedView(view)}
              className={`px-4 py-2 rounded-md transition-colors ${
                selectedView === view
                  ? 'bg-blue-500 text-white'
                  : 'bg-slate-100 dark:bg-slate-700 text-slate-700 dark:text-slate-300 hover:bg-slate-200 dark:hover:bg-slate-600'
              }`}
            >
              {view.charAt(0).toUpperCase() + view.slice(1)}
            </button>
          ))}
        </div>

        {/* Pipeline View */}
        {selectedView === 'pipeline' && (
          <div className="space-y-6">
            {/* Execution Flow Diagram */}
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Activity className="h-5 w-5 text-blue-500" />
                  4-Phase Execution Pipeline
                </CardTitle>
                <CardDescription>
                  Deterministic execution model with parallel condition evaluation and hook execution
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-6">
                  {executionPhases.map((phase, idx) => {
                    const Icon = phase.icon;
                    return (
                      <div key={phase.phase}>
                        <div className="flex items-start gap-4">
                          {/* Phase indicator */}
                          <div className="flex flex-col items-center">
                            <div className={`p-3 rounded-lg ${phase.color} bg-opacity-10 border-2 border-current`}>
                              <Icon className={`h-6 w-6 ${phase.color.replace('bg-', 'text-')}`} />
                            </div>
                            {idx < executionPhases.length - 1 && (
                              <div className="w-0.5 h-24 bg-slate-300 dark:bg-slate-600 my-2" />
                            )}
                          </div>

                          {/* Phase details */}
                          <div className="flex-1">
                            <div className="flex items-center gap-3 mb-2">
                              <Badge variant="outline">Phase {phase.phase}</Badge>
                              <h3 className="text-xl font-semibold text-slate-900 dark:text-slate-50">
                                {phase.name}
                              </h3>
                            </div>
                            <p className="text-slate-600 dark:text-slate-400 mb-3">
                              {phase.description}
                            </p>
                            <div className="bg-slate-50 dark:bg-slate-800 p-4 rounded-lg space-y-2">
                              {phase.steps.map((step, stepIdx) => (
                                <div key={stepIdx} className="flex items-start gap-2">
                                  <ArrowRight className="h-4 w-4 text-blue-500 mt-0.5 flex-shrink-0" />
                                  <span className="text-sm text-slate-700 dark:text-slate-300">
                                    {step}
                                  </span>
                                </div>
                              ))}
                            </div>
                            <div className="mt-3 flex gap-4 text-sm">
                              <span className="text-slate-600 dark:text-slate-400">
                                <strong>Timing:</strong> {phase.timing}
                              </span>
                              <span className="text-slate-600 dark:text-slate-400">
                                <strong>Performance:</strong> {phase.performance}
                              </span>
                            </div>
                          </div>
                        </div>
                      </div>
                    );
                  })}
                </div>
              </CardContent>
            </Card>

            {/* Combined Impact */}
            <Card>
              <CardHeader>
                <CardTitle>Combined Caching Impact</CardTitle>
                <CardDescription>
                  Multiplicative effect of three-tier caching on 10-hook execution
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="overflow-x-auto">
                  <table className="w-full text-sm">
                    <thead>
                      <tr className="border-b border-slate-200 dark:border-slate-700">
                        <th className="text-left py-3 px-4">Operation</th>
                        <th className="text-right py-3 px-4">Baseline</th>
                        <th className="text-right py-3 px-4">With Cache</th>
                        <th className="text-right py-3 px-4">Reduction</th>
                      </tr>
                    </thead>
                    <tbody>
                      <tr className="border-b border-slate-200 dark:border-slate-700">
                        <td className="py-3 px-4">Single hook execution</td>
                        <td className="text-right py-3 px-4">0.853 μs</td>
                        <td className="text-right py-3 px-4">0.853 μs</td>
                        <td className="text-right py-3 px-4">
                          <Badge variant="outline">baseline</Badge>
                        </td>
                      </tr>
                      <tr className="border-b border-slate-200 dark:border-slate-700">
                        <td className="py-3 px-4">Store instantiation</td>
                        <td className="text-right py-3 px-4">28 μs</td>
                        <td className="text-right py-3 px-4 text-green-600 dark:text-green-400 font-semibold">
                          5 μs
                        </td>
                        <td className="text-right py-3 px-4">
                          <Badge className="bg-green-500">82%</Badge>
                        </td>
                      </tr>
                      <tr className="border-b border-slate-200 dark:border-slate-700">
                        <td className="py-3 px-4">Condition evaluation</td>
                        <td className="text-right py-3 px-4">15 μs</td>
                        <td className="text-right py-3 px-4 text-green-600 dark:text-green-400 font-semibold">
                          3 μs
                        </td>
                        <td className="text-right py-3 px-4">
                          <Badge className="bg-green-500">80%</Badge>
                        </td>
                      </tr>
                      <tr className="border-b border-slate-200 dark:border-slate-700">
                        <td className="py-3 px-4">File I/O</td>
                        <td className="text-right py-3 px-4">200 μs</td>
                        <td className="text-right py-3 px-4 text-green-600 dark:text-green-400 font-semibold">
                          0 μs
                        </td>
                        <td className="text-right py-3 px-4">
                          <Badge className="bg-green-500">100%</Badge>
                        </td>
                      </tr>
                      <tr className="bg-slate-50 dark:bg-slate-800 font-semibold">
                        <td className="py-3 px-4">Total (10 hooks)</td>
                        <td className="text-right py-3 px-4">2,850 μs</td>
                        <td className="text-right py-3 px-4 text-green-600 dark:text-green-400">
                          450-500 μs
                        </td>
                        <td className="text-right py-3 px-4">
                          <Badge className="bg-green-500 text-lg">80-92%</Badge>
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </div>
              </CardContent>
            </Card>
          </div>
        )}

        {/* Caching View */}
        {selectedView === 'caching' && (
          <div className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Layers className="h-5 w-5 text-purple-500" />
                  Three-Tier Caching Strategy
                </CardTitle>
                <CardDescription>
                  Store caching, condition caching, and file preloading for multiplicative performance gains
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-6">
                  {cachingTiers.map((tier) => {
                    const Icon = tier.icon;
                    return (
                      <div key={tier.tier} className="border border-slate-200 dark:border-slate-700 rounded-lg p-6">
                        <div className="flex items-start gap-4 mb-4">
                          <div className={`p-3 rounded-lg ${tier.color} bg-opacity-10`}>
                            <Icon className={`h-6 w-6 ${tier.color.replace('bg-', 'text-')}`} />
                          </div>
                          <div className="flex-1">
                            <div className="flex items-center gap-3 mb-2">
                              <Badge variant="outline">Tier {tier.tier}</Badge>
                              <h3 className="text-xl font-semibold text-slate-900 dark:text-slate-50">
                                {tier.name}
                              </h3>
                            </div>
                            <p className="text-slate-600 dark:text-slate-400 mb-4">
                              {tier.description}
                            </p>
                          </div>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                          <div className="bg-slate-50 dark:bg-slate-800 p-4 rounded-lg">
                            <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-2">
                              Cache Details
                            </h4>
                            <div className="space-y-2 text-sm">
                              <div>
                                <span className="text-slate-600 dark:text-slate-400">Hit Condition: </span>
                                <span className="text-slate-900 dark:text-slate-50">{tier.hitCondition}</span>
                              </div>
                              <div>
                                <span className="text-slate-600 dark:text-slate-400">Invalidation: </span>
                                <span className="text-slate-900 dark:text-slate-50">{tier.invalidation}</span>
                              </div>
                              <div>
                                <span className="text-slate-600 dark:text-slate-400">Size Limit: </span>
                                <span className="text-slate-900 dark:text-slate-50">{tier.sizeLimit}</span>
                              </div>
                            </div>
                          </div>

                          <div className="bg-slate-50 dark:bg-slate-800 p-4 rounded-lg">
                            <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-2">
                              Performance Impact
                            </h4>
                            <div className="space-y-2 text-sm">
                              <div>
                                <span className="text-slate-600 dark:text-slate-400">Baseline: </span>
                                <span className="text-slate-900 dark:text-slate-50 font-mono">{tier.baseline}</span>
                              </div>
                              <div>
                                <span className="text-slate-600 dark:text-slate-400">With Cache: </span>
                                <span className="text-green-600 dark:text-green-400 font-mono font-semibold">
                                  {tier.withCache}
                                </span>
                              </div>
                              <div>
                                <Badge className="bg-green-500">
                                  {tier.improvement} improvement
                                </Badge>
                              </div>
                            </div>
                          </div>
                        </div>

                        <div className="mt-4 p-3 bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg">
                          <div className="flex items-center gap-2">
                            <Zap className="h-4 w-4 text-blue-600 dark:text-blue-400" />
                            <span className="font-semibold text-blue-900 dark:text-blue-100">
                              Expected Benefit:
                            </span>
                            <span className="text-blue-700 dark:text-blue-300">{tier.benefit}</span>
                          </div>
                        </div>
                      </div>
                    );
                  })}
                </div>
              </CardContent>
            </Card>
          </div>
        )}

        {/* Triggers View */}
        {selectedView === 'triggers' && (
          <div className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <GitBranch className="h-5 w-5 text-purple-500" />
                  33 Hook Trigger Types
                </CardTitle>
                <CardDescription>
                  Covering all critical junctures in knowledge processing across 10 categories
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  {triggerCategories.map((category) => {
                    const Icon = category.icon;
                    return (
                      <div key={category.category} className="border border-slate-200 dark:border-slate-700 rounded-lg p-5">
                        <div className="flex items-center gap-3 mb-3">
                          <Icon className={`h-5 w-5 ${category.color}`} />
                          <h3 className="font-semibold text-slate-900 dark:text-slate-50">
                            {category.category}
                          </h3>
                          <Badge variant="outline" className="ml-auto">
                            {category.count} triggers
                          </Badge>
                        </div>
                        <div className="space-y-1">
                          {category.triggers.map((trigger) => (
                            <div
                              key={trigger}
                              className="text-sm bg-slate-50 dark:bg-slate-800 px-3 py-2 rounded font-mono text-slate-700 dark:text-slate-300"
                            >
                              {trigger}
                            </div>
                          ))}
                        </div>
                      </div>
                    );
                  })}
                </div>

                <div className="mt-6 p-4 bg-purple-50 dark:bg-purple-900/20 border border-purple-200 dark:border-purple-800 rounded-lg">
                  <p className="text-sm text-purple-900 dark:text-purple-100">
                    <strong>Trigger Model:</strong> Each trigger type defines when and where policy enforcement occurs
                    in the system lifecycle. The 33 trigger types provide loose coupling - applications register
                    interest in specific events without knowing implementation details.
                  </p>
                </div>
              </CardContent>
            </Card>
          </div>
        )}

        {/* Performance View */}
        {selectedView === 'performance' && (
          <div className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Zap className="h-5 w-5 text-yellow-500" />
                  Performance Optimization Features
                </CardTitle>
                <CardDescription>
                  Parallel execution, transformation chaining, quad pooling, dependency batching, and batched telemetry
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  {performanceFeatures.map((feature) => (
                    <div key={feature.feature} className="border border-slate-200 dark:border-slate-700 rounded-lg p-5">
                      <h3 className="text-lg font-semibold text-slate-900 dark:text-slate-50 mb-2">
                        {feature.feature}
                      </h3>
                      <p className="text-slate-600 dark:text-slate-400 mb-4">
                        {feature.description}
                      </p>

                      <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mb-3">
                        <div className="bg-slate-50 dark:bg-slate-800 p-3 rounded">
                          <div className="text-xs text-slate-600 dark:text-slate-400 mb-1">Formula</div>
                          <div className="font-mono text-sm text-slate-900 dark:text-slate-50">
                            {feature.formula}
                          </div>
                        </div>
                        <div className="bg-slate-50 dark:bg-slate-800 p-3 rounded">
                          <div className="text-xs text-slate-600 dark:text-slate-400 mb-1">Implementation</div>
                          <div className="text-sm text-slate-900 dark:text-slate-50">
                            {feature.implementation}
                          </div>
                        </div>
                      </div>

                      <div className="flex items-center gap-4">
                        <Badge className="bg-green-500">{feature.improvement}</Badge>
                        <span className="text-sm text-slate-600 dark:text-slate-400">
                          {feature.benefit}
                        </span>
                      </div>
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>
          </div>
        )}

        {/* Errors View */}
        {selectedView === 'errors' && (
          <div className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <AlertCircle className="h-5 w-5 text-red-500" />
                  Error Handling & Recovery Paths
                </CardTitle>
                <CardDescription>
                  Comprehensive error propagation with OTEL integration and recovery strategies
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="overflow-x-auto">
                  <table className="w-full text-sm">
                    <thead>
                      <tr className="border-b border-slate-200 dark:border-slate-700">
                        <th className="text-left py-3 px-4">Error Type</th>
                        <th className="text-left py-3 px-4">Trigger</th>
                        <th className="text-left py-3 px-4">Action</th>
                        <th className="text-left py-3 px-4">OTEL</th>
                        <th className="text-left py-3 px-4">Recovery</th>
                      </tr>
                    </thead>
                    <tbody>
                      {errorPaths.map((error, idx) => (
                        <tr key={idx} className="border-b border-slate-200 dark:border-slate-700">
                          <td className="py-3 px-4">
                            <Badge variant="destructive">{error.type}</Badge>
                          </td>
                          <td className="py-3 px-4 text-slate-700 dark:text-slate-300">
                            {error.trigger}
                          </td>
                          <td className="py-3 px-4 text-slate-700 dark:text-slate-300">
                            {error.action}
                          </td>
                          <td className="py-3 px-4 text-slate-700 dark:text-slate-300">
                            {error.otel}
                          </td>
                          <td className="py-3 px-4">
                            <span className="text-green-600 dark:text-green-400 font-medium">
                              {error.recovery}
                            </span>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>

                <div className="mt-6 p-4 bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg">
                  <h4 className="font-semibold text-red-900 dark:text-red-100 mb-2">
                    Error Propagation Guarantee
                  </h4>
                  <p className="text-sm text-red-800 dark:text-red-200">
                    All hook failures are propagated with full context including hook name, trigger type,
                    original quad, transformed quad (if partial), exception details, and stack trace.
                    Every error is emitted as an OTEL event for observability.
                  </p>
                </div>
              </CardContent>
            </Card>
          </div>
        )}
      </div>
    </div>
  );
}
