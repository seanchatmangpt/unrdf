'use client';

import Link from 'next/link';
import { useState } from 'react';
import { Card, CardHeader, CardTitle, CardDescription, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import {
  ArrowLeft,
  Network,
  Layers,
  GitBranch,
  Database,
  Activity,
  Shield,
  Zap,
  AlertCircle
} from 'lucide-react';

/**
 * Architecture Diagrams Visualization
 *
 * Interactive visualizations of hook chains, caching architecture,
 * operator composition, and component relationships.
 */
export default function ArchitecturePage() {
  const [selectedDiagram, setSelectedDiagram] = useState('hook-chain');

  // Diagram catalog
  const diagrams = [
    {
      id: 'hook-chain',
      name: 'Hook Execution Chain',
      icon: GitBranch,
      color: 'text-blue-500',
      description: 'Flowchart showing quad → registration → condition → effect → result pipeline'
    },
    {
      id: 'caching',
      name: 'Three-Tier Caching',
      icon: Layers,
      color: 'text-green-500',
      description: 'Store cache, condition cache, and file preloading architecture'
    },
    {
      id: 'operators',
      name: 'Operator Composition',
      icon: Network,
      color: 'text-purple-500',
      description: '8 semantic operators with composition arrows showing common patterns'
    },
    {
      id: 'dependencies',
      name: 'Dependency Graph',
      icon: GitBranch,
      color: 'text-orange-500',
      description: 'Hook dependency DAG with parallel execution batches'
    },
    {
      id: 'errors',
      name: 'Error Propagation Paths',
      icon: AlertCircle,
      color: 'text-red-500',
      description: 'Error handling paths with circuit breaker and recovery flows'
    },
    {
      id: 'otel',
      name: 'OTEL Integration',
      icon: Activity,
      color: 'text-cyan-500',
      description: 'Span structure and telemetry emission points'
    },
    {
      id: 'event-flow',
      name: 'Event Sourcing Flow',
      icon: Database,
      color: 'text-yellow-500',
      description: 'Integration with KGC 4D changelog and audit trail'
    },
    {
      id: 'components',
      name: 'Component Architecture',
      icon: Shield,
      color: 'text-indigo-500',
      description: 'Engine, manager, executor, and quality components relationships'
    }
  ];

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
            Architecture Diagrams
          </h1>
          <p className="text-lg text-slate-600 dark:text-slate-400">
            Interactive visualizations of hook chains, caching, and operator composition
          </p>
        </div>

        {/* Stats Banner */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Diagrams</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">8</div>
            <div className="text-xs text-slate-500">Interactive visualizations</div>
          </div>
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Core Components</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">7</div>
            <div className="text-xs text-slate-500">Engine, Manager, Executor, etc.</div>
          </div>
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Caching Tiers</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">3</div>
            <div className="text-xs text-slate-500">Store, Condition, File</div>
          </div>
          <div className="bg-white dark:bg-slate-800 p-6 rounded-lg shadow-sm border border-slate-200 dark:border-slate-700">
            <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Execution Phases</div>
            <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">4</div>
            <div className="text-xs text-slate-500">Cache → Condition → Execute → Receipt</div>
          </div>
        </div>

        {/* Diagram Selection Grid */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
          {diagrams.map((diagram) => {
            const Icon = diagram.icon;
            const isSelected = selectedDiagram === diagram.id;
            return (
              <button
                key={diagram.id}
                onClick={() => setSelectedDiagram(diagram.id)}
                className={`p-4 rounded-lg border-2 text-left transition-all ${
                  isSelected
                    ? 'border-blue-500 bg-blue-50 dark:bg-blue-900/20 shadow-md'
                    : 'border-slate-200 dark:border-slate-700 bg-white dark:bg-slate-800 hover:border-blue-300'
                }`}
              >
                <Icon className={`h-6 w-6 ${diagram.color} mb-2`} />
                <div className="font-semibold text-slate-900 dark:text-slate-50 mb-1 text-sm">
                  {diagram.name}
                </div>
                <div className="text-xs text-slate-600 dark:text-slate-400">
                  {diagram.description}
                </div>
              </button>
            );
          })}
        </div>

        {/* Diagram Display */}
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              {diagrams.find((d) => d.id === selectedDiagram) && (
                <>
                  {(() => {
                    const Icon = diagrams.find((d) => d.id === selectedDiagram).icon;
                    return <Icon className={`h-5 w-5 ${diagrams.find((d) => d.id === selectedDiagram).color}`} />;
                  })()}
                  {diagrams.find((d) => d.id === selectedDiagram).name}
                </>
              )}
            </CardTitle>
            <CardDescription>
              {diagrams.find((d) => d.id === selectedDiagram)?.description}
            </CardDescription>
          </CardHeader>
          <CardContent>
            {/* Hook Chain Diagram */}
            {selectedDiagram === 'hook-chain' && (
              <div className="bg-slate-50 dark:bg-slate-900 p-8 rounded-lg">
                <svg viewBox="0 0 800 600" className="w-full h-auto">
                  {/* Quad Input */}
                  <rect x="50" y="50" width="120" height="60" fill="#3b82f6" rx="5" />
                  <text x="110" y="85" textAnchor="middle" fill="white" fontSize="14" fontWeight="bold">
                    Quad Input
                  </text>
                  <text x="110" y="100" textAnchor="middle" fill="white" fontSize="10">
                    RDF 4-tuple
                  </text>

                  {/* Arrow to Hook Registration */}
                  <line x1="170" y1="80" x2="230" y2="80" stroke="#64748b" strokeWidth="2" markerEnd="url(#arrowhead)" />

                  {/* Hook Registration */}
                  <rect x="230" y="50" width="140" height="60" fill="#10b981" rx="5" />
                  <text x="300" y="85" textAnchor="middle" fill="white" fontSize="14" fontWeight="bold">
                    Hook Registration
                  </text>
                  <text x="300" y="100" textAnchor="middle" fill="white" fontSize="10">
                    33 trigger types
                  </text>

                  {/* Arrow to Condition Evaluation */}
                  <line x1="370" y1="80" x2="430" y2="80" stroke="#64748b" strokeWidth="2" markerEnd="url(#arrowhead)" />

                  {/* Condition Evaluation */}
                  <rect x="430" y="50" width="140" height="60" fill="#8b5cf6" rx="5" />
                  <text x="500" y="85" textAnchor="middle" fill="white" fontSize="14" fontWeight="bold">
                    Condition Check
                  </text>
                  <text x="500" y="100" textAnchor="middle" fill="white" fontSize="10">
                    Boolean result
                  </text>

                  {/* Arrow down to Effect Execution */}
                  <line x1="500" y1="110" x2="500" y2="170" stroke="#64748b" strokeWidth="2" markerEnd="url(#arrowhead)" />

                  {/* Effect Execution */}
                  <rect x="430" y="170" width="140" height="80" fill="#f59e0b" rx="5" />
                  <text x="500" y="200" textAnchor="middle" fill="white" fontSize="14" fontWeight="bold">
                    8 Operators Execute
                  </text>
                  <text x="500" y="217" textAnchor="middle" fill="white" fontSize="9">
                    μ₁: validate
                  </text>
                  <text x="500" y="230" textAnchor="middle" fill="white" fontSize="9">
                    μ₂: transform
                  </text>
                  <text x="500" y="243" textAnchor="middle" fill="white" fontSize="9">
                    ... (6 more)
                  </text>

                  {/* Arrow to Result */}
                  <line x1="500" y1="250" x2="500" y2="310" stroke="#64748b" strokeWidth="2" markerEnd="url(#arrowhead)" />

                  {/* Result Output */}
                  <rect x="430" y="310" width="140" height="60" fill="#06b6d4" rx="5" />
                  <text x="500" y="345" textAnchor="middle" fill="white" fontSize="14" fontWeight="bold">
                    Result Output
                  </text>
                  <text x="500" y="360" textAnchor="middle" fill="white" fontSize="10">
                    Success/Error
                  </text>

                  {/* Cache layer indicators */}
                  <rect x="620" y="50" width="150" height="320" fill="#fef3c7" stroke="#fbbf24" strokeWidth="2" rx="5" />
                  <text x="695" y="75" textAnchor="middle" fontSize="12" fontWeight="bold" fill="#92400e">
                    Caching Layers
                  </text>
                  <rect x="635" y="90" width="120" height="40" fill="#fcd34d" rx="3" />
                  <text x="695" y="110" textAnchor="middle" fontSize="10" fill="#78350f">
                    Store Cache
                  </text>
                  <text x="695" y="123" textAnchor="middle" fontSize="8" fill="#92400e">
                    50-70% reduction
                  </text>

                  <rect x="635" y="145" width="120" height="40" fill="#fcd34d" rx="3" />
                  <text x="695" y="165" textAnchor="middle" fontSize="10" fill="#78350f">
                    Condition Cache
                  </text>
                  <text x="695" y="178" textAnchor="middle" fontSize="8" fill="#92400e">
                    40-50% reduction
                  </text>

                  <rect x="635" y="200" width="120" height="40" fill="#fcd34d" rx="3" />
                  <text x="695" y="220" textAnchor="middle" fontSize="10" fill="#78350f">
                    File Preload
                  </text>
                  <text x="695" y="233" textAnchor="middle" fontSize="8" fill="#92400e">
                    20-30% reduction
                  </text>

                  {/* Error path */}
                  <path d="M 570 80 L 620 80 L 620 450 L 110 450 L 110 370" stroke="#ef4444" strokeWidth="2" strokeDasharray="5,5" markerEnd="url(#arrowhead-error)" />
                  <text x="620" y="470" textAnchor="middle" fontSize="10" fill="#dc2626">
                    Error Path (Circuit Breaker)
                  </text>

                  {/* Arrow markers */}
                  <defs>
                    <marker id="arrowhead" markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto">
                      <polygon points="0 0, 10 3.5, 0 7" fill="#64748b" />
                    </marker>
                    <marker id="arrowhead-error" markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto">
                      <polygon points="0 0, 10 3.5, 0 7" fill="#ef4444" />
                    </marker>
                  </defs>
                </svg>

                <div className="mt-6 grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="bg-white dark:bg-slate-800 p-4 rounded-lg">
                    <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-2">Pipeline Phases</h4>
                    <ul className="text-sm space-y-1 text-slate-700 dark:text-slate-300">
                      <li>1. Quad arrives (RDF 4-tuple)</li>
                      <li>2. Hooks registered by trigger type</li>
                      <li>3. Conditions evaluated (with cache)</li>
                      <li>4. 8 operators execute in sequence</li>
                      <li>5. Result returned (success/error)</li>
                    </ul>
                  </div>
                  <div className="bg-white dark:bg-slate-800 p-4 rounded-lg">
                    <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-2">Performance</h4>
                    <ul className="text-sm space-y-1 text-slate-700 dark:text-slate-300">
                      <li>Dispatch: 0.1 ms</li>
                      <li>Condition check: 0.3-0.5 μs</li>
                      <li>8 operators: 6.8 μs total</li>
                      <li>Total overhead: 0.11 ms</li>
                      <li>Cache benefit: 80-92% reduction</li>
                    </ul>
                  </div>
                </div>
              </div>
            )}

            {/* Caching Architecture */}
            {selectedDiagram === 'caching' && (
              <div className="bg-slate-50 dark:bg-slate-900 p-8 rounded-lg">
                <svg viewBox="0 0 800 500" className="w-full h-auto">
                  {/* Tier 1: Store Cache */}
                  <rect x="50" y="50" width="680" height="100" fill="#3b82f6" fillOpacity="0.1" stroke="#3b82f6" strokeWidth="2" rx="5" />
                  <text x="400" y="75" textAnchor="middle" fontSize="16" fontWeight="bold" fill="#1e40af">
                    Tier 1: Oxigraph Store Caching
                  </text>
                  <text x="70" y="100" fontSize="12" fill="#1e40af">
                    Hit: Same store object in multiple hooks
                  </text>
                  <text x="70" y="120" fontSize="12" fill="#1e40af">
                    Invalidation: Cleared on transaction (store version)
                  </text>
                  <rect x="550" y="85" width="160" height="50" fill="#3b82f6" rx="3" />
                  <text x="630" y="107" textAnchor="middle" fill="white" fontSize="12" fontWeight="bold">
                    50-70% reduction
                  </text>
                  <text x="630" y="123" textAnchor="middle" fill="white" fontSize="10">
                    28 μs → 5 μs
                  </text>

                  {/* Tier 2: Condition Cache */}
                  <rect x="50" y="180" width="680" height="100" fill="#10b981" fillOpacity="0.1" stroke="#10b981" strokeWidth="2" rx="5" />
                  <text x="400" y="205" textAnchor="middle" fontSize="16" fontWeight="bold" fill="#047857">
                    Tier 2: Condition Evaluation Caching
                  </text>
                  <text x="70" y="230" fontSize="12" fill="#047857">
                    Key: (hook ID, store version)
                  </text>
                  <text x="70" y="250" fontSize="12" fill="#047857">
                    Invalidation: TTL-based (60s) or version change
                  </text>
                  <rect x="550" y="215" width="160" height="50" fill="#10b981" rx="3" />
                  <text x="630" y="237" textAnchor="middle" fill="white" fontSize="12" fontWeight="bold">
                    40-50% reduction
                  </text>
                  <text x="630" y="253" textAnchor="middle" fill="white" fontSize="10">
                    15 μs → 3 μs
                  </text>

                  {/* Tier 3: File Preload */}
                  <rect x="50" y="310" width="680" height="100" fill="#8b5cf6" fillOpacity="0.1" stroke="#8b5cf6" strokeWidth="2" rx="5" />
                  <text x="400" y="335" textAnchor="middle" fontSize="16" fontWeight="bold" fill="#6d28d9">
                    Tier 3: File Content Preloading
                  </text>
                  <text x="70" y="360" fontSize="12" fill="#6d28d9">
                    When: First hook execution on engine instance
                  </text>
                  <text x="70" y="380" fontSize="12" fill="#6d28d9">
                    What: All files referenced in hooks/transforms
                  </text>
                  <rect x="550" y="345" width="160" height="50" fill="#8b5cf6" rx="3" />
                  <text x="630" y="367" textAnchor="middle" fill="white" fontSize="12" fontWeight="bold">
                    20-30% reduction
                  </text>
                  <text x="630" y="383" textAnchor="middle" fill="white" fontSize="10">
                    200 μs → 0 μs
                  </text>

                  {/* Combined effect */}
                  <rect x="200" y="440" width="400" height="40" fill="#f59e0b" rx="5" />
                  <text x="400" y="465" textAnchor="middle" fill="white" fontSize="14" fontWeight="bold">
                    Combined Effect: 80-92% latency reduction
                  </text>
                </svg>

                <div className="mt-6 bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg p-4">
                  <h4 className="font-semibold text-yellow-900 dark:text-yellow-100 mb-2">
                    Multiplicative Caching Impact
                  </h4>
                  <p className="text-sm text-yellow-800 dark:text-yellow-200">
                    The three caching tiers work together multiplicatively: 10-hook execution drops from 2,850 μs
                    to 450-500 μs (80-92% reduction). Each tier targets a different bottleneck: store instantiation,
                    condition re-evaluation, and file I/O.
                  </p>
                </div>
              </div>
            )}

            {/* Operator Composition */}
            {selectedDiagram === 'operators' && (
              <div className="bg-slate-50 dark:bg-slate-900 p-8 rounded-lg">
                <svg viewBox="0 0 800 600" className="w-full h-auto">
                  {/* Central hub */}
                  <circle cx="400" cy="300" r="80" fill="#94a3b8" />
                  <text x="400" y="305" textAnchor="middle" fill="white" fontSize="16" fontWeight="bold">
                    μ(O) Calculus
                  </text>

                  {/* 8 operators in circular arrangement */}
                  {[
                    { name: 'μ₁: validate', angle: 0, color: '#3b82f6' },
                    { name: 'μ₂: transform', angle: 45, color: '#10b981' },
                    { name: 'μ₃: enrich', angle: 90, color: '#8b5cf6' },
                    { name: 'μ₄: filter', angle: 135, color: '#f59e0b' },
                    { name: 'μ₅: aggregate', angle: 180, color: '#ef4444' },
                    { name: 'μ₆: derive', angle: 225, color: '#06b6d4' },
                    { name: 'μ₇: monitor', angle: 270, color: '#ec4899' },
                    { name: 'μ₈: sandbox', angle: 315, color: '#84cc16' }
                  ].map((op, idx) => {
                    const rad = (op.angle * Math.PI) / 180;
                    const x = 400 + 200 * Math.cos(rad);
                    const y = 300 + 200 * Math.sin(rad);
                    return (
                      <g key={idx}>
                        <circle cx={x} cy={y} r="50" fill={op.color} />
                        <text x={x} y={y + 5} textAnchor="middle" fill="white" fontSize="12" fontWeight="bold">
                          {op.name}
                        </text>
                        <line x1="400" y1="300" x2={x} y2={y} stroke={op.color} strokeWidth="2" strokeDasharray="3,3" />
                      </g>
                    );
                  })}

                  {/* Common composition arrows */}
                  <path d="M 400 100 L 565 165" stroke="#64748b" strokeWidth="3" markerEnd="url(#arrow)" />
                  <text x="485" y="125" fill="#1e293b" fontSize="10">validate → transform</text>

                  <path d="M 565 235 L 565 365" stroke="#64748b" strokeWidth="3" markerEnd="url(#arrow)" />
                  <text x="575" y="305" fill="#1e293b" fontSize="10">transform → filter</text>

                  <path d="M 235 365 L 235 235" stroke="#64748b" strokeWidth="3" markerEnd="url(#arrow)" />
                  <text x="150" y="305" fill="#1e293b" fontSize="10">aggregate → derive</text>

                  <defs>
                    <marker id="arrow" markerWidth="10" markerHeight="10" refX="10" refY="5" orient="auto">
                      <polygon points="0 0, 10 5, 0 10" fill="#64748b" />
                    </marker>
                  </defs>
                </svg>

                <div className="mt-6 grid grid-cols-2 md:grid-cols-4 gap-3">
                  {[
                    { op: 'μ₁', name: 'validate', desc: 'Schema validation', color: 'bg-blue-500' },
                    { op: 'μ₂', name: 'transform', desc: 'Quad transformation', color: 'bg-green-500' },
                    { op: 'μ₃', name: 'enrich', desc: 'Context enrichment', color: 'bg-purple-500' },
                    { op: 'μ₄', name: 'filter', desc: 'Conditional filtering', color: 'bg-orange-500' },
                    { op: 'μ₅', name: 'aggregate', desc: 'Set aggregation', color: 'bg-red-500' },
                    { op: 'μ₆', name: 'derive', desc: 'Inference derivation', color: 'bg-cyan-500' },
                    { op: 'μ₇', name: 'monitor', desc: 'Performance monitoring', color: 'bg-pink-500' },
                    { op: 'μ₈', name: 'sandbox', desc: 'Security sandboxing', color: 'bg-lime-500' }
                  ].map((item) => (
                    <div key={item.op} className="bg-white dark:bg-slate-800 p-3 rounded-lg">
                      <div className={`${item.color} text-white px-2 py-1 rounded text-xs font-mono font-bold mb-1`}>
                        {item.op}
                      </div>
                      <div className="font-semibold text-sm text-slate-900 dark:text-slate-50">
                        {item.name}
                      </div>
                      <div className="text-xs text-slate-600 dark:text-slate-400">
                        {item.desc}
                      </div>
                    </div>
                  ))}
                </div>
              </div>
            )}

            {/* Other diagrams placeholder */}
            {['dependencies', 'errors', 'otel', 'event-flow', 'components'].includes(selectedDiagram) && (
              <div className="bg-slate-50 dark:bg-slate-900 p-12 rounded-lg text-center">
                <div className="max-w-2xl mx-auto">
                  <div className="text-6xl mb-4">📊</div>
                  <h3 className="text-2xl font-bold text-slate-900 dark:text-slate-50 mb-3">
                    {diagrams.find((d) => d.id === selectedDiagram)?.name}
                  </h3>
                  <p className="text-slate-600 dark:text-slate-400 mb-6">
                    This diagram visualization demonstrates {diagrams.find((d) => d.id === selectedDiagram)?.description.toLowerCase()}.
                  </p>

                  {/* Dependency Graph Info */}
                  {selectedDiagram === 'dependencies' && (
                    <div className="bg-white dark:bg-slate-800 p-6 rounded-lg text-left">
                      <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-3">Key Concepts</h4>
                      <ul className="space-y-2 text-sm text-slate-700 dark:text-slate-300">
                        <li>• Build dependency DAG from hook specifications</li>
                        <li>• Compute transitive closure to find independent sets</li>
                        <li>• Execute independent sets in parallel, respecting dependencies</li>
                        <li>• Result: 30-50% reduction in wall-clock time for large hook sets</li>
                      </ul>
                    </div>
                  )}

                  {/* Error Paths Info */}
                  {selectedDiagram === 'errors' && (
                    <div className="bg-white dark:bg-slate-800 p-6 rounded-lg text-left">
                      <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-3">Error Types</h4>
                      <ul className="space-y-2 text-sm text-slate-700 dark:text-slate-300">
                        <li>• Validation Failure: Stop chain, report reason</li>
                        <li>• Transformation Failure: Include original + partial quad</li>
                        <li>• Condition Timeout: Assume false, skip hook</li>
                        <li>• Effect Sandbox Timeout: Terminate worker, open circuit breaker</li>
                        <li>• Recursion Guard: Throw error (depth &gt; 3)</li>
                      </ul>
                    </div>
                  )}

                  {/* OTEL Integration Info */}
                  {selectedDiagram === 'otel' && (
                    <div className="bg-white dark:bg-slate-800 p-6 rounded-lg text-left">
                      <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-3">Span Structure</h4>
                      <pre className="text-xs bg-slate-900 text-slate-100 p-4 rounded-lg overflow-x-auto">
{`transaction_span
├── hook:validate-quad
│   ├── condition_eval
│   ├── validation_exec
│   └── telemetry_emit
├── hook:transform-namespace
│   ├── condition_eval
│   ├── transform_exec
│   └── telemetry_emit
└── receipt_generation`}
                      </pre>
                    </div>
                  )}

                  {/* Event Flow Info */}
                  {selectedDiagram === 'event-flow' && (
                    <div className="bg-white dark:bg-slate-800 p-6 rounded-lg text-left">
                      <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-3">KGC 4D Integration</h4>
                      <ul className="space-y-2 text-sm text-slate-700 dark:text-slate-300">
                        <li>• Event Types: "quad-added", "quad-removed", "policy-enforced"</li>
                        <li>• Payload: Operator execution trace, performance metrics</li>
                        <li>• Timestamp: Exact moment of enforcement</li>
                        <li>• Creates audit trail for compliance</li>
                      </ul>
                    </div>
                  )}

                  {/* Components Info */}
                  {selectedDiagram === 'components' && (
                    <div className="bg-white dark:bg-slate-800 p-6 rounded-lg text-left">
                      <h4 className="font-semibold text-slate-900 dark:text-slate-50 mb-3">Core Components</h4>
                      <ul className="space-y-2 text-sm text-slate-700 dark:text-slate-300">
                        <li>• KnowledgeHookEngine: Standalone execution core</li>
                        <li>• KnowledgeHookManager: Class wrapper for engine</li>
                        <li>• HookExecutor: Parallel hook execution</li>
                        <li>• QuadPool: Zero-allocation quad pooling</li>
                        <li>• ConditionCache: TTL-based caching</li>
                        <li>• QualityMetrics: Lean Six Sigma SPC</li>
                        <li>• EffectSandbox: Worker thread isolation</li>
                      </ul>
                    </div>
                  )}
                </div>
              </div>
            )}
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
