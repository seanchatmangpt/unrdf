'use client';

import { useState } from 'react';
import Link from 'next/link';
import { Card, CardHeader, CardTitle, CardDescription, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { ArrowLeft, Check, X, Zap } from 'lucide-react';

/**
 * μ(O) Operator Composition Visualization
 *
 * Interactive demonstration of the 8 semantic operators of the μ(O) Calculus,
 * showing their composition, performance characteristics, and real-world usage
 * patterns from JTBD scenarios.
 */
export default function OperatorsPage() {
  const [selectedOperator, setSelectedOperator] = useState(null);

  const operators = [
    {
      id: 'μ₁',
      name: 'validate',
      fullName: 'Subject Coherence Validation',
      purpose: 'Schema validation and structural integrity checks using Zod',
      signature: 'μ₁(quad, schema) → {valid, invalid, error}',
      semantics: 'Checks quad subject, predicate, object against Zod schema. Enforces type constraints (IRI, literal, blank node).',
      performance: '10-15 μs',
      bottleneck: 'Zod schema parsing',
      optimization: 'Cache schema',
      category: 'Validation',
      color: 'bg-blue-500',
      lightColor: 'bg-blue-50',
      textColor: 'text-blue-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Validate order ID format' },
        { scenario: 'Payment Verification', usage: 'Validate card format (PCI compliance)' },
        { scenario: 'Shipping Address', usage: 'Validate address format (postal code, street)' }
      ]
    },
    {
      id: 'μ₂',
      name: 'transform',
      fullName: 'Ontology Transformation',
      purpose: 'Normalize and convert quad data to canonical representation',
      signature: 'μ₂(quad, transformation) → {quad, error}',
      semantics: 'Applies transformations to quad components. Normalizes units (currency, length, date). Converts between ontologies.',
      performance: '5-10 μs',
      bottleneck: 'Data conversion',
      optimization: 'Batch transforms',
      category: 'Transformation',
      color: 'bg-green-500',
      lightColor: 'bg-green-50',
      textColor: 'text-green-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Transform to schema.org triple structure' },
        { scenario: 'Multi-Currency', usage: 'Convert EUR price to USD equivalent' },
        { scenario: 'Shipping Address', usage: 'Normalize to USPS/DHL canonical format' }
      ]
    },
    {
      id: 'μ₃',
      name: 'enrich',
      fullName: 'Context Enrichment',
      purpose: 'Add contextual information by joining with external data sources',
      signature: 'μ₃(quad, context) → {quad + enrichments, error}',
      semantics: 'Fetches related data from store or API. Adds context as additional quads. Merges enrichments non-destructively.',
      performance: '50-200 μs',
      bottleneck: 'External I/O',
      optimization: 'Query caching',
      category: 'Enrichment',
      color: 'bg-yellow-500',
      lightColor: 'bg-yellow-50',
      textColor: 'text-yellow-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Check product availability (inventory)' },
        { scenario: 'Payment Verification', usage: 'Fraud scoring (external service)' },
        { scenario: 'Multi-Currency', usage: 'Fetch real-time exchange rate' }
      ]
    },
    {
      id: 'μ₄',
      name: 'filter',
      fullName: 'Conditional Filtering',
      purpose: 'Conditional exclusion of quads based on predicates',
      signature: 'μ₄(quad, predicate) → {quad, skip}',
      semantics: 'Evaluates boolean predicate on quad. Skips quad if predicate returns false. Passes through if true.',
      performance: '1-5 μs',
      bottleneck: 'Logic evaluation',
      optimization: 'JIT compile predicates',
      category: 'Filtering',
      color: 'bg-orange-500',
      lightColor: 'bg-orange-50',
      textColor: 'text-orange-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Filter by regional constraints (serviced regions)' },
        { scenario: 'Payment Verification', usage: 'Issuer country whitelist' },
        { scenario: 'Seasonal Availability', usage: 'Apply seller timezone offset' }
      ]
    },
    {
      id: 'μ₅',
      name: 'aggregate',
      fullName: 'Set Aggregation',
      purpose: 'Combine multiple quads into a single aggregated result',
      signature: 'μ₅({quad₁, ..., quadₙ}, aggregation) → {aggregated_quad, error}',
      semantics: 'Groups related quads by subject or property. Applies aggregation function (SUM, COUNT, CONCAT). Preserves provenance metadata.',
      performance: '5-20 μs',
      bottleneck: 'Grouping hash',
      optimization: 'Incremental aggregate',
      category: 'Aggregation',
      color: 'bg-red-500',
      lightColor: 'bg-red-50',
      textColor: 'text-red-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Seller verification (business legitimacy)' },
        { scenario: 'Payment Verification', usage: 'Merge with billing address' },
        { scenario: 'Inventory Sync', usage: 'Sum available across all sources' }
      ]
    },
    {
      id: 'μ₆',
      name: 'derive',
      fullName: 'Inference Derivation',
      purpose: 'Infer new quads from existing quads using logical rules',
      signature: 'μ₆(quad, rules) → {quad + inferred_quads, error}',
      semantics: 'Applies inference rules (Semantic Web, OWL). Generates new quads based on logical entailment. Handles transitivity, inverse properties.',
      performance: '10-50 μs',
      bottleneck: 'Rule complexity',
      optimization: 'Memoize rules',
      category: 'Derivation',
      color: 'bg-purple-500',
      lightColor: 'bg-purple-50',
      textColor: 'text-purple-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Infer payment compatibility from order type' },
        { scenario: 'Multi-Currency', usage: 'Calculate final total in buyer currency' },
        { scenario: 'Seasonal Availability', usage: 'Compute next availability date' }
      ]
    },
    {
      id: 'μ₇',
      name: 'monitor',
      fullName: 'Observability Monitoring',
      purpose: 'Emit observability signals (metrics, traces, logs) without modifying quads',
      signature: 'μ₇(quad) → {OTEL span, quad unchanged}',
      semantics: 'Records execution metrics (latency, success/failure). Emits OTEL spans with quad context. Transparent to downstream operators.',
      performance: '5-15 μs',
      bottleneck: 'OTEL span emission',
      optimization: 'Async emission',
      category: 'Monitoring',
      color: 'bg-cyan-500',
      lightColor: 'bg-cyan-50',
      textColor: 'text-cyan-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Terms acceptance (audit trail)' },
        { scenario: 'Payment Verification', usage: 'Log transaction for dispute' },
        { scenario: 'Seasonal Availability', usage: 'Alert seller X days before' }
      ]
    },
    {
      id: 'μ₈',
      name: 'sandbox',
      fullName: 'Sandboxed Execution',
      purpose: 'Execute effects in isolated, audited, transactional context',
      signature: 'μ₈(quad, effect) → {result, audit_log}',
      semantics: 'Creates isolated execution environment. Runs effect function (e.g., database write, external API call). Commits atomically or rolls back.',
      performance: '50-500 μs',
      bottleneck: 'Transaction commit',
      optimization: 'Connection pool',
      category: 'Sandboxing',
      color: 'bg-gray-600',
      lightColor: 'bg-gray-50',
      textColor: 'text-gray-700',
      examples: [
        { scenario: 'Order Fulfillment', usage: 'Order finalization (commit to system)' },
        { scenario: 'Payment Verification', usage: 'Tokenize and store securely' },
        { scenario: 'Inventory Sync', usage: 'Lock inventory for 10 minutes per order' }
      ]
    }
  ];

  const selectedOp = operators.find(op => op.id === selectedOperator);

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800">
      <div className="container mx-auto px-4 py-8 max-w-7xl">
        {/* Header with back button */}
        <div className="mb-8">
          <Link href="/hooks">
            <Button variant="ghost" className="mb-4">
              <ArrowLeft className="mr-2 h-4 w-4" />
              Back to Dashboard
            </Button>
          </Link>
          <h1 className="text-4xl font-bold text-slate-900 dark:text-slate-50 mb-2">
            μ(O) Operator Composition
          </h1>
          <p className="text-lg text-slate-600 dark:text-slate-400">
            8 semantic operators forming the complete intent-to-outcome transformation pipeline
          </p>
        </div>

        {/* Stats Banner */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
          <Card>
            <CardContent className="p-6">
              <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Total Operators</div>
              <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">8</div>
              <div className="text-xs text-slate-500">All necessary & sufficient</div>
            </CardContent>
          </Card>
          <Card>
            <CardContent className="p-6">
              <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Avg Execution</div>
              <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">0.853 μs</div>
              <div className="text-xs text-slate-500">Per operator</div>
            </CardContent>
          </Card>
          <Card>
            <CardContent className="p-6">
              <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">Throughput</div>
              <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">1.17M</div>
              <div className="text-xs text-slate-500">ops/sec</div>
            </CardContent>
          </Card>
          <Card>
            <CardContent className="p-6">
              <div className="text-sm text-slate-600 dark:text-slate-400 mb-1">JTBD Usage</div>
              <div className="text-3xl font-bold text-slate-900 dark:text-slate-50">100%</div>
              <div className="text-xs text-slate-500">All 8 scenarios</div>
            </CardContent>
          </Card>
        </div>

        {/* Operator Grid */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
          {operators.map((op, idx) => (
            <button
              key={op.id}
              onClick={() => setSelectedOperator(selectedOperator === op.id ? null : op.id)}
              className={`p-6 rounded-lg border-2 transition-all duration-200 text-left
                ${selectedOperator === op.id
                  ? `${op.color} text-white shadow-lg scale-105`
                  : `bg-white dark:bg-slate-800 hover:shadow-md ${op.lightColor} hover:border-slate-300`
                }`}
            >
              <div className="flex items-center justify-between mb-3">
                <span className="text-2xl font-bold">{op.id}</span>
                {selectedOperator === op.id && <Check className="h-5 w-5" />}
              </div>
              <div className={`text-sm font-semibold mb-1 ${selectedOperator === op.id ? 'text-white' : 'text-slate-900 dark:text-slate-50'}`}>
                {op.name}
              </div>
              <div className={`text-xs ${selectedOperator === op.id ? 'text-white/80' : 'text-slate-600 dark:text-slate-400'}`}>
                {op.performance}
              </div>
              <Badge className="mt-2" variant={selectedOperator === op.id ? "secondary" : "outline"}>
                {op.category}
              </Badge>
            </button>
          ))}
        </div>

        {/* Detailed View */}
        {selectedOp && (
          <Card className="mb-8 border-2">
            <CardHeader className={`${selectedOp.lightColor} border-b-2`}>
              <div className="flex items-center justify-between">
                <div>
                  <CardTitle className="text-2xl mb-2">
                    {selectedOp.id}: {selectedOp.name}
                  </CardTitle>
                  <CardDescription className="text-base">
                    {selectedOp.fullName}
                  </CardDescription>
                </div>
                <div className={`p-4 rounded-lg ${selectedOp.color} text-white`}>
                  <Zap className="h-8 w-8" />
                </div>
              </div>
            </CardHeader>
            <CardContent className="p-6">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                {/* Left Column */}
                <div>
                  <h3 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">Purpose</h3>
                  <p className="text-sm text-slate-600 dark:text-slate-400 mb-4">{selectedOp.purpose}</p>

                  <h3 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">Formal Signature</h3>
                  <code className="text-xs bg-slate-100 dark:bg-slate-800 p-2 rounded block mb-4 font-mono">
                    {selectedOp.signature}
                  </code>

                  <h3 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">Semantics</h3>
                  <p className="text-sm text-slate-600 dark:text-slate-400">{selectedOp.semantics}</p>
                </div>

                {/* Right Column */}
                <div>
                  <h3 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">Performance</h3>
                  <div className="grid grid-cols-3 gap-2 mb-4">
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded">
                      <div className="text-xs text-slate-600 dark:text-slate-400">Latency</div>
                      <div className="text-sm font-bold text-slate-900 dark:text-slate-50">{selectedOp.performance}</div>
                    </div>
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded">
                      <div className="text-xs text-slate-600 dark:text-slate-400">Bottleneck</div>
                      <div className="text-sm font-bold text-slate-900 dark:text-slate-50">{selectedOp.bottleneck}</div>
                    </div>
                    <div className="bg-slate-100 dark:bg-slate-800 p-3 rounded">
                      <div className="text-xs text-slate-600 dark:text-slate-400">Optimization</div>
                      <div className="text-sm font-bold text-slate-900 dark:text-slate-50">{selectedOp.optimization}</div>
                    </div>
                  </div>

                  <h3 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">JTBD Examples</h3>
                  <div className="space-y-2">
                    {selectedOp.examples.map((ex, idx) => (
                      <div key={idx} className="bg-slate-100 dark:bg-slate-800 p-3 rounded">
                        <div className="text-xs font-semibold text-slate-700 dark:text-slate-300">{ex.scenario}</div>
                        <div className="text-xs text-slate-600 dark:text-slate-400">{ex.usage}</div>
                      </div>
                    ))}
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>
        )}

        {/* Composition Patterns */}
        <Card>
          <CardHeader>
            <CardTitle>Common Composition Patterns</CardTitle>
            <CardDescription>How operators combine in real-world JTBD scenarios</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="p-4 bg-slate-50 dark:bg-slate-800 rounded-lg">
                <h4 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">Pattern 1: Validation → Transformation</h4>
                <p className="text-sm text-slate-600 dark:text-slate-400 mb-2">Use Case: Input normalization</p>
                <div className="flex items-center gap-2 text-sm font-mono">
                  <span className="px-2 py-1 bg-blue-100 dark:bg-blue-900 text-blue-700 dark:text-blue-300 rounded">μ₁</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-green-100 dark:bg-green-900 text-green-700 dark:text-green-300 rounded">μ₂</span>
                </div>
              </div>

              <div className="p-4 bg-slate-50 dark:bg-slate-800 rounded-lg">
                <h4 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">Pattern 2: Enrichment → Filtering</h4>
                <p className="text-sm text-slate-600 dark:text-slate-400 mb-2">Use Case: Context-aware filtering</p>
                <div className="flex items-center gap-2 text-sm font-mono">
                  <span className="px-2 py-1 bg-yellow-100 dark:bg-yellow-900 text-yellow-700 dark:text-yellow-300 rounded">μ₃</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-orange-100 dark:bg-orange-900 text-orange-700 dark:text-orange-300 rounded">μ₄</span>
                </div>
              </div>

              <div className="p-4 bg-slate-50 dark:bg-slate-800 rounded-lg">
                <h4 className="font-semibold mb-2 text-slate-900 dark:text-slate-50">Pattern 3: Full JTBD Pipeline</h4>
                <p className="text-sm text-slate-600 dark:text-slate-400 mb-2">Use Case: Order fulfillment workflow</p>
                <div className="flex items-center gap-2 text-sm font-mono flex-wrap">
                  <span className="px-2 py-1 bg-blue-100 dark:bg-blue-900 text-blue-700 dark:text-blue-300 rounded">μ₁</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-green-100 dark:bg-green-900 text-green-700 dark:text-green-300 rounded">μ₂</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-yellow-100 dark:bg-yellow-900 text-yellow-700 dark:text-yellow-300 rounded">μ₃</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-orange-100 dark:bg-orange-900 text-orange-700 dark:text-orange-300 rounded">μ₄</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-red-100 dark:bg-red-900 text-red-700 dark:text-red-300 rounded">μ₅</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-purple-100 dark:bg-purple-900 text-purple-700 dark:text-purple-300 rounded">μ₆</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-cyan-100 dark:bg-cyan-900 text-cyan-700 dark:text-cyan-300 rounded">μ₇</span>
                  <span>→</span>
                  <span className="px-2 py-1 bg-gray-100 dark:bg-gray-900 text-gray-700 dark:text-gray-300 rounded">μ₈</span>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
