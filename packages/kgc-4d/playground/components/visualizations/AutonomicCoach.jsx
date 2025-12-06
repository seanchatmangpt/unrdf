'use client';

/**
 * AutonomicCoach - Rejection Explanation Modal
 *
 * Displays:
 * - Why rejection occurred (guard fired)
 * - Counter-factual receipt (what would have succeeded)
 * - How to fix (step-by-step)
 * - Similar successful deltas (examples)
 * - Pre-filled retry button
 *
 * Phase 3: Transform rejections into learning experiences
 */

import { useState } from 'react';
import { AlertTriangle, CheckCircle, XCircle, RefreshCw, Code, ArrowRight } from 'lucide-react';

export function AutonomicCoach({ rejection, onRetry, onDismiss }) {
  const [showRawDelta, setShowRawDelta] = useState(false);

  if (!rejection) return null;

  const { reason, counterFactual, examples } = rejection.coaching || {};

  // Handle cases where coaching hints might not be present
  if (!counterFactual) {
    return (
      <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
        <div className="bg-slate-900 border border-slate-700 rounded-xl max-w-2xl w-full p-6">
          <div className="flex items-start gap-3 mb-4">
            <XCircle className="w-6 h-6 text-red-400 flex-shrink-0 mt-0.5" />
            <div className="flex-1">
              <h3 className="text-lg font-semibold text-white mb-2">Operation Rejected</h3>
              <p className="text-sm text-red-300">{reason || 'Unknown error'}</p>
            </div>
          </div>

          <div className="flex justify-end gap-3 mt-6">
            <button
              onClick={onDismiss}
              className="px-4 py-2 rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-300 font-medium transition-colors"
            >
              Dismiss
            </button>
          </div>
        </div>
      </div>
    );
  }

  const handleRetryClick = () => {
    if (onRetry && counterFactual.correctedDelta) {
      onRetry(counterFactual.correctedDelta);
    }
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
      <div className="bg-slate-900 border border-slate-700 rounded-xl max-w-4xl w-full max-h-[90vh] overflow-y-auto">
        {/* Header */}
        <div className="sticky top-0 bg-slate-900 border-b border-slate-700 p-6">
          <div className="flex items-start justify-between">
            <div className="flex items-start gap-3">
              <AlertTriangle className="w-6 h-6 text-yellow-400 flex-shrink-0 mt-0.5" />
              <div>
                <h2 className="text-xl font-bold text-white mb-1">Why This Failed</h2>
                <p className="text-sm text-slate-400">
                  Let's learn from this rejection and try again
                </p>
              </div>
            </div>
            <button
              onClick={onDismiss}
              className="text-slate-400 hover:text-slate-300 transition-colors"
            >
              <XCircle className="w-5 h-5" />
            </button>
          </div>
        </div>

        <div className="p-6 space-y-6">
          {/* Rejection Reason */}
          <div className="bg-red-500/10 border border-red-500/30 rounded-lg p-4">
            <div className="flex items-start gap-3">
              <XCircle className="w-5 h-5 text-red-400 flex-shrink-0 mt-0.5" />
              <div className="flex-1">
                <div className="text-sm font-medium text-red-300 mb-1">
                  Guard Triggered: <span className="font-mono">{counterFactual.guardFired}</span>
                </div>
                <div className="text-sm text-red-400">{reason}</div>
              </div>
            </div>
          </div>

          {/* Counter-Factual Receipt */}
          <div className="bg-slate-800 border border-slate-700 rounded-lg p-5">
            <div className="flex items-center gap-2 mb-4">
              <CheckCircle className="w-5 h-5 text-green-400" />
              <h3 className="text-base font-semibold text-white">Counter-Factual Receipt</h3>
            </div>
            <p className="text-sm text-slate-300 mb-4">{counterFactual.suggestion}</p>

            {/* Corrected Delta Preview */}
            <div className="bg-slate-950 border border-slate-700 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <span className="text-xs font-medium text-slate-400">What Would Have Succeeded</span>
                <button
                  onClick={() => setShowRawDelta(!showRawDelta)}
                  className="flex items-center gap-1.5 px-2 py-1 rounded bg-slate-800 hover:bg-slate-700 text-xs text-slate-400 transition-colors"
                >
                  <Code className="w-3 h-3" />
                  {showRawDelta ? 'Hide' : 'Show'} Raw Delta
                </button>
              </div>

              {showRawDelta ? (
                <pre className="text-xs font-mono text-green-400 overflow-x-auto">
                  {JSON.stringify(counterFactual.correctedDelta, null, 2)}
                </pre>
              ) : (
                <div className="space-y-2">
                  <div className="flex items-center gap-2 text-sm">
                    <span className="text-slate-500">Type:</span>
                    <span className="text-slate-300 font-mono">{counterFactual.correctedDelta.type}</span>
                  </div>
                  <div className="flex items-center gap-2 text-sm">
                    <span className="text-slate-500">Subject:</span>
                    <span className="text-slate-300 font-mono text-xs truncate">
                      {counterFactual.correctedDelta.subject}
                    </span>
                  </div>
                  <div className="flex items-center gap-2 text-sm">
                    <span className="text-slate-500">Predicate:</span>
                    <span className="text-slate-300 font-mono text-xs">
                      {counterFactual.correctedDelta.predicate}
                    </span>
                  </div>
                  {counterFactual.correctedDelta.oldValue && (
                    <div className="flex items-center gap-2 text-sm">
                      <span className="text-slate-500">Old Value:</span>
                      <span className="text-red-400 font-mono line-through">
                        {counterFactual.correctedDelta.oldValue}
                      </span>
                    </div>
                  )}
                  {counterFactual.correctedDelta.suggestedValue && (
                    <div className="flex items-center gap-2 text-sm">
                      <span className="text-slate-500">Suggested Value:</span>
                      <span className="text-green-400 font-mono">
                        {counterFactual.correctedDelta.suggestedValue}
                      </span>
                    </div>
                  )}
                </div>
              )}
            </div>
          </div>

          {/* How to Fix */}
          {counterFactual.allowedRange && (
            <div className="bg-slate-800 border border-slate-700 rounded-lg p-5">
              <h3 className="text-base font-semibold text-white mb-3 flex items-center gap-2">
                <ArrowRight className="w-5 h-5 text-indigo-400" />
                How to Fix
              </h3>
              <ul className="space-y-2 text-sm text-slate-300">
                <li className="flex items-start gap-2">
                  <span className="text-indigo-400 mt-1">•</span>
                  <span>
                    Change <span className="font-mono text-red-400">{counterFactual.correctedDelta.oldValue}</span>
                    {' → '}
                    <span className="font-mono text-green-400">{counterFactual.correctedDelta.suggestedValue}</span>
                  </span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-indigo-400 mt-1">•</span>
                  <span>
                    Allowed range: {counterFactual.allowedRange.min} - {counterFactual.allowedRange.max}{' '}
                    {counterFactual.allowedRange.unit}
                  </span>
                </li>
              </ul>
            </div>
          )}

          {/* Similar Successful Deltas */}
          {examples && examples.length > 0 && (
            <div className="bg-slate-800 border border-slate-700 rounded-lg p-5">
              <h3 className="text-base font-semibold text-white mb-3">Similar Successful Deltas</h3>
              <div className="space-y-3">
                {examples.map((example, i) => (
                  <div key={i} className="bg-slate-950 border border-slate-700 rounded-lg p-3">
                    <div className="flex items-center justify-between mb-2">
                      <div className="flex items-center gap-2">
                        <CheckCircle className="w-4 h-4 text-green-400" />
                        <span className="text-xs font-medium text-green-400">ACK</span>
                      </div>
                      <span className="text-xs text-slate-500 font-mono">{example.timestamp}</span>
                    </div>
                    <pre className="text-xs font-mono text-slate-400 overflow-x-auto">
                      {JSON.stringify(example.delta, null, 2)}
                    </pre>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Actions */}
          <div className="flex items-center justify-between pt-4 border-t border-slate-700">
            <button
              onClick={onDismiss}
              className="px-4 py-2 rounded-lg bg-slate-800 hover:bg-slate-700 text-slate-300 font-medium transition-colors"
            >
              Dismiss
            </button>

            {counterFactual.correctedDelta && (
              <button
                onClick={handleRetryClick}
                className="flex items-center gap-2 px-5 py-2 rounded-lg bg-gradient-to-r from-indigo-500 to-purple-500 hover:from-indigo-600 hover:to-purple-600 text-white font-medium transition-all shadow-lg shadow-indigo-500/20"
              >
                <RefreshCw className="w-4 h-4" />
                Try Again (Pre-filled)
              </button>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
