'use client';

/**
 * MultiverseSandbox - Fork/Merge Reality Interface
 *
 * Allows users to:
 * - Fork reality from current time
 * - View parallel timelines (main + forks)
 * - Test destructive actions in sandbox
 * - Merge back or discard
 */

import { useState } from 'react';
import { useMultiverse } from '../../lib/hooks/useMultiverse.mjs';
import { useKGC } from '../../lib/client/kgc-context.mjs';
import { GitBranch, Plus, Trash2, GitMerge, AlertTriangle, CheckCircle } from 'lucide-react';

export function MultiverseSandbox() {
  const { forks, createFork, merge, discardFork, loading, error } = useMultiverse();
  const { eventLog } = useKGC();
  const [selectedFork, setSelectedFork] = useState(null);
  const [mergeResult, setMergeResult] = useState(null);

  const handleCreateFork = async () => {
    // Fork from current time (last event)
    if (!eventLog || eventLog.length === 0) {
      alert('No events in timeline. Connect and perform some actions first.');
      return;
    }

    const latestEvent = eventLog[eventLog.length - 1];
    const forkId = `fork-${Date.now()}`;

    const result = await createFork(forkId, BigInt(latestEvent.t_ns));

    if (result) {
      setSelectedFork(result.forkId);
    }
  };

  const handleMerge = async (forkId, strategy) => {
    const result = await merge(forkId, strategy);
    setMergeResult(result);

    // Clear result after 5s
    setTimeout(() => setMergeResult(null), 5000);
  };

  const handleDiscard = (forkId) => {
    if (confirm(`Discard fork ${forkId.slice(0, 8)}? This cannot be undone.`)) {
      discardFork(forkId);
      if (selectedFork === forkId) {
        setSelectedFork(null);
      }
    }
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-2 text-lg font-semibold text-white">
          <GitBranch className="w-5 h-5 text-orange-400" />
          <span>Multiverse Sandbox</span>
        </div>
        <button
          onClick={handleCreateFork}
          disabled={loading}
          className="flex items-center gap-2 px-4 py-2 rounded-lg bg-orange-500 hover:bg-orange-600 text-white font-medium transition-colors disabled:opacity-50"
        >
          <Plus className="w-4 h-4" />
          Fork Reality
        </button>
      </div>

      {/* Error Display */}
      {error && (
        <div className="bg-red-500/20 border border-red-500/50 rounded-lg p-4 flex items-start gap-3">
          <AlertTriangle className="w-5 h-5 text-red-400 mt-0.5" />
          <div>
            <div className="font-medium text-red-300">Error</div>
            <div className="text-sm text-red-400">{error}</div>
          </div>
        </div>
      )}

      {/* Merge Result */}
      {mergeResult && (
        <div
          className={`${
            mergeResult.status === 'success'
              ? 'bg-green-500/20 border-green-500/50'
              : mergeResult.status === 'conflict'
                ? 'bg-yellow-500/20 border-yellow-500/50'
                : 'bg-red-500/20 border-red-500/50'
          } border rounded-lg p-4 flex items-start gap-3`}
        >
          {mergeResult.status === 'success' ? (
            <>
              <CheckCircle className="w-5 h-5 text-green-400 mt-0.5" />
              <div>
                <div className="font-medium text-green-300">Merge Successful</div>
                <div className="text-sm text-green-400">
                  Merged {mergeResult.mergedEvents} events from fork
                </div>
              </div>
            </>
          ) : (
            <>
              <AlertTriangle className="w-5 h-5 text-yellow-400 mt-0.5" />
              <div>
                <div className="font-medium text-yellow-300">
                  {mergeResult.status === 'conflict' ? 'Merge Conflict' : 'Merge Failed'}
                </div>
                <div className="text-sm text-yellow-400">{mergeResult.message}</div>
                {mergeResult.conflicts && (
                  <div className="mt-2 text-xs text-yellow-500">
                    {mergeResult.conflicts.length} conflict(s) detected
                  </div>
                )}
              </div>
            </>
          )}
        </div>
      )}

      {/* Fork List */}
      {forks.length === 0 ? (
        <div className="bg-slate-900 border border-slate-700 rounded-lg p-8 text-center">
          <GitBranch className="w-12 h-12 text-slate-600 mx-auto mb-4" />
          <div className="text-slate-400">No active forks</div>
          <div className="text-sm text-slate-500 mt-2">
            Click "Fork Reality" to create a parallel timeline for testing
          </div>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {forks.map((fork) => (
            <div
              key={fork.forkId}
              className={`bg-slate-900 border-2 rounded-lg p-4 transition-all cursor-pointer ${
                selectedFork === fork.forkId
                  ? 'border-orange-500 ring-2 ring-orange-500/50'
                  : 'border-slate-700 hover:border-slate-600'
              }`}
              onClick={() => setSelectedFork(fork.forkId)}
            >
              {/* Fork Header */}
              <div className="flex items-start justify-between mb-3">
                <div className="flex items-center gap-2">
                  <GitBranch className="w-4 h-4 text-orange-400" />
                  <span className="text-sm font-mono text-slate-300">
                    {fork.forkId.slice(0, 12)}...
                  </span>
                </div>
                <div className="px-2 py-0.5 rounded text-xs font-medium bg-orange-500/20 text-orange-400">
                  Active
                </div>
              </div>

              {/* Fork Metadata */}
              <div className="space-y-2 text-xs">
                <div className="flex justify-between">
                  <span className="text-slate-500">Base Time:</span>
                  <span className="text-slate-300 font-mono">
                    {new Date(fork.baseTimeIso).toLocaleTimeString()}
                  </span>
                </div>
                <div className="flex justify-between">
                  <span className="text-slate-500">Quads:</span>
                  <span className="text-slate-300">{fork.quadCount}</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-slate-500">Events:</span>
                  <span className="text-slate-300">{fork.eventCount || 0}</span>
                </div>
              </div>

              {/* Actions */}
              <div className="mt-4 flex gap-2">
                <button
                  onClick={(e) => {
                    e.stopPropagation();
                    handleMerge(fork.forkId, 'auto');
                  }}
                  disabled={loading}
                  className="flex-1 flex items-center justify-center gap-1.5 px-3 py-1.5 rounded bg-green-500/20 hover:bg-green-500/30 text-green-400 text-xs font-medium transition-colors disabled:opacity-50"
                >
                  <GitMerge className="w-3 h-3" />
                  Merge
                </button>
                <button
                  onClick={(e) => {
                    e.stopPropagation();
                    handleDiscard(fork.forkId);
                  }}
                  disabled={loading}
                  className="flex items-center justify-center gap-1.5 px-3 py-1.5 rounded bg-red-500/20 hover:bg-red-500/30 text-red-400 text-xs font-medium transition-colors disabled:opacity-50"
                >
                  <Trash2 className="w-3 h-3" />
                </button>
              </div>
            </div>
          ))}
        </div>
      )}

      {/* Usage Instructions */}
      <div className="bg-slate-900 border border-slate-700 rounded-lg p-6">
        <div className="text-sm font-medium text-slate-300 mb-3">How to Use Multiverse</div>
        <ol className="space-y-2 text-sm text-slate-400">
          <li>1. Click "Fork Reality" to create a parallel timeline from the current state</li>
          <li>2. Apply test operations (e.g., destructive deletes) in the forked universe</li>
          <li>3. If satisfied, click "Merge" to apply changes to the main timeline</li>
          <li>4. If not needed, click the trash icon to discard the fork</li>
          <li className="text-orange-400 mt-3">
            ⚠️ Phase 2 MVP: Fork/merge UI is functional. Full delta application coming in Phase 2.5
          </li>
        </ol>
      </div>
    </div>
  );
}
