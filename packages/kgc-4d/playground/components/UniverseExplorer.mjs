'use client';

/**
 * Universe Explorer - Visual representation of the 4D Architecture
 *
 * Shows:
 * - Server (Universe) with Oxigraph, Event Log, Git Backbone
 * - Browser (Shard) with local view
 * - The Tether (connection) between them
 */

import { motion } from 'framer-motion';
import { useUniverseStats, useConnection, useVectorClock } from '../lib/client/hooks.mjs';
import { Database, GitBranch, Layers, Wifi, WifiOff, RefreshCw, Server, Monitor } from 'lucide-react';

export function UniverseExplorer() {
  const { stats, loading, refresh } = useUniverseStats();
  const { state: connectionState, isConnected, isSyncing } = useConnection();
  const { formatted: clockFormatted } = useVectorClock();

  return (
    <div className="bg-slate-900 border border-slate-800 rounded-xl p-6 space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <h2 className="text-xl font-bold text-white flex items-center gap-2">
          <Layers className="w-5 h-5 text-universe-500" />
          4D Architecture Explorer
        </h2>
        <button
          onClick={refresh}
          disabled={loading}
          className="p-2 rounded-lg bg-slate-800 hover:bg-slate-700 transition-colors"
        >
          <RefreshCw className={`w-4 h-4 text-slate-400 ${loading ? 'animate-spin' : ''}`} />
        </button>
      </div>

      {/* Architecture Diagram */}
      <div className="relative">
        {/* Server (Universe) */}
        <motion.div
          initial={{ opacity: 0, y: -20 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-slate-800/50 border border-universe-500/30 rounded-xl p-6 glow-universe"
        >
          <div className="flex items-center gap-2 mb-4">
            <Server className="w-5 h-5 text-universe-500" />
            <h3 className="font-semibold text-universe-400">Server — The Universe</h3>
          </div>

          <div className="grid grid-cols-3 gap-4">
            {/* Oxigraph */}
            <div className="bg-slate-900/50 rounded-lg p-4 space-y-2">
              <div className="flex items-center gap-2">
                <Database className="w-4 h-4 text-blue-400" />
                <span className="text-sm font-medium text-slate-300">Oxigraph</span>
              </div>
              <div className="text-2xl font-bold text-white">
                {stats?.universe?.quad_count?.toLocaleString() || '—'}
              </div>
              <div className="text-xs text-slate-500">quads</div>
            </div>

            {/* Event Log */}
            <div className="bg-slate-900/50 rounded-lg p-4 space-y-2">
              <div className="flex items-center gap-2">
                <Layers className="w-4 h-4 text-purple-400" />
                <span className="text-sm font-medium text-slate-300">Event Log</span>
              </div>
              <div className="text-2xl font-bold text-white">
                {stats?.event_log?.event_count?.toLocaleString() || '—'}
              </div>
              <div className="text-xs text-slate-500">events</div>
            </div>

            {/* Git Backbone */}
            <div className="bg-slate-900/50 rounded-lg p-4 space-y-2">
              <div className="flex items-center gap-2">
                <GitBranch className="w-4 h-4 text-green-400" />
                <span className="text-sm font-medium text-slate-300">Git</span>
              </div>
              <div className="text-2xl font-bold text-white">
                {stats?.universe?.entity_count?.toLocaleString() || '—'}
              </div>
              <div className="text-xs text-slate-500">entities</div>
            </div>
          </div>

          {/* Type Breakdown */}
          {stats?.universe?.types && (
            <div className="mt-4 flex flex-wrap gap-2">
              {Object.entries(stats.universe.types).map(([type, count]) => (
                <span
                  key={type}
                  className="px-2 py-1 bg-slate-900 rounded text-xs text-slate-400"
                >
                  {type}: {count}
                </span>
              ))}
            </div>
          )}
        </motion.div>

        {/* The Tether (Connection) */}
        <div className="flex justify-center py-4">
          <motion.div
            animate={{
              scale: isConnected ? [1, 1.1, 1] : 1,
              opacity: isConnected ? 1 : 0.5,
            }}
            transition={{ repeat: isConnected ? Infinity : 0, duration: 2 }}
            className="flex flex-col items-center gap-2"
          >
            <div className="w-0.5 h-8 bg-gradient-to-b from-universe-500 to-tether-500" />

            {isConnected ? (
              <Wifi className="w-6 h-6 text-tether-500" />
            ) : (
              <WifiOff className="w-6 h-6 text-red-500" />
            )}

            <span
              className={`text-xs font-medium ${
                isConnected
                  ? 'text-tether-500'
                  : isSyncing
                    ? 'text-yellow-500'
                    : 'text-red-500'
              }`}
            >
              {connectionState.toUpperCase()}
            </span>

            <div className="w-0.5 h-8 bg-gradient-to-b from-tether-500 to-shard-500" />
          </motion.div>
        </div>

        {/* Browser (Shard) */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-slate-800/50 border border-shard-500/30 rounded-xl p-6 glow-shard"
        >
          <div className="flex items-center gap-2 mb-4">
            <Monitor className="w-5 h-5 text-shard-500" />
            <h3 className="font-semibold text-shard-400">Browser — The Shard</h3>
          </div>

          <div className="grid grid-cols-2 gap-4">
            {/* Transient Store */}
            <div className="bg-slate-900/50 rounded-lg p-4 space-y-2">
              <span className="text-sm font-medium text-slate-300">Transient Store</span>
              <div className="text-lg font-mono text-slate-400">In-Memory</div>
              <div className="text-xs text-slate-500">Filtered view of Universe</div>
            </div>

            {/* Vector Clock */}
            <div className="bg-slate-900/50 rounded-lg p-4 space-y-2">
              <span className="text-sm font-medium text-slate-300">Vector Clock</span>
              <div className="text-sm font-mono text-slate-400 truncate">{clockFormatted}</div>
              <div className="text-xs text-slate-500">Causality tracking</div>
            </div>
          </div>
        </motion.div>
      </div>

      {/* Legend */}
      <div className="flex flex-wrap justify-center gap-4 text-xs text-slate-500">
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 rounded-full bg-universe-500" />
          <span>Universe (Server)</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 rounded-full bg-tether-500" />
          <span>Tether (Connection)</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 rounded-full bg-shard-500" />
          <span>Shard (Browser)</span>
        </div>
      </div>
    </div>
  );
}
