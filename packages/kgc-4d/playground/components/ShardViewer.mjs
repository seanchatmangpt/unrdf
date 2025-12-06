'use client';

/**
 * Shard Viewer - Display current Shard quads
 *
 * Shows:
 * - Raw quad data from the current Shard
 * - Filtering and search
 * - Export capability
 */

import { useState, useMemo } from 'react';
import { motion } from 'framer-motion';
import { useShard } from '../lib/client/hooks.mjs';
import { Search, Download, RefreshCw, Database } from 'lucide-react';

function QuadRow({ quad, index }) {
  const formatValue = (term) => {
    if (term.termType === 'Literal') {
      return `"${term.value}"${term.language ? `@${term.language}` : ''}${
        term.datatype ? `^^${term.datatype.split('#').pop()}` : ''
      }`;
    }
    return term.value.split('/').pop() || term.value;
  };

  // Alt+Click to navigate to forensic view
  const handleCellClick = (e) => {
    if (e.altKey) {
      // Navigate to visualizations page with quad context
      const params = new URLSearchParams({
        subject: quad.subject.value,
        predicate: quad.predicate.value,
      });
      window.location.href = `/visualizations?${params.toString()}`;
    }
  };

  return (
    <motion.tr
      initial={{ opacity: 0 }}
      animate={{ opacity: 1 }}
      transition={{ delay: index * 0.02 }}
      onClick={handleCellClick}
      className="border-b border-slate-800 hover:bg-slate-800/50 cursor-pointer hover:ring-2 hover:ring-indigo-500/30 transition-all"
      title="Alt+Click to view related events in Forensic Mode"
    >
      <td className="px-3 py-2 text-sm font-mono text-universe-400 truncate max-w-[200px]">
        {formatValue(quad.subject)}
      </td>
      <td className="px-3 py-2 text-sm font-mono text-slate-400 truncate max-w-[150px]">
        {formatValue(quad.predicate)}
      </td>
      <td className="px-3 py-2 text-sm font-mono text-shard-400 truncate max-w-[200px]">
        {formatValue(quad.object)}
      </td>
    </motion.tr>
  );
}

export function ShardViewer() {
  const { quads, loading, refresh, metadata } = useShard();
  const [search, setSearch] = useState('');

  const filteredQuads = useMemo(() => {
    if (!search) return quads;

    const lower = search.toLowerCase();
    return quads.filter(
      (q) =>
        q.subject.value.toLowerCase().includes(lower) ||
        q.predicate.value.toLowerCase().includes(lower) ||
        q.object.value.toLowerCase().includes(lower)
    );
  }, [quads, search]);

  const exportNQuads = () => {
    const nquads = quads
      .map((q) => {
        const s =
          q.subject.termType === 'NamedNode' ? `<${q.subject.value}>` : `_:${q.subject.value}`;
        const p = `<${q.predicate.value}>`;
        let o;
        if (q.object.termType === 'NamedNode') {
          o = `<${q.object.value}>`;
        } else if (q.object.termType === 'BlankNode') {
          o = `_:${q.object.value}`;
        } else {
          o = `"${q.object.value}"`;
          if (q.object.datatype) {
            o += `^^<${q.object.datatype}>`;
          } else if (q.object.language) {
            o += `@${q.object.language}`;
          }
        }
        return `${s} ${p} ${o} .`;
      })
      .join('\n');

    const blob = new Blob([nquads], { type: 'application/n-quads' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'shard.nq';
    a.click();
    URL.revokeObjectURL(url);
  };

  return (
    <div className="bg-slate-900 border border-slate-800 rounded-xl overflow-hidden">
      {/* Header */}
      <div className="flex items-center justify-between p-4 border-b border-slate-800">
        <div className="flex items-center gap-2">
          <Database className="w-5 h-5 text-shard-500" />
          <h2 className="text-lg font-semibold text-white">Shard Data</h2>
          {metadata && (
            <span className="text-xs text-slate-500">
              {metadata.total_quads} quads | ID: {metadata.id?.slice(0, 8)}...
            </span>
          )}
        </div>

        <div className="flex items-center gap-2">
          {/* Search */}
          <div className="relative">
            <Search className="absolute left-2 top-1/2 -translate-y-1/2 w-4 h-4 text-slate-500" />
            <input
              type="text"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Filter..."
              className="pl-8 pr-3 py-1.5 bg-slate-800 border border-slate-700 rounded-lg text-sm text-white placeholder-slate-500 focus:outline-none focus:border-universe-500 w-48"
            />
          </div>

          {/* Actions */}
          <button
            onClick={refresh}
            disabled={loading}
            className="p-2 rounded-lg bg-slate-800 hover:bg-slate-700 transition-colors"
            title="Refresh Shard"
          >
            <RefreshCw className={`w-4 h-4 text-slate-400 ${loading ? 'animate-spin' : ''}`} />
          </button>

          <button
            onClick={exportNQuads}
            className="p-2 rounded-lg bg-slate-800 hover:bg-slate-700 transition-colors"
            title="Export as N-Quads"
          >
            <Download className="w-4 h-4 text-slate-400" />
          </button>
        </div>
      </div>

      {/* Table */}
      <div className="overflow-x-auto max-h-96">
        <table className="w-full">
          <thead className="bg-slate-800/50 sticky top-0">
            <tr>
              <th className="px-3 py-2 text-left text-xs font-medium text-slate-400 uppercase">
                Subject
              </th>
              <th className="px-3 py-2 text-left text-xs font-medium text-slate-400 uppercase">
                Predicate
              </th>
              <th className="px-3 py-2 text-left text-xs font-medium text-slate-400 uppercase">
                Object
              </th>
            </tr>
          </thead>
          <tbody>
            {filteredQuads.length === 0 ? (
              <tr>
                <td colSpan={3} className="px-3 py-8 text-center text-slate-500">
                  {loading ? 'Loading...' : 'No quads in Shard'}
                </td>
              </tr>
            ) : (
              filteredQuads.map((quad, index) => (
                <QuadRow key={`${quad.subject.value}-${quad.predicate.value}-${index}`} quad={quad} index={index} />
              ))
            )}
          </tbody>
        </table>
      </div>

      {/* Footer */}
      <div className="px-4 py-2 border-t border-slate-800 flex justify-between text-xs text-slate-500">
        <span>
          Showing {filteredQuads.length} of {quads.length} quads
        </span>
        {metadata?.timestamp && <span>Last updated: {metadata.timestamp}</span>}
      </div>
    </div>
  );
}
