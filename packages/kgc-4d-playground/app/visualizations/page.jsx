'use client';

/**
 * Visualizations Page - Advanced 4D Time-Travel Visualizations
 *
 * Showcases cutting-edge visualization innovations:
 * - Forensic Mode: Causal cone DAG with nanosecond timeline
 * - Multiverse Sandbox: Reality forking (Phase 2)
 * - Autonomic Coach: Rejection explanations (Phase 3)
 */

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { KGCProvider } from '../../lib/client/kgc-context.mjs';
import { useKGC } from '../../lib/client/kgc-context.mjs';
import { useTimeTravel } from '../../lib/hooks/useTimeTravel.mjs';
import { useCausalCone } from '../../lib/hooks/useCausalCone.mjs';
import { ForensicView } from '../../components/visualizations/ForensicView.jsx';
import { NanosecondTimeline } from '../../components/visualizations/NanosecondTimeline.jsx';
import { MultiverseSandbox } from '../../components/visualizations/MultiverseSandbox.jsx';
import { AutonomicCoach } from '../../components/visualizations/AutonomicCoach.jsx';
import { Eye, History, GitBranch, ArrowLeft, Clock, AlertCircle } from 'lucide-react';

function VisualizationsContent() {
  const router = useRouter();
  const { eventLog, isConnected, connect } = useKGC();
  const { reconstructed, loading: timeTravelLoading, error: timeTravelError, travelTo } = useTimeTravel();
  const [activeTab, setActiveTab] = useState('forensic');
  const [selectedEventId, setSelectedEventId] = useState(null);
  const [selectedTime, setSelectedTime] = useState(null);

  // Build causal cone for selected event
  const causalCone = useCausalCone(selectedEventId, eventLog);

  // Auto-connect on mount
  useEffect(() => {
    if (!isConnected) {
      connect();
    }
  }, [isConnected, connect]);

  // Read query params and auto-select event (Alt+Click navigation)
  useEffect(() => {
    if (typeof window === 'undefined' || !eventLog || eventLog.length === 0) return;

    const params = new URLSearchParams(window.location.search);
    const eventIdParam = params.get('eventId');
    const subjectParam = params.get('subject');
    const predicateParam = params.get('predicate');

    if (eventIdParam) {
      // Direct event ID from EventTimeline
      const event = eventLog.find((e) => e.id === eventIdParam);
      if (event) {
        setSelectedEventId(event.id);
        setSelectedTime(event.t_ns);
      }
    } else if (subjectParam && predicateParam) {
      // Quad context from ShardViewer - find most recent related event
      const relatedEvent = eventLog.find((e) => {
        const payload = e.payload;
        if (!payload) return false;
        return (
          payload.subject === subjectParam ||
          payload.entity === subjectParam ||
          payload.predicate === predicateParam
        );
      });
      if (relatedEvent) {
        setSelectedEventId(relatedEvent.id);
        setSelectedTime(relatedEvent.t_ns);
      }
    }
  }, [eventLog]);

  // Auto-select first event if none selected and no query params
  useEffect(() => {
    if (!selectedEventId && eventLog && eventLog.length > 0) {
      // Only auto-select if no query params
      if (typeof window !== 'undefined') {
        const params = new URLSearchParams(window.location.search);
        if (!params.get('eventId') && !params.get('subject')) {
          const firstEvent = eventLog[0];
          setSelectedEventId(firstEvent.id);
          setSelectedTime(firstEvent.t_ns);
        }
      }
    }
  }, [selectedEventId, eventLog]);

  const tabs = [
    {
      id: 'forensic',
      label: 'Forensic Mode',
      icon: Eye,
      description: 'Causal debugging with visual event chains',
    },
    {
      id: 'multiverse',
      label: 'Multiverse Sandbox',
      icon: GitBranch,
      description: 'Fork reality, test actions, merge back',
      disabled: false,
    },
    {
      id: 'coach',
      label: 'Autonomic Coach',
      icon: AlertCircle,
      description: 'Learn from rejections with counter-factual hints',
      disabled: false,
    },
  ];

  const handleEventClick = (event) => {
    setSelectedEventId(event.id);
    setSelectedTime(event.t_ns);
  };

  const handleTimelineSelect = (tNs, event) => {
    setSelectedTime(tNs);
    if (event) {
      setSelectedEventId(event.id);
    }
  };

  const handleTimeTravel = async () => {
    if (!selectedTime) return;
    await travelTo(selectedTime);
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="bg-gradient-to-r from-slate-900 via-slate-800 to-slate-900 border border-slate-800 rounded-xl p-6">
        <div className="flex items-start justify-between">
          <div className="space-y-2">
            <div className="flex items-center gap-3">
              <button
                onClick={() => router.push('/')}
                className="p-2 rounded-lg bg-slate-800 hover:bg-slate-700 transition-colors"
              >
                <ArrowLeft className="w-4 h-4 text-slate-400" />
              </button>
              <h2 className="text-3xl font-bold bg-gradient-to-r from-indigo-400 via-purple-400 to-pink-400 bg-clip-text text-transparent">
                Advanced Visualizations
              </h2>
            </div>
            <p className="text-slate-400 max-w-2xl ml-14">
              Interactive 4D time-travel visualizations leveraging nanosecond-precision event sourcing,
              causal cone analysis, and reality forking.
            </p>
          </div>
          <div className="flex items-center gap-2">
            <div
              className={`px-3 py-1.5 rounded-full text-xs font-medium ${
                isConnected ? 'bg-green-500/20 text-green-400' : 'bg-red-500/20 text-red-400'
              }`}
            >
              {isConnected ? 'Connected' : 'Disconnected'}
            </div>
          </div>
        </div>
      </div>

      {/* Tab Navigation */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        {tabs.map((tab) => {
          const Icon = tab.icon;
          return (
            <button
              key={tab.id}
              onClick={() => !tab.disabled && setActiveTab(tab.id)}
              disabled={tab.disabled}
              className={`p-4 rounded-lg border-2 transition-all ${
                activeTab === tab.id
                  ? 'bg-indigo-500/20 border-indigo-500 text-white'
                  : tab.disabled
                    ? 'bg-slate-900 border-slate-800 text-slate-600 cursor-not-allowed'
                    : 'bg-slate-900 border-slate-700 text-slate-300 hover:border-slate-600'
              }`}
            >
              <div className="flex items-center gap-3 mb-2">
                <Icon className="w-5 h-5" />
                <span className="font-semibold">{tab.label}</span>
              </div>
              <div className="text-sm text-left opacity-75">{tab.description}</div>
            </button>
          );
        })}
      </div>

      {/* Tab Content */}
      {activeTab === 'forensic' && (
        <div className="space-y-6">
          {/* Nanosecond Timeline */}
          <div className="bg-slate-900 border border-slate-800 rounded-xl p-6">
            <NanosecondTimeline
              events={eventLog || []}
              selectedTime={selectedTime}
              onSelect={handleTimelineSelect}
              highlightRange={
                causalCone && causalCone.earliestTime && causalCone.latestTime
                  ? {
                      start: causalCone.earliestTime,
                      end: causalCone.latestTime,
                    }
                  : null
              }
            />
          </div>

          {/* Forensic View */}
          <div className="bg-slate-900 border border-slate-800 rounded-xl p-6">
            <ForensicView
              causalCone={causalCone}
              onNodeClick={handleEventClick}
              selectedNodeId={selectedEventId}
            />
          </div>

          {/* Time-Travel Controls */}
          <div className="bg-slate-900 border border-slate-800 rounded-xl p-6">
            <div className="flex items-center justify-between mb-4">
              <div className="flex items-center gap-2 text-sm font-medium text-slate-300">
                <Clock className="w-4 h-4" />
                <span>Time-Travel Controls</span>
              </div>
              {selectedTime && (
                <div className="text-xs text-slate-400 font-mono">
                  Target: {new Date(Number(BigInt(selectedTime) / 1000000n)).toISOString()}
                </div>
              )}
            </div>

            <div className="flex items-center gap-4">
              <button
                onClick={handleTimeTravel}
                disabled={!selectedTime || timeTravelLoading}
                className="px-4 py-2 rounded-lg bg-indigo-500 hover:bg-indigo-600 text-white font-medium disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
              >
                {timeTravelLoading ? 'Reconstructing...' : 'Travel to Selected Time'}
              </button>

              {timeTravelError && (
                <div className="text-sm text-red-400">Error: {timeTravelError}</div>
              )}

              {reconstructed && (
                <div className="text-sm text-green-400">
                  ✓ Reconstructed {reconstructed.quad_count} quads at {reconstructed.timestamp}
                </div>
              )}
            </div>
          </div>

          {/* Reconstructed State Display */}
          {reconstructed && (
            <div className="bg-slate-900 border border-slate-800 rounded-xl p-6">
              <div className="text-sm font-medium text-slate-300 mb-4">Reconstructed State</div>
              <div className="bg-slate-950 border border-slate-700 rounded-lg p-4">
                <div className="grid grid-cols-2 gap-4 text-sm">
                  <div>
                    <div className="text-slate-500">Timestamp</div>
                    <div className="text-slate-300 font-mono">{reconstructed.timestamp}</div>
                  </div>
                  <div>
                    <div className="text-slate-500">Quad Count</div>
                    <div className="text-slate-300 font-mono">{reconstructed.quad_count}</div>
                  </div>
                  <div className="col-span-2">
                    <div className="text-slate-500 mb-2">Nanosecond Precision</div>
                    <div className="text-slate-300 font-mono text-xs">{reconstructed.t_ns}</div>
                  </div>
                </div>
              </div>
            </div>
          )}
        </div>
      )}

      {activeTab === 'multiverse' && (
        <div className="space-y-6">
          <MultiverseSandbox />
        </div>
      )}

      {activeTab === 'coach' && (
        <div className="bg-slate-900 border border-slate-800 rounded-xl p-8">
          <div className="text-center mb-6">
            <AlertCircle className="w-12 h-12 text-indigo-400 mx-auto mb-4" />
            <div className="text-slate-300 font-medium mb-2">Autonomic Coach</div>
            <div className="text-sm text-slate-500">
              The Coach appears automatically when deltas are rejected, providing counter-factual hints to help you succeed.
            </div>
          </div>

          <div className="bg-slate-950 border border-slate-700 rounded-lg p-6">
            <h3 className="text-sm font-semibold text-slate-300 mb-3">How It Works</h3>
            <ol className="space-y-3 text-sm text-slate-400">
              <li className="flex items-start gap-3">
                <span className="text-indigo-400 font-medium flex-shrink-0">1.</span>
                <span>Submit a delta that violates validation rules (e.g., budget exceeds $100,000)</span>
              </li>
              <li className="flex items-start gap-3">
                <span className="text-indigo-400 font-medium flex-shrink-0">2.</span>
                <span>Server rejects with detailed reasoning and counter-factual hints</span>
              </li>
              <li className="flex items-start gap-3">
                <span className="text-indigo-400 font-medium flex-shrink-0">3.</span>
                <span>AutonomicCoach modal shows why it failed and what would succeed</span>
              </li>
              <li className="flex items-start gap-3">
                <span className="text-indigo-400 font-medium flex-shrink-0">4.</span>
                <span>Click "Try Again" to retry with pre-filled corrected values</span>
              </li>
            </ol>
          </div>

          <div className="mt-6 bg-indigo-500/10 border border-indigo-500/30 rounded-lg p-4">
            <div className="flex items-start gap-3">
              <AlertCircle className="w-5 h-5 text-indigo-400 flex-shrink-0 mt-0.5" />
              <div className="text-sm text-indigo-300">
                <div className="font-medium mb-1">Try It Out</div>
                <div className="text-indigo-400">
                  Go to the main dashboard, create or edit an entity, and try setting an invalid value (e.g., budget: -5000).
                  The Coach will appear with helpful guidance.
                </div>
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

export default function VisualizationsPage() {
  return (
    <KGCProvider autoConnect={false}>
      <VisualizationsContent />
    </KGCProvider>
  );
}
