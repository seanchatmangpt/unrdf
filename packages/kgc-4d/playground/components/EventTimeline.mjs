'use client';

/**
 * Event Timeline - Real-time event log visualization
 *
 * Shows:
 * - Connection events (CONNECTED, HEARTBEAT)
 * - Shard updates
 * - Delta ACK/REJECT responses
 */

import { motion, AnimatePresence } from 'framer-motion';
import { useEventLog } from '../lib/client/hooks.mjs';
import {
  CheckCircle,
  XCircle,
  RefreshCw,
  Wifi,
  Heart,
  Layers,
  ArrowRight,
} from 'lucide-react';

const eventConfig = {
  CONNECTED: {
    icon: Wifi,
    color: 'text-tether-500',
    bg: 'bg-tether-500/10',
    border: 'border-tether-500/30',
  },
  HEARTBEAT: {
    icon: Heart,
    color: 'text-slate-400',
    bg: 'bg-slate-500/10',
    border: 'border-slate-500/30',
  },
  SHARD: {
    icon: Layers,
    color: 'text-shard-500',
    bg: 'bg-shard-500/10',
    border: 'border-shard-500/30',
  },
  DELTA: {
    icon: ArrowRight,
    color: 'text-yellow-500',
    bg: 'bg-yellow-500/10',
    border: 'border-yellow-500/30',
  },
  ACK: {
    icon: CheckCircle,
    color: 'text-green-500',
    bg: 'bg-green-500/10',
    border: 'border-green-500/30',
  },
  REJECT: {
    icon: XCircle,
    color: 'text-red-500',
    bg: 'bg-red-500/10',
    border: 'border-red-500/30',
  },
};

function EventItem({ event, index }) {
  const config = eventConfig[event.type] || {
    icon: RefreshCw,
    color: 'text-slate-400',
    bg: 'bg-slate-500/10',
    border: 'border-slate-500/30',
  };

  const Icon = config.icon;

  const formatTime = (timestamp) => {
    if (!timestamp) return '';
    const date = new Date(timestamp);
    return date.toLocaleTimeString('en-US', {
      hour12: false,
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
    });
  };

  return (
    <motion.div
      initial={{ opacity: 0, x: -20 }}
      animate={{ opacity: 1, x: 0 }}
      exit={{ opacity: 0, x: 20 }}
      transition={{ delay: index * 0.05 }}
      className={`flex items-start gap-3 p-3 rounded-lg ${config.bg} border ${config.border}`}
    >
      <div className={`p-1.5 rounded-full ${config.bg}`}>
        <Icon className={`w-4 h-4 ${config.color}`} />
      </div>

      <div className="flex-1 min-w-0">
        <div className="flex items-center justify-between gap-2">
          <span className={`text-sm font-medium ${config.color}`}>{event.type}</span>
          <span className="text-xs text-slate-500">{formatTime(event.timestamp)}</span>
        </div>

        {/* Event-specific details */}
        {event.type === 'SHARD' && (
          <div className="text-xs text-slate-400 mt-1">{event.quad_count} quads received</div>
        )}

        {event.type === 'ACK' && (
          <div className="text-xs text-slate-400 mt-1">
            Event #{event.event_count} committed
          </div>
        )}

        {event.type === 'REJECT' && (
          <div className="text-xs text-red-400 mt-1">{event.reason}</div>
        )}

        {event.type === 'DELTA' && event.delta && (
          <div className="text-xs text-slate-400 mt-1">
            {event.delta.subject?.split('/').pop()} updated
          </div>
        )}
      </div>
    </motion.div>
  );
}

export function EventTimeline() {
  const { events, count } = useEventLog(20);

  return (
    <div className="bg-slate-900 border border-slate-800 rounded-xl p-6 space-y-4">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold text-white">Event Timeline</h2>
        <span className="text-xs text-slate-500">{count} events</span>
      </div>

      <div className="space-y-2 max-h-96 overflow-y-auto">
        <AnimatePresence>
          {events.length === 0 ? (
            <div className="text-center py-8 text-slate-500">
              No events yet. Connect to start receiving updates.
            </div>
          ) : (
            events.map((event, index) => (
              <EventItem key={`${event.type}-${index}`} event={event} index={index} />
            ))
          )}
        </AnimatePresence>
      </div>
    </div>
  );
}
