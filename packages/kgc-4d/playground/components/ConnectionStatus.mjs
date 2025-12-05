'use client';

/**
 * Connection Status - Visual indicator of Tether state
 */

import { motion } from 'framer-motion';
import { useConnection } from '../lib/client/hooks.mjs';
import { Wifi, WifiOff, Loader2, AlertTriangle } from 'lucide-react';

const statusConfig = {
  disconnected: {
    icon: WifiOff,
    color: 'text-slate-400',
    bg: 'bg-slate-500/20',
    label: 'Disconnected',
  },
  connecting: {
    icon: Loader2,
    color: 'text-yellow-500',
    bg: 'bg-yellow-500/20',
    label: 'Connecting...',
    animate: true,
  },
  connected: {
    icon: Wifi,
    color: 'text-tether-500',
    bg: 'bg-tether-500/20',
    label: 'Connected',
  },
  syncing: {
    icon: Loader2,
    color: 'text-universe-500',
    bg: 'bg-universe-500/20',
    label: 'Syncing...',
    animate: true,
  },
  error: {
    icon: AlertTriangle,
    color: 'text-red-500',
    bg: 'bg-red-500/20',
    label: 'Error',
  },
};

export function ConnectionStatus() {
  const { state, error, connect, disconnect, isConnected } = useConnection();
  const config = statusConfig[state] || statusConfig.disconnected;
  const Icon = config.icon;

  return (
    <div className="flex items-center gap-3">
      <motion.div
        animate={
          config.animate
            ? { rotate: 360 }
            : isConnected
              ? { scale: [1, 1.1, 1] }
              : {}
        }
        transition={
          config.animate
            ? { repeat: Infinity, duration: 1, ease: 'linear' }
            : { repeat: Infinity, duration: 2 }
        }
        className={`p-2 rounded-full ${config.bg}`}
      >
        <Icon className={`w-4 h-4 ${config.color}`} />
      </motion.div>

      <div className="flex flex-col">
        <span className={`text-sm font-medium ${config.color}`}>{config.label}</span>
        {error && <span className="text-xs text-red-400">{error}</span>}
      </div>

      {/* Connect/Disconnect button */}
      <button
        onClick={isConnected ? disconnect : connect}
        className={`px-3 py-1 rounded-lg text-xs font-medium transition-colors ${
          isConnected
            ? 'bg-slate-700 hover:bg-slate-600 text-slate-300'
            : 'bg-tether-500 hover:bg-tether-600 text-white'
        }`}
      >
        {isConnected ? 'Disconnect' : 'Connect'}
      </button>
    </div>
  );
}

/**
 * Compact connection indicator for header
 */
export function ConnectionIndicator() {
  const { state, isConnected } = useConnection();
  const config = statusConfig[state] || statusConfig.disconnected;

  return (
    <motion.div
      animate={isConnected ? { scale: [1, 1.2, 1] } : {}}
      transition={{ repeat: Infinity, duration: 2 }}
      className={`w-2 h-2 rounded-full ${
        isConnected ? 'bg-tether-500' : state === 'error' ? 'bg-red-500' : 'bg-slate-500'
      }`}
      title={config.label}
    />
  );
}
