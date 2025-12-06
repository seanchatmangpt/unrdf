'use client';

/**
 * Entity Editor - Edit entities with optimistic updates and validation
 *
 * Demonstrates:
 * - Reading entity properties from Shard
 * - Submitting deltas with optimistic updates
 * - ACK/REJECT handling with visual feedback
 */

import { useState, useCallback } from 'react';
import { motion, AnimatePresence } from 'framer-motion';
import { useEntity, useDelta } from '../lib/client/hooks.mjs';
import { Save, AlertCircle, CheckCircle, Loader2, Edit3 } from 'lucide-react';
import { AutonomicCoach } from './visualizations/AutonomicCoach.jsx';

function PropertyEditor({ name, property, onSave, saving }) {
  const [editing, setEditing] = useState(false);
  const [value, setValue] = useState(property.value);
  const [error, setError] = useState(null);

  const handleSave = async () => {
    setError(null);
    const result = await onSave(property.predicate, value);

    if (!result.success) {
      setError(result.error);
      setValue(property.value); // Rollback
    } else {
      setEditing(false);
    }
  };

  const handleCancel = () => {
    setValue(property.value);
    setEditing(false);
    setError(null);
  };

  return (
    <div className="flex items-center gap-3 p-3 bg-slate-800/50 rounded-lg group">
      <div className="flex-1">
        <div className="text-xs text-slate-500 mb-1">{name}</div>

        {editing ? (
          <input
            type="text"
            value={value}
            onChange={(e) => setValue(e.target.value)}
            className="w-full bg-slate-900 border border-slate-700 rounded px-2 py-1 text-white text-sm focus:outline-none focus:border-universe-500"
            autoFocus
          />
        ) : (
          <div className="text-white text-sm font-mono">{property.value}</div>
        )}

        {error && (
          <motion.div
            initial={{ opacity: 0, y: -10 }}
            animate={{ opacity: 1, y: 0 }}
            className="flex items-center gap-1 text-red-400 text-xs mt-1"
          >
            <AlertCircle className="w-3 h-3" />
            {error}
          </motion.div>
        )}
      </div>

      <div className="flex items-center gap-2">
        {editing ? (
          <>
            <button
              onClick={handleSave}
              disabled={saving}
              className="p-2 rounded bg-tether-500 hover:bg-tether-600 disabled:opacity-50"
            >
              {saving ? (
                <Loader2 className="w-4 h-4 text-white animate-spin" />
              ) : (
                <Save className="w-4 h-4 text-white" />
              )}
            </button>
            <button
              onClick={handleCancel}
              className="p-2 rounded bg-slate-700 hover:bg-slate-600"
            >
              <span className="text-xs text-slate-300">Cancel</span>
            </button>
          </>
        ) : (
          <button
            onClick={() => setEditing(true)}
            className="p-2 rounded bg-slate-700 hover:bg-slate-600 opacity-0 group-hover:opacity-100 transition-opacity"
          >
            <Edit3 className="w-4 h-4 text-slate-400" />
          </button>
        )}
      </div>
    </div>
  );
}

export function EntityEditor({ entityUri }) {
  const { properties, loading, update } = useEntity(entityUri);
  const { hasPending, lastResult } = useDelta();
  const [saving, setSaving] = useState(false);
  const [coachingRejection, setCoachingRejection] = useState(null);

  const handleSave = useCallback(
    async (predicate, newValue) => {
      setSaving(true);
      const result = await update(predicate, newValue);
      setSaving(false);

      // Phase 3: Show AutonomicCoach on rejection
      if (!result.success && result.coaching) {
        setCoachingRejection({
          reason: result.error,
          coaching: result.coaching,
          predicate,
          attemptedValue: newValue,
        });
      }

      return result;
    },
    [update]
  );

  const handleCoachRetry = useCallback(
    async (correctedDelta) => {
      setCoachingRejection(null);
      // Auto-retry with corrected value
      if (correctedDelta && correctedDelta.suggestedValue) {
        const predicate = coachingRejection.predicate;
        setSaving(true);
        const result = await update(predicate, correctedDelta.suggestedValue);
        setSaving(false);

        // If still rejected, show coach again
        if (!result.success && result.coaching) {
          setCoachingRejection({
            reason: result.error,
            coaching: result.coaching,
            predicate,
            attemptedValue: correctedDelta.suggestedValue,
          });
        }
      }
    },
    [update, coachingRejection]
  );

  const handleCoachDismiss = useCallback(() => {
    setCoachingRejection(null);
  }, []);

  if (loading && Object.keys(properties).length === 0) {
    return (
      <div className="bg-slate-900 border border-slate-800 rounded-xl p-6">
        <div className="flex items-center justify-center py-8">
          <Loader2 className="w-6 h-6 text-universe-500 animate-spin" />
        </div>
      </div>
    );
  }

  const entityName = entityUri.split('/').pop();

  return (
    <div className="bg-slate-900 border border-slate-800 rounded-xl p-6 space-y-4">
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-lg font-semibold text-white">{entityName}</h2>
          <div className="text-xs text-slate-500 font-mono truncate max-w-xs">{entityUri}</div>
        </div>

        {/* Status indicator */}
        <AnimatePresence>
          {hasPending && (
            <motion.div
              initial={{ opacity: 0, scale: 0.8 }}
              animate={{ opacity: 1, scale: 1 }}
              exit={{ opacity: 0, scale: 0.8 }}
              className="flex items-center gap-2 px-3 py-1 bg-yellow-500/20 rounded-full"
            >
              <Loader2 className="w-3 h-3 text-yellow-500 animate-spin" />
              <span className="text-xs text-yellow-500">Syncing...</span>
            </motion.div>
          )}

          {lastResult?.success && !hasPending && (
            <motion.div
              initial={{ opacity: 0, scale: 0.8 }}
              animate={{ opacity: 1, scale: 1 }}
              exit={{ opacity: 0, scale: 0.8 }}
              className="flex items-center gap-2 px-3 py-1 bg-tether-500/20 rounded-full"
            >
              <CheckCircle className="w-3 h-3 text-tether-500" />
              <span className="text-xs text-tether-500">Saved</span>
            </motion.div>
          )}
        </AnimatePresence>
      </div>

      {/* Properties */}
      <div className="space-y-2">
        {Object.entries(properties).length === 0 ? (
          <div className="text-center py-8 text-slate-500">No properties found</div>
        ) : (
          Object.entries(properties)
            .filter(([name]) => name !== 'type') // Hide rdf:type
            .map(([name, prop]) => (
              <PropertyEditor
                key={name}
                name={name}
                property={prop}
                onSave={handleSave}
                saving={saving}
              />
            ))
        )}
      </div>

      {/* Validation Rules Info */}
      <div className="pt-4 border-t border-slate-800">
        <div className="text-xs text-slate-500 space-y-1">
          <div>
            <strong>Validation Rules (mu hooks):</strong>
          </div>
          <ul className="list-disc list-inside text-slate-600">
            <li>Budget: 0 - 100,000</li>
            <li>Status: active, paused, completed, cancelled</li>
            <li>Name/Title: non-empty, max 100 chars</li>
          </ul>
        </div>
      </div>

      {/* Phase 3: AutonomicCoach Modal */}
      <AutonomicCoach
        rejection={coachingRejection}
        onRetry={handleCoachRetry}
        onDismiss={handleCoachDismiss}
      />
    </div>
  );
}
