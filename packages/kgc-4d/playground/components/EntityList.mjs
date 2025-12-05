'use client';

/**
 * Entity List - Display and select entities by type
 */

import { motion } from 'framer-motion';
import { useEntities } from '../lib/client/hooks.mjs';
import { Box, CheckSquare, User, Folder, ChevronRight } from 'lucide-react';

const typeIcons = {
  Project: Folder,
  Task: CheckSquare,
  User: User,
};

export function EntityList({ typeUri, selectedUri, onSelect }) {
  const { entities, loading, count } = useEntities(typeUri);

  const typeName = typeUri.split('/').pop();
  const Icon = typeIcons[typeName] || Box;

  return (
    <div className="bg-slate-900 border border-slate-800 rounded-xl overflow-hidden">
      <div className="flex items-center gap-2 p-4 border-b border-slate-800">
        <Icon className="w-5 h-5 text-universe-500" />
        <h3 className="font-semibold text-white">{typeName}s</h3>
        <span className="text-xs text-slate-500">({count})</span>
      </div>

      <div className="max-h-64 overflow-y-auto">
        {loading && entities.length === 0 ? (
          <div className="p-4 text-center text-slate-500">Loading...</div>
        ) : entities.length === 0 ? (
          <div className="p-4 text-center text-slate-500">No {typeName.toLowerCase()}s found</div>
        ) : (
          entities.map((entity, index) => (
            <motion.button
              key={entity.uri}
              initial={{ opacity: 0, x: -10 }}
              animate={{ opacity: 1, x: 0 }}
              transition={{ delay: index * 0.05 }}
              onClick={() => onSelect(entity.uri)}
              className={`w-full flex items-center justify-between p-3 hover:bg-slate-800/50 transition-colors ${
                selectedUri === entity.uri ? 'bg-slate-800 border-l-2 border-universe-500' : ''
              }`}
            >
              <div className="flex flex-col items-start">
                <span className="text-sm text-white">{entity.name || entity.title || entity.uri.split('/').pop()}</span>
                {entity.status && (
                  <span
                    className={`text-xs ${
                      entity.status === 'active'
                        ? 'text-tether-500'
                        : entity.status === 'completed'
                          ? 'text-green-500'
                          : 'text-slate-500'
                    }`}
                  >
                    {entity.status}
                  </span>
                )}
              </div>
              <ChevronRight className="w-4 h-4 text-slate-500" />
            </motion.button>
          ))
        )}
      </div>
    </div>
  );
}
