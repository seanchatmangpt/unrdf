/**
 * @file Knowledge Hooks Editor Component
 * @description Interactive editor for creating, managing, and executing Knowledge Hooks
 * @example
 * import { KnowledgeHooksEditor } from 'unrdf/examples/react-hooks/knowledge-hooks-editor.jsx';
 * export default function App() {
 *   return <KnowledgeHooksEditor />;
 * }
 */

import React, { useState, useCallback, useMemo } from 'react';
import {
  KnowledgeEngineProvider,
  useKnowledgeEngineContext,
  useHookManager,
  useKnowledgeHook,
  useHookRegistry,
  usePerformanceTracking,
  useSPARQLQuery,
} from 'unrdf/react-hooks';

/**
 * Knowledge Hooks Editor App
 * Provides context and main layout
 */
export function KnowledgeHooksEditorApp() {
  return (
    <KnowledgeEngineProvider
      config={{
        enableKnowledgeHooks: true,
        enableObservability: true,
        hookConfig: {
          autoEvaluate: true,
          batchSize: 10,
        },
      }}
    >
      <KnowledgeHooksEditor />
    </KnowledgeEngineProvider>
  );
}

/**
 * Main Knowledge Hooks Editor Component
 */
function KnowledgeHooksEditor() {
  const { store } = useKnowledgeEngineContext();
  const { hooks, addHook, removeHook, evaluateHook } = useHookManager();
  const [selectedHook, setSelectedHook] = useState(null);
  const [isCreating, setIsCreating] = useState(false);

  const handleCreateHook = useCallback(
    hookDef => {
      addHook(hookDef);
      setIsCreating(false);
    },
    [addHook]
  );

  if (!store) {
    return <div>Loading...</div>;
  }

  return (
    <div style={styles.container}>
      <header style={styles.header}>
        <h1>Knowledge Hooks Editor</h1>
        <p>Create and manage autonomous knowledge triggers</p>
      </header>

      <div style={styles.mainContent}>
        <div style={styles.sidebar}>
          <HooksList
            hooks={hooks}
            selectedHook={selectedHook}
            onSelectHook={setSelectedHook}
            onRemoveHook={removeHook}
          />
          <button
            onClick={() => setIsCreating(true)}
            style={{ ...styles.button, marginTop: '1rem' }}
          >
            + New Hook
          </button>
        </div>

        <div style={styles.mainPanel}>
          {isCreating ? (
            <HookCreator onSave={handleCreateHook} onCancel={() => setIsCreating(false)} />
          ) : selectedHook ? (
            <HookEditor hook={selectedHook} onEvaluate={() => evaluateHook(selectedHook.name)} />
          ) : (
            <div style={styles.emptyState}>
              <p>Select a hook to view details or create a new one</p>
            </div>
          )}
        </div>

        <div style={styles.sidebarRight}>
          <HooksMetrics hooks={hooks} />
          <HookExecutionLog />
        </div>
      </div>
    </div>
  );
}

/**
 * Hooks List Component
 * Displays all registered hooks
 */
function HooksList({ hooks, selectedHook, onSelectHook, onRemoveHook }) {
  return (
    <div style={styles.panel}>
      <h3>Registered Hooks ({hooks.length})</h3>
      <ul style={styles.list}>
        {hooks.map(hook => (
          <li
            key={hook.name}
            style={{
              ...styles.hookListItem,
              backgroundColor: selectedHook?.name === hook.name ? '#e0f2fe' : 'transparent',
            }}
            onClick={() => onSelectHook(hook)}
          >
            <div style={styles.hookListItemName}>{hook.name}</div>
            <div style={styles.hookListItemDesc}>{hook.meta?.description}</div>
            <div style={styles.hookListItemStatus}>{hook.enabled ? '✓ Enabled' : '✗ Disabled'}</div>
            <button
              onClick={e => {
                e.stopPropagation();
                onRemoveHook(hook.name);
              }}
              style={styles.deleteButton}
            >
              Remove
            </button>
          </li>
        ))}
      </ul>
    </div>
  );
}

/**
 * Hook Creator Component
 * Form to create new knowledge hooks
 */
function HookCreator({ onSave, onCancel }) {
  const [hookDef, setHookDef] = useState({
    name: '',
    meta: {
      description: '',
      ontology: '',
    },
    when: {
      kind: 'sparql-ask',
      query: '',
    },
    run: {
      code: '',
    },
  });

  const handleSave = useCallback(() => {
    if (hookDef.name && hookDef.when.query && hookDef.run.code) {
      onSave(hookDef);
    }
  }, [hookDef, onSave]);

  return (
    <div style={styles.panel}>
      <h3>Create New Hook</h3>

      <div style={styles.formGroup}>
        <label>Hook Name *</label>
        <input
          type="text"
          placeholder="e.g., auto-tag-documents"
          value={hookDef.name}
          onChange={e =>
            setHookDef(prev => ({
              ...prev,
              name: e.target.value,
            }))
          }
          style={styles.input}
        />
      </div>

      <div style={styles.formGroup}>
        <label>Description</label>
        <textarea
          placeholder="What does this hook do?"
          value={hookDef.meta.description}
          onChange={e =>
            setHookDef(prev => ({
              ...prev,
              meta: { ...prev.meta, description: e.target.value },
            }))
          }
          style={{ ...styles.input, minHeight: '60px' }}
        />
      </div>

      <div style={styles.formGroup}>
        <label>Trigger Condition (SPARQL) *</label>
        <textarea
          placeholder="ASK { ?s ?p ?o }"
          value={hookDef.when.query}
          onChange={e =>
            setHookDef(prev => ({
              ...prev,
              when: { ...prev.when, query: e.target.value },
            }))
          }
          style={{
            ...styles.input,
            minHeight: '100px',
            fontFamily: 'monospace',
          }}
        />
      </div>

      <div style={styles.formGroup}>
        <label>Effect Code (JavaScript) *</label>
        <textarea
          placeholder="async (event) => { /* your code */ }"
          value={hookDef.run.code}
          onChange={e =>
            setHookDef(prev => ({
              ...prev,
              run: { ...prev.run, code: e.target.value },
            }))
          }
          style={{
            ...styles.input,
            minHeight: '150px',
            fontFamily: 'monospace',
          }}
        />
      </div>

      <div style={styles.buttonGroup}>
        <button onClick={handleSave} style={{ ...styles.button, backgroundColor: '#22c55e' }}>
          Create Hook
        </button>
        <button onClick={onCancel} style={{ ...styles.button, backgroundColor: '#6b7280' }}>
          Cancel
        </button>
      </div>
    </div>
  );
}

/**
 * Hook Editor Component
 * View and edit existing hooks
 */
function HookEditor({ hook, onEvaluate }) {
  const [isEditing, setIsEditing] = useState(false);

  return (
    <div style={styles.panel}>
      <div style={styles.hookHeader}>
        <h2>{hook.name}</h2>
        <button onClick={onEvaluate} style={{ ...styles.button, backgroundColor: '#8b5cf6' }}>
          Evaluate Now
        </button>
      </div>

      {hook.meta?.description && <p style={styles.description}>{hook.meta.description}</p>}

      <div style={styles.hookSection}>
        <h4>Trigger</h4>
        <div style={styles.codeBlock}>{hook.when.query}</div>
      </div>

      <div style={styles.hookSection}>
        <h4>Effect</h4>
        <div style={styles.codeBlock}>{hook.run.code}</div>
      </div>

      <div style={styles.hookSection}>
        <h4>Status</h4>
        <dl>
          <dt>Enabled:</dt>
          <dd>{hook.enabled ? '✓ Yes' : '✗ No'}</dd>
          <dt>Last Executed:</dt>
          <dd>{hook.lastExecuted ? new Date(hook.lastExecuted).toLocaleString() : 'Never'}</dd>
          <dt>Execution Count:</dt>
          <dd>{hook.executionCount || 0}</dd>
        </dl>
      </div>
    </div>
  );
}

/**
 * Hooks Metrics Component
 * Display overall metrics
 */
function HooksMetrics({ hooks }) {
  const totalHooks = hooks.length;
  const enabledHooks = hooks.filter(h => h.enabled).length;
  const totalExecutions = hooks.reduce((sum, h) => sum + (h.executionCount || 0), 0);

  return (
    <div style={styles.panel}>
      <h3>Metrics</h3>
      <dl style={styles.metricsList}>
        <dt>Total Hooks:</dt>
        <dd>{totalHooks}</dd>
        <dt>Enabled:</dt>
        <dd>{enabledHooks}</dd>
        <dt>Total Executions:</dt>
        <dd>{totalExecutions.toLocaleString()}</dd>
      </dl>
    </div>
  );
}

/**
 * Hook Execution Log Component
 * Shows recent hook executions
 */
function HookExecutionLog() {
  const [logs, setLogs] = useState([
    { timestamp: Date.now() - 60000, hook: 'auto-tag', status: 'success' },
    {
      timestamp: Date.now() - 30000,
      hook: 'validate-schema',
      status: 'success',
    },
  ]);

  return (
    <div style={styles.panel}>
      <h3>Execution Log</h3>
      <div style={styles.logContainer}>
        {logs.slice(0, 5).map((log, idx) => (
          <div key={idx} style={styles.logEntry}>
            <small>{new Date(log.timestamp).toLocaleTimeString()}</small>
            <span style={styles.logHook}>{log.hook}</span>
            <span
              style={{
                ...styles.logStatus,
                backgroundColor: log.status === 'success' ? '#dcfce7' : '#fee2e2',
                color: log.status === 'success' ? '#166534' : '#991b1b',
              }}
            >
              {log.status}
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}

/**
 * Component Styles
 */
const styles = {
  container: {
    fontFamily: 'system-ui, -apple-system, sans-serif',
    backgroundColor: '#f5f5f5',
    minHeight: '100vh',
    display: 'flex',
    flexDirection: 'column',
  },
  header: {
    backgroundColor: '#7c3aed',
    color: 'white',
    padding: '2rem',
    textAlign: 'center',
  },
  mainContent: {
    display: 'flex',
    flex: 1,
    gap: '1rem',
    padding: '1rem',
  },
  sidebar: {
    width: '280px',
    display: 'flex',
    flexDirection: 'column',
    gap: '1rem',
  },
  sidebarRight: {
    width: '250px',
    display: 'flex',
    flexDirection: 'column',
    gap: '1rem',
  },
  mainPanel: {
    flex: 1,
    display: 'flex',
    flexDirection: 'column',
  },
  panel: {
    backgroundColor: 'white',
    borderRadius: '8px',
    padding: '1rem',
    boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
  },
  list: {
    listStyle: 'none',
    padding: 0,
    margin: 0,
  },
  hookListItem: {
    padding: '0.75rem',
    borderRadius: '4px',
    cursor: 'pointer',
    marginBottom: '0.5rem',
    transition: 'background-color 0.2s',
    border: '1px solid #e5e7eb',
  },
  hookListItemName: {
    fontWeight: 'bold',
    marginBottom: '0.25rem',
  },
  hookListItemDesc: {
    fontSize: '0.875rem',
    color: '#666',
    marginBottom: '0.25rem',
  },
  hookListItemStatus: {
    fontSize: '0.75rem',
    color: '#999',
  },
  deleteButton: {
    marginTop: '0.5rem',
    padding: '0.25rem 0.5rem',
    fontSize: '0.75rem',
    backgroundColor: '#ef4444',
    color: 'white',
    border: 'none',
    borderRadius: '3px',
    cursor: 'pointer',
  },
  formGroup: {
    marginBottom: '1rem',
  },
  input: {
    width: '100%',
    padding: '0.5rem',
    marginTop: '0.25rem',
    border: '1px solid #e0e0e0',
    borderRadius: '4px',
    fontFamily: 'inherit',
  },
  button: {
    backgroundColor: '#3b82f6',
    color: 'white',
    padding: '0.5rem 1rem',
    border: 'none',
    borderRadius: '4px',
    cursor: 'pointer',
  },
  buttonGroup: {
    display: 'flex',
    gap: '0.5rem',
    marginTop: '1rem',
  },
  hookHeader: {
    display: 'flex',
    justifyContent: 'space-between',
    alignItems: 'center',
    marginBottom: '1rem',
  },
  description: {
    marginBottom: '1rem',
    color: '#666',
  },
  hookSection: {
    marginBottom: '1rem',
  },
  codeBlock: {
    backgroundColor: '#f3f4f6',
    border: '1px solid #e5e7eb',
    borderRadius: '4px',
    padding: '0.75rem',
    fontFamily: 'monospace',
    fontSize: '0.875rem',
    overflow: 'auto',
    maxHeight: '150px',
  },
  metricsList: {
    display: 'grid',
    gridTemplateColumns: '1fr 1fr',
    gap: '0.5rem',
    fontSize: '0.875rem',
  },
  logContainer: {
    display: 'flex',
    flexDirection: 'column',
    gap: '0.5rem',
    maxHeight: '300px',
    overflow: 'auto',
  },
  logEntry: {
    display: 'flex',
    gap: '0.5rem',
    fontSize: '0.75rem',
    padding: '0.5rem',
    backgroundColor: '#f9fafb',
    borderRadius: '3px',
    alignItems: 'center',
  },
  logHook: {
    fontWeight: 'bold',
    flex: 1,
  },
  logStatus: {
    padding: '0.25rem 0.5rem',
    borderRadius: '3px',
    fontSize: '0.7rem',
    fontWeight: 'bold',
  },
  emptyState: {
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    flex: 1,
    color: '#999',
    fontSize: '1.1rem',
  },
};

export default KnowledgeHooksEditorApp;
