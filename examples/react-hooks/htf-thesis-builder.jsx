/**
 * @file HTF Thesis Builder
 * @description Complete React application demonstrating the Hyper-Thesis Framework
 * Shows: Λ-Scheduler, Π-Profiler, Γ-Checker, τ-Dashboard, Q-Monitor
 *
 * @example
 * import HTFThesisBuilderApp from 'unrdf/examples/react-hooks/htf-thesis-builder.jsx';
 * export default HTFThesisBuilderApp;
 */

import React, { useState, useCallback } from 'react';
import {
  useHTFOntology,
  useΛSchedule,
  useΠMerge,
  useΓGlobalization,
  useQInvariants,
  useτEvolution,
} from 'unrdf/react-hooks/htf';

/**
 * Main HTF Thesis Builder App
 */
export function HTFThesisBuilderApp() {
  const [activeTab, setActiveTab] = useState('editor');
  const [thesis, setThesis] = useState({
    title: 'Democratizing Medical Knowledge through RDF-Based Patient Graphs',
    abstract: '',
  });

  return (
    <div style={styles.app}>
      <header style={styles.header}>
        <h1>🎓 Hyper-Thesis Framework (HTF)</h1>
        <p>Mathematical framework for unified thesis composition</p>
      </header>

      <nav style={styles.nav}>
        <button
          onClick={() => setActiveTab('editor')}
          style={{
            ...styles.navButton,
            backgroundColor: activeTab === 'editor' ? '#3b82f6' : '#666',
          }}
        >
          Editor
        </button>
        <button
          onClick={() => setActiveTab('scheduler')}
          style={{
            ...styles.navButton,
            backgroundColor: activeTab === 'scheduler' ? '#3b82f6' : '#666',
          }}
        >
          Λ-Scheduler
        </button>
        <button
          onClick={() => setActiveTab('profiler')}
          style={{
            ...styles.navButton,
            backgroundColor: activeTab === 'profiler' ? '#3b82f6' : '#666',
          }}
        >
          Π-Profiler
        </button>
        <button
          onClick={() => setActiveTab('checker')}
          style={{
            ...styles.navButton,
            backgroundColor: activeTab === 'checker' ? '#3b82f6' : '#666',
          }}
        >
          Γ-Checker
        </button>
        <button
          onClick={() => setActiveTab('dashboard')}
          style={{
            ...styles.navButton,
            backgroundColor: activeTab === 'dashboard' ? '#3b82f6' : '#666',
          }}
        >
          τ-Dashboard
        </button>
      </nav>

      <div style={styles.content}>
        {activeTab === 'editor' && <ΔEditorTab thesis={thesis} setThesis={setThesis} />}
        {activeTab === 'scheduler' && <ΛSchedulerTab />}
        {activeTab === 'profiler' && <ΠProfilerTab />}
        {activeTab === 'checker' && <ΓCheckerTab />}
        {activeTab === 'dashboard' && <τDashboardTab />}
      </div>
    </div>
  );
}

/**
 * Δ-Editor Tab: Edit shards
 */
function ΔEditorTab({ thesis, setThesis }) {
  const { shards, addShard, removeShard, updateShard } = useHTFOntology();
  const [selectedShard, setSelectedShard] = useState(null);
  const [editContent, setEditContent] = useState('');

  const handleEdit = useCallback(
    shardId => {
      updateShard(shardId, { content: editContent });
      setEditContent('');
    },
    [updateShard, editContent]
  );

  return (
    <div style={styles.tab}>
      <h2>Δ-Shard Editor</h2>

      <div style={styles.twoColumn}>
        <div>
          <h3>Shards ({shards.length})</h3>
          <div style={styles.shardList}>
            {shards.length === 0 ? (
              <p>No shards yet. Create one to start.</p>
            ) : (
              shards.map(shard => (
                <div
                  key={shard.id}
                  onClick={() => {
                    setSelectedShard(shard);
                    setEditContent(shard.content || '');
                  }}
                  style={{
                    ...styles.shardItem,
                    backgroundColor: selectedShard?.id === shard.id ? '#e0f2fe' : '#f0f0f0',
                  }}
                >
                  <strong>{shard.type}</strong>
                  <small> {shard.family}</small>
                  <small style={{ float: 'right' }}>{shard.wordCount || 0} words</small>
                </div>
              ))
            )}
          </div>

          <div style={styles.addShardForm}>
            <AddShardForm onAdd={addShard} />
          </div>
        </div>

        <div>
          <h3>Edit Shard</h3>
          {selectedShard ? (
            <div>
              <p>
                <strong>{selectedShard.type}</strong> - {selectedShard.family}
              </p>
              <textarea
                value={editContent}
                onChange={e => setEditContent(e.target.value)}
                style={styles.textarea}
                placeholder="Edit shard content..."
              />
              <button onClick={() => handleEdit(selectedShard.id)} style={styles.button}>
                Save
              </button>
              <button
                onClick={() => {
                  removeShard(selectedShard.id);
                  setSelectedShard(null);
                }}
                style={{ ...styles.button, backgroundColor: '#ef4444' }}
              >
                Delete
              </button>
            </div>
          ) : (
            <p>Select a shard to edit</p>
          )}
        </div>
      </div>
    </div>
  );
}

/**
 * Λ-Scheduler Tab
 */
function ΛSchedulerTab() {
  const { shards } = useHTFOntology();
  const { schedule, flowScore, positions } = useΛSchedule(shards);

  return (
    <div style={styles.tab}>
      <h2>Λ-Scheduler: Optimal Reading Order</h2>

      <div style={styles.scoreBox}>
        <p>
          <strong>Flow Score:</strong> {flowScore.toFixed(1)}/100
        </p>
        <div style={styles.scoreBar}>
          <div
            style={{
              ...styles.scoreBarFill,
              width: `${flowScore}%`,
              backgroundColor: flowScore > 80 ? '#22c55e' : flowScore > 60 ? '#f59e0b' : '#ef4444',
            }}
          ></div>
        </div>
      </div>

      <div style={styles.scheduleList}>
        <h3>Reading Order (Λ-chain)</h3>
        {schedule.length === 0 ? (
          <p>Add shards to see optimal reading order</p>
        ) : (
          <ol>
            {schedule.map((shard, i) => {
              const pos = positions[i] || {};
              return (
                <li key={shard.id}>
                  <strong>{shard.type}</strong> ({shard.family})
                  {!pos.isCanonical && (
                    <span style={styles.warning}> ⚠ Not in canonical position</span>
                  )}
                </li>
              );
            })}
          </ol>
        )}
      </div>

      <div style={styles.hint}>
        <strong>Λ-Tip:</strong> The canonical order ensures optimal logical flow. Green indicates
        ideal positioning.
      </div>
    </div>
  );
}

/**
 * Π-Profiler Tab
 */
function ΠProfilerTab() {
  const { shards, families, familyNames } = useHTFOntology();
  const { coherence, isMerged, connections } = useΠMerge(shards);

  const familyCoverage = {};
  for (const name of familyNames) {
    const count = shards.filter(s => s.family === name).length;
    const expected = families[name].length;
    familyCoverage[name] = {
      count,
      expected,
      percent: (count / expected) * 100,
    };
  }

  return (
    <div style={styles.tab}>
      <h2>Π-Profiler: Family Composition</h2>

      <div style={styles.scoreBox}>
        <p>
          <strong>Π-Coherence:</strong> {(coherence * 100).toFixed(1)}%
        </p>
        <p>
          {isMerged ? (
            <span style={{ color: 'green' }}>✓ Ready to merge</span>
          ) : (
            <span style={{ color: 'orange' }}>⚠ Needs work</span>
          )}
        </p>
      </div>

      <div style={styles.familyGrid}>
        {familyNames.map(family => {
          const cov = familyCoverage[family];
          const color = cov.percent > 80 ? '#22c55e' : cov.percent > 50 ? '#f59e0b' : '#ef4444';

          return (
            <div key={family} style={styles.familyCard}>
              <strong>{family}</strong>
              <p>
                {cov.count}/{cov.expected} shards
              </p>
              <div style={styles.scoreBar}>
                <div
                  style={{
                    ...styles.scoreBarFill,
                    width: `${cov.percent}%`,
                    backgroundColor: color,
                  }}
                ></div>
              </div>
              <small>{cov.percent.toFixed(0)}%</small>
            </div>
          );
        })}
      </div>

      <div style={styles.hint}>
        <strong>Π-Tip:</strong> Aim for 100% coverage of each family for balanced thesis
        composition. Found {connections.length} cross-shard connections.
      </div>
    </div>
  );
}

/**
 * Γ-Checker Tab
 */
function ΓCheckerTab() {
  const { shards } = useHTFOntology();
  const { driftScore, violations, buildGlossary, fixDrift } = useΓGlobalization(shards);

  const driftPercent = driftScore * 100;
  const driftColor = driftScore < 0.1 ? '#22c55e' : driftScore < 0.3 ? '#f59e0b' : '#ef4444';

  return (
    <div style={styles.tab}>
      <h2>Γ-Checker: Coherence Validation</h2>

      <div style={styles.scoreBox}>
        <p>
          <strong>Drift Score:</strong> {driftPercent.toFixed(1)}%
        </p>
        <div style={styles.scoreBar}>
          <div
            style={{
              ...styles.scoreBarFill,
              width: `${100 - driftPercent}%`,
              backgroundColor: driftColor,
            }}
          ></div>
        </div>
        <small>Target: &lt; 10% drift</small>
      </div>

      <div style={styles.violationList}>
        <h3>Violations ({violations.length})</h3>
        {violations.length === 0 ? (
          <p style={{ color: 'green' }}>✓ No drift detected!</p>
        ) : (
          violations.slice(0, 5).map((v, i) => (
            <div key={i} style={styles.violation}>
              <strong>{v.type}</strong>
              <p>{v.message || v.issue}</p>
            </div>
          ))
        )}
        {violations.length > 5 && <p>... and {violations.length - 5} more</p>}
      </div>

      <div style={styles.buttonGroup}>
        <button onClick={buildGlossary} style={styles.button}>
          Build Glossary
        </button>
        <button onClick={fixDrift} style={styles.button}>
          Get Fixes
        </button>
      </div>

      <div style={styles.hint}>
        <strong>Γ-Tip:</strong> Low drift indicates good conceptual coherence. Build glossaries to
        maintain consistency.
      </div>
    </div>
  );
}

/**
 * τ-Dashboard Tab
 */
function τDashboardTab() {
  const { shards } = useHTFOntology();
  const { coherence } = useΠMerge(shards);
  const { driftScore } = useΓGlobalization(shards);
  const { overallScore } = useQInvariants(shards);
  const { currentDraft, energy, completionPercent, atFinal } = useτEvolution(shards, {
    overallScore,
    coherence,
    driftScore,
  });

  return (
    <div style={styles.tab}>
      <h2>τ-Dashboard: Evolution Tracking</h2>

      <div style={styles.scoreBox}>
        <p>
          <strong>Current Draft:</strong> {currentDraft}
        </p>
        <p>
          <strong>Completion:</strong> {completionPercent}%
        </p>
        <div style={styles.scoreBar}>
          <div
            style={{
              ...styles.scoreBarFill,
              width: `${completionPercent}%`,
              backgroundColor: atFinal ? '#22c55e' : '#3b82f6',
            }}
          ></div>
        </div>
      </div>

      <div style={styles.metricsGrid}>
        <div style={styles.metricBox}>
          <p>Q-Invariants</p>
          <p style={styles.largeNumber}>{(overallScore * 100).toFixed(0)}%</p>
        </div>
        <div style={styles.metricBox}>
          <p>Π-Coherence</p>
          <p style={styles.largeNumber}>{(coherence * 100).toFixed(0)}%</p>
        </div>
        <div style={styles.metricBox}>
          <p>Γ-Drift</p>
          <p style={styles.largeNumber}>{(100 - driftScore * 100).toFixed(0)}%</p>
        </div>
        <div style={styles.metricBox}>
          <p>Energy (μ-distance)</p>
          <p style={styles.largeNumber}>{(energy * 100).toFixed(0)}%</p>
        </div>
      </div>

      {atFinal && (
        <div
          style={{
            ...styles.successBox,
            backgroundColor: '#dcfce7',
            color: '#166534',
          }}
        >
          <h3>✓ Thesis Complete!</h3>
          <p>Your thesis has converged to μ(μ(HTF_O)). Ready for submission.</p>
        </div>
      )}

      <div style={styles.hint}>
        <strong>τ-Tip:</strong> Monitor energy and completion percentage. Auto-advance when each
        metric crosses threshold.
      </div>
    </div>
  );
}

/**
 * Add Shard Form Component
 */
function AddShardForm({ onAdd }) {
  const [type, setType] = useState('Intro');
  const [family, setFamily] = useState('IMRaD');

  const families = {
    IMRaD: ['Intro', 'Method', 'Result', 'Discussion'],
    Papers: ['Paper1', 'Paper2', 'Paper3', 'Synthesis'],
    Argument: ['Claim', 'Ground', 'Proof', 'Objection', 'Reply'],
    Contribution: ['Gap', 'Design', 'Eval', 'Impact'],
    Monograph: ['Context', 'Canon', 'Method2', 'Analysis', 'Conclusion'],
    DSR: ['Problem', 'Artifact', 'Eval2', 'Theory'],
    Narrative: ['Field', 'Voice', 'Pattern', 'Insight'],
  };

  return (
    <div>
      <h4>Add New Shard</h4>
      <select value={family} onChange={e => setFamily(e.target.value)} style={styles.input}>
        {Object.keys(families).map(f => (
          <option key={f} value={f}>
            {f}
          </option>
        ))}
      </select>
      <select value={type} onChange={e => setType(e.target.value)} style={styles.input}>
        {families[family].map(t => (
          <option key={t} value={t}>
            {t}
          </option>
        ))}
      </select>
      <button
        onClick={() => onAdd({ type, family, content: '' })}
        style={{ ...styles.button, width: '100%' }}
      >
        + Add
      </button>
    </div>
  );
}

/**
 * Styles
 */
const styles = {
  app: {
    fontFamily: 'system-ui, -apple-system, sans-serif',
    backgroundColor: '#f5f5f5',
    minHeight: '100vh',
  },
  header: {
    backgroundColor: '#1e293b',
    color: 'white',
    padding: '2rem',
    textAlign: 'center',
  },
  nav: {
    display: 'flex',
    gap: '0.5rem',
    padding: '1rem',
    backgroundColor: '#334155',
    flexWrap: 'wrap',
  },
  navButton: {
    color: 'white',
    border: 'none',
    padding: '0.5rem 1rem',
    cursor: 'pointer',
    borderRadius: '4px',
  },
  content: {
    padding: '1.5rem',
    maxWidth: '1200px',
    margin: '0 auto',
  },
  tab: {
    backgroundColor: 'white',
    borderRadius: '8px',
    padding: '1.5rem',
    boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
  },
  twoColumn: {
    display: 'grid',
    gridTemplateColumns: '1fr 1fr',
    gap: '1.5rem',
  },
  shardList: {
    display: 'flex',
    flexDirection: 'column',
    gap: '0.5rem',
    maxHeight: '400px',
    overflowY: 'auto',
  },
  shardItem: {
    padding: '0.75rem',
    borderRadius: '4px',
    cursor: 'pointer',
    border: '1px solid #e0e0e0',
  },
  addShardForm: {
    marginTop: '1rem',
    padding: '1rem',
    backgroundColor: '#f9fafb',
    borderRadius: '4px',
  },
  input: {
    width: '100%',
    padding: '0.5rem',
    marginBottom: '0.5rem',
    border: '1px solid #d0d0d0',
    borderRadius: '4px',
  },
  button: {
    backgroundColor: '#3b82f6',
    color: 'white',
    padding: '0.5rem 1rem',
    border: 'none',
    borderRadius: '4px',
    cursor: 'pointer',
  },
  textarea: {
    width: '100%',
    minHeight: '250px',
    padding: '0.75rem',
    border: '1px solid #d0d0d0',
    borderRadius: '4px',
    fontFamily: 'monospace',
    marginBottom: '0.5rem',
  },
  scoreBox: {
    padding: '1rem',
    backgroundColor: '#f0f0f0',
    borderRadius: '4px',
    marginBottom: '1rem',
  },
  scoreBar: {
    width: '100%',
    height: '24px',
    backgroundColor: '#e0e0e0',
    borderRadius: '4px',
    overflow: 'hidden',
    margin: '0.5rem 0',
  },
  scoreBarFill: {
    height: '100%',
    transition: 'width 0.3s ease',
  },
  scheduleList: {
    marginTop: '1rem',
  },
  warning: {
    color: '#ef4444',
    fontSize: '0.9em',
  },
  hint: {
    marginTop: '1rem',
    padding: '1rem',
    backgroundColor: '#e0f2fe',
    borderRadius: '4px',
    color: '#0369a1',
  },
  familyGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(150px, 1fr))',
    gap: '1rem',
    marginTop: '1rem',
  },
  familyCard: {
    padding: '1rem',
    backgroundColor: '#f9fafb',
    borderRadius: '4px',
    border: '1px solid #e0e0e0',
    textAlign: 'center',
  },
  violationList: {
    marginTop: '1rem',
  },
  violation: {
    padding: '0.75rem',
    backgroundColor: '#fef3c7',
    borderLeft: '4px solid #f59e0b',
    marginBottom: '0.5rem',
    borderRadius: '4px',
  },
  buttonGroup: {
    display: 'flex',
    gap: '0.5rem',
    marginTop: '1rem',
  },
  metricsGrid: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(150px, 1fr))',
    gap: '1rem',
    marginTop: '1rem',
  },
  metricBox: {
    padding: '1rem',
    backgroundColor: '#f0f0f0',
    borderRadius: '4px',
    textAlign: 'center',
  },
  largeNumber: {
    fontSize: '2em',
    fontWeight: 'bold',
    color: '#3b82f6',
    margin: '0.5rem 0 0 0',
  },
  successBox: {
    padding: '1rem',
    borderRadius: '4px',
    marginTop: '1rem',
  },
};

export default HTFThesisBuilderApp;
