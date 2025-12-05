'use client';

import { useState, useRef, useEffect } from 'react';

// Sample of key PlantUML diagrams from /docs/diagrams
const PLANTUML_DIAGRAMS = [
  {
    id: 'knowledge-hooks-c4-context',
    title: 'Knowledge Hooks - C4 Context',
    description: 'System context diagram showing knowledge hooks architecture',
    category: 'Architecture',
    content: `@startuml knowledge-hooks-c4-context
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Context.puml

LAYOUT_WITH_LEGEND()

title Knowledge Hooks - C4 Context Diagram

Person(user, "User", "Creates intents and observes outcomes")
System(kh, "Knowledge Hooks Engine", "Transforms intent to outcome through μ(O) operators")
System_Ext(rdf, "RDF Store", "Persists ontologies and quads")

Rel(user, kh, "Executes hooks")
Rel(kh, rdf, "Reads/writes")

@enduml`,
  },
  {
    id: 'knowledge-hooks-sequence',
    title: 'Knowledge Hooks - Execution Sequence',
    description: 'Sequence diagram showing hook execution flow',
    category: 'Flow',
    content: `@startuml knowledge-hooks-sequence
title Knowledge Hooks - Execution Sequence

participant User as user
participant "Hook Manager" as hm
participant "Executor" as ex
participant "μ(O) Operators" as op
participant "Store" as store

user -> hm: executeByTrigger(trigger, quad)
hm -> hm: findHooksByTrigger()
hm -> ex: compiledChain(hooks)
ex -> op: validate(quad)
op -> op: reduce entropy
ex -> store: updateQuad()
store --> ex: success
ex --> hm: result
hm --> user: outcome

@enduml`,
  },
  {
    id: 'hook-execution-flow',
    title: 'Hook Execution Flow',
    description: 'Detailed flow of hook execution with error handling',
    category: 'Flow',
    content: `@startuml hook-execution-flow
title Hook Execution Flow

start
:Receive trigger + quad;
:Validate hook schema;
if (Valid?) then (no)
  :Return error;
  stop
else (yes)
  :Find matching hooks;
  if (Found?) then (no)
    :Return noop;
    stop
  else (yes)
    :Execute hook chain;
    if (Has validation?) then (yes)
      :Run validate();
      if (Valid?) then (no)
        :Log failure;
        :Return invalid;
        stop
      else (yes)
      endif
    endif
    if (Has transform?) then (yes)
      :Run transform();
      if (Successful?) then (no)
        :Log error;
        :Return error;
        stop
      else (yes)
      endif
    endif
    :Persist changes;
    :Return success;
    stop
  endif
endif
end

@enduml`,
  },
  {
    id: 'operator-chain-entropy',
    title: 'μ(O) Operator Chain - Entropy Reduction',
    description: 'Shows 8-operator chain with entropy reduction (50→1 nats)',
    category: 'Operators',
    content: `@startuml operator-chain-entropy
title μ(O) Calculus: Operator Chain Entropy Reduction

skinparam backgroundColor #1e293b
skinparam ArrowColor #a855f7
skinparam ClassBackgroundColor #ea580c
skinparam ClassBorderColor #f97316

class μ1 {
  **Validation**
  Entropy: 50 → 45.8
  Reduction: 4.2 nats
  Trigger: before-add
}

class μ2 {
  **Temporal**
  Entropy: 45.8 → 40.0
  Reduction: 5.8 nats
  Trigger: on-interval
}

class μ3 {
  **Geographic**
  Entropy: 40.0 → 32.9
  Reduction: 7.1 nats
  Trigger: before-add
}

class μ4 {
  **Batch**
  Entropy: 32.9 → 26.6
  Reduction: 6.3 nats
  Trigger: before-add
}

class μ5 {
  **Threshold**
  Entropy: 26.6 → 20.4
  Reduction: 5.9 nats
  Trigger: on-interval
}

class μ6 {
  **Cascading**
  Entropy: 20.4 → 14.2
  Reduction: 6.2 nats
  Trigger: after-add
}

class μ7 {
  **Complex**
  Entropy: 14.2 → 8.0
  Reduction: 5.4 nats
  Trigger: before-add
}

class μ8 {
  **Deterministic**
  Entropy: 8.0 → 1.0
  Reduction: 0.1 nats
  Trigger: after-add
}

μ1 --> μ2
μ2 --> μ3
μ3 --> μ4
μ4 --> μ5
μ5 --> μ6
μ6 --> μ7
μ7 --> μ8

@enduml`,
  },
  {
    id: 'federation-protocol',
    title: 'Federation Protocol',
    description: 'Multi-agent federation and consensus protocol',
    category: 'Protocol',
    content: `@startuml federation-protocol
title Knowledge Hooks - Federation Protocol

participant "Agent A" as a
participant "Federation Manager" as fm
participant "Agent B" as b
participant "Agent C" as c

a -> fm: propose(change)
fm -> b: broadcast(change)
fm -> c: broadcast(change)
b -> b: validate()
c -> c: validate()
b -> fm: vote(yes)
c -> fm: vote(yes)
fm -> fm: consensus check
fm -> a: commit(change)
fm -> b: commit(change)
fm -> c: commit(change)

@enduml`,
  },
  {
    id: 'raft-consensus',
    title: 'Raft Consensus Protocol',
    description: 'Leader election and log replication in Raft',
    category: 'Protocol',
    content: `@startuml raft-consensus
title Raft Consensus - Leader Election

state Follower
state Candidate
state Leader

[*] --> Follower

Follower --> Candidate: election timeout\\nincrement term\\nvote for self
Candidate --> Follower: receive higher term
Candidate --> Leader: receive majority votes\\nbecome leader
Leader --> Follower: receive higher term\\nstep down
Follower --> [*]

note right of Leader
  - Send heartbeats
  - Replicate log entries
  - Commit entries
end note

@enduml`,
  },
  {
    id: 'swarm-coordination',
    title: 'Swarm Coordination',
    description: 'Multi-agent swarm topology and task distribution',
    category: 'Architecture',
    content: `@startuml swarm-coordination
title Knowledge Hooks - Swarm Coordination

skinparam BackgroundColor #1e293b

package "Coordinator" {
  [Task Orchestrator]
  [Load Balancer]
}

package "Agent Pool" {
  [Agent 1]
  [Agent 2]
  [Agent 3]
  [Agent 4]
}

package "Shared State" {
  database "Memory" as mem
  database "Store" as store
}

[Task Orchestrator] --> [Load Balancer]
[Load Balancer] --> [Agent 1]
[Load Balancer] --> [Agent 2]
[Load Balancer] --> [Agent 3]
[Load Balancer] --> [Agent 4]

[Agent 1] --> mem
[Agent 2] --> mem
[Agent 3] --> mem
[Agent 4] --> mem

[Agent 1] --> store
[Agent 2] --> store
[Agent 3] --> store
[Agent 4] --> store

@enduml`,
  },
];

export default function PlantUMLViewer({ data }) {
  const [selectedDiagram, setSelectedDiagram] = useState(PLANTUML_DIAGRAMS[0]);
  const [category, setCategory] = useState('All');
  const diagramRef = useRef(null);

  const categories = ['All', ...new Set(PLANTUML_DIAGRAMS.map((d) => d.category))];
  const filteredDiagrams =
    category === 'All'
      ? PLANTUML_DIAGRAMS
      : PLANTUML_DIAGRAMS.filter((d) => d.category === category);

  useEffect(() => {
    if (diagramRef.current && selectedDiagram) {
      // For now, render as text since we need plantuml.com API
      // In production, use @mermaid-js/mermaid-cli or plantuml-encoder
      diagramRef.current.innerHTML = '';
      const pre = document.createElement('pre');
      pre.className = 'bg-slate-900 text-slate-300 p-4 rounded text-xs overflow-auto max-h-96';
      pre.textContent = selectedDiagram.content;
      diagramRef.current.appendChild(pre);
    }
  }, [selectedDiagram]);

  return (
    <div className="space-y-6">
      <div className="space-y-2">
        <h3 className="text-lg font-semibold text-cyan-400">Architecture & Design Diagrams</h3>
        <p className="text-sm text-slate-400">
          PlantUML diagrams showing knowledge hooks architecture, execution flows, federation
          protocols, and consensus mechanisms.
        </p>
      </div>

      {/* Category Filter */}
      <div className="flex flex-wrap gap-2">
        {categories.map((cat) => (
          <button
            key={cat}
            onClick={() => setCategory(cat)}
            className={`px-4 py-2 rounded text-sm font-medium transition-colors ${
              category === cat
                ? 'bg-cyan-500 text-slate-900'
                : 'bg-slate-800 text-slate-400 hover:text-slate-200 border border-slate-700'
            }`}
          >
            {cat}
          </button>
        ))}
      </div>

      {/* Diagram Selector */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="space-y-2">
          <h4 className="text-sm font-semibold text-slate-300">
            Diagrams ({filteredDiagrams.length})
          </h4>
          <div className="space-y-2 max-h-96 overflow-y-auto border border-slate-700 rounded p-3 bg-slate-900/50">
            {filteredDiagrams.map((diagram) => (
              <button
                key={diagram.id}
                onClick={() => setSelectedDiagram(diagram)}
                className={`w-full text-left px-3 py-2 rounded text-sm transition-colors ${
                  selectedDiagram.id === diagram.id
                    ? 'bg-cyan-900/50 border border-cyan-500 text-cyan-300'
                    : 'border border-slate-700 text-slate-300 hover:bg-slate-800'
                }`}
              >
                <div className="font-semibold">{diagram.title}</div>
                <div className="text-xs text-slate-500">{diagram.category}</div>
              </button>
            ))}
          </div>
        </div>

        {/* Diagram Display */}
        <div className="space-y-2">
          <div className="bg-slate-900/50 border border-slate-700 rounded p-4">
            <h4 className="text-sm font-semibold text-cyan-400 mb-2">{selectedDiagram.title}</h4>
            <p className="text-xs text-slate-400 mb-3">{selectedDiagram.description}</p>
            <div
              ref={diagramRef}
              className="bg-slate-950 rounded border border-slate-700 p-3 min-h-64 overflow-auto"
            />
          </div>

          {/* PlantUML Info */}
          <div className="bg-slate-900/50 border border-slate-700 rounded p-3 text-xs text-slate-400">
            <div className="font-semibold text-cyan-400 mb-1">PlantUML Format</div>
            <div>Diagrams show architecture, flows, protocols, and operator chains</div>
          </div>
        </div>
      </div>

      {/* Statistics */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <div className="bg-slate-900/50 border border-slate-700 rounded p-4">
          <div className="text-xs text-slate-400 uppercase">Total Diagrams</div>
          <div className="text-2xl font-bold text-cyan-400 mt-1">{PLANTUML_DIAGRAMS.length}</div>
        </div>
        <div className="bg-slate-900/50 border border-slate-700 rounded p-4">
          <div className="text-xs text-slate-400 uppercase">Categories</div>
          <div className="text-2xl font-bold text-cyan-400 mt-1">{categories.length - 1}</div>
        </div>
        <div className="bg-slate-900/50 border border-slate-700 rounded p-4">
          <div className="text-xs text-slate-400 uppercase">Operators</div>
          <div className="text-2xl font-bold text-cyan-400 mt-1">8 (μ₁-μ₈)</div>
        </div>
        <div className="bg-slate-900/50 border border-slate-700 rounded p-4">
          <div className="text-xs text-slate-400 uppercase">Entropy Reduction</div>
          <div className="text-2xl font-bold text-cyan-400 mt-1">50 → 1</div>
        </div>
      </div>

      {/* Architecture Overview */}
      <div className="card space-y-3">
        <h4 className="font-semibold text-cyan-400">Knowledge Hooks System Overview</h4>
        <ul className="space-y-2 text-sm text-slate-300">
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-orange-400 mt-1.5 flex-shrink-0"></span>
            <span>
              <strong>Architecture:</strong> C4 context diagrams showing system boundaries and
              interactions
            </span>
          </li>
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-purple-400 mt-1.5 flex-shrink-0"></span>
            <span>
              <strong>Flows:</strong> Execution sequences showing hook validation, transformation,
              and persistence
            </span>
          </li>
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-cyan-400 mt-1.5 flex-shrink-0"></span>
            <span>
              <strong>Operators:</strong> 8-stage operator chain (μ₁-μ₈) with entropy reduction from
              50 to 1 nats
            </span>
          </li>
          <li className="flex gap-2">
            <span className="inline-block w-1.5 h-1.5 rounded-full bg-green-400 mt-1.5 flex-shrink-0"></span>
            <span>
              <strong>Protocols:</strong> Federation, Raft consensus, and swarm coordination for
              distributed execution
            </span>
          </li>
        </ul>
      </div>

      {/* Diagram Sources Info */}
      <div className="bg-slate-800/50 rounded-lg p-4 space-y-2 text-xs text-slate-400">
        <div className="font-semibold text-slate-300">PlantUML Diagram Sources</div>
        <div>
          Diagrams sourced from: <code>/docs/diagrams/*.puml</code> including knowledge hooks
          architecture, execution flows, federation protocols, RAFT consensus, and distributed
          system coordination patterns.
        </div>
        <div className="mt-2">
          <strong>Note:</strong> For full PlantUML rendering in production, integrate with
          plantuml.com API or @mermaid-js/mermaid-cli for server-side SVG generation.
        </div>
      </div>
    </div>
  );
}
