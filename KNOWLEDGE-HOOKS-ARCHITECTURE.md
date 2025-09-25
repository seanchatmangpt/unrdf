# Knowledge Hooks System Architecture

## Plant Diagram - Knowledge Hooks Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                        Knowledge Hooks System                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐ │
│  │   defineHook   │    │  evaluateHook   │    │ useKnowledgeHooks│ │
│  │                 │    │                 │    │                 │ │
│  │ • id            │    │ • Execute Query │    │ • defineHook     │ │
│  │ • query         │    │ • Check Predicates│   │ • evaluateHook  │ │
│  │ • predicates    │    │ • Return Result │    │ • evaluateHooks │ │
│  │ • combine        │    │                 │    │ • getStats      │ │
│  └─────────────────┘    └─────────────────┘    └─────────────────┘ │
│           │                       │                       │         │
│           │                       │                       │         │
│           ▼                       ▼                       ▼         │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │                    Existing Composables                         │ │
│  │                                                                 │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │ │
│  │  │  useGraph   │  │ useReasoner │  │ useTerms    │  │useDelta │ │ │
│  │  │             │  │             │  │             │  │         │ │ │
│  │  │ • select()  │  │ • infer()   │  │ • iri()     │  │ • compare│ │ │
│  │  │ • ask()     │  │ • wouldInfer│  │ • lit()     │  │ • apply │ │ │
│  │  │ • construct │  │ • export()  │  │ • bnode()   │  │ • stats │ │ │
│  │  │ • update    │  │ • import()  │  │ • quad()    │  │         │ │ │
│  │  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │ │
│  │                                                                 │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │ │
│  │  │ useTurtle   │  │   useZod     │  │ useCanon    │              │ │
│  │  │             │  │             │  │             │              │ │
│  │  │ • parse()   │  │ • generate  │  │ • canonicalize│            │ │
│  │  │ • serialize │  │ • validate  │  │ • isIsomorphic│            │ │
│  │  │ • load()    │  │ • inferType │  │ • skolemize │              │ │
│  │  │ • save()    │  │             │  │             │              │ │
│  │  └─────────────┘  └─────────────┘  └─────────────┘              │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │                    Predicate Types                              │ │
│  │                                                                 │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────┐ │ │
│  │  │   COUNT     │  │ THRESHOLD   │  │    ASK      │  │  OWL    │ │ │
│  │  │             │  │             │  │             │  │         │ │ │
│  │  │ • operator  │  │ • variable  │  │ • query     │  │ • rules │ │ │
│  │  │ • value     │  │ • operator  │  │ • expected  │  │         │ │ │
│  │  │             │  │ • value     │  │             │  │         │ │ │
│  │  └─────────────┘  └─────────────┘  └─────────────┘  └─────────┘ │ │
│  │                                                                 │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │ │
│  │  │   SHACL    │  │   DELTA     │  │   ZOD       │              │ │
│  │  │             │  │             │  │             │              │ │
│  │  │ • shape     │  │ • compare   │  │ • schema    │              │ │
│  │  │ • strict    │  │ • changes   │  │ • validate  │              │ │
│  │  │             │  │             │  │             │              │ │
│  │  └─────────────┘  └─────────────┘  └─────────────┘              │ │
│  └─────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘

## Data Flow

1. **Hook Definition**: User defines hook with SPARQL query + predicates
2. **Query Execution**: useGraph.select() executes SPARQL query
3. **Predicate Evaluation**: Each predicate type uses appropriate composable:
   - COUNT: Count query results
   - THRESHOLD: Extract numeric values from results
   - ASK: useGraph.ask() for boolean queries
   - OWL: useReasoner.infer() for reasoning
   - SHACL: useValidator (stub) for validation
   - DELTA: useDelta.compareWith() for changes
   - ZOD: useZod.validate() for schema validation
4. **Result Combination**: AND/OR logic combines predicate results
5. **Statistics**: Aggregate results across multiple hooks

## Key Design Principles

- **Composable-First**: Leverage existing composables, don't reinvent
- **Single Responsibility**: Each predicate type has one clear purpose
- **Context-Aware**: All operations work with the global store context
- **Extensible**: Easy to add new predicate types
- **Testable**: Clear separation of concerns for testing

