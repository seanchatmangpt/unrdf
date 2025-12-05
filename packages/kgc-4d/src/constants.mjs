/**
 * KGC 4D Constants - Named Graphs and Event Types
 */

export const GRAPHS = {
  UNIVERSE: 'http://kgc.io/Universe',     // Current mutable state
  EVENT_LOG: 'http://kgc.io/EventLog',    // Immutable append-only history
  SYSTEM: 'http://kgc.io/System',         // Metadata and configuration
};

export const EVENT_TYPES = {
  CREATE: 'CREATE',
  UPDATE: 'UPDATE',
  DELETE: 'DELETE',
  SNAPSHOT: 'SNAPSHOT',
  HOOK_EXECUTION: 'HOOK_EXECUTION',
};

// Predicate URIs for serialization
export const PREDICATES = {
  T_NS: 'http://kgc.io/t_ns',
  TYPE: 'http://kgc.io/type',
  GIT_REF: 'http://kgc.io/git_ref',
  PAYLOAD: 'http://kgc.io/payload',
  VECTOR_CLOCK: 'http://kgc.io/vector',
};
