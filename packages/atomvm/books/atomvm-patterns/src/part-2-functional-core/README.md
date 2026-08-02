# Part 2: The Functional Core

FleetPulse business rules remain pure `(state, message) => nextState` functions. Composition stays flat and purpose-oriented. Tests call real functions directly; no framework or collaborator doubles are required.

Every example in this part is executed by the Chicago suite and has a corresponding real AtomVM marker.
