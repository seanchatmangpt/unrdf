# Part 3: Process Boundaries

The functional core is wrapped in real FIFO mailboxes. `tell` is asynchronous, `ask` is bounded, `ProcRef` survives restarts, names provide discovery, and trapped exits turn linked failures into ordinary messages.

Every example in this part is executed by the Chicago suite and has a corresponding real AtomVM marker.
