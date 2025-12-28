# Reference: Named Graphs

**Source:** `/home/user/unrdf/packages/kgc-4d/src/constants.mjs`

---

## Graph URIs

```javascript
export const GRAPHS = {
  UNIVERSE: 'http://kgc.io/Universe',
  EVENT_LOG: 'http://kgc.io/EventLog',
  SYSTEM: 'http://kgc.io/System',
};
```

### Descriptions

**Universe** - Current observable state (hot, mutable)  
**EventLog** - Immutable event history (append-only)  
**System** - Metadata, configuration, snapshot cache

---

## Usage

```javascript
import { GRAPHS } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
const quads = [...store.match(null, null, null, universeGraph)];
```

---

## Related

- [Tutorial 02: Create and Freeze](../tutorials/02-create-freeze-universe.md)
- [Explanation: Why Partitioned Universes](../../explanation/why-partitioned-universes.md)
