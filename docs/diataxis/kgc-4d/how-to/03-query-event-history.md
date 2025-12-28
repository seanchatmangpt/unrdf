# How-To: Query Event History with SPARQL

**Problem:** You need to extract insights from immutable event logs for analytics or audit reports.

**Solution:** Use SPARQL queries on the EventLog graph with temporal filtering and aggregation.

**Time:** 15 minutes

---

## Prerequisites

- [Tutorial 04: Query Event Logs](../tutorials/04-query-event-logs.md)
- Basic SPARQL knowledge

---

## Common Queries

### 1. Events by Time Range

```javascript
const startTime = fromISO('2025-12-01T00:00:00.000Z');
const endTime = fromISO('2025-12-31T23:59:59.999Z');

const results = await store.queryEventLog(`
  PREFIX kgc: <http://kgc.io/>
  SELECT ?event ?type ?t_ns
  WHERE {
    GRAPH <${GRAPHS.EVENT_LOG}> {
      ?event kgc:type ?type .
      ?event kgc:t_ns ?t_ns .
      FILTER (?t_ns >= ${startTime} && ?t_ns <= ${endTime})
    }
  }
  ORDER BY ?t_ns
`);
```

### 2. Event Count by Type

```javascript
const stats = await store.queryEventLog(`
  PREFIX kgc: <http://kgc.io/>
  SELECT ?type (COUNT(?event) as ?count)
  WHERE {
    GRAPH <${GRAPHS.EVENT_LOG}> {
      ?event kgc:type ?type .
    }
  }
  GROUP BY ?type
`);
```

### 3. Audit Trail for Entity

```javascript
const entityAudit = await store.queryEventLog(`
  PREFIX kgc: <http://kgc.io/>
  SELECT ?event ?type ?t_ns ?payload
  WHERE {
    GRAPH <${GRAPHS.EVENT_LOG}> {
      ?event kgc:type ?type .
      ?event kgc:t_ns ?t_ns .
      ?event kgc:payload ?payload .
      FILTER (CONTAINS(STR(?payload), "Alice"))
    }
  }
  ORDER BY ?t_ns
`);
```

---

## Evidence

**Source:** `/home/user/unrdf/packages/kgc-4d/src/store.mjs` (queryEventLog)  
**Example:** `/home/user/unrdf/packages/kgc-4d/examples/basic-usage.mjs:97-119`

---

## Related

- [Reference: Events & Predicates](../reference/events-predicates.md)
- [How-To 02: Audit Decision Trail](../../how-to/02-audit-decision-trail.md)
