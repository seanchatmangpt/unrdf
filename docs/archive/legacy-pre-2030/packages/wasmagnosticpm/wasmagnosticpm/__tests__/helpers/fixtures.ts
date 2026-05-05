/**
 * Shared test fixtures - XES and OCEL data for test files
 * Eliminates duplication across 20+ test files
 */

/**
 * Minimal XES log with 1 trace, 2 events
 * Use for: fast tests, type checking, basic functionality
 */
export const XES_MINIMAL = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" openlog.version="1.0">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <global scope="trace"><string key="concept:name" value="undefined"/></global>
  <global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global>
  <trace><string key="concept:name" value="Case1"/>
    <event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event>
    <event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event>
  </trace>
</log>`;

/**
 * Sequential XES log with 3 traces, clear activity sequence
 * Use for: algorithm tests, DFG discovery, conformance checking
 */
export const XES_SEQUENTIAL = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" openlog.version="1.0">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <global scope="trace"><string key="concept:name" value="undefined"/></global>
  <global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global>
  <trace><string key="concept:name" value="Case1"/>
    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event>
    <event><string key="concept:name" value="Process"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event>
    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-01T12:00:00"/></event>
  </trace>
  <trace><string key="concept:name" value="Case2"/>
    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-02T10:00:00"/></event>
    <event><string key="concept:name" value="Process"/><date key="time:timestamp" value="2023-01-02T11:00:00"/></event>
    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-02T12:00:00"/></event>
  </trace>
  <trace><string key="concept:name" value="Case3"/>
    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-03T10:00:00"/></event>
    <event><string key="concept:name" value="Process"/><date key="time:timestamp" value="2023-01-03T11:00:00"/></event>
    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-03T12:00:00"/></event>
  </trace>
</log>`;

/**
 * Parallel XES log with concurrent activities
 * Use for: complex discovery algorithms, parallel mining
 */
export const XES_PARALLEL = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" openlog.version="1.0">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <global scope="trace"><string key="concept:name" value="undefined"/></global>
  <global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global>
  <trace><string key="concept:name" value="Case1"/>
    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event>
    <event><string key="concept:name" value="TaskA"/><date key="time:timestamp" value="2023-01-01T10:30:00"/></event>
    <event><string key="concept:name" value="TaskB"/><date key="time:timestamp" value="2023-01-01T10:30:00"/></event>
    <event><string key="concept:name" value="Merge"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event>
    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-01T12:00:00"/></event>
  </trace>
</log>`;

/**
 * OCEL JSON minimal object-centric event log (1 object, 2 events)
 * Use for: OCEL-specific tests
 */
export const OCEL_MINIMAL = `{
  "event_types": ["Create", "Ship"],
  "object_types": ["Order"],
  "events": [
    {
      "id": "evt1",
      "event_type": "Create",
      "timestamp": "2023-01-01T10:00:00Z",
      "attributes": {},
      "object_ids": ["Order-1"],
      "object_refs": []
    },
    {
      "id": "evt2",
      "event_type": "Ship",
      "timestamp": "2023-01-01T11:00:00Z",
      "attributes": {},
      "object_ids": ["Order-1"],
      "object_refs": []
    }
  ],
  "objects": [
    {
      "id": "Order-1",
      "object_type": "Order",
      "attributes": {},
      "changes": []
    }
  ],
  "object_relations": []
}`;

/**
 * Multi-trace XES for workflow testing with multiple cases
 * Use for: full workflow tests, integration scenarios
 */
export const XES_WORKFLOW = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" xmlns="http://www.xes-standard.org/">
  <trace>
    <string key="concept:name" value="Case1"/>
    <event>
      <string key="concept:name" value="Activity A"/>
      <date key="time:timestamp" value="2023-01-01T10:00:00.000+00:00"/>
    </event>
    <event>
      <string key="concept:name" value="Activity B"/>
      <date key="time:timestamp" value="2023-01-01T10:05:00.000+00:00"/>
    </event>
    <event>
      <string key="concept:name" value="Activity C"/>
      <date key="time:timestamp" value="2023-01-01T10:10:00.000+00:00"/>
    </event>
  </trace>
  <trace>
    <string key="concept:name" value="Case2"/>
    <event>
      <string key="concept:name" value="Activity A"/>
      <date key="time:timestamp" value="2023-01-01T11:00:00.000+00:00"/>
    </event>
    <event>
      <string key="concept:name" value="Activity B"/>
      <date key="time:timestamp" value="2023-01-01T11:05:00.000+00:00"/>
    </event>
  </trace>
</log>`;
