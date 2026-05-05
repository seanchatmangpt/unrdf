/**
 * Shared test fixtures - XES and OCEL data for test files
 * Eliminates duplication across 20+ test files
 */
/**
 * Minimal XES log with 1 trace, 2 events
 * Use for: fast tests, type checking, basic functionality
 */
export declare const XES_MINIMAL =
  '<?xml version="1.0" encoding="UTF-8"?>\n<log xes.version="1.0" openlog.version="1.0">\n  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>\n  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>\n  <global scope="trace"><string key="concept:name" value="undefined"/></global>\n  <global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global>\n  <trace><string key="concept:name" value="Case1"/>\n    <event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event>\n    <event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event>\n  </trace>\n</log>';
/**
 * Sequential XES log with 3 traces, clear activity sequence
 * Use for: algorithm tests, DFG discovery, conformance checking
 */
export declare const XES_SEQUENTIAL =
  '<?xml version="1.0" encoding="UTF-8"?>\n<log xes.version="1.0" openlog.version="1.0">\n  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>\n  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>\n  <global scope="trace"><string key="concept:name" value="undefined"/></global>\n  <global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global>\n  <trace><string key="concept:name" value="Case1"/>\n    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event>\n    <event><string key="concept:name" value="Process"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event>\n    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-01T12:00:00"/></event>\n  </trace>\n  <trace><string key="concept:name" value="Case2"/>\n    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-02T10:00:00"/></event>\n    <event><string key="concept:name" value="Process"/><date key="time:timestamp" value="2023-01-02T11:00:00"/></event>\n    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-02T12:00:00"/></event>\n  </trace>\n  <trace><string key="concept:name" value="Case3"/>\n    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-03T10:00:00"/></event>\n    <event><string key="concept:name" value="Process"/><date key="time:timestamp" value="2023-01-03T11:00:00"/></event>\n    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-03T12:00:00"/></event>\n  </trace>\n</log>';
/**
 * Parallel XES log with concurrent activities
 * Use for: complex discovery algorithms, parallel mining
 */
export declare const XES_PARALLEL =
  '<?xml version="1.0" encoding="UTF-8"?>\n<log xes.version="1.0" openlog.version="1.0">\n  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>\n  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>\n  <global scope="trace"><string key="concept:name" value="undefined"/></global>\n  <global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global>\n  <trace><string key="concept:name" value="Case1"/>\n    <event><string key="concept:name" value="Start"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event>\n    <event><string key="concept:name" value="TaskA"/><date key="time:timestamp" value="2023-01-01T10:30:00"/></event>\n    <event><string key="concept:name" value="TaskB"/><date key="time:timestamp" value="2023-01-01T10:30:00"/></event>\n    <event><string key="concept:name" value="Merge"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event>\n    <event><string key="concept:name" value="End"/><date key="time:timestamp" value="2023-01-01T12:00:00"/></event>\n  </trace>\n</log>';
/**
 * OCEL JSON minimal object-centric event log (1 object, 2 events)
 * Use for: OCEL-specific tests
 */
export declare const OCEL_MINIMAL =
  '{\n  "event_types": ["Create", "Ship"],\n  "object_types": ["Order"],\n  "events": [\n    {\n      "id": "evt1",\n      "event_type": "Create",\n      "timestamp": "2023-01-01T10:00:00Z",\n      "attributes": {},\n      "object_ids": ["Order-1"],\n      "object_refs": []\n    },\n    {\n      "id": "evt2",\n      "event_type": "Ship",\n      "timestamp": "2023-01-01T11:00:00Z",\n      "attributes": {},\n      "object_ids": ["Order-1"],\n      "object_refs": []\n    }\n  ],\n  "objects": [\n    {\n      "id": "Order-1",\n      "object_type": "Order",\n      "attributes": {},\n      "changes": []\n    }\n  ],\n  "object_relations": []\n}';
/**
 * Multi-trace XES for workflow testing with multiple cases
 * Use for: full workflow tests, integration scenarios
 */
export declare const XES_WORKFLOW =
  '<?xml version="1.0" encoding="UTF-8"?>\n<log xes.version="1.0" xmlns="http://www.xes-standard.org/">\n  <trace>\n    <string key="concept:name" value="Case1"/>\n    <event>\n      <string key="concept:name" value="Activity A"/>\n      <date key="time:timestamp" value="2023-01-01T10:00:00.000+00:00"/>\n    </event>\n    <event>\n      <string key="concept:name" value="Activity B"/>\n      <date key="time:timestamp" value="2023-01-01T10:05:00.000+00:00"/>\n    </event>\n    <event>\n      <string key="concept:name" value="Activity C"/>\n      <date key="time:timestamp" value="2023-01-01T10:10:00.000+00:00"/>\n    </event>\n  </trace>\n  <trace>\n    <string key="concept:name" value="Case2"/>\n    <event>\n      <string key="concept:name" value="Activity A"/>\n      <date key="time:timestamp" value="2023-01-01T11:00:00.000+00:00"/>\n    </event>\n    <event>\n      <string key="concept:name" value="Activity B"/>\n      <date key="time:timestamp" value="2023-01-01T11:05:00.000+00:00"/>\n    </event>\n  </trace>\n</log>';
//# sourceMappingURL=fixtures.d.ts.map
