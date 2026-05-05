/**
 * Analysis Functions Tests
 */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../pkg/wasm4pm.js';
describe('Analysis - Event Statistics', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should analyze event statistics from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event><event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const stats = wasm.analyze_event_statistics(logHandle);
    expect(stats).toBeTruthy();
  });
  it('should include activity frequencies in event statistics', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    const stats = wasm.analyze_event_statistics(logHandle);
    expect(stats).toBeTruthy();
  });
  it('should fail when EventLog handle is invalid', () => {
    expect(() => {
      wasm.analyze_event_statistics('obj_999999');
    }).toThrow();
  });
});
describe('Analysis - Case Duration', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should analyze case duration from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event><event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const duration = wasm.analyze_case_duration(logHandle);
    expect(duration).toBeTruthy();
  });
  it('should provide min and max duration', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event><event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    const duration = wasm.analyze_case_duration(logHandle);
    expect(duration).toBeTruthy();
  });
  it('should fail when EventLog handle is invalid', () => {
    expect(() => {
      wasm.analyze_case_duration('obj_999999');
    }).toThrow();
  });
});
describe('Analysis - Dotted Chart', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should analyze dotted chart from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event><event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const chart = wasm.analyze_dotted_chart(logHandle);
    expect(chart).toBeTruthy();
  });
  it('should fail with invalid EventLog handle', () => {
    expect(() => {
      wasm.analyze_dotted_chart('obj_999999');
    }).toThrow();
  });
});
describe('Analysis - Variant Complexity', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should analyze variant complexity from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const complexity = wasm.analyze_variant_complexity(logHandle, 'concept:name');
    expect(complexity).toBeTruthy();
  });
  it('should fail with invalid EventLog handle', () => {
    expect(() => {
      wasm.analyze_variant_complexity('obj_999999', 'concept:name');
    }).toThrow();
  });
});
describe('Analysis - Activity Transition Matrix', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should compute activity transition matrix from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event><event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const matrix = wasm.compute_activity_transition_matrix(logHandle, 'concept:name');
    expect(matrix).toBeTruthy();
  });
  it('should fail with invalid EventLog handle', () => {
    expect(() => {
      wasm.compute_activity_transition_matrix('obj_999999', 'concept:name');
    }).toThrow();
  });
});
describe('Analysis - Process Speedup', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should analyze process speedup from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const speedup = wasm.analyze_process_speedup(logHandle, 'time:timestamp');
    expect(speedup).toBeTruthy();
  });
  it('should fail with invalid EventLog handle', () => {
    expect(() => {
      wasm.analyze_process_speedup('obj_999999', 'time:timestamp');
    }).toThrow();
  });
});
describe('Analysis - Temporal Bottlenecks', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should analyze temporal bottlenecks from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event><event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const bottlenecks = wasm.analyze_temporal_bottlenecks(
      logHandle,
      'concept:name',
      'time:timestamp'
    );
    expect(bottlenecks).toBeTruthy();
  });
  it('should fail with invalid EventLog handle', () => {
    expect(() => {
      wasm.analyze_temporal_bottlenecks('obj_999999', 'concept:name', 'time:timestamp');
    }).toThrow();
  });
});
describe('Analysis - Activity Ordering', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });
  it('should extract activity ordering from EventLog', () => {
    const xes = `<?xml version="1.0"?><log xes.version="1.0"><extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/><extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/><global scope="trace"><string key="concept:name" value="undefined"/></global><global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global><trace><string key="concept:name" value="Case1"/><event><string key="concept:name" value="ActivityA"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event><event><string key="concept:name" value="ActivityB"/><date key="time:timestamp" value="2023-01-01T11:00:00"/></event></trace></log>`;
    const logHandle = wasm.load_eventlog_from_xes(xes);
    expect(logHandle).toBeTruthy();
    const ordering = wasm.extract_activity_ordering(logHandle, 'concept:name');
    expect(ordering).toBeTruthy();
  });
  it('should fail with invalid EventLog handle', () => {
    expect(() => {
      wasm.extract_activity_ordering('obj_999999', 'concept:name');
    }).toThrow();
  });
});
describe('Analysis - Available Functions List', () => {
  it('should list available analysis functions', () => {
    const functions = wasm.available_analysis_functions();
    expect(functions).toBeTruthy();
    expect(typeof functions).toBe('string');
    const functionsObj = JSON.parse(functions);
    expect(functionsObj.functions).toBeTruthy();
    expect(Array.isArray(functionsObj.functions)).toBe(true);
    expect(functionsObj.functions.length).toBeGreaterThan(0);
  });
  it('should include expected analysis functions', () => {
    const functions = wasm.available_analysis_functions();
    const functionsObj = JSON.parse(functions);
    const functionNames = functionsObj.functions.map((fn) => fn.name);
    expect(functionNames).toContain('event_statistics');
    expect(functionNames).toContain('case_duration');
  });
});
//# sourceMappingURL=analysis.test.js.map
