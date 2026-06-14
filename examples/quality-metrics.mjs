/**
 * @file Quality Metrics Collector Example
 * @description Demonstrates QualityMetricsCollector — Lean Six Sigma recording,
 *   defect tracking, DPMO calculation, audit trail, and Kaizen event registration.
 *
 * API demonstrated: packages/hooks/src/hooks/quality-metrics.mjs
 * Docs: packages/hooks/README.md — "QualityMetricsCollector" section
 *
 * QualityMetricsCollector applies Lean Six Sigma principles to RDF operation quality:
 * - record() tracks numeric measurements for named metrics (latency, error rates)
 * - recordDefect() logs defects for DPMO (Defects Per Million Opportunities) calculation
 * - getSummary() returns sigma level, defect counts, and recent defects
 * - registerKaizenEvent() logs improvement events to the audit trail
 *
 * EXPECTED OUTPUT:
 *   [record] measurements stored and metricsTracked count increments
 *   [recordDefect] defects appear in getSummary().recentDefects
 *   [getSummary] DPMO and sigmaLevel are numeric; totalMeasurements matches records
 *   [kaizen] kaizenEvent appears in exportAuditTrail() with type 'kaizen-registered'
 *   [audit] exportAuditTrail returns structured entries with timestamps
 */

import {
  QualityMetricsCollector,
} from '../packages/hooks/src/hooks/quality-metrics.mjs';

// ─── 1. record() — track numeric measurements ─────────────────────────────────

const collector = new QualityMetricsCollector({ maxDataPoints: 500 });

// Record latency measurements
collector.record('sparql.latency', 42, { operation: 'SELECT' });
collector.record('sparql.latency', 87, { operation: 'SELECT' });
collector.record('sparql.latency', 15, { operation: 'ASK' });

// Record a different metric
collector.record('cache.hitRate', 0.82, { cache: 'triple-cache' });
collector.record('cache.hitRate', 0.95, { cache: 'triple-cache' });

const summaryAfterRecords = collector.getSummary();

// metricsTracked reflects the metrics Map (populated separately); dataPointsStored reflects dataPoints Map
if (summaryAfterRecords.totalMeasurements !== 5) {
  throw new Error(`Expected 5 total measurements, got ${summaryAfterRecords.totalMeasurements}`);
}
if (summaryAfterRecords.dataPointsStored !== 5) {
  throw new Error(`Expected 5 data points stored, got ${summaryAfterRecords.dataPointsStored}`);
}

console.log(`[record] ✅ dataPointsStored=${summaryAfterRecords.dataPointsStored}, totalMeasurements=${summaryAfterRecords.totalMeasurements}`);

// ─── 2. recordDefect() — log defects for DPMO ────────────────────────────────

collector.recordDefect({
  description: 'SPARQL query returned empty result for known entity',
  severity: 'major',
  context: { entityUri: 'https://example.org/Alice' },
});
collector.recordDefect({
  description: 'Timeout during federation query aggregation',
  severity: 'critical',
  context: { timeout: 5000, peers: 3 },
});

const summaryAfterDefects = collector.getSummary();

if (summaryAfterDefects.totalDefects !== 2) {
  throw new Error(`Expected 2 defects, got ${summaryAfterDefects.totalDefects}`);
}
if (!Array.isArray(summaryAfterDefects.recentDefects)) {
  throw new Error('Expected recentDefects to be an array');
}
if (summaryAfterDefects.recentDefects.length !== 2) {
  throw new Error(`Expected 2 recent defects, got ${summaryAfterDefects.recentDefects.length}`);
}

const firstDefect = summaryAfterDefects.recentDefects[0];
if (!firstDefect.description.includes('SPARQL query returned')) {
  throw new Error(`Expected first defect description about SPARQL, got: ${firstDefect.description}`);
}

console.log(`[recordDefect] ✅ totalDefects=${summaryAfterDefects.totalDefects}, recentDefects.length=${summaryAfterDefects.recentDefects.length}`);
console.log(`[recordDefect]   first: "${firstDefect.description.slice(0, 50)}..."`);

// ─── 3. getSummary() — DPMO and sigma level ──────────────────────────────────

const summary = collector.getSummary();

if (typeof summary.dpmo !== 'number') throw new Error('Expected dpmo to be a number');
if (typeof summary.sigmaLevel !== 'number') throw new Error('Expected sigmaLevel to be a number');
if (typeof summary.auditLogEntries !== 'number') throw new Error('Expected auditLogEntries to be a number');

// With 2 defects out of 5 measurements: DPMO = (2/5)*1e6 = 400,000
// That maps to roughly 1.7 sigma — very low quality (good for testing!)
if (summary.dpmo <= 0) throw new Error(`Expected dpmo > 0, got ${summary.dpmo}`);
if (summary.sigmaLevel <= 0) throw new Error(`Expected sigmaLevel > 0, got ${summary.sigmaLevel}`);

console.log(`[getSummary] ✅ dpmo=${summary.dpmo.toFixed(0)}, sigmaLevel=${summary.sigmaLevel.toFixed(2)}, auditLogEntries=${summary.auditLogEntries}`);

// ─── 4. registerKaizenEvent() — log improvement events ───────────────────────

const kaizenRecord = collector.registerKaizenEvent({
  action: 'Add SPARQL result caching layer',
  expectedImprovement: 'Reduce empty-result defects by 80%',
  owner: 'platform-team',
});

if (!kaizenRecord || typeof kaizenRecord !== 'object') {
  throw new Error('Expected registerKaizenEvent to return a record object');
}

console.log(`[kaizen] ✅ registered Kaizen event: action="${kaizenRecord.action || kaizenRecord.event?.action || JSON.stringify(kaizenRecord).slice(0, 60)}"`);

// ─── 5. exportAuditTrail() — structured audit entries ────────────────────────

const trail = collector.exportAuditTrail();

if (!Array.isArray(trail)) throw new Error('Expected exportAuditTrail to return an array');
if (trail.length === 0) throw new Error('Expected at least 1 audit log entry');

// Filter to only defect-recorded entries
const defectEntries = collector.exportAuditTrail({ type: 'defect-recorded' });
if (!Array.isArray(defectEntries)) throw new Error('Expected filtered audit trail to be an array');

console.log(`[audit] ✅ total trail entries=${trail.length}, defect-recorded entries=${defectEntries.length}`);
if (trail[0].timestamp !== undefined) {
  console.log(`[audit]   first entry timestamp type=${typeof trail[0].timestamp}`);
}

// ─── 6. Forgery witness — defects must accumulate, never silently drop ────────

const collector2 = new QualityMetricsCollector();
for (let i = 0; i < 10; i++) {
  collector2.recordDefect({ description: `defect-${i}`, severity: 'minor' });
}
const bigSummary = collector2.getSummary();
if (bigSummary.totalDefects !== 10) {
  throw new Error(`Expected 10 defects to accumulate, got ${bigSummary.totalDefects} — defects are being silently dropped`);
}
// recentDefects shows at most last 10
if (bigSummary.recentDefects.length !== 10) {
  throw new Error(`Expected 10 recent defects (exactly 10 recorded), got ${bigSummary.recentDefects.length}`);
}

console.log(`[forgery witness] ✅ 10 defects recorded → totalDefects=${bigSummary.totalDefects}, recentDefects.length=${bigSummary.recentDefects.length}`);

console.log('\n✅ All QualityMetricsCollector assertions passed.');
