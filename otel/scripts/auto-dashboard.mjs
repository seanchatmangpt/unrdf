#!/usr/bin/env node
/**
 * @file Auto Dashboard Generator
 * @module otel/scripts/auto-dashboard
 *
 * Reads Weaver semantic convention registry YAML files from otel/registry/
 * and generates a Grafana dashboard JSON (schema v8) with panels for each
 * instrumented service/domain.
 *
 * Each service gets:
 *   - Request Rate (timeseries)
 *   - Error Rate (timeseries)
 *   - Latency p50/p95/p99 (timeseries)
 *
 * A global stats row is added at the top (total traces, total errors).
 *
 * Usage:
 *   node otel/scripts/auto-dashboard.mjs --output otel/grafana-provisioning/dashboards/auto-generated.json
 *   node otel/scripts/auto-dashboard.mjs                    # writes to stdout
 */

import { readFile, writeFile, readdir, mkdir } from 'node:fs/promises';
import { readFileSync, readdirSync, existsSync } from 'node:fs';
import { resolve, join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse as parseYaml } from 'yaml';

// ---------------------------------------------------------------------------
// Registry parsing
// ---------------------------------------------------------------------------

/**
 * Read all YAML files from the registry directory and extract service domains.
 */
function parseRegistryDir(registryDir) {
  const services = [];
  const files = readdirSync(registryDir);

  for (const file of files) {
    if (!file.endsWith('.yaml') && !file.endsWith('.yml')) continue;
    if (file === 'manifest.yaml') continue;

    const filePath = join(registryDir, file);
    const content = readFileSync(filePath, 'utf-8');
    if (!content) continue;

    const data = parseYaml(content);
    const groups = data?.groups ?? [];

    for (const group of groups) {
      const id = group.id ?? '';
      const displayName = group.display_name ?? id;
      const attrs = group.attributes ?? [];

      // Extract service name from id (e.g. "registry.unrdf.cli" -> "cli")
      const parts = id.split('.');
      const serviceName = parts[parts.length - 1] || id;

      services.push({
        id,
        displayName,
        serviceName,
        attributes: attrs.map(a => a.id ?? a.name ?? ''),
      });
    }
  }

  return services;
}

// ---------------------------------------------------------------------------
// Grafana dashboard generation
// ---------------------------------------------------------------------------

const UID_CHARS = 'abcdefghijklmnopqrstuvwxyz0123456789';

function generateUid(prefix) {
  let uid = prefix;
  for (let i = 0; i < 12 - prefix.length; i++) {
    uid += UID_CHARS[Math.floor(Math.random() * UID_CHARS.length)];
  }
  return uid;
}

function createPanel(id, title, targets, datasource = '${datasource}', type = 'timeseries') {
  return {
    id,
    title,
    type,
    datasource,
    gridPos: { h: 8, w: 12, x: (id % 2 === 1) ? 0 : 12, y: Math.floor((id - 1) / 2) * 8 },
    targets: targets.map(t => ({
      datasource,
      ...t,
    })),
    fieldConfig: {
      defaults: {
        unit: type === 'timeseries' ? 'short' : 'short',
        custom: {
          drawStyle: 'line',
          lineInterpolation: 'smooth',
          fillOpacity: 10,
          pointSize: 5,
          showPoints: 'never',
          legendDisplayMode: 'list',
          lineWidth: 2,
        },
      },
    },
  };
}

function buildDashboard(services) {
  const panels = [];
  let panelId = 1;

  // ---- Global stats row ----
  panels.push({
    id: panelId++,
    title: 'Total Trace Rate',
    type: 'timeseries',
    datasource: '${datasource}',
    gridPos: { h: 8, w: 8, x: 0, y: 0 },
    targets: [
      {
        expr: 'sum(rate(traces_spanmetrics_calls_total[5m]))',
        legendFormat: 'traces/s',
        refId: 'A',
      },
    ],
    fieldConfig: {
      defaults: {
        unit: 'short',
        custom: {
          drawStyle: 'line',
          lineInterpolation: 'smooth',
          fillOpacity: 10,
          showPoints: 'never',
          legendDisplayMode: 'list',
          lineWidth: 2,
        },
      },
    },
  });

  panels.push({
    id: panelId++,
    title: 'Total Error Rate',
    type: 'timeseries',
    datasource: '${datasource}',
    gridPos: { h: 8, w: 8, x: 8, y: 0 },
    targets: [
      {
        expr: 'sum(rate(traces_spanmetrics_calls_total{status_code="STATUS_CODE_ERROR"}[5m]))',
        legendFormat: 'errors/s',
        refId: 'A',
      },
    ],
    fieldConfig: {
      defaults: {
        unit: 'short',
        color: { mode: 'thresholds' },
        thresholds: { mode: 'absolute', steps: [{ color: 'green', value: null }, { color: 'red', value: 1 }] },
        custom: {
          drawStyle: 'line',
          lineInterpolation: 'smooth',
          fillOpacity: 10,
          showPoints: 'never',
          legendDisplayMode: 'list',
          lineWidth: 2,
        },
      },
    },
  });

  panels.push({
    id: panelId++,
    title: 'Total Spans',
    type: 'stat',
    datasource: '${datasource}',
    gridPos: { h: 8, w: 8, x: 16, y: 0 },
    targets: [
      {
        expr: 'sum(traces_spanmetrics_calls_total)',
        legendFormat: 'total',
        refId: 'A',
      },
    ],
    fieldConfig: {
      defaults: {
        unit: 'short',
        color: { mode: 'thresholds' },
        thresholds: { mode: 'absolute', steps: [{ color: 'green', value: null }] },
      },
    },
  });

  // ---- Per-service rows ----
  const yOffset = 8; // after global row

  for (let si = 0; si < services.length; si++) {
    const svc = services[si];
    const rowY = yOffset + si * 24; // 3 panels per service, 8px each

    // Request Rate
    panels.push({
      id: panelId++,
      title: `${svc.displayName} - Request Rate`,
      type: 'timeseries',
      datasource: '${datasource}',
      gridPos: { h: 8, w: 8, x: 0, y: rowY },
      targets: [
        {
          expr: `sum(rate(traces_spanmetrics_calls_total{service_name=~".*${svc.serviceName}.*"}[5m])) by (operation)`,
          legendFormat: '{{operation}}',
          refId: 'A',
        },
      ],
      fieldConfig: {
        defaults: {
          unit: 'short',
          custom: {
            drawStyle: 'line',
            lineInterpolation: 'smooth',
            fillOpacity: 10,
            showPoints: 'never',
            legendDisplayMode: 'list',
            lineWidth: 2,
          },
        },
      },
    });

    // Error Rate
    panels.push({
      id: panelId++,
      title: `${svc.displayName} - Error Rate`,
      type: 'timeseries',
      datasource: '${datasource}',
      gridPos: { h: 8, w: 8, x: 8, y: rowY },
      targets: [
        {
          expr: `sum(rate(traces_spanmetrics_calls_total{service_name=~".*${svc.serviceName}.*", status_code="STATUS_CODE_ERROR"}[5m])) by (operation)`,
          legendFormat: '{{operation}}',
          refId: 'A',
        },
      ],
      fieldConfig: {
        defaults: {
          unit: 'short',
          color: { mode: 'thresholds' },
          thresholds: { mode: 'absolute', steps: [{ color: 'green', value: null }, { color: 'red', value: 0.1 }] },
          custom: {
            drawStyle: 'line',
            lineInterpolation: 'smooth',
            fillOpacity: 10,
            showPoints: 'never',
            legendDisplayMode: 'list',
            lineWidth: 2,
          },
        },
      },
    });

    // Latency percentiles
    panels.push({
      id: panelId++,
      title: `${svc.displayName} - Latency (p50/p95/p99)`,
      type: 'timeseries',
      datasource: '${datasource}',
      gridPos: { h: 8, w: 8, x: 16, y: rowY },
      targets: [
        {
          expr: `histogram_quantile(0.50, sum(rate(traces_spanmetrics_duration_bucket{service_name=~".*${svc.serviceName}.*"}[5m])) by (le))`,
          legendFormat: 'p50',
          refId: 'A',
        },
        {
          expr: `histogram_quantile(0.95, sum(rate(traces_spanmetrics_duration_bucket{service_name=~".*${svc.serviceName}.*"}[5m])) by (le))`,
          legendFormat: 'p95',
          refId: 'B',
        },
        {
          expr: `histogram_quantile(0.99, sum(rate(traces_spanmetrics_duration_bucket{service_name=~".*${svc.serviceName}.*"}[5m])) by (le))`,
          legendFormat: 'p99',
          refId: 'C',
        },
      ],
      fieldConfig: {
        defaults: {
          unit: 's',
          custom: {
            drawStyle: 'line',
            lineInterpolation: 'smooth',
            fillOpacity: 10,
            showPoints: 'never',
            legendDisplayMode: 'list',
            lineWidth: 2,
          },
        },
      },
    });
  }

  const dashboard = {
    __inputs: [
      {
        name: 'DS_PROMETHEUS',
        label: 'Prometheus',
        description: '',
        type: 'datasource',
        pluginId: 'prometheus',
        pluginName: 'Prometheus',
      },
    ],
    __requires: [
      { type: 'grafana', id: 'grafana', name: 'Grafana', version: '10.0.0' },
      { type: 'datasource', id: 'prometheus', name: 'Prometheus', version: '1.0.0' },
      { type: 'panel', id: 'timeseries', name: 'Time series', version: '' },
      { type: 'panel', id: 'stat', name: 'Stat', version: '' },
    ],
    annotations: { list: [] },
    editable: true,
    fiscalYearStartMonth: 0,
    graphTooltip: 1,
    id: null,
    links: [],
    liveNow: false,
    panels,
    refresh: '10s',
    schemaVersion: 39,
    tags: ['unrdf', 'auto-generated', 'weaver'],
    templating: {
      list: [
        {
          current: {},
          hide: 0,
          includeAll: false,
          label: 'Data Source',
          multi: false,
          name: 'datasource',
          options: [],
          query: 'prometheus',
          queryValue: '',
          refresh: 1,
          regex: '',
          skipUrlSync: false,
          type: 'datasource',
        },
      ],
    },
    time: { from: 'now-1h', to: 'now' },
    timepicker: {},
    timezone: 'browser',
    title: 'UNRDF Auto-Generated Dashboard',
    uid: generateUid('unrdf-auto'),
    version: 1,
  };

  return dashboard;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const args = process.argv.slice(2);
  let output = null;

  const outIdx = args.indexOf('--output');
  if (outIdx !== -1 && args[outIdx + 1]) {
    output = resolve(args[outIdx + 1]);
  }

  const scriptDir = dirname(fileURLToPath(import.meta.url));
  const projectRoot = resolve(scriptDir, '..', '..');
  const registryDir = resolve(projectRoot, 'otel', 'registry');

  const services = parseRegistryDir(registryDir);

  if (services.length === 0) {
    console.error('Warning: no services found in registry. Generating dashboard with global panels only.');
  }

  const dashboard = buildDashboard(services);
  const json = JSON.stringify(dashboard, null, 2);

  if (output) {
    await mkdir(dirname(output), { recursive: true });
    await writeFile(output, json, 'utf-8');
    console.log(`Dashboard written to ${output} (${services.length} services, ${dashboard.panels.length} panels)`);
  } else {
    console.log(json);
  }
}

main().catch(err => {
  console.error(`Fatal: ${err.message}`);
  process.exit(2);
});
