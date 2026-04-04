#!/usr/bin/env python3
"""
PM4Py Process Mining Analysis for UNRDF OTEL Stack

Usage:
    python3 pm4py-analyze.py [--hours 24] [--limit 1000] [--output report.html]

This script extracts traces from Tempo and logs from Loki,
performs process discovery, conformance checking, and bottleneck analysis.
"""

import argparse
import json
import os
import re
import sys
from datetime import datetime, timedelta, timezone

# Monkey-patch psutil to handle PID 0 in Docker containers
# pm4py/constants.py calls psutil.Process(parent_pid).name() which fails when parent PID is 0
import psutil
_orig_init = psutil.Process.__init__
def _patched_init(self, pid=None, **kwargs):
    try:
        _orig_init(self, pid=pid, **kwargs)
    except psutil.NoSuchProcess:
        self._init(pid=pid)
        self._name = "unknown"
        self._pid = pid or 0
psutil.Process.__init__ = _patched_init

try:
    import requests
    import pandas as pd
    import pm4py
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install requests pandas pm4py")
    sys.exit(1)


def fetch_traces_from_tempo(hours=24, limit=1000):
    """Fetch traces from Tempo and return as DataFrame."""
    tempo_url = os.environ.get("TEMPO_URL", "http://localhost:3200")

    now = datetime.now(timezone.utc)
    end = int(now.timestamp())
    start = int((now - timedelta(hours=hours)).timestamp())

    url = f"{tempo_url}/api/search?start={start}&end={end}&limit={limit}"
    try:
        resp = requests.get(url, timeout=30)
        resp.raise_for_status()
    except requests.RequestException as e:
        print(f"Warning: Could not reach Tempo at {tempo_url}: {e}")
        return pd.DataFrame()

    traces = resp.json().get('traces', [])
    events = []

    for trace in traces:
        trace_id = trace['traceID']

        trace_url = f"{tempo_url}/api/traces/{trace_id}"
        try:
            trace_resp = requests.get(trace_url, timeout=30)
            if trace_resp.status_code != 200:
                continue
        except requests.RequestException:
            continue

        trace_data = trace_resp.json()
        for batch in trace_data.get('batches', []):
            resource_attrs = {}
            for ra in batch.get('resource', {}).get('attributes', []):
                v = ra.get('value', {})
                resource_attrs[ra['key']] = v.get('stringValue') or v.get('intValue') or str(v)

            for scope_span in batch.get('scopeSpans', []):
                for span in scope_span.get('spans', []):
                    attrs = dict(resource_attrs)
                    for a in span.get('attributes', []):
                        v = a.get('value', {})
                        attrs[a['key']] = v.get('stringValue') or v.get('intValue') or str(v)

                    start_ns = int(span.get('startTimeUnixNano', 0))
                    end_ns = int(span.get('endTimeUnixNano', 0))

                    events.append({
                        'case:concept:name': trace_id,
                        'concept:name': span['name'],
                        'time:timestamp': datetime.fromtimestamp(start_ns / 1e9),
                        'org:resource': attrs.get('service.name', 'unknown'),
                        'duration_ms': (end_ns - start_ns) / 1e6 if end_ns > start_ns else 0,
                        'mcp.tool.name': attrs.get('mcp.tool.name', ''),
                        'mcp.tool.success': attrs.get('mcp.tool.success', ''),
                    })

    return pd.DataFrame(events)


def fetch_logs_from_loki(hours=24, limit=1000):
    """Fetch logs from Loki and return as DataFrame."""
    loki_url = os.environ.get("LOKI_URL", "http://localhost:3100")

    now = datetime.now(timezone.utc)
    end = int(now.timestamp())
    start = int((now - timedelta(hours=hours)).timestamp())

    query = '{container=~".*unrdf.*"}'
    url = f"{loki_url}/loki/api/v1/query_range"
    params = {
        'query': query,
        'start': str(start) + '000000000',
        'end': str(end) + '000000000',
        'limit': limit,
        'direction': 'forward',
    }

    try:
        resp = requests.get(url, params=params, timeout=30)
        resp.raise_for_status()
    except requests.RequestException as e:
        print(f"Warning: Could not reach Loki at {loki_url}: {e}")
        return pd.DataFrame()

    data = resp.json()
    events = []

    for result in data.get('data', {}).get('result', []):
        container = result.get('stream', {}).get('container', 'unknown')
        for value in result.get('values', []):
            ts_ns, line = value
            ts = datetime.fromtimestamp(int(ts_ns) / 1e9)

            trace_id = ''
            m = re.search(r'"traceID":"([a-f0-9]+)"', line)
            if m:
                trace_id = m.group(1)

            log_level = 'info'
            if 'error' in line.lower():
                log_level = 'error'
            elif 'warn' in line.lower():
                log_level = 'warning'

            events.append({
                'case:concept:name': trace_id or container,
                'concept:name': 'log_entry',
                'time:timestamp': ts,
                'org:resource': container,
                'log_level': log_level,
                'message': line[:200],
            })

    return pd.DataFrame(events)


def discover_process_model(df):
    """Discover process model using Inductive Miner."""
    if len(df) == 0:
        return None, None, None

    try:
        net, im, fm = pm4py.discover_petri_net_inductive(df)
        return net, im, fm
    except Exception as e:
        print(f"Process discovery failed: {e}")
        return None, None, None


def conformance_check(df, net, im, fm):
    """Run conformance checking via token-based replay."""
    if net is None:
        return None

    try:
        fitness = pm4py.fitness_token_based_replay(df, net, im, fm)
        precision = pm4py.precision_token_based_replay(df, net, im, fm)
        return {'fitness': fitness, 'precision': precision}
    except Exception as e:
        print(f"Conformance checking failed: {e}")
        return None


def bottleneck_analysis(df):
    """Analyze activity durations for bottlenecks."""
    if len(df) == 0 or 'duration_ms' not in df.columns:
        return None

    stats = df.groupby('concept:name')['duration_ms'].agg(
        ['mean', 'median', 'max', 'count', 'std']
    ).sort_values('mean', ascending=False)

    return stats


def main():
    parser = argparse.ArgumentParser(description='PM4Py Process Mining for UNRDF OTEL')
    parser.add_argument('--hours', type=int, default=24, help='Time range in hours')
    parser.add_argument('--limit', type=int, default=1000, help='Max traces to fetch')
    parser.add_argument('--output', type=str, default=None, help='Output JSON file')
    args = parser.parse_args()

    print("=" * 60)
    print("PM4Py Process Mining Analysis for UNRDF OTEL Stack")
    print("=" * 60)
    print(f"  Time range: last {args.hours}h")
    print(f"  Limit: {args.limit} traces")
    print()

    # Step 1: Fetch traces
    print("[1/5] Fetching traces from Tempo...")
    df = fetch_traces_from_tempo(hours=args.hours, limit=args.limit)
    print(f"      Found {len(df)} events from {df['case:concept:name'].nunique() if len(df) > 0 else 0} traces")

    # Step 2: Fetch logs
    print("[2/5] Fetching logs from Loki...")
    log_df = fetch_logs_from_loki(hours=args.hours, limit=args.limit)
    print(f"      Found {len(log_df)} log entries")

    # Step 3: Process discovery
    print("[3/5] Discovering process model...")
    net, im, fm = discover_process_model(df)
    if net:
        print(f"      Places: {len(net.places)}, Transitions: {len(net.transitions)}, Arcs: {len(net.arcs)}")
    else:
        print("      No process model discovered (insufficient traces)")

    # Step 4: Conformance checking
    print("[4/5] Running conformance check...")
    conformance = conformance_check(df, net, im, fm)
    if conformance:
        print(f"      Fitness: {conformance['fitness']}")
        print(f"      Precision: {conformance['precision']}")

    # Step 5: Bottleneck analysis
    print("[5/5] Analyzing bottlenecks...")
    bottlenecks = bottleneck_analysis(df)
    if bottlenecks is not None and len(bottlenecks) > 0:
        print("      Top 5 activities by mean duration:")
        for i, (activity, row) in enumerate(bottlenecks.head(5).iterrows()):
            print(f"        {i+1}. {activity:40s} mean={row['mean']:8.1f}ms  count={int(row['count'])}")

    # Output results
    results = {
        'timestamp': datetime.now(timezone.utc).isoformat(),
        'hours': args.hours,
        'trace_events': len(df),
        'unique_traces': df['case:concept:name'].nunique() if len(df) > 0 else 0,
        'log_entries': len(log_df),
        'process_model': {
            'places': len(net.places) if net else 0,
            'transitions': len(net.transitions) if net else 0,
            'arcs': len(net.arcs) if net else 0,
        },
        'conformance': conformance,
        'bottlenecks': bottlenecks.to_dict() if bottlenecks is not None else None,
    }

    if args.output:
        with open(args.output, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        print(f"\nResults saved to {args.output}")

    print("\n" + "=" * 60)
    print("Analysis complete.")
    print("=" * 60)

    return results


if __name__ == '__main__':
    main()
