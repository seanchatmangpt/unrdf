#!/usr/bin/env python3
"""
Synthetic trace generator for UNRDF OTEL stack.

Generates predictable trace patterns that can be compared against baselines
to detect regressions in service behavior.

Usage:
    python3 synthetic-trace-gen.py [--endpoint http://localhost:4318] [--interval 60] [--operations 5]

This generates traces with known latency distributions per operation.
A regression is detected when the actual latency distribution shifts
significantly from the baseline.
"""

import argparse
import json
import os
import random
import time
import urllib.request
import urllib.error
from datetime import datetime, timezone

# Operation definitions with expected latency bounds (ms)
OPERATIONS = {
    'mcp.query':      {'mean': 50,  'std': 15,  'expected_max': 200},
    'mcp.graph_load': {'mean': 120, 'std': 40,  'expected_max': 500},
    'mcp.hooks_exec': {'mean': 80,  'std': 25,  'expected_max': 300},
    'daemon.schedule': {'mean': 30, 'std': 10,  'expected_max': 100},
    'daemon.health':   {'mean': 10, 'std': 5,   'expected_max': 50},
}


def generate_trace(operation_name, trace_id_hex):
    """Generate a single OTLP trace export request body."""
    now_ns = int(datetime.now(timezone.utc).timestamp() * 1e9)
    op = OPERATIONS.get(operation_name, {'mean': 50, 'std': 20, 'expected_max': 200})

    # Simulate latency from expected distribution
    latency_ms = max(1, random.gauss(op['mean'], op['std']))
    duration_ns = int(latency_ms * 1e6)

    # Determine success/failure based on expected_max
    success = latency_ms <= op['expected_max']

    body = {
        "resourceSpans": [{
            "resource": {
                "attributes": [
                    {"key": "service.name", "value": {"stringValue": "synthetic-probe"}},
                    {"key": "service.version", "value": {"stringValue": "1.0.0"}},
                    {"key": "deployment.environment", "value": {"stringValue": "synthetic"}},
                ]
            },
            "scopeSpans": [{
                "scope": {"name": "synthetic-trace-gen"},
                "spans": [{
                    "traceId": trace_id_hex,
                    "spanId": os.urandom(8).hex(),
                    "parentSpanId": "",
                    "name": operation_name,
                    "kind": 1,  # SPAN_KIND_INTERNAL
                    "startTimeUnixNano": str(now_ns),
                    "endTimeUnixNano": str(now_ns + duration_ns),
                    "attributes": [
                        {"key": "synthetic", "value": {"boolValue": True}},
                        {"key": "mcp.tool.name", "value": {"stringValue": operation_name}},
                        {"key": "mcp.tool.success", "value": {"boolValue": success}},
                        {"key": "expected_max_ms", "value": {"intValue": str(op['expected_max'])}},
                    ],
                    "status": {} if success else {"code": 2, "message": f"latency {latency_ms:.0f}ms exceeded expected_max {op['expected_max']}ms"},
                }]
            }]
        }]
    }
    return body


def send_trace(endpoint, trace_id_hex, operation_name):
    """Send a trace to the OTLP endpoint."""
    body = generate_trace(operation_name, trace_id_hex)
    data = json.dumps(body).encode('utf-8')

    req = urllib.request.Request(
        f"{endpoint}/v1/traces",
        data=data,
        headers={'Content-Type': 'application/json'},
        method='POST',
    )

    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            return resp.status == 200
    except urllib.error.URLError as e:
        print(f"  Failed to send trace: {e}")
        return False


def generate_trace_id():
    """Generate a random 32-char hex trace ID."""
    return os.urandom(16).hex()


def run_once(endpoint, operations):
    """Generate and send one batch of synthetic traces."""
    timestamp = datetime.now(timezone.utc).isoformat()
    print(f"[{timestamp}] Generating {len(operations)} synthetic traces...")

    success_count = 0
    for op_name in operations:
        trace_id = generate_trace_id()
        if send_trace(endpoint, trace_id, op_name):
            success_count += 1
            # Small delay to avoid overwhelming collector
            time.sleep(0.1)

    print(f"  Sent {success_count}/{len(operations)} traces")
    return success_count


def main():
    parser = argparse.ArgumentParser(description='Synthetic trace generator for UNRDF OTEL')
    parser.add_argument('--endpoint', default='http://localhost:4318', help='OTLP HTTP endpoint')
    parser.add_argument('--interval', type=int, default=0, help='Interval between batches in seconds (0=once)')
    parser.add_argument('--operations', nargs='+', default=list(OPERATIONS.keys()), help='Operations to generate traces for')
    parser.add_argument('--count', type=int, default=1, help='Number of batches to run (0=forever)')
    args = parser.parse_args()

    print("Synthetic Trace Generator for UNRDF OTEL Stack")
    print(f"  Endpoint: {args.endpoint}")
    print(f"  Operations: {args.operations}")
    print()

    batches = 0
    while True:
        run_once(args.endpoint, args.operations)
        batches += 1

        if args.count > 0 and batches >= args.count:
            break

        if args.interval > 0:
            time.sleep(args.interval)
        else:
            break

    print(f"\nCompleted {batches} batches")


if __name__ == '__main__':
    main()
