#!/usr/bin/env python3
"""
Auto-dashboard generator from UNRDF semantic conventions.

Reads custom-conventions.yaml and generates Grafana dashboard JSON
panels for each attribute group.

Usage:
    python3 scripts/auto-dashboard.py [--input custom-conventions.yaml] [--output dashboards/auto-generated.json]
"""

import argparse
import json
import yaml  # pip install pyyaml
import sys
from pathlib import Path


def load_conventions(path):
    """Load semantic conventions YAML file."""
    with open(path) as f:
        data = yaml.safe_load(f)
    return data


def generate_dashboard(conventions, datasource="Prometheus"):
    """Generate Grafana dashboard from conventions."""
    uid_map = {}

    panels = []
    y_pos = 0

    for group in conventions:
        group_id = group.get('id', 'unknown')
        group_name = group.get('name', group.get('brief', group_id))
        attributes = group.get('attributes', [])

        if not attributes:
            continue

        # Row header
        panels.append({
            "type": "row",
            "title": group_name,
            "gridPos": {"h": 1, "w": 24, "x": 0, "y": y_pos},
            "id": len(panels) + 1,
        })
        y_pos += 1

        col_index = 0
        for attr in attributes:
            attr_id = attr.get('id', '')
            attr_name = attr.get('name', attr.get('brief', attr_id))
            attr_type = attr.get('type', 'string')

            # Normalize type: handle enum types (dict with 'members')
            if isinstance(attr_type, dict):
                attr_type = 'string'

            panel_id = len(panels) + 1
            uid = f"{group_id}_{attr_id}".replace('.', '_')
            uid_map[attr_id] = uid

            # Generate appropriate panel based on type
            if attr_type in ('int', 'float', 'double'):
                # Numeric: time series
                panels.append({
                    "type": "timeseries",
                    "title": attr_name,
                    "gridPos": {"h": 8, "w": 12, "x": col_index * 12, "y": y_pos},
                    "id": panel_id,
                    "uid": uid,
                    "datasource": {"type": "prometheus", "uid": datasource},
                    "targets": [{
                        "expr": f'sum by (service_name) ({{__name__=~".*{attr_id}.*"}})',
                        "legendFormat": "{{{{service_name}}}}",
                        "refId": "A",
                    }],
                    "fieldConfig": {
                        "defaults": {"unit": attr_type == 'int' and 'short' or 'none'},
                        "overrides": [],
                    },
                })
                col_index += 1
                if col_index % 2 == 0:
                    y_pos += 8
                    col_index = 0
            elif attr_type == 'boolean':
                # Boolean: stat panel
                panels.append({
                    "type": "stat",
                    "title": attr_name,
                    "gridPos": {"h": 4, "w": 6, "x": col_index * 6, "y": y_pos},
                    "id": panel_id,
                    "uid": uid,
                    "datasource": {"type": "prometheus", "uid": datasource},
                    "targets": [{
                        "expr": f'sum({{__name__=~".*{attr_id}.*"}})',
                        "refId": "A",
                    }],
                })
                col_index += 1
                if col_index % 4 == 0:
                    y_pos += 4
                    col_index = 0
            else:
                # String: table panel
                panels.append({
                    "type": "table",
                    "title": attr_name,
                    "gridPos": {"h": 8, "w": 12, "x": col_index * 12, "y": y_pos},
                    "id": panel_id,
                    "uid": uid,
                    "datasource": {"type": "prometheus", "uid": datasource},
                    "targets": [{
                        "expr": f'{attr_id}',
                        "format": "table",
                        "instant": True,
                        "refId": "A",
                    }],
                })
                col_index += 1
                if col_index % 2 == 0:
                    y_pos += 8
                    col_index = 0

    dashboard = {
        "uid": "auto-gen-conventions",
        "title": "UNRDF Auto-Generated (Semantic Conventions)",
        "tags": ["auto-generated", "unrdf", "otel"],
        "timezone": "browser",
        "schemaVersion": 39,
        "version": 1,
        "refresh": "30s",
        "panels": panels,
        "templating": {
            "list": [
                {
                    "name": "datasource",
                    "type": "datasource",
                    "query": "prometheus",
                    "current": {"selected": True, "text": "Prometheus", "value": "Prometheus"},
                }
            ]
        },
    }

    return dashboard


def main():
    parser = argparse.ArgumentParser(description='Auto-dashboard generator from UNRDF conventions')
    parser.add_argument('--input', default='custom-conventions.yaml', help='Conventions YAML file')
    parser.add_argument('--output', default=None, help='Output dashboard JSON file')
    parser.add_argument('--datasource', default='Prometheus', help='Grafana datasource UID')
    args = parser.parse_args()

    conventions_path = Path(args.input)
    if not conventions_path.exists():
        print(f"ERROR: Conventions file not found: {conventions_path}")
        sys.exit(1)

    print(f"Loading conventions from {conventions_path}...")
    conventions = load_conventions(conventions_path)

    # Handle both top-level `groups` key and direct list
    if isinstance(conventions, dict):
        if 'groups' in conventions:
            conventions = conventions['groups']
        else:
            conventions = [conventions]
    elif not isinstance(conventions, list):
        print("ERROR: Expected list of attribute groups or dict with 'groups' key")
        sys.exit(1)

    print(f"Found {len(conventions)} attribute groups")

    dashboard = generate_dashboard(conventions, args.datasource)

    output_json = json.dumps(dashboard, indent=2)

    if args.output:
        output_path = Path(args.output)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(output_json)
        print(f"Dashboard written to {output_path}")
        print(f"  Panels: {len(dashboard['panels'])}")
    else:
        print(output_json)


if __name__ == '__main__':
    main()
