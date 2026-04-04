#!/usr/bin/env python3
"""
PM4Py Process Mining Analysis for UNRDF OTEL Stack

Usage:
    python3 pm4py-analyze.py [--hours 24] [--limit 1000] [--output report.html]
    python3 pm4py-analyze.py --mode full     # All analyses
    python3 pm4py-analyze.py --mode predict  # Predictive monitoring only
    python3 pm4py-analyze.py --mode ocel     # Object-centric mining only
    python3 pm4py-analyze.py --mode drift    # Process drift detection only

This script extracts traces from Tempo and logs from Loki,
performs process discovery, conformance checking, bottleneck analysis,
predictive monitoring, object-centric mining, alignment-based conformance,
variant analysis, stochastic mining, anomaly detection, and drift detection.
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
    import numpy as np
    import pm4py
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install requests pandas numpy pm4py scikit-learn")
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


def alignment_conformance_check(df, net, im, fm):
    """Run alignment-based conformance checking for per-trace deviation diagnosis."""
    if net is None or len(df) == 0:
        return None

    try:
        aligned_traces = pm4py.conformance_diagnostics_alignments(df, net, im, fm)
        if not aligned_traces:
            return None

        # Aggregate alignment results
        fitnesses = [a['fitness'] for a in aligned_traces if 'fitness' in a]
        costs = [a.get('cost', 0) for a in aligned_traces]

        # Find deviating traces (fitness < 1.0)
        deviating = [
            {
                'trace': a.get('trace', ''),
                'fitness': a.get('fitness', 0),
                'cost': a.get('cost', 0),
            }
            for a in aligned_traces
            if a.get('fitness', 1.0) < 1.0
        ]

        return {
            'mean_fitness': float(np.mean(fitnesses)) if fitnesses else 0,
            'min_fitness': float(np.min(fitnesses)) if fitnesses else 0,
            'deviating_traces': len(deviating),
            'total_traces': len(aligned_traces),
            'top_deviations': deviating[:5],  # Top 5 most deviating
        }
    except Exception as e:
        print(f"Alignment conformance checking failed: {e}")
        return None


def bottleneck_analysis(df):
    """Analyze activity durations for bottlenecks."""
    if len(df) == 0 or 'duration_ms' not in df.columns:
        return None

    stats = df.groupby('concept:name')['duration_ms'].agg(
        ['mean', 'median', 'max', 'count', 'std']
    ).sort_values('mean', ascending=False)

    return stats


def variant_analysis(df):
    """Analyze trace variants — the most common execution patterns."""
    if len(df) == 0:
        return None

    try:
        variants = pm4py.get_variants(df)
        # Sort by count (descending)
        sorted_variants = sorted(variants.items(), key=lambda x: x[1], reverse=True)

        return {
            'total_variants': len(sorted_variants),
            'total_traces': sum(v[1] for v in sorted_variants),
            'top_variants': [
                {
                    'pattern': ' → '.join(v[0]),
                    'count': v[1],
                    'percentage': round(100 * v[1] / sum(vv[1] for vv in sorted_variants), 1),
                }
                for v in sorted_variants[:10]
            ],
        }
    except Exception as e:
        print(f"Variant analysis failed: {e}")
        return None


def predictive_monitoring(df):
    """Predict remaining time and next activity for running traces."""
    if len(df) == 0:
        return None

    results = {}

    # Case duration prediction (proxy for remaining time)
    try:
        durations = pm4py.get_all_case_durations(df)
        if durations and len(durations) > 0:
            results['case_duration'] = {
                'mean_ms': float(np.mean(durations)),
                'median_ms': float(np.median(durations)),
                'max_ms': float(np.max(durations)),
                'min_ms': float(np.min(durations)),
            }
    except Exception as e:
        print(f"  Case duration prediction failed: {e}")

    # Prefix-based replay (enables next activity prediction)
    try:
        prefixes = pm4py.get_prefixes_from_log(df, length=2)
        if prefixes is not None and len(prefixes) > 0:
            # Replay prefixes against discovered model for fitness
            net, im, fm = pm4py.discover_petri_net_inductive(df)
            tbr_result = pm4py.replay_prefix_tbr(prefixes, net, im, fm)
            if tbr_result is not None:
                fitnesses = []
                for trace_fitness in tbr_result:
                    if isinstance(trace_fitness, dict) and 'average_trace_fitness' in trace_fitness:
                        fitnesses.append(trace_fitness['average_trace_fitness'])
                    elif isinstance(trace_fitness, (int, float)):
                        fitnesses.append(float(trace_fitness))
                if fitnesses:
                    unfit = sum(1 for f in fitnesses if f < 1.0)
                    results['prefix_replay'] = {
                        'total_prefixes': len(fitnesses),
                        'unfit_prefixes': unfit,
                        'mean_fitness': float(np.mean(fitnesses)),
                    }
    except Exception as e:
        print(f"  Prefix replay failed: {e}")

    return results if results else None


def object_centric_mining(df):
    """Convert traces to OCEL format and discover object-centric Petri net."""
    if len(df) == 0:
        return None

    try:
        # Convert to OCEL — services and MCP tools as object types
        ocel = pm4py.convert_log_to_ocel(
            df,
            object_types=['org:resource', 'mcp.tool.name'],
            activity_column='concept:name',
            timestamp_column='time:timestamp',
        )

        # Discover object-centric Petri net
        ocpn = pm4py.discover_oc_petri_net(ocel)

        # Get OCEL statistics
        obj_types = pm4py.ocel_get_object_types(ocel)
        ocel_df = pm4py.ocel_flattening(ocel, obj_types[0]) if obj_types else pd.DataFrame()

        return {
            'ocel_events': len(ocel_df),
            'object_types': list(ocel.object_types) if hasattr(ocel, 'object_types') else [],
            'ocpn_places': len(ocpn.places) if hasattr(ocpn, 'places') else 0,
            'ocpn_transitions': len(ocpn.transitions) if hasattr(ocpn, 'transitions') else 0,
        }
    except Exception as e:
        print(f"  Object-centric mining failed: {e}")
        return None


def anomaly_detection(df):
    """ML-based trace anomaly detection using feature extraction and AutoML (TPOT2)."""
    if len(df) == 0:
        return None

    try:
        from sklearn.ensemble import IsolationForest
        from sklearn.preprocessing import StandardScaler
    except ImportError:
        print("  Anomaly detection requires scikit-learn: pip install scikit-learn")
        return None

    try:
        # Feature extraction per trace
        case_features = df.groupby('case:concept:name').agg(
            span_count=('concept:name', 'count'),
            unique_services=('org:resource', 'nunique'),
            total_duration_ms=('duration_ms', 'sum'),
            mean_duration_ms=('duration_ms', 'mean'),
            max_duration_ms=('duration_ms', 'max'),
            std_duration_ms=('duration_ms', 'std'),
            has_mcp_tool=('mcp.tool.name', lambda x: int((x.str.strip() != '').sum())),
        ).fillna(0)

        # Scale features
        scaler = StandardScaler()
        features_scaled = scaler.fit_transform(case_features)

        # Label anomalies: traces with duration > 2 std from mean are anomalous
        mean_dur = case_features['total_duration_ms'].mean()
        std_dur = case_features['total_duration_ms'].std()
        case_features['is_anomaly_label'] = (
            (case_features['total_duration_ms'] > mean_dur + 2 * std_dur) |
            (case_features['total_duration_ms'] < mean_dur - 2 * std_dur)
        ).astype(int)

        # Isolation Forest (fast baseline)
        clf = IsolationForest(contamination=0.1, random_state=42, n_estimators=100)
        predictions = clf.fit_predict(features_scaled)
        case_features['anomaly_score'] = clf.decision_function(features_scaled)
        case_features['is_anomaly'] = predictions

        # Identify anomalies
        anomalies = case_features[case_features['is_anomaly'] == -1]

        result = {
            'total_cases': len(case_features),
            'anomalous_cases': len(anomalies),
            'anomaly_rate': round(100 * len(anomalies) / len(case_features), 1),
            'top_anomalies': anomalies.nlargest(5, 'span_count').index.tolist()[:5],
            'method': 'isolation_forest',
        }

        # TPOT2 AutoML for classification (if enough samples)
        try:
            import tpot2

            n_anomalous = case_features['is_anomaly_label'].sum()
            n_normal = len(case_features) - n_anomalous

            if n_anomalous >= 3 and n_normal >= 5 and len(case_features) >= 20:
                print("    Running TPOT2 AutoML pipeline optimization (max 60s)...")

                from tpot2 import TPOTClassifier

                X_train = features_scaled
                y_train = case_features['is_anomaly_label'].values

                # Run TPOT2 with a short time budget
                tpot = TPOTClassifier(
                    generations=5,
                    population_size=20,
                    cv=3,
                    scoring='f1',
                    verbosity=0,
                    random_state=42,
                    max_time_mins=1,
                    n_jobs=-1,
                )
                tpot.fit(X_train, y_train)

                # Get the best pipeline
                best_pipeline = str(tpot.fitted_pipeline_)
                best_score = tpot.score(X_train, y_train)

                result['automl'] = {
                    'method': 'tpot2',
                    'best_pipeline': best_pipeline[:200],
                    'best_f1_score': round(float(best_score), 4),
                    'generations': 5,
                }

                print(f"    TPOT2 best F1: {best_score:.4f} — pipeline: {best_pipeline[:100]}...")
            else:
                print(f"    TPOT2 skipped: need more samples (have {len(case_features)}, anomalous={n_anomalous})")
        except ImportError:
            print("    TPOT2 not installed: pip install tpot2")
        except Exception as e:
            print(f"    TPOT2 AutoML failed: {e}")

        return result
    except Exception as e:
        print(f"  Anomaly detection failed: {e}")
        return None


def temporal_profile_conformance(df):
    """Discover temporal profile and check temporal conformance (SLA at process level).

    Conformance output format: List[List[Tuple[act1, act2, observed_time, zeta_score]]]
    Each tuple: (activity_name_1, activity_name_2, observed_seconds, zeta_score)
    """
    if len(df) == 0:
        return None

    try:
        # Discover temporal profile (expected time between activity pairs)
        temporal_profile = pm4py.discover_temporal_profile(df)

        # Check conformance against temporal profile
        conformance = pm4py.conformance_temporal_profile(df, temporal_profile)

        # Aggregate deviations
        total_checks = 0
        total_deviations = 0
        worst_deviations = []

        for trace_conformance in conformance:
            for check in trace_conformance:
                total_checks += 1
                if isinstance(check, (list, tuple)) and len(check) >= 4:
                    # Format: (act1_name, act2_name, observed_seconds, zeta_score)
                    act1 = str(check[0])
                    act2 = str(check[1])
                    observed = float(check[2])
                    zeta_score = float(check[3])

                    # A deviation occurs when zeta_score > threshold (typically > 2.0)
                    if zeta_score > 2.0:
                        total_deviations += 1
                        if len(worst_deviations) < 5:
                            worst_deviations.append({
                                'pair': f"{act1} → {act2}",
                                'observed_s': round(observed, 3),
                                'zeta_score': round(zeta_score, 2),
                            })

        # Find the most variable activity pairs from the temporal profile
        violated_pairs = []
        for (act1, act2), (mean_t, std_t) in sorted(
            temporal_profile.items(),
            key=lambda x: x[1][1],  # sort by std (most variable first)
            reverse=True
        )[:5]:
            violated_pairs.append({
                'pair': f"{act1} → {act2}",
                'mean_ms': round(float(mean_t) * 1000, 1),
                'std_ms': round(float(std_t) * 1000, 1),
            })

        return {
            'temporal_profile_pairs': len(temporal_profile),
            'total_checks': total_checks,
            'significant_deviations': total_deviations,
            'deviation_rate': round(100 * total_deviations / max(total_checks, 1), 1),
            'most_variable_pairs': violated_pairs,
            'worst_deviations': worst_deviations,
        }
    except Exception as e:
        print(f"  Temporal profile conformance failed: {e}")
        return None


def rework_detection(df):
    """Detect rework patterns — activities that occur multiple times in the same trace."""
    if len(df) == 0:
        return None

    try:
        # Get rework statistics per activity
        rework_cases = pm4py.get_rework_cases_per_activity(df)

        # Sort by rework count (descending)
        sorted_rework = sorted(rework_cases.items(), key=lambda x: x[1], reverse=True)

        # Filter rework traces for top reworked activities
        rework_details = []
        for activity, count in sorted_rework[:5]:
            if count > 0:
                # Filter traces that contain rework of this activity
                rework_df = pm4py.filter_activities_rework(df, activity, min_occurrences=2)
                rework_trace_count = rework_df['case:concept:name'].nunique() if len(rework_df) > 0 else 0
                rework_details.append({
                    'activity': activity,
                    'rework_cases': count,
                    'rework_traces': rework_trace_count,
                    'rework_rate': round(100 * rework_trace_count / max(df['case:concept:name'].nunique(), 1), 1),
                })

        return {
            'total_rework_activities': sum(1 for _, c in sorted_rework if c > 0),
            'total_rework_cases': sum(c for _, c in sorted_rework),
            'top_rework': rework_details,
        }
    except Exception as e:
        print(f"  Rework detection failed: {e}")
        return None


def batch_detection(df):
    """Detect batch processing patterns that explain tail latency."""
    if len(df) == 0:
        return None

    try:
        # Discover batches with large merge_distance for distributed traces
        # Try multiple merge distances to find meaningful batches
        batches = pm4py.discover_batches(df, merge_distance=30000, min_batch_size=2)

        if not batches:
            # Try activity-level grouping as fallback
            from collections import Counter
            activity_starts = df.groupby(['concept:name', 'case:concept:name']).agg(
                first=('time:timestamp', 'min')
            ).reset_index()

            # Check if multiple traces start the same activity within 30s windows
            activity_counts = Counter()
            for activity in activity_starts['concept:name'].unique():
                times = activity_starts[activity_starts['concept:name'] == activity]['first'].sort_values()
                for i in range(len(times) - 1):
                    if (times.iloc[i + 1] - times.iloc[i]).total_seconds() < 30:
                        activity_counts[activity] += 1

            return {
                'batches_found': 0,
                'note': 'No formal batches detected (distributed traces may not exhibit batching)',
                'near_concurrent_activities': [
                    {'activity': a, 'near_concurrent_pairs': c}
                    for a, c in activity_counts.most_common(5)
                ] if activity_counts else [],
            }

        # Analyze batch sizes
        batch_sizes = [b[1] for b in batches]
        batch_resources = [b[0][1] for b in batches]

        # Count unique batched cases
        total_batched_cases = sum(batch_sizes)

        # Per-resource batch stats
        from collections import Counter
        resource_batches = Counter(batch_resources)

        return {
            'batches_found': len(batches),
            'total_batched_cases': total_batched_cases,
            'avg_batch_size': round(np.mean(batch_sizes), 1),
            'max_batch_size': max(batch_sizes),
            'resources_with_batches': len(resource_batches),
            'top_batch_resources': [
                {'resource': r, 'batches': c}
                for r, c in resource_batches.most_common(5)
            ],
        }
    except Exception as e:
        print(f"  Batch detection failed: {e}")
        return None


def social_network_analysis(df):
    """Discover service interaction networks from trace data."""
    if len(df) == 0:
        return None

    # Check that we have enough distinct resources (services)
    n_resources = df['org:resource'].nunique() if 'org:resource' in df.columns else 0
    if n_resources < 2:
        print(f"  SNA skipped: need >= 2 distinct resources, found {n_resources}")
        return None

    how_edges = []
    wt_edges = []
    role_list = []

    # Handover-of-work network
    try:
        how_network = pm4py.discover_handover_of_work_network(df)
        # SNA object has .connections (list of tuples) and .is_directed
        if hasattr(how_network, 'connections'):
            for conn in how_network.connections:
                if isinstance(conn, (tuple, list)) and len(conn) >= 2:
                    weight = conn[2] if len(conn) > 2 else 1
                    how_edges.append({
                        'source': str(conn[0]),
                        'target': str(conn[1]),
                        'weight': float(weight) if weight else 1,
                    })
    except Exception as e:
        print(f"  Handover-of-work network failed: {e}")

    # Working-together network
    try:
        wt_network = pm4py.discover_working_together_network(df)
        if hasattr(wt_network, 'connections'):
            for conn in wt_network.connections:
                if isinstance(conn, (tuple, list)) and len(conn) >= 2:
                    weight = conn[2] if len(conn) > 2 else 1
                    wt_edges.append({
                        'source': str(conn[0]),
                        'target': str(conn[1]),
                        'weight': float(weight) if weight else 1,
                    })
    except Exception as e:
        print(f"  Working-together network failed: {e}")

    # Organizational roles
    try:
        roles = pm4py.discover_organizational_roles(df)
        for r in (roles or []):
            role_list.append({
                'name': r.name if hasattr(r, 'name') else str(r),
                'activities': len(r.activities) if hasattr(r, 'activities') else 0,
            })
    except Exception as e:
        print(f"  Organizational roles failed: {e}")

    return {
        'handover_edges': len(how_edges),
        'working_together_edges': len(wt_edges),
        'organizational_roles': len(role_list),
        'top_handovers': sorted(how_edges, key=lambda x: x['weight'], reverse=True)[:10],
        'top_collaborations': sorted(wt_edges, key=lambda x: x['weight'], reverse=True)[:10],
        'roles': role_list[:5],
    }


def decision_mining(df, net, im, fm):
    """Discover data-driven routing decisions at gateway points using alignments."""
    if net is None or len(df) == 0:
        return None

    try:
        from sklearn.tree import DecisionTreeClassifier, export_text

        # Get alignments with full diagnostic info
        aligned = pm4py.conformance_diagnostics_alignments(df, net, im, fm)

        # For each trace, extract features from alignment and label the variant
        trace_features = []
        trace_labels = []

        for alignment in aligned:
            features = {}
            if isinstance(alignment, dict):
                features['alignment_cost'] = alignment.get('cost', 0)
                features['fitness'] = alignment.get('fitness', 0)
                # Count log moves (unexpected) and model moves (skipped)
                alignment_data = alignment.get('alignment', [])
                log_moves = sum(1 for m in alignment_data if isinstance(m, tuple) and m[0] is None)
                model_moves = sum(1 for m in alignment_data if isinstance(m, tuple) and m[1] is None)
                features['log_moves'] = log_moves
                features['model_moves'] = model_moves
                trace_labels.append(alignment.get('trace_is_fit', True))

            if features:
                trace_features.append(features)

        if len(trace_features) < 5:
            return None

        feat_df = pd.DataFrame(trace_features)
        label_series = pd.Series(trace_labels)

        # Only train if we have both classes
        if label_series.nunique() < 2:
            reason_str = 'insufficient class diversity (all traces are ' + str(label_series.iloc[0])
            return {
                'decision_points': 0,
                'reason': reason_str,
            }

        # Train decision tree
        clf = DecisionTreeClassifier(max_depth=3, random_state=42)
        clf.fit(feat_df, label_series)

        tree_rules = export_text(clf, feature_names=list(feat_df.columns))

        return {
            'decision_points': 1,
            'tree_depth': clf.get_depth(),
            'feature_importance': dict(zip(feat_df.columns, clf.feature_importances_.round(3))),
            'tree_rules': tree_rules[:500],
            'fit_traces': int(label_series.sum()),
            'unfit_traces': int((~label_series).sum()),
        }
    except Exception as e:
        print(f"  Decision mining failed: {e}")
        return None


def performance_spectrum_analysis(df):
    """Analyze performance spectrum for bimodal latency patterns."""
    if len(df) == 0 or 'duration_ms' not in df.columns:
        return None

    try:
        # Get top 5 activities by frequency for spectrum analysis
        activity_counts = df['concept:name'].value_counts().head(5)

        spectrum_results = []
        for activity, count in activity_counts.items():
            durations = df[df['concept:name'] == activity]['duration_ms']

            # Detect bimodal distribution
            if len(durations) >= 10:
                q25 = durations.quantile(0.25)
                q75 = durations.quantile(0.75)
                iqr = q75 - q25
                # Outliers: beyond 1.5 * IQR
                outlier_count = int(((durations < q25 - 1.5 * iqr) | (durations > q75 + 1.5 * iqr)).sum())

                # Coefficient of variation (higher = more variable)
                cv = durations.std() / durations.mean() if durations.mean() > 0 else 0

                spectrum_results.append({
                    'activity': activity,
                    'count': int(count),
                    'mean_ms': round(float(durations.mean()), 1),
                    'median_ms': round(float(durations.median()), 1),
                    'p95_ms': round(float(durations.quantile(0.95)), 1),
                    'p99_ms': round(float(durations.quantile(0.99)), 1),
                    'std_ms': round(float(durations.std()), 1),
                    'cv': round(float(cv), 3),
                    'outliers': outlier_count,
                    'bimodal': cv > 0.8 or outlier_count > len(durations) * 0.1,
                })

        # Sort by coefficient of variation (most variable first)
        spectrum_results.sort(key=lambda x: x['cv'], reverse=True)

        return {
            'activities_analyzed': len(spectrum_results),
            'bimodal_activities': sum(1 for s in spectrum_results if s['bimodal']),
            'spectrum': spectrum_results[:10],
        }
    except Exception as e:
        print(f"  Performance spectrum analysis failed: {e}")
        return None


def declare_discovery(df):
    """Discover Declare constraints from the event log."""
    if len(df) == 0:
        return None

    try:
        declare_model = pm4py.discover_declare(df, min_support_ratio=0.5)

        if not declare_model:
            return None

        # Summarize discovered constraints
        constraints = []
        for template_name, instances in declare_model.items():
            for (act1, act2), metrics in instances.items():
                constraints.append({
                    'template': template_name,
                    'activities': f"{act1}, {act2}",
                    'support': round(float(metrics.get('support', 0)), 3),
                    'confidence': round(float(metrics.get('confidence', 0)), 3),
                })

        # Sort by confidence descending
        constraints.sort(key=lambda x: x['confidence'], reverse=True)

        return {
            'total_constraints': len(constraints),
            'templates_found': len(set(c['template'] for c in constraints)),
            'top_constraints': constraints[:10],
        }
    except Exception as e:
        print(f"  Declare discovery failed: {e}")
        return None


def declare_conformance(df):
    """Check Declare conformance against a discovered Declare model."""
    if len(df) == 0:
        return None

    try:
        # Discover the Declare model first
        declare_model = pm4py.discover_declare(df, min_support_ratio=0.5)

        if not declare_model:
            return None

        # Check conformance
        conformance = pm4py.conformance_declare(df, declare_model)

        if not conformance:
            return None

        # Aggregate conformance results
        total_traces = len(conformance)
        fit_traces = sum(1 for c in conformance if c.get('is_fit', False))
        unfit_traces = total_traces - fit_traces

        fitnesses = [c.get('dev_fitness', 0) for c in conformance]
        mean_fitness = float(np.mean(fitnesses)) if fitnesses else 0

        # Collect deviation summaries
        deviation_templates = []
        for c in conformance:
            if not c.get('is_fit', False):
                for dev in c.get('deviations', []):
                    if isinstance(dev, (list, tuple)) and len(dev) >= 1:
                        deviation_templates.append(str(dev[0]))

        from collections import Counter
        dev_counter = Counter(deviation_templates)

        return {
            'total_traces': total_traces,
            'fit_traces': fit_traces,
            'unfit_traces': unfit_traces,
            'mean_fitness': round(mean_fitness, 4),
            'mean_no_deviations': round(float(np.mean([c.get('no_dev_total', 0) for c in conformance])), 2),
            'top_deviation_templates': [
                {'template': t, 'count': cnt}
                for t, cnt in dev_counter.most_common(5)
            ],
        }
    except Exception as e:
        print(f"  Declare conformance failed: {e}")
        return None


def process_cube_analysis(df):
    """Build a process cube (OLAP-style aggregation) from the event log."""
    if len(df) == 0:
        return None

    try:
        # Build feature table via outcome-enriched dataframe
        enriched = pm4py.extract_outcome_enriched_dataframe(df)
        features = pm4py.extract_features_dataframe(enriched, include_case_id=True)

        if features is None or len(features) == 0:
            return None

        # Find the @@sojourn_time column (or fallback to first @@ column)
        agg_col = '@@sojourn_time'
        if agg_col not in features.columns:
            at_cols = [c for c in features.columns if c.startswith('@@')]
            if not at_cols:
                return None
            agg_col = at_cols[0]

        cube, cfg = pm4py.get_process_cube(
            features,
            x_col='concept:name',
            y_col='org:resource',
            agg_col=agg_col,
        )

        if cube is None or (isinstance(cube, pd.DataFrame) and len(cube) == 0):
            return {
                'cube_rows': 0,
                'note': 'Process cube returned empty (data too uniform for meaningful aggregation)',
                'agg_column': agg_col,
            }

        return {
            'cube_rows': len(cube) if isinstance(cube, pd.DataFrame) else 0,
            'cube_columns': list(cube.columns)[:10] if isinstance(cube, pd.DataFrame) else [],
            'agg_column': agg_col,
            'x_col': 'concept:name',
            'y_col': 'org:resource',
        }
    except Exception as e:
        print(f"  Process cube analysis failed: {e}")
        return None


def process_simulation(df):
    """Simulate process execution by playing out a discovered Petri net model."""
    if len(df) == 0:
        return None

    try:
        net, im, fm = pm4py.discover_petri_net_inductive(df)

        if net is None:
            return None

        sim_log = pm4py.play_out(net, im, fm, num_traces=100)

        if not sim_log:
            return None

        # Analyze simulated traces
        sim_traces = list(sim_log) if hasattr(sim_log, '__iter__') else []
        trace_lengths = [len(t) if hasattr(t, '__len__') else 1 for t in sim_traces]
        unique_activities = set()
        for t in sim_traces:
            for event in (t if hasattr(t, '__iter__') else [t]):
                if hasattr(event, '__getitem__'):
                    act = event.get('concept:name', None) if isinstance(event, dict) else None
                    if act:
                        unique_activities.add(act)

        return {
            'simulated_traces': len(sim_traces),
            'mean_trace_length': round(float(np.mean(trace_lengths)), 1) if trace_lengths else 0,
            'max_trace_length': max(trace_lengths) if trace_lengths else 0,
            'min_trace_length': min(trace_lengths) if trace_lengths else 0,
            'unique_activities_in_simulation': len(unique_activities),
        }
    except Exception as e:
        print(f"  Process simulation failed: {e}")
        return None


def streaming_analysis(df):
    """Convert log to event stream and run streaming DFG discovery."""
    if len(df) == 0:
        return None

    try:
        # Convert to event stream
        stream = pm4py.convert_to_event_stream(df)

        if not stream or len(stream) == 0:
            return None

        # Streaming DFG discovery
        from pm4py.streaming.algo.discovery.dfg.variants.frequency import StreamingDfgDiscovery

        disc = StreamingDfgDiscovery()
        for event in stream:
            disc.receive_event(event)
        dfg_result = disc.check()

        # Parse DFG result
        dfg_edges = []
        if isinstance(dfg_result, dict) and 'dfg' in dfg_result:
            for (src, tgt), count in dfg_result['dfg'].items():
                dfg_edges.append({
                    'source': str(src),
                    'target': str(tgt),
                    'count': int(count),
                })
        elif isinstance(dfg_result, dict):
            for (src, tgt), count in dfg_result.items():
                dfg_edges.append({
                    'source': str(src),
                    'target': str(tgt),
                    'count': int(count),
                })

        # Sort by count descending
        dfg_edges.sort(key=lambda x: x['count'], reverse=True)

        return {
            'stream_events': len(stream),
            'dfg_edges': len(dfg_edges),
            'top_edges': dfg_edges[:10],
        }
    except Exception as e:
        print(f"  Streaming analysis failed: {e}")
        return None


def event_data_quality(df):
    """Assess event data quality across completeness, validity, timeliness, uniqueness, and coverage."""
    if len(df) == 0:
        return None

    try:
        required_columns = ['case:concept:name', 'concept:name', 'time:timestamp']
        all_columns = list(df.columns)

        # Completeness: check for missing required columns
        missing_required = [col for col in required_columns if col not in all_columns]
        missing_values = {}
        for col in all_columns:
            null_count = int(df[col].isna().sum())
            empty_count = int((df[col].astype(str).str.strip() == '').sum()) if df[col].dtype == 'object' else 0
            if null_count > 0 or empty_count > 0:
                missing_values[col] = {'null': null_count, 'empty': empty_count}

        # Validity: check timestamps are monotonic within each trace
        monotonic_violations = 0
        total_checks = 0
        if 'case:concept:name' in df.columns and 'time:timestamp' in df.columns:
            df_sorted = df.sort_values(['case:concept:name', 'time:timestamp'])
            for case_id, group in df_sorted.groupby('case:concept:name'):
                timestamps = group['time:timestamp'].values
                if len(timestamps) > 1:
                    total_checks += len(timestamps) - 1
                    for i in range(len(timestamps) - 1):
                        if timestamps[i + 1] < timestamps[i]:
                            monotonic_violations += 1

        # Timeliness: check for clock skew (negative duration spans)
        clock_skew_count = 0
        if 'duration_ms' in df.columns:
            clock_skew_count = int((df['duration_ms'] < 0).sum())

        # Uniqueness: check for duplicate span IDs if available
        duplicate_count = 0
        total_events = len(df)
        if 'case:concept:name' in df.columns and 'concept:name' in df.columns and 'time:timestamp' in df.columns:
            dup_mask = df.duplicated(subset=['case:concept:name', 'concept:name', 'time:timestamp'], keep=False)
            duplicate_count = int(dup_mask.sum())

        # Coverage: column and event statistics
        unique_cases = df['case:concept:name'].nunique() if 'case:concept:name' in df.columns else 0
        unique_activities = df['concept:name'].nunique() if 'concept:name' in df.columns else 0

        return {
            'completeness': {
                'total_columns': len(all_columns),
                'required_columns_present': [col for col in required_columns if col in all_columns],
                'missing_required_columns': missing_required,
                'columns_with_missing_values': len(missing_values),
                'missing_details': missing_values,
            },
            'validity': {
                'monotonic_timestamp_violations': monotonic_violations,
                'total_timestamp_checks': total_checks,
                'violation_rate': round(100 * monotonic_violations / max(total_checks, 1), 2),
            },
            'timeliness': {
                'clock_skew_events': clock_skew_count,
                'clock_skew_rate': round(100 * clock_skew_count / max(total_events, 1), 2),
            },
            'uniqueness': {
                'duplicate_events': duplicate_count,
                'duplicate_rate': round(100 * duplicate_count / max(total_events, 1), 2),
            },
            'coverage': {
                'total_events': total_events,
                'unique_cases': unique_cases,
                'unique_activities': unique_activities,
                'events_per_case': round(total_events / max(unique_cases, 1), 1),
            },
        }
    except Exception as e:
        print(f"  Event data quality assessment failed: {e}")
        return None


def act_remediation(df):
    """Run Declare conformance and generate remediation actions for unfit traces."""
    if len(df) == 0:
        return None

    try:
        # Discover Declare model
        declare_model = pm4py.discover_declare(df, min_support_ratio=0.5)

        if not declare_model:
            return None

        # Check conformance
        conformance = pm4py.conformance_declare(df, declare_model)

        if not conformance:
            return None

        # Identify unfit traces and generate remediation actions
        unfit_traces = []
        for i, c in enumerate(conformance):
            fitness = c.get('dev_fitness', 1.0)
            if fitness < 0.5:
                deviations = c.get('deviations', [])
                no_dev = c.get('no_dev_total', 0)
                no_constr = c.get('no_constr_total', 0)

                # Map violation templates to remediation actions
                actions = []
                for dev in deviations:
                    if isinstance(dev, (list, tuple)) and len(dev) >= 1:
                        template = str(dev[0])
                        act_info = str(dev[1]) if len(dev) >= 2 else 'N/A'

                        if 'absence' in template.lower():
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: activity '{act_info}' occurs too many times — add guard condition or deduplication",
                            })
                        elif 'existence' in template.lower():
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: missing required activity '{act_info}' — add mandatory step enforcement",
                            })
                        elif 'response' in template.lower():
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: response obligation violated for '{act_info}' — ensure follow-up activity is triggered",
                            })
                        elif 'precedence' in template.lower():
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: precedence constraint violated for '{act_info}' — enforce ordering dependency",
                            })
                        elif 'succession' in template.lower():
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: succession constraint violated for '{act_info}' — enforce both ordering and presence",
                            })
                        elif 'not_response' in template.lower():
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: forbidden response triggered for '{act_info}' — add negative constraint check",
                            })
                        elif 'not_succession' in template.lower():
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: forbidden succession triggered for '{act_info}' — block disallowed transition",
                            })
                        else:
                            actions.append({
                                'violation': f"{template}({act_info})",
                                'action': f"Review trace {i}: {template} constraint violated for '{act_info}' — investigate and resolve",
                            })

                unfit_traces.append({
                    'trace_index': i,
                    'dev_fitness': round(float(fitness), 4),
                    'no_deviations': no_dev,
                    'no_constraints': no_constr,
                    'remediation_actions': actions,
                })

        total_traces = len(conformance)
        total_unfit = len(unfit_traces)

        return {
            'total_traces': total_traces,
            'traces_requiring_remediation': total_unfit,
            'remediation_rate': round(100 * total_unfit / max(total_traces, 1), 1),
            'remediations': unfit_traces[:20],  # Limit output
        }
    except Exception as e:
        print(f"  ACT remediation failed: {e}")
        return None


def stochastic_process_mining(df):
    """Discover stochastic language from event log."""
    if len(df) == 0:
        return None

    try:
        # Get stochastic language — maps trace variants to probabilities
        language = pm4py.get_stochastic_language(df)

        # Sort by probability (descending)
        sorted_lang = sorted(language.items(), key=lambda x: x[1], reverse=True)

        return {
            'total_variants': len(sorted_lang),
            'total_probability': sum(v for _, v in sorted_lang),
            'top_variants': [
                {
                    'pattern': ' → '.join(k) if isinstance(k, tuple) else str(k),
                    'probability': round(v, 4),
                }
                for k, v in sorted_lang[:10]
            ],
        }
    except Exception as e:
        print(f"  Stochastic mining failed: {e}")
        return None


def process_drift_detection(df, window_hours=6):
    """Detect process drift using PM4Py's concept drift detection (Bose algorithm)."""
    if len(df) == 0:
        return None

    try:
        from pm4py.algo.concept_drift import algorithm as cd

        # Use Bose concept drift detection
        changes, indices, values = cd.apply(df)

        return {
            'drift_points': len(changes),
            'drift_indices': [int(i) for i in indices[:5]] if indices else [],
            'drift_values': [round(float(v), 4) for v in values[:5]] if values else [],
            'drift_detected': len(changes) > 0,
            'changes': [
                {
                    'index': int(idx),
                    'value': round(float(val), 4),
                    'change_point': 'YES' if val > 0 else 'NO',
                }
                for idx, val in zip(indices[:10], values[:10])
            ] if indices else [],
        }
    except Exception as e:
        print(f"  Bose drift detection failed: {e}")
        # Fallback to variant comparison
        return _variant_drift_fallback(df)


def _variant_drift_fallback(df):
    """Fallback drift detection by comparing variants across time windows."""
    if len(df) < 20:
        return None

    try:
        df = df.sort_values('time:timestamp')
        min_ts = df['time:timestamp'].min()
        max_ts = df['time:timestamp'].max()
        total_hours = (max_ts - min_ts).total_seconds() / 3600

        if total_hours < 0.1:
            return None

        midpoint = min_ts + timedelta(hours=total_hours / 2)
        df_early = df[df['time:timestamp'] < midpoint]
        df_late = df[df['time:timestamp'] >= midpoint]

        if len(df_early) < 5 or len(df_late) < 5:
            return None

        variants_early = pm4py.get_variants(df_early)
        variants_late = pm4py.get_variants(df_late)

        common = set(variants_early.keys()) & set(variants_late.keys())
        only_early = set(variants_early.keys()) - set(variants_late.keys())
        only_late = set(variants_late.keys()) - set(variants_early.keys())

        return {
            'drift_detected': len(only_late) > 0 or len(only_early) > 0,
            'method': 'variant_comparison_fallback',
            'window_1': f"first {total_hours / 2:.1f}h ({len(variants_early)} variants)",
            'window_2': f"last {total_hours / 2:.1f}h ({len(variants_late)} variants)",
            'new_variants': len(only_late),
            'disappeared_variants': len(only_early),
        }
    except Exception:
        return None


def main():
    parser = argparse.ArgumentParser(description='PM4Py Process Mining for UNRDF OTEL')
    parser.add_argument('--hours', type=int, default=24, help='Time range in hours')
    parser.add_argument('--limit', type=int, default=1000, help='Max traces to fetch')
    parser.add_argument('--output', type=str, default=None, help='Output JSON file')
    parser.add_argument('--mode', type=str, default='full',
                        choices=['full', 'core', 'predict', 'ocel', 'drift', 'anomaly',
                                 'spectrum', 'rework', 'batch', 'sna', 'decision',
                                 'declare', 'cube', 'simulate', 'stream', 'quality', 'act'],
                        help='Analysis mode: full (all), core (original), predict, ocel, drift, anomaly, '
                             'spectrum (performance spectrum), rework (rework detection), batch (batch detection), '
                             'sna (social network analysis), decision (decision mining), '
                             'declare (Declare constraints), cube (process cube OLAP), simulate (process simulation), '
                             'stream (streaming DFG), quality (event data quality), act (Declare remediation actions)')
    args = parser.parse_args()

    print("=" * 60)
    print("PM4Py Process Mining Analysis for UNRDF OTEL Stack")
    print("=" * 60)
    print(f"  Time range: last {args.hours}h")
    print(f"  Limit: {args.limit} traces")
    print(f"  Mode: {args.mode}")
    print()

    # Step 1: Fetch traces
    print("[1] Fetching traces from Tempo...")
    df = fetch_traces_from_tempo(hours=args.hours, limit=args.limit)
    print(f"    Found {len(df)} events from {df['case:concept:name'].nunique() if len(df) > 0 else 0} traces")

    # Step 2: Fetch logs
    print("[2] Fetching logs from Loki...")
    log_df = fetch_logs_from_loki(hours=args.hours, limit=args.limit)
    print(f"    Found {len(log_df)} log entries")

    results = {
        'timestamp': datetime.now(timezone.utc).isoformat(),
        'hours': args.hours,
        'mode': args.mode,
        'trace_events': len(df),
        'unique_traces': df['case:concept:name'].nunique() if len(df) > 0 else 0,
        'log_entries': len(log_df),
    }

    # Core analyses (steps 3-5 from original script)
    step = 3

    if args.mode in ('full', 'core'):
        print(f"[{step}] Discovering process model (Inductive Miner)...")
        net, im, fm = discover_process_model(df)
        if net:
            print(f"    Places: {len(net.places)}, Transitions: {len(net.transitions)}, Arcs: {len(net.arcs)}")
        else:
            print("    No process model discovered (insufficient traces)")
        results['process_model'] = {
            'places': len(net.places) if net else 0,
            'transitions': len(net.transitions) if net else 0,
            'arcs': len(net.arcs) if net else 0,
        }
        step += 1

        print(f"[{step}] Running token-based replay conformance check...")
        conformance = conformance_check(df, net, im, fm)
        if conformance:
            print(f"    Fitness: {conformance['fitness']}")
            print(f"    Precision: {conformance['precision']}")
        results['conformance'] = conformance
        step += 1

        print(f"[{step}] Analyzing bottlenecks...")
        bottlenecks = bottleneck_analysis(df)
        if bottlenecks is not None and len(bottlenecks) > 0:
            print("    Top 5 activities by mean duration:")
            for i, (activity, row) in enumerate(bottlenecks.head(5).iterrows()):
                print(f"      {i+1}. {activity:40s} mean={row['mean']:8.1f}ms  count={int(row['count'])}")
        results['bottlenecks'] = bottlenecks.to_dict() if bottlenecks is not None else None
        step += 1

        # Variant analysis
        print(f"[{step}] Analyzing trace variants...")
        variants = variant_analysis(df)
        if variants:
            print(f"    {variants['total_variants']} variants across {variants['total_traces']} traces")
            print("    Top 5 variants:")
            for v in variants['top_variants'][:5]:
                print(f"      [{v['percentage']}%] {v['count']}x: {v['pattern']}")
        results['variants'] = variants
        step += 1

    # Advanced analyses
    if args.mode in ('full', 'core'):
        print(f"[{step}] Running alignment-based conformance checking...")
        alignment = alignment_conformance_check(df, net, im, fm)
        if alignment:
            print(f"    Mean fitness: {alignment['mean_fitness']:.4f}")
            print(f"    Deviating traces: {alignment['deviating_traces']}/{alignment['total_traces']}")
        results['alignment_conformance'] = alignment
        step += 1

    if args.mode in ('full', 'predict'):
        print(f"[{step}] Running predictive monitoring...")
        predictions = predictive_monitoring(df)
        if predictions:
            if 'case_duration' in predictions:
                cd = predictions['case_duration']
                print(f"    Case duration: mean={cd['mean_ms']:.1f}ms, median={cd['median_ms']:.1f}ms, max={cd['max_ms']:.1f}ms")
            if 'prefix_replay' in predictions:
                pr = predictions['prefix_replay']
                print(f"    Prefix replay: {pr['unfit_prefixes']}/{pr['total_prefixes']} unfit prefixes (mean fitness={pr['mean_fitness']:.4f})")
        results['predictive_monitoring'] = predictions
        step += 1

    if args.mode in ('full', 'ocel'):
        print(f"[{step}] Running object-centric process mining (OCEL)...")
        ocel_results = object_centric_mining(df)
        if ocel_results:
            print(f"    OCEL events: {ocel_results['ocel_events']}")
            print(f"    Object types: {ocel_results['object_types']}")
            print(f"    OCPN: {ocel_results['ocpn_places']} places, {ocel_results['ocpn_transitions']} transitions")
        results['object_centric'] = ocel_results
        step += 1

    if args.mode in ('full', 'anomaly'):
        print(f"[{step}] Running ML-based anomaly detection...")
        anomalies = anomaly_detection(df)
        if anomalies:
            print(f"    Anomalous traces: {anomalies['anomalous_cases']}/{anomalies['total_cases']} ({anomalies['anomaly_rate']}%)")
        results['anomaly_detection'] = anomalies
        step += 1

    if args.mode in ('full', 'core'):
        print(f"[{step}] Discovering stochastic language...")
        stochastic = stochastic_process_mining(df)
        if stochastic:
            print(f"    Total variants: {stochastic['total_variants']}")
            print(f"    Total probability: {stochastic['total_probability']:.4f}")
        results['stochastic_model'] = stochastic
        step += 1

    if args.mode in ('full', 'drift'):
        print(f"[{step}] Detecting process drift (Bose algorithm)...")
        drift = process_drift_detection(df, window_hours=args.hours / 4)
        if drift:
            status = "DRIFT DETECTED" if drift['drift_detected'] else "No drift"
            method = drift.get('method', 'bose')
            print(f"    {status} (method: {method})")
            if drift.get('drift_points', 0) > 0:
                print(f"    Drift points: {drift['drift_points']}")
            elif drift.get('new_variants', 0) > 0:
                print(f"    New variants: {drift['new_variants']}, Disappeared: {drift['disappeared_variants']}")
        results['drift_detection'] = drift
        step += 1

    # New advanced analyses
    if args.mode in ('full', 'core'):
        print(f"[{step}] Running temporal profile conformance...")
        temporal = temporal_profile_conformance(df)
        if temporal:
            print(f"    Profile pairs: {temporal['temporal_profile_pairs']}")
            print(f"    Significant deviations: {temporal['significant_deviations']}/{temporal['total_checks']} ({temporal['deviation_rate']}%)")
        results['temporal_profile'] = temporal
        step += 1

    if args.mode in ('full', 'rework'):
        print(f"[{step}] Detecting rework patterns...")
        rework = rework_detection(df)
        if rework:
            print(f"    Activities with rework: {rework['total_rework_activities']}")
            print(f"    Total rework cases: {rework['total_rework_cases']}")
            if rework['top_rework']:
                print(f"    Top rework: {rework['top_rework'][0]['activity']} ({rework['top_rework'][0]['rework_cases']} cases)")
        results['rework_detection'] = rework
        step += 1

    if args.mode in ('full', 'batch'):
        print(f"[{step}] Detecting batch processing patterns...")
        batches = batch_detection(df)
        if batches:
            print(f"    Batches found: {batches['batches_found']}")
            print(f"    Total batched cases: {batches['total_batched_cases']}")
            print(f"    Avg batch size: {batches['avg_batch_size']}, Max: {batches['max_batch_size']}")
        results['batch_detection'] = batches
        step += 1

    if args.mode in ('full', 'sna'):
        print(f"[{step}] Running social network analysis...")
        sna = social_network_analysis(df)
        if sna:
            print(f"    Handover edges: {sna['handover_edges']}")
            print(f"    Collaboration edges: {sna['working_together_edges']}")
            print(f"    Organizational roles: {sna['organizational_roles']}")
        results['social_network'] = sna
        step += 1

    if args.mode in ('full', 'decision'):
        print(f"[{step}] Running decision mining (alignment-based)...")
        decisions = decision_mining(df, net, im, fm)
        if decisions:
            if decisions.get('tree_rules'):
                print(f"    Decision tree depth: {decisions['tree_depth']}")
                print(f"    Feature importance: {decisions['feature_importance']}")
                print(f"    Fit: {decisions['fit_traces']}, Unfit: {decisions['unfit_traces']}")
            else:
                print(f"    {decisions.get('reason', 'No decision points found')}")
        results['decision_mining'] = decisions
        step += 1

    if args.mode in ('full', 'spectrum'):
        print(f"[{step}] Analyzing performance spectrum...")
        spectrum = performance_spectrum_analysis(df)
        if spectrum:
            print(f"    Activities analyzed: {spectrum['activities_analyzed']}")
            print(f"    Bimodal activities: {spectrum['bimodal_activities']}")
            for s in spectrum['spectrum'][:3]:
                bimodal_flag = " [BIMODAL]" if s['bimodal'] else ""
                print(f"    {s['activity']:40s} cv={s['cv']:.3f} outliers={s['outliers']}{bimodal_flag}")
        results['performance_spectrum'] = spectrum
        step += 1

    # Declare constraint discovery
    if args.mode in ('full', 'declare'):
        print(f"[{step}] Discovering Declare constraints...")
        declare = declare_discovery(df)
        if declare:
            print(f"    Constraints found: {declare['total_constraints']}")
            print(f"    Templates used: {declare['templates_found']}")
            if declare['top_constraints']:
                top = declare['top_constraints'][0]
                print(f"    Top constraint: {top['template']} (confidence={top['confidence']}, support={top['support']})")
        results['declare_discovery'] = declare
        step += 1

    # Declare conformance checking
    if args.mode in ('full', 'declare'):
        print(f"[{step}] Running Declare conformance checking...")
        declare_conf = declare_conformance(df)
        if declare_conf:
            print(f"    Fit traces: {declare_conf['fit_traces']}/{declare_conf['total_traces']}")
            print(f"    Mean fitness: {declare_conf['mean_fitness']}")
            print(f"    Mean deviations per trace: {declare_conf['mean_no_deviations']}")
        results['declare_conformance'] = declare_conf
        step += 1

    # Process cube (OLAP-style aggregation)
    if args.mode in ('full', 'cube'):
        print(f"[{step}] Building process cube (OLAP aggregation)...")
        cube = process_cube_analysis(df)
        if cube:
            print(f"    Cube rows: {cube['cube_rows']}")
            print(f"    Aggregation column: {cube['agg_column']}")
            if cube.get('note'):
                print(f"    Note: {cube['note']}")
        results['process_cube'] = cube
        step += 1

    # Process simulation (play-out)
    if args.mode in ('full', 'simulate'):
        print(f"[{step}] Running process simulation (play-out)...")
        sim = process_simulation(df)
        if sim:
            print(f"    Simulated traces: {sim['simulated_traces']}")
            print(f"    Mean trace length: {sim['mean_trace_length']}")
            print(f"    Unique activities in simulation: {sim['unique_activities_in_simulation']}")
        results['process_simulation'] = sim
        step += 1

    # Streaming DFG analysis
    if args.mode in ('full', 'stream'):
        print(f"[{step}] Running streaming DFG discovery...")
        stream = streaming_analysis(df)
        if stream:
            print(f"    Stream events: {stream['stream_events']}")
            print(f"    DFG edges: {stream['dfg_edges']}")
            if stream['top_edges']:
                top = stream['top_edges'][0]
                print(f"    Top edge: {top['source']} -> {top['target']} (count={top['count']})")
        results['streaming_analysis'] = stream
        step += 1

    # Event data quality assessment
    if args.mode in ('full', 'quality'):
        print(f"[{step}] Assessing event data quality...")
        quality = event_data_quality(df)
        if quality:
            comp = quality['completeness']
            print(f"    Columns: {comp['total_columns']}, Missing required: {comp['missing_required_columns'] or 'none'}")
            val = quality['validity']
            print(f"    Monotonic timestamp violations: {val['monotonic_timestamp_violations']}/{val['total_timestamp_checks']}")
            tim = quality['timeliness']
            print(f"    Clock skew events: {tim['clock_skew_events']}")
            uniq = quality['uniqueness']
            print(f"    Duplicate events: {uniq['duplicate_events']}")
            cov = quality['coverage']
            print(f"    Coverage: {cov['total_events']} events, {cov['unique_cases']} cases, {cov['unique_activities']} activities")
        results['event_data_quality'] = quality
        step += 1

    # ACT remediation actions
    if args.mode in ('full', 'act'):
        print(f"[{step}] Running ACT remediation analysis...")
        act = act_remediation(df)
        if act:
            print(f"    Traces requiring remediation: {act['traces_requiring_remediation']}/{act['total_traces']}")
            print(f"    Remediation rate: {act['remediation_rate']}%")
            if act['remediations']:
                top = act['remediations'][0]
                print(f"    Top unfit trace: index={top['trace_index']}, fitness={top['dev_fitness']}, actions={len(top['remediation_actions'])}")
        results['act_remediation'] = act
        step += 1

    # Output results
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
