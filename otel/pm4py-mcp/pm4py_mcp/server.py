"""
pm4py-mcp — MCP server wrapping PM4Py process mining analysis on OTel traces.

Exposes 26 tools that fetch traces from Tempo, discover process models,
run conformance checks, and perform advanced process mining analyses.
"""

import importlib.util
import json
import os
import sys

from mcp.server.fastmcp import FastMCP

# pm4py-analyze.py lives at /Users/sac/unrdf/otel/pm4py-analyze.py.
# The filename contains a hyphen so it cannot be imported with a bare
# `import` statement.  Use importlib to load it by file path.
_OTEL_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
_PM4PY_ANALYZE_PATH = os.path.join(_OTEL_DIR, "pm4py-analyze.py")

if not os.path.isfile(_PM4PY_ANALYZE_PATH):
    raise FileNotFoundError(
        f"Cannot find pm4py-analyze.py at {_PM4PY_ANALYZE_PATH}"
    )

_spec = importlib.util.spec_from_file_location("pm4py_analyze", _PM4PY_ANALYZE_PATH)
_pm4py_analyze = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_pm4py_analyze)

# Bind analysis functions into module scope
fetch_traces_from_tempo = _pm4py_analyze.fetch_traces_from_tempo
fetch_logs_from_loki = _pm4py_analyze.fetch_logs_from_loki
discover_process_model = _pm4py_analyze.discover_process_model
conformance_check = _pm4py_analyze.conformance_check
alignment_conformance_check = _pm4py_analyze.alignment_conformance_check
bottleneck_analysis = _pm4py_analyze.bottleneck_analysis
variant_analysis = _pm4py_analyze.variant_analysis
predictive_monitoring = _pm4py_analyze.predictive_monitoring
object_centric_mining = _pm4py_analyze.object_centric_mining
anomaly_detection = _pm4py_analyze.anomaly_detection
temporal_profile_conformance = _pm4py_analyze.temporal_profile_conformance
rework_detection = _pm4py_analyze.rework_detection
batch_detection = _pm4py_analyze.batch_detection
social_network_analysis = _pm4py_analyze.social_network_analysis
decision_mining = _pm4py_analyze.decision_mining
performance_spectrum_analysis = _pm4py_analyze.performance_spectrum_analysis
declare_discovery = _pm4py_analyze.declare_discovery
declare_conformance = _pm4py_analyze.declare_conformance
process_cube_analysis = _pm4py_analyze.process_cube_analysis
process_simulation = _pm4py_analyze.process_simulation
streaming_analysis = _pm4py_analyze.streaming_analysis
event_data_quality = _pm4py_analyze.event_data_quality
act_remediation = _pm4py_analyze.act_remediation
goal_oriented_analysis = _pm4py_analyze.goal_oriented_analysis
hierarchical_process_mining = _pm4py_analyze.hierarchical_process_mining
counterfactual_analysis = _pm4py_analyze.counterfactual_analysis
normative_process_mining = _pm4py_analyze.normative_process_mining
multi_agent_orchestration = _pm4py_analyze.multi_agent_orchestration
self_reflective_analysis = _pm4py_analyze.self_reflective_analysis
stochastic_process_mining = _pm4py_analyze.stochastic_process_mining
process_drift_detection = _pm4py_analyze.process_drift_detection

mcp = FastMCP("pm4py-mcp")


def _safe_json(obj, default=str):
    """Serialize to JSON, handling non-serializable types."""
    return json.dumps(obj, default=default, ensure_ascii=False)


def _fetch_or_error(hours: int, limit: int):
    """Fetch traces; return (df, error_json) — error_json is None on success."""
    df = fetch_traces_from_tempo(hours=hours, limit=limit)
    if len(df) == 0:
        return df, json.dumps({"error": "No traces found", "trace_count": 0})
    return df, None


def _discover_or_error(df):
    """Discover a process model; return (net, im, fm, error_json)."""
    net, im, fm = discover_process_model(df)
    if net is None:
        return net, im, fm, json.dumps({"error": "Process discovery failed", "trace_count": len(df)})
    return net, im, fm, None


# ---------------------------------------------------------------------------
# 1. Health check
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_health(hours: int = 1, limit: int = 100) -> str:
    """Quick health check of the observability stack. Returns trace count, log count, and basic event statistics."""
    try:
        df = fetch_traces_from_tempo(hours=hours, limit=limit)
        log_df = fetch_logs_from_loki(hours=hours, limit=limit)
        unique_traces = df["case:concept:name"].nunique() if len(df) > 0 else 0
        unique_activities = df["concept:name"].nunique() if len(df) > 0 else 0
        unique_services = df["org:resource"].nunique() if len(df) > 0 else 0
        result = {
            "trace_events": len(df),
            "unique_traces": int(unique_traces),
            "unique_activities": int(unique_activities),
            "unique_services": int(unique_services),
            "log_entries": len(log_df),
            "status": "ok" if len(df) > 0 else "no_traces",
        }
        return _safe_json(result)
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 2. Process model discovery
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_discover(hours: int = 24, limit: int = 1000) -> str:
    """Discover process model from OTel traces using Inductive Miner algorithm. Returns Petri net statistics (places, transitions, arcs)."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        net, im, fm, err = _discover_or_error(df)
        if err:
            return err
        result = {
            "trace_events": len(df),
            "unique_traces": int(df["case:concept:name"].nunique()),
            "process_model": {
                "places": len(net.places),
                "transitions": len(net.transitions),
                "arcs": len(net.arcs),
            },
        }
        return _safe_json(result)
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 3. Token-based replay conformance
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_conformance(hours: int = 24, limit: int = 1000) -> str:
    """Token-based replay conformance checking. Computes fitness and precision metrics against the discovered process model."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        net, im, fm, err = _discover_or_error(df)
        if err:
            return err
        result = conformance_check(df, net, im, fm)
        return _safe_json(result) if result else _safe_json({"error": "Conformance check returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 4. Alignment-based conformance
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_align(hours: int = 24, limit: int = 1000) -> str:
    """Alignment-based conformance checking. Computes optimal alignments between traces and discovered process model to identify deviations."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        net, im, fm, err = _discover_or_error(df)
        if err:
            return err
        result = alignment_conformance_check(df, net, im, fm)
        return _safe_json(result) if result else _safe_json({"error": "Alignment check returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 5. Bottleneck analysis
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_bottleneck(hours: int = 24, limit: int = 1000) -> str:
    """Bottleneck analysis. Identifies activities with highest mean and median durations that slow down the process."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = bottleneck_analysis(df)
        if result is not None and len(result) > 0:
            return _safe_json(result.to_dict())
        return _safe_json({"error": "Bottleneck analysis returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 6. Variant analysis
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_variant(hours: int = 24, limit: int = 1000) -> str:
    """Variant analysis. Lists the most common execution patterns (trace variants) with their frequency and coverage percentage."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = variant_analysis(df)
        return _safe_json(result) if result else _safe_json({"error": "Variant analysis returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 7. Predictive monitoring
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_predict(hours: int = 24, limit: int = 1000) -> str:
    """Predictive monitoring. Predicts case durations and evaluates prefix-based fitness against the discovered model."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = predictive_monitoring(df)
        return _safe_json(result) if result else _safe_json({"error": "Predictive monitoring returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 8. Object-centric mining
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_ocel(hours: int = 24, limit: int = 1000) -> str:
    """Object-centric process mining. Converts traces to OCEL format and discovers an object-centric Petri net across services and MCP tools."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = object_centric_mining(df)
        return _safe_json(result) if result else _safe_json({"error": "Object-centric mining returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 9. Anomaly detection
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_anomaly(hours: int = 24, limit: int = 1000) -> str:
    """Anomaly detection using Isolation Forest and optional TPOT2 AutoML. Identifies traces with abnormal duration or behavior patterns."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = anomaly_detection(df)
        return _safe_json(result) if result else _safe_json({"error": "Anomaly detection returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 10. Temporal profile conformance
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_temporal(hours: int = 24, limit: int = 1000) -> str:
    """Temporal profile conformance checking. Discovers expected time between activity pairs and flags SLA violations via zeta scores."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = temporal_profile_conformance(df)
        return _safe_json(result) if result else _safe_json({"error": "Temporal profile conformance returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 11. Rework detection
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_rework(hours: int = 24, limit: int = 1000) -> str:
    """Rework detection. Finds activities that repeat within the same trace, indicating retries or loops."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = rework_detection(df)
        return _safe_json(result) if result else _safe_json({"error": "Rework detection returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 12. Batch detection
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_batch(hours: int = 24, limit: int = 1000) -> str:
    """Batch detection. Identifies batch processing patterns that may explain tail latency in distributed traces."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = batch_detection(df)
        return _safe_json(result) if result else _safe_json({"error": "Batch detection returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 13. Social network analysis
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_sna(hours: int = 24, limit: int = 1000) -> str:
    """Social network analysis. Discovers handover-of-work and working-together networks between services from trace data."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = social_network_analysis(df)
        return _safe_json(result) if result else _safe_json({"error": "Social network analysis returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 14. Decision mining
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_decision(hours: int = 24, limit: int = 1000) -> str:
    """Decision mining. Discovers data-driven routing decisions at gateway points using alignment-based feature extraction and decision trees."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        net, im, fm, err = _discover_or_error(df)
        if err:
            return err
        result = decision_mining(df, net, im, fm)
        return _safe_json(result) if result else _safe_json({"error": "Decision mining returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 15. Performance spectrum
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_spectrum(hours: int = 24, limit: int = 1000) -> str:
    """Performance spectrum analysis. Analyzes per-activity latency distributions to detect bimodal patterns and outliers."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = performance_spectrum_analysis(df)
        return _safe_json(result) if result else _safe_json({"error": "Performance spectrum analysis returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 16. Declare constraint discovery
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_declare_discover(hours: int = 24, limit: int = 1000) -> str:
    """Declare constraint discovery. Discovers declarative process rules (e.g. precedence, response, absence) from the event log."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = declare_discovery(df)
        return _safe_json(result) if result else _safe_json({"error": "Declare discovery returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 17. Declare conformance check
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_declare_check(hours: int = 24, limit: int = 1000) -> str:
    """Declare conformance checking. Checks each trace against discovered Declare constraints and reports deviations."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = declare_conformance(df)
        return _safe_json(result) if result else _safe_json({"error": "Declare conformance returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 18. Process cube OLAP
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_cube(hours: int = 24, limit: int = 1000) -> str:
    """Process cube OLAP analysis. Builds a multi-dimensional aggregation of process performance by activity and resource."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = process_cube_analysis(df)
        return _safe_json(result) if result else _safe_json({"error": "Process cube analysis returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 19. Process simulation
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_simulate(hours: int = 24, limit: int = 1000) -> str:
    """Process simulation. Plays out the discovered Petri net to generate synthetic traces and reports trace length statistics."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = process_simulation(df)
        return _safe_json(result) if result else _safe_json({"error": "Process simulation returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 20. Streaming DFG analysis
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_stream(hours: int = 24, limit: int = 1000) -> str:
    """Streaming DFG analysis. Converts the log to an event stream and discovers a directly-follows graph using streaming frequency counting."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = streaming_analysis(df)
        return _safe_json(result) if result else _safe_json({"error": "Streaming analysis returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 21. Event data quality
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_quality(hours: int = 24, limit: int = 1000) -> str:
    """Event data quality assessment. Evaluates completeness, validity, timeliness, uniqueness, and coverage of the event log."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = event_data_quality(df)
        return _safe_json(result) if result else _safe_json({"error": "Event data quality assessment returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 22. Declare remediation actions
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_act(hours: int = 24, limit: int = 1000) -> str:
    """Declare remediation actions. Runs Declare conformance and generates actionable remediation suggestions for unfit traces."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = act_remediation(df)
        return _safe_json(result) if result else _safe_json({"error": "ACT remediation returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 23. Goal-oriented analysis
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_goal(hours: int = 24, limit: int = 1000) -> str:
    """Goal-oriented analysis. Compares planned vs executed traces using the most common variant as the plan, including MCP tool sequence compliance."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = goal_oriented_analysis(df)
        return _safe_json(result) if result else _safe_json({"error": "Goal-oriented analysis returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 24. Process drift detection
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_drift(hours: int = 24, limit: int = 1000, window_hours: float = 6.0) -> str:
    """Process drift detection. Uses the Bose concept drift algorithm (with variant comparison fallback) to detect behavioral changes over time."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        result = process_drift_detection(df, window_hours=int(window_hours))
        return _safe_json(result) if result else _safe_json({"error": "Drift detection returned no results"})
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 25. Full analysis (composite)
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_full(hours: int = 24, limit: int = 1000) -> str:
    """Run full process mining analysis: discovery, conformance, alignment, bottleneck, variant, temporal, rework, stochastic, and drift analysis. Returns comprehensive report."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err

        report = {
            "trace_events": len(df),
            "unique_traces": int(df["case:concept:name"].nunique()),
        }

        # Discover process model once
        net, im, fm = discover_process_model(df)
        if net is not None:
            report["process_model"] = {
                "places": len(net.places),
                "transitions": len(net.transitions),
                "arcs": len(net.arcs),
            }

            # Conformance
            conf = conformance_check(df, net, im, fm)
            if conf:
                report["conformance"] = conf

            # Alignment
            align = alignment_conformance_check(df, net, im, fm)
            if align:
                report["alignment"] = align

        # Bottleneck
        bn = bottleneck_analysis(df)
        if bn is not None and len(bn) > 0:
            report["bottleneck"] = bn.to_dict()

        # Variant
        va = variant_analysis(df)
        if va:
            report["variants"] = va

        # Temporal profile
        tp = temporal_profile_conformance(df)
        if tp:
            report["temporal_profile"] = tp

        # Rework
        rw = rework_detection(df)
        if rw:
            report["rework"] = rw

        # Stochastic
        sp = stochastic_process_mining(df)
        if sp:
            report["stochastic"] = sp

        # Drift
        dr = process_drift_detection(df, window_hours=max(1, hours // 4))
        if dr:
            report["drift"] = dr

        return _safe_json(report)
    except Exception as e:
        return _safe_json({"error": str(e)})


# ---------------------------------------------------------------------------
# 26. CI conformance gate (composite)
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_ci_gate(hours: int = 24, limit: int = 1000, min_fitness: float = 0.8, min_precision: float = 0.7) -> str:
    """CI gate for process mining conformance. Checks if fitness and precision meet thresholds. Returns pass/fail with metrics."""
    try:
        df, err = _fetch_or_error(hours, limit)
        if err:
            return err
        net, im, fm, err = _discover_or_error(df)
        if err:
            return err

        result = conformance_check(df, net, im, fm)
        if result is None:
            return _safe_json({"error": "Conformance check failed", "passed": False})

        fitness = result.get("fitness", {}).get("average_trace_fitness", 0)
        precision = result.get("precision", 0)
        passed = fitness >= min_fitness and precision >= min_precision

        return _safe_json({
            "passed": passed,
            "fitness": fitness,
            "precision": precision,
            "min_fitness": min_fitness,
            "min_precision": min_precision,
            "trace_events": len(df),
            "unique_traces": int(df["case:concept:name"].nunique()),
        })
    except Exception as e:
        return _safe_json({"error": str(e), "passed": False})


# ---------------------------------------------------------------------------
# 27. POWL model discovery from XES content
# ---------------------------------------------------------------------------

@mcp.tool()
async def pm4py_discover_powl(xes_content: str, variant: str = "inductive", filtering_weight_factor: float = 0.0) -> str:
    """Discover a POWL (Partially Ordered Workflow Language) model from XES event log content string. Returns the POWL model as a nested JSON structure with operator types, activity labels, and a string representation. Supports variants: 'inductive' (default) and 'maximal'."""
    import os
    import tempfile
    try:
        import pm4py
    except ImportError as e:
        return _safe_json({"error": f"pm4py not available: {e}"})

    try:
        from pm4py.objects.powl.obj import OperatorPOWL, StrictPartialOrder, Transition, SilentTransition
    except ImportError:
        return _safe_json({"error": "pm4py POWL objects not available — upgrade pm4py >= 2.7.0"})

    try:
        # pm4py XES importer requires a file path
        with tempfile.NamedTemporaryFile(mode="w", suffix=".xes", delete=False, encoding="utf-8") as f:
            f.write(xes_content)
            tmp_path = f.name

        try:
            log = pm4py.read_xes(tmp_path)
        finally:
            os.unlink(tmp_path)

        # Discover POWL — newer pm4py accepts variant + filtering_weight_factor
        try:
            powl_model = pm4py.discover_powl(
                log,
                variant=variant,
                filtering_weight_factor=filtering_weight_factor,
            )
        except TypeError:
            # Older pm4py API — no kwargs
            powl_model = pm4py.discover_powl(log)

        def _serialize(node):
            if isinstance(node, SilentTransition):
                return {"type": "silent", "label": None}
            if isinstance(node, Transition):
                return {"type": "activity", "label": node.label}
            if isinstance(node, StrictPartialOrder):
                nodes_list = list(node.order.nodes)
                edges_list = [
                    {"source": str(u.label if hasattr(u, "label") else u),
                     "target": str(v.label if hasattr(v, "label") else v)}
                    for u, v in node.order.edges
                ]
                return {
                    "type": "partial_order",
                    "children": [_serialize(c) for c in nodes_list],
                    "edges": edges_list,
                }
            if isinstance(node, OperatorPOWL):
                op_name = str(node.operator).split(".")[-1].lower()
                return {
                    "type": op_name,
                    "children": [_serialize(c) for c in node.children],
                }
            return {"type": "unknown", "repr": str(node)}

        def _collect(node, activities, op_types):
            t = node.get("type", "unknown")
            if t == "activity" and node.get("label"):
                activities.add(node["label"])
            elif t not in ("silent", "unknown"):
                op_types.add(t)
            for child in node.get("children", []):
                _collect(child, activities, op_types)

        serialized = _serialize(powl_model)
        activities: set = set()
        op_types: set = set()
        _collect(serialized, activities, op_types)

        return _safe_json({
            "powl_model": serialized,
            "powl_string": str(powl_model),
            "node_count": len(activities) + len(op_types),
            "activity_count": len(activities),
            "activity_labels": sorted(activities),
            "operator_types": sorted(op_types),
            "variant_used": variant,
            "model_description": (
                f"POWL model with {len(activities)} activities"
                + (f" and operators: {', '.join(sorted(op_types))}" if op_types else "")
            ),
        })
    except Exception as e:
        return _safe_json({"error": str(e)})


if __name__ == "__main__":
    mcp.run()
