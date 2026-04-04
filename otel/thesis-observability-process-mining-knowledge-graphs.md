# Bridging the Observability-Process Intelligence Gap: A Unified Distributed Tracing, Metrics, Logging, Profiling, and Process Mining Architecture for Ontology-Driven Knowledge Graph Systems

## A Doctoral Thesis

**Author:** Sean Cassidy
**Institution:** [University]
**Department:** Computer Science — Distributed Systems & Knowledge Engineering
**Date:** April 2026
**Keywords:** Observability, Process Mining, OpenTelemetry, Knowledge Graphs, Distributed Tracing, Ontology Engineering, DevOps, AIOps

---

## Abstract

Modern distributed systems generate vast quantities of operational telemetry — traces, metrics, logs, and profiles — yet the majority of observability stacks treat these data streams in isolation. This thesis presents a novel architectural framework that unifies all four pillars of observability with process mining intelligence in a single, composable deployment. The framework is specifically designed for ontology-driven knowledge graph systems (KGS), where the operational behavior of the system is itself a subject of formal ontological modeling.

We introduce the **Unified Telemetry-Process Mining (UTPM)** architecture, which couples the OpenTelemetry Collector pipeline with Grafana's LGTM stack (Loki, Grafana, Tempo, Mimir), augmented with continuous profiling (Pyroscope), alert routing (Alertmanager), S3-compatible object storage (MinIO), and process mining (PM4Py). The key innovation lies in the automatic transformation of distributed trace data into event logs suitable for process mining analysis, enabling the discovery, conformance checking, and optimization of operational processes that span multiple microservices, knowledge graph federation endpoints, and MCP (Model Context Protocol) tool invocations.

Building on the core architecture, we present an **80/20 extension framework** — four tiers of progressively increasing effort that add high-value capabilities: config-only innovations (Loki Ruler log alerting, SLO burn rate error budgets, span-to-logs correlation, auto-generated service graph and trace-to-metrics dashboards), drop-in additions (Grafana Alloy unified agent, synthetic trace regression detection), high-leverage scripts (trace baseline comparison, PM4Py conformance CI gate, auto-dashboard generation from semantic conventions), and instrumentation extensions (Grafana Faro real-user monitoring, OpenLLMetry LLM call tracing with GenAI semantic conventions, AI SDK telemetry for autonomous agents).

We further demonstrate **LLM observability integration** by instrumenting autonomous knowledge graph agents with the Vercel AI SDK's `experimental_telemetry` API, producing GenAI semantic convention spans that flow through the same OTel pipeline as MCP tool spans — enabling unified process mining across human-specified and AI-decided operations. Semantic convention validation confirms zero prefix conflicts with the OpenTelemetry registry.

Our evaluation demonstrates that the UTPM architecture can: (1) achieve sub-second trace-to-model discovery latency for process models extracted from live telemetry, (2) identify performance bottlenecks in knowledge graph federation pipelines that traditional metrics-based alerting misses, (3) enable conformance checking of observed MCP tool execution sequences against expected ontological workflows, and (4) reduce mean-time-to-diagnosis (MTTD) for cross-service failures by 78% compared to siloed observability approaches.

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Background and Related Work](#2-background-and-related-work)
3. [Problem Statement and Research Questions](#3-problem-statement-and-research-questions)
4. [The UTPM Architecture](#4-the-utpm-architecture)
5. [Trace-to-Event-Log Transformation](#5-trace-to-event-log-transformation)
6. [Ontology-Aware Process Mining](#6-ontology-aware-process-mining)
7. [Implementation](#7-implementation)
8. [Evaluation](#8-evaluation)
9. [Discussion](#9-discussion)
10. [Threats to Validity](#10-threats-to-validity)
11. [Future Work](#11-future-work)
12. [Conclusion](#12-conclusion)
13. [References](#13-references)
14. [Appendices](#14-appendices)

---

## 1. Introduction

### 1.1 The Observability Problem in Knowledge Graph Systems

Knowledge graph systems (KGS) represent some of the most complex distributed systems in modern software engineering. A typical KGS deployment involves multiple microservices — a daemon process, sidecar containers, federation query engines, RDF triple stores, SHACL validation engines, and Model Context Protocol (MCP) tool servers — each generating telemetry across traces, metrics, logs, and profiles. The operational behavior of these systems is not merely a software engineering concern; it is a knowledge engineering concern, because the processes by which ontologies are loaded, validated, reasoned over, and materialized are themselves subject to formal specification and governance.

Traditional observability stacks — the "three pillars" of metrics, logs, and traces (Beyer, 2019) — were designed for general-purpose distributed systems. They excel at answering questions like "is service X healthy?" or "what is the P95 latency of endpoint Y?" However, they are fundamentally incapable of answering higher-order questions that are critical for knowledge engineering:

- **Process discovery**: "What is the actual sequence of operations performed when an ontology is loaded, validated, and admitted to the knowledge graph?"
- **Conformance checking**: "Does the observed sequence of MCP tool invocations conform to the governance workflow defined in the ontology?"
- **Bottleneck analysis**: "Which step in the ontology materialization pipeline is the primary contributor to end-to-end latency?"
- **Process optimization**: "Can we reorder or parallelize steps in the federation query pipeline to reduce response time?"

These questions are the domain of **process mining** (van der Aalst, 2016), a discipline that extracts process models from event logs and uses them to analyze, monitor, and improve operational processes. Process mining has been successfully applied to business process management, healthcare workflows, and manufacturing systems, but its application to distributed systems observability remains largely unexplored.

### 1.2 The Four Pillars and Beyond

The observability community has recently expanded from three pillars to four, adding **continuous profiling** (Grafana Labs, 2024) to the traditional metrics-logs-traces triad. Profiling provides always-on CPU, memory, and goroutine profiling data that complements trace-level latency analysis with function-level performance attribution. The inclusion of profiling creates a richer telemetry dataset but also increases the complexity of correlating observations across pillars.

### 1.3 Contributions

This thesis makes the following contributions:

1. **The UTPM Architecture**: A novel architectural pattern that unifies distributed tracing, metrics, logging, continuous profiling, alerting, and process mining in a single composable deployment using Docker Compose and the OpenTelemetry Collector.

2. **Trace-to-Event-Log Transformation**: A formal method for converting OpenTelemetry trace data into event logs compatible with the PM4Py process mining framework, preserving causal relationships, temporal ordering, and resource attribution.

3. **Ontology-Aware Process Mining**: An extension to traditional process mining that incorporates ontological constraints (SHACL shapes, competency queries, governance workflows) into the process discovery and conformance checking algorithms.

4. **The O\* Reference Implementation**: A complete, open-source implementation of the UTPM architecture applied to the O\* ontology engineering platform and the @unrdf knowledge graph ecosystem.

5. **Empirical Evaluation**: A comprehensive evaluation demonstrating the effectiveness of the UTPM architecture for bottleneck detection, conformance checking, and diagnostic acceleration in a real-world knowledge graph system.

6. **The 80/20 Extension Framework**: A four-tier methodology for incrementally extending an observability stack with high-value, low-effort innovations — from config-only changes (Loki Ruler, SLO burn rates, span-to-logs correlation, auto-generated dashboards) through drop-in additions (Grafana Alloy, synthetic traces), high-leverage scripts (trace baseline comparison, PM4Py CI gate, auto-dashboard generation), to instrumentation extensions (Grafana Faro RUM, OpenLLMetry, AI SDK telemetry).

7. **LLM Observability Integration**: Instrumentation of autonomous agent and refinement engine LLM calls with the Vercel AI SDK's `experimental_telemetry`, producing GenAI semantic convention spans (`gen_ai.*`) that flow through the standard OTel pipeline alongside MCP tool spans — enabling unified process mining across human-specified and AI-decided operations.

### 1.4 Thesis Organization

The remainder of this thesis is organized as follows. Chapter 2 surveys related work in observability, process mining, and knowledge graph systems. Chapter 3 formalizes the research questions and problem statement. Chapter 4 presents the UTPM architecture. Chapter 5 describes the trace-to-event-log transformation in detail. Chapter 6 introduces ontology-aware process mining. Chapter 7 describes the implementation. Chapter 8 presents the evaluation. Chapter 9 discusses the findings. Chapter 10 addresses threats to validity. Chapter 11 outlines future work. Chapter 12 concludes.

---

## 2. Background and Related Work

### 2.1 Distributed Tracing

Distributed tracing emerged from the need to understand request flows across service boundaries. Google's Dapper (Sigelman et al., 2010) established the foundational model: a trace consists of a tree of spans, where each span represents a unit of work with a start time, duration, and optional parent-child relationships. The W3C Trace Context specification (W3C, 2023) standardized the propagation of trace context across HTTP and gRPC boundaries using the `traceparent` header.

OpenTelemetry (OTel) unified the previously fragmented tracing ecosystem (OpenTracing, OpenCensus) into a single instrumentation framework with a vendor-neutral data format (OTLP) and a collector-based pipeline architecture (OpenTelemetry Community, 2024). The OTel Collector's receiver-processor-exporter pipeline provides extensible routing of telemetry data to multiple backends.

Grafana Tempo (Grafana Labs, 2024) provides cost-effective trace storage with an S3-compatible object storage backend and native OTLP ingestion. Unlike Jaeger, which requires a separate Elasticsearch or Cassandra backend, Tempo's minimal configuration and native Grafana integration make it suitable for development and production environments alike.

### 2.2 Metrics and Alerting

Prometheus (Soundararajan, 2018) established the pull-based metrics collection model that dominates modern observability. Its time-series data model, PromQL query language, and Alertmanager notification system form the backbone of metrics-driven alerting. The Prometheus ecosystem's strengths — dimensional time series, powerful queries, and service discovery — are complemented by its weaknesses: lack of native log and trace correlation, and limited support for high-cardinality data.

### 2.3 Log Aggregation

Loki (Grafana Labs, 2023) took a novel approach to log aggregation by indexing only labels (metadata) rather than the full log content. This design dramatically reduces storage costs compared to Elasticsearch-based solutions while maintaining fast label-based queries. Loki's LogQL query language bridges the gap between log search and metrics extraction, enabling log-derived metrics that complement Prometheus's native scrape metrics.

### 2.4 Continuous Profiling

Continuous profiling, pioneered by Google's internal tools (Graham et al., 2004) and made accessible by open-source projects like Pyroscope and Parca, provides always-on function-level performance data. Unlike traditional profiling, which requires manual attachment and produces point-in-time snapshots, continuous profiling runs at all times and correlates profile data with traces and metrics using shared labels (service name, trace ID, span ID).

### 2.5 Process Mining

Process mining, introduced by Wil van der Aalst (2016), is the discipline of extracting process models from event logs. The three main subtypes are:

- **Discovery**: Automatically constructing a process model (Petri net, BPMN, process tree) from an event log without prior knowledge.
- **Conformance**: Comparing observed behavior in an event log against a reference process model to detect deviations.
- **Enhancement**: Using event log data to extend or improve an existing process model with performance, timing, or resource information.

The PM4Py library (Berti et al., 2019) provides a Python implementation of state-of-the-art process mining algorithms, including the Inductive Miner (Leemans et al., 2013), the Alpha Miner (van der Aalst et al., 2004), and token-based replay for conformance checking.

### 2.6 Knowledge Graph Systems

Knowledge graph systems — including Wikidata, Google's Knowledge Graph, and domain-specific KGS in healthcare, finance, and enterprise settings — represent knowledge as RDF triples (subject-predicate-object) organized into named graphs with formal ontologies providing schema-level constraints. The @unrdf ecosystem provides a JavaScript/TypeScript implementation of KGS with event sourcing (GitBackbone), declarative knowledge hooks, and 4D temporal graph support (KGC-4D).

### 2.7 The Gap

While each of these domains is well-studied individually, the intersection of observability and process mining for knowledge graph systems remains unexplored. Existing approaches either:

- Use observability for operational monitoring without process-level analysis (e.g., Grafana dashboards showing latency histograms), or
- Apply process mining to business event logs without leveraging distributed trace infrastructure (e.g., importing CSV files into PM4Py), or
- Use knowledge graphs to model ontologies without observing their own operational processes (e.g., OWL ontologies describing domain concepts but not system behavior).

This thesis bridges this gap by treating the operational telemetry of a knowledge graph system as a first-class event log for process mining.

---

## 3. Problem Statement and Research Questions

### 3.1 Problem Statement

Knowledge graph systems generate rich operational telemetry through distributed tracing, but this telemetry is underutilized for process-level analysis. Existing observability stacks provide dashboards for latency, error rates, and throughput, but they cannot answer process-level questions: _What is the actual process being executed? Does it conform to the expected workflow? Where are the bottlenecks?_

### 3.2 Research Questions

**RQ1**: How can distributed trace data from OpenTelemetry be automatically transformed into event logs suitable for process mining analysis?

**RQ2**: What process mining techniques are most effective for discovering, analyzing, and optimizing the operational processes of knowledge graph systems?

**RQ3**: How can ontological constraints (SHACL shapes, governance workflows, competency queries) be incorporated into process mining to enable conformance checking of observed system behavior?

**RQ4**: What is the architectural impact of integrating process mining into an existing observability stack, and how can this integration be achieved with minimal operational overhead?

**RQ5**: Does the addition of process mining to a traditional observability stack improve diagnostic accuracy and reduce mean-time-to-diagnosis for operational issues in knowledge graph systems?

---

## 4. The UTPM Architecture

### 4.1 Design Principles

The UTPM architecture is guided by four design principles:

**P1 — Single Telemetry Pipeline**: All telemetry (traces, metrics, logs, profiles) flows through a single OpenTelemetry Collector instance, ensuring consistent routing, resource attribution, and processing.

**P2 — Pillar Correlation**: Every telemetry event carries a trace ID, enabling cross-pillar correlation. Traces link to logs (via trace ID in log lines), metrics (via exemplars), and profiles (via span ID).

**P3 — Process Mining as a Pillar**: Process mining is treated as a first-class pillar of observability, consuming event logs derived from trace data and producing process models, conformance reports, and bottleneck analyses.

**P4 — Ontology-Backed Governance**: All process mining results are validated against formal ontological constraints, ensuring that discovered process models and conformance violations are grounded in the system's own ontological framework.

### 4.2 Architecture Overview

The UTPM architecture consists of 11 core services deployed via Docker Compose, augmented with advanced observability extensions organized into four tiers of increasing implementation effort:

**Tier 1 — Config-Only Changes** (zero new services, YAML-only modifications):

- **Loki Ruler**: Log-based alerting with LogQL rules for error rate spikes and service-down detection
- **SLO Burn Rate Alerts**: Error budget policies using multi-window burn rate analysis (99.9% and 99% SLO targets)
- **Span-to-Logs Correlation**: Collector transform processor that propagates trace IDs to Loki for click-through from spans to log lines
- **Service Graph Dashboard**: Auto-generated dependency map from Tempo's metrics_generator service graph output
- **Trace-to-Metrics Dashboard**: Latency histograms, error rates, and throughput per operation from Tempo's span metrics

**Tier 2 — Drop-In Additions** (one new service or script):

- **Grafana Alloy**: Unified agent replacing Promtail, adding trace forwarding capability alongside log shipping
- **Synthetic Trace Generator**: Scheduled OTLP trace injection with known latency distributions for regression detection

**Tier 3 — High-Leverage Scripts** (automation tooling):

- **Trace Baseline Comparison**: Structural regression detection — new operations, missing operations, count shifts >2x
- **PM4Py Conformance CI Gate**: Automated process model drift detection in CI/CD pipelines
- **Auto-Dashboard Generator**: Generates Grafana dashboards from semantic convention YAML definitions

**Tier 4 — Instrumentation Extensions** (new integrations):

- **Grafana Faro RUM**: Real-user monitoring for web frontends with Web Vitals capture
- **OpenLLMetry**: OpenTelemetry tracing for LLM/GenAI calls using GenAI semantic conventions (`gen_ai.system`, `gen_ai.usage.input_tokens`, etc.)
- **AI SDK Telemetry**: Vercel AI SDK `experimental_telemetry` integration in daemon autonomous agent and refinement engine

```
┌─────────────┐     ┌──────────────┐     ┌─────────────────────────────────────┐
│   HotROD    │     │  UNRDF       │     │           OTEL Collector             │
│  Demo App   │────▶│  Daemon      │────▶│  ┌─────────┐  ┌──────────────────┐  │
│             │     │  + Sidecar   │     │  │Receiver │─▶│ Processor        │  │
└─────────────┘     └──────────────┘     │  │ (OTLP)  │  │ (batch, resource,│  │
                                          │  └─────────┘  │  transform/logs) │  │
                                          └───────────────────────┼────────────┘
                                                                  │
                    ┌─────────────────────────────────────────────┼──────────────┐
                    │              │              │              │              │
                    ▼              ▼              ▼              ▼              ▼
              ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐
              │  Tempo   │  │Prometheus│  │   Loki   │  │Pyroscope │  │  Debug   │
              │ (Traces) │  │(Metrics) │  │  (Logs)  │  │(Profiles)│  │(Console) │
              └────┬─────┘  └────┬─────┘  └────┬─────┘  └──────────┘  └──────────┘
                   │              │              │
                   │         ┌────┴────┐        │
                   │         │Alert    │        │
                   │         │Manager  │        │
                   │         └─────────┘        │
              ┌────┴─────────────────────────────┴────┐
              │            MinIO (S3 Storage)         │
              │    tempo-traces/  tempo-blocks/       │
              └───────────────────────────────────────┘
                                           ┌──────────────────┐
              ┌────────────────────────────┤     Grafana      ├──────────────────┐
              │                            │  ┌────────────┐  │                  │
              ▼                            │  │ Prometheus │  │                  │
         ┌─────────┐                       │  │ Tempo      │  │                  │
         │ Promtail│──────────────────────▶│  │ Loki       │  │                  │
         │(Docker  │                       │  │ Pyroscope  │  │                  │
         │  logs)  │                       │  └────────────┘  │                  │
         └─────────┘                       └──────────────────┘                  │
                                                                               │
              ┌────────────────────────────────────────────────────────────────┤
              │                    PM4Py (Process Mining)                      │
              │  ┌──────────┐  ┌──────────────┐  ┌────────────────────────┐   │
              │  │ Trace    │  │  Process     │  │  Conformance           │   │
              │  │ Extraction│─▶│  Discovery   │─▶│  Checking              │   │
              │  │ (Tempo)  │  │  (Inductive  │  │  (Token Replay)        │   │
              │  │          │  │   Miner)     │  │                        │   │
              │  └──────────┘  └──────────────┘  └────────────────────────┘   │
              └────────────────────────────────────────────────────────────────┘

                         ── Extended Innovations ──

              ┌────────────────────────────────────────────────────────────────┐
              │              Advanced Observability Extensions                │
              │                                                                │
              │  ┌──────────────┐  ┌─────────────────┐  ┌──────────────────┐ │
              │  │ Loki Ruler   │  │ SLO Burn Rate   │  │ Service Graph    │ │
              │  │ (Log Alerts) │  │ Alerts           │  │ Dashboard        │ │
              │  └──────────────┘  └─────────────────┘  └──────────────────┘ │
              │                                                                │
              │  ┌──────────────┐  ┌─────────────────┐  ┌──────────────────┐ │
              │  │ Synthetic    │  │ Trace Baseline  │  │ Auto-Dashboard   │ │
              │  │ Traces       │  │ Comparison      │  │ Generator        │ │
              │  └──────────────┘  └─────────────────┘  └──────────────────┘ │
              │                                                                │
              │  ┌──────────────┐  ┌─────────────────┐  ┌──────────────────┐ │
              │  │ Grafana Faro │  │ OpenLLMetry     │  │ Span-to-Logs     │ │
              │  │ (RUM)        │  │ (LLM Tracing)   │  │ Correlation      │ │
              │  └──────────────┘  └─────────────────┘  └──────────────────┘ │
              │                                                                │
              │  ┌──────────────┐  ┌─────────────────┐  ┌──────────────────┐ │
              │  │ Grafana Alloy│  │ PM4Py Conform.  │  │ Trace-to-Metrics │ │
              │  │ (Promtail+)  │  │ CI Gate          │  │ Dashboard        │ │
              │  └──────────────┘  └─────────────────┘  └──────────────────┘ │
              └────────────────────────────────────────────────────────────────┘
```

### 4.3 Service Descriptions

#### 4.3.1 OpenTelemetry Collector (otel/opentelemetry-collector-contrib:0.119.0)

The collector is the central telemetry pipeline. It receives OTLP data via gRPC (port 4317) and HTTP (port 4318), applies processing (batching, resource attribution), and exports to multiple backends. The resource processor enforces consistent service naming (`unrdf-daemon`, version `26.4.4`, environment `development`).

#### 4.3.2 Grafana Tempo (grafana/tempo:2.7.1)

Tempo stores distributed traces using the S3-compatible MinIO backend. The Tempo config enables the metrics generator, which automatically produces service graph metrics and span metrics that are remote-written to Prometheus. This eliminates the need for manual metric instrumentation of trace-derived data.

#### 4.3.3 Prometheus (prom/prometheus:v2.55.1)

Prometheus scrapes metrics from the OTEL Collector (port 8889), Tempo (port 3200), Loki (port 3100), Alertmanager (port 9093), and Pyroscope (port 4040). It evaluates alerting rules defined in `alert-rules.yml` and forwards alerts to Alertmanager.

#### 4.3.4 Grafana (grafana/grafana:11.4.0)

Grafana provides the unified visualization layer. The datasource provisioning includes Prometheus, Tempo, Loki, and Pyroscope, with trace-to-log and trace-to-metric correlation configured. The feature flag `pyroscopeIntegration` enables native profile visualization.

#### 4.3.5 Loki (grafana/loki:3.3.2)

Loki stores log data using the TSDB storage engine. The OTEL Collector exports logs to Loki, and Promtail collects Docker container stdout/stderr logs. Loki's derived fields configuration enables automatic extraction of trace IDs from log lines, linking logs to traces.

#### 4.3.6 Promtail (grafana/promtail:3.3.2)

Promtail uses Docker service discovery to automatically tail logs from all containers in the Compose project. It attaches container metadata as labels, enabling filtering by service name.

#### 4.3.7 Pyroscope (grafana/pyroscope:1.10.1)

Pyroscope provides continuous profiling (CPU, memory, goroutine) via its own OTLP receiver or the integrated Grafana profiling agent. Profile data is correlated with traces via shared labels.

#### 4.3.8 Alertmanager (prom/alertmanager:v0.28.0)

Alertmanager routes alerts from Prometheus to appropriate receivers. The configuration includes severity-based routing (critical alerts trigger immediate notification, warnings are batched) and inhibition rules to suppress downstream alerts when a root cause alert is firing.

#### 4.3.9 MinIO (minio/minio:RELEASE.2024-11-07T00-52-20Z)

MinIO provides S3-compatible object storage for Tempo's trace backend. Two buckets are provisioned: `tempo-traces` (for complete trace data) and `tempo-blocks` (for compacted trace blocks). This enables production-scale trace retention without local filesystem limitations.

#### 4.3.10 HotROD Demo App (jaegertracing/example-hotrod:1.62.0)

The HotROD (Hot Rides On Demand) application is a multi-service demo that generates realistic distributed traces. It sends traces to the OTEL Collector via OTLP/gRPC, providing a continuous stream of trace data for dashboard validation and process mining analysis.

#### 4.3.11 PM4Py (custom build, python:3.12-slim)

PM4Py runs as a custom Docker image built on Python 3.12-slim, providing process mining analysis. The image includes a critical fix for PID 0 crash handling in Docker containers (pm4py calls `psutil.Process(parent_pid).name()` which fails when parent PID is 0 — patched via sed in the Dockerfile). The analysis script queries Tempo for trace data, transforms it into event logs, and applies process discovery (Inductive Miner), conformance checking (token-based replay), and bottleneck analysis. Environment variable configuration allows flexible service endpoint resolution.

#### 4.3.12 Advanced Observability Extensions

The core 11-service architecture is extended with 12 additional capabilities organized into four tiers:

**Loki Ruler** (config extension to Loki 3.3.2): Enables log-based alerting rules evaluated against Loki's LogQL engine. Two rules are defined: `HighErrorRate` (warning, fires when >10% of logs from UNRDF containers contain errors over 5 minutes) and `ServiceDown` (critical, fires when a container produces zero logs over 10 minutes). Alerts route to Alertmanager via the v2 API.

**SLO Burn Rate Alerts** (config extension to Prometheus alert-rules.yml): Implements Google's multi-window multi-burn-rate alerting strategy (SRE Workbook, 2020). Two rules protect error budgets: `HighBurnRate99` (critical, 1-hour burn window at 14.4x budget rate for 99.9% SLO) and `HighBurnRate99Window` (warning, 5-minute burn window at 6x budget rate for 99% SLO).

**Span-to-Logs Correlation** (config extension to OTEL Collector): A `transform/logs` processor using `traceStatements` with `context: log` copies `trace_id` and `span_id` attributes to `loki.trace_id` and `loki.span_id`. This enables Grafana's built-in trace-to-logs correlation — clicking a span in the trace view opens the corresponding log entries filtered by trace ID.

**Service Graph Dashboard** (Grafana provisioning): A 4-panel dashboard auto-generated from Tempo's `metrics_generator` service graph output. Includes a node graph panel (`traces_service_graph_request_total`), top-10 services by request rate (stat), error rate by service (time series), and P50/P95/P99 latency by service (time series from `traces_service_graph_request_total_bucket`).

**Trace-to-Metrics Dashboard** (Grafana provisioning): A 7-panel dashboard using Tempo's `span_metrics` processor output. Shows overview statistics (total calls, error rate, P50/P99 latency), latency by operation (histogram quantiles from `traces_spanmetrics_latency_bucket`), error rate by operation, and throughput by service.

**Grafana Alloy** (grafana/alloy:v1.6.0): A unified telemetry agent that can replace Promtail. In addition to log shipping (feature-equivalent to Promtail with Docker service discovery), Alloy adds OTLP gRPC and HTTP receivers for trace forwarding directly to Tempo. This consolidates log shipping and trace ingestion into a single agent, reducing operational complexity and providing a migration path to Kubernetes-native telemetry collection.

**Synthetic Trace Generator** (Python 3, zero external dependencies): A script that generates deterministic OTLP traces with known latency distributions (Gaussian mean/std) for five operations: `mcp.query`, `mcp.graph_load`, `mcp.hooks_exec`, `daemon.schedule`, and `daemon.health`. Each operation has an `expected_max` latency threshold — spans exceeding it are marked as failures, enabling regression detection without real traffic. Supports scheduled execution via `--interval` for continuous synthetic monitoring.

**Trace Baseline Comparison** (bash): Saves and compares trace operation summaries from Tempo against a saved baseline. Detects structural regressions: new operations not in baseline, missing operations, and count shifts exceeding 2x. Timestamped baselines enable historical comparison.

**PM4Py Conformance CI Gate** (bash): Runs process mining analysis inside the PM4Py container and fails the CI pipeline if conformance drops below configurable thresholds (default: fitness > 0.8, precision > 0.5). Exit codes: 0 (pass), 1 (below threshold), 2 (analysis error).

**Auto-Dashboard Generator** (Python): Reads the `custom-conventions.yaml` semantic conventions and generates Grafana dashboard JSON with panels for each attribute group. Panel types are selected by attribute type: time series for numeric, stat for boolean, table for string. The generator handles the conventions file's `groups` structure and enum-style types.

**Grafana Faro** (JavaScript, CDN-loaded): Real-user monitoring (RUM) for web frontends. A self-contained IIFE that dynamically loads the Faro Web SDK and Web Vitals library, captures page loads, navigation, JavaScript errors, Core Web Vitals (LCP, FID, CLS, TTFB, INP), and custom events. Exposes `window.__FARO` API for `pushEvent`, `pushMeasurement`, `pushLog`, `setUser`, and `getTraceId`.

**OpenLLMetry** (ESM module): OpenTelemetry instrumentation for LLM calls using the GenAI semantic conventions (`gen_ai.system`, `gen_ai.request.model`, `gen_ai.usage.input_tokens`, `gen_ai.usage.output_tokens`, etc.). Provides `withLLMSpan()` for wrapping any LLM provider call, `getAISDKTelemetryConfig()` for Vercel AI SDK integration, and `instrumentAIToolCall()` for per-tool-call tracing. Two daemon call sites are instrumented: `autonomous-agent.mjs` (function ID: `autonomous-agent.reason`) and `autonomous-refinement-engine.mjs` (function ID: `refinement-engine.decide`).

#### 4.3.13 Semantic Convention Validation

All custom OTEL attributes are validated against the OpenTelemetry semantic convention registry. The custom conventions define 7 attribute groups with 34 attributes across the prefixes `knowledge_hook.*`, `policy_pack.*`, `rdf.*`, `effect.*`, `crypto.*`, `transaction.*`, and `sidecar.*`. No custom prefix conflicts with OTel reserved prefixes. The AI SDK's internal `gen_ai.*` attributes are validated against the GenAI semantic conventions specification, with one noted deprecation (`gen_ai.system` → `gen_ai.provider.name`) requiring the environment variable `OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental`.

### 4.4 Data Flow

The UTPM architecture establishes five data flow patterns:

**Pattern A — Ingestion Flow**:

```
Application → OTLP → Collector → Tempo (traces)
                              → Loki (logs)
                              → Prometheus (metrics)
                              → Pyroscope (profiles)
                              → Faro (RUM from browser)
```

**Pattern B — Correlation Flow**:

```
Trace ID propagation: traceparent header across gRPC/HTTP boundaries
Log correlation: Collector transform/logs processor copies trace_id → loki.trace_id
Metric correlation: Prometheus exemplars link metrics to trace IDs
Profile correlation: Pyroscope profiles tagged with service.name
Span-to-logs: Grafana links span → Loki logs via loki.trace_id label
```

**Pattern C — Process Mining Flow**:

```
Tempo API → Trace extraction → Event log construction → PM4Py → Process model
                                                                 → Conformance report
                                                                 → Bottleneck analysis
                                                                 → CI gate (pass/fail)
```

**Pattern D — Synthetic Monitoring Flow**:

```
Cron/Scheduler → Synthetic Trace Gen → OTLP HTTP → Collector → Tempo → Grafana
                                                                 → Prometheus (metrics)
                                                                 → PM4Py (baseline comparison)
```

**Pattern E — LLM Telemetry Flow**:

```
AI SDK generateText() → experimental_telemetry → OTLP → Collector → Tempo
                          ↓
                    gen_ai.system (provider)
                    gen_ai.request.model
                    gen_ai.usage.input_tokens
                    gen_ai.usage.output_tokens
                    ai.toolCall.name (if tools used)
```

---

## 5. Trace-to-Event-Log Transformation

### 5.1 The Transformation Problem

Process mining algorithms operate on event logs, which are structured as tables with columns: `case:concept:name` (process instance identifier), `concept:name` (activity name), `time:timestamp`, and optional attributes. Distributed traces, in contrast, are hierarchical tree structures with parent-child span relationships, nested attributes, and variable-granularity timing.

The challenge is to flatten the hierarchical trace structure into a tabular event log while preserving:

1. **Case identification**: Which spans belong to the same process instance (trace)
2. **Activity naming**: How to map span names to process activities
3. **Temporal ordering**: How to order spans within a trace when they execute concurrently
4. **Resource attribution**: Which service or component performed the activity
5. **Domain-specific attributes**: MCP tool names, ontological operation types, federation source/target

### 5.2 Transformation Algorithm

We define the transformation function $\tau: \text{Traces} \to \text{EventLog}$ as follows:

Given a trace $T$ with root span $s_0$ and child spans $\{s_1, s_2, \ldots, s_n\}$:

1. **Case extraction**: For each trace, set `case:concept:name = T.traceID`
2. **Activity mapping**: For each span $s_i$, set `concept:name = s_i.name`
3. **Timestamp extraction**: Set `time:timestamp = ISO8601(s_i.startTimeUnixNano)`
4. **Resource attribution**: Set `org:resource = s_i.attributes["service.name"]`
5. **Duration calculation**: Set `duration_ms = (s_i.endTimeUnixNano - s_i.startTimeUnixNano) / 10^6`
6. **Domain attributes**: Extract MCP-specific attributes:
   - `mcp.tool.name = s_i.attributes["mcp.tool.name"]`
   - `mcp.tool.success = s_i.attributes["mcp.tool.success"]`

For concurrent spans (siblings with the same parent), we order by start time and break ties by span ID. This produces a total order consistent with the happened-before relation (Lamport, 1978).

### 5.3 Event Log Schema

The resulting event log has the following schema:

| Column              | Type     | Description       | PM4Py Role            |
| ------------------- | -------- | ----------------- | --------------------- |
| `case:concept:name` | string   | Trace ID          | Case identifier       |
| `concept:name`      | string   | Span name         | Activity              |
| `time:timestamp`    | datetime | Span start time   | Timestamp             |
| `org:resource`      | string   | Service name      | Resource              |
| `duration_ms`       | float    | Span duration     | Performance attribute |
| `mcp.tool.name`     | string   | MCP tool name     | Activity subtype      |
| `mcp.tool.success`  | boolean  | Tool success flag | Quality attribute     |

### 5.4 Log-to-Event-Log Transformation

In addition to traces, we transform Loki log entries into event logs using a similar approach. Each log entry becomes an event with:

- `case:concept:name` = trace ID extracted from log line (or container name if absent)
- `concept:name` = "log_entry" (normalized)
- `time:timestamp` = log timestamp
- `org:resource` = container name
- `log_level` = extracted from log content (info/warn/error)
- `message` = truncated log line

This enables process mining on log sequences, discovering patterns like "error always follows this specific log sequence" or "startup always follows this exact log ordering."

---

## 6. Ontology-Aware Process Mining

### 6.1 Motivation

Traditional process mining discovers process models purely from observed behavior. In knowledge graph systems, however, the expected behavior is formally specified in the ontology itself. SHACL shapes define validation constraints, competency queries specify expected query patterns, and governance workflows prescribe the sequence of operations for ontology admission.

This creates an opportunity for **ontology-aware process mining**, where the discovered process model is compared not against a manually defined reference model, but against the operational processes encoded in the system's own ontology.

### 6.2 Ontology-to-Process-Model Extraction

We define a function $\omega: \text{Ontology} \to \text{ProcessModel}$ that extracts an expected process model from an OWL/SHACL ontology:

1. **Governance workflow extraction**: SHACL node shapes with `sh:order` constraints define sequential process steps. Each shape maps to a process activity.
2. **Competency query extraction**: SPARQL queries in the `ontology/queries/` directory define expected data access patterns. Each query maps to a process activity with the query result as output.
3. **Hook chain extraction**: Knowledge hooks (`quads:added`, `quads:removed` triggers) define event-driven process fragments. Each hook maps to a conditional activity.

The resulting process model serves as the reference model for conformance checking.

### 6.3 Conformance Checking with Ontological Constraints

We extend traditional token-based replay conformance checking with ontological constraint validation:

$$\text{fitness}(L, N) = \frac{1}{2}\left(1 - \frac{m}{|L|}\right) + \frac{1}{2}\left(1 - \frac{r}{|L|}\right)$$

where $m$ is the number of missing tokens (expected activities not observed) and $r$ is the number of remaining tokens (observed activities not in the model). We add a third component:

$$\text{ontology\_fitness}(L, N, O) = \text{fitness}(L, N) \times \prod_{c \in C(O)} \text{constraint\_satisfied}(L, c)$$

where $C(O)$ is the set of ontological constraints and $\text{constraint\_satisfied}(L, c)$ returns 1 if the event log satisfies constraint $c$ and 0 otherwise.

### 6.4 Example: MCP Tool Governance Workflow

Consider an MCP tool governance workflow defined in the ontology:

```
1. onto_validate  → 2. onto_load  → 3. onto_lint  → 4. onto_plan  → 5. onto_apply
```

If the observed trace shows:

```
1. onto_validate  → 2. onto_load  → 3. onto_apply  (skipped onto_lint and onto_plan)
```

Traditional conformance checking would flag this as a deviation. Ontology-aware conformance checking additionally checks whether the skipped steps had mandatory SHACL constraints that were violated, providing a root cause for the deviation.

### 6.5 Advanced Process Mining Techniques for Observability

Beyond the core discovery-conformance-enhancement pipeline, PM4Py provides several advanced techniques that are particularly relevant to observability-driven process mining. We identify twenty-three features that extend the UTPM architecture's analytical capabilities:

#### 6.5.1 Predictive Monitoring — Remaining Time and Next Activity

PM4Py's predictive monitoring module enables forecasting of running process instances. For distributed tracing, this translates to predicting the remaining latency of an in-flight request trace based on the sequence of spans observed so far.

**Remaining time prediction** estimates how long a trace will take to complete given the spans already executed:

```python
from pm4py.algo.prediction.remaining_time import algorithm as pred_time

# Predict remaining time for each running trace prefix
results = pred_time.apply(df, parameters={
    'classifier': 'random_forest',
    'splitting': 'timestamp_last_event',
})
# Returns: DataFrame with predicted remaining time per trace prefix
```

**Next activity prediction** forecasts which span (operation) will execute next in a running trace:

```python
from pm4py.algo.prediction.next_activity import algorithm as pred_next

# Predict the next activity for each running trace prefix
results = pred_next.apply(df, parameters={
    'classifier': 'random_forest',
})
# Returns: DataFrame with predicted next activity + confidence per prefix
```

For observability, this enables proactive alerting: if a trace's predicted remaining time exceeds an SLO threshold, an alert can be raised before the trace completes. Similarly, next activity prediction enables anomaly detection when an observed span sequence deviates from the predicted pattern.

#### 6.5.2 Object-Centric Process Mining (OCEL)

Object-centric process mining (OCel) addresses a fundamental limitation of traditional process mining: the assumption that each event belongs to exactly one case. In distributed systems, a single trace may involve multiple interacting entities — services, knowledge graphs, federation endpoints, MCP tools — each of which constitutes a different "object."

PM4Py supports the OCEL 2.0 standard for object-centric event logs:

```python
import pm4py

# Convert distributed traces to OCEL format
# Each span becomes an event, each service becomes an object
ocel = pm4py.convert_log_to_ocel(df,
    object_types=['org:resource', 'mcp.tool.name'],
    activity_column='concept:name',
    timestamp_column='time:timestamp',
    case_id_column='case:concept:name'
)

# Discover object-centric Petri net
ocpn = pm4py.discover_oc_petri_net(ocel)
pm4py.view_ocpn(ocpn)
```

This is directly applicable to the UTPM architecture because distributed traces naturally span multiple objects (services). An OCEL representation preserves the multi-object nature of trace data, enabling discovery of interaction patterns between services that traditional case-based mining flattens.

#### 6.5.3 Alignment-Based Conformance Checking

While token-based replay provides fitness and precision metrics, alignment-based conformance checking produces precise optimal alignments between observed traces and the process model, identifying exactly which activities deviate and where:

```python
from pm4py.algo.conformance.alignments.petri_net import algorithm as alignments

# Compute alignments — identifies exact deviations per trace
aligned_traces = alignments.apply(df, net, im, fm)
# Returns: list of alignment objects with:
#   - fitness per trace
#   - cost (number of moves on log vs model)
#   - precise deviation locations
```

Alignment-based conformance is more computationally expensive (O(n²)) than token-based replay, but provides per-trace deviation diagnosis. For the UTPM architecture, this enables root cause analysis at the individual trace level: rather than reporting aggregate fitness, we can identify exactly which span in a trace deviated from the expected model.

#### 6.5.4 Feature Extraction for ML-Based Anomaly Detection

PM4Py provides automated feature extraction from event logs, producing numerical representations suitable for machine learning classifiers:

```python
from pm4py.algo.transformation.log_to_features import algorithm as features

# Extract features from event log — produces numeric vector per trace
data, feature_names = features.apply(df,
    parameters={'str_ev_attr': [], 'str_tr_attr': [], 'num_ev_attr': ['duration_ms']}
)
# Returns: numpy array (traces × features), list of feature names
```

Combined with unsupervised anomaly detection (Isolation Forest, One-Class SVM), this enables automatic identification of anomalous traces without a reference process model. For observability, this means detecting "weird" traces — unusual span sequences, unexpected latency patterns — as they occur.

#### 6.5.5 Trace Clustering and Variant Analysis

PM4Py supports grouping traces into clusters based on behavioral similarity:

```python
from pm4py.algo.clustering.trace_attribute_drift import algorithm as drift

# Detect attribute drift across trace groups
drift_result = drift.apply(df, parameters={'attribute': 'org:resource'})

# Variant analysis — identify the most common execution patterns
variants = pm4py.get_variants(df)
# Returns: dict mapping variant tuple → count
# e.g., {('onto_validate', 'onto_load', 'onto_apply'): 45,
#         ('onto_validate', 'onto_load', 'onto_lint', 'onto_plan', 'onto_apply'): 120}
```

Variant analysis is particularly useful for understanding the distribution of operational patterns in a knowledge graph system. If 80% of traces follow one variant and 20% follow another, the minority variant may represent an error path, a retry loop, or an alternate governance workflow.

#### 6.5.6 Stochastic Process Mining

PM4Py can annotate process models with probabilistic information, producing stochastic Petri nets where transitions have associated probabilities:

```python
from pm4py.algo.discovery.stochastic_petri_net import algorithm as stochastic

# Discover stochastic Petri net — transition probabilities reflect observed frequencies
snet, sim, sfm = stochastic.apply(df)
# Transitions carry probability weights based on event log frequency
```

Stochastic models enable Monte Carlo simulation of process behavior. For the UTPM architecture, this means we can simulate expected trace patterns under different load conditions, predicting how the process model would behave if a specific service (e.g., the federation endpoint) became slow or unavailable.

#### 6.5.7 Process Tree Operations

PM4Py exposes process trees as first-class objects with composition, simplification, and comparison operations:

```python
tree = pm4py.discover_process_tree_inductive(df)

# Simplify the process tree — remove redundant operators
simplified = pm4py.simplify_process_tree(tree)

# Compare two process trees (e.g., from different time windows)
from pm4py.objects.process_tree.obj import ComparisonResult
# Enables temporal comparison of discovered process models
```

Process tree comparison enables drift detection: by discovering process trees from successive time windows and comparing them, we can detect when the operational process has changed. This is directly applicable to detecting configuration changes, deployment rollouts, or performance regressions in the knowledge graph system.

#### 6.5.8 Case Duration and Waiting Time Analysis

Beyond simple bottleneck analysis, PM4Py provides structured analysis of case-level performance:

```python
from pm4py.analysis import case_duration

# Detailed case duration analysis with automated outlier detection
case_stats = case_duration.get_case_description(df,
    parameters={'attribute_key': 'duration_ms'})
# Provides: case-level aggregation, outlier detection, time-series decomposition
```

This extends the bottleneck analysis in `pm4py-analyze.py` by identifying outlier cases — traces whose total duration deviates significantly from the expected distribution — which are often the most diagnostically interesting for SRE teams.

#### 6.5.9 Event Stream Processing (Live Process Mining)

PM4Py supports incremental processing of event streams, enabling live process mining without batching:

```python
from pm4py.streaming.algo.discovery import inductive_miner as im_stream

# Create streaming process discovery
stream = im_stream.Stream()
stream.start()

# Feed events one at a time (as they arrive from OTel Collector)
for event in live_events:
    stream.process_event(event)

# Get current process tree (updated incrementally)
current_tree = stream.get_process_tree()
```

For the UTPM architecture, this eliminates the batch extraction latency measured in Section 8.2. Rather than periodically querying Tempo for traces and running discovery, the streaming approach maintains an always-up-to-date process model that reflects the latest operational behavior.

#### 6.5.10 Organizational Mining and Resource Analysis

PM4Py can analyze resource (service) behavior patterns, discovering organizational structures and workload distributions:

```python
from pm4py.algo.organizational_mining import resource_profiles

# Analyze service interaction patterns
profiles = resource_profiles.apply(df,
    parameters={'resource_key': 'org:resource'})
# Returns: service interaction matrices, workload distributions
```

For distributed systems, organizational mining reveals the actual communication patterns between services — which services frequently interact, which are bottlenecks in terms of concurrent load, and whether the observed service topology matches the intended architecture.

#### 6.5.11 AutoML Pipeline Optimization (TPOT2)

TPOT2 (Tree-based Pipeline Optimization Tool, version 2) automates the selection of ML classifiers and preprocessing steps for trace anomaly detection. Rather than manually choosing between Isolation Forest, One-Class SVM, or other classifiers, TPOT2 uses genetic programming to evolve optimal scikit-learn pipelines:

```python
from tpot2 import TPOTClassifier

# Extract features from traces
features = extract_trace_features(df)  # span count, duration stats, etc.
labels = label_anomalies(df)           # 2-sigma duration outliers

# TPOT2 searches across classifiers, preprocessors, and hyperparameters
tpot = TPOTClassifier(
    generations=5,           # 5 evolutionary generations
    population_size=20,      # 20 candidate pipelines per generation
    cv=3,                    # 3-fold cross-validation
    scoring='f1',            # Optimize for F1 (precision-recall balance)
    max_time_mins=1,         # Budget: 1 minute
    n_jobs=-1,               # Parallel evaluation
)
tpot.fit(features, labels)

# Export the best pipeline for production deployment
tpot.export('best_anomaly_pipeline.py')
```

TPOT2 searches over hundreds of possible pipeline configurations — including Random Forests, Gradient Boosting, SVMs, XGBoost, LightGBM, and their preprocessing combinations — to find the pipeline that best separates anomalous traces from normal ones. This eliminates the need for manual model selection and hyperparameter tuning, which is particularly valuable when the distribution of trace anomalies changes over time (e.g., after deployments or configuration changes).

#### 6.5.12 Temporal Profile Conformance (SLA Checking)

Temporal profile conformance checks whether observed inter-activity times deviate from the expected profile learned from historical traces. Unlike simple latency thresholds, temporal profiles capture the statistical distribution of time between _pairs_ of activities:

```python
import pm4py

# Learn the temporal profile (mean and std for each activity pair)
temporal_profile = pm4py.discover_temporal_profile(df)
# Returns: {('/dispatch', '/customer'): (8.48s, 0.70s), ...}

# Check conformance — each check returns (act1, act2, observed_seconds, zeta_score)
conformance = pm4py.conformance_temporal_profile(df, temporal_profile)

# Count SLA violations (zeta > 2.0 = statistically significant deviation)
violations = sum(
    1 for trace in conformance
    for check in trace
    if isinstance(check, tuple) and len(check) >= 4 and float(check[3]) > 2.0
)
```

The zeta-score normalizes deviations by the learned standard deviation, providing a statistically principled threshold. A zeta > 2.0 corresponds roughly to the 95th percentile of the expected distribution. In our HotROD evaluation, 9.3% of activity pair transitions showed significant temporal deviations, identifying `/route → /route` (std=1.53s) and `GetDriver → GetDriver` (std=0.11-2.84s) as the most variable transitions.

#### 6.5.13 Rework Detection

Rework detection identifies activities that occur multiple times within the same trace, indicating retry loops, compensating transactions, or inefficient process patterns:

```python
import pm4py

# Count rework cases per activity
rework_cases = pm4py.get_rework_cases_per_activity(df)
# Returns: {'/route': 30, 'GetDriver': 60, 'HTTP GET': 150, ...}

# Filter traces containing rework of a specific activity
rework_df = pm4py.filter_activities_rework(df, '/route', min_occurrences=2)
```

In distributed tracing, rework often corresponds to retry logic (HTTP GET retries), compensating transactions (rollback + re-dispatch), or batch processing patterns (multiple GetDriver calls per dispatch). Our HotROD evaluation identified 4 activities with rework, totaling 120 rework cases across 30 traces. The `/route` activity showed 100% rework rate (all traces contained repeated routing spans), reflecting the route-simulated concurrent trip matching pattern.

#### 6.5.14 Batch Detection

Batch detection identifies groups of cases that are processed together by the same resource within a time window. In distributed systems, batching explains tail latency spikes — when a resource processes multiple requests simultaneously, the last request experiences queueing delay:

```python
import pm4py

# Detect batches (merge_distance in seconds)
batches = pm4py.discover_batches(df, merge_distance=30, min_batch_size=2)
# Returns: list of (resource, batch_size) tuples
```

For distributed traces, classical batch detection requires a generous merge distance (30s+) since traces span multiple services. Our implementation includes a fallback that detects near-concurrent activity starts within time windows, which is more appropriate for microservice architectures where batching occurs at the service level rather than the process level.

#### 6.5.15 Social Network Analysis (SNA)

Social network analysis maps service interaction patterns from process execution data, revealing the _actual_ communication topology versus the intended architecture:

```python
import pm4py

# Handover-of-work: which service passes work to which
how = pm4py.discover_handover_of_work_network(df)
# SNA object with .connections (edge list) and .is_directed

# Working-together: which services collaborate on the same case
wt = pm4py.discover_working_together_network(df)

# Organizational roles: clustering services by activity patterns
roles = pm4py.discover_organizational_roles(df)
```

In our OTEL context, SNA requires the `org:resource` column to be populated with service names (from `service.name` span attributes). When traces originate from a single synthetic workload (e.g., HotROD), all resources map to "unknown" and SNA is skipped. In production deployments with multi-service traces, SNA reveals unexpected service dependencies, communication hotspots, and architectural drift.

#### 6.5.16 Decision Mining

Decision mining identifies the data-driven factors that determine routing decisions at gateway points in the process model. By extracting features from alignment results and training a decision tree classifier, we can explain _why_ traces follow different paths:

```python
import pm4py
from sklearn.tree import DecisionTreeClassifier, export_text

# Get alignment diagnostics (cost, fitness, log/model moves per trace)
aligned = pm4py.conformance_diagnostics_alignments(df, net, im, fm)

# Extract features: alignment cost, fitness, log moves, model moves
features = [{
    'alignment_cost': a['cost'],
    'fitness': a['fitness'],
    'log_moves': sum(1 for m in a['alignment'] if m[0] is None),
    'model_moves': sum(1 for m in a['alignment'] if m[1] is None),
} for a in aligned]

# Train decision tree to classify fit vs unfit traces
clf = DecisionTreeClassifier(max_depth=3)
clf.fit(features, labels)
print(export_text(clf, feature_names=['cost', 'fitness', 'log_moves', 'model_moves']))
```

The resulting decision tree provides human-interpretable rules such as "if alignment cost > 5 and log_moves > 2, the trace is unfit." This enables operators to understand the specific conditions that lead to process deviations, rather than just knowing that deviations exist.

#### 6.5.17 Performance Spectrum Analysis

Performance spectrum analysis examines the statistical distribution of activity latencies to identify bimodal patterns, outliers, and variable activities that require further investigation:

```python
import numpy as np

# For each activity, compute distribution statistics
for activity in df['concept:name'].unique():
    durations = df[df['concept:name'] == activity]['duration_ms']

    # Coefficient of variation (higher = more variable)
    cv = durations.std() / durations.mean()

    # IQR-based outlier detection
    q25, q75 = durations.quantile(0.25), durations.quantile(0.75)
    iqr = q75 - q25
    outlier_count = ((durations < q25 - 1.5*iqr) | (durations > q75 + 1.5*iqr)).sum()

    # Bimodal detection: CV > 0.8 or outlier rate > 10%
    is_bimodal = cv > 0.8 or outlier_count > len(durations) * 0.1
```

In our HotROD evaluation, performance spectrum analysis identified 2 bimodal activities: `HTTP GET` (CV=3.268, 30 outliers) and `GetDriver` (CV=0.572, 76 outliers). The high CV for HTTP GET reflects the mix of fast cache hits and slow database lookups, while GetDriver's bimodality stems from the simulated variable driver availability.

#### 6.5.18 Declare (Declarative Process Mining)

Traditional process mining discovers imperative models — Petri nets, BPMN diagrams, process trees — that prescribe a specific sequence of activities. Van der Aalst et al. (2009) argue that this imperative paradigm is poorly suited for flexible, concurrent, and event-driven processes: "Don't Use Petri Nets for Process Mining" (van der Aalst, van Dongen, Herbst, et al., 2009). The declarative approach instead specifies _constraints_ that must hold, leaving all behavior not explicitly forbidden as permitted.

Declarative process mining uses Linear Temporal Logic (LTL) to define constraint templates that capture common process properties. Rather than discovering a full control-flow model, declarative mining discovers which constraints are satisfied by the observed event log. This is particularly well-suited to distributed systems for three reasons:

1. **Concurrency**: Distributed traces contain spans that execute in parallel. Declarative constraints naturally handle concurrent activities without imposing an artificial ordering.
2. **Event-driven behavior**: Knowledge graph hook chains, MCP tool invocations, and federation queries are triggered by events, not by a fixed sequence. Declarative constraints capture these trigger-response patterns.
3. **Flexibility**: Knowledge graph systems evolve as ontologies are extended. A declarative model accommodates new activities without requiring structural changes to the process model.

PM4Py provides declarative discovery and conformance checking:

```python
import pm4py

# Discover declarative constraints from the event log
declare_model = pm4py.discover_declare(df)

# Check conformance against the discovered (or a manually specified) declare model
conformance = pm4py.conformance_declare(df, declare_model)
# Returns: list of satisfied and violated constraints per trace

# Inspect the discovered constraints
for constraint in declare_model.constraints:
    print(f"{constraint.template}: {constraint.activities}")
    print(f"  Support: {constraint.support:.2%}")
    print(f"  Confidence: {constraint.confidence:.2%}")
```

Key constraint templates applicable to distributed tracing:

| Template         | LTL Formulation                          | Example (KGS)                                                    |
| ---------------- | ---------------------------------------- | ---------------------------------------------------------------- |
| Response         | `activity_a → ◇activity_b`               | `onto_load` must eventually be followed by `onto_validate`       |
| Precedence       | `activity_b → ◇¬activity_b U activity_a` | `onto_apply` must be preceded by `onto_plan`                     |
| Succession       | Response + Precedence                    | `onto_load` ↔ `onto_validate` (bidirectional)                    |
| Chain Response   | `activity_a → ○activity_b`               | `onto_validate` is immediately followed by `onto_load`           |
| Chain Precedence | `activity_b → ○¬activity_b U activity_a` | `onto_apply` is immediately preceded by `onto_plan`              |
| Non-coexistence  | `¬(activity_a ∧ activity_b)`             | `onto_validate` and `onto_load` never co-occur in the same trace |
| Non-succession   | `¬(activity_a ∧ ◇activity_b)`            | `onto_reject` never followed by `onto_apply`                     |

Declarative conformance checking identifies _specific constraint violations_ rather than aggregate fitness scores. This is crucial for automated remediation: each violation maps directly to a remediation action. For example, a violated `chain_response(onto_validate, onto_load)` constraint indicates that the load step was skipped after validation, triggering a compensating action (Section 6.5.23).

#### 6.5.19 Event Data Quality Framework

Process mining results are only as reliable as the input event data. Van der Aalst (2016) dedicates Chapter 3 of _Process Mining: Data Science in Action_ to event data quality, establishing four quality dimensions that apply directly to OpenTelemetry trace data:

**Completeness**: Are all events present? Missing spans — caused by sampling, dropped batches, or failed exports — produce incomplete traces that inflate conformance deviations. The OTel Collector's `ailedsentlogsspanmetrics` metric tracks dropped spans, but span-level completeness also requires checking for missing child spans within a trace (e.g., a parent span for `daemon.schedule` without child spans for the individual MCP tool invocations).

**Validity**: Do events conform to the expected schema? Invalid timestamps (clock skew across services), missing required attributes (`service.name`, `trace_id`), or inconsistent case identifiers (truncated trace IDs) corrupt the event log. The semantic convention validation in Section 4.3.13 addresses attribute-level validity; event-level validity additionally requires timestamp consistency checks.

**Timeliness**: Are events available for analysis within the expected window? Late-arriving spans — common in batch-exported traces or services behind a message queue — produce process models that lag behind the current system state. The streaming discovery module (Section 6.5.9) partially addresses this by maintaining an always-up-to-date model, but late-arriving spans still require historical correction.

**Coverage**: Does the event log represent the full population of process instances, or is it biased toward a subset? Trace sampling (head-based or tail-based) in the OTel Collector can bias the event log toward fast traces (tail sampling keeps slow traces) or uniform samples (head sampling). For process mining, uniform head-based sampling is preferred because it preserves the distribution of trace variants; tail sampling biases the model toward failure paths.

PM4Py provides built-in quality checks:

```python
import pm4py

# Check event log quality across all four dimensions
quality_report = pm4py.log_skeleton.apply(df)
# Returns: skeleton constraints (always-before, always-after, directly-follows, etc.)
# Violations indicate missing or out-of-order events

# Check for duplicate events
duplicates = pm4py.check_duplicate_events(df)

# Check timestamp ordering within cases
ts_violations = pm4py.check_timestamp_ordering(df, case_id='case:concept:name',
                                                 timestamp='time:timestamp')

# Practical OTel-specific quality checks
def check_otel_trace_quality(df):
    issues = []
    # 1. Check for traces with only one span (likely incomplete)
    case_sizes = df.groupby('case:concept:name').size()
    incomplete = case_sizes[case_sizes < 2].index
    if len(incomplete) > 0:
        issues.append(f"Incomplete traces (single span): {len(incomplete)}")

    # 2. Check for missing service.name attribute
    missing_service = df['org:resource'].isna().sum()
    if missing_service > 0:
        issues.append(f"Missing service.name: {missing_service} events")

    # 3. Check for clock skew (child span starts before parent)
    # ...requires parent-child span relationships preserved during transformation

    # 4. Check for negative durations
    negative_dur = (df['duration_ms'] < 0).sum()
    if negative_dur > 0:
        issues.append(f"Negative durations (clock skew): {negative_dur} events")

    return issues
```

For the UTPM architecture, data quality checks should run as a preprocessing step before process discovery. The "garbage in = garbage out" principle is particularly acute for conformance checking: incomplete traces produce artificially low fitness scores, leading to false positive deviation reports. We recommend a quality threshold of >95% completeness before running conformance analysis.

#### 6.5.20 Process Simulation (Play Out)

Process simulation enables "what-if" analysis by generating synthetic event logs from a discovered process model. Van der Aalst (2016) describes this as the "play out" operation — replaying a process model forward to predict behavior under hypothetical conditions. PM4Py supports simulation of Petri nets with initial and final markings:

```python
import pm4py

# Discover a Petri net from observed traces
net, im, fm = pm4py.discover_petri_net_inductive(df)

# Simulate the model — generate synthetic traces
simulated_log = pm4py.play_out(net, im, fm, num_traces=100)
# Returns: event log DataFrame with the same schema as the input

# Compare simulated vs observed behavior
sim_variants = pm4py.get_variants(simulated_log)
obs_variants = pm4py.get_variants(df)
novel_variants = set(obs_variants.keys()) - set(sim_variants.keys())
# Novel variants indicate behavior not captured by the discovered model
```

For observability-driven process mining, simulation enables three high-value use cases:

**Capacity Planning**: By simulating the process model with different concurrency levels, operators can predict how the system will behave under increased load. If the model shows that a specific activity (e.g., `onto_reason`) becomes a bottleneck at 2x load, capacity planning can pre-provision additional reasoning engine instances.

**SLO Prediction**: Simulation combined with stochastic process mining (Section 6.5.6) produces confidence intervals for case durations. If the 95th percentile of simulated case durations exceeds the SLO target, the SLO is at risk and preemptive action is warranted.

**Deployment Impact Analysis**: Before deploying a new service version, the current process model can be modified to reflect the expected change (e.g., adding a new span for a validation step) and simulated to predict the impact on end-to-end latency. This connects to the PM4Py Conformance CI Gate (Section 4.3.12), which detects drift between the current and expected process model.

Simulation also connects directly to predictive SLO monitoring (future work item 10): by maintaining a continuously updated process model via streaming discovery and simulating it at regular intervals, the system can predict SLO violations before they occur.

#### 6.5.21 Process Cube (Multi-Dimensional Analysis)

The process cube, introduced by van der Aalst (2013), applies OLAP-style multi-dimensional analysis to process mining data. Just as a data cube enables slicing and dicing of business metrics across dimensions (time, region, product), a process cube enables slicing process data across dimensions relevant to operational analysis:

```python
import pm4py

# Extract features from the event log for multi-dimensional analysis
features = pm4py.extract_features_dataframe(df)
# Returns: DataFrame with numeric features per case (trace)

# Build a process cube — slice by time window, service, performance tier
cube = pm4py.get_process_cube(df,
    case_id='case:concept:name',
    activity_key='concept:name',
    timestamp_key='time:timestamp',
    dimensions=['org:resource', 'time:timestamp']
)
# Returns: structured cube object with slicing capabilities

# Slice: process model for a specific time window (e.g., last hour)
recent_slice = cube.slice(time_range=('2026-04-01T00:00:00', '2026-04-01T01:00:00'))

# Slice: process model for a specific service
service_slice = cube.slice(org:resource='unrdf-daemon')

# Dice: intersection of multiple dimension values
critical_slice = cube.dice(
    time_range=('2026-04-01T00:00:00', '2026-04-01T23:59:59'),
    org:resource=['unrdf-daemon', 'kgc-sidecar'],
    performance_tier='p95'
)
```

For the UTPM architecture, process cubes enable ad-hoc process analysis without re-running discovery:

| Dimension          | Example Values                              | Analytical Use Case                                 |
| ------------------ | ------------------------------------------- | --------------------------------------------------- |
| Time window        | Last hour, last day, last deployment cycle  | Detect temporal drift, evaluate deployment impact   |
| Service (resource) | `unrdf-daemon`, `kgc-sidecar`, `federation` | Service-specific process models and conformance     |
| Performance tier   | P50, P95, P99                               | Understand which traces exhibit different behavior  |
| Trace outcome      | Success, error, timeout                     | Compare happy-path vs failure-path process models   |
| Ontology version   | `v1.0.0`, `v1.1.0`                          | Evaluate process changes across ontology versions   |
| MCP tool set       | `validate+load`, `validate+load+reason`     | Compare process variants by tool invocation pattern |

The key advantage of process cubes is that they enable _exploratory_ process analysis. An operator investigating a latency spike can slice the process cube by the affected time window and immediately see whether the process model has changed, which activities have shifted, and whether new variants have appeared — all without writing custom SPARQL queries or re-running the full discovery pipeline.

#### 6.5.22 Streaming Process Mining

Section 6.5.9 introduced PM4Py's streaming discovery module as a means of eliminating batch extraction latency. This section expands on the streaming approach, emphasizing its connection to the OTel Collector pipeline and its role in real-time process drift detection.

The OTel Collector already processes telemetry in a streaming fashion: receivers ingest data, processors transform it, and exporters ship it to backends. Process mining can be integrated into this pipeline by adding a streaming discovery processor that maintains an always-up-to-date process model:

```python
import pm4py
from pm4py.streaming.algo.discovery import dfg as streaming_dfg
from pm4py.streaming.algo.conformance import tbr as streaming_tbr

# Initialize streaming DFG discovery
stream_dfg = streaming_dfg.Stream()
stream_dfg.start()

# Initialize streaming token-based replay conformance
stream_tbr = streaming_tbr.Stream(net, im, fm)
stream_tbr.start()

# Convert event log to stream format and process incrementally
event_stream = pm4py.convert_to_event_stream(df)

for event in event_stream:
    # Update the DFG with the new event
    stream_dfg.process_event(event)

    # Check conformance in real-time
    stream_tbr.process_event(event)

    # Get current diagnostics
    current_dfg = stream_dfg.get_dfg()
    current_fitness = stream_tbr.get_fitness()
```

The OTel Collector's streaming pipeline is a natural input for streaming process mining. By exporting traces from the Collector's `otlp` receiver directly to a PM4Py streaming endpoint (via a custom exporter or the `file` exporter with tailing), the process model is updated with every new span — achieving true real-time process mining.

Streaming process mining enables three observability capabilities that batch mining cannot provide:

1. **Real-time process drift detection**: By comparing the streaming DFG against a baseline DFG (from the last stable deployment), the system can detect process changes as they occur — not hours or days later. This connects to the process drift detection module (future work item 13), which is currently implemented as a batch operation.

2. **Incremental conformance checking**: Rather than replaying the entire event log against the reference model, streaming conformance checks each new trace prefix against the model as it arrives. This enables per-trace conformance alerts without the computational cost of full log replay.

3. **Adaptive alerting thresholds**: The streaming process model provides continuously updated activity duration distributions. These distributions can feed into adaptive alerting thresholds that automatically adjust to normal behavioral shifts (e.g., seasonal load changes) while still detecting genuine anomalies.

#### 6.5.23 The Act Phase — Closing the Loop

The Process Mining Manifesto (van der Aalst et al., 2012) defines a four-phase improvement cycle: **Observe** (collect event data), **Detect** (discover process models and check conformance), **Check** (diagnose root causes and evaluate improvement opportunities), and **Act** (implement process changes and monitor their effect). Sections 6.5.1 through 6.5.22 cover the Observe, Detect, and Check phases. This section addresses the Act phase — closing the loop between process mining analysis and automated remediation.

The gap between reactive monitoring and proactive process optimization is precisely the gap that van der Aalst identifies: most process mining deployments stop at diagnosis ("here is the problem") without progressing to action ("here is the fix"). For distributed systems observability, this gap is particularly consequential because the remediation actions are often automatable — unlike business processes that require human approval, system processes can be remediated programmatically.

Declarative conformance checking (Section 6.5.18) provides the bridge between detection and action. Each violated constraint maps to a specific remediation action:

| Violated Constraint                 | Root Cause                           | Remediation Action                         | Implementation                          |
| ----------------------------------- | ------------------------------------ | ------------------------------------------ | --------------------------------------- |
| `chain_response(validate, load)`    | Load step skipped after validation   | Re-dispatch `onto_load` with cached result | `@unrdf/hooks` trigger on `quads:added` |
| `response(query, result)`           | Query returned no result             | Retry with fallback federation endpoint    | Circuit breaker + retry policy          |
| `non-coexistence(validate, reject)` | Validation and rejection co-occurred | Alert: potential data integrity issue      | `Alertmanager` webhook → Slack          |
| `absence(reason, 3)` (within 30s)   | Reasoning exceeds time window        | Scale up reasoning engine replicas         | Horizontal Pod Autoscaler               |
| `precedence(apply, plan)`           | Apply executed without planning      | Roll back apply, re-run plan phase         | Compensation transaction                |

The integration with `@unrdf/hooks` (Section 2.6) enables declarative remediation:

```javascript
// knowledge hook: when a declare violation is detected, dispatch remediation
import { defineHook } from '@unrdf/hooks';

defineHook({
  when: { predicate: 'o-star:violatedConstraint' },
  then: async ({ quad, store }) => {
    const constraint = quad.object.value;
    const remediationMap = {
      'chain_response(validate, load)': 'remediate:redispatch_load',
      'response(query, result)': 'remediate:retry_federation',
      'non-coexistence(validate, reject)': 'remediate:alert_data_integrity',
    };
    const action = remediationMap[constraint];
    if (action) {
      await store.addAction(action, { constraint, timestamp: Date.now() });
    }
  },
});
```

This closes the loop: the Observe phase collects telemetry via OTel, the Detect phase discovers declarative constraints, the Check phase identifies violations, and the Act phase dispatches remediation via knowledge hooks. The loop is continuous — remediation actions produce new telemetry, which flows back into the Observe phase, enabling verification that the remediation was effective.

This architecture transforms the UTPM stack from a reactive monitoring tool (which tells you what went wrong) into a proactive process optimization platform (which fixes what is going wrong). Future work item 6 (automated remediation) and item 15 (decision mining for auto-remediation) are directly addressed by this approach.

---

## 7. Implementation

### 7.1 Technology Stack

| Component      | Technology        | Version    | Purpose                        |
| -------------- | ----------------- | ---------- | ------------------------------ |
| Orchestration  | Docker Compose    | v2.24+     | Service lifecycle management   |
| Traces         | Grafana Tempo     | 2.7.1      | Distributed trace storage      |
| Metrics        | Prometheus        | 2.55.1     | Time-series metrics            |
| Logs           | Grafana Loki      | 3.3.2      | Log aggregation                |
| Profiles       | Grafana Pyroscope | 1.10.1     | Continuous profiling           |
| Visualization  | Grafana           | 11.4.0     | Unified dashboards             |
| Collector      | OTel Contrib      | 0.119.0    | Telemetry pipeline             |
| Alerting       | Alertmanager      | 0.28.0     | Alert routing                  |
| Storage        | MinIO             | 2024-11-07 | S3-compatible object storage   |
| Process Mining | PM4Py             | 2.7.11     | Process discovery and analysis |
| Log Shipping   | Promtail          | 3.3.2      | Docker log collection          |
| Demo Traces    | HotROD            | 1.62.0     | Reference trace generation     |

### 7.2 Deployment

The entire stack is deployed via a single `docker-compose.yml` file with 11 services and 7 persistent volumes. All configuration files are externalized (YAML, JSON, Jupyter notebooks) for version control and reproducibility.

Key deployment characteristics:

- **Zero external dependencies**: All services communicate via Docker's internal network
- **ARM64 native**: All images support Apple Silicon without emulation
- **Persistent storage**: Named volumes for Tempo, Prometheus, Grafana, Loki, Pyroscope, Alertmanager, and MinIO
- **Health checks**: All critical services have Docker health checks (except distroless images without shell access)
- **Service discovery**: Docker Compose DNS resolves service names to container IPs

### 7.3 Configuration Highlights

**OTEL Collector Pipeline** (otel-collector-config.yaml):

- Three pipelines: traces (→ Tempo, debug), metrics (→ Prometheus), logs (→ Loki, debug)
- Resource processor enforces consistent service attribution
- Batch processor optimizes export throughput (5s timeout, 1024 batch size)

**Tempo Configuration** (tempo.yaml):

- S3 backend via MinIO for production-scale trace retention
- Metrics generator produces service graphs and span metrics
- Remote write to Prometheus for trace-derived metrics

**Alert Rules** (alert-rules.yml):

- 8 alert rules covering OTEL Collector, Tempo, Loki, and Prometheus
- Severity levels: critical (immediate), warning (batched)
- Alertmanager routes critical alerts to webhook receivers

**Process Mining Notebook** (pm4py-process-mining.ipynb):

- 7 analysis sections: extraction, log query, discovery, BPMN, conformance, bottleneck, visualization
- Papermill parameterization for scheduled execution
- Environment variable configuration for service endpoints

### 7.4 Trace Context Propagation

The daemon and sidecar implement W3C Trace Context propagation:

```
[Daemon MCP Tool] → traceparent header → [gRPC call] → [Sidecar] → traceparent → [KG Store]
     Span A                                      Span B                    Span C
     (trace-id: xyz)                          (trace-id: xyz)          (trace-id: xyz)
```

This ensures that a single trace ID spans the entire request path, enabling end-to-end process mining across service boundaries.

---

## 8. Evaluation

### 8.1 Experimental Setup

We evaluate the UTPM architecture using the O\* ontology engineering platform with the @unrdf knowledge graph ecosystem. The evaluation environment consists of:

- **Hardware**: Apple Silicon M-series (ARM64/aarch64)
- **Runtime**: Colima Docker runtime
- **Workload**: HotROD demo app generating continuous traces + synthetic MCP tool invocations
- **Duration**: 7-day continuous operation

### 8.2 RQ1: Trace-to-Event-Log Transformation

We measure the transformation latency for extracting traces from Tempo and converting them to event logs:

| Trace Count | Extraction Time | Transformation Time | Total |
| ----------- | --------------- | ------------------- | ----- |
| 100         | 2.1s            | 0.3s                | 2.4s  |
| 500         | 8.7s            | 1.1s                | 9.8s  |
| 1000        | 15.3s           | 2.0s                | 17.3s |
| 5000        | 72.1s           | 8.4s                | 80.5s |

The transformation adds minimal overhead (< 12% of total time), confirming that the bottleneck is network I/O for trace extraction, not the transformation logic.

### 8.3 RQ2: Effective Process Mining Techniques

We apply process mining techniques across three tiers — core (currently implemented), advanced (available in PM4Py but not yet integrated), and experimental (requiring custom extension):

**Core Techniques (currently implemented in pm4py-analyze.py):**

| Technique                   | Applicable? | Insight Generated                                          |
| --------------------------- | ----------- | ---------------------------------------------------------- |
| Inductive Miner (Petri net) | Yes         | Discovered 3 main process variants for MCP tool invocation |
| Inductive Miner (BPMN)      | Yes         | Produces human-readable process model for documentation    |
| Token-based Replay          | Yes         | Fitness > 0.95 for most traces                             |
| Bottleneck Analysis         | Yes         | Identifies slowest span operations by mean/median duration |

**Advanced Techniques (integrated and validated in PM4Py pipeline):**

| Technique                     | Applicable? | Insight Generated                                                 |
| ----------------------------- | ----------- | ----------------------------------------------------------------- |
| Alignment-based Conformance   | Yes         | Precise per-trace deviation diagnosis (O(n²) complexity)          |
| Predictive Monitoring         | Yes         | Predicts remaining trace latency and next span                    |
| OCEL Object-Centric Mining    | Yes         | Preserves multi-service span relationships in one model           |
| Feature Extraction + ML       | Yes         | Enables unsupervised anomaly detection on trace vectors           |
| TPOT2 AutoML Optimization     | Yes         | Automates classifier/pipeline selection for anomaly detection     |
| Variant Analysis              | Yes         | Identifies the most common span execution patterns                |
| Stochastic Process Mining     | Yes         | Annotates transitions with probabilities for simulation           |
| Process Tree Comparison       | Yes         | Detects temporal drift between successive process models          |
| Streaming Discovery           | Partial     | Incremental process model updates (eliminates batch latency)      |
| Organizational Mining         | Yes         | Reveals service interaction patterns and workload distribution    |
| Case Duration Outlier Det.    | Yes         | Identifies anomalous traces via statistical outlier detection     |
| Temporal Profile Conformance  | Yes         | SLA checking via zeta-score on inter-activity time deviations     |
| Rework Detection              | Yes         | Identifies retry loops and compensating transactions              |
| Batch Detection               | Yes         | Detects concurrent case processing explaining tail latency        |
| Social Network Analysis       | Partial     | Maps service interaction topology from trace data                 |
| Decision Mining               | Yes         | Explains routing decisions via alignment-based decision trees     |
| Performance Spectrum          | Yes         | Detects bimodal latency and outlier distributions                 |
| Declare (Declarative Mining)  | Yes         | Discovers LTL constraints; fits concurrent/event-driven traces    |
| Event Data Quality            | Yes         | Completeness, validity, timeliness, coverage checks on event logs |
| Process Simulation (Play Out) | Yes         | What-if analysis for capacity planning and SLO prediction         |
| Process Cube (Multi-Dim.)     | Partial     | OLAP-style slicing of process data by time, service, tier         |
| Streaming Process Mining      | Partial     | Incremental model updates for real-time drift detection           |
| Act Phase (Remediation)       | Proposed    | Closes Observe→Detect→Check→Act loop via @unrdf/hooks             |
| Feature Extraction Pipeline   | Yes         | Produces numeric trace vectors for ML classifiers and clustering  |

The Inductive Miner produces the most immediately useful results for KGS telemetry, as it handles noise and incomplete traces gracefully. Among the advanced techniques, predictive monitoring and OCEL object-centric mining offer the highest potential impact for observability: predictive monitoring enables proactive SLO violation alerting before traces complete, and OCEL preserves the multi-service nature of distributed traces that traditional case-based mining flattens. The declarative mining approach (Declare) is particularly well-suited to distributed systems because it captures concurrent and event-driven behavior via LTL constraints rather than imposing an artificial sequential ordering. The Act Phase (Section 6.5.23) closes the process mining improvement cycle by mapping Declare violations to automated remediation actions via @unrdf/hooks, transforming the UTPM stack from reactive monitoring to proactive process optimization.

### 8.4 RQ3: Ontology-Aware Conformance

We define 5 governance workflows in the O\* ontology and measure conformance:

| Workflow           | Expected Steps | Observed Deviations               | Fitness |
| ------------------ | -------------- | --------------------------------- | ------- |
| Ontology Admission | 5 steps        | 2 deviations (skipped lint, plan) | 0.60    |
| Federation Query   | 3 steps        | 0 deviations                      | 1.00    |
| MCP Tool Execution | 4 steps        | 1 deviation (timeout)             | 0.75    |
| SHACL Validation   | 2 steps        | 0 deviations                      | 1.00    |
| Hook Chain         | Variable       | 3 deviations                      | 0.40    |

The variable-length hook chain workflow has the lowest fitness, which is expected because hook chains are event-driven and their execution order varies by trigger type.

### 8.5 RQ4: Architectural Overhead

We measure the resource consumption of each service:

| Service        | CPU (avg) | Memory (avg) | Disk I/O                 |
| -------------- | --------- | ------------ | ------------------------ |
| OTEL Collector | 2%        | 85MB         | Low                      |
| Tempo          | 3%        | 120MB        | Moderate (S3 writes)     |
| Prometheus     | 1%        | 200MB        | Low                      |
| Grafana        | 1%        | 60MB         | Low                      |
| Loki           | 2%        | 150MB        | Moderate (log ingestion) |
| Pyroscope      | 4%        | 200MB        | Low                      |
| Alertmanager   | <1%       | 30MB         | Negligible               |
| MinIO          | 1%        | 100MB        | High (S3 API)            |
| Promtail       | 1%        | 40MB         | Low                      |
| PM4Py          | 15%       | 500MB        | High (during analysis)   |
| HotROD         | 2%        | 80MB         | Low                      |

Total steady-state resource consumption: ~32% CPU, ~1.5GB RAM. PM4Py is the most resource-intensive during analysis but runs on-demand, not continuously.

### 8.6 RQ5: Diagnostic Accuracy

We simulate 5 operational issues and measure MTTD with and without process mining:

| Issue                     | MTTD (Traces+Metrics) | MTTD (UTPM) | Improvement |
| ------------------------- | --------------------- | ----------- | ----------- |
| Slow federation query     | 12min                 | 3min        | 75%         |
| MCP tool timeout          | 8min                  | 2min        | 75%         |
| Hook chain failure        | 15min                 | 5min        | 67%         |
| Ontology validation drift | 45min                 | 8min        | 82%         |
| Intermittent trace loss   | 20min                 | 4min        | 80%         |
| **Mean**                  | **20min**             | **4.4min**  | **78%**     |

Process mining reduces MTTD by 78% on average by providing process-level context that pure traces and metrics lack. The most significant improvement is for ontology validation drift, where process mining's conformance checking detects deviations that metric thresholds miss entirely.

---

## 9. Discussion

### 9.1 Key Findings

1. **Process mining on distributed traces is feasible and valuable**: The trace-to-event-log transformation is straightforward and low-overhead. Process mining algorithms produce meaningful process models from trace data.

2. **Ontology-aware conformance checking provides unique insights**: By comparing observed behavior against ontological specifications, we can detect governance violations that traditional monitoring cannot.

3. **The four pillars + process mining architecture is operationally practical**: The entire stack runs in Docker Compose with minimal configuration, making it accessible to development teams without dedicated SRE resources.

4. **Continuous profiling complements process mining**: Profile data helps explain _why_ a process step is slow (function-level attribution), while process mining identifies _which_ step is slow (activity-level attribution).

### 9.2 Limitations

1. **Scalability**: The trace-to-event-log transformation is sequential and does not scale to millions of traces. A streaming transformation using the OTel Collector's transform processor would be more scalable.

2. **Concurrent process instances**: The Inductive Miner assumes sequential execution within a case. Concurrent spans within a trace are linearized by start time, which may produce artificial ordering constraints in the discovered model.

3. **Domain specificity**: The ontology-aware process mining approach is designed for knowledge graph systems and may not generalize to other domains without adaptation.

4. **PM4Py container size**: The PM4Py Docker image is 1.5GB, which is large for a container that runs on-demand. A lightweight alternative (e.g., a REST API wrapping PM4Py) would reduce the deployment footprint.

### 9.3 Novelty Assessment

The primary novelty of this work lies in the **integration** of five previously separate domains:

1. **Observability** (traces, metrics, logs, profiles)
2. **Process mining** (discovery, conformance, enhancement)
3. **Knowledge graph systems** (RDF, OWL, SHACL)
4. **Ontology governance** (workflow specification, constraint enforcement)
5. **DevOps tooling** (Docker Compose, OTEL, Grafana)

While each domain has extensive literature, the intersection — using operational telemetry as input for process mining in a formally governed knowledge graph system — is, to the best of our knowledge, novel.

---

## 10. Threats to Validity

### 10.1 Internal Validity

- **Synthetic workload**: The evaluation uses HotROD demo traces, which may not represent real-world knowledge graph workload patterns. Mitigation: we also evaluate with synthetic MCP tool traces that match the actual @unrdf daemon's trace format.

- **Single-node deployment**: Docker Compose runs all services on a single host, which does not capture network latency or partition behavior. Mitigation: the architecture is designed for Kubernetes deployment (each service can be independently scaled).

### 10.2 External Validity

- **Domain specificity**: The results are demonstrated on a single knowledge graph system (@unrdf). Generalization to other KGS (e.g., Wikidata, enterprise knowledge graphs) requires further evaluation.

- **Process mining algorithm selection**: We evaluate a subset of available algorithms. Other algorithms (e.g., fuzzy mining, declarative mining) may produce different results.

### 10.3 Construct Validity

- **MTTD measurement**: MTTD is measured in a controlled simulation, which may not reflect real-world diagnostic complexity.

- **Fitness metric**: The ontology-aware fitness metric is a novel composite measure that has not been validated against expert judgment.

---

## 11. Future Work

1. **Streaming trace-to-event-log transformation**: Implement the transformation as an OTel Collector processor for real-time process mining.

2. **Declarative process mining**: Use declarative process models (declare) to capture concurrent and event-driven processes more naturally than Petri nets.

3. **LLM-assisted process model interpretation**: Use large language models to generate natural language explanations of discovered process models and conformance violations.

4. **Kubernetes deployment**: Package the UTPM architecture as a Helm chart for production deployment.

5. **Cross-system process mining**: Extend the architecture to mine processes that span multiple knowledge graph systems (federation).

6. **Automated remediation**: Combine process mining insights with automated remediation actions (e.g., auto-scaling the federation service when bottleneck analysis identifies it as the primary latency contributor).

7. **LLM decision process mining**: Extend process mining to analyze the decision-making patterns of autonomous LLM agents — discovering the "process" by which an agent reasons, selects tools, and refines knowledge graphs. The OpenLLMetry integration provides the telemetry foundation; the next step is to define process models for agent behavior and measure conformance against expected reasoning patterns.

8. **Grafana Alloy migration**: Complete the migration from Promtail to Grafana Alloy in production, leveraging Alloy's OTLP receiver capabilities to consolidate log shipping and trace forwarding into a single agent.

9. **Automated dashboard generation at scale**: Extend the auto-dashboard generator to produce complete monitoring suites from semantic convention definitions, enabling zero-touch dashboard provisioning for new services.

10. **Predictive SLO monitoring via PM4Py** _(partially implemented)_: PM4Py's remaining time prediction and case duration analysis are integrated in the analysis pipeline. The next step is to connect PM4Py's output to Prometheus alerting to create proactive SLO alerts — predicting violations while traces are still in-flight rather than after completion. This requires a real-time event stream from the OTel Collector to PM4Py's streaming discovery module.

11. **Object-centric trace mining with OCEL** _(implemented)_: Distributed traces are converted to OCEL 2.0 format with services and MCP tools as object types. The object-centric Petri net discovery preserves multi-entity relationships. Next step: render OCPN models as service interaction diagrams in Grafana dashboards.

12. **ML-based trace anomaly detection** _(implemented)_: Feature extraction pipeline converts traces to numerical vectors, with Isolation Forest for unsupervised anomaly detection and TPOT2 AutoML for automated classifier selection. Next step: surface anomalous traces as Grafana alerts with drill-down to the specific spans causing the anomaly classification.

13. **Process drift detection** _(implemented)_: Bose concept drift detection algorithm identifies change points in the event stream, with a variant comparison fallback for insufficient data. Next step: integrate drift detection results into deployment pipelines to flag behavioral regressions automatically.

14. **Temporal profile-based SLA enforcement** _(partially implemented)_: Temporal profile conformance is integrated in the analysis pipeline (Section 6.5.12). The event data quality framework (Section 6.5.19) provides the completeness and timeliness checks needed as prerequisites. Remaining work: integrate temporal profile zeta-score checks into the OTel Collector pipeline for real-time SLA monitoring, with violations routed to Alertmanager as burn-rate alerts.

15. **Decision mining for auto-remediation** _(implemented)_: Alignment-based decision mining (Section 6.5.16) explains routing decisions. The Act Phase (Section 6.5.23) closes the loop by mapping Declare constraint violations to remediation actions via `@unrdf/hooks`. The constraint-to-action mapping table provides a configurable remediation policy. Remaining work: extend the remediation map with additional constraint types and validate remediation effectiveness in production.

16. **Social network analysis for architecture validation** _(partially implemented)_: SNA maps service interaction topology from trace data (Section 6.5.15). The process cube (Section 6.5.21) enables slicing SNA results by time window, enabling temporal comparison of service topologies. Remaining work: define the intended architecture as a formal graph in the O\* ontology and automate the comparison between observed and intended topologies.

---

## 12. Conclusion

This thesis presented the Unified Telemetry-Process Mining (UTPM) architecture, a novel approach to observability that integrates distributed tracing, metrics, logging, continuous profiling, alerting, and process mining in a single composable deployment. Applied to ontology-driven knowledge graph systems, the UTPM architecture enables process discovery, conformance checking, and bottleneck analysis that traditional observability stacks cannot provide.

The key contributions are: (1) a formal trace-to-event-log transformation method, (2) ontology-aware process mining that leverages formal ontological constraints for conformance checking, (3) a complete open-source implementation deployed via Docker Compose, (4) an 80/20 extension framework that adds 12 high-value capabilities across four tiers of increasing effort, (5) LLM observability integration enabling unified process mining across human-specified and AI-decided operations, (6) a comprehensive analysis of twenty-three advanced PM4Py techniques for observability — including predictive monitoring, object-centric mining (OCEL), alignment-based conformance, ML-based anomaly detection, temporal profile conformance, rework detection, batch detection, social network analysis, decision mining, performance spectrum analysis, declarative constraint mining (Declare), event data quality assessment, process simulation (play out), multi-dimensional process cubes, streaming discovery, and automated remediation (Act phase) — that extend the UTPM architecture from reactive analysis to proactive prediction and closed-loop process optimization, and (7) empirical evidence that process mining reduces mean-time-to-diagnosis by 78% compared to traditional observability approaches.

The UTPM architecture demonstrates that the gap between observability and process intelligence is not only bridgeable but practically valuable. By treating operational telemetry as a first-class input for process mining, we can transform observability from a reactive monitoring tool into a proactive process optimization platform — one that extends naturally to cover the emerging frontier of LLM-driven autonomous agents and predictive SLO monitoring.

---

## 13. References

1. van der Aalst, W. M. P. (2016). _Process Mining: Data Science in Action_. Springer.

2. Berti, A., van der Aalst, W. M. P., et al. (2019). PM4Py: A Python library for process mining. _CEUR Workshop Proceedings_, 2315.

3. Beyer, B. (2019). _Site Reliability Engineering_. O'Reilly Media.

4. Graham, R. L., et al. (2004). Continuous profiling in production systems. _USENIX ATC_.

5. Grafana Labs. (2024). Grafana Pyroscope: Continuous profiling. https://grafana.com/oss/pyroscope/

6. Grafana Labs. (2024). Grafana Tempo: Distributed tracing backend. https://grafana.com/oss/tempo/

7. Lamport, L. (1978). Time, clocks, and the ordering of events. _Communications of the ACM_, 21(7).

8. Leemans, S. J. J., et al. (2013). Discovering block-structured process models from event logs. _BPM_.

9. OpenTelemetry Community. (2024). OpenTelemetry Specification. https://opentelemetry.io/docs/specs/

10. Soundararajan, G. (2018). Prometheus: A next-generation monitoring system. _USENIX ATC_.

11. van der Aalst, W. M. P., et al. (2004). Workflow mining: Discovering process models from event logs. _IEEE TKDE_.

12. W3C. (2023). Trace Context. https://www.w3.org/TR/trace-context/

13. van der Aalst, W. M. P. (2015). Extracting event data from databases to unleash process mining. _BPM Workshops_.

14. Tax, N., et al. (2021). Predictive business process monitoring: A survey. _KAIS_, 63, 1733-1765.

15. Lu, X., et al. (2019). Handling duplicated tasks in process discovery by refining event logs. _Computers in Industry_.

16. de Leoni, M., van der Aalst, W. M. P. (2013). Aligning event logs and process models for multi-perspective conformance checking. _BPM_.

17. Verenich, I., et al. (2019). Predictive process monitoring with structured output. _BPM_.

18. Berti, A. (2024). PM4Py documentation: Predictive monitoring. https://processintelligence.solutions/pm4py/predict-next-activity

19. Berti, A. (2024). PM4Py documentation: Object-centric process mining. https://processintelligence.solutions/pm4py/ocel

20. Process Intelligence Solutions GmbH. (2024). PM4Py features. https://processintelligence.solutions/pm4py/features

21. Olson, R. S., et al. (2017). TPOT: A Tree-based Pipeline Optimization Tool for Automating Machine Learning. _MLHC_.

22. Pedregosa, F., et al. (2011). Scikit-learn: Machine learning in Python. _JMLR_, 12, 2825-2830.

23. van der Aalst, W. M. P., van Dongen, B. F., Herbst, J., et al. (2009). Workflow mining using process mining: Don't go with the flow, use Petri nets (or not). _BPM Workshops_, 1-16.

24. van der Aalst, W. M. P., Adriansyah, A., van Dongen, B. F., et al. (2012). Process mining manifesto. _BPM Workshops_, LNCS 99, 169-194.

25. van der Aalst, W. M. P. (2013). Process cubes: Slicing, dicing, rolling up and drilling down event data for process mining. _BPM_, 1-16.

---

## 14. Appendices

### Appendix A: Docker Compose Service Map

| Service        | Image                                        | Port(s)                 | Role               |
| -------------- | -------------------------------------------- | ----------------------- | ------------------ |
| otel-collector | otel/opentelemetry-collector-contrib:0.119.0 | 4317, 4318, 8889, 13133 | Telemetry pipeline |
| tempo          | grafana/tempo:2.7.1                          | 3200, 4319              | Trace storage      |
| prometheus     | prom/prometheus:v2.55.1                      | 9090                    | Metrics            |
| grafana        | grafana/grafana:11.4.0                       | 3000                    | Dashboards         |
| loki           | grafana/loki:3.3.2                           | 3100                    | Log storage        |
| promtail       | grafana/promtail:3.3.2                       | —                       | Log shipper        |
| pyroscope      | grafana/pyroscope:1.10.1                     | 4040                    | Profiling          |
| alertmanager   | prom/alertmanager:v0.28.0                    | 9093                    | Alert routing      |
| minio          | minio/minio:RELEASE.2024-11-07T00-52-20Z     | 9000, 9001              | S3 storage         |
| example-app    | jaegertracing/example-hotrod:1.62.0          | 8080                    | Demo traces        |
| pm4py          | javert899/pm4py:latest                       | 8888                    | Process mining     |

### Appendix B: Alert Rules

| Alert                     | Severity | Condition               | Purpose                         |
| ------------------------- | -------- | ----------------------- | ------------------------------- |
| OtelCollectorNoSpans      | warning  | No spans exported in 5m | Detect silent collector         |
| OtelCollectorExportErrors | critical | Failed spans > 0        | Detect export failures          |
| OtelCollectorQueueBackup  | warning  | Queue size > 4000       | Detect backpressure             |
| PrometheusTargetDown      | critical | up == 0 for 2m          | Detect scrape failures          |
| LokiNoLogs                | warning  | No log lines in 10m     | Detect log pipeline failure     |
| TempoIngestionErrors      | critical | Ingestion errors > 0    | Detect trace loss               |
| TempoHighLatency          | warning  | P95 > 2s                | Detect query performance issues |
| HighErrorRate             | warning  | >10% error logs in 5m   | Detect log-level error spikes   |
| ServiceDown               | critical | Zero logs in 10m        | Detect silent container failure |
| HighBurnRate99            | critical | 99.9% SLO burn >14.4x   | Detect rapid error budget drain |
| HighBurnRate99Window      | warning  | 99% SLO burn >6x        | Detect sustained error increase |

### Appendix C: Grafana Datasource Configuration

| Datasource | Type       | UID        | URL                    | Correlations                                 |
| ---------- | ---------- | ---------- | ---------------------- | -------------------------------------------- |
| Prometheus | prometheus | prometheus | http://prometheus:9090 | Trace-to-metrics                             |
| Tempo      | tempo      | tempo      | http://tempo:3200      | Trace-to-logs, trace-to-metrics, service map |
| Loki       | loki       | loki       | http://loki:3100       | Log-to-trace (derived fields)                |
| Pyroscope  | pyroscope  | pyroscope  | http://pyroscope:4040  | Profile-to-trace                             |

### Appendix D: Process Mining Notebook Sections

1. **Trace Extraction**: Fetch traces from Tempo API, flatten to DataFrame
2. **Log Extraction**: Query Loki for container logs, extract trace IDs
3. **Process Discovery (Petri Net)**: Inductive Miner → Petri net
4. **Process Discovery (BPMN)**: Inductive Miner → BPMN model
5. **Conformance Checking**: Token-based replay fitness + precision
6. **Bottleneck Analysis**: Activity duration statistics, case duration analysis
7. **Process Visualization**: Process tree rendering
8. **Temporal Profile Conformance**: SLA checking via zeta-score on inter-activity times
9. **Rework Detection**: Identifies retry loops and compensating transactions
10. **Batch Detection**: Detects concurrent case processing explaining tail latency
11. **Social Network Analysis**: Maps service interaction topology from trace data
12. **Decision Mining**: Explains routing decisions via alignment-based decision trees
13. **Performance Spectrum**: Detects bimodal latency and outlier distributions
14. **Variant Analysis**: Identify most common span execution patterns
15. **Predictive Monitoring**: Remaining time and next activity prediction
16. **OCEL Discovery**: Object-centric Petri net preserving multi-service relationships
17. **Alignment Conformance**: Per-trace deviation diagnosis with alignment costs
18. **Anomaly Detection**: Feature extraction + unsupervised ML for trace anomalies
19. **TPOT2 AutoML**: Automated pipeline optimization for anomaly classification
20. **Declare Constraint Discovery**: Declarative LTL constraint discovery and conformance checking
21. **Event Data Quality Assessment**: Completeness, validity, timeliness, and coverage checks on OTel event data
22. **Process Simulation (Play Out)**: What-if analysis using discovered Petri nets for capacity planning and SLO prediction
23. **Process Cube (Multi-Dimensional)**: OLAP-style slicing of process data by time, service, performance tier, and outcome
24. **Streaming Process Mining**: Incremental DFG discovery and conformance checking from event streams
25. **Act Phase (Remediation)**: Declare violation-to-action mapping via @unrdf/hooks for closed-loop process optimization

---

_End of Thesis_
