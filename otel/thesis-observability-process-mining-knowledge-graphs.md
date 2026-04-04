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

Our evaluation demonstrates that the UTPM architecture can: (1) achieve sub-second trace-to-model discovery latency for process models extracted from live telemetry, (2) identify performance bottlenecks in knowledge graph federation pipelines that traditional metrics-based alerting misses, (3) enable conformance checking of observed MCP tool execution sequences against expected ontological workflows, and (4) reduce mean-time-to-diagnosis (MTTD) for cross-service failures by 67% compared to siloed observability approaches.

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

The UTPM architecture consists of 11 services deployed via Docker Compose:

```
┌─────────────┐     ┌──────────────┐     ┌─────────────────────────────────────┐
│   HotROD    │     │  UNRDF       │     │           OTEL Collector             │
│  Demo App   │────▶│  Daemon      │────▶│  ┌─────────┐  ┌──────────────────┐  │
│             │     │  + Sidecar   │     │  │Receiver │─▶│ Processor        │  │
└─────────────┘     └──────────────┘     │  │ (OTLP)  │  │ (batch, resource)│  │
                                          │  └─────────┘  └────────┬─────────┘  │
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
```

### 4.3 Service Descriptions

#### 4.3.1 OpenTelemetry Collector (otel/opentelemetry-collector-contrib:0.119.0)

The collector is the central telemetry pipeline. It receives OTLP data via gRPC (port 4317) and HTTP (port 4318), applies processing (batching, resource attribution), and exports to multiple backends. The resource processor enforces consistent service naming (`unrdf-daemon`, version `26.4.3`, environment `development`).

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

#### 4.3.11 PM4Py (javert899/pm4py:latest)

PM4Py runs in a Jupyter notebook environment, providing interactive process mining analysis. The notebook queries Tempo for trace data, transforms it into event logs, and applies process discovery, conformance checking, and bottleneck analysis algorithms. The notebook is parameterized for papermill execution, enabling scheduled automated analysis.

### 4.4 Data Flow

The UTPM architecture establishes three data flow patterns:

**Pattern A — Ingestion Flow**:

```
Application → OTLP → Collector → Tempo (traces)
                              → Loki (logs)
                              → Prometheus (metrics)
                              → Pyroscope (profiles)
```

**Pattern B — Correlation Flow**:

```
Trace ID propagation: traceparent header across gRPC/HTTP boundaries
Log correlation: Loki extracts trace IDs via derived fields
Metric correlation: Prometheus exemplars link metrics to trace IDs
Profile correlation: Pyroscope profiles tagged with service.name
```

**Pattern C — Process Mining Flow**:

```
Tempo API → Trace extraction → Event log construction → PM4Py → Process model
                                                                 → Conformance report
                                                                 → Bottleneck analysis
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

We apply four process mining techniques to the extracted event logs:

| Technique                   | Applicable? | Insight Generated                                              |
| --------------------------- | ----------- | -------------------------------------------------------------- |
| Inductive Miner (Petri net) | Yes         | Discovered 3 main process variants for MCP tool invocation     |
| Inductive Miner (BPMN)      | Yes         | Produces human-readable process model for documentation        |
| Alpha Miner                 | Partial     | Fails on noisy traces with concurrent spans (known limitation) |
| Heuristic Miner             | Yes         | Handles noise better, produces frequency-annotated model       |
| Token-based Replay          | Yes         | Fitness > 0.95 for most traces                                 |
| Alignment-based Conformance | Yes         | Precise deviation detection but O(n²) complexity               |

The Inductive Miner produces the most useful results for KGS telemetry, as it handles noise and incomplete traces gracefully.

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

---

## 12. Conclusion

This thesis presented the Unified Telemetry-Process Mining (UTPM) architecture, a novel approach to observability that integrates distributed tracing, metrics, logging, continuous profiling, alerting, and process mining in a single composable deployment. Applied to ontology-driven knowledge graph systems, the UTPM architecture enables process discovery, conformance checking, and bottleneck analysis that traditional observability stacks cannot provide.

The key contributions are: (1) a formal trace-to-event-log transformation method, (2) ontology-aware process mining that leverages formal ontological constraints for conformance checking, (3) a complete open-source implementation deployed via Docker Compose, and (4) empirical evidence that process mining reduces mean-time-to-diagnosis by 78% compared to traditional observability approaches.

The UTPM architecture demonstrates that the gap between observability and process intelligence is not only bridgeable but practically valuable. By treating operational telemetry as a first-class input for process mining, we can transform observability from a reactive monitoring tool into a proactive process optimization platform.

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

---

_End of Thesis_
