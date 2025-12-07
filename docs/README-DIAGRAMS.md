# AtomVM System Architecture Diagrams

This directory contains PlantUML diagrams documenting the production-ready AtomVM system architecture.

## Diagrams

### 1. `atomvm-system-architecture.puml`
**System Architecture Overview**
- Shows the complete system layers: Validation, Execution, Runtime, Bridge, and OTEL
- Illustrates how components interact
- Highlights production-ready aspects (real OTEL spans, no simulation)

### 2. `otel-span-hierarchy.puml`
**OTEL Span Hierarchy**
- Complete span tree from validation suite to individual operations
- Shows parent-child relationships
- Documents all span types created by the system

### 3. `bridge-interceptor-flow.puml`
**Bridge Interceptor Sequence Diagram**
- Shows how Erlang processes communicate with JavaScript via bridge interceptor
- Illustrates the flow from `io:format` to OTEL spans
- Documents real KGC-4D and hooks integration

### 4. `validation-pipeline.puml`
**Validation Pipeline Sequence Diagram**
- Shows the complete validation flow from runner to span validation
- Documents how spans are collected and validated
- Highlights production-ready aspects (no simulation)

### 5. `atomvm-runtime-states.puml`
**Runtime State Machine**
- Documents the poka-yoke state machine design
- Shows valid state transitions
- Highlights fail-fast error handling

### 6. `complete-system-overview.puml`
**Complete System Overview**
- High-level view of all system components
- Shows data flow and integration points
- Documents production-ready architecture

## Viewing the Diagrams

### Using PlantUML

1. **Online**: Use [PlantUML Online Server](http://www.plantuml.com/plantuml/uml/)
   - Copy the `.puml` file contents
   - Paste into the online editor
   - View or export as PNG/SVG

2. **VS Code**: Install the "PlantUML" extension
   - Open a `.puml` file
   - Press `Alt+D` to preview

3. **Command Line**: Install PlantUML
   ```bash
   # macOS
   brew install plantuml
   
   # Generate PNG
   plantuml docs/*.puml
   ```

### Using Mermaid (Alternative)

If you prefer Mermaid diagrams, these can be converted. The PlantUML syntax is more expressive for complex architectures.

## Key Production-Ready Features Documented

1. **Real OTEL Instrumentation**: All spans come from actual OTEL tracers, not simulation
2. **End-to-End Tracing**: Complete span hierarchy from validation to individual operations
3. **Bridge Integration**: Real KGC-4D and hooks integration via bridge interceptor
4. **State Machine Design**: Poka-yoke design prevents invalid operations
5. **Fail-Fast Error Handling**: Errors propagate immediately, no graceful degradation
6. **Span Collection**: All spans collected automatically by OTEL span processor

## Architecture Principles

- **No Simulation**: All operations use real implementations
- **OTEL First**: All observability via OpenTelemetry
- **Poka-Yoke**: Invalid states are unrepresentable
- **Fail Fast**: Errors propagate immediately
- **End-to-End Tracing**: Complete visibility into system behavior

