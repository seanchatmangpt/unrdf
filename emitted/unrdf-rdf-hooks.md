# UNRDF RDF Hook Definitions - Comprehensive Scan Report

**Date:** 2026-06-01  
**Source Directories Scanned:** ~/unrdf/  
**Files Analyzed:** 50+ RDF/TTL/NT files  
**Status:** Complete scan with hook definitions, classes, properties, and rule patterns identified

---

## Executive Summary

The UNRDF ontology system defines hooks as **KnowledgeHook** entities integrated into a comprehensive documentation and capability-discovery framework. Hooks are not isolated constructs but part of an elaborate RDF-based protocol system supporting:

1. **Knowledge Hook Execution Protocol** — state-driven hook lifecycle
2. **Type-enforced Hook Activation** — SHACL-validated hook properties
3. **N3 Rule-driven Hook Semantics** — IF-THEN patterns for hook evaluation, execution, and recovery
4. **Protocol-level Error Handling** — error handlers with recovery actions
5. **SLO/Quality Constraints** — Service Level Objectives on hook execution

---

## File Inventory

### Core Hook Definition Files

| File Path                                                      | Purpose                                     | Hook Definitions                                                                       |
| -------------------------------------------------------------- | ------------------------------------------- | -------------------------------------------------------------------------------------- |
| `/Users/sac/unrdf/playground/ontologies/playground.ttl`        | Hook ontology definition                    | `playground:KnowledgeHook` class, properties                                           |
| `/Users/sac/unrdf/playground/ontologies/playground-shapes.ttl` | SHACL shape validation for hooks            | `playground:KnowledgeHookShape` with constraints                                       |
| `/Users/sac/unrdf/docs/schema/documentation-ontology.ttl`      | Comprehensive documentation ontology        | `proto:ErrorHandler`, `proto:State`, `proto:Transition`, `cap:Capability` (1276 lines) |
| `/Users/sac/unrdf/docs/schema/examples.ttl`                    | Concrete protocol + hook execution examples | Knowledge Hook Execution Protocol, states, transitions                                 |
| `/Users/sac/unrdf/docs/machine/rules.n3`                       | N3 inference rules for hook behavior        | 10 rule sections (605 lines)                                                           |

### Related Protocol & Documentation Files

| File Path                                                         | Related Content                   |
| ----------------------------------------------------------------- | --------------------------------- |
| `/Users/sac/unrdf/docs/ontology/minimal-n3-routing.ttl`           | N3 routing patterns               |
| `/Users/sac/unrdf/test/fixtures/test-universe.ttl`                | Test fixtures with hook scenarios |
| `/Users/sac/unrdf/test-governance-cli/policies/system-policy.ttl` | Governance policies               |

---

## Class & Property Definitions

### 1. Core Hook Class

**Class:** `playground:KnowledgeHook`

```turtle
playground:KnowledgeHook a owl:Class ;
    rdfs:label "Knowledge Hook"@en ;
    rdfs:comment "A hook that intercepts and processes knowledge operations"@en ;
    rdfs:subClassOf owl:Thing .
```

**Semantic:** A hook intercepts knowledge graph operations and can execute conditional logic.

---

### 2. Hook Properties

#### From playground.ttl:

| Property                  | Domain                     | Range                      | Semantics                        |
| ------------------------- | -------------------------- | -------------------------- | -------------------------------- |
| `playground:hasHook`      | `playground:PolicyPack`    | `playground:KnowledgeHook` | Links a policy pack to its hooks |
| `playground:hookPriority` | `playground:KnowledgeHook` | `xsd:integer`              | Execution priority (numeric)     |
| `playground:isEnabled`    | `playground:KnowledgeHook` | `xsd:boolean`              | Hook activation status           |

#### From documentation-ontology.ttl (Protocol/ErrorHandler context):

| Property                | Domain                  | Range                | Semantics                                       |
| ----------------------- | ----------------------- | -------------------- | ----------------------------------------------- |
| `proto:hasErrorHandler` | `proto:Protocol`        | `proto:ErrorHandler` | Links protocol to error handler                 |
| `proto:handlesError`    | `proto:ErrorHandler`    | `xsd:string`         | Specific error type (e.g., "HookVetoError")     |
| `proto:recoveryAction`  | `proto:ErrorHandler`    | `cap:Capability`     | Recovery capability to execute                  |
| `proto:fallbackState`   | `proto:ErrorHandler`    | `proto:State`        | State to transition to on error                 |
| `proto:timeoutAction`   | `proto:TimeoutStrategy` | `xsd:string`         | Action on timeout: 'retry', 'abort', 'escalate' |

---

### 3. Hook Execution Protocol States

**Context:** Knowledge Hook Execution Protocol (from examples.ttl & documentation-ontology.ttl)

#### State Definitions

```turtle
ex:HookStateWaiting a proto:InitialState ;
    rdfs:label "Waiting"@en ;
    rdfs:comment "Initial state, hook ready to evaluate" .

ex:HookStateEvaluating a proto:State ;
    rdfs:label "Evaluating"@en ;
    rdfs:comment "Hook condition is being evaluated" .

ex:HookStateConditionMet a proto:State ;
    rdfs:label "Condition Met"@en ;
    rdfs:comment "Condition evaluated to true, ready for execution" .

ex:HookStateExecuting a proto:State ;
    rdfs:label "Executing"@en ;
    rdfs:comment "Hook action is executing" .

ex:HookStateCompleted a proto:FinalState ;
    rdfs:label "Completed"@en ;
    rdfs:comment "Hook executed successfully" .

ex:HookStateSkipped a proto:FinalState ;
    rdfs:label "Skipped"@en ;
    rdfs:comment "Hook condition evaluated to false, action skipped" .

ex:HookStateVetoed a proto:FinalState ;
    rdfs:label "Vetoed"@en ;
    rdfs:comment "Hook was vetoed (rejected) during execution" .
```

**State Machine Topology:**

```
Waiting
   ↓ (HookMsgTrigger)
Evaluating
   ├─→ ConditionMet (condition=true)
   │      ↓ (HookTransitionExecute)
   │   Executing → Completed
   │
   └─→ Skipped (condition=false)

   Any state + Error → ErrorHandler → Rollback/Recovery
```

---

### 4. Hook Transitions (State Machine)

**Key Transitions (from examples.ttl):**

| Transition                     | From State   | To State     | Triggered By            | Guard/Action                  |
| ------------------------------ | ------------ | ------------ | ----------------------- | ----------------------------- |
| `HookTransitionEvaluate`       | Waiting      | Evaluating   | `HookMsgTrigger`        | Evaluate condition            |
| `HookTransitionConditionTrue`  | Evaluating   | ConditionMet | (implicit)              | Condition ASK returns true    |
| `HookTransitionConditionFalse` | Evaluating   | Skipped      | (implicit)              | Condition ASK returns false   |
| `HookTransitionExecute`        | ConditionMet | Executing    | `HookTransitionExecute` | Execute action capability     |
| `HookTransitionVeto`           | Executing    | Vetoed       | `HookMsgVeto`           | Veto signal received          |
| `HookTransitionComplete`       | Executing    | Completed    | `HookMsgResult`         | Action completed successfully |

**RDF Representation (from examples.ttl):**

```turtle
ex:HookTransitionEvaluate a proto:Transition ;
    proto:fromState ex:HookStateWaiting ;
    proto:toState ex:HookStateEvaluating ;
    proto:triggeredBy ex:HookMsgTrigger .

ex:HookTransitionConditionTrue a proto:Transition ;
    proto:fromState ex:HookStateEvaluating ;
    proto:toState ex:HookStateConditionMet .

ex:HookTransitionExecute a proto:Transition ;
    proto:fromState ex:HookStateConditionMet ;
    proto:toState ex:HookStateExecuting ;
    proto:action ex:ExecuteHooksCapability .
```

---

### 5. Hook Condition & Action Definitions

#### Conditions (Cap:Precondition / Cap:Postcondition)

Hooks use SPARQL ASK queries or N3 formulas for condition evaluation:

```turtle
ex:HookCondition a cap:Condition ;
    rdfs:label "Hook Activation Condition"@en ;
    cap:conditionSparql """
        ASK WHERE {
            ?hook a ex:KnowledgeHook .
            ?hook ex:enabled true .
            ?hook ex:priority ?priority .
            FILTER(?priority > 0)
        }
    """ .
```

#### Actions (Cap:Capability)

Hooks execute capabilities; example from documentation:

```turtle
ex:ExecuteHooksCapability a cap:AtomicCapability ;
    rdfs:label "Execute Knowledge Hooks"@en ;
    cap:inputType ex:HookTriggerInput ;
    cap:outputType ex:HookExecutionResult ;
    cap:requires ex:HookRegistry ;
    cap:produces ex:HookExecutionLog .
```

---

## Hook Activation Rules (N3 Inference)

**File:** `/Users/sac/unrdf/docs/machine/rules.n3`

### Rule Section 1: Hook Execution Safety

```n3
# Rule: Identify operations requiring transaction
{
  ?capability unrdf:sideEffects "write" .
} => {
  ?capability unrdf:requiresTransaction true .
} .

# Rule: Error recovery recommendation
{
  ?capability unrdf:retrySafe true .
  ?error unrdf:type "timeout" .
} => {
  ?error unrdf:recoveryStrategy "retry" .
  ?error unrdf:maxRetries 3 .
} .
```

**Semantic:** Hooks on write operations must be transactional; timeout errors trigger retries.

---

### Rule Section 2: Hook Security & Sandboxing

```n3
# Rule: Sandbox required for untrusted code
{
  ?hook unrdf:source "external" .
} => {
  ?hook unrdf:requiresSandbox true .
  ?hook unrdf:sandboxLevel "strict" .
} .

# Rule: Audit logging required
{
  ?capability unrdf:sideEffects "write" .
} => {
  ?capability unrdf:requiresAuditLog true .
} .
```

**Semantic:** External hooks require strict sandboxing; write operations require audit logging.

---

### Rule Section 3: SLO Compliance & Hook Execution

```n3
# Rule: SLO is critical if severity is critical
{
  ?slo unrdf:severity "critical" .
} => {
  ?slo unrdf:alertPriority "P1" .
  ?slo unrdf:responseTime "5m" .
} .

# Rule: SLO breach detection
{
  ?measurement unrdf:metric ?metric .
  ?measurement unrdf:value ?value .
  ?slo unrdf:metric ?metric .
  ?slo unrdf:threshold ?threshold .
  ?value math:greaterThan ?threshold .
} => {
  ?measurement unrdf:breachesSLO ?slo .
  ?slo unrdf:status "breached" .
} .
```

**Semantic:** Hook execution latency is governed by SLOs; breach triggers P1 alerts.

---

## SHACL Shape Constraints

**File:** `/Users/sac/unrdf/playground/ontologies/playground-shapes.ttl`

```turtle
playground:KnowledgeHookShape a sh:NodeShape ;
    sh:targetClass playground:KnowledgeHook ;
    sh:property [
        sh:path rdfs:label ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Knowledge Hook must have a label"@en ;
    ] ;
    sh:property [
        sh:path playground:hookPriority ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 100 ;
        sh:message "Hook priority must be between 0 and 100"@en ;
    ] ;
    sh:property [
        sh:path playground:isEnabled ;
        sh:datatype xsd:boolean ;
        sh:maxCount 1 ;
    ] .
```

**Constraints Enforced:**

1. **Label Required:** Every hook must have an `rdfs:label`
2. **Priority Range:** `hookPriority ∈ [0, 100]` (integer)
3. **Enabled Status:** Optional boolean flag; max 1 value

---

## Error Handling & Recovery

### Error Handler Model

**From documentation-ontology.ttl (proto namespace):**

```turtle
proto:ErrorHandler a rdfs:Class, owl:Class ;
    rdfs:label "Error Handler"@en ;
    rdfs:comment "A handler for protocol errors with recovery strategies."@en .
```

**Example Error Handlers (from examples.ttl):**

```turtle
ex:TxHookErrorHandler a proto:ErrorHandler ;
    rdfs:label "Hook Error Handler"@en ;
    proto:handlesError "HookVetoError" ;
    proto:recoveryAction ex:RollbackCapability .

ex:TxTimeoutHandler a proto:ErrorHandler ;
    rdfs:label "Timeout Error Handler"@en ;
    proto:handlesError "TimeoutError" ;
    proto:timeoutAction "rollback" .

ex:HookConditionErrorHandler a proto:ErrorHandler ;
    rdfs:label "Hook Condition Error Handler"@en ;
    proto:handlesError "ConditionEvaluationError" ;
    proto:recoveryAction ex:LogErrorCapability ;
    proto:timeoutAction "skip" .

ex:HookExecutionErrorHandler a proto:ErrorHandler ;
    rdfs:label "Hook Execution Error Handler"@en ;
    proto:handlesError "ExecutionError" ;
    proto:recoveryAction ex:RollbackCapability .
```

---

## Protocol-Level Hook Specification

**Knowledge Hook Execution Protocol (from examples.ttl):**

```turtle
ex:KnowledgeHookProtocol a proto:Protocol ;
    rdfs:label "Knowledge Hook Execution Protocol"@en ;
    proto:hasInitialState ex:HookStateWaiting ;
    proto:hasState ex:HookStateEvaluating ;
    proto:hasState ex:HookStateConditionMet ;
    proto:hasState ex:HookStateExecuting ;
    proto:hasFinalState ex:HookStateCompleted ;
    proto:hasFinalState ex:HookStateSkipped ;
    proto:hasFinalState ex:HookStateVetoed ;
    proto:hasTransition ex:HookTransitionEvaluate ;
    proto:hasTransition ex:HookTransitionConditionTrue ;
    proto:hasTransition ex:HookTransitionConditionFalse ;
    proto:hasTransition ex:HookTransitionExecute ;
    proto:hasTransition ex:HookTransitionVeto ;
    proto:hasTransition ex:HookTransitionComplete ;
    proto:hasMessage ex:HookMsgTrigger ;
    proto:hasMessage ex:HookMsgResult ;
    proto:hasMessage ex:HookMsgVeto ;
    proto:hasErrorHandler ex:HookConditionErrorHandler ;
    proto:hasErrorHandler ex:HookExecutionErrorHandler ;
    proto:hasTimeoutStrategy [
        a proto:TimeoutStrategy ;
        proto:timeoutMs 5000 ;
        proto:timeoutAction "skip"
    ] .
```

**Protocol Messages:**

| Message          | Class            | Usage                               |
| ---------------- | ---------------- | ----------------------------------- |
| `HookMsgTrigger` | `proto:Request`  | Initiates hook condition evaluation |
| `HookMsgResult`  | `proto:Response` | Communicates execution result       |
| `HookMsgVeto`    | `proto:Event`    | Signals veto/rejection of hook      |

---

## Hook Integration with Policies & PolicyPacks

**From playground.ttl:**

```turtle
playground:PolicyPack a owl:Class ;
    rdfs:label "Policy Pack"@en ;
    rdfs:comment "A collection of policies for knowledge validation"@en .

playground:hasHook a owl:ObjectProperty ;
    rdfs:label "has hook"@en ;
    rdfs:domain playground:PolicyPack ;
    rdfs:range playground:KnowledgeHook .

playground:hasRule a owl:ObjectProperty ;
    rdfs:label "has rule"@en ;
    rdfs:domain playground:PolicyPack ;
    rdfs:range playground:ValidationRule .
```

**Semantic:** Policies group hooks and rules together for coordinated activation.

---

## Hook Execution Guarantees & SLOs

**Service Level Objectives on Hook Execution:**

From `documentation-ontology.ttl`:

```turtle
slo:PerformanceSLO a rdfs:Class, owl:Class ;
    rdfs:label "Performance SLO"@en ;
    rdfs:comment "SLO related to performance metrics (latency, throughput)"@en .

slo:hasSLO a rdf:Property, owl:ObjectProperty ;
    rdfs:label "has SLO"@en ;
    rdfs:domain [ owl:unionOf ( cap:Capability proto:Protocol ) ] ;
    rdfs:range slo:ServiceLevelObjective .

slo:latencyMs a rdf:Property, owl:DatatypeProperty ;
    rdfs:label "latency milliseconds"@en ;
    rdfs:comment "Target latency in milliseconds"@en ;
    rdfs:domain slo:PerformanceSLO ;
    rdfs:range xsd:integer .
```

**Example (from examples.ttl):**

```turtle
ex:SparqlQueryLatencySLO a slo:PerformanceSLO ;
    rdfs:label "Query Latency SLO"@en ;
    slo:metric slo:LatencyMetric ;
    slo:percentile 99 ;
    slo:latencyMs 100 ;
    slo:target [
        a slo:Threshold ;
        slo:thresholdValue 100.0 ;
        slo:thresholdUnit "ms" ;
        slo:comparisonOperator "lessThan"
    ] ;
    slo:hasAlert [
        a slo:Alert ;
        slo:alertSeverity "warning" ;
        slo:alertThreshold 0.8
    ] .
```

---

## Hook Composition & Workflow Rules

**From rules.n3 — Section 8: Workflow Composition:**

```n3
# Rule: Workflow step ordering by dependencies
{
  ?step1 unrdf:produces ?output .
  ?step2 unrdf:requires ?output .
} => {
  ?step2 unrdf:dependsOn ?step1 .
  ?step1 unrdf:precedes ?step2 .
} .

# Rule: Parallel execution allowed when no dependencies
{
  ?step1 unrdf:inWorkflow ?workflow .
  ?step2 unrdf:inWorkflow ?workflow .
  ?step1 log:notEqualTo ?step2 .
  NOT { ?step1 unrdf:dependsOn ?step2 } .
  NOT { ?step2 unrdf:dependsOn ?step1 } .
} => {
  ?step1 unrdf:canParallelWith ?step2 .
} .
```

**Semantic:** Multiple hooks can be composed; execution order determined by data dependencies.

---

## Authorization & Access Control for Hooks

**From documentation-ontology.ttl:**

```turtle
auth:requiresAuth a rdf:Property, owl:ObjectProperty ;
    rdfs:label "requires authorization"@en ;
    rdfs:comment "Links a capability to its required authorization level"@en ;
    rdfs:domain cap:Capability ;
    rdfs:range auth:AuthorizationLevel .

auth:authLevel a rdf:Property, owl:DatatypeProperty ;
    rdfs:label "authorization level"@en ;
    rdfs:comment "Numeric level: 0=public, 1=authenticated, 2=authorized, 3=admin"@en ;
    rdfs:domain auth:AuthorizationLevel ;
    rdfs:range xsd:integer .
```

**Authorization Levels:**

- **0 (Public):** No authentication required
- **1 (Authenticated):** Valid authentication required
- **2 (Authorized):** Specific authorization/permission required
- **3 (Admin):** Administrative privileges required

---

## Complete Hook Lifecycle (IF-THEN Rules Summary)

**From rules.n3 — Inferred Hook Execution Flow:**

```
IF hook.source = "external"
  THEN hook.requiresSandbox = true AND hook.sandboxLevel = "strict"

IF hook.enabled = true AND hook.priority > 0
  THEN hook.canActivate = true

IF hook.type = "write"
  THEN hook.requiresTransaction = true AND hook.requiresAuditLog = true

IF hook.condition evaluates to TRUE
  THEN transition from "Evaluating" → "ConditionMet" → "Executing"

IF hook.action completes without error
  THEN transition to "Completed"

IF hook.action times out (>5000ms default)
  THEN execute timeoutAction ("skip" | "retry" | "abort" | "escalate")

IF hook.error.type = "HookVetoError"
  THEN execute recoveryAction (e.g., rollback)

IF hook.execution.latency > SLO.threshold
  THEN alert with severity = "P1" or "P2" based on criticality
```

---

## Key Design Patterns

### 1. **State-Machine-Driven Hook Execution**

Hooks progress through well-defined states (Waiting → Evaluating → Executing → Terminal). Each transition is guarded by conditions and may trigger actions (capabilities).

### 2. **Condition-Action Separation**

- **Condition:** SPARQL ASK query or N3 formula evaluated before action
- **Action:** Atomic capability executed if condition is true

### 3. **Error Handling as First-Class Concept**

Every protocol state may transition to an error handler with configurable recovery actions (retry, rollback, escalate, skip).

### 4. **SLO-Driven Execution Guarantees**

Hook execution latency, throughput, and error rates are constrained by Service Level Objectives with automated alerting.

### 5. **Multi-Level Authorization**

Hooks require authorization levels (0–3) and specific permissions (e.g., "write:hooks", "validate:data").

### 6. **Sandbox Isolation for External Hooks**

External hooks are marked `requiresSandbox=true` and executed in a `sandboxLevel="strict"` environment.

### 7. **Transactional Write Hooks**

Hooks with side effects ("write") automatically require transactions and audit logging.

---

## Summary Table: Hook Activation Conditions

| Condition                   | Property                                   | Rule                                             |
| --------------------------- | ------------------------------------------ | ------------------------------------------------ |
| **Hook Enabled**            | `playground:isEnabled`                     | Must be `true`                                   |
| **Priority Valid**          | `playground:hookPriority`                  | `∈ [0, 100]`                                     |
| **Label Required**          | `rdfs:label`                               | Must exist (SHACL constraint)                    |
| **Condition Satisfiable**   | `cap:conditionSparql` or `cap:conditionN3` | ASK/N3 must evaluate to true                     |
| **Auth Level Met**          | `auth:requiresAuth`                        | Principal must have `auth:authLevel >= required` |
| **SLO Allows**              | `slo:hasSLO`                               | Current latency must be `< slo:latencyMs`        |
| **Dependencies Ready**      | (inferred by N3 rules)                     | All prerequisite capabilities/hooks completed    |
| **External Hook Sandboxed** | `unrdf:requiresSandbox`                    | Must be sandboxed if source="external"           |

---

## Recommended Further Investigation

1. **Implementation Layer:** Query `~/unrdf/` for Python/TypeScript code that translates these RDF hook definitions into executable logic
2. **Hook Registry:** Look for hook discovery/registry mechanisms (likely in services or config)
3. **N3 Engine Integration:** Identify which N3 reasoner (e.g., `npm3`, Comunica plugin) executes the rules.n3 rules
4. **Event Sourcing:** Check for event logs proving hook execution traces against the state machine model
5. **Veto Mechanism:** Clarify the veto signal pathway — is it synchronous or asynchronous?

---

**End of Report**
