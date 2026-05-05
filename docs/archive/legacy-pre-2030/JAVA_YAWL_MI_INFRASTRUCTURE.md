# YAWL Multiple Instance Infrastructure Analysis

> **Research Date**: 2026-01-11
> **Focus**: Runtime mechanics and infrastructure for Multiple Instance pattern execution in Java YAWL
> **Purpose**: Guide implementation of MI infrastructure in @unrdf/daemon

---

## Executive Summary

Java YAWL implements Multiple Instance (MI) patterns through a sophisticated infrastructure combining:
- **Hierarchical identifiers** (YIdentifier) for parent-child tracking
- **Work item repository** (YWorkItemRepository) managing lifecycle states
- **Threshold-based synchronization** for completion conditions
- **Custom Service architecture** (Interface B) for distributed execution
- **Cancellation regions** for coordinated instance termination

**Key Finding**: YAWL does NOT use thread pools internally. Instead, it delegates execution to **Custom Services** via Interface B, making it a **service-oriented** rather than thread-pool-based architecture.

---

## 1. Multiple Instance Metadata Storage

### YTask.getMultiInstanceAttributes()

**API Signature**:
```java
public YMultiInstanceAttributes getMultiInstanceAttributes()
```

**Configuration Method**:
```java
public void setUpMultipleInstanceAttributes(
    String minInstanceQuery,    // XQuery: minimum instances to create
    String maxInstanceQuery,    // XQuery: maximum instances allowed
    String thresholdQuery,      // XQuery: instances needed to complete
    String creationMode         // "static" or "dynamic"
)
```

### YMultiInstanceAttributes Structure

Contains:
- **Min/Max/Threshold Queries**: XQuery expressions evaluated at runtime against workflow data
- **Creation Mode**:
  - `"static"` - All instances created upfront, no additions allowed
  - `"dynamic"` - Instances can be added during execution
- **Input Data Mappings**: How data distributes to instances
- **Output Data Mappings**: How instance results aggregate

**Data Mapping Methods**:
```java
// Distribute data TO instances
public void setMultiInstanceInputDataMappings(
    String remoteVariableName,
    String inputProcessingExpression
)

// Aggregate data FROM instances
public void setMultiInstanceOutputDataMappings(
    String remoteOutputQuery,
    String aggregationQuery
)
```

### Internal Conditions (MI Lifecycle Tracking)

YAWL uses **internal conditions** (Petri net places) to track MI state:

```java
public YInternalCondition getMIEntered()    // Instances created
public YInternalCondition getMIActive()     // Instances enabled
public YInternalCondition getMIExecuting()  // Instances running
public YInternalCondition getMIComplete()   // Instances finished
```

**Pattern**: These conditions form a **state machine** inside each MI task, tracking collective progress.

---

## 2. Instance Spawning Mechanism

### YIdentifier: Hierarchical Instance Tracking

**Core Concept**: YIdentifier objects are **tokens** flowing through the net. For MI tasks, they form **parent-child trees**.

**Creation Methods**:
```java
// Create next sequential child
public YIdentifier createChild(YPersistenceManager pmgr)
    throws YPersistenceException

// Create child with specific number
public YIdentifier createChild(YPersistenceManager pmgr, int childNum)
    throws YPersistenceException
```

**Parent-Child API**:
```java
// Relationship management
public void set_parent(YIdentifier parent)
public YIdentifier get_parent()
public List<YIdentifier> get_children()
public void removeChild(YIdentifier child)
public void clearChildren()

// Ancestry queries
public boolean isImmediateChildOf(YIdentifier identifier)
public boolean isAncestorOf(YIdentifier identifier)
public Set<YIdentifier> getDescendants()
public YIdentifier getRootAncestor()
```

### Instance ID Generation Pattern

**Hierarchical Naming**:
```
Parent WorkItem:   123:Enrol_5
Child Instance 1:  123.1:Enrol_5
Child Instance 2:  123.2:Enrol_5
Child Instance 3:  123.3:Enrol_5

Nested MI Composite:
  Parent:          n.n.n:CompositeTask
  MI Instance 1:   n.n.n.1:CompositeTask
    Atomic Task:   n.n.n.1.1:AtomicTask
  MI Instance 2:   n.n.n.2:CompositeTask
    Atomic Task:   n.n.n.2.1:AtomicTask
```

**Format**: `{parent_id}.{child_number}:{task_name}`

**Properties**:
- Globally unique within case
- Encodes full ancestry path
- Supports arbitrary nesting depth
- Enables efficient parent/child queries

---

## 3. Work Item Lifecycle and State Management

### YWorkItemRepository

**Purpose**: Centralized store of all work items across all cases.

**Lifecycle States**:
```
Enabled → Fired → Executing → Completed
```

**State Semantics**:
- **Enabled**: Task ready to start, work item announced to services
- **Fired**: Parent MI task checked out, child instances created
- **Executing**: Instance actively being performed by Custom Service
- **Completed**: Instance finished, data returned

### Multiple Instance Work Item Pattern

**When MI task is checked out**:
```
1. Parent work item transitions: Enabled → Fired
2. N child work items created (based on minInstanceQuery)
3. ONE child chosen for immediate execution: Fired → Executing
4. Remaining children stay in Fired state
5. As Custom Service requests more work: Fired → Executing
```

**Critical Insight**: The parent work item **never executes**. It's a **coordination container** for child instances.

### Key Repository Methods

```java
// State queries
Set<YWorkItem> getEnabledWorkItems()
Set<YWorkItem> getFiredWorkItems()
Set<YWorkItem> getExecutingWorkItems()
Set<YWorkItem> getCompletedWorkItems()

// Family management
Set<YWorkItem> getChildrenOf(String workItemID)
Set<YWorkItem> getParentWorkItems()
void removeWorkItemFamily(YWorkItem workItem)  // Remove parent + all children

// Case management
Set<YWorkItem> getWorkItemsForCase(YIdentifier caseID)
void removeWorkItemsForCase(YIdentifier caseID)
```

---

## 4. Execution Model: Custom Services via Interface B

### Service-Oriented Architecture (NOT Thread Pools)

**Key Architectural Decision**: YAWL Engine does **NOT** execute work items directly. Instead:

```
┌─────────────────┐
│   YAWL Engine   │  Manages workflow logic, state transitions
└────────┬────────┘
         │ Interface B (HTTP/REST)
         │
    ┌────┴────┬─────────┬─────────┐
    │         │         │         │
┌───▼──┐  ┌──▼───┐  ┌──▼───┐  ┌──▼───┐
│Worklist│ │Custom│  │Custom│  │Custom│
│Handler │ │Svc 1 │  │Svc 2 │  │Svc 3 │  External services
└────────┘ └──────┘  └──────┘  └──────┘
```

**Responsibilities**:
- **Engine**: Orchestration, state management, data flow
- **Custom Services**: Actual work execution (parallel execution decided by service)

### Interface B API

**Core Operations**:
```java
// Custom Service checks out work
WorkItemRecord checkOut(String workItemID, String serviceID)

// Custom Service completes work
String checkInWorkItem(String workItemID, String serviceID, String data)

// Query available work
Set<WorkItemRecord> getAvailableWorkItems(String serviceID)
```

**For Multiple Instance**:
```java
// Service can pull multiple MI instances concurrently
for (WorkItemRecord item : getAvailableWorkItems(myServiceID)) {
    if (item.getTaskID().equals(miTaskID)) {
        // Service decides: sequential, parallel, thread pool, etc.
        checkOutAndExecute(item);
    }
}
```

**Execution Strategy**: Each Custom Service independently decides:
- Thread pool size
- Parallel vs sequential execution
- Resource allocation
- Retry logic

**Implication for @unrdf/daemon**: We need to implement:
1. Engine core (workflow orchestration)
2. Interface B API (service communication)
3. One or more Custom Services (work execution)

---

## 5. Synchronization Mechanisms

### Threshold-Based Completion

**Configuration**:
```java
setUpMultipleInstanceAttributes(
    "3",        // minInstanceQuery: Start with 3 instances
    "10",       // maxInstanceQuery: Never exceed 10 instances
    "5",        // thresholdQuery: Complete when 5 finish
    "dynamic"   // creationMode: Allow adding instances
)
```

**Behavior**:
```
Time T0: minInstanceQuery=3 → Create 3 instances
Time T1: User requests +2 → Create 2 more (total 5, ≤ max 10)
Time T2: 5 instances complete → thresholdQuery=5 met
Time T3: Task completes, remaining instances CANCELLED
```

**Special Values**:
- `thresholdQuery="infinite"` → All instances must complete
- `minInstanceQuery = maxInstanceQuery` → Fixed instance count
- `thresholdQuery < minInstanceQuery` → Task can complete before all initial instances finish

### Internal Synchronization via Petri Net Places

**MI Task Internal Structure**:
```
            ┌───────────────────────────────┐
            │      Multiple Instance        │
            │           Task                │
            ├───────────────────────────────┤
Input ─────>│ MI_Entered   (instances created) │
            │      ↓                         │
            │ MI_Active    (instances enabled)│
            │      ↓                         │
            │ MI_Executing (instances running)│
            │      ↓                         │
            │ MI_Complete  (threshold met)   │────> Output
            └───────────────────────────────┘
```

**Token Semantics**:
- One token in `MI_Entered` per instance created
- Tokens flow to `MI_Complete` as instances finish
- When `MI_Complete` has `thresholdQuery` tokens → task completes

**Concurrency Control**: Built into Petri net semantics:
- Atomic token movement
- Transitions fire when all input places have required tokens
- No explicit locks/mutexes needed at workflow level

---

## 6. Data Flow To/From Instances

### Input Distribution

**Method**:
```java
setMultiInstanceInputDataMappings(
    "candidate",                    // Variable name in each instance
    "/candidates/candidate[position()=?]"  // XPath to extract from array
)
```

**Execution**:
```xml
<!-- Parent task data -->
<candidates>
    <candidate><name>Alice</name></candidate>
    <candidate><name>Bob</name></candidate>
    <candidate><name>Carol</name></candidate>
</candidates>

<!-- Instance 1 receives -->
<candidate><name>Alice</name></candidate>

<!-- Instance 2 receives -->
<candidate><name>Bob</name></candidate>

<!-- Instance 3 receives -->
<candidate><name>Carol</name></candidate>
```

**Pattern**: XPath `position()=?` replaced with instance number (1, 2, 3...).

### Output Aggregation

**Method**:
```java
setMultiInstanceOutputDataMappings(
    "/candidate/score",           // Extract score from each instance
    "sum(/scores/score)"          // Aggregate function
)
```

**Execution**:
```xml
<!-- Instance 1 returns -->
<candidate><score>85</score></candidate>

<!-- Instance 2 returns -->
<candidate><score>92</score></candidate>

<!-- Instance 3 returns -->
<candidate><score>78</score></candidate>

<!-- Parent task receives -->
<scores>
    <score>85</score>
    <score>92</score>
    <score>78</score>
</scores>
<totalScore>255</totalScore>  <!-- sum() result -->
```

**Supported Aggregations**:
- `sum()`, `avg()`, `min()`, `max()`, `count()`
- Custom XQuery expressions
- Array construction `array { ... }`

---

## 7. Cancellation and Error Handling

### Cancellation Regions

**Definition**: A set of tasks that are **collectively cancelled** when a specific condition occurs.

**Multiple Instance Cancellation**:
```
When MI task cancels:
1. Parent work item removed from repository
2. All child work items with status != Completed are cancelled
3. Custom Services notified via Interface B
4. Completed instances remain in repository (audit trail)
```

**Cancellation Region Setup**:
- MI task included in cancellation region
- **Must include ARCS** into tasks, not just tasks themselves
- When region trigger fires → all active tokens removed

**Example**:
```
Interview Candidates (MI: 1-10 instances, threshold=3)
   │
   ├─> Instance 1 (Completed)
   ├─> Instance 2 (Executing)
   ├─> Instance 3 (Completed)
   ├─> Instance 4 (Executing)
   ├─> Instance 5 (Fired)
   └─> [Cancellation triggered]

Result:
   - Instances 1, 3: Remain as Completed (audit)
   - Instances 2, 4, 5: Cancelled, removed
   - Task moves to next state
```

### Threshold-Triggered Cancellation

**Automatic Behavior**:
```
When thresholdQuery instances complete:
1. Remaining Fired/Executing instances automatically cancelled
2. MI_Complete condition satisfied
3. Task completes
```

**No explicit cancellation region needed** - built into MI semantics.

---

## 8. Implementation Insights for @unrdf/daemon

### Architecture Recommendations

**1. Identifier Management**
```javascript
class YAWLIdentifier {
  constructor(parent = null, childNum = null) {
    this.id = parent ? `${parent.id}.${childNum}` : '1';
    this.parent = parent;
    this.children = [];
  }

  createChild() {
    const childNum = this.children.length + 1;
    const child = new YAWLIdentifier(this, childNum);
    this.children.push(child);
    return child;
  }

  isAncestorOf(other) {
    return other.id.startsWith(this.id + '.');
  }
}
```

**2. Work Item Repository**
```javascript
class WorkItemRepository {
  constructor() {
    this.items = new Map(); // workItemID → WorkItem
    this.byState = {
      enabled: new Set(),
      fired: new Set(),
      executing: new Set(),
      completed: new Set()
    };
    this.children = new Map(); // parentID → Set<childIDs>
  }

  getChildrenOf(workItemID) {
    return Array.from(this.children.get(workItemID) || [])
      .map(id => this.items.get(id));
  }

  removeWorkItemFamily(workItem) {
    // Recursively remove parent and all descendants
    const toRemove = [workItem.id];
    for (const id of toRemove) {
      const children = this.children.get(id) || [];
      toRemove.push(...children);
    }
    toRemove.forEach(id => this.remove(id));
  }
}
```

**3. Multiple Instance Attributes**
```javascript
class MultiInstanceAttributes {
  constructor({
    minInstanceQuery,    // XPath/JSONPath expression
    maxInstanceQuery,
    thresholdQuery,
    creationMode,        // 'static' | 'dynamic'
    inputMappings,       // { varName: xpathExpr }
    outputMappings       // { varName: aggregateFn }
  }) {
    this.minInstanceQuery = minInstanceQuery;
    this.maxInstanceQuery = maxInstanceQuery;
    this.thresholdQuery = thresholdQuery;
    this.creationMode = creationMode;
    this.inputMappings = inputMappings;
    this.outputMappings = outputMappings;
  }

  evaluateMin(data) {
    return this.evaluateQuery(this.minInstanceQuery, data);
  }

  evaluateMax(data) {
    return this.evaluateQuery(this.maxInstanceQuery, data);
  }

  evaluateThreshold(data) {
    const result = this.evaluateQuery(this.thresholdQuery, data);
    return result === 'infinite' ? Infinity : result;
  }
}
```

**4. Instance Spawning**
```javascript
class MITaskExecutor {
  async spawnInstances(task, parentWorkItem, data) {
    const miAttrs = task.getMultiInstanceAttributes();
    const min = miAttrs.evaluateMin(data);
    const max = miAttrs.evaluateMax(data);
    const threshold = miAttrs.evaluateThreshold(data);

    // Transition parent: enabled → fired
    parentWorkItem.setState('fired');

    // Create child work items
    const children = [];
    for (let i = 1; i <= min; i++) {
      const childId = parentWorkItem.identifier.createChild();
      const childData = miAttrs.distributeInputData(data, i);
      const childWorkItem = new WorkItem({
        id: `${parentWorkItem.id}.${i}`,
        identifier: childId,
        taskID: task.id,
        data: childData,
        state: 'fired'
      });
      children.push(childWorkItem);
      this.repository.add(childWorkItem);
    }

    // Announce first child as executing
    children[0].setState('executing');

    return { parentWorkItem, children, threshold };
  }
}
```

**5. Threshold Synchronization**
```javascript
class MISynchronizer {
  constructor(threshold) {
    this.threshold = threshold;
    this.completed = 0;
    this.completionPromise = new Promise(resolve => {
      this.resolveCompletion = resolve;
    });
  }

  async instanceCompleted(workItem, result) {
    this.completed++;

    if (this.completed >= this.threshold) {
      this.resolveCompletion();
    }

    return this.completed >= this.threshold;
  }

  async waitForCompletion() {
    return this.completionPromise;
  }
}
```

**6. Interface B Service Communication**
```javascript
class InterfaceB {
  constructor(engine) {
    this.engine = engine;
    this.services = new Map(); // serviceID → CustomService
  }

  // Custom Service APIs
  async checkOut(workItemID, serviceID) {
    const workItem = this.engine.repository.get(workItemID);
    if (workItem.state !== 'fired') {
      throw new Error('Work item not in fired state');
    }
    workItem.setState('executing');
    workItem.assignedService = serviceID;
    return workItem.toRecord();
  }

  async checkIn(workItemID, serviceID, resultData) {
    const workItem = this.engine.repository.get(workItemID);
    if (workItem.assignedService !== serviceID) {
      throw new Error('Service not authorized');
    }

    workItem.data = resultData;
    workItem.setState('completed');

    // Check if parent MI task threshold met
    await this.engine.checkMICompletion(workItem);

    return workItem.id;
  }

  async getAvailableWorkItems(serviceID) {
    const service = this.services.get(serviceID);
    return this.engine.repository
      .getEnabledWorkItems()
      .filter(item => service.canHandle(item));
  }
}
```

### Thread Pool Strategy (Service-Side)

**YAWL delegates parallelism to Custom Services**. Our daemon can implement:

**Option 1: Built-in Worker Pool**
```javascript
class WorkerPoolService {
  constructor(concurrency = 10) {
    this.pool = new WorkerPool({ size: concurrency });
    this.interfaceB = new InterfaceBClient(engineURL);
  }

  async start() {
    while (true) {
      const workItems = await this.interfaceB.getAvailableWorkItems(this.id);

      // Execute in parallel using pool
      await Promise.all(
        workItems.map(item => this.pool.execute(async () => {
          const checkedOut = await this.interfaceB.checkOut(item.id, this.id);
          const result = await this.performWork(checkedOut);
          await this.interfaceB.checkIn(item.id, this.id, result);
        }))
      );

      await sleep(1000); // Poll interval
    }
  }
}
```

**Option 2: External Services (Microservices)**
- Deploy separate service containers
- Each registers with Interface B
- Engine distributes work via REST/gRPC
- Services manage own concurrency

**Recommendation**: Start with Option 1 (built-in pool), evolve to Option 2 for scale.

---

## 9. Key Differences from Traditional Thread Pools

| Aspect | Thread Pool Model | YAWL Model |
|--------|-------------------|------------|
| Execution | Engine spawns threads | Custom Services execute |
| Parallelism | Pool size limit | Service decides concurrency |
| Distribution | In-memory queue | HTTP/REST API |
| Failure Handling | Retry in same JVM | Service can restart independently |
| Scalability | Single JVM limit | Distribute across machines |

**Advantages of YAWL's Approach**:
- **Polyglot Services**: Python, JavaScript, Rust services coexist
- **Independent Scaling**: Scale services independently
- **Fault Isolation**: Service crash doesn't crash engine
- **Heterogeneous Resources**: CPU-bound vs IO-bound services on different hardware

**Challenges**:
- Network latency overhead
- More complex deployment
- Eventual consistency issues

---

## 10. Observability and Monitoring

### Event Logging (YEventLogger)

**Database Schema**:
```sql
YLogTaskInstance
  - instanceID (PK)
  - parentInstanceID (FK → YLogTaskInstance)  -- Parent-child tracking
  - taskID
  - taskName
  - startTime
  - completionTime
  - status (enabled, fired, executing, completed, cancelled)
```

**Tracked Events**:
- Task enabled
- Work item checked out (→ executing)
- Work item checked in (→ completed)
- Task cancelled
- MI instance created
- MI threshold reached

**Metrics to Track**:
```javascript
{
  "task_id": "InterviewCandidates",
  "mi_metrics": {
    "min_instances": 3,
    "max_instances": 10,
    "threshold": 5,
    "instances_created": 8,
    "instances_completed": 5,
    "instances_cancelled": 3,
    "completion_time_ms": 45000,
    "avg_instance_duration_ms": 9000
  }
}
```

---

## 11. Code Snippets from YAWL Codebase

### YIdentifier.createChild (Decompiled Pattern)

```java
public YIdentifier createChild(YPersistenceManager pmgr, int childNum)
    throws YPersistenceException {

    YIdentifier child = new YIdentifier(this._idString + "." + childNum);
    child.set_parent(this);
    this._children.add(child);

    if (pmgr != null) {
        pmgr.storeObject(child);  // Persist to database
    }

    return child;
}
```

### YWorkItemRepository.getChildrenOf

```java
public Set<YWorkItem> getChildrenOf(String workItemID) {
    Set<YWorkItem> children = new HashSet<>();

    for (YWorkItem item : _allItems.values()) {
        if (item.getParentID() != null &&
            item.getParentID().equals(workItemID)) {
            children.add(item);
        }
    }

    return children;
}
```

### YTask MI Instance Creation (Conceptual)

```java
public void startMultiInstanceTask(YTask task, YIdentifier caseID, Map<String, Object> data) {
    YMultiInstanceAttributes miAttrs = task.getMultiInstanceAttributes();

    int min = evaluateQuery(miAttrs.getMinInstanceQuery(), data);
    int max = evaluateQuery(miAttrs.getMaxInstanceQuery(), data);
    int threshold = evaluateQuery(miAttrs.getThresholdQuery(), data);

    // Create parent work item
    YWorkItem parent = new YWorkItem(task, caseID, data);
    parent.setState(YWorkItemStatus.Fired);
    workItemRepository.add(parent);

    // Spawn child instances
    for (int i = 1; i <= min; i++) {
        YIdentifier childID = caseID.createChild(persistenceManager, i);
        Map<String, Object> childData = distributeData(data, miAttrs, i);

        YWorkItem child = new YWorkItem(task, childID, childData);
        child.setParent(parent);
        child.setState(YWorkItemStatus.Fired);
        workItemRepository.add(child);
    }

    // Announce first child to services
    YWorkItem firstChild = workItemRepository.getChildrenOf(parent.getID()).iterator().next();
    announceWorkItem(firstChild);
}
```

---

## 12. Critical Implementation Checklist for @unrdf/daemon

### Core Infrastructure
- [ ] `YAWLIdentifier` class with parent/child tracking
- [ ] `WorkItemRepository` with state management and family operations
- [ ] `MultiInstanceAttributes` with XPath/JSONPath evaluation
- [ ] `MITaskExecutor` for spawning instances
- [ ] `MISynchronizer` for threshold-based completion

### Service Architecture
- [ ] `InterfaceB` HTTP/REST API
- [ ] `CustomService` base class
- [ ] Built-in worker pool service
- [ ] Service registration and discovery

### Data Handling
- [ ] Input data distribution via XPath/JSONPath
- [ ] Output data aggregation (sum, avg, array construction)
- [ ] Variable scoping (parent vs child data)

### Synchronization
- [ ] Threshold evaluation (including "infinite")
- [ ] Automatic cancellation of remaining instances
- [ ] Completion notification to parent task

### Cancellation
- [ ] Cancellation region support
- [ ] Recursive cancellation (parent → children)
- [ ] Service notification on cancellation

### Observability
- [ ] Event logging to OTEL spans
- [ ] MI-specific metrics (instances created, completed, cancelled)
- [ ] Parent-child relationship tracking in logs

### Testing
- [ ] Static MI (fixed instances)
- [ ] Dynamic MI (add instances at runtime)
- [ ] Threshold < min (early completion)
- [ ] Threshold = infinity (all must complete)
- [ ] Nested MI (composite tasks with MI decomposition)
- [ ] Cancellation during execution

---

## 13. Research Sources

### Official Documentation
- [YAWL Technical Manual v4.3](https://yawlfoundation.github.io/assets/files/YAWLTechnicalManual4.pdf) - Work item lifecycle, MI semantics
- [YAWL Technical Manual v5.0](https://yawlfoundation.github.io/assets/files/YAWLTechnicalManual5.0.pdf) - Latest architecture
- [YAWL User Manual v4.3](https://yawlfoundation.github.io/assets/files/YAWLUserManual4.3.pdf) - Configuration examples

### JavaDoc API References
- [YIdentifier](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/elements/state/YIdentifier.html) - Parent-child tracking, createChild methods
- [YWorkItemRepository](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/engine/YWorkItemRepository.html) - Work item lifecycle
- [YTask](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/elements/YTask.html) - Multiple instance attributes
- [YEventLogger](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/logging/YEventLogger.html) - Logging infrastructure
- [InterfaceBWebsideController](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/engine/interfce/interfaceB/InterfaceBWebsideController.html) - Custom Service API

### GitHub Repository
- [yawlfoundation/yawl](https://github.com/yawlfoundation/yawl) - Source code
- [Issue #644: Dynamic Multiple Instance creation](https://github.com/yawlfoundation/yawl/issues/644) - Runtime behavior insights
- [Issue #647: ResourceManager multiple instances](https://github.com/yawlfoundation/yawl/issues/647) - ID generation patterns

### Academic Papers
- [YAWL: Yet Another Workflow Language (van der Aalst)](https://www.vdaalst.com/publications/p174.pdf) - Formal semantics
- [Design and Implementation of the YAWL System](https://www.researchgate.net/publication/27463106_Design_and_Implementation_of_the_YAWL_System) - Architecture decisions

### Workflow Patterns
- [Workflow Patterns - Multiple Instance Data](http://www.workflowpatterns.com/patterns/data/visibility/wdp4.php) - Data visibility patterns
- [Workflow Patterns - Cancel Multiple Instance Task](http://www.workflowpatterns.com/patterns/control/new/wcp26.php) - Cancellation semantics

### Community Resources
- [YAUG: Using Multiple Instance Tasks](https://www.yaug.org/content/using-multiple-instance-tasks) - Practical examples
- [YAUG: Multiple Instance Atomic Task and Composite Task](https://www.yaug.org/index.php/node/56) - Task type differences
- [YAUG: Cancellation Region](https://www.yaug.org/content/cancellation-region) - Region configuration

---

## 14. Conclusion

**Java YAWL's MI Infrastructure** is built on:

1. **Hierarchical Identifiers**: Elegant parent-child tracking via dotted notation
2. **Service-Oriented Execution**: No built-in thread pool, delegates to Custom Services
3. **Threshold-Based Synchronization**: Flexible completion conditions (min/max/threshold)
4. **Petri Net Semantics**: Internal conditions provide formal synchronization guarantees
5. **XPath/XQuery Data Handling**: Powerful input distribution and output aggregation

**For @unrdf/daemon**, this means:

- ✅ **Implement hierarchical identifiers** - Critical for tracking instances
- ✅ **Build Interface B API** - Foundation for service communication
- ✅ **Start with built-in worker pool** - Evolve to microservices later
- ✅ **Use Petri net places for synchronization** - Proven formal model
- ✅ **Support JSONPath for data** - Modern alternative to XPath
- ✅ **OTEL logging from day one** - Parent-child relationships, MI metrics

**Next Steps**:
1. Implement `YAWLIdentifier` and `WorkItemRepository` in `@unrdf/daemon/core`
2. Define Interface B API in `@unrdf/daemon/interfaces`
3. Build `MITaskExecutor` with threshold synchronization
4. Create reference Custom Service with worker pool
5. Add comprehensive MI tests (static, dynamic, nested, cancellation)

**Runtime complexity validated** ✅ Ready to build.
