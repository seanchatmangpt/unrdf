# Java YAWL Engine Architecture

> **Research Focus**: Infrastructure-level architecture of the Java YAWL reference implementation
> **Version**: Based on YAWL 4.3/5.0 analysis
> **Date**: 2026-01-11

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Core Engine Services](#core-engine-services)
3. [Concurrency & Thread Model](#concurrency--thread-model)
4. [Work Item Lifecycle](#work-item-lifecycle)
5. [Timer & Timeout Infrastructure](#timer--timeout-infrastructure)
6. [Persistence Architecture](#persistence-architecture)
7. [Event Notification System](#event-notification-system)
8. [Multi-Instance Implementation](#multi-instance-implementation)
9. [Service Interfaces](#service-interfaces)
10. [Runtime Initialization Sequence](#runtime-initialization-sequence)
11. [Dependency Graph](#dependency-graph)

---

## Executive Summary

The Java YAWL Engine is a **service-oriented workflow execution engine** built on a **singleton pattern** with **coarse-grained synchronization**. It uses:

- **Singleton YEngine** as the central control point
- **ConcurrentHashMap** for thread-safe collections
- **Method-level synchronization** with persistence manager as lock
- **Hibernate ORM** for state persistence
- **Observer pattern** for event notifications via Interface B
- **Deep cloning** of specifications for case isolation

**Key Insight**: YAWL favors **simplicity over fine-grained concurrency** - serializing major operations through a single lock (`_pmgr`) rather than complex lock hierarchies.

---

## Core Engine Services

### 1. YEngine (Singleton)

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- Stores all active process specifications in object format
- Single control point for all case instance operations
- Manages service registration and external client authentication
- Delegates execution to YNetRunner instances
- Aggregates and correlates YNetRunner with YIdentifier

**Initialization**:
```java
public static YEngine getInstance(boolean persisting,
    boolean gatherHbnStats, boolean redundantMode)
    throws YPersistenceException
```

**Key Data Structures**:
- `YWorkItemRepository _workItemRepository` - Work item cache
- `YNetRunnerRepository _netRunnerRepository` - Active case runners
- `YSpecificationTable _specifications` - Loaded specifications
- `ConcurrentHashMap _yawlServices` - Registered YAWL services
- `ConcurrentHashMap _externalClients` - User credentials
- `Map _runningCaseIDToSpecMap` - Case-to-specification mapping

**Major Operations**:
- `launchCase()` - Creates YNetRunner for specification root net
- `startWorkItem()` - Transitions enabled → executing
- `completeWorkItem()` - Processes output data, triggers continuation
- `suspendCase()` / `resumeCase()` - Case lifecycle control
- `cancelCase()` - Terminates case execution
- `addYawlService()` - Registers custom services

### 2. YNetRunner

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- Executes a single YNet instance (deep-cloned from specification)
- Implements "kick-and-continue" execution pattern
- Manages task enabling, firing, and completion
- Handles deferred choice through YEnabledTransitionSet

**Execution Model**:
```java
public synchronized void kick() {
    if (!continueIfPossible(pmgr)) {
        announceCaseCompletion();
    }
}
```

**Key Methods**:
- `kick()` - Primary execution driver
- `continueIfPossible(YPersistenceManager)` - Attempts net progression
- `completeWorkItemInTask()` - Processes completed work items
- `withdrawEnabledTask()` - Removes unselected tasks (deferred choice)

**Synchronization**: Method-level synchronized to prevent race conditions during execution flow.

### 3. YWorkItemRepository

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- In-memory cache of active work items
- Thread-safe storage and retrieval
- State-based queries (enabled, executing, fired, completed)
- Case-level and service-level filtering

**Storage**:
```java
private final ConcurrentHashMap<String, YWorkItem> _itemMap;
// Key format: "caseID:taskID"
// Initial capacity: 500 items
```

**Core Operations**:
- `add(YWorkItem)` - Adds to repository
- `remove(YWorkItem)` - Removes from repository
- `get(String caseID, String taskID)` - Retrieves by identifiers
- `getEnabledWorkItems()` - Filters by enabled status
- `getExecutingWorkItems(String serviceURI)` - Filters by service + status
- `removeWorkItemsForCase(String caseID)` - Bulk case removal
- `removeWorkItemFamily(YWorkItem)` - Removes parent + children
- `cleanseRepository()` - Synchronizes with active engine state

### 4. YWorkItem

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- Represents individual work item instances
- Tracks status transitions (enabled → executing → completed)
- Contains task definition data, case identifiers, and output data

**Status Enumeration** (`YWorkItemStatus`):
- `Enabled` - Ready for execution
- `Fired` - Atomic task created, not yet started
- `Executing` - Currently being processed
- `Completed` - Finished execution
- `Suspended` - Paused (case-level)
- `Deadlocked` - Unable to progress

### 5. YPersistenceManager

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- Wraps Hibernate ORM for database operations
- Provides transaction management
- Serves as synchronization lock for YEngine
- Handles case numbering via YCaseNbrStore

**Integration with Hibernate**:
- Uses **HibernateEngine** singleton for database access
- **Hyperjaxb3 framework** bridges XML (YAWL native) ↔ Java entities
- Enables YAWL to work with XML while persisting to relational DB

**Transaction Pattern**:
```java
synchronized(_pmgr) {
    _pmgr.startTransaction();
    try {
        // Operation (case launch, work item completion, etc.)
        _pmgr.commit();
    } catch (Exception e) {
        _pmgr.rollback();
    }
}
```

### 6. YEngineRestorer

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- Restores engine state from persisted data on startup
- Sequentially reconstructs services, specifications, and case instances
- Ensures consistency between database and in-memory state

### 7. YAnnouncer & ObserverGateway

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- Implements observer pattern for event notifications
- Announces work item and case lifecycle events
- Routes events to registered Interface B services

**Event Types**:
- Work item enabled
- Work item cancelled
- Case started
- Case completed
- Engine initialization complete

---

## Concurrency & Thread Model

### Synchronization Strategy: Coarse-Grained Locking

YAWL uses **method-level synchronization** and a **single global lock** (`_pmgr`) rather than fine-grained locking:

```java
// YEngine: Coarse-grained synchronization
synchronized(_pmgr) {
    startTransaction();
    try {
        // Major operation (case launch, completion, etc.)
        commitTransaction();
    } finally {
        // Cleanup
    }
}

// YNetRunner: Method-level synchronization
public synchronized void kick() { }
public synchronized boolean continueIfPossible(YPersistenceManager pmgr) { }
public synchronized boolean completeWorkItemInTask(...) { }
```

### Thread-Safe Collections

YAWL uses `ConcurrentHashMap` for collections accessed by multiple threads:

- `_yawlServices` (YEngine) - Service registry
- `_externalClients` (YEngine) - Client credentials
- `_itemMap` (YWorkItemRepository) - Work item storage

These provide **lock-free reads** and **fine-grained write locks** internally.

### No Explicit Thread Pools in Core Engine

**Critical Finding**: The core YAWL engine does **NOT** use explicit thread pools for work item execution. Instead:

1. **Work items are delegated** to external Custom Services via Interface B
2. **Custom Services** are responsible for threading/concurrency in their execution
3. **Engine serializes** major operations through `_pmgr` lock

**Threading Model**:
- **Single-threaded** for major engine operations (case launch, completion)
- **Multi-threaded** via web container (Tomcat) handling HTTP requests
- **Concurrent** access to collections via ConcurrentHashMap

### Timer Threading

The `YTimer` class (in `org.yawlfoundation.yawl.engine.time` package) uses:
- **java.util.Timer** internally (single background thread)
- Schedules timeout tasks for work items
- Callbacks execute on Timer's thread

---

## Work Item Lifecycle

### State Transitions

```
┌─────────────────────────────────────────────────────────────┐
│                  WORK ITEM LIFECYCLE                         │
└─────────────────────────────────────────────────────────────┘

  Specification      ┌──────────┐
  Loaded into    ───>│ Template │
  YEngine            └──────────┘
                          │
                          │ Case Launch
                          ▼
                    ┌──────────┐
                    │ Enabled  │◄────┐
                    └──────────┘     │
                          │          │
                          │ Fire     │ Deferred Choice
                          ▼          │ Withdrawal
  Atomic Task ────> ┌──────────┐    │
                    │  Fired   │────┘
                    └──────────┘
                          │
                          │ Check-out (Interface B)
                          ▼
                    ┌──────────┐
                    │Executing │
                    └──────────┘
                          │
                          │ Check-in (Interface B)
                          ▼
                    ┌──────────┐
                    │Completed │
                    └──────────┘
                          │
                          │ Net Continuation
                          ▼
                    [Next Task Enabled or Case Complete]

  Suspended State (case-level) ─> All work items in case
  Deadlocked State ────────────> Unable to progress
  Cancelled ───────────────────> Work item terminated
```

### Lifecycle Operations

#### 1. Enablement

- **Trigger**: Task preconditions satisfied in YNet
- **Action**: YNetRunner creates YWorkItem, sets status = `Enabled`
- **Repository**: Added to YWorkItemRepository
- **Notification**: Engine announces via Interface B to registered services

#### 2. Firing (Atomic Tasks)

- **Trigger**: Engine calls `startWorkItem()` for atomic task
- **Action**: Status transitions to `Fired`, work item prepared for delegation
- **Template**: Task definition data copied to work item

#### 3. Execution

- **Trigger**: Custom Service checks out work item (Interface B)
- **Action**: Status = `Executing`, service URI recorded
- **Responsibility**: Custom Service executes task logic

#### 4. Completion

- **Trigger**: Custom Service checks in work item with output data
- **Action**:
  - Engine calls `completeWorkItemInTask()`
  - Output data merged into case data
  - Work item removed from repository
  - YNetRunner calls `continueIfPossible()`
  - Next tasks enabled if preconditions met

#### 5. Withdrawal (Deferred Choice)

- **Trigger**: Alternative path selected in deferred choice
- **Action**:
  - `withdrawEnabledTask()` called
  - Work item removed from repository
  - Cancellation announced via Interface B

### Repository Management

**Adding to Repository**:
```java
YWorkItem workItem = new YWorkItem(pmgr, atomicTask.getNet().getSpecification(),
    atomicTask, caseID, enablementData);
_workItemRepository.add(workItem);
```

**Retrieving from Repository**:
```java
// By case and task
YWorkItem item = _workItemRepository.get(caseID, taskID);

// By status
Set<YWorkItem> enabled = _workItemRepository.getEnabledWorkItems();
Set<YWorkItem> executing = _workItemRepository.getExecutingWorkItems(serviceURI);
```

**Removing from Repository**:
```java
_workItemRepository.remove(workItem);

// Or bulk removal
_workItemRepository.removeWorkItemsForCase(caseID);
```

### Parent-Child Hierarchies

Composite tasks create **parent work items** with **child work items** for sub-net instances:

- `getParentWorkItems()` - Returns top-level items
- `getChildrenOf(YWorkItem parent)` - Returns children
- `removeWorkItemFamily(YWorkItem parent)` - Cascades removal

---

## Timer & Timeout Infrastructure

### YTimer Class

**Package**: `org.yawlfoundation.yawl.engine.time`

**JavaDoc**: Version 2.3 (later versions likely similar)

**Responsibilities**:
- Schedules timeout tasks for work items
- Cancels timers when work items complete
- Handles case-level and task-level timer cancellation

**Key Methods**:
- `schedule()` - Schedules a timer task
- `cancelAll()` - Cancels all active timers
- `cancelTimersForCase(String caseID)` - Cancels timers for specific case
- `cancelTimerTask()` - Cancels individual timer

### Timer Configuration

Timers are configured in the YAWL Editor:

- **Property**: "Timer" entry in task properties
- **Format**: ISO 8601 durations (e.g., `PT2M` for 2 minutes)
- **Trigger**: After task enablement or at absolute time
- **Action**: Configurable (e.g., escalate, cancel, notify)

### Implementation Details

- **Backend**: `java.util.Timer` (single background thread)
- **Task Execution**: Callbacks fire on Timer thread
- **Expired Timers**: Can trigger task cancellation or escalation
- **Delayed Case Launch**: Supports scheduled case starts

### Known Issues

- **YAWL 2.3.5**: Timer cancellation bug with extended attributes
- **Dynamic MI**: Timer interaction with dynamic multi-instance tasks

---

## Persistence Architecture

### Hibernate Integration

YAWL uses **Hibernate ORM** for database persistence:

```
┌─────────────────────────────────────────────────────────────┐
│                  PERSISTENCE STACK                           │
├─────────────────────────────────────────────────────────────┤
│ YAWL Engine (XML-based data)                                │
├─────────────────────────────────────────────────────────────┤
│ Hyperjaxb3 Framework (XML ↔ Java Entity mapping)            │
├─────────────────────────────────────────────────────────────┤
│ Hibernate ORM (Java Entities ↔ SQL)                         │
├─────────────────────────────────────────────────────────────┤
│ JDBC (Database access)                                       │
├─────────────────────────────────────────────────────────────┤
│ Relational Database (PostgreSQL, MySQL, etc.)               │
└─────────────────────────────────────────────────────────────┘
```

### HibernateEngine Singleton

**Package**: `org.yawlfoundation.yawl.elements.data.external`

**Responsibilities**:
- Provides basic database support methods via Hibernate
- Manages Hibernate SessionFactory
- Handles entity CRUD operations

### Persister Class

**Package**: `org.yawlfoundation.yawl.resourcing.datastore.persistence`

**Responsibilities**:
- Thin client of HibernateEngine
- Implements organizational data CRUD
- Abstracts persistence from business logic

### YPersistenceManager

**Responsibilities**:
- Transaction management (start, commit, rollback)
- Case numbering via YCaseNbrStore
- Synchronization lock for engine operations
- Persistence flag control (`isPersisting()`)

### Persisted Entities

- **YSpecification** - Workflow specifications
- **YAWLServiceReference** - Registered services
- **YExternalClient** - User credentials
- **Case data** - Running case instances (via YEngineRestorer)
- **Event logs** - YEventLogger writes to database

### Restoration on Startup

**YEngineRestorer** sequentially reconstructs:

1. **Services** - Registered YAWL services
2. **Specifications** - Loaded workflow definitions
3. **Case Instances** - Deep-clone YNet, create YNetRunner, restore state

---

## Event Notification System

### Observer Pattern Implementation

YAWL uses **Interface B** and **Observer Gateways** for event notifications:

```
┌─────────────────────────────────────────────────────────────┐
│                  EVENT NOTIFICATION FLOW                     │
└─────────────────────────────────────────────────────────────┘

  YEngine                InterfaceB           Custom Service
     │                       │                       │
     │ Work Item Enabled     │                       │
     ├──────────────────────>│ HTTP POST             │
     │                       ├──────────────────────>│
     │                       │  (handleEnabledWorkItem)
     │                       │                       │
     │                       │ Check-out             │
     │<──────────────────────┼───────────────────────┤
     │                       │                       │
     │ Status: Executing     │                       │
     │                       │                       │
     │                       │ [Service processes]   │
     │                       │                       │
     │                       │ Check-in + data       │
     │<──────────────────────┼───────────────────────┤
     │                       │                       │
     │ Complete Work Item    │                       │
     │ Continue Net          │                       │
```

### Interface B Event Framework

**Rebuilt in YAWL 2.2**: Much faster with no errors under heavy load.

**InterfaceBWebsideController**:
- Abstract class for custom services
- Provides access to Engine via Interface B
- Receives event notifications from Engine

**Event Types**:

1. **Work Item Events**:
   - `handleEnabledWorkItemEvent()` - Task ready for execution
   - `handleCancelledWorkItemEvent()` - Task cancelled
   - `handleFiredWorkItemEvent()` - Atomic task created

2. **Case Events**:
   - Case started notification
   - Case completed notification

3. **Engine Events**:
   - `engineInitialisationCompleted()` - Engine startup complete

### Observer Gateway Registration

Custom services register with the engine:

1. **Context Parameter**: `InterfaceBWebsideController` = fully qualified class name
2. **Backend URL**: `InterfaceB Backend` = Engine's Interface B API URL
3. **Startup**: Service registers on initialization
4. **Callbacks**: Engine sends HTTP POST to registered services

### YAnnouncer

**Package**: `org.yawlfoundation.yawl.engine`

**Responsibilities**:
- Routes events to ObserverGateway
- Manages event announcement queue
- Decouples event generation from notification

---

## Multi-Instance Implementation

### Multi-Instance Parameters

YAWL directly supports MI patterns with **four parameters**:

1. **Minimum Instances** (lower bound)
2. **Maximum Instances** (upper bound)
3. **Threshold** (N-out-of-M join)
4. **Static/Dynamic** (instance creation mode)

### Static vs. Dynamic

- **Static**: Number of instances **fixed** after creation
- **Dynamic**: Additional instances can be added **during execution**

### Runtime Implementation

**Infrastructure View**:

#### Static MI (Fully Implemented)

1. **Creation Phase**:
   - YNetRunner calculates instance count from input data
   - Creates N child YWorkItem instances
   - Adds all to YWorkItemRepository with parent-child links

2. **Execution Phase**:
   - Each child work item follows standard lifecycle
   - Parallel or sequential execution based on pattern

3. **Completion Phase**:
   - Threshold logic in YNetRunner checks completed count
   - When threshold met, parent task completes
   - Remaining instances cancelled if configured

#### Dynamic MI (Partially Implemented)

**Known Limitation**: "There is no facility (and never has been) in the engine or worklist for creating a new instance of a dynamic multi-instance composite task (i.e. a sub-net instance) at runtime."

**YAWL 5.0 Issue**: "Add a new instance" option does not work in worklist for dynamic MI tasks.

**Theoretical Design** (not fully implemented):
- Interface for runtime instance creation
- Repository extension for dynamic addition
- Threshold recalculation on instance addition

### Multi-Instance Data Patterns

**Multiple Instance Data** (WDP-4):
- Each instance has isolated input data
- Output data merged based on aggregation rules
- Repository tracks data per child work item

---

## Service Interfaces

### Interface A: Designer & Manager

**Purpose**: Interactions between YAWL Designer/Manager and Engine

**Default URL**: `http://localhost:8080/yawl/ia`

**WSDL**: Yes (XML-based interface)

**Operations**:
- Upload specifications
- Launch cases
- Cancel cases
- Suspend/resume cases
- Query case status
- Retrieve work item data

**Clients**:
- YAWL Editor
- YAWL Manager
- Custom management tools

### Interface B: Custom Services

**Purpose**: Interactions between Custom Services and Engine

**WSDL**: Yes (XML-based interface)

**Operations**:

**Service → Engine**:
- `checkOut(workItemID)` - Take responsibility for work item
- `checkIn(workItemID, outputData)` - Complete work item with data

**Engine → Service** (callbacks):
- `handleEnabledWorkItemEvent(workItem)` - Notification of enabled task
- `handleCancelledWorkItemEvent(workItemID)` - Task cancelled
- `handleFiredWorkItemEvent(workItem)` - Atomic task created
- `engineInitialisationCompleted()` - Engine ready

**InterfaceBWebsideController**:
- Abstract class extended by custom services
- Provides access to Interface B API
- Receives event notifications

### Interface E: Event Logging

**Purpose**: Access to execution logs and monitoring

**Capabilities**:
- Download logs in XES format (since YAWL 2.3)
- Query event logs
- Monitor case progress

**YEventLogger**:
- Logs case, subnet, workitem, and workitem data events
- Writes to database via Hibernate
- Instantiates log objects as rows in tables

**EventLogger**:
- Logs resource 'events'
- Records specification keys and event types

**YLogGatewayClient**:
- Interface for detailed log access
- Custom logs defined in editor

### Interface X: Exception Handling

**Purpose**: Interface between Engine and Exception Service

**Package**: `org.yawlfoundation.yawl.engine.interfce.interfaceX`

**InterfaceX_Service**:
- Defines exception event methods
- Passed from engine to exception service
- Manages process-level exceptions

**WSDL**: Yes

---

## Runtime Initialization Sequence

### 1. Container Startup (Tomcat)

```
┌─────────────────────────────────────────────────────────────┐
│         YAWL ENGINE INITIALIZATION SEQUENCE                  │
└─────────────────────────────────────────────────────────────┘

  [Tomcat Starts]
       │
       │ Web apps load alphabetically
       ▼
  [Custom Services Load First]
       │ (e.g., resourceService.war, schedulingService.war)
       │
       ├─> Read context parameters:
       │   - InterfaceBWebsideController (class name)
       │   - InterfaceB Backend (Engine URL)
       │
       ├─> Initialize service (partial)
       │   - Cannot fully init without engine
       │   - Register pending
       │
       ▼
  [yawl.war Loads]
       │
       ├─> YEngine.getInstance(persisting, stats, redundant)
       │       │
       │       ├─> Initialize repositories
       │       │   - YWorkItemRepository
       │       │   - YNetRunnerRepository
       │       │   - YSpecificationTable
       │       │
       │       ├─> Initialize persistence
       │       │   - YPersistenceManager
       │       │   - HibernateEngine.getInstance()
       │       │
       │       ├─> Restore state (if persisting)
       │       │   - YEngineRestorer.restore()
       │       │       - Load services
       │       │       - Load specifications
       │       │       - Restore cases
       │       │
       │       └─> Mark engine as running
       │
       ├─> Announce engine initialization complete
       │       │
       │       └─> HTTP POST to all registered services
       │           - engineInitialisationCompleted() callback
       │
       ▼
  [Custom Services Complete Init]
       │
       ├─> Receive engineInitialisationCompleted()
       │
       ├─> Final initialization tasks
       │   - Register with engine
       │   - Load service-specific data
       │   - Subscribe to event types
       │
       └─> Ready to receive work items

  [Engine Ready for Case Launches]
```

### 2. YEngine Initialization Details

**Singleton Creation**:
```java
YEngine.getInstance(boolean persisting, boolean gatherHbnStats, boolean redundantMode)
```

**Initialization Steps**:

1. **Create Repositories**:
   ```java
   _workItemRepository = new YWorkItemRepository();
   _netRunnerRepository = new YNetRunnerRepository();
   _specifications = new YSpecificationTable();
   ```

2. **Initialize Persistence**:
   ```java
   _pmgr = new YPersistenceManager(persisting);
   if (persisting) {
       HibernateEngine.getInstance(gatherHbnStats);
   }
   ```

3. **Restore State** (if persisting):
   ```java
   if (!_restoring && isPersisting()) {
       YEngineRestorer.restore(_pmgr);
   }
   ```

4. **Mark Running**:
   ```java
   _engineInitialisationCompleted = true;
   ```

5. **Announce to Services**:
   ```java
   for (YAWLServiceReference service : _yawlServices.values()) {
       service.announceEngineInitialised();
   }
   ```

### 3. Service Registration

**Context Parameters** (in service's web.xml):
```xml
<context-param>
    <param-name>InterfaceBWebsideController</param-name>
    <param-value>org.example.MyService</param-value>
</context-param>
<context-param>
    <param-name>InterfaceB Backend</param-name>
    <param-value>http://localhost:8080/yawl/ib</param-value>
</context-param>
```

**Registration Flow**:
1. Service reads context parameters
2. Service creates instance of InterfaceBWebsideController subclass
3. Service connects to Engine's Interface B URL
4. Engine adds service to `_yawlServices` map
5. Engine persists service (if persisting enabled)

### 4. Case Launch Sequence

**Trigger**: External client calls `launchCase()` via Interface A

**Execution**:
```java
synchronized(_pmgr) {
    _pmgr.startTransaction();
    try {
        // 1. Deep-clone specification's root net
        YNet netCopy = specification.getRootNet().clone();

        // 2. Create YNetRunner for the copy
        YNetRunner runner = new YNetRunner(_pmgr, netCopy, data, caseID);

        // 3. Add to repository
        _netRunnerRepository.addRunner(caseID, runner);

        // 4. Map case to specification
        _runningCaseIDToSpecMap.put(caseID, specID);

        // 5. Start execution
        runner.kick();

        _pmgr.commit();
    } catch (Exception e) {
        _pmgr.rollback();
        throw e;
    }
}
```

**Result**: YNetRunner begins execution, enabling first tasks.

---

## Dependency Graph

### Component Dependencies

```
┌─────────────────────────────────────────────────────────────┐
│              YAWL ENGINE COMPONENT DEPENDENCIES              │
└─────────────────────────────────────────────────────────────┘

                          ┌──────────────┐
                          │   YEngine    │
                          │  (Singleton) │
                          └──────┬───────┘
                                 │
                 ┌───────────────┼───────────────┐
                 │               │               │
        ┌────────▼─────┐  ┌──────▼──────┐  ┌────▼──────────┐
        │ YNetRunner   │  │YWorkItem    │  │YPersistence   │
        │  Repository  │  │ Repository  │  │   Manager     │
        └────────┬─────┘  └──────┬──────┘  └────┬──────────┘
                 │               │               │
                 │         ┌─────▼─────┐         │
                 │         │ YWorkItem │         │
                 │         └─────┬─────┘         │
                 │               │               │
        ┌────────▼─────┐   ┌─────▼──────┐  ┌────▼──────────┐
        │  YNetRunner  │   │ YWorkItem  │  │ Hibernate     │
        │              │   │   Status   │  │   Engine      │
        └────────┬─────┘   └────────────┘  └────┬──────────┘
                 │                              │
           ┌─────▼─────┐                  ┌─────▼──────┐
           │   YNet    │                  │  Database  │
           │ (cloned)  │                  └────────────┘
           └─────┬─────┘
                 │
        ┌────────┼────────┐
        │        │        │
   ┌────▼───┐ ┌─▼────┐ ┌─▼────────┐
   │ YTask  │ │YCond.│ │YIdentifier│
   └────────┘ └──────┘ └──────────┘


External Dependencies:

┌──────────────┐
│   YEngine    │
└──────┬───────┘
       │
       ├─> YAnnouncer ──> ObserverGateway ──> Interface B
       │                                          │
       │                                    ┌─────▼──────┐
       │                                    │  Custom    │
       │                                    │  Services  │
       │                                    └────────────┘
       │
       ├─> YTimer ──> java.util.Timer
       │
       ├─> YEventLogger ──> Database (via Hibernate)
       │
       └─> YSpecificationTable ──> YSpecification
```

### Library Dependencies

**Core Libraries**:
- **Hibernate ORM** - Database persistence
- **Hyperjaxb3** - XML ↔ Java entity mapping
- **Java Util Timer** - Timer scheduling
- **Servlet API** - Web container integration (Tomcat)

**Optional Libraries**:
- **PostgreSQL/MySQL JDBC** - Database drivers
- **Log4j/SLF4J** - Logging (inferred from Java conventions)

### Service Dependencies

**Circular Dependency Prevention**:
- Engine loads FIRST (yawl.war)
- Services load SECOND (alphabetically before)
- Services use **engineInitialisationCompleted()** callback for final init
- Avoids deadlock via event-driven completion

---

## Key Architectural Insights

### 1. Simplicity Over Complexity

YAWL favors **coarse-grained locking** (`synchronized(_pmgr)`) over fine-grained concurrency. This:
- **Simplifies** reasoning about state
- **Avoids** deadlock complexity
- **Serializes** major operations (acceptable for typical workflow loads)

### 2. Deep Cloning for Isolation

Each case gets a **deep-cloned YNet** instance:
- **Isolates** case state from specification
- **Enables** parallel case execution without interference
- **Trades** memory for concurrency safety

### 3. Observer Pattern for Extensibility

**Interface B + Observer Gateways** enable:
- **Pluggable** custom services
- **Loose coupling** between engine and execution
- **Extensibility** without modifying engine

### 4. Hibernate for Flexibility

Using **Hibernate ORM + Hyperjaxb3**:
- **Bridges** XML-based workflow definitions ↔ relational DB
- **Enables** portability across databases
- **Simplifies** persistence logic

### 5. Repository Pattern for Caching

**YWorkItemRepository** and **YNetRunnerRepository**:
- **In-memory cache** for active entities
- **Fast access** without database queries
- **Synchronized** with persistence on commits

### 6. Singleton Engine for Control

**YEngine singleton**:
- **Single control point** for all operations
- **Simplified** service discovery
- **Centralized** state management

### 7. Delegation to Custom Services

Engine **delegates execution** rather than executing tasks:
- **Separation of concerns**: scheduling vs. execution
- **Scalability**: services can run on separate servers
- **Flexibility**: services can be written in any language (HTTP/XML)

---

## References & Sources

### Official Documentation
- [YAWL Technical Manual Version 5.0 (PDF)](https://yawlfoundation.github.io/assets/files/YAWLTechnicalManual5.0.pdf)
- [YAWL Technical Manual Version 4.3 (PDF)](https://yawlfoundation.github.io/assets/files/YAWLTechnicalManual4.3.pdf)
- [YAWL Foundation Website](https://yawlfoundation.github.io/)

### Source Code
- [GitHub - yawlfoundation/yawl](https://github.com/yawlfoundation/yawl)
- [YEngine.java](https://github.com/yawlfoundation/yawl/blob/master/src/org/yawlfoundation/yawl/engine/YEngine.java)
- [YNetRunner.java](https://github.com/yawlfoundation/yawl/blob/master/src/org/yawlfoundation/yawl/engine/YNetRunner.java)
- [YWorkItemRepository.java](https://github.com/yawlfoundation/yawl/blob/master/src/org/yawlfoundation/yawl/engine/YWorkItemRepository.java)

### JavaDoc References
- [YWorkItemRepository JavaDoc (Version 4.3)](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/engine/YWorkItemRepository.html)
- [InterfaceBWebsideController JavaDoc](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/engine/interfce/interfaceB/InterfaceBWebsideController.html)
- [YEventLogger JavaDoc](https://yawlfoundation.github.io/javadoc/org/yawlfoundation/yawl/logging/YEventLogger.html)
- [YTimer JavaDoc (Version 2.3)](http://www.yawlfoundation.org/javadoc/yawl/org/yawlfoundation/yawl/engine/time/YTimer.html)

### Academic Papers
- [Design and Implementation of the YAWL System | SpringerLink](https://link.springer.com/chapter/10.1007/978-3-540-25975-6_12)
- [YAWL: Yet Another Workflow Language (Revised version)](https://yawlfoundation.github.io/assets/files/yawlrevtech.pdf)

### Community Resources
- [YAWL User Group - Timer Discussion](https://www.yaug.org/content/using-timer-timeout-manual-tasks-0)
- [YAWL User Group - Persistence with Hyperjaxb3](https://www.yaug.org/content/persistence-hyperjaxb3-framework)
- [GitHub Issues - Multi-Instance](https://github.com/yawlfoundation/yawl/issues/323)

### Wikipedia & Encyclopedia
- [YAWL - Wikipedia](https://en.wikipedia.org/wiki/YAWL)
- [YAWL - HandWiki](https://handwiki.org/wiki/YAWL)

---

## Conclusion

The Java YAWL Engine is a **mature, production-grade workflow engine** with a **service-oriented architecture** emphasizing:

- **Simplicity** - Coarse-grained locking, singleton pattern
- **Extensibility** - Observer pattern, Interface B
- **Persistence** - Hibernate ORM with Hyperjaxb3
- **Isolation** - Deep cloning for case instances
- **Delegation** - Custom Services handle execution

**For JavaScript YAWL Implementation**: Consider adapting:
- **Event-driven** model (Node.js EventEmitter vs. Observer Gateways)
- **Async/await** for continuation (vs. synchronized methods)
- **In-process execution** (vs. delegated HTTP services)
- **Lighter persistence** (JSON/SQLite vs. Hibernate)
- **Promise-based** work item lifecycle (vs. callback-based Interface B)

The core architectural patterns (singleton engine, repository caching, observer notifications) remain valuable regardless of implementation language.
