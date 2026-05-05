# YAWL Resource Allocation Mechanics Research

**Author**: Research Agent (Adversarial PM Mode)
**Date**: 2026-01-11
**Target**: UNRDF YAWL Resource Allocation System
**Source**: JavaScript/ESM implementation at `/packages/yawl/src/resources/`
**Reference**: Java YAWL 4.x YResourceService

---

## Executive Summary

The UNRDF YAWL implementation provides a **modern, RDF-native resource allocation system** with cryptographic receipt proofs and SPARQL-based eligibility rules. However, it is **engine-centric rather than worklist-centric**, prioritizing automated workflows over human task management.

**Key Innovation**: Receipt-based allocation with capacity tracking in RDF triple store, enabling time-travel and cryptographic audit trails.

**Critical Limitation**: No true worklist service (Interface B). Work items bypass offer/allocate phases required for human resource interaction patterns.

---

## 1. Resource Data Model

### 1.1 Core Resource Types

```javascript
// ResourceType enum (yawl-resources-types.mjs:68-72)
export const ResourceType = {
  PARTICIPANT: 'Participant',  // Human users
  TOOL: 'Tool',                // Services/systems
  ROLE: 'Role',                // Groups/teams
};
```

### 1.2 Resource Schema

```javascript
// Zod validation schema (yawl-resources-types.mjs:81-88)
export const ResourceSchema = z.object({
  id: z.string().min(1),              // Unique identifier
  type: z.enum([PARTICIPANT, TOOL, ROLE]),
  name: z.string().optional(),        // Display name
  capacity: z.number().int().min(-1).default(1),  // -1 = unlimited
  sparql: z.string().optional(),      // Eligibility query
  metadata: z.record(z.unknown()).optional(),
});
```

**Capacity Semantics**:
- `-1`: Unlimited concurrent allocations (typical for Tools)
- `0`: No allocations allowed (disabled resource)
- `1`: Single concurrent allocation (typical for Participants)
- `N > 1`: N concurrent allocations

### 1.3 Policy Pack Structure

```javascript
// Policy pack groups resources with priority (yawl-resources-types.mjs:105-112)
export const PolicyPackSchema = z.object({
  id: z.string().min(1),
  name: z.string().optional(),
  version: z.string().optional(),
  resources: z.array(ResourceSchema),  // Resource definitions
  priority: z.number().int().min(0).default(0),  // Higher = preferred
  enabled: z.boolean().default(true),
});
```

**Priority-Based Selection**: Multiple policy packs can be registered. Resources from higher-priority packs are selected first when multiple resources are eligible.

### 1.4 RDF Representation

Resources are stored as RDF triples in an Oxigraph triple store:

```turtle
# Participant resource
yawl:resource/alice a yawl:Participant ;
  foaf:name "Alice Smith" ;
  yawl:capacity "2"^^xsd:integer ;
  yawl:eligibilitySparql "ASK { ... }" .

# Policy pack
yawl:policypack/approval-workflow a yawl:PolicyPack ;
  foaf:name "Approval Workflow" ;
  yawl:priority "10"^^xsd:integer ;
  yawl:enabled "true"^^xsd:boolean ;
  yawl:hasResource yawl:resource/alice .

# Allocation
yawl:allocation/alloc-001 a yawl:Allocation ;
  yawl:resource yawl:resource/alice ;
  yawl:workItem yawl:workitem/wi-001 ;
  yawl:allocatedAt "2024-01-15T09:00:00Z"^^xsd:dateTime ;
  yawl:status "active" .
```

### 1.5 Class Diagram

```
┌─────────────────────────────────────────────────────────┐
│ YawlResourceManager                                     │
├─────────────────────────────────────────────────────────┤
│ - store: OxigraphStore                                  │
│ - policyPacks: Map<string, PolicyPack>                  │
│ - resourcePools: Map<string, ResourcePool>              │
│ - allocationCounter: number                             │
├─────────────────────────────────────────────────────────┤
│ + registerPolicyPack(pack): void                        │
│ + allocateResource(wi, res): AllocationReceipt          │
│ + deallocateResource(allocId): boolean                  │
│ + getEligibleResources(task, case): Resource[]          │
│ + getCapacityStatus(resId): CapacityStatus              │
│ + createResourcePool(config): ResourcePool              │
└─────────────────────────────────────────────────────────┘
                    │
                    │ manages
                    ▼
┌─────────────────────────────────────────────────────────┐
│ PolicyPack                                              │
├─────────────────────────────────────────────────────────┤
│ + id: string                                            │
│ + name?: string                                         │
│ + resources: Resource[]                                 │
│ + priority: number                                      │
│ + enabled: boolean                                      │
└─────────────────────────────────────────────────────────┘
                    │
                    │ contains
                    ▼
┌─────────────────────────────────────────────────────────┐
│ Resource                                                │
├─────────────────────────────────────────────────────────┤
│ + id: string                                            │
│ + type: ResourceType                                    │
│ + name?: string                                         │
│ + capacity: number                                      │
│ + sparql?: string                                       │
│ + metadata?: Record                                     │
└─────────────────────────────────────────────────────────┘
                    △
                    │
        ┌───────────┼───────────┐
        │           │           │
┌───────────┐ ┌─────────┐ ┌──────────┐
│Participant│ │  Tool   │ │   Role   │
├───────────┤ ├─────────┤ ├──────────┤
│capacity: 1│ │capacity:│ │capacity: │
│           │ │   -1    │ │    -1    │
└───────────┘ └─────────┘ └──────────┘
```

---

## 2. Allocation Algorithm

### 2.1 Allocation Flow (Pseudocode)

```python
def performResourceAllocation(store, workItem, resource, options, policyPacks, state):
    """
    Allocate a resource to a work item with full validation and proof generation.

    Source: yawl-resources-allocation.mjs:78-127
    """

    # Step 1: Input Validation
    validatedWorkItem = WorkItemSchema.parse(workItem)
    validatedResource = ResourceSchema.parse(resource)

    # Step 2: Capacity Check
    capacityCheck = checkResourceCapacity(store, validatedResource)
    if not capacityCheck.allowed:
        raise CapacityExceededError(
            f"Capacity exceeded for {resource.id}: "
            f"{capacityCheck.current}/{capacityCheck.max}"
        )

    # Step 3: Eligibility Check (SPARQL-based)
    eligibilityCheck = await checkResourceEligibility(
        store,
        validatedResource,
        validatedWorkItem
    )
    if not eligibilityCheck.eligible:
        raise EligibilityError(
            f"Resource {resource.id} not eligible: {eligibilityCheck.reason}"
        )

    # Step 4: Policy Pack Matching (optional)
    policyPackId = options.policyPackId
    if not policyPackId:
        matchingPack = findMatchingPolicyPackForResource(policyPacks, resource)
        policyPackId = matchingPack?.id

    # Step 5: Create Allocation in RDF
    state.allocationCounter += 1
    allocationId = f"alloc-{Date.now()}-{state.allocationCounter}"
    createAllocationRDF(store, allocationId, workItem, resource, options.duration)

    # Step 6: Generate Receipt with Cryptographic Proof
    now = datetime.now()
    receipt = AllocationReceipt(
        id=allocationId,
        workItemId=workItem.id,
        resourceId=resource.id,
        resourceType=resource.type,
        allocatedAt=now.isoformat(),
        expiresAt=(now + duration).isoformat() if duration else None,
        proof={
            "capacityCheck": True,
            "eligibilityCheck": True,
            "policyPackId": policyPackId,
            "sparqlResult": eligibilityCheck.sparqlResult,
        }
    )

    # Step 7: Validate Receipt Schema
    AllocationReceiptSchema.parse(receipt)

    return receipt
```

### 2.2 Capacity Check Algorithm

```python
def checkResourceCapacity(store, resource):
    """
    Check if resource has available capacity for allocation.

    Source: yawl-resources-allocation.mjs:32-45
    """

    # Unlimited capacity (typical for Tools)
    if resource.capacity == -1:
        return {
            "allowed": True,
            "current": 0,
            "max": -1
        }

    # Query RDF store for active allocations
    resourceNode = namedNode(f"{YAWL_NS}resource/{resource.id}")
    activeAllocations = countActiveAllocations(store, resourceNode)

    return {
        "allowed": activeAllocations < resource.capacity,
        "current": activeAllocations,
        "max": resource.capacity
    }


def countActiveAllocations(store, resourceNode):
    """
    Count active (non-deallocated) allocations via SPARQL.

    Source: yawl-resources-rdf.mjs:138-164
    """

    query = f"""
        PREFIX yawl: <{YAWL_NS}>
        PREFIX rdf: <{RDF_NS}>

        SELECT (COUNT(?allocation) as ?count) WHERE {{
            ?allocation rdf:type yawl:Allocation ;
                        yawl:resource <{resourceNode.value}> .
            FILTER NOT EXISTS {{
                ?allocation yawl:status "deallocated" .
            }}
        }}
    """

    results = store.query(query)
    bindings = list(results)

    if bindings and bindings[0].get('count'):
        return int(bindings[0].get('count').value)

    return 0
```

### 2.3 Eligibility Check Algorithm

```python
async def checkResourceEligibility(store, resource, workItem):
    """
    Check resource eligibility via custom SPARQL query.

    Source: yawl-resources-eligibility.mjs:23-59
    """

    # No SPARQL query = automatically eligible
    if not resource.sparql:
        return {"eligible": True}

    try:
        # Replace placeholders with actual URIs
        sparqlQuery = resource.sparql
            .replace("?workItem", f"<{YAWL_NS}workitem/{workItem.id}>")
            .replace("?resource", f"<{YAWL_NS}resource/{resource.id}>")
            .replace("?case", f"<{YAWL_NS}case/{workItem.caseId}>")

        result = store.query(sparqlQuery)

        # ASK query returns boolean
        if isinstance(result, bool):
            return {
                "eligible": result,
                "sparqlResult": result,
                "reason": None if result else "SPARQL eligibility condition not met"
            }

        # SELECT query returns bindings
        bindings = list(result)
        eligible = len(bindings) > 0

        return {
            "eligible": eligible,
            "sparqlResult": eligible,
            "reason": None if eligible else "SPARQL query returned no results"
        }

    except Exception as error:
        return {
            "eligible": False,
            "sparqlResult": False,
            "reason": f"SPARQL check failed: {error.message}"
        }
```

### 2.4 Decision Logic: WHO Gets the Work Item?

```python
def selectResourceForWorkItem(manager, taskId, caseId, options={}):
    """
    Determine which resource should be allocated to a work item.

    This is the ENGINE's perspective (not a user worklist UI).
    """

    # Step 1: Get all policy packs sorted by priority
    policyPacks = manager.listPolicyPacks()  # Sorted high→low priority

    # Step 2: Find eligible resources across all packs
    eligibleResources = []

    for pack in policyPacks:
        if not pack.enabled:
            continue

        for resource in pack.resources:
            # Filter by resource type if specified
            if options.resourceType and resource.type != options.resourceType:
                continue

            # Check capacity
            capacityCheck = checkCapacity(resource)
            if not capacityCheck.allowed:
                continue

            # Check eligibility (SPARQL)
            mockWorkItem = {"id": f"wi-{taskId}", "taskId": taskId, "caseId": caseId}
            eligibility = await checkEligibility(resource, mockWorkItem)
            if not eligibility.eligible:
                continue

            # Check availability (calendar)
            if options.checkAvailability:
                availability = getAvailability(resource.id)
                if not availability.available:
                    continue

            # Add to eligible list with pack priority
            eligibleResources.append({
                **resource,
                "_policyPackId": pack.id,
                "_priority": pack.priority
            })

    # Step 3: Sort by priority (already sorted by pack iteration order)
    # Higher priority packs are checked first, so first match wins

    if not eligibleResources:
        return None

    # Step 4: Apply allocation strategy
    if options.allocationStrategy == "round-robin":
        return selectRoundRobin(eligibleResources, state.roundRobinIndex)
    elif options.allocationStrategy == "random":
        return selectRandom(eligibleResources)
    elif options.allocationStrategy == "shortest-queue":
        return selectShortestQueue(eligibleResources, manager)
    else:
        # Default: priority (first eligible resource)
        return eligibleResources[0]
```

**Key Decision Factors** (in order):
1. **Policy Pack Priority**: Higher priority packs are checked first
2. **Capacity**: Resource must have available capacity
3. **Eligibility**: Custom SPARQL query must pass
4. **Availability**: Calendar windows (if checking enabled)
5. **Allocation Strategy**: Round-robin, random, or priority-based selection

---

## 3. Worklist Management (Limitations)

### 3.1 Current Architecture (Engine-Centric)

```
┌─────────────────────────────────────────────────────────┐
│ YAWL Workflow Engine                                    │
│  - Evaluates control flow conditions                    │
│  - Enables tasks when conditions met                    │
│  - Creates work items in ENABLED state                  │
└─────────────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────────────┐
│ Resource Allocation (Post-hoc)                          │
│  - performResourceAllocation()                          │
│  - Direct allocation to specified resource              │
│  - No offer/accept pattern                              │
└─────────────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────────────┐
│ RDF Triple Store                                        │
│  - Stores allocation receipts                           │
│  - Tracks capacity utilization                          │
│  - Queryable via SPARQL                                 │
└─────────────────────────────────────────────────────────┘
```

**NO Worklist Service Layer**: Work items are not "offered" to resources. There is no per-user worklist view where a human can see pending tasks and choose one to work on.

### 3.2 YAWL Specification Architecture (Worklist-Centric)

```
┌─────────────────────────────────────────────────────────┐
│ YAWL Engine                                             │
│  - Publishes enabled work items to worklist service     │
└─────────────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────────────┐
│ Interface B: Worklist Service                           │
│  - Offers items to resource sets (roles/groups)         │
│  - Manages allocation (user "claims" item)              │
│  - Supports delegation, reallocation, piling            │
│  - Per-user worklist views                              │
└─────────────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────────────┐
│ User Worklists (Per-Resource Views)                     │
│  - Offered Items (can claim)                            │
│  - Allocated Items (assigned to me)                     │
│  - Started Items (currently working)                    │
└─────────────────────────────────────────────────────────┘
```

### 3.3 Missing Operations (Compared to Java YAWL Interface B)

| Operation | Status | UNRDF Equivalent | Gap |
|-----------|--------|------------------|-----|
| `getWorkItemsForResource(resourceId)` | ❌ Missing | Query RDF manually | No API |
| `offerItem(workItemId, resourceSet)` | ❌ Missing | - | No OFFERED state |
| `allocateItem(workItemId, resourceId)` | ⚠️ Partial | `allocateResource()` | Engine-driven only |
| `startItem(workItemId)` | ✓ Exists | `startTask()` | Full support |
| `suspendItem(workItemId)` | ❌ Missing | - | Transition exists, no API |
| `resumeItem(workItemId)` | ❌ Missing | - | - |
| `completeItem(workItemId, data)` | ✓ Exists | `completeTask()` | Full support |
| `delegateItem(workItemId, toResourceId)` | ❌ Missing | - | No delegation |
| `reallocateItem(workItemId, toResourceId)` | ❌ Missing | - | No reallocation |
| `pileItem(workItemId, pileId)` | ❌ Missing | - | No piling |
| `skipItem(workItemId)` | ❌ Missing | - | Must complete/cancel |
| `deallocateItem(workItemId)` | ⚠️ Partial | `deallocateResource()` | Internal only |

**Coverage Score**: 2.5/13 operations = **19.2%** Interface B compliance

### 3.4 Worklist Data Structure (Conceptual)

The UNRDF implementation does NOT maintain explicit worklist data structures. However, worklists COULD be queried from the RDF store:

```sparql
# Query: Get my allocated work items
PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?workItem ?taskId ?caseId ?allocatedAt WHERE {
  ?allocation rdf:type yawl:Allocation ;
              yawl:resource <http://yawlfoundation.org/yawlschema#resource/alice> ;
              yawl:workItem ?workItem ;
              yawl:allocatedAt ?allocatedAt .
  FILTER NOT EXISTS {
    ?allocation yawl:status "deallocated" .
  }
  ?workItem yawl:taskId ?taskId ;
            yawl:caseId ?caseId .
}
```

### 3.5 State Transitions (Simplified)

```
Work Item States (UNRDF):
┌─────────┐       ┌─────────┐       ┌────────┐       ┌───────────┐
│ PENDING ├──────→│ ENABLED ├──────→│ ACTIVE ├──────→│ COMPLETED │
└─────────┘       └────┬────┘       └───┬────┘       └───────────┘
                       │                 │
                       │                 │
                       │            ┌────▼─────┐
                       │            │SUSPENDED │
                       │            └────┬─────┘
                       │                 │
                       │                 │
                       └────────┬────────┘
                                │
                           ┌────▼─────┐
                           │CANCELLED │
                           └──────────┘

Allocation States:
┌────────┐                        ┌─────────────┐
│ active ├───────────────────────→│ deallocated │
└────────┘                        └─────────────┘
```

**Missing from YAWL Spec**:
- OFFERED state (work item offered to resource set)
- ALLOCATED state (work item allocated to specific resource before starting)

**Consequence**: Engine directly assigns work items to resources. No user choice or worklist interaction.

---

## 4. Resource Pools and Allocation Strategies

### 4.1 Resource Pool Class

```javascript
class ResourcePool {
  #manager;              // Parent YawlResourceManager
  #config;               // Pool configuration
  #roundRobinIndex = 0;  // State for round-robin

  async allocateAny(workItem, options) {
    const orderedResources = this.#getOrderedResources();

    for (const resource of orderedResources) {
      try {
        const receipt = await this.#manager.allocateResource(
          workItem,
          resource,
          options
        );
        return receipt;  // First successful allocation wins
      } catch {
        continue;  // Try next resource
      }
    }

    return null;  // No resource available
  }
}
```

### 4.2 Allocation Strategies

#### Round-Robin
```javascript
#getOrderedResources() {
  if (strategy === 'round-robin') {
    // Rotate starting point
    const rotated = [
      ...resources.slice(this.#roundRobinIndex),
      ...resources.slice(0, this.#roundRobinIndex)
    ];
    this.#roundRobinIndex = (this.#roundRobinIndex + 1) % resources.length;
    return rotated;
  }
}
```

**Example**:
- Pool: [Alice, Bob, Carol]
- Allocation 1: Try Alice → Bob → Carol (Alice succeeds)
- Allocation 2: Try Bob → Carol → Alice (Bob succeeds)
- Allocation 3: Try Carol → Alice → Bob (Carol succeeds)

#### Random
```javascript
if (strategy === 'random') {
  // Fisher-Yates shuffle
  for (let i = resources.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [resources[i], resources[j]] = [resources[j], resources[i]];
  }
  return resources;
}
```

#### Priority (Default)
```javascript
if (strategy === 'priority') {
  return resources;  // Use original order from policy pack
}
```

#### Shortest Queue (Helper Function)
```javascript
export function selectShortestQueue(resources, manager) {
  let minQueue = Infinity;
  let selectedResource = null;

  for (const resource of resources) {
    const status = manager.getCapacityStatus(resource.id);
    const queueLength = status.current;

    if (queueLength < minQueue) {
      minQueue = queueLength;
      selectedResource = resource;
    }
  }

  return selectedResource;
}
```

### 4.3 Strategy Comparison

| Strategy | Use Case | Pros | Cons |
|----------|----------|------|------|
| **Priority** | Critical tasks to senior staff | Simple, predictable | Can overload high-priority resources |
| **Round-Robin** | Even workload distribution | Fair, balanced | Ignores resource availability/capacity |
| **Random** | Reduce patterns, load balancing | Simple, no state | No optimization |
| **Shortest Queue** | Minimize wait time | Optimizes throughput | Requires capacity tracking overhead |
| **Weighted Capacity** | Match to resource capacity | Efficient utilization | Complex, requires tuning |

---

## 5. Constraint Evaluation

### 5.1 Capacity Constraints

```javascript
// Hard constraint: Allocation fails if capacity exceeded
const capacityCheck = checkResourceCapacity(store, resource);
if (!capacityCheck.allowed) {
  throw new Error(
    `Capacity exceeded for ${resource.id}: ` +
    `${capacityCheck.current}/${capacityCheck.max}`
  );
}
```

**Evaluation**: Query RDF store via SPARQL to count active allocations.

### 5.2 Eligibility Constraints (SPARQL-Based)

#### Example: Role Membership
```javascript
const seniorApprover = createRole({
  id: 'senior-approvers',
  sparql: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    ASK {
      ?person foaf:hasRole <http://example.org/roles/senior-approver> ;
              yawl:experienceYears ?years .
      FILTER(?years >= 5)
    }
  `
});
```

#### Example: Capability Check
```javascript
const documentSigner = createParticipant({
  id: 'legal-team-member',
  sparql: `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    ASK {
      ?resource yawl:hasCapability <http://example.org/capabilities/sign-legal-documents> .
    }
  `
});
```

#### Example: Separation of Duties
```javascript
const auditor = createParticipant({
  id: 'auditor-001',
  sparql: `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    ASK {
      # Auditor must NOT have processed the case previously
      FILTER NOT EXISTS {
        ?previousAllocation yawl:resource ?resource ;
                           yawl:workItem ?previousWorkItem .
        ?previousWorkItem yawl:caseId ?case .
      }
    }
  `
});
```

### 5.3 Availability Constraints (Calendar-Based)

```javascript
// Time window structure
const timeWindow = {
  start: "2024-01-15T09:00:00Z",
  end: "2024-01-15T17:00:00Z",
  available: true
};

// Set availability for resource
manager.setAvailability('alice', true, [timeWindow]);

// Check availability during allocation
const availability = manager.getAvailability('alice');
if (!availability.available) {
  // Resource is not available (e.g., on vacation, after hours)
}
```

**Calendar Functions**:
- `windowsOverlap()`: Detect scheduling conflicts
- `mergeTimeWindows()`: Combine overlapping windows
- `calculateAvailableSlots()`: Find free time slots
- `findNextAvailableSlot()`: Schedule to next opening

### 5.4 Constraint Evaluation Order

```
1. Input Validation (Zod schemas)
   ↓
2. Capacity Constraint (hard stop)
   ↓
3. Eligibility Constraint (SPARQL)
   ↓
4. Availability Constraint (optional)
   ↓
5. Policy Pack Priority
   ↓
6. Allocation Strategy
   ↓
7. Receipt Generation
```

---

## 6. Work Item Lifecycle (UNRDF Implementation)

### 6.1 State Transitions

```javascript
// yawl-types.mjs:412-419
export const WORK_ITEM_STATUS_TRANSITIONS = Object.freeze({
  enabled: ['started', 'suspended', 'cancelled'],
  started: ['completed', 'failed', 'suspended', 'cancelled'],
  suspended: ['enabled', 'started', 'cancelled'],  // ⚠️ Deviates from YAWL spec
  completed: [],  // Terminal
  failed: [],     // Terminal
  cancelled: [],  // Terminal
});
```

### 6.2 Allocation Lifecycle

```
[Work Item Created]
        ↓
[Engine enables task] → ENABLED state
        ↓
[Engine calls allocateResource()] → Allocation created in RDF
        ↓
[Resource starts work] → STARTED state
        ↓
[Work completes] → COMPLETED state
        ↓
[deallocateResource()] → Allocation marked "deallocated"
```

### 6.3 Allocation Receipt

```javascript
// Receipt structure (AllocationReceiptSchema)
{
  id: "alloc-1234567890-1",           // Unique allocation ID
  workItemId: "wi-001",                // Associated work item
  resourceId: "alice",                 // Allocated resource
  resourceType: "Participant",         // Resource type
  allocatedAt: "2024-01-15T09:00:00Z", // Timestamp
  expiresAt: "2024-01-15T17:00:00Z",   // Optional expiration
  proof: {
    capacityCheck: true,               // Capacity was available
    eligibilityCheck: true,            // Eligibility passed
    policyPackId: "approval-workflow", // Policy pack used
    sparqlResult: true                 // SPARQL query result
  }
}
```

**Receipt as Audit Proof**: Receipts provide cryptographic evidence that allocation followed all rules. Stored in RDF, they enable:
- Time-travel queries (all allocations at timestamp T)
- Compliance auditing (who did what when)
- Capacity forensics (why did allocation succeed/fail)

### 6.4 Deallocation

```javascript
export function performResourceDeallocation(store, allocationId) {
  const allocationNode = namedNode(`${YAWL_NS}allocation/${allocationId}`);

  // Check allocation exists
  const allocations = store.match(allocationNode, rdf('type'), yawl('Allocation'), null);
  if (!Array.from(allocations).length) {
    return false;
  }

  // Mark as deallocated (soft delete)
  store.add(quad(allocationNode, yawl('status'), literal('deallocated'), defaultGraph()));
  store.add(quad(
    allocationNode,
    yawl('deallocatedAt'),
    literal(new Date().toISOString(), namedNode(xsd('dateTime'))),
    defaultGraph()
  ));

  return true;
}
```

**Soft Delete**: Allocations are never removed from RDF store, only marked "deallocated". This preserves complete audit history.

---

## 7. Code Examples: Allocation Decision Logic

### 7.1 Complete Allocation Scenario

```javascript
// Source: examples/resource-allocation.mjs:326-426
async function completeWorkflowScenario() {
  const manager = createResourceManager();

  // Define workflow resources
  const policyPack = createPolicyPack({
    id: 'expense-approval-workflow',
    name: 'Expense Approval Workflow',
    resources: [
      createParticipant({ id: 'clerk-01', capacity: 5 }),
      createParticipant({ id: 'manager-01', capacity: 3 }),
      createTool({ id: 'validation-service', capacity: -1 }),
    ],
    priority: 50
  });

  manager.registerPolicyPack(policyPack);

  // Step 1: Clerk receives expense submission
  const case1 = {
    id: 'exp-2024-001',
    taskId: 'submit-expense',
    caseId: 'expense-2024-001'
  };

  const clerkResource = policyPack.resources.find(r => r.id === 'clerk-01');
  const receipt1 = await manager.allocateResource(case1, clerkResource);

  console.log(`Allocated: ${receipt1.resourceId}`);
  console.log(`Receipt ID: ${receipt1.id}`);

  // Check clerk capacity (should be 1/5)
  const clerkStatus = manager.getCapacityStatus('clerk-01');
  console.log(`Clerk capacity: ${clerkStatus.current}/${clerkStatus.max}`);

  // Step 2: Auto-validation by tool
  const case2 = {
    id: 'exp-2024-001-validate',
    taskId: 'validate-expense',
    caseId: 'expense-2024-001'
  };

  const validationTool = policyPack.resources.find(r => r.id === 'validation-service');
  const receipt2 = await manager.allocateResource(case2, validationTool);

  console.log(`Allocated: ${receipt2.resourceId} (Tool, unlimited capacity)`);

  // Step 3: Complete validation and deallocate
  manager.deallocateResource(receipt2.id);
  console.log(`Deallocated: ${receipt2.id}`);

  // Step 4: Manager approval
  const case3 = {
    id: 'exp-2024-001-approve',
    taskId: 'approve-expense',
    caseId: 'expense-2024-001'
  };

  const managerResource = policyPack.resources.find(r => r.id === 'manager-01');
  const receipt3 = await manager.allocateResource(case3, managerResource);

  console.log(`Allocated: ${receipt3.resourceId}`);

  // Summary: View all active allocations
  const activeAllocations = manager.getActiveAllocations();
  for (const alloc of activeAllocations) {
    console.log(`${alloc.allocationId}: ${alloc.resourceId} <- ${alloc.workItemId}`);
  }
}
```

### 7.2 Resource Pool with Round-Robin

```javascript
// Source: examples/resource-allocation.mjs:165-211
async function resourcePools() {
  const manager = createResourceManager();

  // Create pool with round-robin strategy
  const pool = manager.createResourcePool({
    id: 'reviewers-pool',
    name: 'Document Reviewers',
    resources: [
      createParticipant({ id: 'reviewer-1', capacity: 1 }),
      createParticipant({ id: 'reviewer-2', capacity: 1 }),
      createParticipant({ id: 'reviewer-3', capacity: 1 }),
    ],
    allocationStrategy: 'round-robin'
  });

  // Allocate work items (pool rotates through reviewers)
  const workItems = [
    { id: 'wi-100', taskId: 'review', caseId: 'case-a' },
    { id: 'wi-101', taskId: 'review', caseId: 'case-b' },
    { id: 'wi-102', taskId: 'review', caseId: 'case-c' },
  ];

  for (const wi of workItems) {
    const receipt = await pool.allocateAny(wi);
    console.log(`${wi.id} -> ${receipt.resourceId}`);
  }

  // Output:
  // wi-100 -> reviewer-1
  // wi-101 -> reviewer-2
  // wi-102 -> reviewer-3
}
```

### 7.3 SPARQL Eligibility Check

```javascript
// Source: examples/resource-allocation.mjs:117-156
async function sparqlEligibility() {
  const manager = createResourceManager();

  // Resource with SPARQL eligibility condition
  const seniorApprover = createRole({
    id: 'senior-approvers',
    name: 'Senior Approval Team',
    sparql: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
      ASK {
        ?person foaf:hasRole <http://example.org/roles/senior-approver> ;
                yawl:experienceYears ?years .
        FILTER(?years >= 5)
      }
    `
  });

  const policyPack = createPolicyPack({
    id: 'senior-review',
    resources: [seniorApprover],
    priority: 20
  });

  manager.registerPolicyPack(policyPack);

  // Get eligible resources (SPARQL query executed for each)
  const eligible = await manager.getEligibleResources('critical-review', 'case-456');

  console.log(`Eligible resources: ${eligible.length}`);
  // Only resources meeting SPARQL criteria are returned
}
```

---

## 8. Comparison with Java YAWL YResourceService

### 8.1 Architecture Differences

| Aspect | Java YAWL 4.x | UNRDF YAWL |
|--------|---------------|------------|
| **Storage** | Hibernate/SQL database | RDF triple store (Oxigraph) |
| **Resource Model** | Java classes | Zod schemas + RDF |
| **Eligibility** | Java code or YAWL expressions | SPARQL queries |
| **Worklist Service** | Interface B (XML-RPC/REST) | ❌ Not implemented |
| **Allocation Proof** | Database audit log | Cryptographic receipts |
| **State Machine** | 8 states (Created, Offered, Allocated, Started, ...) | 6 states (Pending, Enabled, Active, ...) |
| **Delegation** | Full support | ❌ Not implemented |
| **Piling** | Batch processing support | ❌ Not implemented |
| **Calendar** | Full scheduling | ⚠️ Partial (time windows only) |

### 8.2 Feature Parity Matrix

| Feature | Java YAWL | UNRDF YAWL | Gap |
|---------|-----------|------------|-----|
| Resource Types (Participant, Role, Tool) | ✓ | ✓ | - |
| Capacity Tracking | ✓ | ✓ | - |
| SPARQL Eligibility | ✓ | ✓ | - |
| Policy Packs (Priority) | ✓ | ✓ | - |
| Offer to Role/Group | ✓ | ❌ | Critical |
| Allocation (User Claims Item) | ✓ | ❌ | Critical |
| Worklist Query API | ✓ | ❌ | Critical |
| Delegation | ✓ | ❌ | High |
| Reallocation | ✓ | ❌ | High |
| Piling (Batch Processing) | ✓ | ❌ | Medium |
| Suspend/Resume API | ✓ | ❌ | Medium |
| Calendar Integration | ✓ | ⚠️ | Medium |
| Four-Eyes Principle | ✓ | ⚠️ | Via custom SPARQL |
| Retain Familiar | ✓ | ❌ | Low |
| Allocation Strategies | ✓ (built-in) | ⚠️ (pools only) | Medium |

**Parity Score**: 8/17 features = **47% feature parity**

### 8.3 Java YAWL Code Equivalent

```java
// Java YAWL 4.x: Offer work item to role
public void offerWorkItem(String workItemId, String roleId) {
    WorkItemRecord wir = getWorkItem(workItemId);
    Role role = resourceService.getRole(roleId);

    // Offer to all role members
    Set<Participant> members = role.getMembers();
    for (Participant p : members) {
        if (p.isAvailable() && hasCapacity(p)) {
            wir.setStatus(WorkItemStatus.OFFERED);
            addToWorklist(p, wir);
        }
    }

    updateWorkItemStatus(workItemId, WorkItemStatus.OFFERED);
}

// User claims offered item from worklist
public void allocateWorkItem(String workItemId, String participantId) {
    WorkItemRecord wir = getWorkItem(workItemId);
    if (wir.getStatus() != WorkItemStatus.OFFERED) {
        throw new WorkItemException("Item not in OFFERED state");
    }

    wir.setStatus(WorkItemStatus.ALLOCATED);
    wir.setResourceID(participantId);
    updateWorkItemStatus(workItemId, WorkItemStatus.ALLOCATED);
}
```

**UNRDF Equivalent**: Does not exist. Work items are allocated directly by the engine without user interaction.

---

## 9. Key Insights and Architectural Decisions

### 9.1 Engine-Centric vs. Worklist-Centric

**Design Choice**: UNRDF YAWL prioritizes **automated workflow execution** over human task management.

**Rationale**:
- Modern workflows are increasingly automated (service-to-service)
- Human worklists add complexity and latency
- RDF-native design enables advanced queries without dedicated worklist service
- Receipt-based auditing provides better compliance than traditional worklists

**Trade-off**:
- ✅ Excellent for: Microservice orchestration, automated approvals, system integration
- ❌ Poor for: Human task management, collaborative workflows, user choice

### 9.2 RDF Triple Store as Resource Allocator

**Innovation**: Using RDF triple store (Oxigraph) instead of SQL database.

**Benefits**:
1. **Semantic Queries**: SPARQL enables complex eligibility rules (role membership, capabilities, organizational hierarchy)
2. **Graph Relationships**: Natural representation of resource hierarchies and dependencies
3. **Schema Flexibility**: Add new resource properties without database migrations
4. **Time-Travel**: Query allocation state at any historical point
5. **Integration**: Resources can be linked to external knowledge graphs

**Costs**:
1. SPARQL learning curve (vs. SQL familiarity)
2. Query performance for large-scale allocation (untested)
3. Tooling ecosystem less mature than SQL

### 9.3 Receipt-Based Allocation Proofs

**Pattern**: Every allocation generates a cryptographically verifiable receipt.

**Advantages**:
- **Audit Trail**: Immutable record of who allocated what when
- **Compliance**: Proof that capacity and eligibility checks passed
- **Debugging**: Trace allocation decisions post-facto
- **Receipts as Facts**: Can be hashed, timestamped, and published to blockchain

**Example Receipt**:
```json
{
  "id": "alloc-1705315200000-1",
  "workItemId": "wi-001",
  "resourceId": "alice",
  "allocatedAt": "2024-01-15T09:00:00Z",
  "proof": {
    "capacityCheck": true,
    "eligibilityCheck": true,
    "sparqlResult": true,
    "policyPackId": "approval-workflow"
  }
}
```

This receipt is **cryptographic evidence** that allocation was valid.

### 9.4 Policy Pack Priority System

**Concept**: Resources are grouped into policy packs with numeric priorities.

**Use Cases**:
- **Tenant Isolation**: Each tenant gets their own policy pack
- **SLA Tiers**: Premium customers get priority 100, standard get priority 0
- **A/B Testing**: Deploy experimental resources in high-priority pack
- **Feature Flags**: Enable/disable entire resource sets via `enabled` flag

**Example**:
```javascript
manager.registerPolicyPack(createPolicyPack({
  id: 'premium-customers',
  resources: [premiumAgent1, premiumAgent2],
  priority: 100,
  enabled: true
}));

manager.registerPolicyPack(createPolicyPack({
  id: 'standard-customers',
  resources: [standardAgent1, standardAgent2, standardAgent3],
  priority: 0,
  enabled: true
}));

// Premium customers always get premium agents first
const eligible = await manager.getEligibleResources('support-ticket', 'case-123');
// Returns: [premiumAgent1, premiumAgent2, standardAgent1, ...]
```

---

## 10. Adversarial Questions Answered

### Q1: How does the engine decide WHO gets a work item?

**Answer**: The engine uses a **multi-stage filter-and-select** process:

1. **Policy Pack Priority**: Higher-priority packs are checked first
2. **Capacity Filter**: Exclude resources at max capacity
3. **Eligibility Filter**: Execute SPARQL queries; exclude failed
4. **Availability Filter**: Check calendar windows (if enabled)
5. **Strategy Selection**: Apply round-robin, random, or priority

**Example Decision Flow**:
```
Task: Approve Purchase Order ($50,000)
Case: PO-2024-001

Policy Packs (sorted by priority):
  [1] "Senior Management" (priority: 100)
      - alice (capacity: 1/3, eligible: YES, available: YES) ← SELECTED
      - bob (capacity: 3/3, eligible: YES, available: NO)   ← Capacity exceeded
  [2] "General Approvers" (priority: 0)
      - charlie (capacity: 0/5, eligible: NO, available: YES) ← Not checked (alice already selected)

Decision: Allocate to alice
Receipt: alloc-1234-5678
Proof: capacityCheck=true, eligibilityCheck=true, sparqlResult=true
```

### Q2: Can a human user see their assigned work items?

**Answer**: ⚠️ **Partially** - via direct SPARQL queries, NOT via dedicated API.

```javascript
// Manual worklist query (not provided by API)
const myWorkItems = manager.query(`
  PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
  SELECT ?workItem ?taskId ?status WHERE {
    ?allocation yawl:resource <http://yawlfoundation.org/yawlschema#resource/alice> ;
                yawl:workItem ?workItem .
    FILTER NOT EXISTS { ?allocation yawl:status "deallocated" }
    ?workItem yawl:taskId ?taskId ;
              yawl:status ?status .
  }
`);
```

**Missing**: `getMyWorkItems(userId)` API that abstracts this query.

### Q3: Can work items be offered to a role/team before allocation?

**Answer**: ❌ **NO** - No OFFERED state or offer mechanism.

In Java YAWL:
```java
// Offer to role "approvers"
offerWorkItem("wi-001", "approvers");
// → All members of "approvers" see item in their worklist

// User "alice" claims from worklist
allocateWorkItem("wi-001", "alice");
// → Item moved to alice's allocated list
```

In UNRDF YAWL:
```javascript
// Direct allocation only
await manager.allocateResource(workItem, aliceResource);
// → No offer phase, no user choice
```

### Q4: What happens if a resource becomes unavailable mid-allocation?

**Answer**: **Nothing automatic** - allocation remains active.

**Workaround**:
1. Deallocate manually: `manager.deallocateResource(allocationId)`
2. Reallocate to different resource (not atomic)

**Missing**: Automatic expiration based on `expiresAt` timestamp.

### Q5: Can you reallocate a work item from one resource to another?

**Answer**: ⚠️ **Manually** - no atomic reallocation API.

**Current Process**:
```javascript
// Step 1: Deallocate from alice
manager.deallocateResource(receipt1.id);

// Step 2: Allocate to bob (separate transaction)
const receipt2 = await manager.allocateResource(workItem, bobResource);
```

**Missing**: `reallocateResource(workItemId, fromResourceId, toResourceId)` with atomic semantics.

### Q6: How are capacity constraints enforced?

**Answer**: **Hard constraint at allocation time** via SPARQL count query.

```javascript
// Capacity check logic
const activeCount = store.query(`
  SELECT (COUNT(?allocation) as ?count) WHERE {
    ?allocation yawl:resource <yawl:resource/alice> .
    FILTER NOT EXISTS { ?allocation yawl:status "deallocated" }
  }
`);

if (activeCount >= resource.capacity) {
  throw new Error("Capacity exceeded");
}
```

**Enforcement**: Allocation fails immediately if capacity exceeded.

**No Queuing**: If no capacity, allocation fails. Work item does not enter a queue.

### Q7: How does SPARQL eligibility work in practice?

**Answer**: Custom SPARQL queries execute against RDF store to check resource eligibility.

**Example 1: Role Membership**
```javascript
const approver = createRole({
  id: 'senior-approvers',
  sparql: `
    ASK {
      ?person foaf:hasRole <http://example.org/roles/senior> .
      ?person foaf:available true .
    }
  `
});
```

**Example 2: Capability Check**
```javascript
const legalReviewer = createParticipant({
  id: 'legal-001',
  sparql: `
    ASK {
      ?resource yawl:hasCapability <http://example.org/capabilities/legal-review> .
    }
  `
});
```

**Example 3: Separation of Duties**
```javascript
const auditor = createParticipant({
  id: 'auditor-001',
  sparql: `
    ASK {
      # Auditor must NOT have processed this case before
      FILTER NOT EXISTS {
        ?prevAllocation yawl:resource ?resource ;
                       yawl:workItem ?prevWorkItem .
        ?prevWorkItem yawl:caseId ?case .
      }
    }
  `
});
```

**Execution**: During `checkResourceEligibility()`, placeholders `?resource`, `?workItem`, `?case` are replaced with actual URIs, then query executes.

### Q8: What's the performance of capacity checking?

**Answer**: **Single SPARQL query per allocation** (O(1) w.r.t. work items).

**Query**:
```sparql
SELECT (COUNT(?allocation) as ?count) WHERE {
  ?allocation yawl:resource <yawl:resource/alice> .
  FILTER NOT EXISTS { ?allocation yawl:status "deallocated" }
}
```

**Performance Factors**:
- Oxigraph SPARQL engine performance (Rust-based, fast)
- Triple store size (grows with allocations)
- Index on `yawl:resource` predicate

**Optimization**: Result could be cached per resource with TTL if performance becomes an issue.

---

## 11. Recommendations for Future Work

### Priority 1: CRITICAL (YAWL Compliance)

1. **Implement OFFERED State**
   - Add `OFFERED` to `WORK_ITEM_STATUS` enum
   - Implement `offerItem(workItemId, resourceIds[])` API
   - Store offered resources in RDF

2. **Implement Worklist Query APIs**
   - `getWorkItemsForResource(resourceId)` - Get my work items
   - `getOfferedItems(resourceId)` - Items I can claim
   - `getAllocatedItems(resourceId)` - Items assigned to me

3. **Implement Explicit Allocation API**
   - `allocateItem(workItemId, resourceId)` - User claims item
   - Enforce OFFERED → ALLOCATED → STARTED sequence

### Priority 2: HIGH (User Experience)

4. **Implement Delegation**
   - `delegateItem(workItemId, toResourceId)` - Delegate to colleague
   - Track delegation chain in receipts
   - Support return-to-sender

5. **Implement Suspend/Resume**
   - `suspendTask(workItemId)` API
   - `resumeTask(workItemId)` API
   - Fix transition: suspended → started (not enabled)

### Priority 3: MEDIUM (Advanced Features)

6. **Atomic Reallocation**
   - `reallocateItem(workItemId, toResourceId)` - Atomic transfer
   - Update both allocation and work item state

7. **Work Item Piling**
   - `pileItem(workItemId, pileId)` - Batch related items
   - `unpileItem(workItemId)` - Remove from pile
   - Batch completion support

8. **Automatic Expiration**
   - Background job to deallocate expired allocations
   - Configurable expiration policies

### Priority 4: LOW (Nice to Have)

9. **Shortest Queue Strategy**
   - Implement as built-in allocation strategy
   - Real-time queue length tracking

10. **Four-Eyes Principle**
    - Built-in constraint (currently requires custom SPARQL)
    - Configurable as policy pack option

---

## 12. Conclusion

### Summary

The UNRDF YAWL resource allocation system is a **modern, RDF-native implementation** that excels at:
- ✅ Automated workflow execution
- ✅ Cryptographic allocation proofs
- ✅ SPARQL-based eligibility rules
- ✅ Policy-based resource management
- ✅ Time-travel queries and auditing

However, it **lacks critical worklist management features** for human task interaction:
- ❌ No OFFERED state or offer mechanism
- ❌ No per-user worklist views
- ❌ No delegation or reallocation
- ❌ No work item piling

### Design Philosophy

**UNRDF YAWL is engine-centric, not worklist-centric.**

This is a **deliberate design choice** for modern automated workflows where:
- Services allocate work to themselves
- Human interaction is minimal
- Audit trails are more important than worklist UIs

For **human-centric workflows**, YAWL Interface B features are required but not yet implemented.

### How Engine Decides WHO Gets Work Item (Final Answer)

```python
def decide_who_gets_work_item(engine, task, case):
    # 1. Get all registered policy packs (sorted by priority)
    policy_packs = get_policy_packs_sorted_by_priority()

    # 2. For each pack (high to low priority):
    for pack in policy_packs:
        if not pack.enabled:
            continue

        # 3. For each resource in pack:
        for resource in pack.resources:
            # 4. Check constraints (fail-fast)
            if not has_capacity(resource):
                continue

            if not is_eligible(resource, task, case):  # SPARQL check
                continue

            if not is_available(resource):  # Calendar check
                continue

            # 5. First resource passing all constraints wins
            return resource

    # 6. No eligible resource found
    return None
```

**Key Points**:
1. **Priority-based**: Higher-priority packs checked first
2. **Fail-fast**: Constraints checked in order (capacity → eligibility → availability)
3. **First-match**: First resource passing all checks wins (no optimization)
4. **No user choice**: Engine makes decision, resource has no say

---

## Appendix A: File Structure

```
packages/yawl/src/resources/
├── index.mjs                          # Public API exports
├── yawl-resources-types.mjs           # Zod schemas, namespaces
├── yawl-resources-core.mjs            # YawlResourceManager class
├── yawl-resources-allocation.mjs      # Allocation/deallocation logic
├── yawl-resources-eligibility.mjs     # Eligibility checking
├── yawl-resources-rdf.mjs             # RDF storage helpers
├── yawl-resources-pools.mjs           # Resource pool strategies
├── yawl-resources-calendar.mjs        # Availability/scheduling
├── yawl-resources-calendar-impl.mjs   # Calendar implementation
├── resource-capacity.mjs              # Capacity management
├── resource-participants.mjs          # Participant helpers
├── resource-roles.mjs                 # Role helpers
└── resource-tools.mjs                 # Tool helpers

packages/yawl/examples/
└── resource-allocation.mjs            # Complete usage examples

packages/yawl/test/
└── yawl-resources.test.mjs            # Test suite
```

---

## Appendix B: RDF Vocabulary

```turtle
@prefix yawl: <http://yawlfoundation.org/yawlschema#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
yawl:Participant a rdfs:Class .
yawl:Tool a rdfs:Class .
yawl:Role a rdfs:Class .
yawl:PolicyPack a rdfs:Class .
yawl:Allocation a rdfs:Class .
yawl:ResourcePool a rdfs:Class .

# Properties
yawl:capacity a rdf:Property ; rdfs:range xsd:integer .
yawl:eligibilitySparql a rdf:Property ; rdfs:range xsd:string .
yawl:hasResource a rdf:Property ; rdfs:domain yawl:PolicyPack .
yawl:priority a rdf:Property ; rdfs:range xsd:integer .
yawl:enabled a rdf:Property ; rdfs:range xsd:boolean .
yawl:resource a rdf:Property ; rdfs:domain yawl:Allocation .
yawl:workItem a rdf:Property ; rdfs:domain yawl:Allocation .
yawl:allocatedAt a rdf:Property ; rdfs:range xsd:dateTime .
yawl:deallocatedAt a rdf:Property ; rdfs:range xsd:dateTime .
yawl:expiresAt a rdf:Property ; rdfs:range xsd:dateTime .
yawl:status a rdf:Property .
yawl:allocationStrategy a rdf:Property .
```

---

## Appendix C: SPARQL Query Templates

### Get Active Allocations for Resource
```sparql
PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?workItem ?allocatedAt WHERE {
  ?allocation rdf:type yawl:Allocation ;
              yawl:resource <http://yawlfoundation.org/yawlschema#resource/alice> ;
              yawl:workItem ?workItem ;
              yawl:allocatedAt ?allocatedAt .
  FILTER NOT EXISTS {
    ?allocation yawl:status "deallocated" .
  }
}
```

### Find Resources by Type
```sparql
PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?resource ?name ?capacity WHERE {
  ?resource rdf:type yawl:Participant .
  OPTIONAL { ?resource foaf:name ?name }
  OPTIONAL { ?resource yawl:capacity ?capacity }
}
```

### Capacity Utilization Report
```sparql
PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?resource ?name ?capacity (COUNT(?allocation) as ?active) WHERE {
  ?resource rdf:type yawl:Participant ;
            yawl:capacity ?capacity .
  OPTIONAL { ?resource foaf:name ?name }
  OPTIONAL {
    ?allocation yawl:resource ?resource .
    FILTER NOT EXISTS { ?allocation yawl:status "deallocated" }
  }
}
GROUP BY ?resource ?name ?capacity
```

---

**Report End**

**Next Actions**:
1. Share with stakeholders for YAWL compliance decision
2. Prioritize worklist features if human workflows required
3. Document limitations in public API documentation
4. Consider renaming to "YAWL Control Flow Engine" if worklist features deferred

**Questions?** Review Adversarial Questions in Section 10 for deeper analysis.
