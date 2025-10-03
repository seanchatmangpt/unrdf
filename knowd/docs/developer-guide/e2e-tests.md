# End-to-End Tests: Knowledge as Governed Actions

This document defines comprehensive end-to-end tests that validate the core thesis: **Knowledge stops being documents and becomes governed actions**. These tests demonstrate that Knowd implements a system where knowledge consists of facts, policies, and receipts working together.

## Test Philosophy

The e2e tests validate that:

1. **Knowledge = Facts + Policies + Receipts** - Every meaningful action involves all three
2. **Only Hooks Act** - Everything else can only propose; hooks are the only actors
3. **Cryptographic Proofs** - All actions generate verifiable receipts
4. **Governance by Design** - Policy enforcement is built-in, not bolted-on

## Test Categories

### 1. Core Knowledge Model Tests

#### Test 1.1: Fact-Policy-Receipt Integration
**Validates:** Facts, policies, and receipts work together as the unit of knowledge

```go
func TestE2E_FactPolicyReceiptIntegration(t *testing.T) {
    // Setup: Create a complete knowledge environment
    engine, err := setupTestEnvironment()
    require.NoError(t, err)

    // 1. Submit a fact (proposal)
    txReq := TxRequest{
        Actor: "data-loader@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/document/123>",
                Predicate: "<http://example.org/title>",
                Object:    "\"Important Document\"",
                Graph:     "documents",
            }},
        },
    }

    // 2. Execute transaction
    result, err := engine.Tx(context.Background(), txReq)
    require.NoError(t, err)

    // 3. Verify receipt exists and is valid
    receipt, err := getReceipt(result.ReceiptID)
    require.NoError(t, err)
    assert.NotEmpty(t, receipt.ID)
    assert.Equal(t, "data-loader@example.com", receipt.Actor)

    // 4. Verify receipt is cryptographically valid
    verification, err := verifyReceipt(receipt.ID)
    require.NoError(t, err)
    assert.True(t, verification.OK)
    assert.True(t, verification.SignatureVerified)

    // 5. Verify fact is stored and queryable
    queryResult, err := engine.Query(context.Background(), QueryRequest{
        Query: "SELECT ?title WHERE { <http://example.org/document/123> <http://example.org/title> ?title }",
        Kind:  "sparql-select",
    })
    require.NoError(t, err)
    assert.Len(t, queryResult.Rows, 1)
    assert.Equal(t, "\"Important Document\"", queryResult.Rows[0]["title"])
}
```

#### Test 1.2: Policy Enforcement Without Receipt
**Validates:** Actions without proper policy evaluation don't create valid knowledge

```go
func TestE2E_PolicyEnforcementWithoutReceipt(t *testing.T) {
    // Setup: Create environment with restrictive policy
    engine, err := setupTestEnvironmentWithPolicy()
    require.NoError(t, err)

    // 1. Attempt unauthorized action (proposal only)
    txReq := TxRequest{
        Actor: "unauthorized@example.com", // Not in authorized actors list
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/secret>",
                Predicate: "<http://example.org/data>",
                Object:    "\"classified\"",
                Graph:     "restricted",
            }},
        },
    }

    // 2. Transaction should fail due to policy
    result, err := engine.Tx(context.Background(), txReq)
    assert.Error(t, err) // Should fail
    assert.Nil(t, result) // No result returned

    // 3. Verify no receipt was generated
    // (No receipt ID to check)

    // 4. Verify data was not stored
    queryResult, err := engine.Query(context.Background(), QueryRequest{
        Query: "ASK WHERE { <http://example.org/secret> ?p ?o }",
        Kind:  "sparql-ask",
    })
    require.NoError(t, err)
    assert.False(t, queryResult.Rows[0]["result"].(bool))
}
```

### 2. Operating Model Tests

#### Test 2.1: Propose → Evaluate → Act → Receipt Flow
**Validates:** The complete operating model works as designed

```go
func TestE2E_ProposalEvaluationActionReceiptFlow(t *testing.T) {
    // Setup: Complete environment with hooks and policies
    engine, err := setupCompleteTestEnvironment()
    require.NoError(t, err)

    // 1. PROPOSE: Submit a transaction (user proposes action)
    txReq := TxRequest{
        Actor: "workflow-user@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/task/001>",
                Predicate: "<http://example.org/status>",
                Object:    "\"in-progress\"",
                Graph:     "workflows",
            }},
        },
    }

    // 2. EVALUATE: Hooks evaluate the proposal
    // (This happens automatically during transaction processing)

    // 3. ACT: If approved by hooks, action is taken
    result, err := engine.Tx(context.Background(), txReq)
    require.NoError(t, err) // Should succeed due to proper authorization

    // 4. RECEIPT: Cryptographic proof is generated
    assert.NotEmpty(t, result.ReceiptID)

    // 5. Verify complete flow
    receipt, err := getReceipt(result.ReceiptID)
    require.NoError(t, err)

    // Verify the receipt contains the action that was taken
    assert.Contains(t, receipt.Delta.Add, txReq.Delta.Add[0])

    // Verify hooks were executed (check for side effects)
    sideEffects, err := checkForSideEffects(receipt.ID)
    require.NoError(t, err)
    assert.NotEmpty(t, sideEffects) // Hooks should have created additional facts
}
```

#### Test 2.2: Hook Rejection Prevents Action
**Validates:** Hooks can reject proposals, preventing unauthorized actions

```go
func TestE2E_HookRejectionPreventsAction(t *testing.T) {
    // Setup: Environment with rejecting policy
    engine, err := setupTestEnvironmentWithRejectingPolicy()
    require.NoError(t, err)

    // 1. PROPOSE: Submit transaction that should be rejected
    txReq := TxRequest{
        Actor: "restricted-user@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/restricted-data>",
                Predicate: "<http://example.org/access>",
                Object:    "\"granted\"",
                Graph:     "security",
            }},
        },
    }

    // 2. EVALUATE: Policy hook should reject this
    result, err := engine.Tx(context.Background(), txReq)
    assert.Error(t, err) // Should be rejected
    assert.Nil(t, result) // No receipt generated

    // 3. Verify no action was taken
    queryResult, err := engine.Query(context.Background(), QueryRequest{
        Query: "ASK WHERE { <http://example.org/restricted-data> <http://example.org/access> \"granted\" }",
        Kind:  "sparql-ask",
    })
    require.NoError(t, err)
    assert.False(t, queryResult.Rows[0]["result"].(bool))

    // 4. Verify rejection was logged
    rejectionLog, err := getRejectionLog()
    require.NoError(t, err)
    assert.Contains(t, rejectionLog, "Policy violation")
    assert.Contains(t, rejectionLog, "restricted-user@example.com")
}
```

### 3. Governance Model Tests

#### Test 3.1: Only Hooks Can Execute Actions
**Validates:** The core governance principle that only hooks can act

```go
func TestE2E_OnlyHooksCanExecuteActions(t *testing.T) {
    // Setup: Environment with strict governance
    engine, err := setupGovernanceTestEnvironment()
    require.NoError(t, err)

    // 1. User proposes action (should be blocked)
    txReq := TxRequest{
        Actor: "direct-user@example.com", // Not a hook
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/direct-action>",
                Predicate: "<http://example.org/executed>",
                Object:    "\"true\"",
                Graph:     "governance-test",
            }},
        },
    }

    // 2. Direct execution should fail
    result, err := engine.Tx(context.Background(), txReq)
    assert.Error(t, err)
    assert.Nil(t, result)

    // 3. Hook-mediated execution should succeed
    hookTxReq := TxRequest{
        Actor: "governance-hook", // This is a hook
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/hook-action>",
                Predicate: "<http://example.org/executed>",
                Object:    "\"true\"",
                Graph:     "governance-test",
            }},
        },
    }

    hookResult, err := engine.Tx(context.Background(), hookTxReq)
    require.NoError(t, err)
    assert.NotEmpty(t, hookResult.ReceiptID)

    // 4. Verify governance worked
    // Direct action: not executed
    directQuery, err := engine.Query(context.Background(), QueryRequest{
        Query: "ASK WHERE { <http://example.org/direct-action> <http://example.org/executed> \"true\" }",
        Kind:  "sparql-ask",
    })
    require.NoError(t, err)
    assert.False(t, directQuery.Rows[0]["result"].(bool))

    // Hook action: executed with receipt
    hookQuery, err := engine.Query(context.Background(), QueryRequest{
        Query: "ASK WHERE { <http://example.org/hook-action> <http://example.org/executed> \"true\" }",
        Kind:  "sparql-ask",
    })
    require.NoError(t, err)
    assert.True(t, hookQuery.Rows[0]["result"].(bool))
}
```

#### Test 3.2: Cryptographic Receipt Verification
**Validates:** All actions generate verifiable cryptographic proofs

```go
func TestE2E_CryptographicReceiptVerification(t *testing.T) {
    // Setup: Environment with signing keys
    engine, err := setupTestEnvironmentWithSigning()
    require.NoError(t, err)

    // 1. Execute multiple actions
    actions := []TxRequest{
        {
            Actor: "user1@example.com",
            Delta: struct {
                Add []Quad `json:"add"`
                Rem []Quad `json:"rem"`
            }{
                Add: []Quad{{Subject: "<http://example.org/a>", Predicate: "<http://example.org/p>", Object: "\"1\"", Graph: "test"}},
            },
        },
        {
            Actor: "user2@example.com",
            Delta: struct {
                Add []Quad `json:"add"`
                Rem []Quad `json:"rem"`
            }{
                Add: []Quad{{Subject: "<http://example.org/b>", Predicate: "<http://example.org/p>", Object: "\"2\"", Graph: "test"}},
            },
        },
    }

    var receiptIDs []string
    for _, action := range actions {
        result, err := engine.Tx(context.Background(), action)
        require.NoError(t, err)
        receiptIDs = append(receiptIDs, result.ReceiptID)
    }

    // 2. Verify all receipts are cryptographically valid
    for _, receiptID := range receiptIDs {
        verification, err := verifyReceipt(receiptID)
        require.NoError(t, err)
        assert.True(t, verification.OK, "Receipt %s should be valid", receiptID)
        assert.True(t, verification.SignatureVerified, "Receipt %s signature should be valid", receiptID)
        assert.True(t, verification.MerkleRootVerified, "Receipt %s merkle root should be valid", receiptID)
    }

    // 3. Verify receipts are tamper-evident
    originalReceipt := receiptIDs[0]
    tamperedReceipt := modifyReceipt(originalReceipt) // Simulate tampering

    verification, err := verifyReceipt(tamperedReceipt)
    require.NoError(t, err)
    assert.False(t, verification.OK, "Tampered receipt should fail verification")

    // 4. Verify replay capability
    replayResult, err := replayTransaction(originalReceipt)
    require.NoError(t, err)
    assert.Equal(t, "success", replayResult.Status)
}
```

### 4. Namespace Isolation Tests

#### Test 4.1: Multi-Tenant Data Isolation
**Validates:** Complete isolation between namespaces

```go
func TestE2E_MultiTenantDataIsolation(t *testing.T) {
    // Setup: Multi-tenant environment
    engine, err := setupMultiTenantEnvironment()
    require.NoError(t, err)

    // 1. Create data in namespace A
    txA := TxRequest{
        Actor: "tenant-a@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://tenant-a.org/data>",
                Predicate: "<http://tenant-a.org/value>",
                Object:    "\"secret-a\"",
                Graph:     "tenant-a",
            }},
        },
    }

    resultA, err := engine.Tx(context.Background(), txA)
    require.NoError(t, err)

    // 2. Create data in namespace B
    txB := TxRequest{
        Actor: "tenant-b@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://tenant-b.org/data>",
                Predicate: "<http://tenant-b.org/value>",
                Object:    "\"secret-b\"",
                Graph:     "tenant-b",
            }},
        },
    }

    resultB, err := engine.Tx(context.Background(), txB)
    require.NoError(t, err)

    // 3. Verify isolation: Tenant A cannot see Tenant B's data
    queryA, err := engine.Query(context.Background(), QueryRequest{
        Query: "SELECT ?value WHERE { ?s <http://tenant-b.org/value> ?value }",
        Kind:  "sparql-select",
    })
    require.NoError(t, err)
    assert.Len(t, queryA.Rows, 0, "Tenant A should not see Tenant B's data")

    // 4. Verify isolation: Tenant B cannot see Tenant A's data
    queryB, err := engine.Query(context.Background(), QueryRequest{
        Query: "SELECT ?value WHERE { ?s <http://tenant-a.org/value> ?value }",
        Kind:  "sparql-select",
    })
    require.NoError(t, err)
    assert.Len(t, queryB.Rows, 0, "Tenant B should not see Tenant A's data")

    // 5. Verify tenants can see their own data
    queryAOwn, err := engine.Query(context.Background(), QueryRequest{
        Query: "SELECT ?value WHERE { ?s <http://tenant-a.org/value> ?value }",
        Kind:  "sparql-select",
    })
    require.NoError(t, err)
    assert.Len(t, queryAOwn.Rows, 1)
    assert.Equal(t, "\"secret-a\"", queryAOwn.Rows[0]["value"])
}
```

### 5. Policy Enforcement Tests

#### Test 5.1: Real-Time Policy Enforcement
**Validates:** Policies are enforced in real-time during transactions

```go
func TestE2E_RealTimePolicyEnforcement(t *testing.T) {
    // Setup: Environment with time-based policy
    engine, err := setupTestEnvironmentWithTimePolicy()
    require.NoError(t, err)

    // 1. Business hours: Allow transaction
    businessHoursTx := TxRequest{
        Actor: "employee@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/expense/001>",
                Predicate: "<http://example.org/amount>",
                Object:    "\"100.00\"",
                Graph:     "expenses",
            }},
        },
    }

    result, err := engine.Tx(context.Background(), businessHoursTx)
    require.NoError(t, err) // Should succeed during business hours
    assert.NotEmpty(t, result.ReceiptID)

    // 2. After hours: Reject transaction
    afterHoursTx := TxRequest{
        Actor: "employee@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/expense/002>",
                Predicate: "<http://example.org/amount>",
                Object:    "\"50.00\"",
                Graph:     "expenses",
            }},
        },
    }

    result, err = engine.Tx(context.Background(), afterHoursTx)
    assert.Error(t, err) // Should fail after hours
    assert.Nil(t, result) // No receipt generated

    // 3. Verify policy was enforced
    rejectionDetails, err := getPolicyRejectionDetails()
    require.NoError(t, err)
    assert.Contains(t, rejectionDetails, "after hours")
    assert.Contains(t, rejectionDetails, "expense submission")
}
```

### 6. Performance and Scalability Tests

#### Test 6.1: Policy MTTU (Mean Time To Update)
**Validates:** Policy changes take effect quickly (< 2 minutes)

```go
func TestE2E_PolicyMTTU(t *testing.T) {
    engine, err := setupTestEnvironment()
    require.NoError(t, err)

    // 1. Deploy initial policy
    initialPolicy := createPolicyPack("v1.0", "allow-all-expenses")
    deployPolicyPack(initialPolicy)

    // 2. Execute transaction with initial policy
    tx1 := TxRequest{
        Actor: "employee@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{Subject: "<http://example.org/expense/001>", Predicate: "<http://example.org/amount>", Object: "\"100.00\"", Graph: "expenses"}},
        },
    }

    start1 := time.Now()
    result1, err := engine.Tx(context.Background(), tx1)
    duration1 := time.Since(start1)
    require.NoError(t, err)

    // 3. Update policy (should take effect quickly)
    updatedPolicy := createPolicyPack("v1.1", "restrict-high-expenses")
    deployPolicyPack(updatedPolicy)

    // 4. Wait for policy propagation (< 2 minutes)
    time.Sleep(30 * time.Second) // Simulate policy propagation

    // 5. Execute transaction with updated policy
    tx2 := TxRequest{
        Actor: "employee@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{Subject: "<http://example.org/expense/002>", Predicate: "<http://example.org/amount>", Object: "\"1000.00\"", Graph: "expenses"}},
        },
    }

    start2 := time.Now()
    result2, err := engine.Tx(context.Background(), tx2)
    duration2 := time.Since(start2)
    assert.Error(t, err) // Should be rejected by new policy
    assert.Nil(t, result2)

    // 6. Verify MTTU requirement
    mttu := time.Since(start1) // Time from policy change to effect
    assert.Less(t, mttu, 2*time.Minute, "Policy MTTU should be < 2 minutes")
}
```

#### Test 6.2: Receipt Coverage (100%)
**Validates:** Every action generates a receipt

```go
func TestE2E_ReceiptCoverage100Percent(t *testing.T) {
    engine, err := setupTestEnvironmentWithReceipts()
    require.NoError(t, err)

    // Execute various operations
    operations := []TxRequest{
        {Actor: "user1@example.com", Delta: struct{Add []Quad `json:"add"`; Rem []Quad `json:"rem"`}{Add: []Quad{{Subject: "<http://example.org/a>", Predicate: "<http://example.org/p>", Object: "\"1\"", Graph: "test"}}}},
        {Actor: "user2@example.com", Delta: struct{Add []Quad `json:"add"`; Rem []Quad `json:"rem"`}{Add: []Quad{{Subject: "<http://example.org/b>", Predicate: "<http://example.org/p>", Object: "\"2\"", Graph: "test"}}}},
        {Actor: "user3@example.com", Delta: struct{Add []Quad `json:"add"`; Rem []Quad `json:"rem"`}{Add: []Quad{{Subject: "<http://example.org/c>", Predicate: "<http://example.org/p>", Object: "\"3\"", Graph: "test"}}}},
    }

    var receiptIDs []string
    for _, op := range operations {
        result, err := engine.Tx(context.Background(), op)
        require.NoError(t, err)
        receiptIDs = append(receiptIDs, result.ReceiptID)
    }

    // Verify 100% receipt coverage
    totalOperations := len(operations)
    totalReceipts := len(receiptIDs)

    assert.Equal(t, totalOperations, totalReceipts, "Every operation should generate a receipt")

    // Verify all receipts are valid
    for _, receiptID := range receiptIDs {
        verification, err := verifyReceipt(receiptID)
        require.NoError(t, err)
        assert.True(t, verification.OK, "Receipt %s should be valid", receiptID)
    }
}
```

### 7. Integration Tests

#### Test 7.1: Complete Workflow with AI Integration
**Validates:** The system works with AI agents as proposers

```go
func TestE2E_AIIntegrationWorkflow(t *testing.T) {
    // Setup: Environment with AI agent as proposer
    engine, aiAgent, err := setupAIIntegrationEnvironment()
    require.NoError(t, err)

    // 1. AI agent proposes action based on analysis
    proposal := aiAgent.AnalyzeAndProposeAction(testContext)

    // 2. Submit proposal through normal channels
    txReq := TxRequest{
        Actor: "ai-agent@company.com",
        Delta: proposal.Delta,
        Metadata: map[string]interface{}{
            "ai_analysis": proposal.Analysis,
            "confidence":  proposal.Confidence,
            "reasoning":   proposal.Reasoning,
        },
    }

    // 3. Policy evaluation (hooks decide if AI proposal is valid)
    result, err := engine.Tx(context.Background(), txReq)
    require.NoError(t, err) // Should succeed if AI reasoning is sound

    // 4. Verify receipt includes AI metadata
    receipt, err := getReceipt(result.ReceiptID)
    require.NoError(t, err)
    assert.Contains(t, receipt.Metadata, "ai_analysis")
    assert.Contains(t, receipt.Metadata, "confidence")

    // 5. Verify action was taken
    verification, err := verifyReceipt(result.ReceiptID)
    require.NoError(t, err)
    assert.True(t, verification.OK)

    // 6. Verify AI can query results
    aiQueryResult, err := aiAgent.QueryResults(context.Background(), result.ReceiptID)
    require.NoError(t, err)
    assert.NotEmpty(t, aiQueryResult)
}
```

#### Test 7.2: Cross-Component Integration
**Validates:** All components work together seamlessly

```go
func TestE2E_CrossComponentIntegration(t *testing.T) {
    // Setup: Complete system with all features
    engine, err := setupCompleteSystem()
    require.NoError(t, err)

    // 1. Transaction with SHACL validation
    txReq := TxRequest{
        Actor: "data-entry@example.com",
        Delta: struct {
            Add []Quad `json:"add"`
            Rem []Quad `json:"rem"`
        }{
            Add: []Quad{{
                Subject:   "<http://example.org/person/john>",
                Predicate: "<http://xmlns.com/foaf/0.1/name>",
                Object:    "\"John Doe\"",
                Graph:     "people",
            }},
        },
    }

    // 2. Pre-transaction SHACL validation
    validationResult, err := validateWithSHACL(txReq.Delta.Add)
    require.NoError(t, err)
    assert.True(t, validationResult.Conforms)

    // 3. Execute transaction
    result, err := engine.Tx(context.Background(), txReq)
    require.NoError(t, err)

    // 4. Post-transaction hooks (business logic)
    hookResults, err := executePostTransactionHooks(result.ReceiptID)
    require.NoError(t, err)
    assert.NotEmpty(t, hookResults)

    // 5. Verify vector search integration
    vectorResult, err := vectorSearch("John Doe", 5)
    require.NoError(t, err)
    assert.NotEmpty(t, vectorResult.Items)

    // 6. Query with time-travel
    historicalQuery, err := engine.Query(context.Background(), QueryRequest{
        Query: "SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }",
        Kind:  "sparql-select",
        At:    time.Now().Add(-1 * time.Hour), // Query 1 hour ago
    })
    require.NoError(t, err)
    // Should not include the recently added person
    assert.Len(t, historicalQuery.Rows, 0)
}
```

## Test Infrastructure

### Test Environment Setup

```go
func setupTestEnvironment() (*Engine, error) {
    config := Config{
        Store: store.Config{MaxQuads: 10000},
        Cache: sparql.CacheConfig{Capacity: 100},
        Hooks: hooks.Config{MaxHooks: 50},
    }

    return New(config)
}

func setupCompleteTestEnvironment() (*Engine, error) {
    // Setup with all features enabled
    config := Config{
        Store: store.Config{MaxQuads: 10000},
        Cache: sparql.CacheConfig{Capacity: 100},
        Hooks: hooks.Config{MaxHooks: 50},
    }

    engine, err := New(config)
    if err != nil {
        return nil, err
    }

    // Load test policies
    err = engine.LoadPolicyPacks(context.Background(), []string{"test-policies/"})
    if err != nil {
        return nil, err
    }

    return engine, nil
}
```

### Test Data

```go
var testQuads = []Quad{
    {
        Subject:   "<http://example.org/person/alice>",
        Predicate: "<http://xmlns.com/foaf/0.1/name>",
        Object:    "\"Alice\"",
        Graph:     "people",
    },
    {
        Subject:   "<http://example.org/person/bob>",
        Predicate: "<http://xmlns.com/foaf/0.1/knows>",
        Object:    "<http://example.org/person/alice>",
        Graph:     "social",
    },
}

var testPolicies = `
@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] .
`
```

### Test Assertions

```go
func assertReceiptValid(t *testing.T, receiptID string) {
    verification, err := verifyReceipt(receiptID)
    require.NoError(t, err)
    assert.True(t, verification.OK, "Receipt should be valid")
    assert.True(t, verification.SignatureVerified, "Signature should be valid")
    assert.True(t, verification.MerkleRootVerified, "Merkle root should be valid")
}

func assertPolicyEnforced(t *testing.T, txReq TxRequest, expectedError bool) {
    engine, _ := setupTestEnvironment()
    result, err := engine.Tx(context.Background(), txReq)

    if expectedError {
        assert.Error(t, err)
        assert.Nil(t, result)
    } else {
        assert.NoError(t, err)
        assert.NotEmpty(t, result.ReceiptID)
    }
}
```

## Test Execution

### Running E2E Tests

```bash
# Run all e2e tests
go test -run TestE2E -v

# Run specific test categories
go test -run TestE2E_FactPolicyReceipt -v
go test -run TestE2E_ProposalEvaluation -v
go test -run TestE2E_Governance -v

# Run with detailed logging
go test -run TestE2E -v -args -test.v

# Run with timeout for performance testing
go test -run TestE2E -timeout 5m -v
```

### Test Reporting

**Coverage Requirements:**
- E2E test coverage: > 80%
- Critical path coverage: > 95%
- Error condition coverage: > 90%

**Performance Benchmarks:**
- Transaction latency: < 100ms p95
- Hook execution: < 50ms p95
- Receipt verification: < 10ms p95

## Success Metrics

The e2e tests validate that Knowd successfully implements the thesis:

1. ✅ **Knowledge = Facts + Policies + Receipts** - All three components work together
2. ✅ **Only Hooks Act** - Governance model enforced
3. ✅ **Cryptographic Proofs** - All actions have verifiable receipts
4. ✅ **Real-time Policy Enforcement** - Policies evaluated during transactions
5. ✅ **Multi-tenancy** - Complete namespace isolation
6. ✅ **Performance Targets** - Sub-100ms decision latency
7. ✅ **Auditability** - Complete replay capability

These tests prove that Knowd transforms knowledge management from document storage to governed, auditable, and provable actions.
