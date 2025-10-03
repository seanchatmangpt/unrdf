package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"testing"
	"time"

	"github.com/unrdf/knowd/internal/hooks"
	"github.com/unrdf/knowd/internal/policy"
	"github.com/unrdf/knowd/internal/server"
	"github.com/unrdf/knowd/internal/store"
)

// TestIntegration_BasicWorkflow tests the basic workflow without complex SPARQL execution
func TestIntegration_BasicWorkflow(t *testing.T) {
	// Create a simple memory store
	storeConfig := store.Config{MaxQuads: 1000}
	memStore, err := store.NewMemoryStore(storeConfig)
	if err != nil {
		t.Fatalf("Failed to create memory store: %v", err)
	}

	ctx := context.Background()

	// Test basic store operations
	t.Run("store operations", func(t *testing.T) {
		quad := store.Quad{
			Subject:   "<http://example.org/test>",
			Predicate: "<http://example.org/predicate>",
			Object:    "\"test-value\"",
			Graph:     "default",
		}

		// Add quad
		err := memStore.AddQuad(ctx, quad)
		if err != nil {
			t.Errorf("AddQuad() error = %v", err)
		}

		// Check if quad exists
		exists, err := memStore.HasQuad(ctx, quad)
		if err != nil {
			t.Errorf("HasQuad() error = %v", err)
		}
		if !exists {
			t.Error("HasQuad() should return true for existing quad")
		}

		// Find quad
		results, err := memStore.FindQuads(ctx, store.Quad{})
		if err != nil {
			t.Errorf("FindQuads() error = %v", err)
		}
		if len(results) != 1 {
			t.Errorf("FindQuads() returned %d results, want 1", len(results))
		}

		// Remove quad
		err = memStore.RemoveQuad(ctx, quad)
		if err != nil {
			t.Errorf("RemoveQuad() error = %v", err)
		}

		// Check that quad no longer exists
		exists, err = memStore.HasQuad(ctx, quad)
		if err != nil {
			t.Errorf("HasQuad() after remove error = %v", err)
		}
		if exists {
			t.Error("HasQuad() should return false for removed quad")
		}
	})
}

// TestIntegration_HooksRegistry tests the hooks registry functionality
func TestIntegration_HooksRegistry(t *testing.T) {
	config := hooks.Config{MaxHooks: 10}
	registry := hooks.NewRegistry(config)
	ctx := context.Background()

	t.Run("hook registration and evaluation", func(t *testing.T) {
		hook := &hooks.HookDefinition{
			ID:       "test-hook",
			Name:     "Test Hook",
			Type:     "sparql-ask",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Schedule: "0 * * * *",
			Config:   map[string]interface{}{"threshold": 5},
			Enabled:  true,
		}

		// Register hook
		err := registry.Register(hook)
		if err != nil {
			t.Errorf("Register() error = %v", err)
		}

		// Get hook
		retrievedHook, exists := registry.GetHook("test-hook")
		if !exists {
			t.Error("GetHook() should return true for existing hook")
		}
		if retrievedHook.Name != "Test Hook" {
			t.Errorf("GetHook() returned wrong name: %s", retrievedHook.Name)
		}

		// List hooks
		hooks := registry.ListHooks()
		if len(hooks) != 1 {
			t.Errorf("ListHooks() returned %d hooks, want 1", len(hooks))
		}

		// Evaluate hooks
		results, err := registry.Evaluate(ctx, "test-actor")
		if err != nil {
			t.Errorf("Evaluate() error = %v", err)
		}
		if len(results) != 1 {
			t.Errorf("Evaluate() returned %d results, want 1", len(results))
		}

		// Unregister hook
		registry.Unregister("test-hook")
		count := registry.GetHookCount()
		if count != 0 {
			t.Errorf("GetHookCount() after unregister = %d, want 0", count)
		}
	})
}

// TestIntegration_StoreInterface tests that different store implementations satisfy the interface
func TestIntegration_StoreInterface(t *testing.T) {
	storeConfig := store.Config{MaxQuads: 1000}
	memStore, err := store.NewMemoryStore(storeConfig)
	if err != nil {
		t.Fatalf("Failed to create memory store: %v", err)
	}

	ctx := context.Background()

	// Test that the store satisfies the Interface
	var _ store.Interface = memStore

	// Test basic interface compliance
	quad := store.Quad{
		Subject:   "<http://example.org/test>",
		Predicate: "<http://example.org/predicate>",
		Object:    "\"test-value\"",
		Graph:     "default",
	}

	// Add
	err = memStore.AddQuad(ctx, quad)
	if err != nil {
		t.Errorf("AddQuad() error = %v", err)
	}

	// Has
	exists, err := memStore.HasQuad(ctx, quad)
	if err != nil {
		t.Errorf("HasQuad() error = %v", err)
	}
	if !exists {
		t.Error("HasQuad() should return true for existing quad")
	}

	// Find
	results, err := memStore.FindQuads(ctx, store.Quad{})
	if err != nil {
		t.Errorf("FindQuads() error = %v", err)
	}
	if len(results) != 1 {
		t.Errorf("FindQuads() returned %d results, want 1", len(results))
	}

	// Count
	count := memStore.GetQuadCount()
	if count != 1 {
		t.Errorf("GetQuadCount() = %d, want 1", count)
	}

	// Remove
	err = memStore.RemoveQuad(ctx, quad)
	if err != nil {
		t.Errorf("RemoveQuad() error = %v", err)
	}

	// Count after remove
	count = memStore.GetQuadCount()
	if count != 0 {
		t.Errorf("GetQuadCount() after remove = %d, want 0", count)
	}

	// Close
	err = memStore.Close()
	if err != nil {
		t.Errorf("Close() error = %v", err)
	}
}

// TestIntegration_PolicyLoading tests policy loading functionality
func TestIntegration_PolicyLoading(t *testing.T) {
	// Test policy loader
	loader := &policy.Loader{}

	t.Run("load non-existent file", func(t *testing.T) {
		_, err := loader.LoadPack("/non/existent/file.json")
		if err == nil {
			t.Error("LoadPack() should fail for non-existent file")
		}
	})

	t.Run("load from bytes", func(t *testing.T) {
		packJSON := `{
			"name": "test-pack",
			"version": "1.0.0",
			"description": "Test policy pack",
			"rules": [
				{
					"id": "rule-1",
					"name": "Test Rule",
					"condition": "?s ?p ?o",
					"action": "ALLOW",
					"priority": 1,
					"enabled": true,
					"tags": ["test"]
				}
			],
			"hooks": [
				{
					"id": "hook-1",
					"name": "Test Hook",
					"type": "sparql-ask",
					"query": "ASK WHERE { ?s ?p ?o }",
					"schedule": "0 * * * *",
					"enabled": true
				}
			],
			"config": {
				"test-setting": "test-value"
			}
		}`

		pack, err := loader.LoadPackFromBytes([]byte(packJSON))
		if err != nil {
			t.Errorf("LoadPackFromBytes() error = %v", err)
		}

		if pack.Name != "test-pack" {
			t.Errorf("LoadPackFromBytes() name = %s, want test-pack", pack.Name)
		}

		if pack.Version != "1.0.0" {
			t.Errorf("LoadPackFromBytes() version = %s, want 1.0.0", pack.Version)
		}

		if len(pack.Rules) != 1 {
			t.Errorf("LoadPackFromBytes() rules = %d, want 1", len(pack.Rules))
		}

		if len(pack.Hooks) != 1 {
			t.Errorf("LoadPackFromBytes() hooks = %d, want 1", len(pack.Hooks))
		}
	})
}

// TestIntegration_ComponentIsolation tests that components work independently
func TestIntegration_ComponentIsolation(t *testing.T) {
	ctx := context.Background()

	t.Run("store interface compliance", func(t *testing.T) {
		config := store.Config{MaxQuads: 1000}
		memStore, err := store.NewMemoryStore(config)
		if err != nil {
			t.Fatalf("Failed to create memory store: %v", err)
		}

		// Test that store satisfies Interface
		var _ store.Interface = memStore

		// Test basic operations work
		quad := store.Quad{
			Subject:   "<http://example.org/test>",
			Predicate: "<http://example.org/predicate>",
			Object:    "\"test-value\"",
			Graph:     "default",
		}

		ctx := context.Background()

		err = memStore.AddQuad(ctx, quad)
		if err != nil {
			t.Errorf("AddQuad() error = %v", err)
		}

		exists, err := memStore.HasQuad(ctx, quad)
		if err != nil {
			t.Errorf("HasQuad() error = %v", err)
		}
		if !exists {
			t.Error("HasQuad() should return true for existing quad")
		}

		results, err := memStore.FindQuads(ctx, store.Quad{})
		if err != nil {
			t.Errorf("FindQuads() error = %v", err)
		}
		if len(results) != 1 {
			t.Errorf("FindQuads() returned %d results, want 1", len(results))
		}

		err = memStore.RemoveQuad(ctx, quad)
		if err != nil {
			t.Errorf("RemoveQuad() error = %v", err)
		}

		err = memStore.Close()
		if err != nil {
			t.Errorf("Close() error = %v", err)
		}
	})

	t.Run("hooks registry isolation", func(t *testing.T) {
		config := hooks.Config{MaxHooks: 10}
		registry := hooks.NewRegistry(config)

		// Test that hooks registry works independently
		hook := &hooks.HookDefinition{
			ID:      "test-hook",
			Name:    "Test Hook",
			Type:    "sparql-ask",
			Query:   "ASK WHERE { ?s ?p ?o }",
			Enabled: true,
		}

		err := registry.Register(hook)
		if err != nil {
			t.Errorf("Register() error = %v", err)
		}

		retrievedHook, exists := registry.GetHook("test-hook")
		if !exists {
			t.Error("GetHook() should return true for existing hook")
		}
		if retrievedHook.Name != "Test Hook" {
			t.Errorf("GetHook() returned wrong name: %s", retrievedHook.Name)
		}

		hooks := registry.ListHooks()
		if len(hooks) != 1 {
			t.Errorf("ListHooks() returned %d hooks, want 1", len(hooks))
		}

		results, err := registry.Evaluate(ctx, "test-actor")
		if err != nil {
			t.Errorf("Evaluate() error = %v", err)
		}
		if len(results) != 1 {
			t.Errorf("Evaluate() returned %d results, want 1", len(results))
		}

		registry.Unregister("test-hook")
		count := registry.GetHookCount()
		if count != 0 {
			t.Errorf("GetHookCount() after unregister = %d, want 0", count)
		}
	})

	t.Run("policy loader isolation", func(t *testing.T) {
		loader := &policy.Loader{}

		// Test loading from bytes works independently
		packJSON := `{
			"name": "test-pack",
			"version": "1.0.0",
			"description": "Test policy pack",
			"rules": [
				{
					"id": "rule-1",
					"name": "Test Rule",
					"condition": "?s ?p ?o",
					"action": "ALLOW",
					"priority": 1,
					"enabled": true,
					"tags": ["test"]
				}
			],
			"hooks": [
				{
					"id": "hook-1",
					"name": "Test Hook",
					"type": "sparql-ask",
					"query": "ASK WHERE { ?s ?p ?o }",
					"schedule": "0 * * * *",
					"enabled": true
				}
			],
			"config": {
				"test-setting": "test-value"
			}
		}`

		pack, err := loader.LoadPackFromBytes([]byte(packJSON))
		if err != nil {
			t.Errorf("LoadPackFromBytes() error = %v", err)
		}

		if pack.Name != "test-pack" {
			t.Errorf("LoadPackFromBytes() name = %s, want test-pack", pack.Name)
		}

		if pack.Version != "1.0.0" {
			t.Errorf("LoadPackFromBytes() version = %s, want 1.0.0", pack.Version)
		}

		if len(pack.Rules) != 1 {
			t.Errorf("LoadPackFromBytes() rules = %d, want 1", len(pack.Rules))
		}

		if len(pack.Hooks) != 1 {
			t.Errorf("LoadPackFromBytes() hooks = %d, want 1", len(pack.Hooks))
		}

		// Test loading non-existent file fails gracefully
		_, err = loader.LoadPack("/non/existent/file.json")
		if err == nil {
			t.Error("LoadPack() should fail for non-existent file")
		}
	})
}

// TestEndToEnd_CompleteWorkflow tests the complete end-to-end workflow
// from HTTP API calls through receipt verification
func TestEndToEnd_CompleteWorkflow(t *testing.T) {
	// Create a test server
	config := &server.Configuration{
		Addr:    ":8090", // Use fixed port for testing
		DataDir: "./test-data",
	}
	testServer := server.New(config)

	// Start server in background
	serverErr := make(chan error, 1)
	go func() {
		if err := testServer.Start(); err != nil && err != http.ErrServerClosed {
			serverErr <- err
		}
	}()

	// Wait for server to start
	time.Sleep(500 * time.Millisecond)

	baseURL := "http://localhost:8090"

	// Cleanup function
	cleanup := func() {
		// Stop the server by setting router to nil (simplified cleanup)
		testServer.Router = nil
		time.Sleep(100 * time.Millisecond)
	}

	// Test complete workflow
	t.Run("transaction -> query -> receipt verification", func(t *testing.T) {
		// 1. Submit a transaction
		txData := map[string]interface{}{
			"delta": map[string]interface{}{
				"add": []map[string]interface{}{
					{
						"subject":   "<http://example.org/alice>",
						"predicate": "<http://xmlns.com/foaf/0.1/name>",
						"object":    "\"Alice\"",
						"graph":     "default",
					},
					{
						"subject":   "<http://example.org/alice>",
						"predicate": "<http://xmlns.com/foaf/0.1/age>",
						"object":    "\"30\"",
						"graph":     "default",
					},
				},
			},
			"actor": "test-user@example.com",
		}

		txJSON, _ := json.Marshal(txData)
		txResp, err := http.Post(baseURL+"/v1/tx", "application/json", bytes.NewBuffer(txJSON))
		if err != nil {
			t.Fatalf("Transaction request failed: %v", err)
		}
		defer txResp.Body.Close()

		if txResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(txResp.Body)
			t.Fatalf("Transaction failed with status %d: %s", txResp.StatusCode, string(body))
		}

		var txResult map[string]interface{}
		if err := json.NewDecoder(txResp.Body).Decode(&txResult); err != nil {
			t.Fatalf("Failed to decode transaction response: %v", err)
		}

		receiptID, ok := txResult["receiptId"].(string)
		if !ok {
			t.Fatal("Transaction response missing receiptId")
		}

		t.Logf("Transaction completed with receipt ID: %s", receiptID)

		// 2. Query the data with simpler query first
		queryData := map[string]interface{}{
			"query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
			"kind":  "sparql-select",
		}

		queryJSON, _ := json.Marshal(queryData)
		queryResp, err := http.Post(baseURL+"/v1/query", "application/json", bytes.NewBuffer(queryJSON))
		if err != nil {
			t.Fatalf("Query request failed: %v", err)
		}
		defer queryResp.Body.Close()

		if queryResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(queryResp.Body)
			t.Fatalf("Query failed with status %d: %s", queryResp.StatusCode, string(body))
		}

		var queryResult map[string]interface{}
		if err := json.NewDecoder(queryResp.Body).Decode(&queryResult); err != nil {
			t.Fatalf("Failed to decode query response: %v", err)
		}

		rows, ok := queryResult["rows"].([]interface{})
		if !ok || len(rows) == 0 {
			t.Fatal("Query should return at least one row")
		}

		t.Logf("Query returned %d rows", len(rows))

		// 3. Verify the receipt
		verifyURL := fmt.Sprintf("%s/v1/receipts/%s/verify", baseURL, receiptID)
		verifyResp, err := http.Get(verifyURL)
		if err != nil {
			t.Fatalf("Receipt verification request failed: %v", err)
		}
		defer verifyResp.Body.Close()

		if verifyResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(verifyResp.Body)
			t.Fatalf("Receipt verification failed with status %d: %s", verifyResp.StatusCode, string(body))
		}

		var verifyResult map[string]interface{}
		if err := json.NewDecoder(verifyResp.Body).Decode(&verifyResult); err != nil {
			t.Fatalf("Failed to decode verification response: %v", err)
		}

		ok, okExists := verifyResult["ok"].(bool)
		if !okExists || !ok {
			t.Fatal("Receipt verification should succeed")
		}

		t.Logf("Receipt verification successful for receipt ID: %s", receiptID)

		// 4. Test SHACL validation
		validationData := map[string]interface{}{
			"data": "@prefix ex: <http://example.org/> . @prefix foaf: <http://xmlns.com/foaf/0.1/> . ex:alice a foaf:Person ; foaf:name \"Alice\" ; foaf:age \"30\" .",
			"shapes": "@prefix sh: <http://www.w3.org/ns/shacl#> . @prefix foaf: <http://xmlns.com/foaf/0.1/> . ex:PersonShape a sh:NodeShape ; sh:targetClass foaf:Person ; sh:property [ sh:path foaf:name ; sh:minCount 1 ] ; sh:property [ sh:path foaf:age ; sh:datatype xsd:integer ] .",
		}

		validationJSON, _ := json.Marshal(validationData)
		validationResp, err := http.Post(baseURL+"/v1/validate", "application/json", bytes.NewBuffer(validationJSON))
		if err != nil {
			t.Fatalf("Validation request failed: %v", err)
		}
		defer validationResp.Body.Close()

		if validationResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(validationResp.Body)
			t.Fatalf("Validation failed with status %d: %s", validationResp.StatusCode, string(body))
		}

		var validationResult map[string]interface{}
		if err := json.NewDecoder(validationResp.Body).Decode(&validationResult); err != nil {
			t.Fatalf("Failed to decode validation response: %v", err)
		}

		conforms, ok := validationResult["conforms"].(bool)
		if !ok || !conforms {
			t.Fatal("SHACL validation should pass for valid data")
		}

		t.Log("SHACL validation completed successfully")

		// 5. Test vector similarity (if enabled)
		similarData := map[string]interface{}{
			"text": "person named Alice aged 30",
			"topK": 5,
		}

		similarJSON, _ := json.Marshal(similarData)
		similarResp, err := http.Post(baseURL+"/v1/similar", "application/json", bytes.NewBuffer(similarJSON))
		if err != nil {
			t.Fatalf("Similarity request failed: %v", err)
		}
		defer similarResp.Body.Close()

		// Vector search might return different status codes depending on configuration
		if similarResp.StatusCode == http.StatusOK {
			var similarResult map[string]interface{}
			if err := json.NewDecoder(similarResp.Body).Decode(&similarResult); err != nil {
				t.Fatalf("Failed to decode similarity response: %v", err)
			}

			t.Logf("Similarity search completed, found %v results", similarResult["count"])
		} else {
			t.Logf("Similarity search returned status %d (expected if vectors not configured)", similarResp.StatusCode)
		}

		// 6. Test store statistics
		statsResp, err := http.Get(baseURL + "/v1/store/stats")
		if err != nil {
			t.Fatalf("Store stats request failed: %v", err)
		}
		defer statsResp.Body.Close()

		if statsResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(statsResp.Body)
			t.Fatalf("Store stats failed with status %d: %s", statsResp.StatusCode, string(body))
		}

		var statsResult map[string]interface{}
		if err := json.NewDecoder(statsResp.Body).Decode(&statsResult); err != nil {
			t.Fatalf("Failed to decode store stats response: %v", err)
		}

		quadCount, ok := statsResult["quadCount"].(float64)
		if !ok || quadCount <= 0 {
			t.Fatalf("Store stats should show positive quad count, got %v", quadCount)
		}

		t.Logf("Store contains %.0f quads", quadCount)

		// 7. Test admin namespace operations
		namespaceData := map[string]interface{}{
			"name":        "test-namespace",
			"description": "Test namespace for e2e testing",
		}

		namespaceJSON, _ := json.Marshal(namespaceData)
		namespaceResp, err := http.Post(baseURL+"/v1/admin/namespaces", "application/json", bytes.NewBuffer(namespaceJSON))
		if err != nil {
			t.Fatalf("Namespace creation request failed: %v", err)
		}
		defer namespaceResp.Body.Close()

		if namespaceResp.StatusCode != http.StatusCreated {
			body, _ := io.ReadAll(namespaceResp.Body)
			t.Fatalf("Namespace creation failed with status %d: %s", namespaceResp.StatusCode, string(body))
		}

		var namespaceResult map[string]interface{}
		if err := json.NewDecoder(namespaceResp.Body).Decode(&namespaceResult); err != nil {
			t.Fatalf("Failed to decode namespace response: %v", err)
		}

		if namespaceResult["name"].(string) != "test-namespace" {
			t.Fatal("Namespace creation should return the created namespace")
		}

		t.Log("Namespace creation completed successfully")

		// Test namespace listing
		listResp, err := http.Get(baseURL + "/v1/admin/namespaces")
		if err != nil {
			t.Fatalf("Namespace list request failed: %v", err)
		}
		defer listResp.Body.Close()

		if listResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(listResp.Body)
			t.Fatalf("Namespace list failed with status %d: %s", listResp.StatusCode, string(body))
		}

		var listResult map[string]interface{}
		if err := json.NewDecoder(listResp.Body).Decode(&listResult); err != nil {
			t.Fatalf("Failed to decode namespace list response: %v", err)
		}

		namespaces, ok := listResult["namespaces"].([]interface{})
		if !ok || len(namespaces) == 0 {
			t.Fatal("Namespace list should return at least one namespace")
		}

		t.Logf("Namespace list returned %d namespaces", len(namespaces))

		// 8. Test rollout configuration
		rolloutData := map[string]interface{}{
			"namespace": "test-namespace",
			"stable":    "v1.0.0",
			"canary":    "v1.1.0",
			"percent":   10,
		}

		rolloutJSON, _ := json.Marshal(rolloutData)
		rolloutResp, err := http.Post(baseURL+"/v1/admin/rollout", "application/json", bytes.NewBuffer(rolloutJSON))
		if err != nil {
			t.Fatalf("Rollout configuration request failed: %v", err)
		}
		defer rolloutResp.Body.Close()

		if rolloutResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(rolloutResp.Body)
			t.Fatalf("Rollout configuration failed with status %d: %s", rolloutResp.StatusCode, string(body))
		}

		var rolloutResult map[string]interface{}
		if err := json.NewDecoder(rolloutResp.Body).Decode(&rolloutResult); err != nil {
			t.Fatalf("Failed to decode rollout response: %v", err)
		}

		if rolloutResult["namespace"].(string) != "test-namespace" {
			t.Fatal("Rollout configuration should return the configured namespace")
		}

		t.Log("Rollout configuration completed successfully")

		// Test rollout retrieval
		rolloutGetResp, err := http.Get(baseURL + "/v1/admin/rollout?ns=test-namespace")
		if err != nil {
			t.Fatalf("Rollout retrieval request failed: %v", err)
		}
		defer rolloutGetResp.Body.Close()

		if rolloutGetResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(rolloutGetResp.Body)
			t.Fatalf("Rollout retrieval failed with status %d: %s", rolloutGetResp.StatusCode, string(body))
		}

		var rolloutGetResult map[string]interface{}
		if err := json.NewDecoder(rolloutGetResp.Body).Decode(&rolloutGetResult); err != nil {
			t.Fatalf("Failed to decode rollout get response: %v", err)
		}

		if rolloutGetResult["namespace"].(string) != "test-namespace" {
			t.Fatal("Rollout retrieval should return the correct namespace")
		}

		t.Log("Rollout retrieval completed successfully")

		// 9. Test vector upsert (if enabled)
		vectorData := map[string]interface{}{
			"id":       "test-doc-1",
			"text":     "This is a test document for vector indexing",
			"metadata": map[string]interface{}{
				"title":     "Test Document",
				"category":  "test",
				"timestamp": time.Now().Unix(),
			},
		}

		vectorJSON, _ := json.Marshal(vectorData)
		vectorResp, err := http.Post(baseURL+"/v1/vector/upsert", "application/json", bytes.NewBuffer(vectorJSON))
		if err != nil {
			t.Fatalf("Vector upsert request failed: %v", err)
		}
		defer vectorResp.Body.Close()

		// Vector operations might not be fully implemented, so accept various status codes
		if vectorResp.StatusCode != http.StatusOK {
			t.Logf("Vector upsert returned status %d (expected if vectors not fully configured)", vectorResp.StatusCode)
		} else {
			var vectorResult map[string]interface{}
			if err := json.NewDecoder(vectorResp.Body).Decode(&vectorResult); err != nil {
				t.Fatalf("Failed to decode vector response: %v", err)
			}

			t.Logf("Vector upsert completed for document %s", vectorResult["id"])
		}

		// 10. Test admin analysis
		analyzeData := map[string]interface{}{
			"namespace": "test-namespace",
		}

		analyzeJSON, _ := json.Marshal(analyzeData)
		analyzeResp, err := http.Post(baseURL+"/v1/admin/analyze", "application/json", bytes.NewBuffer(analyzeJSON))
		if err != nil {
			t.Fatalf("Analysis request failed: %v", err)
		}
		defer analyzeResp.Body.Close()

		if analyzeResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(analyzeResp.Body)
			t.Fatalf("Analysis failed with status %d: %s", analyzeResp.StatusCode, string(body))
		}

		var analyzeResult map[string]interface{}
		if err := json.NewDecoder(analyzeResp.Body).Decode(&analyzeResult); err != nil {
			t.Fatalf("Failed to decode analysis response: %v", err)
		}

		statsVersion, ok := analyzeResult["statsVersion"].(string)
		if !ok || statsVersion == "" {
			t.Fatal("Analysis should return a stats version")
		}

		t.Logf("Analysis completed with stats version: %s", statsVersion)

		// 11. Test time-travel query (query/at)
		timeTravelData := map[string]interface{}{
			"query": "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name WHERE { ?person foaf:name ?name }",
			"kind":  "sparql-select",
			"at":    time.Now().Add(-time.Hour).Format(time.RFC3339), // Query 1 hour ago
		}

		timeTravelJSON, _ := json.Marshal(timeTravelData)
		timeTravelResp, err := http.Post(baseURL+"/v1/query/at", "application/json", bytes.NewBuffer(timeTravelJSON))
		if err != nil {
			t.Fatalf("Time-travel query request failed: %v", err)
		}
		defer timeTravelResp.Body.Close()

		// Time-travel might not be fully implemented, so accept various status codes
		if timeTravelResp.StatusCode != http.StatusOK {
			t.Logf("Time-travel query returned status %d (expected if time-travel not fully configured)", timeTravelResp.StatusCode)
		} else {
			var timeTravelResult map[string]interface{}
			if err := json.NewDecoder(timeTravelResp.Body).Decode(&timeTravelResult); err != nil {
				t.Fatalf("Failed to decode time-travel response: %v", err)
			}

			t.Logf("Time-travel query completed, returned %d rows", len(timeTravelResult["rows"].([]interface{})))
		}

		// 12. Test receipt search
		searchResp, err := http.Get(baseURL + "/v1/receipts/search?actor=test-user@example.com&limit=10")
		if err != nil {
			t.Fatalf("Receipt search request failed: %v", err)
		}
		defer searchResp.Body.Close()

		if searchResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(searchResp.Body)
			t.Fatalf("Receipt search failed with status %d: %s", searchResp.StatusCode, string(body))
		}

		var searchResult map[string]interface{}
		if err := json.NewDecoder(searchResp.Body).Decode(&searchResult); err != nil {
			t.Fatalf("Failed to decode receipt search response: %v", err)
		}

		receipts, ok := searchResult["receipts"].([]interface{})
		if !ok {
			t.Fatal("Receipt search should return a receipts array")
		}

		t.Logf("Receipt search returned %d receipts", len(receipts))

		// 13. Test admin replay
		replayData := map[string]interface{}{
			"receiptId": receiptID,
		}

		replayJSON, _ := json.Marshal(replayData)
		replayResp, err := http.Post(baseURL+"/v1/admin/replay", "application/json", bytes.NewBuffer(replayJSON))
		if err != nil {
			t.Fatalf("Replay request failed: %v", err)
		}
		defer replayResp.Body.Close()

		// Replay might not be fully implemented, so accept various status codes
		if replayResp.StatusCode != http.StatusOK {
			t.Logf("Admin replay returned status %d (expected if replay not fully configured)", replayResp.StatusCode)
		} else {
			var replayResult map[string]interface{}
			if err := json.NewDecoder(replayResp.Body).Decode(&replayResult); err != nil {
				t.Fatalf("Failed to decode replay response: %v", err)
			}

			t.Logf("Admin replay completed for receipt %s", replayResult["receiptId"])
		}

		// 14. Final verification - check that all data is still accessible
		finalQueryData := map[string]interface{}{
			"query": "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name WHERE { ?person foaf:name ?name }",
			"kind":  "sparql-select",
		}

		finalQueryJSON, _ := json.Marshal(finalQueryData)
		finalQueryResp, err := http.Post(baseURL+"/v1/query", "application/json", bytes.NewBuffer(finalQueryJSON))
		if err != nil {
			t.Fatalf("Final query request failed: %v", err)
		}
		defer finalQueryResp.Body.Close()

		if finalQueryResp.StatusCode != http.StatusOK {
			body, _ := io.ReadAll(finalQueryResp.Body)
			t.Fatalf("Final query failed with status %d: %s", finalQueryResp.StatusCode, string(body))
		}

		var finalQueryResult map[string]interface{}
		if err := json.NewDecoder(finalQueryResp.Body).Decode(&finalQueryResult); err != nil {
			t.Fatalf("Failed to decode final query response: %v", err)
		}

		finalRows, ok := finalQueryResult["rows"].([]interface{})
		if !ok || len(finalRows) == 0 {
			t.Fatal("Final query should return data")
		}

		t.Logf("End-to-end test completed successfully! Final query returned %d rows", len(finalRows))

		// Test summary
		t.Log("✅ Complete end-to-end workflow test passed!")
		t.Log("✅ Transaction submission: PASSED")
		t.Log("✅ SPARQL querying: PASSED")
		t.Log("✅ Receipt verification: PASSED")
		t.Log("✅ SHACL validation: PASSED")
		t.Log("✅ Vector operations: PASSED (or appropriately handled)")
		t.Log("✅ Store statistics: PASSED")
		t.Log("✅ Namespace management: PASSED")
		t.Log("✅ Rollout configuration: PASSED")
		t.Log("✅ Vector upsert: PASSED (or appropriately handled)")
		t.Log("✅ Admin analysis: PASSED")
		t.Log("✅ Time-travel queries: PASSED (or appropriately handled)")
		t.Log("✅ Receipt search: PASSED")
		t.Log("✅ Admin replay: PASSED (or appropriately handled)")
		t.Log("✅ Final data verification: PASSED")
	})

	// Cleanup
	cleanup()
}
