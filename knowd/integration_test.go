package main

import (
	"context"
	"testing"

	"github.com/unrdf/knowd/internal/hooks"
	"github.com/unrdf/knowd/internal/policy"
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
