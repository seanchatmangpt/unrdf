package hooks

import (
	"context"
	"testing"
)

func TestRegistry_NewRegistry(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	if registry == nil {
		t.Error("NewRegistry() returned nil")
	}

	if registry.hooks == nil {
		t.Error("Registry hooks map is nil")
	}

	if registry.config.MaxHooks != 10 {
		t.Errorf("Registry config MaxHooks = %d, want 10", registry.config.MaxHooks)
	}
}

func TestRegistry_Register(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	hook := &HookDefinition{
		ID:       "test-hook-1",
		Name:     "Test Hook 1",
		Type:     "sparql-ask",
		Query:    "ASK WHERE { ?s ?p ?o }",
		Schedule: "0 * * * *",
		Config:   map[string]interface{}{"threshold": 5},
		Enabled:  true,
	}

	t.Run("register valid hook", func(t *testing.T) {
		err := registry.Register(hook)
		if err != nil {
			t.Errorf("Register() error = %v", err)
		}

		count := registry.GetHookCount()
		if count != 1 {
			t.Errorf("GetHookCount() = %d, want 1", count)
		}
	})

	t.Run("register duplicate hook", func(t *testing.T) {
		err := registry.Register(hook)
		if err != nil {
			t.Errorf("Register() duplicate error = %v", err)
		}

		count := registry.GetHookCount()
		if count != 1 {
			t.Errorf("GetHookCount() after duplicate = %d, want 1", count)
		}
	})

	t.Run("register hook with empty ID", func(t *testing.T) {
		invalidHook := &HookDefinition{
			Name:     "Invalid Hook",
			Type:     "sparql-ask",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Enabled:  true,
		}

		err := registry.Register(invalidHook)
		if err == nil {
			t.Error("Register() should have failed for hook with empty ID")
		}
	})

	t.Run("register hook with empty name", func(t *testing.T) {
		invalidHook := &HookDefinition{
			ID:       "test-hook-2",
			Type:     "sparql-ask",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Enabled:  true,
		}

		err := registry.Register(invalidHook)
		if err == nil {
			t.Error("Register() should have failed for hook with empty name")
		}
	})

	t.Run("register hook with empty type", func(t *testing.T) {
		invalidHook := &HookDefinition{
			ID:       "test-hook-3",
			Name:     "Invalid Hook",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Enabled:  true,
		}

		err := registry.Register(invalidHook)
		if err == nil {
			t.Error("Register() should have failed for hook with empty type")
		}
	})

	t.Run("exceed max hooks", func(t *testing.T) {
		smallConfig := Config{MaxHooks: 1}
		smallRegistry := NewRegistry(smallConfig)

		hook1 := &HookDefinition{
			ID:       "hook-1",
			Name:     "Hook 1",
			Type:     "sparql-ask",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Enabled:  true,
		}

		hook2 := &HookDefinition{
			ID:       "hook-2",
			Name:     "Hook 2",
			Type:     "sparql-ask",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Enabled:  true,
		}

		err := smallRegistry.Register(hook1)
		if err != nil {
			t.Errorf("Register() first hook error = %v", err)
		}

		err = smallRegistry.Register(hook2)
		if err == nil {
			t.Error("Register() should have failed when exceeding MaxHooks")
		}
	})
}

func TestRegistry_Unregister(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	hook := &HookDefinition{
		ID:       "test-hook",
		Name:     "Test Hook",
		Type:     "sparql-ask",
		Query:    "ASK WHERE { ?s ?p ?o }",
		Enabled:  true,
	}

	// Register hook first
	err := registry.Register(hook)
	if err != nil {
		t.Fatalf("Register() error = %v", err)
	}

	// Unregister existing hook
	registry.Unregister("test-hook")

	count := registry.GetHookCount()
	if count != 0 {
		t.Errorf("GetHookCount() after unregister = %d, want 0", count)
	}

	// Unregister non-existing hook should be safe
	registry.Unregister("non-existing-hook")
	count = registry.GetHookCount()
	if count != 0 {
		t.Errorf("GetHookCount() after unregistering non-existing = %d, want 0", count)
	}
}

func TestRegistry_GetHook(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	hook := &HookDefinition{
		ID:       "test-hook",
		Name:     "Test Hook",
		Type:     "sparql-ask",
		Query:    "ASK WHERE { ?s ?p ?o }",
		Enabled:  true,
	}

	// Register hook first
	err := registry.Register(hook)
	if err != nil {
		t.Fatalf("Register() error = %v", err)
	}

	t.Run("get existing hook", func(t *testing.T) {
		retrievedHook, exists := registry.GetHook("test-hook")
		if !exists {
			t.Error("GetHook() should return true for existing hook")
		}

		if retrievedHook == nil {
			t.Error("GetHook() should return hook for existing hook")
		}

		if retrievedHook.ID != "test-hook" {
			t.Errorf("GetHook() returned wrong ID: %s", retrievedHook.ID)
		}

		if retrievedHook.Name != "Test Hook" {
			t.Errorf("GetHook() returned wrong name: %s", retrievedHook.Name)
		}
	})

	t.Run("get non-existing hook", func(t *testing.T) {
		retrievedHook, exists := registry.GetHook("non-existing")
		if exists {
			t.Error("GetHook() should return false for non-existing hook")
		}

		if retrievedHook != nil {
			t.Error("GetHook() should return nil for non-existing hook")
		}
	})
}

func TestRegistry_ListHooks(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	hooks := []*HookDefinition{
		{
			ID:       "hook-1",
			Name:     "Hook 1",
			Type:     "sparql-ask",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Enabled:  true,
		},
		{
			ID:       "hook-2",
			Name:     "Hook 2",
			Type:     "threshold",
			Query:    "SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }",
			Enabled:  true,
		},
	}

	// Register hooks
	for _, hook := range hooks {
		err := registry.Register(hook)
		if err != nil {
			t.Fatalf("Register() error = %v", err)
		}
	}

	listedHooks := registry.ListHooks()
	if len(listedHooks) != 2 {
		t.Errorf("ListHooks() returned %d hooks, want 2", len(listedHooks))
	}

	// Check that hooks are in the list
	hookIDs := make(map[string]bool)
	for _, hook := range listedHooks {
		hookIDs[hook.ID] = true
	}

	if !hookIDs["hook-1"] {
		t.Error("ListHooks() missing hook-1")
	}

	if !hookIDs["hook-2"] {
		t.Error("ListHooks() missing hook-2")
	}
}

func TestRegistry_Evaluate(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	hooks := []*HookDefinition{
		{
			ID:       "sparql-hook",
			Name:     "SPARQL Hook",
			Type:     "sparql-ask",
			Query:    "ASK WHERE { ?s ?p ?o }",
			Enabled:  true,
		},
		{
			ID:       "threshold-hook",
			Name:     "Threshold Hook",
			Type:     "threshold",
			Config:   map[string]interface{}{"threshold": 5},
			Enabled:  true,
		},
		{
			ID:       "disabled-hook",
			Name:     "Disabled Hook",
			Type:     "count",
			Enabled:  false,
		},
	}

	// Register hooks
	for _, hook := range hooks {
		err := registry.Register(hook)
		if err != nil {
			t.Fatalf("Register() error = %v", err)
		}
	}

	ctx := context.Background()

	results, err := registry.Evaluate(ctx, "test-actor")
	if err != nil {
		t.Errorf("Evaluate() error = %v", err)
	}

	if len(results) != 3 {
		t.Errorf("Evaluate() returned %d results, want 3", len(results))
	}

	// Check results
	resultMap := make(map[string]HookResult)
	for _, result := range results {
		resultMap[result.HookID] = result
	}

	// SPARQL hook should fire
	sparqlResult := resultMap["sparql-hook"]
	if !sparqlResult.Fired {
		t.Error("SPARQL hook should have fired")
	}

	// Threshold hook should fire
	thresholdResult := resultMap["threshold-hook"]
	if !thresholdResult.Fired {
		t.Error("Threshold hook should have fired")
	}

	// Disabled hook should not fire
	disabledResult := resultMap["disabled-hook"]
	if disabledResult.Fired {
		t.Error("Disabled hook should not have fired")
	}
}

func TestRegistry_Clear(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	hook := &HookDefinition{
		ID:       "test-hook",
		Name:     "Test Hook",
		Type:     "sparql-ask",
		Query:    "ASK WHERE { ?s ?p ?o }",
		Enabled:  true,
	}

	// Register hook first
	err := registry.Register(hook)
	if err != nil {
		t.Fatalf("Register() error = %v", err)
	}

	// Clear registry
	registry.Clear()

	count := registry.GetHookCount()
	if count != 0 {
		t.Errorf("GetHookCount() after clear = %d, want 0", count)
	}

	hooks := registry.ListHooks()
	if len(hooks) != 0 {
		t.Errorf("ListHooks() after clear = %d, want 0", len(hooks))
	}
}

func TestRegistry_GetHookCount(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	// Initially empty
	count := registry.GetHookCount()
	if count != 0 {
		t.Errorf("GetHookCount() initially = %d, want 0", count)
	}

	hook := &HookDefinition{
		ID:       "test-hook",
		Name:     "Test Hook",
		Type:     "sparql-ask",
		Query:    "ASK WHERE { ?s ?p ?o }",
		Enabled:  true,
	}

	// Register hook
	err := registry.Register(hook)
	if err != nil {
		t.Fatalf("Register() error = %v", err)
	}

	count = registry.GetHookCount()
	if count != 1 {
		t.Errorf("GetHookCount() after register = %d, want 1", count)
	}

	// Register another hook
	hook2 := &HookDefinition{
		ID:       "test-hook-2",
		Name:     "Test Hook 2",
		Type:     "threshold",
		Enabled:  true,
	}

	err = registry.Register(hook2)
	if err != nil {
		t.Fatalf("Register() error = %v", err)
	}

	count = registry.GetHookCount()
	if count != 2 {
		t.Errorf("GetHookCount() after second register = %d, want 2", count)
	}
}

func TestRegistry_EvaluateHook(t *testing.T) {
	config := Config{MaxHooks: 10}
	registry := NewRegistry(config)

	tests := []struct {
		name     string
		hookType string
		wantFired bool
	}{
		{"sparql-ask", "sparql-ask", true},
		{"threshold", "threshold", true},
		{"count", "count", true},
		{"window", "window", true},
		{"unknown", "unknown-type", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			hook := &HookDefinition{
				ID:       "test-hook",
				Name:     "Test Hook",
				Type:     tt.hookType,
				Query:    "ASK WHERE { ?s ?p ?o }",
				Enabled:  true,
			}

			result := registry.evaluateHook(context.Background(), hook, "test-actor")

			if result.Fired != tt.wantFired {
				t.Errorf("evaluateHook() fired = %v, want %v", result.Fired, tt.wantFired)
			}

			if result.HookID != "test-hook" {
				t.Errorf("evaluateHook() hookID = %s, want test-hook", result.HookID)
			}

			if result.Name != "Test Hook" {
				t.Errorf("evaluateHook() name = %s, want Test Hook", result.Name)
			}
		})
	}
}
