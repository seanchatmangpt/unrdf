package engine

import (
	"context"
	"testing"

	"github.com/unrdf/knowd/internal/hooks"
	"github.com/unrdf/knowd/internal/policy"
	"github.com/unrdf/knowd/internal/sparql"
	"github.com/unrdf/knowd/internal/store"
)

func TestEngine_New(t *testing.T) {
	config := Config{
		Store: store.Config{MaxQuads: 1000},
		Cache: sparql.CacheConfig{Capacity: 100},
		Hooks: hooks.Config{MaxHooks: 50},
	}

	engine, err := New(config)
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	if engine == nil {
		t.Error("New() returned nil engine")
	}

	if engine.store == nil {
		t.Error("Engine store is nil")
	}

	if engine.hooks == nil {
		t.Error("Engine hooks registry is nil")
	}

	if engine.parser == nil {
		t.Error("Engine parser is nil")
	}

	if engine.cache == nil {
		t.Error("Engine cache is nil")
	}

	if engine.loader == nil {
		t.Error("Engine loader is nil")
	}

	if engine.packRegistry == nil {
		t.Error("Engine pack registry is nil")
	}
}

func TestEngine_Tx(t *testing.T) {
	config := Config{
		Store: store.Config{MaxQuads: 1000},
		Cache: sparql.CacheConfig{Capacity: 100},
		Hooks: hooks.Config{MaxHooks: 50},
	}

	engine, err := New(config)
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	ctx := context.Background()

	t.Run("add quads", func(t *testing.T) {
		req := TxRequest{
			Actor: "test-actor",
			Delta: struct {
				Add []Quad `json:"add"`
				Rem []Quad `json:"rem"`
			}{
				Add: []Quad{
					{Subject: "<http://example.org/s1>", Predicate: "<http://example.org/p1>", Object: "<http://example.org/o1>", Graph: "default"},
					{Subject: "<http://example.org/s2>", Predicate: "<http://example.org/p2>", Object: "<http://example.org/o2>", Graph: "default"},
				},
			},
		}

		result, err := engine.Tx(ctx, req)
		if err != nil {
			t.Fatalf("Tx() error = %v", err)
		}

		if result.ReceiptID == "" {
			t.Error("Tx() returned empty receipt ID")
		}

		if result.Added != 2 {
			t.Errorf("Tx() added = %d, want 2", result.Added)
		}

		if result.Removed != 0 {
			t.Errorf("Tx() removed = %d, want 0", result.Removed)
		}
	})

	t.Run("remove quads", func(t *testing.T) {
		req := TxRequest{
			Actor: "test-actor",
			Delta: struct {
				Add []Quad `json:"add"`
				Rem []Quad `json:"rem"`
			}{
				Rem: []Quad{
					{Subject: "<http://example.org/s1>", Predicate: "<http://example.org/p1>", Object: "<http://example.org/o1>", Graph: "default"},
				},
			},
		}

		result, err := engine.Tx(ctx, req)
		if err != nil {
			t.Fatalf("Tx() error = %v", err)
		}

		if result.Added != 0 {
			t.Errorf("Tx() added = %d, want 0", result.Added)
		}

		if result.Removed != 1 {
			t.Errorf("Tx() removed = %d, want 1", result.Removed)
		}
	})

	t.Run("add and remove quads", func(t *testing.T) {
		req := TxRequest{
			Actor: "test-actor",
			Delta: struct {
				Add []Quad `json:"add"`
				Rem []Quad `json:"rem"`
			}{
				Add: []Quad{
					{Subject: "<http://example.org/s3>", Predicate: "<http://example.org/p3>", Object: "<http://example.org/o3>", Graph: "default"},
				},
				Rem: []Quad{
					{Subject: "<http://example.org/s2>", Predicate: "<http://example.org/p2>", Object: "<http://example.org/o2>", Graph: "default"},
				},
			},
		}

		result, err := engine.Tx(ctx, req)
		if err != nil {
			t.Fatalf("Tx() error = %v", err)
		}

		if result.Added != 1 {
			t.Errorf("Tx() added = %d, want 1", result.Added)
		}

		if result.Removed != 1 {
			t.Errorf("Tx() removed = %d, want 1", result.Removed)
		}
	})
}

func TestEngine_Query(t *testing.T) {
	config := Config{
		Store: store.Config{MaxQuads: 1000},
		Cache: sparql.CacheConfig{Capacity: 100},
		Hooks: hooks.Config{MaxHooks: 50},
	}

	engine, err := New(config)
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	// Add some test data first
	ctx := context.Background()
	addReq := TxRequest{
		Actor: "test-actor",
		Delta: struct {
			Add []Quad `json:"add"`
			Rem []Quad `json:"rem"`
		}{
			Add: []Quad{
				{Subject: "<http://example.org/alice>", Predicate: "<http://example.org/knows>", Object: "<http://example.org/bob>", Graph: "default"},
				{Subject: "<http://example.org/bob>", Predicate: "<http://example.org/age>", Object: "\"25\"", Graph: "default"},
			},
		},
	}

	_, err = engine.Tx(ctx, addReq)
	if err != nil {
		t.Fatalf("Failed to add test data: %v", err)
	}

	tests := []struct {
		name      string
		query     string
		kind      string
		wantRows  int
		wantError bool
	}{
		{
			name:     "simple SELECT",
			query:    "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
			kind:     "sparql-select",
			wantRows: 2,
		},
		{
			name:     "SELECT with filter",
			query:    "SELECT ?s ?o WHERE { ?s <http://example.org/age> ?o }",
			kind:     "sparql-select",
			wantRows: 1,
		},
		{
			name:     "ASK query",
			query:    "ASK WHERE { <http://example.org/alice> <http://example.org/knows> <http://example.org/bob> }",
			kind:     "sparql-ask",
			wantRows: 1,
		},
		{
			name:     "invalid query",
			query:    "INVALID QUERY",
			kind:     "sparql-select",
			wantRows: 0,
			wantError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			req := QueryRequest{
				Query: tt.query,
				Kind:  tt.kind,
			}

			result, err := engine.Query(ctx, req)
			if (err != nil) != tt.wantError {
				t.Errorf("Query() error = %v, wantError %v", err, tt.wantError)
				return
			}

			if err != nil {
				return
			}

			if len(result.Rows) != tt.wantRows {
				t.Errorf("Query() rows = %d, want %d", len(result.Rows), tt.wantRows)
			}

			if result.Kind != tt.kind {
				t.Errorf("Query() kind = %s, want %s", result.Kind, tt.kind)
			}
		})
	}
}

func TestEngine_GetStats(t *testing.T) {
	config := Config{
		Store: store.Config{MaxQuads: 1000},
		Cache: sparql.CacheConfig{Capacity: 100},
		Hooks: hooks.Config{MaxHooks: 50},
	}

	engine, err := New(config)
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	ctx := context.Background()

	stats := engine.GetStats(ctx)
	if stats == nil {
		t.Error("GetStats() returned nil")
	}

	// Check that expected keys exist
	expectedKeys := []string{"quads", "hooks", "plans", "packCount"}
	for _, key := range expectedKeys {
		if _, exists := stats[key]; !exists {
			t.Errorf("GetStats() missing key %s", key)
		}
	}
}

func TestEngine_LoadPolicyPacks(t *testing.T) {
	config := Config{
		Store: store.Config{MaxQuads: 1000},
		Cache: sparql.CacheConfig{Capacity: 100},
		Hooks: hooks.Config{MaxHooks: 50},
	}

	engine, err := New(config)
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	ctx := context.Background()

	// Test with empty paths
	err = engine.LoadPolicyPacks(ctx, []string{})
	if err != nil {
		t.Errorf("LoadPolicyPacks() with empty paths error = %v", err)
	}

	// Test with non-existent path
	err = engine.LoadPolicyPacks(ctx, []string{"/non/existent/path"})
	if err != nil {
		t.Errorf("LoadPolicyPacks() with non-existent path error = %v", err)
	}
}

func TestEngine_PackManagement(t *testing.T) {
	config := Config{
		Store: store.Config{MaxQuads: 1000},
		Cache: sparql.CacheConfig{Capacity: 100},
		Hooks: hooks.Config{MaxHooks: 50},
	}

	engine, err := New(config)
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}

	// Initially no packs
	packs := engine.GetLoadedPacks()
	if len(packs) != 0 {
		t.Errorf("GetLoadedPacks() initially = %d, want 0", len(packs))
	}

	// Test getting non-existent pack
	pack, exists := engine.GetPack("non-existent")
	if exists {
		t.Error("GetPack() should return false for non-existent pack")
	}
	if pack != nil {
		t.Error("GetPack() should return nil for non-existent pack")
	}
}
