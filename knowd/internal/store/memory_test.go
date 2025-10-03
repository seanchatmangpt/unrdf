package store

import (
	"context"
	"testing"
)

func TestMemoryStore_NewMemoryStore(t *testing.T) {
	config := Config{MaxQuads: 1000}

	store, err := NewMemoryStore(config)
	if err != nil {
		t.Fatalf("NewMemoryStore() error = %v", err)
	}

	if store == nil {
		t.Error("NewMemoryStore() returned nil store")
	}

	memStore, ok := store.(*MemoryStore)
	if !ok {
		t.Error("NewMemoryStore() did not return a MemoryStore")
	}

	if memStore.quads == nil {
		t.Error("MemoryStore quads map is nil")
	}

	if memStore.config.MaxQuads != 1000 {
		t.Errorf("MemoryStore config MaxQuads = %d, want 1000", memStore.config.MaxQuads)
	}
}

func TestMemoryStore_AddQuad(t *testing.T) {
	config := Config{MaxQuads: 1000}
	store, err := NewMemoryStore(config)
	if err != nil {
		t.Fatalf("NewMemoryStore() error = %v", err)
	}

	ctx := context.Background()

	t.Run("add single quad", func(t *testing.T) {
		quad := Quad{
			Subject:   "<http://example.org/s1>",
			Predicate: "<http://example.org/p1>",
			Object:    "<http://example.org/o1>",
			Graph:     "default",
		}

		err := store.AddQuad(ctx, quad)
		if err != nil {
			t.Errorf("AddQuad() error = %v", err)
		}

		count := store.GetQuadCount()
		if count != 1 {
			t.Errorf("GetQuadCount() = %d, want 1", count)
		}
	})

	t.Run("add duplicate quad", func(t *testing.T) {
		quad := Quad{
			Subject:   "<http://example.org/s1>",
			Predicate: "<http://example.org/p1>",
			Object:    "<http://example.org/o1>",
			Graph:     "default",
		}

		err := store.AddQuad(ctx, quad)
		if err != nil {
			t.Errorf("AddQuad() duplicate error = %v", err)
		}

		count := store.GetQuadCount()
		if count != 1 {
			t.Errorf("GetQuadCount() after duplicate = %d, want 1", count)
		}
	})

	t.Run("add multiple quads", func(t *testing.T) {
		quads := []Quad{
			{Subject: "<http://example.org/s2>", Predicate: "<http://example.org/p2>", Object: "<http://example.org/o2>", Graph: "default"},
			{Subject: "<http://example.org/s3>", Predicate: "<http://example.org/p3>", Object: "<http://example.org/o3>", Graph: "default"},
		}

		for _, quad := range quads {
			err := store.AddQuad(ctx, quad)
			if err != nil {
				t.Errorf("AddQuad() error = %v", err)
			}
		}

		count := store.GetQuadCount()
		if count != 3 {
			t.Errorf("GetQuadCount() = %d, want 3", count)
		}
	})

	t.Run("exceed max quads", func(t *testing.T) {
		smallConfig := Config{MaxQuads: 2}
		smallStore, err := NewMemoryStore(smallConfig)
		if err != nil {
			t.Fatalf("NewMemoryStore() error = %v", err)
		}

		// Add 2 quads (at limit)
		quad1 := Quad{Subject: "<http://example.org/s1>", Predicate: "<http://example.org/p1>", Object: "<http://example.org/o1>", Graph: "default"}
		quad2 := Quad{Subject: "<http://example.org/s2>", Predicate: "<http://example.org/p2>", Object: "<http://example.org/o2>", Graph: "default"}

		err = smallStore.AddQuad(ctx, quad1)
		if err != nil {
			t.Errorf("AddQuad() error = %v", err)
		}

		err = smallStore.AddQuad(ctx, quad2)
		if err != nil {
			t.Errorf("AddQuad() error = %v", err)
		}

		// Try to add third quad (should fail)
		quad3 := Quad{Subject: "<http://example.org/s3>", Predicate: "<http://example.org/p3>", Object: "<http://example.org/o3>", Graph: "default"}
		err = smallStore.AddQuad(ctx, quad3)
		if err == nil {
			t.Error("AddQuad() should have failed when exceeding MaxQuads")
		}
	})
}

func TestMemoryStore_RemoveQuad(t *testing.T) {
	config := Config{MaxQuads: 1000}
	store, err := NewMemoryStore(config)
	if err != nil {
		t.Fatalf("NewMemoryStore() error = %v", err)
	}

	ctx := context.Background()

	// Add some test data
	quads := []Quad{
		{Subject: "<http://example.org/s1>", Predicate: "<http://example.org/p1>", Object: "<http://example.org/o1>", Graph: "default"},
		{Subject: "<http://example.org/s2>", Predicate: "<http://example.org/p2>", Object: "<http://example.org/o2>", Graph: "default"},
		{Subject: "<http://example.org/s3>", Predicate: "<http://example.org/p3>", Object: "<http://example.org/o3>", Graph: "default"},
	}

	for _, quad := range quads {
		err := store.AddQuad(ctx, quad)
		if err != nil {
			t.Fatalf("AddQuad() error = %v", err)
		}
	}

	t.Run("remove existing quad", func(t *testing.T) {
		quadToRemove := Quad{
			Subject:   "<http://example.org/s2>",
			Predicate: "<http://example.org/p2>",
			Object:    "<http://example.org/o2>",
			Graph:     "default",
		}

		err := store.RemoveQuad(ctx, quadToRemove)
		if err != nil {
			t.Errorf("RemoveQuad() error = %v", err)
		}

		count := store.GetQuadCount()
		if count != 2 {
			t.Errorf("GetQuadCount() after remove = %d, want 2", count)
		}
	})

	t.Run("remove non-existing quad", func(t *testing.T) {
		quadToRemove := Quad{
			Subject:   "<http://example.org/nonexistent>",
			Predicate: "<http://example.org/p1>",
			Object:    "<http://example.org/o1>",
			Graph:     "default",
		}

		err := store.RemoveQuad(ctx, quadToRemove)
		if err != nil {
			t.Errorf("RemoveQuad() for non-existing quad error = %v", err)
		}

		count := store.GetQuadCount()
		if count != 2 {
			t.Errorf("GetQuadCount() after removing non-existing = %d, want 2", count)
		}
	})
}

func TestMemoryStore_HasQuad(t *testing.T) {
	config := Config{MaxQuads: 1000}
	store, err := NewMemoryStore(config)
	if err != nil {
		t.Fatalf("NewMemoryStore() error = %v", err)
	}

	ctx := context.Background()

	// Add test data
	quad := Quad{
		Subject:   "<http://example.org/s1>",
		Predicate: "<http://example.org/p1>",
		Object:    "<http://example.org/o1>",
		Graph:     "default",
	}

	err = store.AddQuad(ctx, quad)
	if err != nil {
		t.Fatalf("AddQuad() error = %v", err)
	}

	t.Run("existing quad", func(t *testing.T) {
		exists, err := store.HasQuad(ctx, quad)
		if err != nil {
			t.Errorf("HasQuad() error = %v", err)
		}

		if !exists {
			t.Error("HasQuad() should return true for existing quad")
		}
	})

	t.Run("non-existing quad", func(t *testing.T) {
		nonExistentQuad := Quad{
			Subject:   "<http://example.org/nonexistent>",
			Predicate: "<http://example.org/p1>",
			Object:    "<http://example.org/o1>",
			Graph:     "default",
		}

		exists, err := store.HasQuad(ctx, nonExistentQuad)
		if err != nil {
			t.Errorf("HasQuad() error = %v", err)
		}

		if exists {
			t.Error("HasQuad() should return false for non-existing quad")
		}
	})
}

func TestMemoryStore_FindQuads(t *testing.T) {
	config := Config{MaxQuads: 1000}
	store, err := NewMemoryStore(config)
	if err != nil {
		t.Fatalf("NewMemoryStore() error = %v", err)
	}

	ctx := context.Background()

	// Add test data
	quads := []Quad{
		{Subject: "<http://example.org/alice>", Predicate: "<http://example.org/knows>", Object: "<http://example.org/bob>", Graph: "default"},
		{Subject: "<http://example.org/bob>", Predicate: "<http://example.org/age>", Object: "\"25\"", Graph: "default"},
		{Subject: "<http://example.org/charlie>", Predicate: "<http://example.org/knows>", Object: "<http://example.org/alice>", Graph: "default"},
	}

	for _, quad := range quads {
		err := store.AddQuad(ctx, quad)
		if err != nil {
			t.Fatalf("AddQuad() error = %v", err)
		}
	}

	t.Run("find all quads", func(t *testing.T) {
		pattern := Quad{
			Subject:   "",
			Predicate: "",
			Object:    "",
			Graph:     "",
		}

		results, err := store.FindQuads(ctx, pattern)
		if err != nil {
			t.Errorf("FindQuads() error = %v", err)
		}

		if len(results) != 3 {
			t.Errorf("FindQuads() returned %d results, want 3", len(results))
		}
	})

	t.Run("find by subject", func(t *testing.T) {
		pattern := Quad{
			Subject:   "<http://example.org/alice>",
			Predicate: "",
			Object:    "",
			Graph:     "",
		}

		results, err := store.FindQuads(ctx, pattern)
		if err != nil {
			t.Errorf("FindQuads() error = %v", err)
		}

		if len(results) != 1 {
			t.Errorf("FindQuads() by subject returned %d results, want 1", len(results))
		}

		if results[0].Subject != "<http://example.org/alice>" {
			t.Errorf("FindQuads() by subject returned wrong subject: %s", results[0].Subject)
		}
	})

	t.Run("find by predicate", func(t *testing.T) {
		pattern := Quad{
			Subject:   "",
			Predicate: "<http://example.org/knows>",
			Object:    "",
			Graph:     "",
		}

		results, err := store.FindQuads(ctx, pattern)
		if err != nil {
			t.Errorf("FindQuads() error = %v", err)
		}

		if len(results) != 2 {
			t.Errorf("FindQuads() by predicate returned %d results, want 2", len(results))
		}
	})

	t.Run("find exact match", func(t *testing.T) {
		pattern := Quad{
			Subject:   "<http://example.org/bob>",
			Predicate: "<http://example.org/age>",
			Object:    "\"25\"",
			Graph:     "default",
		}

		results, err := store.FindQuads(ctx, pattern)
		if err != nil {
			t.Errorf("FindQuads() error = %v", err)
		}

		if len(results) != 1 {
			t.Errorf("FindQuads() exact match returned %d results, want 1", len(results))
		}
	})
}

func TestMemoryStore_Close(t *testing.T) {
	config := Config{MaxQuads: 1000}
	store, err := NewMemoryStore(config)
	if err != nil {
		t.Fatalf("NewMemoryStore() error = %v", err)
	}

	err = store.Close()
	if err != nil {
		t.Errorf("Close() error = %v", err)
	}

	// Multiple closes should be safe
	err = store.Close()
	if err != nil {
		t.Errorf("Close() second call error = %v", err)
	}
}

func TestMemoryStore_GetQuadCount(t *testing.T) {
	config := Config{MaxQuads: 1000}
	store, err := NewMemoryStore(config)
	if err != nil {
		t.Fatalf("NewMemoryStore() error = %v", err)
	}

	// Initially empty
	count := store.GetQuadCount()
	if count != 0 {
		t.Errorf("GetQuadCount() initially = %d, want 0", count)
	}

	ctx := context.Background()

	// Add some quads
	quads := []Quad{
		{Subject: "<http://example.org/s1>", Predicate: "<http://example.org/p1>", Object: "<http://example.org/o1>", Graph: "default"},
		{Subject: "<http://example.org/s2>", Predicate: "<http://example.org/p2>", Object: "<http://example.org/o2>", Graph: "default"},
	}

	for _, quad := range quads {
		err := store.AddQuad(ctx, quad)
		if err != nil {
			t.Fatalf("AddQuad() error = %v", err)
		}
	}

	count = store.GetQuadCount()
	if count != 2 {
		t.Errorf("GetQuadCount() after adding = %d, want 2", count)
	}

	// Remove a quad
	err = store.RemoveQuad(ctx, quads[0])
	if err != nil {
		t.Fatalf("RemoveQuad() error = %v", err)
	}

	count = store.GetQuadCount()
	if count != 1 {
		t.Errorf("GetQuadCount() after removing = %d, want 1", count)
	}
}
