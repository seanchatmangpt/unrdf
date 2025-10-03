package sparql

import (
	"testing"
)

func TestPlanCache_NewPlanCache(t *testing.T) {
	config := CacheConfig{Capacity: 10}
	cache := NewPlanCache(config)

	if cache.capacity != 10 {
		t.Errorf("NewPlanCache() capacity = %v, want 10", cache.capacity)
	}

	if cache.Size() != 0 {
		t.Errorf("NewPlanCache() size = %v, want 0", cache.Size())
	}
}

func TestPlanCache_PutAndGet(t *testing.T) {
	config := CacheConfig{Capacity: 2}
	cache := NewPlanCache(config)

	plan1 := &Plan{Type: "SELECT"}
	plan2 := &Plan{Type: "ASK"}

	// Put plans
	cache.Put("query1", plan1)
	cache.Put("query2", plan2)

	if cache.Size() != 2 {
		t.Errorf("Put() size = %v, want 2", cache.Size())
	}

	// Get plans
	retrieved1 := cache.Get("query1")
	if retrieved1 != plan1 {
		t.Error("Get() returned wrong plan for query1")
	}

	retrieved2 := cache.Get("query2")
	if retrieved2 != plan2 {
		t.Error("Get() returned wrong plan for query2")
	}
}

func TestPlanCache_Update(t *testing.T) {
	config := CacheConfig{Capacity: 2}
	cache := NewPlanCache(config)

	plan1 := &Plan{Type: "SELECT"}
	plan1Updated := &Plan{Type: "SELECT", Limit: 10}

	// Put plan
	cache.Put("query1", plan1)

	// Update plan
	cache.Put("query1", plan1Updated)

	// Get updated plan
	retrieved := cache.Get("query1")
	if retrieved.Limit != 10 {
		t.Errorf("Put() update failed, Limit = %v, want 10", retrieved.Limit)
	}
}

func TestPlanCache_LRU(t *testing.T) {
	config := CacheConfig{Capacity: 2}
	cache := NewPlanCache(config)

	plan1 := &Plan{Type: "SELECT"}
	plan2 := &Plan{Type: "ASK"}
	plan3 := &Plan{Type: "CONSTRUCT"}

	// Fill cache
	cache.Put("query1", plan1)
	cache.Put("query2", plan2)

	// Access query1 (should be most recently used)
	cache.Get("query1")

	// Add query3 (should evict query2)
	cache.Put("query3", plan3)

	// Check that query2 was evicted
	if cache.Get("query2") != nil {
		t.Error("LRU eviction failed, query2 should have been evicted")
	}

	// Check that query1 and query3 are still there
	if cache.Get("query1") == nil {
		t.Error("LRU eviction failed, query1 should still be in cache")
	}

	if cache.Get("query3") == nil {
		t.Error("LRU eviction failed, query3 should be in cache")
	}
}

func TestPlanCache_Clear(t *testing.T) {
	config := CacheConfig{Capacity: 2}
	cache := NewPlanCache(config)

	plan1 := &Plan{Type: "SELECT"}
	plan2 := &Plan{Type: "ASK"}

	cache.Put("query1", plan1)
	cache.Put("query2", plan2)

	cache.Clear()

	if cache.Size() != 0 {
		t.Errorf("Clear() size = %v, want 0", cache.Size())
	}

	if cache.Get("query1") != nil {
		t.Error("Clear() failed, query1 should not be in cache")
	}

	if cache.Get("query2") != nil {
		t.Error("Clear() failed, query2 should not be in cache")
	}
}

func TestPlanCache_Size(t *testing.T) {
	config := CacheConfig{Capacity: 5}
	cache := NewPlanCache(config)

	if cache.Size() != 0 {
		t.Errorf("Size() = %v, want 0", cache.Size())
	}

	plan := &Plan{Type: "SELECT"}
	cache.Put("query1", plan)

	if cache.Size() != 1 {
		t.Errorf("Size() = %v, want 1", cache.Size())
	}

	cache.Put("query2", plan)
	cache.Put("query3", plan)

	if cache.Size() != 3 {
		t.Errorf("Size() = %v, want 3", cache.Size())
	}
}

func TestPlanCache_CapacityExceeded(t *testing.T) {
	config := CacheConfig{Capacity: 2}
	cache := NewPlanCache(config)

	plan1 := &Plan{Type: "SELECT"}
	plan2 := &Plan{Type: "ASK"}
	plan3 := &Plan{Type: "CONSTRUCT"}

	// Fill cache
	cache.Put("query1", plan1)
	cache.Put("query2", plan2)

	// Add one more (should trigger eviction)
	cache.Put("query3", plan3)

	// Check size
	if cache.Size() != 2 {
		t.Errorf("Capacity exceeded, size = %v, want 2", cache.Size())
	}

	// Check which items remain (query1 and query3, query2 evicted)
	if cache.Get("query1") == nil {
		t.Error("Capacity exceeded, query1 should still be in cache")
	}

	if cache.Get("query2") != nil {
		t.Error("Capacity exceeded, query2 should have been evicted")
	}

	if cache.Get("query3") == nil {
		t.Error("Capacity exceeded, query3 should be in cache")
	}
}
