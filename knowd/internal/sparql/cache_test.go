package sparql

import (
	"fmt"
	"strings"
	"testing"
)

func TestNewPlanCache(t *testing.T) {
	config := CacheConfig{Capacity: 100}
	cache := NewPlanCache(config)
	
	if cache == nil {
		t.Fatal("NewPlanCache returned nil")
	}
	
	if cache.capacity != 100 {
		t.Errorf("Expected capacity 100, got %d", cache.capacity)
	}
	
	if cache.plans == nil {
		t.Fatal("Cache plans map not initialized")
	}
	
	if cache.accessOrder == nil {
		t.Fatal("Cache accessOrder list not initialized")
	}
}

func TestCachePutGet(t *testing.T) {
	cache := NewPlanCache(CacheConfig{Capacity: 2})
	
	// Create test plans
	plan1 := &Plan{
		Type: "SELECT",
		Columns: []string{"?name"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{
				{Subject: "?person", Predicate: "<http://xmlns.com/foaf/0.1/name>", Object: "?name"},
			}},
		},
		Limit:  10,
		Offset: 0,
	}
	
	plan2 := &Plan{
		Type: "ASK",
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{
				{Subject: "?person", Predicate: "<http://xmlns.com/foaf/0.1/name>", Object: "<John>"},
			}},
		},
	}
	
	query1 := "SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name } LIMIT 10"
	query2 := "ASK { ?person <http://xmlns.com/foaf/0.1/name> <John> }"
	
	// Test put
	cache.Put(query1, plan1)
	if cache.Size() != 1 {
		t.Errorf("Expected size 1 after first put, got %d", cache.Size())
	}
	
	// Test get
	retrievedPlan := cache.Get(query1)
	if retrievedPlan == nil {
		t.Fatal("Get returned nil for cached plan")
	}
	
	if retrievedPlan.Type != plan1.Type {
		t.Errorf("Retrieved plan type mismatch: expected %s, got %s", plan1.Type, retrievedPlan.Type)
	}
	
	if len(retrievedPlan.Columns) != len(plan1.Columns) {
		t.Errorf("Retrieved plan columns mismatch: expected %d, got %d", len(plan1.Columns), len(retrievedPlan.Columns))
	}
	
	// Test put second plan
	cache.Put(query2, plan2)
	if cache.Size() != 2 {
		t.Errorf("Expected size 2 after second put, got %d", cache.Size())
	}
	
	// Verify both plans can be retrieved
	if cache.Get(query1) == nil {
		t.Error("First plan not retrievable after second put")
	}
	
	if cache.Get(query2) == nil {
		t.Error("Second plan not retrievable")
	}
}

func TestCacheLRUBehavior(t *testing.T) {
	cache := NewPlanCache(CacheConfig{Capacity: 2})
	
	plan1 := &Plan{Type: "SELECT"}
	plan2 := &Plan{Type: "ASK"}
	plan3 := &Plan{Type: "CONSTRUCT"}
	
	query1 := "SELECT WHERE { ?s ?p ?o }"
	query2 := "ASK WHERE { ?s ?p ?o }"
	query3 := "CONSTRUCT WHERE { ?s ?p ?o }"
	
	// Add two plans
	cache.Put(query1, plan1)
	cache.Put(query2, plan2)
	
	if cache.Size() != 2 {
		t.Errorf("Expected size 2, got %d", cache.Size())
	}
	
	// Access first plan to make it more recent
	cache.Get(query1)
	
	// Add third plan - should evict the LRU (plan2)
	cache.Put(query3, plan3)
	
	if cache.Size() != 2 {
		t.Errorf("Expected size 2 after putting third plan, got %d", cache.Size())
	}
	
	// Verify LRU plan was evicted
	if cache.Get(query2) != nil {
		t.Error("Second plan should have been evicted")
	}
	
	// Verify other plans are still cached
	if cache.Get(query1) == nil {
		t.Error("First group should still be cached")
	}
	
	if cache.Get(query3) == nil {
		t.Error("Third plan should be cached")
	}
}

func TestCacheEviction(t *testing.T) {
	cache := NewPlanCache(CacheConfig{Capacity: 3})
	
	plans := make([]*Plan, 5)
	queries := make([]string, 5)
	
	for i := 0; i < 5; i++ {
		plans[i] = &Plan{
			Type: "SELECT",
			Columns: []string{fmt.Sprintf("?var%d", i)},
		}
		queries[i] = fmt.Sprintf("SELECT ?var%d WHERE { ?s ?p ?var%d }", i, i)
	}
	
	// Fill cache to capacity
	for i := 0; i < 3; i++ {
		cache.Put(queries[i], plans[i])
	}
	
	if cache.Size() != 3 {
		t.Errorf("Expected size 3, got %d", cache.Size())
	}
	
	// Add one more - should evict least recently used
	cache.Put(queries[3], plans[3])
	
	if cache.Size() != 3 {
		t.Errorf("Expected size 3 after overflow, got %d", cache.Size())
	}
	
	// First plan should be evicted
	if cache.Get(queries[0]) != nil {
		t.Error("First plan should have been evicted")
	}
	
	// Others should still be cached
	for i := 1; i < 4; i++ {
		if cache.Get(queries[i]) == nil {
			t.Errorf("Plan %d should still be cached", i)
		}
	}
}

func TestCacheClear(t *testing.T) {
	cache := NewPlanCache(CacheConfig{Capacity: 10})
	
	plan := &Plan{Type: "SELECT"}
	query := "SELECT ?s WHERE { ?s ?p ?o }"
	
	// Add plan
	cache.Put(query, plan)
	if cache.Size() != 1 {
		t.Errorf("Expected size 1, got %d", cache.Size())
	}
	
	// Clear cache
	cache.Clear()
	if cache.Size() != 0 {
		t.Errorf("Expected size 0 after clear, got %d", cache.Size())
	}
	
	// Verify plan is not retrievable
	if cache.Get(query) != nil {
		t.Error("Plan should not be retrievable after clear")
	}
}

func TestCacheReplace(t *testing.T) {
	cache := NewPlanCache(CacheConfig{Capacity: 2})
	
	plan1 := &Plan{
		Type: "SELECT",
		Columns: []string{"?name"},
		Limit: 10,
	}
	
	plan2 := &Plan{
		Type: "SELECT",
		Columns: []string{"?title"},
		Limit: 20,
	}
	
	query := "SELECT ?name WHERE { ?s ?p ?name }"
	
	// Put initial plan
	cache.Put(query, plan1)
	if cache.Size() != 1 {
		t.Errorf("Expected size 1, got %d", cache.Size())
	}
	
	retrieved := cache.Get(query)
	if retrieved.Limit != 10 {
		t.Errorf("Expected limit 10, got %d", retrieved.Limit)
	}
	
	// Replace with new plan
	cache.Put(query, plan2)
	if cache.Size() != 1 {
		t.Errorf("Expected size 1 after replace, got %d", cache.Size())
	}
	
	retrieved = cache.Get(query)
	if retrieved.Limit != 20 {
		t.Errorf("Expected limit 20 after replace, got %d", retrieved.Limit)
	}
}

func TestCacheCapacity(t *testing.T) {
	capacities := []int{1, 5, 10, 100}
	
	for _, capacity := range capacities {
		cache := NewPlanCache(CacheConfig{Capacity: capacity})
		
		if cache.capacity != capacity {
			t.Errorf("Expected capacity %d, got %d", capacity, cache.capacity)
		}
		
		// Fill cache to capacity
		for i := 0; i < capacity; i++ {
			plan := &Plan{Type: "SELECT"}
			query := strings.Sprintf("SELECT ?var%d WHERE { ?s ?p ?var%d }", i, i)
			cache.Put(query, plan)
		}
		
		if cache.Size() != capacity {
			t.Errorf("Expected size %d at capacity, got %d", capacity, cache.Size())
		}
		
		// Add one more - size should not exceed capacity
		plan := &Plan{Type: "SELECT"}
		query := "SELECT ?overflow WHERE { ?s ?p ?overflow }"
		cache.Put(query, plan)
		
		if cache.Size() > capacity {
			t.Errorf("Size %d exceeds capacity %d", cache.Size(), capacity)
		}
	}
}

// Benchmark cache operations
func BenchmarkCachePut(b *testing.B) {
	cache := NewPlanCache(CacheConfig{Capacity: 1000})
	
	plan := &Plan{
		Type: "SELECT",
		Columns: []string{"?name"},
		Patterns: []BasicGraphPattern{
			{Triples: []Triple{
				{Subject: "?person", Predicate: "<http://xmlns.com/foaf/0.1/name>", Object: "?name"},
			}},
		},
	}
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		query := strings.Sprintf("SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name LIMIT %d }", i)
		cache.Put(query, plan)
	}
}

func BenchmarkCacheGet(b *testing.B) {
	cache := NewPlanCache(CacheConfig{Capacity: 1000})
	
	plan := &Plan{
		Type: "SELECT",
		Columns: []string{"?name"},
	}
	
	query := "SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }"
	cache.Put(query, plan)
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		cache.Get(query)
	}
}

func BenchmarkCacheLRUEviction(b *testing.B) {
	cache := NewPlanCache(CacheConfig{Capacity: 100})
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		plan := &Plan{
			Type: "SELECT",
			Columns: []string{strings.Sprintf("?var%d", i)},
		}
		query := strings.Sprintf("SELECT ?var%d WHERE { ?s ?p ?var%d }", i, i)
		cache.Put(query, plan)
	}
}
