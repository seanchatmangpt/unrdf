package sparql

import (
	"container/list"
	"sync"
)

// PlanCache implements an LRU cache for SPARQL plans.
type PlanCache struct {
	mu       sync.RWMutex
	capacity int
	cache    map[string]*list.Element
	order    *list.List
}

// CacheConfig holds configuration for the plan cache.
type CacheConfig struct {
	Capacity int
}

// cacheEntry represents an entry in the cache.
type cacheEntry struct {
	query string
	plan  *Plan
}

// NewPlanCache creates a new plan cache.
func NewPlanCache(config CacheConfig) *PlanCache {
	return &PlanCache{
		capacity: config.Capacity,
		cache:    make(map[string]*list.Element),
		order:    list.New(),
	}
}

// Get retrieves a plan from the cache.
func (c *PlanCache) Get(query string) *Plan {
	c.mu.RLock()
	element, found := c.cache[query]
	c.mu.RUnlock()

	if !found {
		return nil
	}

	// Move to front (most recently used) - need write lock for this
	c.mu.Lock()
	c.order.MoveToFront(element)
	c.mu.Unlock()

	return element.Value.(*cacheEntry).plan
}

// Put stores a plan in the cache.
func (c *PlanCache) Put(query string, plan *Plan) {
	c.mu.Lock()
	defer c.mu.Unlock()

	if element, found := c.cache[query]; found {
		// Update existing entry
		element.Value.(*cacheEntry).plan = plan
		c.order.MoveToFront(element)
		return
	}

	// Create new entry
	entry := &cacheEntry{
		query: query,
		plan:  plan,
	}

	element := c.order.PushFront(entry)
	c.cache[query] = element

	// Evict if over capacity
	if len(c.cache) > c.capacity {
		c.evictLeastRecentlyUsed()
	}
}

// evictLeastRecentlyUsed removes the least recently used entry.
func (c *PlanCache) evictLeastRecentlyUsed() {
	if element := c.order.Back(); element != nil {
		c.order.Remove(element)
		entry := element.Value.(*cacheEntry)
		delete(c.cache, entry.query)
	}
}

// Size returns the current size of the cache.
func (c *PlanCache) Size() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return len(c.cache)
}

// Clear removes all entries from the cache.
func (c *PlanCache) Clear() {
	c.mu.Lock()
	defer c.mu.Unlock()

	c.cache = make(map[string]*list.Element)
	c.order = list.New()
}
